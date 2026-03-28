#!/Users/elle/.doom.d/gslides/.venv/bin/python3
"""
Sync org-mode files with Google Slides/Docs.

Usage:
    org-to-google.py upload <file> [--slides | --doc] [--id ID]
    org-to-google.py pull <org-file> --id ID [--slides | --doc]

Upload: PPTX (slides) or ODT (docs) to Google Drive.
Pull: Download from Google, use Claude to update org file.

Examples:
    org-to-google.py upload slides.pptx --slides              # Create new presentation
    org-to-google.py upload doc.odt --doc --id ABC123         # Update existing doc
    org-to-google.py pull notes.org --doc --id ABC123         # Pull changes back to org
"""

import argparse
import io
import os
import re
import subprocess
import sys
from pathlib import Path

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from googleapiclient.discovery import build
from googleapiclient.http import MediaFileUpload, MediaIoBaseDownload

SCOPES = [
    "https://www.googleapis.com/auth/drive.file",
    "https://www.googleapis.com/auth/drive.readonly",
    "https://www.googleapis.com/auth/documents",
]

# Keychain service/account names (matches google-auth script)
KEYCHAIN_SERVICE = "google-oauth"
KEYCHAIN_ACCOUNT_TOKEN = "token"
KEYCHAIN_ACCOUNT_CLIENT = "client-secret"

# Google MIME types
MIME_TYPES = {
    "slides": {
        "google": "application/vnd.google-apps.presentation",
        "office": "application/vnd.openxmlformats-officedocument.presentationml.presentation",
        "extension": ".pptx",
        "export_text": "text/plain",
        "url_base": "https://docs.google.com/presentation/d/",
    },
    "doc": {
        "google": "application/vnd.google-apps.document",
        "office": "application/vnd.oasis.opendocument.text",
        "extension": ".odt",
        "export_text": "text/markdown",
        "url_base": "https://docs.google.com/document/d/",
    },
}


def keychain_get(account):
    """Get value from macOS Keychain."""
    result = subprocess.run(
        [
            "security",
            "find-generic-password",
            "-s",
            KEYCHAIN_SERVICE,
            "-a",
            account,
            "-w",
        ],
        capture_output=True,
        text=True,
    )
    if result.returncode == 0:
        return result.stdout.strip()
    return None


def keychain_set(account, value):
    """Store value in macOS Keychain."""
    # Delete existing if present
    subprocess.run(
        ["security", "delete-generic-password", "-s", KEYCHAIN_SERVICE, "-a", account],
        capture_output=True,
    )
    # Add new
    result = subprocess.run(
        [
            "security",
            "add-generic-password",
            "-s",
            KEYCHAIN_SERVICE,
            "-a",
            account,
            "-w",
            value,
        ],
        capture_output=True,
    )
    return result.returncode == 0


def get_credentials():
    """Get or refresh Google API credentials from Keychain.
    Auto-reauthenticates via browser if refresh fails."""
    import json
    from google.auth.exceptions import RefreshError

    token_json = keychain_get(KEYCHAIN_ACCOUNT_TOKEN)
    if not token_json:
        print(
            "Error: No OAuth token in keychain.\nRun: google-auth refresh",
            file=sys.stderr,
        )
        sys.exit(1)

    creds = Credentials.from_authorized_user_info(json.loads(token_json), SCOPES)

    if not creds.valid:
        if creds.expired and creds.refresh_token:
            try:
                creds.refresh(Request())
                keychain_set(KEYCHAIN_ACCOUNT_TOKEN, creds.to_json())
            except RefreshError:
                print("Token expired, re-authenticating via browser...", file=sys.stderr)
                creds = _reauth()
        else:
            print("Token invalid, re-authenticating via browser...", file=sys.stderr)
            creds = _reauth()

    return creds


def _reauth():
    """Run browser-based OAuth flow and store new token."""
    import json
    import tempfile
    from google_auth_oauthlib.flow import InstalledAppFlow

    client_json = keychain_get(KEYCHAIN_ACCOUNT_CLIENT)
    if not client_json:
        print("No client secret in keychain. Run: google-auth import", file=sys.stderr)
        sys.exit(1)

    with tempfile.NamedTemporaryFile(mode="w", suffix=".json", delete=False) as f:
        f.write(client_json)
        client_file = f.name

    try:
        flow = InstalledAppFlow.from_client_secrets_file(client_file, SCOPES)
        creds = flow.run_local_server(port=0)
        keychain_set(KEYCHAIN_ACCOUNT_TOKEN, creds.to_json())
        print("Re-authenticated successfully", file=sys.stderr)
        return creds
    finally:
        Path(client_file).unlink()


def upload_new(drive_service, office_path, title, output_format):
    """Upload Office file as new Google Slides/Docs."""
    mime_info = MIME_TYPES[output_format]

    file_metadata = {
        "name": title,
        "mimeType": mime_info["google"],
    }

    media = MediaFileUpload(
        office_path,
        mimetype=mime_info["office"],
        resumable=True,
    )

    file = (
        drive_service.files()
        .create(
            body=file_metadata,
            media_body=media,
            fields="id",
        )
        .execute()
    )

    return file["id"]


def update_existing(drive_service, file_id, office_path, title, output_format):
    """Update existing Google Slides/Docs by replacing its content in-place."""
    mime_info = MIME_TYPES[output_format]

    media = MediaFileUpload(
        office_path,
        mimetype=mime_info["office"],
        resumable=True,
    )

    drive_service.files().update(
        fileId=file_id,
        media_body=media,
    ).execute()

    return file_id


def download_as_text(drive_service, file_id, output_format):
    """Download Google Slides/Docs as text/markdown."""
    mime_info = MIME_TYPES[output_format]

    # For slides, export as plain text; for docs, export as markdown
    request = drive_service.files().export_media(
        fileId=file_id, mimeType=mime_info["export_text"]
    )

    content = io.BytesIO()
    downloader = MediaIoBaseDownload(content, request)

    done = False
    while not done:
        status, done = downloader.next_chunk()

    return content.getvalue().decode("utf-8")


def extract_push_hash(google_content):
    """Extract the commit hash embedded during push from Google Doc content.

    Looks for patterns like 'Generated from post.org (abc1234)' or
    'Generated from commit: abc1234' in the doc footer/header."""
    # Match "Generated from ... (HASH)" — the header format
    m = re.search(r"Generated from.*?\(([0-9a-f]{7,40})\)", google_content)
    if m:
        return m.group(1)

    # Match "Generated from commit: HASH" — the footer format
    m = re.search(r"Generated from commit:.*?([0-9a-f]{7,40})", google_content)
    if m:
        return m.group(1)

    return None


def get_org_at_commit(org_path, commit_hash):
    """Get the org file content at a specific git commit."""
    org_dir = os.path.dirname(os.path.abspath(org_path))

    # Get repo-relative path
    rel_path = subprocess.run(
        ["git", "ls-files", "--full-name", os.path.abspath(org_path)],
        capture_output=True, text=True, cwd=org_dir,
    )
    if rel_path.returncode != 0 or not rel_path.stdout.strip():
        return None

    git_path = rel_path.stdout.strip()

    result = subprocess.run(
        ["git", "show", f"{commit_hash}:{git_path}"],
        capture_output=True, text=True, cwd=org_dir,
    )
    if result.returncode != 0:
        return None

    return result.stdout


def strip_generated_markers(content):
    """Remove 'Generated from ...' header/footer lines added during push."""
    # Remove header: italic line with "Generated from"
    content = re.sub(r"^.*Generated from \[.*?\n\n?", "", content)
    # Remove footer: horizontal rule + Generated from commit line
    content = re.sub(r"\n?-----\n.*Generated from commit:.*$", "", content, flags=re.DOTALL)
    return content


def update_org_with_claude(org_path, google_content, base_org_content=None):
    """Use Claude to update org file based on Google content.

    If base_org_content is provided, does a three-way merge:
      base (org at push time) → google changes → applied to current org.
    Otherwise falls back to two-way merge (old behavior).

    Uses a two-pass approach: first enumerate all changes, then apply
    them one by one to the org file."""
    with open(org_path, "r") as f:
        current_org = f.read()

    if base_org_content:
        prompt = f"""Three-way merge: an org file was pushed to Google Docs, edited there, and now
needs changes pulled back. The org file may also have changed since the push.

BASE org file (at time of push):
```org
{base_org_content}
```

CURRENT Google Doc content (markdown export — includes all edits made in Google):
```markdown
{strip_generated_markers(google_content)}
```

CURRENT org file (may have diverged from base):
```org
{current_org}
```

Your task:
1. First, diff the BASE against the Google Doc to enumerate every change made in Google.
   List each change as a numbered item with:
   - What was changed (quote the original text and the new text)
   - Where it is (section/context)
2. Then, go through your list one by one and apply each change to the CURRENT org file.
   After each change, confirm it was applied.
3. Preserve ALL org-mode syntax: #+PROPERTY headers, src blocks, :PROPERTIES: drawers, etc.
4. Do NOT modify src blocks, results drawers, or org metadata — only update prose and headings.
5. If the same section was edited in both Google and the current org, prefer the Google version.

Output ONLY the final updated org file content, nothing else. No markdown fences."""
    else:
        prompt = f"""I have an org-mode file that was exported to Google Docs/Slides, edited there, and now I need to sync the changes back.

Here is the ORIGINAL org file:
```org
{current_org}
```

Here is the CURRENT content from Google (as markdown/text):
```
{google_content}
```

Your task:
1. First, enumerate every difference between the org file and the Google Doc.
   List each change as a numbered item with:
   - What was changed (quote the original text and the new text)
   - Where it is (section/context)
   Skip differences that are just formatting (org vs markdown syntax).
2. Then, go through your list one by one and apply each content change to the org file.
   After each change, confirm it was applied.
3. Preserve org-mode syntax and structure (#+PROPERTY headers, src blocks, etc.)
4. Only update prose content — do not touch src blocks, results drawers, or metadata.

Output ONLY the final updated org file content, nothing else. No markdown fences."""

    result = subprocess.run(
        ["claude", "-p", prompt],
        capture_output=True,
        text=True,
        check=True,
    )

    updated_content = result.stdout.strip()

    # Write back to org file
    with open(org_path, "w") as f:
        f.write(updated_content)

    return True


def set_pageless(creds, file_id):
    """Set a Google Doc to pageless format."""
    docs_service = build("docs", "v1", credentials=creds)
    docs_service.documents().batchUpdate(
        documentId=file_id,
        body={
            "requests": [
                {
                    "updateDocumentStyle": {
                        "documentStyle": {
                            "documentFormat": {"documentMode": "PAGELESS"}
                        },
                        "fields": "documentFormat",
                    }
                }
            ]
        },
    ).execute()


def cmd_upload(args):
    """Upload file to Google (called from Emacs after export).

    For slides: expects PPTX (pandoc conversion done in Emacs).
    For docs: expects ODT (uploaded directly, preserves embedded images).
    """
    if not os.path.exists(args.input_file):
        print(f"Error: {args.input_file} not found", file=sys.stderr)
        sys.exit(1)

    input_ext = Path(args.input_file).suffix.lower()
    if input_ext not in (".pptx", ".odt"):
        print(f"Error: Unsupported file type {input_ext}", file=sys.stderr)
        sys.exit(1)

    output_format = "slides" if args.slides else "doc"
    mime_info = MIME_TYPES[output_format]
    title = Path(args.input_file).stem

    creds = get_credentials()
    drive_service = build("drive", "v3", credentials=creds)

    if args.file_id:
        file_id = update_existing(
            drive_service, args.file_id, args.input_file, title, output_format
        )
    else:
        file_id = upload_new(drive_service, args.input_file, title, output_format)

    # Set pageless format for docs
    if output_format == "doc":
        try:
            set_pageless(creds, file_id)
        except Exception as e:
            print(f"Warning: could not set pageless format: {e}", file=sys.stderr)

    url = f"{mime_info['url_base']}{file_id}/edit"
    print(f"FILE_ID:{file_id}")
    print(f"URL:{url}")


def cmd_pull(args):
    """Pull changes from Google Doc back to org file.

    Uses three-way merge when possible:
    1. Downloads current Google Doc as markdown
    2. Extracts the commit hash embedded during push
    3. Gets the org file at that commit (common ancestor)
    4. Three-way merges: base org + Google edits → current org
    Falls back to two-way merge if no commit hash found."""
    if not args.file_id:
        print("Error: --id required for pull", file=sys.stderr)
        sys.exit(1)

    if not os.path.exists(args.org_file):
        print(f"Error: {args.org_file} not found", file=sys.stderr)
        sys.exit(1)

    output_format = "slides" if args.slides else "doc"

    # Get credentials and build service
    creds = get_credentials()
    drive_service = build("drive", "v3", credentials=creds)

    # Download content
    print("Downloading from Google...", file=sys.stderr)
    google_content = download_as_text(drive_service, args.file_id, output_format)

    # Try three-way merge using embedded commit hash
    base_org = None
    push_hash = extract_push_hash(google_content)
    if push_hash:
        print(f"Found push commit: {push_hash}", file=sys.stderr)
        base_org = get_org_at_commit(args.org_file, push_hash)
        if base_org:
            print("Using three-way merge (base org at push time + Google edits + current org)", file=sys.stderr)
        else:
            print(f"Warning: could not retrieve org at commit {push_hash}, falling back to two-way merge", file=sys.stderr)
    else:
        print("No push commit hash found in Google Doc, using two-way merge", file=sys.stderr)

    # Update org file with Claude
    print("Updating org file with Claude...", file=sys.stderr)
    if update_org_with_claude(args.org_file, google_content, base_org_content=base_org):
        print("PULL:SUCCESS")
        print(f"Updated: {args.org_file}")
    else:
        print("PULL:FAILED", file=sys.stderr)
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(
        description="Sync org-mode with Google Slides/Docs"
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    # Upload command (takes PPTX for slides, ODT for docs)
    upload_parser = subparsers.add_parser("upload", help="Upload PPTX/ODT to Google")
    upload_parser.add_argument("input_file", help="Path to PPTX or ODT file")
    upload_format = upload_parser.add_mutually_exclusive_group(required=True)
    upload_format.add_argument("--slides", "-s", action="store_true")
    upload_format.add_argument("--doc", "-d", action="store_true")
    upload_parser.add_argument("--id", "-i", dest="file_id", help="Existing file ID")

    # Pull command
    pull_parser = subparsers.add_parser("pull", help="Download Google to org")
    pull_parser.add_argument("org_file", help="Path to org file")
    pull_format = pull_parser.add_mutually_exclusive_group(required=True)
    pull_format.add_argument("--slides", "-s", action="store_true")
    pull_format.add_argument("--doc", "-d", action="store_true")
    pull_parser.add_argument(
        "--id", "-i", dest="file_id", required=True, help="File ID to pull"
    )

    args = parser.parse_args()

    if args.command == "upload":
        cmd_upload(args)
    elif args.command == "pull":
        cmd_pull(args)


if __name__ == "__main__":
    main()
