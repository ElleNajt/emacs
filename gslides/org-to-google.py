#!/Users/elle/.doom.d/gslides/.venv/bin/python3
"""
Sync org-mode files with Google Slides or Google Docs via pandoc.

Usage:
    org-to-google.py push <org-file> [--slides | --doc] [--id ID]
    org-to-google.py pull <org-file> --id ID [--slides | --doc]

Push: Upload org file to Google Slides/Docs
Pull: Download Google Slides/Docs, convert to markdown, use Claude to update org file

Examples:
    org-to-google.py push notes.org --slides              # Create new presentation
    org-to-google.py push notes.org --slides --id ABC123  # Update existing
    org-to-google.py pull notes.org --slides --id ABC123  # Pull changes back to org
"""

import argparse
import io
import os
import subprocess
import sys
from pathlib import Path

from google.auth.transport.requests import Request
from google.oauth2.credentials import Credentials
from google_auth_oauthlib.flow import InstalledAppFlow
from googleapiclient.discovery import build
from googleapiclient.http import MediaFileUpload, MediaIoBaseDownload

SCOPES = [
    "https://www.googleapis.com/auth/drive.file",
    "https://www.googleapis.com/auth/drive.readonly",
]

SECRETS_DIR = Path.home() / ".doom.d" / "secrets"
CREDENTIALS_FILE = SECRETS_DIR / "gslides-oauth-credentials.json"
TOKEN_FILE = SECRETS_DIR / "gslides-oauth-token.json"

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
        "office": "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "extension": ".docx",
        "export_text": "text/markdown",
        "url_base": "https://docs.google.com/document/d/",
    },
}


def get_credentials():
    """Get or refresh Google API credentials."""
    creds = None

    if TOKEN_FILE.exists():
        creds = Credentials.from_authorized_user_file(str(TOKEN_FILE), SCOPES)

    if not creds or not creds.valid:
        if creds and creds.expired and creds.refresh_token:
            creds.refresh(Request())
        else:
            if not CREDENTIALS_FILE.exists():
                print(
                    f"Error: {CREDENTIALS_FILE} not found.\n"
                    "Download OAuth credentials from Google Cloud Console:\n"
                    "1. Go to https://console.cloud.google.com/apis/credentials\n"
                    "2. Create OAuth 2.0 Client ID (Desktop app)\n"
                    f"3. Download JSON and save as {CREDENTIALS_FILE}",
                    file=sys.stderr,
                )
                sys.exit(1)

            flow = InstalledAppFlow.from_client_secrets_file(
                str(CREDENTIALS_FILE), SCOPES
            )
            creds = flow.run_local_server(port=0)

        with open(TOKEN_FILE, "w") as token:
            token.write(creds.to_json())

    return creds


def convert_odt_to_office(odt_path, output_format):
    """Convert ODT to Office format (PPTX/DOCX) using pandoc."""
    mime_info = MIME_TYPES[output_format]
    office_path = odt_path.rsplit(".", 1)[0] + mime_info["extension"]

    try:
        subprocess.run(
            ["pandoc", odt_path, "-o", office_path],
            check=True,
            capture_output=True,
        )
    except subprocess.CalledProcessError as e:
        print(f"Pandoc conversion failed: {e.stderr}", file=sys.stderr)
        raise

    return office_path


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
    """Update existing Google Slides/Docs by replacing it.

    Google doesn't support converting uploads to update existing files,
    so we delete the old file and create a new one.
    """
    mime_info = MIME_TYPES[output_format]

    # Get the old file's parent folder(s) to preserve location
    old_file = drive_service.files().get(fileId=file_id, fields="parents").execute()
    parents = old_file.get("parents", [])

    # Delete the old file
    drive_service.files().delete(fileId=file_id).execute()

    # Create new file with same parent(s)
    file_metadata = {
        "name": title,
        "mimeType": mime_info["google"],
    }
    if parents:
        file_metadata["parents"] = parents

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


def update_org_with_claude(org_path, google_content):
    """Use Claude to update org file based on Google content."""
    with open(org_path, "r") as f:
        org_content = f.read()

    prompt = f"""I have an org-mode file that was exported to Google Docs/Slides, edited there, and now I need to sync the changes back.

Here is the ORIGINAL org file:
```org
{org_content}
```

Here is the CURRENT content from Google (as markdown/text):
```
{google_content}
```

Please update the org file to reflect changes made in Google while:
1. Preserving org-mode syntax and structure (#+title:, #+GSLIDES_ID:, #+GDOC_ID:, etc.)
2. Keeping any org-specific features (properties, TODO states, etc.)
3. Updating the actual content (text, headings, bullet points) to match Google

Output ONLY the updated org file content, nothing else."""

    # Write prompt to temp file
    with tempfile.NamedTemporaryFile(mode="w", suffix=".txt", delete=False) as tmp:
        tmp.write(prompt)
        prompt_path = tmp.name

    try:
        # Call claude with the prompt
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

    except subprocess.CalledProcessError as e:
        print(f"Claude error: {e.stderr}", file=sys.stderr)
        return False
    except FileNotFoundError:
        print("Error: claude CLI not found", file=sys.stderr)
        return False
    finally:
        os.unlink(prompt_path)


def cmd_upload(args):
    """Upload file to Google (called from Emacs after export).

    For slides: expects PPTX file (pandoc conversion done in Emacs)
    For docs: expects ODT file, converts ODT -> DOCX via pandoc
    """
    if not os.path.exists(args.input_file):
        print(f"Error: {args.input_file} not found", file=sys.stderr)
        sys.exit(1)

    output_format = "slides" if args.slides else "doc"
    mime_info = MIME_TYPES[output_format]

    # Get title from input filename
    title = Path(args.input_file).stem
    input_ext = Path(args.input_file).suffix.lower()

    # Determine if we need to convert
    if input_ext == ".pptx":
        # PPTX ready for upload (slides from pandoc)
        office_path = args.input_file
        needs_cleanup = False
    elif input_ext == ".odt":
        # ODT needs conversion to DOCX (docs)
        office_path = convert_odt_to_office(args.input_file, output_format)
        os.unlink(args.input_file)
        needs_cleanup = True
    else:
        print(f"Error: Unsupported file type {input_ext}", file=sys.stderr)
        sys.exit(1)

    creds = get_credentials()
    drive_service = build("drive", "v3", credentials=creds)

    try:
        if args.file_id:
            file_id = update_existing(
                drive_service, args.file_id, office_path, title, output_format
            )
        else:
            file_id = upload_new(drive_service, office_path, title, output_format)
    finally:
        # Clean up Office file if we created it
        if needs_cleanup and os.path.exists(office_path):
            os.unlink(office_path)

    url = f"{mime_info['url_base']}{file_id}/edit"
    print(f"FILE_ID:{file_id}")
    print(f"URL:{url}")


def cmd_pull(args):
    """Handle pull command."""
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

    # Update org file with Claude
    print("Updating org file with Claude...", file=sys.stderr)
    if update_org_with_claude(args.org_file, google_content):
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
