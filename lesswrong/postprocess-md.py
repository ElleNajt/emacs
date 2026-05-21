"""Post-process org-exported markdown for LessWrong.

Replaces local image paths with GitHub raw URLs,
strips Cell Timer lines, fixes HTML entities from ox-md,
rewrites org internal links to LessWrong-style heading anchors.

Usage: python postprocess-md.py <input.md> <input.org>
Modifies the md file in-place.
"""
import base64
import json
import os
import re
import sys
import subprocess
from html.parser import HTMLParser


class TableParser(HTMLParser):
    """Parse an HTML table into a list of rows (each a list of cell strings)."""
    def __init__(self):
        super().__init__()
        self.rows = []
        self.current_row = None
        self.current_cell = None
        self.in_header = False
        self.header_rows = 0

    def handle_starttag(self, tag, attrs):
        if tag == 'tr':
            self.current_row = []
        elif tag in ('td', 'th'):
            self.current_cell = []
            if tag == 'th':
                self.in_header = True
        elif tag == 'thead':
            self.in_header = True

    def handle_endtag(self, tag):
        if tag in ('td', 'th') and self.current_cell is not None:
            self.current_row.append(''.join(self.current_cell).strip())
            self.current_cell = None
        elif tag == 'tr' and self.current_row is not None:
            if self.current_row:
                self.rows.append(self.current_row)
                if self.in_header:
                    self.header_rows += 1
            self.current_row = None
            self.in_header = False
        elif tag == 'thead':
            self.in_header = False

    def handle_data(self, data):
        if self.current_cell is not None:
            self.current_cell.append(data)


def html_table_to_md(html):
    """Convert a single HTML table string to a markdown table."""
    parser = TableParser()
    parser.feed(html)
    if not parser.rows:
        return html
    header_count = max(parser.header_rows, 1)
    headers = parser.rows[:header_count]
    body = parser.rows[header_count:]
    ncols = max(len(r) for r in parser.rows)
    # Pad rows to same number of columns
    for r in parser.rows:
        while len(r) < ncols:
            r.append('')
    # Column widths
    widths = [max(len(r[i]) for r in parser.rows) for i in range(ncols)]
    widths = [max(w, 3) for w in widths]

    def fmt_row(row):
        cells = [cell.ljust(widths[i]) for i, cell in enumerate(row)]
        return '| ' + ' | '.join(cells) + ' |'

    lines = []
    for row in headers:
        lines.append(fmt_row(row))
    lines.append('|' + '|'.join('-' * (w + 2) for w in widths) + '|')
    for row in body:
        lines.append(fmt_row(row))
    return '\n'.join(lines)


def html_tables_to_markdown(text):
    """Find all HTML tables in text and replace with markdown tables."""
    return re.sub(
        r'<table[^>]*>.*?</table>',
        lambda m: html_table_to_md(m.group(0)),
        text,
        flags=re.DOTALL
    )


def get_repo_info():
    """Derive GitHub repo path and raw URL from git remote."""
    remote = subprocess.check_output(
        ["git", "remote", "get-url", "origin"], text=True
    ).strip()
    # git@github.com:User/Repo.git or https://github.com/User/Repo.git
    m = re.search(r"github\.com[:/](.+?)(?:\.git)?$", remote)
    if m:
        repo_path = m.group(1)
        head = subprocess.check_output(
            ["git", "rev-parse", "HEAD"], text=True
        ).strip()
        raw_url = f"https://raw.githubusercontent.com/{repo_path}/{head}"
        return repo_path, raw_url
    return None, None


def is_repo_private(repo_path):
    """Check if a GitHub repo is private."""
    result = subprocess.check_output(
        ["gh", "repo", "view", repo_path, "--json", "isPrivate"], text=True
    ).strip()
    return json.loads(result)["isPrivate"]


def get_github_username():
    """Get the authenticated GitHub username."""
    return subprocess.check_output(
        ["gh", "api", "user", "--jq", ".login"], text=True
    ).strip()


def ensure_blog_images_repo(username):
    """Create the public blog-images repo if it doesn't exist."""
    repo = f"{username}/blog-images"
    result = subprocess.run(
        ["gh", "repo", "view", repo, "--json", "name"],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print(f"Creating public repo {repo}...", file=sys.stderr)
        subprocess.check_call([
            "gh", "repo", "create", repo, "--public",
        ])
        # Initialize with a README so main branch exists
        subprocess.check_call([
            "gh", "api", f"repos/{repo}/contents/README.md",
            "--method", "PUT",
            "--field", "message=Initial commit",
            "--field", f"content={base64.b64encode(b'# blog-images\n').decode()}",
        ])
    return repo


def upload_image_to_blog_images(local_path, remote_path, images_repo):
    """Upload an image to the blog-images repo via GitHub API.

    Returns the raw URL. Skips upload if file already exists.
    """
    with open(local_path, "rb") as f:
        content_b64 = base64.b64encode(f.read()).decode()

    existing = subprocess.run(
        ["gh", "api", f"repos/{images_repo}/contents/{remote_path}",
         "--jq", ".sha"],
        capture_output=True, text=True
    )
    if existing.returncode == 0 and existing.stdout.strip():
        print(f"  {remote_path} already exists, skipping", file=sys.stderr)
    else:
        print(f"  Uploading {remote_path}...", file=sys.stderr)
        subprocess.check_call([
            "gh", "api", f"repos/{images_repo}/contents/{remote_path}",
            "--method", "PUT",
            "--field", f"message=Add {remote_path}",
            "--field", f"content={content_b64}",
        ])

    return f"https://raw.githubusercontent.com/{images_repo}/main/{remote_path}"


def get_git_commit_link(repo_path, md_path):
    """Build a markdown footer linking to the source org file at the current commit."""
    hash = subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip()
    short = hash[:7]
    toplevel = subprocess.check_output(
        ["git", "rev-parse", "--show-toplevel"], text=True
    ).strip()
    # The org file is the md file with .org extension
    org_file = re.sub(r"\.md$", ".org", os.path.abspath(md_path))
    rel = os.path.relpath(org_file, toplevel)
    url = f"https://github.com/{repo_path}/blob/{hash}/{rel}"
    org_name = os.path.basename(org_file)
    return f"\n---\n\n*Generated from [{org_name}]({url}) ({short})*\n"


def title_to_anchor(title, used_anchors=None):
    """Convert heading text to LessWrong anchor ID.

    Matches ForumMagnum's titleToAnchor: keep a-zA-Z0-9_, replace
    everything else with _, deduplicate with numeric suffix.
    """
    if used_anchors is None:
        used_anchors = set()
    allowed = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789")
    anchor = ''.join(ch if ch in allowed else '_' for ch in title)
    if anchor not in used_anchors and anchor not in ("top", "comments"):
        used_anchors.add(anchor)
        return anchor
    suffix = 1
    while f"{anchor}{suffix}" in used_anchors:
        suffix += 1
    result = f"{anchor}{suffix}"
    used_anchors.add(result)
    return result


def build_org_link_map(org_path):
    """Parse org file to map display text -> heading text for internal links.

    Finds [[*Heading Text][display text]] patterns and returns
    {display_text: heading_text}.
    """
    with open(org_path) as f:
        org_text = f.read()
    # Match [[*Heading][display]] - org internal heading links
    link_map = {}
    for m in re.finditer(r'\[\[\*([^\]]+)\]\[([^\]]+)\]\]', org_text):
        heading = m.group(1)
        display = m.group(2)
        link_map[display] = heading
    return link_map


def rewrite_org_anchors(text, org_path):
    """Replace #orgXXXXXXX anchors with LessWrong-style heading anchors."""
    if not org_path:
        return text
    link_map = build_org_link_map(org_path)
    if not link_map:
        return text

    # Build heading -> LW anchor map from all markdown headings (in order,
    # for deduplication)
    used_anchors = set()
    heading_anchors = {}
    for m in re.finditer(r'^#+\s+(.+)$', text, re.MULTILINE):
        heading = m.group(1).strip()
        heading_anchors[heading] = title_to_anchor(heading, used_anchors)

    # Normalize quotes for matching (ox-md converts ASCII quotes to smart quotes)
    quote_table = str.maketrans({
        '\u201c': '"', '\u201d': '"',
        '\u2018': "'", '\u2019': "'",
    })

    def replace_link(m):
        display = m.group(1)
        display_normalized = display.translate(quote_table)
        lookup = display_normalized if display_normalized in link_map else display
        if lookup not in link_map:
            return m.group(0)
        heading = link_map[lookup]
        # Find the LW anchor - try exact match first, then fuzzy
        if heading in heading_anchors:
            return f'[{display}](#{heading_anchors[heading]})'
        # Try stripping trailing punctuation (org headings may differ slightly)
        heading_stripped = heading.rstrip('?:!')
        for h, anchor in heading_anchors.items():
            if h.rstrip('?:!') == heading_stripped:
                return f'[{display}](#{anchor})'
        return m.group(0)

    return re.sub(r'\[([^\]]+)\]\(#org[a-f0-9]+\)', replace_link, text)


def postprocess(md_path, repo_raw, repo_path=None, org_path=None,
                private=False, images_repo=None):
    with open(md_path) as f:
        text = f.read()

    md_dir = os.path.dirname(os.path.abspath(md_path))
    toplevel = subprocess.check_output(
        ["git", "rev-parse", "--show-toplevel"], text=True
    ).strip()
    rel_dir = os.path.relpath(md_dir, toplevel)

    # Replace local image paths with public URLs
    def rewrite_img(m):
        path = m.group(1)
        if path.startswith("http"):
            return m.group(0)
        # Resolve relative path from md file, then make relative to repo root
        local = os.path.normpath(os.path.join(md_dir, path))
        repo_rel = os.path.relpath(local, toplevel)
        if private and images_repo:
            repo_name = repo_path.split("/")[-1] if repo_path else "unknown"
            remote = f"{repo_name}/{repo_rel}"
            return f"![img]({upload_image_to_blog_images(local, remote, images_repo)})"
        return f"![img]({repo_raw}/{repo_rel})"

    text = re.sub(r"!\[img\]\(([^)]+)\)", rewrite_img, text)

    # Strip Cell Timer lines
    text = re.sub(r"^Cell Timer:.*\n", "", text, flags=re.MULTILINE)

    # Fix HTML entities from org export
    entities = {
        "&rsquo;": "\u2019",
        "&lsquo;": "\u2018",
        "&rdquo;": "\u201d",
        "&ldquo;": "\u201c",
        "&ndash;": "\u2013",
        "&mdash;": "\u2014",
        "&hellip;": "\u2026",
    }
    for entity, char in entities.items():
        text = text.replace(entity, char)

    # Convert HTML tables to markdown tables
    text = html_tables_to_markdown(text)

    # Rewrite org internal links to LessWrong-style anchors
    text = rewrite_org_anchors(text, org_path)

    # Remove escaped underscores (org export artifact)
    text = text.replace("\\_", "_")

    # Clean up excessive blank lines
    text = re.sub(r"\n{4,}", "\n\n\n", text)

    # Append git commit link
    if repo_path:
        text += get_git_commit_link(repo_path, md_path)

    with open(md_path, "w") as f:
        f.write(text)


if __name__ == "__main__":
    md_path = sys.argv[1]
    org_path = sys.argv[2] if len(sys.argv) > 2 else None
    repo_path, repo_raw = get_repo_info()
    if not repo_raw:
        print("Could not determine GitHub raw URL from git remote.", file=sys.stderr)
        sys.exit(1)

    private = is_repo_private(repo_path)
    images_repo = None
    if private:
        username = get_github_username()
        images_repo = ensure_blog_images_repo(username)
        print(f"Private repo detected, images will go to {images_repo}",
              file=sys.stderr)

    postprocess(md_path, repo_raw, repo_path, org_path,
                private=private, images_repo=images_repo)
