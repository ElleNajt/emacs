# org-gslides

Export org-mode files to Google Slides.

## Setup

### 1. Install Python dependencies

```bash
pip install -r ~/.doom.d/gslides/requirements.txt
```

### 2. Create Google Cloud credentials

1. Go to https://console.cloud.google.com/apis/credentials
2. Create a new project (or use existing)
3. Enable the Google Slides API and Google Drive API
4. Create OAuth 2.0 Client ID (choose "Desktop app")
5. Download the JSON file
6. Save as `~/.doom.d/gslides/credentials.json`

### 3. Reload Emacs config

```
M-x doom/reload
```

## Usage

In any org-mode buffer:

- `M-x org-gslides-export` - Export to Google Slides
  - First run: Creates new presentation, saves ID to file
  - Subsequent runs: Updates existing presentation

- `M-x org-gslides-export-new` - Always create a NEW presentation

The presentation ID is stored in your org file as:
```org
#+GSLIDES_ID: abc123xyz
```

## How it maps

- `#+title:` → Presentation title (and title slide)
- `* Headline` → New slide with that title
- `** Subheadline` → New slide (nested structure flattened)
- List items → Bullet points in slide body
- Paragraphs → Body text
- Code blocks → "[code block]" placeholder (not rendered as code)

## First run

On first export, a browser window will open for Google OAuth.
Grant permissions and the token will be saved for future use.
