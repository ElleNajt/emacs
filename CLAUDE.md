# CLAUDE.md

This is a Doom Emacs configuration directory.

## Key Files

- `init.el` - Doom module configuration
- `config.el` - Main configuration (keybindings, languages, customizations)
- `packages.el` - Package declarations
- `computers/` - Host-specific config loaded via hostname

## AI Integration

- **agent-shell** / **acp**: Claude Code integration via xenodium's packages
- **meta-agent-shell**: Multi-agent coordination (`SPC o m`)
- **agent-shell-to-go**: Send images to Slack threads
- **gptel**: ChatGPT integration (`SPC o g g` for chat, `SPC o g q` for quick lookup)

## Key Keybindings

### Leader Bindings
- `SPC a` - org-capture
- `SPC r` - eglot-rename
- `SPC o t` - eshell (primary terminal)
- `SPC o c` - claude/agent-shell
- `SPC o m` - meta-agent-shell commands
- `SPC o g g` - gptel chat
- `SPC o g q` - gptel quick lookup
- `SPC d v` - vterm cd to dired dir
- `SPC d a` - envrc-allow
- `SPC d r` - envrc-reload

### Navigation
- `s` - evil-avy-goto-char-2
- `-` - dired parent directory
- `] e` / `[ e` - next/previous flycheck error
- `] c` / `[ c` - next/previous spell error (org-mode)
- `s-b` - toggle agent-shell-manager

### Outline (prog-mode)
- `z TAB` - outline-cycle
- `z j/k` - next/previous heading
- `z n` - narrow to subtree

## Language Support

- **Python**: Pyright LSP, ruff formatting, vterm for debugging
- **Clojure**: CIDER, clojure-lsp, zprint formatting (`cpp`/`cpc`/`cpd` for eval)
- **Rust**: rust-analyzer (`rust/run`, `rust/check`)
- **Nix**: nixd LSP
- **Haskell**: haskell-language-server

## Org Mode

- ob-python-extras for enhanced Python babel
- Custom agenda with weekly review (`W`)
- Capture templates: `a`/`t` (todo), `j` (journal), `n` (note)
- LaTeX preview enabled

## Notes

- Primary terminal is eshell (`SPC o t`)
- Tree-sitter disabled
- Flymake disabled (uses flycheck)
- Server watchdog restarts server if it dies
