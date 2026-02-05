# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Doom Emacs configuration directory containing a highly customized setup with extensive language support, AI integration, and specialized workflows.

## Key Configuration Files

- `init.el` - Doom module configuration (what modules are enabled)
- `config.el` - Main configuration file with keybindings, language settings, and customizations
- `packages.el` - Package declarations and custom package sources
- `custom.el` - Emacs custom variables (auto-generated)
- `computers/` - Host-specific configuration files loaded based on hostname
- `experimental/` - Testing and experimental configurations
- `vendored/` - Custom packages and modifications

## Architecture

### Module System
The configuration uses Doom Emacs modules defined in `init.el`. Key enabled modules include:
- Completion: `corfu` for auto-completion, `vertico` for minibuffer completion
- UI: `doom`, `doom-dashboard`, `modeline`, `workspaces`
- Editor: `evil` (vim emulation), `snippets`, `format +onsave`
- Languages: Python, Clojure, Rust, Haskell, LaTeX, Nix, JavaScript, and more
- Tools: `magit`, `lsp +eglot`, `debugger`, `docker`, `terraform`

### Host-Specific Configuration
Configuration automatically loads host-specific files from `computers/` directory:
```elisp
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname"))))
```

### Language Support
Extensive language support with LSP integration via eglot:
- **Python**: Pyright LSP, ruff formatting, vterm integration for debugging/running
- **Clojure**: CIDER REPL, clojure-lsp, zprint formatting, cleverparens
- **Rust**: rust-analyzer, cargo integration
- **Nix**: nixd LSP server
- **Haskell**: haskell-language-server
- **LaTeX**: AUCTeX with cdlatex, org-fragtog for preview

### AI Integration
Multiple AI tools configured:
- **GPTel**: ChatGPT integration with custom prompts and Lovecraftian theming
- **Aider**: AI coding assistant via aidermacs package
- **MCP (Model Context Protocol)**: Tool integration for AI models

### Org Mode Enhancements
Heavily customized Org mode setup:
- Custom agenda commands and weekly review
- Babel support for multiple languages (Python, Clojure, Haskell, TypeScript)
- Custom capture templates for todos and journal entries
- Time tracking with custom i3 status integration
- LaTeX preview with org-fragtog
- Custom evil motions and keybindings

## Development Workflows

### Running Code
- **Python**: `SPC m e r` to run file, `SPC m d` for debugging
- **Rust**: Custom functions `rust/run` and `rust/check` 
- **Clojure**: CIDER evaluation with `cpp`, `cpc`, `cpd` bindings

### Formatting
- Auto-formatting on save enabled for most languages
- Python: ruff with isort integration
- Clojure: zprint formatting

### Git Integration
- Enhanced magit with custom commands for large repos (nixpkgs optimization)
- Custom functions for GitHub permalinks and branch links
- Smerge keybindings for conflict resolution: `SPC [ n` / `SPC ] n`

### Testing
No specific test commands configured - check individual project READMEs or use language-specific test runners.

## Key Keybindings

### Custom Leader Bindings
- `SPC a` - org-capture
- `SPC r` - eglot-rename  
- `SPC o g g` - gptel (AI chat)
- `SPC o g q` - gptel-lookup (quick AI query)
- `SPC d a` - envrc-allow
- `SPC d r` - envrc-reload

### Navigation
- `s` - evil-avy-goto-char-2 (quick jump)
- `-` - dired parent directory navigation
- `] e` / `[ e` - next/previous flycheck error
- `] c` / `[ c` - next/previous spell error (org-mode)

### Outline Navigation (prog-mode)
- `z TAB` - outline-cycle
- `z j/k` - next/previous heading
- `z J/K` - forward/backward same level
- `z u` - up heading
- `z n` - narrow to subtree

## Special Features

### Computer-Specific Loading
Each machine can have custom configuration in `computers/HOSTNAME.el` and `computers/HOSTNAME-packages.el`.

### Org Code TODOs
Automatic collection of code TODOs into org files via `org-collect-code-todos`.

### Smooth Scrolling
Pixel-perfect scrolling enabled in org-mode with custom mouse wheel settings.

### Directory as Text
Custom function to concatenate directory contents: `SPC d c` in dired.

## Package Management

Uses Doom's package system with many custom packages from GitHub:
- Custom packages: `gpt-babel`, `ob-python-extras`, `oneko-macs`, `org-collect-code-todos`
- External integrations: `aidermacs`, `mcp-el`, `claude-code`

## Notes

- Tree-sitter is disabled (`(setq tree-sitter-mode nil)`)
- Flymake is disabled in favor of flycheck
- Large repo optimizations for magit (especially nixpkgs)
- Custom org agenda optimizations with envrc disabled
