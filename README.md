# Angular Mode

Emacs minor mode for Angular projects. Fast navigation, schematic generation, refactoring, namespace management, and CLI integration.

Auto-activates in projects with `angular.json`. Includes Angular Language Server setup for eglot.

## Install

```elisp
(use-package angular-mode
  :vc (:url "https://github.com/kborling/angular-mode" :rev :newest)
  :hook (find-file . angular-mode-maybe))
```

## Usage

`C-c a a` opens the transient menu. Key commands:

| Key | Action |
|-----|--------|
| `C-c a TAB` | Cycle between ts/html/scss/spec |
| `C-c a g` | Generate schematic |
| `C-c a r` | Rename component |
| `C-c a S` | Convert to standalone |
| `C-c a m d/f` | Move directory/file (updates imports) |
| `C-c a o *` | Open by type (component, service, etc.) |
| `C-c a j *` | Jump to associated file |
| `C-c a P` | Open routes file |
| `C-c a n f` | New feature (barrel + alias) |
| `C-c a n b/B` | Create barrel / recursive barrels |
| `C-c a n @` | Convert to alias imports |
| `C-c a R s/b/t/l` | Serve / Build / Test / Lint |
| `M-.` | Jump to component from selector |

## Features

- **Cycle** (`TAB`) — single key to rotate between component parts
- **Generate** — interactive schematic with directory picker and options (`--skip-tests`, `--inline-style`, etc.)
- **Move** — moves all associated files together, updates static and dynamic imports (including lazy routes)
- **Namespace** — create barrels (`index.ts`), path aliases (`@shared/*`), convert imports
- **Standalone migration** — adds `standalone: true`, removes from NgModule declarations
- **LSP** — auto-configures Angular Language Server for eglot on `.component.html` files
- **Performance** — uses `fd`/`rg` when available, falls back to built-in

## Configuration

```elisp
(setq angular-cli-executable "/path/to/ng")  ; default: auto-detects npx/ng
(setq angular-import-path-style 'absolute)    ; default: 'relative
```

## LSP Requirements

```sh
npm install -g @angular/language-server typescript
```
