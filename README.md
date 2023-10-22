# Angular Mode

Angular Mode offers an interactive API for Angular CLI commands through a dedicated keymap.

## Keybindings

| Keybinding     | Command Description                    |
|-----------------|----------------------------------------|
| `C-c a g c`     | Generate an Angular component.        |
| `C-c a g s`     | Generate an Angular service.          |
| `C-c a g d`     | Generate an Angular directive.        |
| `C-c a g e`     | Generate an Angular enum.             |
| `C-c a g n`     | Generate and configure environment files for a project. |
| `C-c a g g`     | Generate an Angular route guard.      |
| `C-c a g i`     | Generate an Angular interceptor.       |
| `C-c a g l`     | Create a new Angular library project.  |
| `C-c a g m`     | Generate an Angular NgModule definition. |
| `C-c a g p`     | Generate an Angular pipe.             |
| `C-c a g r`     | Generate an Angular resolver.         |
| `C-c a g x`     | Create an Angular service worker.     |
| `C-c a g w`     | Generate an Angular web worker.        |

**Note**: At the moment, Angular Mode supports only the `generate` commands.

## Getting Started

To use this package, activate `angular-mode` and leverage the provided keybindings. For example, use **C-c a g c** to generate an Angular component interactively.

**Note**: When running a generate command, this will generate the component, service, etc. in the current directory of the active buffer. I'm looking to improve on this workflow. In the meantime, I suggest using Dired to navigate to the directory, active `angular-mode` and run the generate command of choice.
