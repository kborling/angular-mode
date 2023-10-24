# Angular Mode

Angular Mode offers an interactive API for Angular CLI commands through a dedicated keymap.

## Keybindings

| Keybinding  | Command Description                     | Command                    |
|-------------|-----------------------------------------|----------------------------|
| `C-c a g`   | Generate an Angular Schematic.          | `angular-generate`         |
| `C-c a p`   | Open the project's `angular.json` file. | `angular-project-config`   |
| `C-c a o c` | Open a `component.ts` file.             | `angular-open-component`   |
| `C-c a o d` | Open a `directive.ts` file.             | `angular-open-directive`   |
| `C-c a o g` | Open a `guard.ts` file.                 | `angular-open-guard`       |
| `C-c a o i` | Open a `interceptor.ts` file.           | `angular-open-interceptor` |
| `C-c a o m` | Open a `module.ts` file.                | `angular-open-module`      |
| `C-c a o p` | Open a `pipe.ts` file.                  | `angular-open-pipe`        |
| `C-c a o r` | Open a `resolver.ts` file.              | `angular-open-resolver`    |
| `C-c a o s` | Open a `service.ts` file.               | `angular-open-service`     |
| `C-c a o w` | Open a `worker.ts` file.                | `angular-open-web-worker`  |

**Note**: At the moment, Angular Mode only supports the `generate` commands.

## Getting Started

To utilize this package, activate `angular-mode` to access the keybinding for generating schematics within your Angular project. Additionally, you can use `M-x angular-generate` to generate schematics.

### Generate Angular Schematic

Using the `C-c a g` command, you can perform the following:

- Choose a schematic from the list.
- Specify a name for the selected schematic.
- Pick a destination directory for the generated schematic.

## Installation

You can install `angular-mode` manually or via a package manager.

### Manual Installation

1. Clone or download this repository to your local machine.

   ```sh
   git clone https://github.com/kborling/angular-mode.git
   ```

2. Add the following to your Emacs configuration file:

   ```elisp
   (add-to-list 'load-path "/path/to/angular-mode")
   (require 'angular-mode)
   ```

## Install Using a Package Manager
You can install angular-mode using popular package managers such as Straight and Elpaca. Choose the method that suits your preference:

### Straight
```elisp
(straight-use-package
  '(angular-mode :type git :host github :repo "kborling/angular-mode"))
;; Using the use-package integration
(use-package angular-mode
  :straight (angular-mode :host github :repo "kborling/angular-mode" :files ("*.el")))
```

### Elpaca
```elisp
(use-package angular-mode
  :elpaca (angular-mode :host github :repo "kborling/angular-mode" :files ("*.el")))
```

### Doom Emacs
```elisp
(package! angular-mode
  :recipe (:host github :repo "kborling/angular-mode"))
```

### Spacemacs
```elisp
(angular-mode :location (recipe :fetcher github :repo "kborling/angular-mode"))
```

## Configuration

### Setting `angular-cli` executable

`angular-mode` assumes the Angular CLI tool is installed globally. If that's not the case, set the `angular-cli-executable`:

```elisp
(setq angular-cli-executable "/path/to/ng/command")
```

### Global Activation
To activate `angular-mode` globally, add the following:
```elisp
(global-angular-mode 1)
```

### Project Setup
To activate `angular-mode` automatically for a specific project, follow these steps:

1. In your project root directory, create a `.dir-locals.el` file if it doesn't already exist.

2. Open the `.dir-locals.el` file and add the following:

```elisp
((nil . ((mode . angular))))
```

3. Save the `.dir-locals.el` file.

This will automatically activate `angular-mode` when visiting any file within the project.
