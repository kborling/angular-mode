# Angular Mode

Angular Mode enhances your workflow by providing an interactive API for Angular CLI commands and allows you to quickly open schematic files such as components, services, and more through a dedicated keymap. Additionally, you can conveniently jump between corresponding component files, such as templates, components, tests, and stylesheets.

## Keybindings

| Keybinding  | Command Description                                                  | Command                             |
|-------------|----------------------------------------------------------------------|-------------------------------------|
| `C-c a g`   | Generate an Angular Schematic.                                       | `angular-generate`                  |
| `C-c a o c` | Open a `component.ts` file.                                          | `angular-open-component`            |
| `C-c a o t` | Open a `component.html` file.                                        | `angular-open-component-template`   |
| `C-c a o v` | Open a `component.(scss\|sass\|less\|css)` file.                     | `angular-open-component-stylesheet` |
| `C-c a o x` | Open a `component.spec.ts` file.                                     | `angular-open-component-test`       |
| `C-c a o d` | Open a `directive.ts` file.                                          | `angular-open-directive`            |
| `C-c a o g` | Open a `guard.ts` file.                                              | `angular-open-guard`                |
| `C-c a o i` | Open a `interceptor.ts` file.                                        | `angular-open-interceptor`          |
| `C-c a o m` | Open a `module.ts` file.                                             | `angular-open-module`               |
| `C-c a o p` | Open a `pipe.ts` file.                                               | `angular-open-pipe`                 |
| `C-c a o r` | Open a `resolver.ts` file.                                           | `angular-open-resolver`             |
| `C-c a o s` | Open a `service.ts` file.                                            | `angular-open-service`              |
| `C-c a o w` | Open a `worker.ts` file.                                             | `angular-open-web-worker`           |
| `C-c a j c` | Jump to corresponding `component.ts` file.                           | `angular-jump-to-component`         |
| `C-c a j t` | Jump to corresponding `component.html` file.                         | `angular-jump-to-template`          |
| `C-c a j v` | Jump to corresponding `component.(scss\|sass\|less\|css)` file.      | `angular-jump-to-stylesheet`        |
| `C-c a j x` | Jump to corresponding `component.spec.ts` file.                      | `angular-jump-to-test`              |
| `C-c a p`   | Open the project's `angular.json` file.                              | `angular-project-config`            |
| `C-c a h d` | Lookup current word at point in API reference documentation website. | `angular-lookup-word`               |
| `C-c a h s` | Perform a search of angular.io using the current word at point.      | `angular-search-word`               |

**Note**: At the moment, Angular Mode only supports the `generate` commands.

## Getting Started

To utilize this package, activate `angular-mode` to access the keybinding for generating schematics within your Angular project. Additionally, you can use `M-x angular-generate` to generate schematics.

### Generate Angular Schematic

Using the `C-c a g` command, you can perform the following:

- Choose a schematic from the list.
- Specify a name for the selected schematic.
- Pick a destination directory for the generated schematic.

### Quick Open Schematics

Angular Mode offers convenient shortcuts to quickly open files for various Angular schematics by using the `C-c a o` prefix. This will provide a filtered list of files based on the schematic type. See the list of corresponding [keybindings](#keybindings).

### Jump Between Corresponding Files

Angular Mode provides an easy way to help you jump between associated Angular files with ease. Whether you're working with components, services, or other Angular schematics, you can quickly switch between the following file types:

- **Template**: Jump to the associated `.html` file.
- **Component**: Jump to the associated `.ts` file.
- **Test**: Jump to the associated `.spec.ts` file.
- **Stylesheet**: Jump to the associated `.scss`, `.less`, `.sass`, or `.css` file

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
