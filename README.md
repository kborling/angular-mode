# Angular Mode

Angular Mode offers an interactive API for Angular CLI commands through a dedicated keymap.

## Keybindings

| Keybinding | Command Description            | Command            |
|------------|--------------------------------|--------------------|
| `C-c a g`  | Generate an Angular Schematic. | `angular-generate` |

**Note**: At the moment, Angular Mode only supports the `generate` commands.

## Getting Started

To utilize this package, activate `angular-mode` to access the keybinding for generating schematics within your Angular project. Additionally, you can use `M-x angular-generate` to generate schematics.

### Generate Angular Schematic

Using the `C-c a g` command, you can perform the following:

- Choose a schematic from the list.
- Specify a name for the selected schematic.
- Pick a destination directory for the generated schematic.
