;;; angular-mode.el --- Emacs Minor Mode for Working with Angular CLI
;;
;; This package facilitates interaction with Angular projects by providing a minor mode.
;; It offers an interactive API for Angular CLI commands through a dedicated keymap.
;;
;; Keybindings:
;;   - C-c a g: Generate an Angular schematic.
;;
;; To use this package, activate `angular-mode` and leverage the provided keybindings
;; to generate schematics in the project directory of choice.
;;
;; Copyright (C) 2023 Kevin Borling <kborling@protonmail.com>
;;
;; Author: Kevin Borling <kborling@protonmail.com>
;; Created: October 16, 2023
;; Version: 0.0.1
;; Keywords: angular, angular-cli, angular-mode
;; License: MIT
;; Keywords: custom themes, dark, faces
;; URL: https://github.com/kborling/angular-mode
;; Homepage: https://github.com/kborling/angular-mode
;; Filename: angular-mode.el
;; Package-Requires: ((emacs "24.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; This package simplifies working with Angular projects in Emacs. It offers a minor
;; mode for convenient interactive use of Angular CLI commands. The provided keymap
;; makes it easy to generate components, services, directives, and more with minimal
;; effort.
;;
;; To use this package, copy it to your Emacs configuration, evaluate the code, and
;; activate the `angular-mode` minor mode. You can then use the specified keybindings
;; to streamline your Angular project development.
;;
;; To customize the Angular CLI executable or add more commands, please refer to
;; the package documentation for instructions.
;;
;; Enjoy working with Angular in Emacs!
;;
;;; Code:
(define-minor-mode angular-mode
  "Minor mode for working with Angular CLI."
  :lighter " Angular"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a g") 'angular-generate)
            map))

;; TODO: Allow this to be customized.
(defvar angular-cli-executable
  (if (executable-find "npx")
      "npx ng"
    "ng")
  "Path to the Angular CLI executable.")

(defun angular-generate (schematic name)
  "Generate an Angular 'SCHEMATIC' called 'NAME' in the directory of choice."
  (interactive (list (completing-read "Schematic: "
                                      '("component" "service" "directive" "enum" "environments" "guard" "interceptor" "library" "module" "pipe" "resolver" "service-worker" "web-worker"))
                     (read-string "Name: ")))
  (let* ((selected-directory (file-name-as-directory (expand-file-name (read-directory-name "Select a directory: "))))
         (current-directory (file-name-directory default-directory))
         (relative-path (file-relative-name selected-directory current-directory))
         (directory (if (string-prefix-p "/" relative-path)
                        relative-path
                      (concat (if (not (string-suffix-p "/" relative-path)) "../")
                              relative-path))))
    (setq directory (concat directory name))
    (shell-command (format "%s generate %s %s " angular-cli-executable schematic directory))))

(provide 'angular-mode)
;;; angular-mode.el ends here
