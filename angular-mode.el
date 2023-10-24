;;; angular-mode.el --- Emacs Minor Mode for Working with Angular CLI
;;
;; This package facilitates interaction with Angular projects by providing a minor mode.
;; It offers an interactive API for Angular CLI commands through a dedicated keymap.
;;
;; Keybindings:
;;   - C-c a g: Generate an Angular schematic.
;;   - C-c a p: Open the project's angular.json file.
;;   - C-c a o c: Open a component.ts file.
;;   - C-c a o d: Open a directive.ts file.
;;   - C-c a o g: Open a guard.ts file.
;;   - C-c a o i: Open a interceptor.ts file.
;;   - C-c a o m: Open a module.ts file.
;;   - C-c a o p: Open a pipe.ts file.
;;   - C-c a o r: Open a resolver.ts file.
;;   - C-c a o s: Open a service.ts file.
;;   - C-c a o w: Open a worker.ts file.
;;
;; To use this package, activate `angular-mode` and leverage the provided keybindings
;; to generate schematics in the project directory of choice.
;;
;;
;; Copyright (C) 2023 Kevin Borling <kborling@protonmail.com>
;;
;; Author: Kevin Borling <kborling@protonmail.com>
;; Created: October 16, 2023
;; Version: 0.0.2
;; Keywords: angular, angular-cli, angular-mode
;; License: MIT
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

(defgroup angular-mode nil
  "Angular mode, a minor mode for interacting with the Angular CLI."
  :group 'angular-mode)

(defvar angular-cli-executable
  (if (executable-find "npx")
      "npx ng"
    "ng")
  "Path to the Angular CLI executable.")

(defun angular-generate (schematic name)
  "Generate an Angular 'SCHEMATIC' called 'NAME' in the directory of choice."
  (interactive (list (completing-read "Schematic: "
                                      '("class" "component" "directive" "enum" "guard" "interceptor" "interface" "module" "pipe" "resolver" "service" "web-worker"))
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

(defun angular-open-component ()
  "Open an Angular component file in the project."
  (interactive)
  (angular-open-file "component"))

(defun angular-open-service ()
  "Open an Angular service file in the project."
  (interactive)
  (angular-open-file "service"))

(defun angular-open-module ()
  "Open an Angular module file in the project."
  (interactive)
  (angular-open-file "module"))

(defun angular-open-directive ()
  "Open an Angular directive file in the project."
  (interactive)
  (angular-open-file "directive"))

(defun angular-open-guard ()
  "Open an Angular guard file in the project."
  (interactive)
  (angular-open-file "guard"))

(defun angular-open-interceptor ()
  "Open an Angular interceptor file in the project."
  (interactive)
  (angular-open-file "interceptor"))

(defun angular-open-pipe ()
  "Open an Angular pipe file in the project."
  (interactive)
  (angular-open-file "pipe"))

(defun angular-open-resolver ()
  "Open an Angular resolver file in the project."
  (interactive)
  (angular-open-file "resolver"))

(defun angular-open-web-worker ()
  "Open an Angular web-worker file in the project."
  (interactive)
  (angular-open-file "worker"))

(defun angular-open-file (schematic)
  "Open an Angular 'SCHEMATIC' in the project."
  (interactive)
  (let ((project-root (file-name-as-directory (expand-file-name "src" (find-angular-project-root))))
        (schematic-length (length schematic))
        (schematics '()))
    (when project-root
      (dolist (dir (directory-files-recursively project-root (format ".+\\.%s\\.ts$" schematic)))
        (let* ((relative-path (file-relative-name dir project-root))
               (schematic-name (if (string-suffix-p (format ".%s.ts" schematic) relative-path)
                                   (substring relative-path 0 (- (length relative-path) (+ schematic-length 4)))
                                 relative-path)))
          (push schematic-name schematics)))
      (if schematics
          (let ((selected-schematic (completing-read (format "Select %s: " schematic) schematics)))
            (let ((schematic-file (concat project-root selected-schematic (format ".%s.ts" schematic))))
              (if (file-exists-p schematic-file)
                  (find-file schematic-file)
                (message "%s file not found: %s" schematic schematic-file))))
        (message "No Angular %ss found in the project." schematic)))))


(defun find-angular-project-root ()
  "Find the root directory of an Angular project."
  (let ((current-dir (file-name-directory (or buffer-file-name default-directory)))
        (root-file "angular.json"))
    (locate-dominating-file current-dir root-file)))

(defun angular-project-config ()
  "Open the project's angular.json file."
  (interactive)
  (let ((project-root (find-angular-project-root)))
    (if project-root
        (find-file (expand-file-name "angular.json" project-root))
      (message "Not inside an Angular project with an angular.json file."))))

;;;###autoload
(define-minor-mode angular-mode
  "Minor mode for working with Angular CLI."
  :lighter " Angular"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a g") 'angular-generate)
            (define-key map (kbd "C-c a p") 'angular-project-config)
            (define-key map (kbd "C-c a o c") 'angular-open-component)
            (define-key map (kbd "C-c a o d") 'angular-open-directive)
            (define-key map (kbd "C-c a o g") 'angular-open-guard)
            (define-key map (kbd "C-c a o i") 'angular-open-interceptor)
            (define-key map (kbd "C-c a o m") 'angular-open-module)
            (define-key map (kbd "C-c a o p") 'angular-open-pipe)
            (define-key map (kbd "C-c a o r") 'angular-open-resolver)
            (define-key map (kbd "C-c a o s") 'angular-open-service)
            (define-key map (kbd "C-c a o w") 'angular-open-web-worker)
            map))

(define-globalized-minor-mode global-angular-mode angular-mode
  (lambda () (angular-mode 1)))

(provide 'angular-mode)
;;; angular-mode.el ends here
