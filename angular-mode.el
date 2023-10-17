;;; angular-mode.el --- Emacs Minor Mode for Working with Angular CLI
;;
;; This package facilitates interaction with Angular projects by providing a minor mode.
;; It offers an interactive API for Angular CLI commands through a dedicated keymap.
;;
;; Keybindings:
;;   - C-c a g c: Generate an Angular component.
;;   - C-c a g s: Generate an Angular service.
;;   - C-c a g d: Generate an Angular directive.
;;   - C-c a g e: Generate an Angular enum.
;;   - C-c a g n: Generate and configure environment files for a project.
;;   - C-c a g g: Generate an Angular route guard.
;;   - C-c a g i: Generate an Angular interceptor.
;;   - C-c a g l: Create a new Angular library project.
;;   - C-c a g m: Generate an Angular NgModule definition.
;;   - C-c a g p: Generate an Angular pipe.
;;   - C-c a g r: Generate an Angular resolver.
;;   - C-c a g x: Create an Angular service worker.
;;   - C-c a g w: Generate an Angular web worker.
;;
;; To use this package, activate `angular-mode` and leverage the provided keybindings.
;; For example, use C-c a g c to generate an Angular component interactively.
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
            (define-key map (kbd "C-c a g c") 'angular-generate-component)
            (define-key map (kbd "C-c a g s") 'angular-generate-service)
            (define-key map (kbd "C-c a g d") 'angular-generate-directive)
            (define-key map (kbd "C-c a g e") 'angular-generate-enum)
            (define-key map (kbd "C-c a g n") 'angular-generate-environments)
            (define-key map (kbd "C-c a g g") 'angular-generate-guard)
            (define-key map (kbd "C-c a g i") 'angular-generate-interceptor)
            (define-key map (kbd "C-c a g l") 'angular-generate-library)
            (define-key map (kbd "C-c a g m") 'angular-generate-module)
            (define-key map (kbd "C-c a g p") 'angular-generate-pipe)
            (define-key map (kbd "C-c a g r") 'angular-generate-resolver)
            (define-key map (kbd "C-c a g x") 'angular-generate-service-worker)
            (define-key map (kbd "C-c a g w") 'angular-generate-web-worker)
            map))

;; TODO: Allow this to be customized.
(defvar angular-cli-executable
  (if (executable-find "npx")
      "npx ng"
    "ng")
  "Path to the Angular CLI executable.")

(defun angular-generate(name type)
  "Generate an Angular 'TYPE' called 'NAME'."
  (interactive (list (read-string "Name: ") (completing-read "Type: " '("component" "service"))))
  (shell-command (format "%s generate %s %s" angular-cli-executable type name)))

(defun angular-generate-app-shell(name)
  "Generate an Angular app shell called 'NAME'."
  (interactive "sApp shell name: ")
  (angular-generate name "app-shell"))

(defun angular-generate-application(name)
  "Generate a new basic application definition in the workspace called 'NAME'."
  (interactive "sApplication name: ")
  (angular-generate name "application"))

(defun angular-generate-class(name)
  "Generate a new class definition in the project called 'NAME'."
  (interactive "sClass name: ")
  (angular-generate name "class"))

(defun angular-generate-component(name)
  "Generate a new component definition in the project called 'NAME'."
  (interactive "sComponent name: ")
  (angular-generate name "component"))

(defun angular-generate-directive(name)
  "Generate a new directive definition in the project called 'NAME'."
  (interactive "sDirective name: ")
  (angular-generate name "directive"))

(defun angular-generate-enum(name)
  "Generate a new enum definition in the project called 'NAME'."
  (interactive "sEnum name: ")
  (angular-generate name "enum"))

(defun angular-generate-environments()
  "Generate and configure environment files for a project."
  (interactive)
  (angular-generate "" "environments"))

(defun angular-generate-guard(name)
  "Generate a new route guard definition in the project called 'NAME'."
  (interactive "sGuard name: ")
  (angular-generate name "guard"))

(defun angular-generate-interceptor(name)
  "Generate a new interceptor definition in the project called 'NAME'."
  (interactive "sInterceptor name: ")
  (angular-generate name "interceptor"))

(defun angular-generate-interface(name)
  "Generate a new interface definition in the project called 'NAME'."
  (interactive "sInterface name: ")
  (angular-generate name "interface"))

(defun angular-generate-library(name)
  "Generate a new library project in the current workspace called 'NAME'."
  (interactive "sLibrary name: ")
  (angular-generate name "library"))

(defun angular-generate-module(name)
  "Generate a new NgModule definition in the project called 'NAME'."
  (interactive "sModule name: ")
  (angular-generate name "module"))

(defun angular-generate-pipe(name)
  "Generate a new pipe definition in the project called 'NAME'."
  (interactive "sPipe name: ")
  (angular-generate name "pipe"))

(defun angular-generate-resolver(name)
  "Generate a new resolver definition in the project called 'NAME'."
  (interactive "sResolver name: ")
  (angular-generate name "resolver"))

(defun angular-generate-service(name)
  "Generate a new service definition in the project called 'NAME'."
  (interactive "sService name: ")
  (angular-generate name "service"))

(defun angular-generate-service-worker()
  "Pass this schematic to the run command to create a service worker."
  (interactive)
  (angular-generate "" "service-worker"))

(defun angular-generate-web-worker(name)
  "Generate a new web worker definition in the project called 'NAME'."
  (interactive "sWeb worker name: ")
  (angular-generate name "web-worker"))

(provide 'angular-mode)
;;; angular-mode.el ends here
