;;; angular-mode.el --- Emacs Minor Mode for Working with Angular CLI -*- lexical-binding: t; -*-
;;
;; This package facilitates interaction with Angular projects by providing a minor mode.
;; It offers an interactive API for Angular CLI commands through a dedicated keymap.
;;
;; Keybindings:
;;   - C-c a g: Generate an Angular schematic.
;;   - C-c a p: Open the project's angular.json file.
;;   - C-c a h d: Lookup current word at point in API reference documentation website.
;;   - C-c a h s: Perform a search of angular.io using the current word at point.
;;   - C-c a o c: Open a component.ts file.
;;   - C-c a o t: Open a component.html file.
;;   - C-c a o t: Open a component.(scss|sass|less|css) file.
;;   - C-c a o x: Open a component.spec.ts file.
;;   - C-c a o d: Open a directive.ts file.
;;   - C-c a o g: Open a guard.ts file.
;;   - C-c a o i: Open a interceptor.ts file.
;;   - C-c a o m: Open a module.ts file.
;;   - C-c a o p: Open a pipe.ts file.
;;   - C-c a o r: Open a resolver.ts file.
;;   - C-c a o s: Open a service.ts file.
;;   - C-c a o w: Open a worker.ts file.
;;   - C-c a j c: Jump to the associated component.ts file.
;;   - C-c a j t: Jump to the associated component.html file.
;;   - C-c a j v: Jump to the associated component.(scss|sass|less|css) file.
;;   - C-c a j x: Jump to the associated component.spec.ts file.
;;   - C-c a m d: Move a directory to a new destination and update import paths for all entities within the directory.
;;   - C-c a m f: Move a file and associated spec file to a new destination and update import paths for those files.
;;
;; To use this package, activate `angular-mode` and leverage the provided keybindings
;; to generate schematics in the project directory of choice.
;;
;;
;; Copyright (C) 2024 Kevin Borling <kborling@protonmail.com>
;;
;; Author: Kevin Borling <kborling@protonmail.com>
;; Created: October 16, 2023
;; Version: 0.0.4
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

(require 'xref)

(defgroup angular-mode nil
  "Angular mode, a minor mode for interacting with the Angular CLI."
  :group 'angular-mode)

(defvar angular-cli-executable
  (if (executable-find "npx")
      "npx ng"
    "ng")
  "Path to the Angular CLI executable.")

(defcustom angular-import-path-style 'relative
  "Determines the style of import paths in Angular projects.
Choose \\'absolute for absolute paths or \\'relative for relative paths."
  :type '(choice (const :tag "Absolute" absolute)
                 (const :tag "Relative" relative))
  :group 'angular-customizations)

(defun angular-generate (schematic name)
  "Generate an Angular SCHEMATIC called NAME in the directory of choice."
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

(defun angular-open-component-template ()
  "Open an Angular component template file in the project."
  (interactive)
  (angular-open-file "component" "html"))

(defun angular-open-component-stylesheet ()
  "Open an Angular stylesheet file in the project."
  (interactive)
  (angular-open-file "component" "\\(scss\\|less\\|sass\\|css\\)"))

(defun angular-open-component-test ()
  "Open an Angular component test file in the project."
  (interactive)
  (angular-open-file "component.spec"))

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

(defun angular-jump-to-component ()
  "Jump to the associated component file in the project."
  (interactive)
  (angular-jump-to-file "component"))

(defun angular-jump-to-template ()
  "Jump to the associated template file in the project."
  (interactive)
  (angular-jump-to-file "template"))

(defun angular-jump-to-stylesheet ()
  "Jump to the associated stylesheet file in the project."
  (interactive)
  (angular-jump-to-file "stylesheet"))

(defun angular-jump-to-test ()
  "Jump to the associated test file in the project."
  (interactive)
  (angular-jump-to-file "test"))

(defun angular-lookup-word ()
  "Lookup the current word at point in API reference documentation."
  (interactive)
  (angular-docs nil))

(defun angular-search-word ()
  "Perform a search of angular.io using the current word at point."
  (interactive)
  (angular-docs t))

(defun angular-docs (search)
  "Lookup the current word at point in API reference documentation.
Optionally SEARCH the angular.io website."
  (let ((word (current-word)))
    (if word
        (shell-command (format "%s d %s --search=%s"
                               angular-cli-executable
                               word
                               (if search "true" "false")))
      (message "No word at point."))))

(defun angular-open-file (schematic &optional file-type)
  "Open an Angular SCHEMATIC with optional FILE-TYPE in the project."
  (let ((project-root (file-name-as-directory (expand-file-name "src" (find-angular-project-root))))
        (schematics '()))
    (when project-root
      (or file-type (setq file-type "ts"))
      (dolist (dir (directory-files-recursively project-root (format ".+\\.%s\\.%s$" schematic file-type)))
        (let* ((relative-path (file-relative-name dir project-root)))
          (push relative-path schematics)))
      (if schematics
          (let ((selected-schematic (completing-read (format "Select %s: " schematic) schematics)))
            (let ((schematic-file (concat project-root selected-schematic)))
              (if (file-exists-p schematic-file)
                  (find-file schematic-file)
                (message "%s file not found: %s" schematic schematic-file))))
        (message "No Angular %ss found in the project." schematic)))))

(defun angular-jump-to-file (file-type)
  "Jump to the associated FILE-TYPE in the same directory."
  (let ((current-file (buffer-file-name)))
    (when current-file
      (let* ((file-name (if (string-suffix-p (format ".spec.ts") current-file)
                            (file-name-sans-extension (file-name-sans-extension (file-name-nondirectory current-file)))
                          (file-name-sans-extension (file-name-nondirectory current-file))))
             (dir-name (file-name-directory current-file))
             (extensions (pcase file-type
                           ("template" '("html"))
                           ("component" '("ts"))
                           ("test" '("spec.ts"))
                           ("stylesheet" '("scss" "sass" "less" "css"))))
             (associated-file nil))

        ;; Find the first matching file extension
        (catch 'found
          (dolist (ext extensions)
            (let ((file-path (format "%s%s.%s" dir-name file-name ext)))
              (when (file-exists-p file-path)
                (setq associated-file file-path)
                (throw 'found associated-file)))))

        (if associated-file
            (find-file associated-file)
          (message "Associated %s file not found" file-type))
        ))))

(defun angular-update-import-paths (old-path new-path)
  "Update import paths in TypeScript files from OLD-PATH to NEW-PATH."
  (let* ((project-root (find-angular-project-root))
         (app-directory (expand-file-name "src/app" project-root))
         (search (expand-file-name old-path project-root))
         (replace (expand-file-name new-path project-root))
         (import-regex "import\\s-+{.*?}\\s-+from\\s-+\\(['\\\"]\\)\\(.*?\\)\\1"))

    (dolist (ts-file (directory-files-recursively app-directory "\\.ts$"))
      (with-temp-buffer
        ;; Insert file contents into temp buffer
        (insert-file-contents ts-file)
        ;; Go to the beginning of the file
        (goto-char (point-min))

        (let ((original-contents (buffer-string))
              (file-path (file-name-directory ts-file)))
          ;; Search through each import statement and replace path
          (while (re-search-forward import-regex nil t)
            (let ((match (match-string 2))) ; The matched import path
              (unless (string-prefix-p "@" match) ; TODO Add support for custom modules in future
                (let ((expanded-match
                       ;; If the current 'file-path' ends in src/app/
                       ;; and import begins with src/app, use 'project-root' as base expansion instead of 'file-path'
                       (if (and (string-match-p "src/app" match) (string-match-p "src/app/$" file-path))
                           (expand-file-name match project-root)
                         (expand-file-name match file-path))))
                  (when (string-prefix-p search expanded-match)
                    (let* ((remainder (substring expanded-match (length search)))
                           (final-replacement-path
                            (cond
                             ((eq angular-import-path-style 'absolute)
                              (file-relative-name (concat (directory-file-name replace) remainder) project-root))
                             ((eq angular-import-path-style 'relative)
                              (let ((relative-path (file-relative-name
                                                    (concat (directory-file-name replace) remainder) file-path)))
                                ;; Ensure relative paths start with "./" if they don't already
                                (if (or (string-prefix-p "./" relative-path) (string-prefix-p "../" relative-path))
                                    relative-path
                                  (concat "./" relative-path)))))))
                      ;; Replace the matched text with the new path
                      (replace-match final-replacement-path nil nil nil 2)))))))

          (when (not (equal (buffer-string) original-contents))
            (write-region (point-min) (point-max) ts-file)))))))

(defun angular-move-directory (current-directory destination)
  "Move Angular component from CURRENT-DIRECTORY to DESTINATION.
Update all import paths in files that reference the component."
  (interactive (list (read-directory-name "Current directory: ")
                     (read-directory-name "New directory: ")))
  (let* ((current-dir (expand-file-name (directory-file-name (file-name-directory current-directory))))
         (current-dir-name (file-name-nondirectory current-dir))
         (new-dir (file-name-concat (expand-file-name (file-name-directory destination)) current-dir-name)))
    (when (file-directory-p new-dir)
      (error "Destination already has a directory named '%s'" new-dir))
    (unless (file-directory-p destination)
      (make-directory destination :parents))
    (rename-file current-dir new-dir t)
    (angular-update-import-paths current-dir new-dir)
    ))

(defun angular-move-file (current-path destination)
  "Move Angular file and associated spec file \\
from CURRENT-PATH to DESTINATION.
Update all import paths in files that reference the entity."

  (interactive (list (read-file-name "Select entity: ")
                     (read-directory-name "New directory: ")))

  (let* ((is-spec-file (string-suffix-p (format ".spec.ts") current-path))
         (base-name (file-name-sans-extension (file-name-nondirectory current-path)))
         (base-path (file-name-directory current-path))
         (current-dir (directory-file-name base-path))
         (file-name (if is-spec-file
                        (file-name-sans-extension (file-name-sans-extension (file-name-nondirectory current-path)))
                      (file-name-sans-extension (file-name-nondirectory current-path))))
         (spec-file-exists (or is-spec-file (file-exists-p (expand-file-name (concat base-name ".spec.ts") current-dir))))

         (component-name (file-name-nondirectory (directory-file-name current-dir)))
         (new-dir (expand-file-name component-name destination))

         (full-name (file-name-nondirectory current-path))

         (file-name-ext (if is-spec-file
                            (concat file-name ".ts")
                          full-name))
         (old-base-path (expand-file-name file-name-ext base-path))
         (new-base-path (expand-file-name file-name-ext destination))

         (spec-name-ext (if is-spec-file
                            full-name
                          (concat base-name ".spec.ts")))

         (old-spec-path (expand-file-name spec-name-ext base-path))
         (new-spec-path (expand-file-name spec-name-ext destination))) ;; end let

    (when (file-in-directory-p file-name-ext new-dir)
      (error "Destination already has a file named '%s'" file-name-ext))

    (when (and spec-file-exists (file-in-directory-p spec-name-ext new-dir))
      (error "Destination already has a spec file named '%s'" file-name-ext))

    ;; Create destination directory if it doesn't exist
    (unless (file-directory-p destination)
      (make-directory destination :parents))

    ;; (unless is-spec-file
    (rename-file old-base-path new-base-path t)

    ;; Move the associated spec file if it exists
    (when (or is-spec-file spec-file-exists)
      (rename-file old-spec-path new-spec-path t))

    ;; Update import paths for entity file
    (angular-update-import-paths (file-name-sans-extension old-base-path) (file-name-sans-extension new-base-path))

    ;; Update import paths for spec file
    (when (or is-spec-file spec-file-exists)
      (angular-update-import-paths (file-name-sans-extension old-spec-path) (file-name-sans-extension new-spec-path)))))

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

(defun angular-get-prefix ()
  "Get the prefix value from the nearest Angular JSON file.
Searches for angular.json in the project root or parent directories.
Returns the prefix as a string, or nil if not found."
  (let* ((project-root (find-angular-project-root))
         (angular-json-path (when project-root
                              (expand-file-name "angular.json" project-root))))
    (if (and angular-json-path (file-exists-p angular-json-path))
        (with-temp-buffer
          (insert-file-contents angular-json-path)
          (goto-char (point-min))
          (if (re-search-forward "\"prefix\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
              (match-string 1)
            (message "No 'prefix' key found in %s" angular-json-path)
            nil))
      (message "No Angular project found.")
      nil)))

(defun angular-xref-backend ()
  "Define the xref backend for Angular."
  'angular)

(cl-defmethod xref-backend-definitions ((backend (eql angular)) identifier)
  "Find the definitions of the IDENTIFIER using the Angular BACKEND."
  (let* ((selector (angular-extract-selector identifier))
         (component-file (when selector
                           (angular-find-component-file-by-selector selector))))
    (if component-file
        (list (xref-make selector (xref-make-file-location component-file 1 0)))
      (user-error "Component definition not found for selector: %s" selector))))

(defun angular-thing-at-point ()
  "Get the full Angular component tag at point, including hyphens."
  (let ((start (save-excursion
                 (while (and (not (bobp))
                             (looking-back "[a-z0-9-]" 1))
                   (backward-char))
                 (point)))
        (end (save-excursion
               (while (and (not (eobp))
                           (looking-at "[a-z0-9-]"))
                 (forward-char))
               (point))))
    (buffer-substring-no-properties start end)))

(defun angular-extract-selector (tag)
  "Extract the Angular component selector from TAG."
  (when (string-match "\\([[:alnum:]-]+\\)" tag)
    (match-string 1 tag)))

(defun angular-find-component-file-by-selector (selector)
  "Find the Angular component file that has the given SELECTOR."
  (let ((project-root (find-angular-project-root)))
    (when project-root
      (angular-search-component-file project-root selector))))

(defun angular-search-component-file (directory selector)
  "Search for the Angular component file with SELECTOR in DIRECTORY."
  (let ((stack (list directory))
        (component-file nil))
    (while (and stack (not component-file))
      (let ((current-dir (pop stack)))
        (dolist (file (directory-files current-dir t))
          (cond
           ((and (file-directory-p file)
                 (not (member (file-name-nondirectory file) '("." ".." "node_modules" ".git"))))
            (push file stack))
           ((and (file-regular-p file)
                 (string-match "component.ts$" file))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (when (re-search-forward (format "selector:\\s-*['\"]%s['\"]" selector) nil t)
                (setq component-file file))))))))
    component-file))

(defun angular-rename-component ()
  "Rename an Angular component.
1. Prompts the user to select an existing component to rename.
2. Prompts the user to input a new name in kebab-case format.
This updates:
- The class name in TypeScript files (PascalCase).
- The selector name in TypeScript and HTML files (kebab-case).
- The component's file names (kebab-case).
- All imports and references to the renamed component."
  (interactive)
  (let* ((project-root (find-angular-project-root))
         (components (angular-list-components project-root))
         (old-name (completing-read "Select component to rename: " components))
         (new-name (read-string "New component name (kebab-case): "))
         (prefix (angular-get-prefix))
         (old-selector (format "%s-%s" prefix old-name))
         (new-selector (format "%s-%s" prefix new-name))
         (old-dir (directory-file-name
                   (file-name-directory
                    (car (directory-files-recursively project-root
                                                      (format "%s.component\\.ts$" old-name))))))
         (new-dir (concat (file-name-directory (directory-file-name old-dir))
                          new-name)))

    ;; Rename the parent directory
    (when (and old-dir new-dir (not (string-equal old-dir new-dir)))
      (rename-file old-dir new-dir)
      ;; Update all references to the old directory
      (angular-update-import-paths old-dir new-dir))

    ;; Dynamically resolve the new file paths after renaming the directory
    (let* ((component-files (directory-files-recursively
                           project-root
                           (format "%s.component\\(\\.spec\\)?\\.ts$" old-name)))
         (template-files (directory-files-recursively
                          project-root
                          (format "%s.component.html$" old-name)))
         (style-files (directory-files-recursively
                       project-root
                       (format "%s.component.\\(scss\\|sass\\|less\\|css\\)$" old-name))))

      ;; Update class name, selector, templateUrl, styleUrls, and import paths in TypeScript files
    (dolist (file component-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Update class name
        (while (re-search-forward (format "\\b%sComponent\\b" (angular-pascal-case old-name)) nil t)
          (replace-match (format "%sComponent" (angular-pascal-case new-name))))
        ;; Update selector
        (goto-char (point-min))
        (while (re-search-forward (format "selector:\\s-*['\"]%s['\"]" old-selector) nil t)
          (replace-match (format "selector: '%s'" new-selector)))
        ;; Update templateUrl
        (goto-char (point-min))
        (while (re-search-forward "templateUrl:\\s-*['\"]\\(.*?\\)\\.component\\.html['\"]" nil t)
          (replace-match (format "templateUrl: './%s.component.html'" new-name)))
        ;; Update styleUrls
        (goto-char (point-min))
        (while (re-search-forward "styleUrls:\\s-*\\(\\[.*?['\"]\\)\\(.*?\\)\\.component\\.\\(scss\\|sass\\|less\\|css\\)['\"]\\(.*?\\]\\)" nil t)
          (replace-match (format "styleUrls: ['./%s.component.%s']"
                                 new-name
                                 (match-string 3))))
        ;; Save changes
        (write-region (point-min) (point-max) file)))

    ;; Rename component template and style files
    (dolist (file component-files)
      (message "%s" (replace-regexp-in-string old-name new-name file))
      (rename-file file (replace-regexp-in-string old-name new-name file)))
    
    (dolist (file template-files)
      (message "%s" (replace-regexp-in-string old-name new-name file))
      (rename-file file (replace-regexp-in-string old-name new-name file)))

    (dolist (file style-files)
      (message "%s" (replace-regexp-in-string old-name new-name file))
      (rename-file file (replace-regexp-in-string old-name new-name file)))

    ;; TODO: Fix import update issue
    ;; Update references to the component in other files
    (angular-update-import-paths (format "%s.component" old-name)
                                 (format "%s.component" new-name))
    (angular-update-import-paths (format "%s.component.html" old-name)
                                 (format "%s.component.html" new-name))
    (angular-update-import-paths (format "%s.component.scss" old-name)
                                 (format "%s.component.scss" new-name))

    (message "Renamed component '%s' to '%s' successfully." old-name new-name))))

(defun angular-list-components (project-root)
  "List all components in the Angular project at PROJECT-ROOT."
  (let ((component-regex "\\([a-z0-9-]+\\)\\.component\\.ts$"))
    (seq-map
     (lambda (file)
       (if (string-match component-regex (file-name-nondirectory file))
           (match-string 1 (file-name-nondirectory file))))
     (directory-files-recursively project-root "\\.component\\.ts$"))))

(defun angular-pascal-case (kebab-case-name)
  "Convert KEBAB-CASE-NAME to PascalCase format."
  (mapconcat #'capitalize (split-string kebab-case-name "-") ""))

;;;###autoload
(define-minor-mode angular-mode
  "Minor mode for working with Angular CLI."
  :lighter " Angular"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a g") 'angular-generate)
            (define-key map (kbd "C-c a p") 'angular-project-config)
            (define-key map (kbd "C-c a h d") 'angular-lookup-word)
            (define-key map (kbd "C-c a h s") 'angular-search-word)
            (define-key map (kbd "C-c a o c") 'angular-open-component)
            (define-key map (kbd "C-c a o x") 'angular-open-component-test)
            (define-key map (kbd "C-c a o t") 'angular-open-component-template)
            (define-key map (kbd "C-c a o v") 'angular-open-component-stylesheet)
            (define-key map (kbd "C-c a o d") 'angular-open-directive)
            (define-key map (kbd "C-c a o g") 'angular-open-guard)
            (define-key map (kbd "C-c a o i") 'angular-open-interceptor)
            (define-key map (kbd "C-c a o m") 'angular-open-module)
            (define-key map (kbd "C-c a o p") 'angular-open-pipe)
            (define-key map (kbd "C-c a o r") 'angular-open-resolver)
            (define-key map (kbd "C-c a o s") 'angular-open-service)
            (define-key map (kbd "C-c a o w") 'angular-open-web-worker)
            (define-key map (kbd "C-c a j c") 'angular-jump-to-component)
            (define-key map (kbd "C-c a j t") 'angular-jump-to-template)
            (define-key map (kbd "C-c a j v") 'angular-jump-to-stylesheet)
            (define-key map (kbd "C-c a j x") 'angular-jump-to-test)
            (define-key map (kbd "C-c a m d") 'angular-move-directory)
            (define-key map (kbd "C-c a m f") 'angular-move-file)
            map)

  (add-hook 'xref-backend-functions #'angular-xref-backend nil t))

(define-globalized-minor-mode global-angular-mode angular-mode
  (lambda () (angular-mode 1)))

(provide 'angular-mode)
;;; angular-mode.el ends here
