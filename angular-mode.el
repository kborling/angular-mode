;;; angular-mode.el --- Emacs Minor Mode for Working with Angular CLI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Kevin Borling <kborling@protonmail.com>
;;
;; Author: Kevin Borling <kborling@protonmail.com>
;; Created: October 16, 2023
;; Version: 0.3.0
;; Keywords: angular, angular-cli, angular-mode
;; License: MIT
;; URL: https://github.com/kborling/angular-mode
;; Homepage: https://github.com/kborling/angular-mode
;; Filename: angular-mode.el
;; Package-Requires: ((emacs "29.1"))
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
;; Minor mode for Angular projects. Provides schematic generation, fast
;; navigation between component parts, and refactoring commands.
;;
;; Key bindings (all under C-c a, or use the transient with C-c a a):
;;   C-c a a   Transient menu (discover all commands)
;;   C-c a TAB Cycle between component ts/html/scss/spec
;;   C-c a g   Generate a schematic
;;   C-c a o   Open a schematic by type
;;   C-c a j   Jump to associated file
;;
;;; Code:

(require 'cl-lib)

(defgroup angular-mode nil
  "Angular mode for interacting with Angular CLI."
  :group 'tools)

(defcustom angular-cli-executable nil
  "Path to the Angular CLI executable.
If nil, auto-detects npx or ng at first use."
  :type '(choice (const nil) string)
  :group 'angular-mode)

(defcustom angular-import-path-style 'relative
  "Style of import paths. \\='absolute or \\='relative."
  :type '(choice (const :tag "Absolute" absolute)
                 (const :tag "Relative" relative))
  :group 'angular-mode)

(defun angular-cli-executable ()
  "Return the Angular CLI executable."
  (or angular-cli-executable
      (if (executable-find "npx") "npx ng" "ng")))

;;; ============================================================
;;; Project Root
;;; ============================================================

(defun angular-find-project-root ()
  "Find the root directory of an Angular project."
  (locate-dominating-file (or buffer-file-name default-directory) "angular.json"))

(defun angular-project-config ()
  "Open the project's angular.json file."
  (interactive)
  (let ((root (angular-find-project-root)))
    (if root
        (find-file (expand-file-name "angular.json" root))
      (message "Not inside an Angular project."))))

(defun angular-get-prefix ()
  "Get the prefix from angular.json."
  (let* ((root (angular-find-project-root))
         (json-path (when root (expand-file-name "angular.json" root))))
    (when (and json-path (file-exists-p json-path))
      (with-temp-buffer
        (insert-file-contents json-path)
        (goto-char (point-min))
        (when (re-search-forward "\"prefix\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
          (match-string 1))))))

;;; ============================================================
;;; Fast File Listing (fd or find)
;;; ============================================================

(defun angular--project-files (root pattern)
  "List files matching regex PATTERN under ROOT/src, using fd or find."
  (let ((src-dir (expand-file-name "src" root)))
    (when (file-directory-p src-dir)
      (if (executable-find "fd")
          (let ((default-directory src-dir))
            (mapcar (lambda (f) (expand-file-name f src-dir))
                    (split-string
                     (shell-command-to-string
                      (format "fd --type f --regex %s" (shell-quote-argument pattern)))
                     "\n" t)))
        (directory-files-recursively src-dir pattern)))))

(defun angular--find-by-selector (root selector)
  "Find the component.ts file with SELECTOR under ROOT, using rg if available."
  (let ((src-dir (expand-file-name "src" root))
        (pattern (format "selector:\\s*['\"]%s['\"]" selector)))
    (if (executable-find "rg")
        (let ((output (shell-command-to-string
                       (format "rg --files-with-matches --no-heading %s %s --glob '*.component.ts'"
                               (shell-quote-argument pattern)
                               (shell-quote-argument src-dir)))))
          (let ((file (car (split-string output "\n" t))))
            (when (and file (file-exists-p file)) file)))
      ;; Fallback: read each file
      (cl-find-if
       (lambda (file)
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (re-search-forward pattern nil t)))
       (angular--project-files root "\\.component\\.ts$")))))

;;; ============================================================
;;; Schematic Generation
;;; ============================================================

(defun angular-generate (schematic name)
  "Generate an Angular SCHEMATIC called NAME."
  (interactive
   (list (completing-read "Schematic: "
                          '("component" "service" "directive" "pipe" "guard"
                            "interceptor" "resolver" "interface" "class" "enum"
                            "module" "library" "application" "config"
                            "environments"))
         (read-string "Name: ")))
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (target-dir (read-directory-name "Directory: "
                                          (expand-file-name "src/app" root)))
         (relative (file-relative-name target-dir (expand-file-name "src/app" root)))
         (path (if (string= relative "./") name
                 (concat (directory-file-name relative) "/" name)))
         (options (completing-read-multiple
                   "Options (comma-separated, or empty): "
                   '("--skip-tests" "--inline-style" "--inline-template"
                     "--flat" "--export" "--standalone=false"))))
    (compile (format "%s generate %s %s %s"
                     (angular-cli-executable) schematic path
                     (string-join options " ")))))

;;; ============================================================
;;; Component Cycle (ts ↔ html ↔ scss ↔ spec)
;;; ============================================================

(defun angular-cycle ()
  "Cycle between component files: ts → html → scss → spec → ts."
  (interactive)
  (let* ((file (or buffer-file-name (user-error "Buffer has no file")))
         (dir (file-name-directory file))
         (base (angular--component-base file))
         (current-ext (angular--component-ext file))
         (cycle-order '("ts" "html" "scss" "sass" "less" "css" "spec.ts"))
         (style-exts '("scss" "sass" "less" "css"))
         next-file)
    (unless base
      (user-error "Not in a component file"))
    ;; Find the next file in cycle
    (catch 'found
      ;; Find current position in cycle
      (let* ((pos (cl-position current-ext cycle-order :test #'string=))
             (len (length cycle-order)))
        (when pos
          (cl-loop for i from 1 below len
                   for idx = (mod (+ pos i) len)
                   for ext = (nth idx cycle-order)
                   do (let ((candidate (format "%s%s.%s" dir base ext)))
                        (when (file-exists-p candidate)
                          (setq next-file candidate)
                          (throw 'found t))))
          ;; If a style ext, also check other style extensions
          (when (member current-ext style-exts)
            (dolist (ext '("spec.ts" "ts" "html"))
              (let ((candidate (format "%s%s.%s" dir base ext)))
                (when (file-exists-p candidate)
                  (setq next-file candidate)
                  (throw 'found t))))))))
    (if next-file
        (find-file next-file)
      (message "No other component files found"))))

(defun angular--component-base (file)
  "Extract the component base name from FILE (e.g. 'auth.component' from auth.component.ts)."
  (let ((name (file-name-nondirectory file)))
    (cond
     ((string-match "^\\(.+\\.component\\)\\.spec\\.ts$" name) (match-string 1 name))
     ((string-match "^\\(.+\\.component\\)\\." name) (match-string 1 name))
     (t nil))))

(defun angular--component-ext (file)
  "Extract the extension type from a component FILE."
  (let ((name (file-name-nondirectory file)))
    (cond
     ((string-match "\\.component\\.spec\\.ts$" name) "spec.ts")
     ((string-match "\\.component\\.\\([a-z]+\\)$" name) (match-string 1 name))
     (t nil))))

;;; ============================================================
;;; Open by Type
;;; ============================================================

(defun angular-open-file (schematic &optional file-type)
  "Open an Angular SCHEMATIC with optional FILE-TYPE."
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (ext (or file-type "ts"))
         (pattern (format "\\.%s\\.%s$" schematic ext))
         (files (angular--project-files root pattern)))
    (if files
        (let* ((src-dir (expand-file-name "src" root))
               (candidates (mapcar (lambda (f) (file-relative-name f src-dir)) files))
               (choice (completing-read (format "Open %s: " schematic) candidates nil t)))
          (find-file (expand-file-name choice src-dir)))
      (message "No %s files found" schematic))))

(defun angular-open-component ()         (interactive) (angular-open-file "component"))
(defun angular-open-component-template () (interactive) (angular-open-file "component" "html"))
(defun angular-open-component-stylesheet () (interactive) (angular-open-file "component" "\\(scss\\|less\\|sass\\|css\\)"))
(defun angular-open-component-test ()    (interactive) (angular-open-file "component.spec"))
(defun angular-open-service ()           (interactive) (angular-open-file "service"))
(defun angular-open-module ()            (interactive) (angular-open-file "module"))
(defun angular-open-directive ()         (interactive) (angular-open-file "directive"))
(defun angular-open-guard ()             (interactive) (angular-open-file "guard"))
(defun angular-open-interceptor ()       (interactive) (angular-open-file "interceptor"))
(defun angular-open-pipe ()              (interactive) (angular-open-file "pipe"))
(defun angular-open-resolver ()          (interactive) (angular-open-file "resolver"))
(defun angular-open-web-worker ()        (interactive) (angular-open-file "worker"))
(defun angular-open-interface ()         (interactive) (angular-open-file "interface"))

;;; ============================================================
;;; Jump to Associated File
;;; ============================================================

(defun angular-jump-to-file (file-type)
  "Jump to the associated FILE-TYPE in the same directory."
  (let* ((file (or buffer-file-name (user-error "Buffer has no file")))
         (base (angular--component-base file))
         (dir (file-name-directory file))
         (extensions (pcase file-type
                       ("template" '("html"))
                       ("component" '("ts"))
                       ("test" '("spec.ts"))
                       ("stylesheet" '("scss" "sass" "less" "css")))))
    (unless base
      (user-error "Not in a component file"))
    (let ((found (cl-find-if
                  (lambda (ext)
                    (file-exists-p (format "%s%s.%s" dir base ext)))
                  extensions)))
      (if found
          (find-file (format "%s%s.%s" dir base found))
        (message "Associated %s file not found" file-type)))))

(defun angular-jump-to-component ()  (interactive) (angular-jump-to-file "component"))
(defun angular-jump-to-template ()   (interactive) (angular-jump-to-file "template"))
(defun angular-jump-to-stylesheet () (interactive) (angular-jump-to-file "stylesheet"))
(defun angular-jump-to-test ()       (interactive) (angular-jump-to-file "test"))

;;; ============================================================
;;; Xref Backend
;;; ============================================================

(defun angular-xref-backend ()
  "Define the xref backend for Angular."
  'angular)

(cl-defmethod xref-backend-definitions ((backend (eql angular)) identifier)
  "Find definitions of IDENTIFIER using the Angular BACKEND."
  (let* ((selector (angular-extract-selector identifier))
         (root (angular-find-project-root))
         (component-file (when (and selector root)
                           (angular--find-by-selector root selector))))
    (if component-file
        (list (xref-make selector (xref-make-file-location component-file 1 0)))
      (user-error "Component not found for selector: %s" selector))))

(defun angular-thing-at-point ()
  "Get the full Angular component tag at point."
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

;;; ============================================================
;;; Docs
;;; ============================================================

(defun angular-lookup-word ()
  "Lookup word at point in Angular API docs."
  (interactive)
  (angular-docs nil))

(defun angular-search-word ()
  "Search angular.dev for word at point."
  (interactive)
  (angular-docs t))

(defun angular-docs (search)
  "Lookup word at point in Angular docs. SEARCH t for search, nil for API."
  (let ((word (current-word)))
    (if word
        (browse-url (if search
                       (format "https://angular.dev/search?q=%s" word)
                     (format "https://angular.dev/api/%s" word)))
      (message "No word at point."))))

;;; ============================================================
;;; Import Path Updates
;;; ============================================================

(defun angular--compute-new-import (match file-path search replace project-root)
  "Compute new import path for MATCH in FILE-PATH given SEARCH→REPLACE.
PROJECT-ROOT is the Angular project root."
  (unless (string-prefix-p "@" match)
    (let ((expanded (if (and (string-match-p "src/app" match)
                             (string-match-p "src/app/$" file-path))
                        (expand-file-name match project-root)
                      (expand-file-name match file-path))))
      (when (string-prefix-p search expanded)
        (let* ((remainder (substring expanded (length search)))
               (target (concat (directory-file-name replace) remainder)))
          (cond
           ((eq angular-import-path-style 'absolute)
            (file-relative-name target project-root))
           (t ; relative
            (let ((rel (file-relative-name target file-path)))
              (if (or (string-prefix-p "./" rel) (string-prefix-p "../" rel))
                  rel
                (concat "./" rel))))))))))

(defun angular-update-import-paths (old-path new-path)
  "Update import paths in TypeScript files from OLD-PATH to NEW-PATH.
Handles static imports, dynamic import(), and loadChildren/loadComponent."
  (let* ((project-root (angular-find-project-root))
         (src-directory (expand-file-name "src" project-root))
         (search (expand-file-name old-path project-root))
         (replace (expand-file-name new-path project-root))
         ;; Match: import {...} from '...' (possibly multi-line)
         (static-import-regex "import\\s-+{[^}]*}\\s-+from\\s-+\\(['\"]\\)\\([^'\"]+\\)\\1")
         ;; Match: import('...')  (dynamic imports for lazy loading)
         (dynamic-import-regex "import(\\s-*\\(['\"]\\)\\([^'\"]+\\)\\1\\s-*)")
         (update-count 0))
    (dolist (ts-file (directory-files-recursively src-directory "\\.ts$"))
      (with-temp-buffer
        (insert-file-contents ts-file)
        (let ((original (buffer-string))
              (file-path (file-name-directory ts-file)))
          ;; Static imports
          (goto-char (point-min))
          (while (re-search-forward static-import-regex nil t)
            (let ((new-import (angular--compute-new-import
                               (match-string 2) file-path search replace project-root)))
              (when new-import
                (replace-match new-import nil nil nil 2)
                (cl-incf update-count))))
          ;; Dynamic imports
          (goto-char (point-min))
          (while (re-search-forward dynamic-import-regex nil t)
            (let ((new-import (angular--compute-new-import
                               (match-string 2) file-path search replace project-root)))
              (when new-import
                (replace-match new-import nil nil nil 2)
                (cl-incf update-count))))
          (when (not (equal (buffer-string) original))
            (write-region (point-min) (point-max) ts-file)))))
    (when (> update-count 0)
      (message "Updated %d import paths" update-count))))

;;; ============================================================
;;; Move & Rename
;;; ============================================================

(defun angular-move-directory (current-directory destination)
  "Move Angular component directory to DESTINATION.
Updates all import paths and reverts open buffers."
  (interactive (list (read-directory-name "Directory to move: ")
                     (read-directory-name "Move to: ")))
  (let* ((current-dir (expand-file-name (directory-file-name current-directory)))
         (current-dir-name (file-name-nondirectory current-dir))
         (new-dir (expand-file-name current-dir-name destination)))
    (when (file-directory-p new-dir)
      (user-error "Destination already has '%s'" current-dir-name))
    (unless (file-directory-p destination)
      (make-directory destination :parents))
    (rename-file current-dir new-dir t)
    (angular-update-import-paths current-dir new-dir)
    ;; Revert buffers that were visiting files in the old directory
    (dolist (buf (buffer-list))
      (when-let* ((file (buffer-file-name buf)))
        (when (string-prefix-p (file-name-as-directory current-dir) file)
          (let ((new-file (concat (file-name-as-directory new-dir)
                                  (substring file (length (file-name-as-directory current-dir))))))
            (when (file-exists-p new-file)
              (with-current-buffer buf
                (set-visited-file-name new-file t t)))))))
    (message "Moved %s to %s" current-dir-name (abbreviate-file-name destination))))

(defun angular-move-file (current-path destination)
  "Move Angular file and all associated files to DESTINATION.
For components, moves .ts, .html, .scss/.css, and .spec.ts together.
For services/pipes/etc, moves .ts and .spec.ts."
  (interactive (list (read-file-name "Select file: ")
                     (read-directory-name "New directory: ")))
  (let* ((current-path (expand-file-name current-path))
         (destination (expand-file-name destination))
         (base-dir (file-name-directory current-path))
         (filename (file-name-nondirectory current-path))
         ;; Extract the stem: "auth.component" or "auth.service"
         (stem (cond
                ((string-match "^\\(.+\\)\\.spec\\.ts$" filename)
                 (match-string 1 filename))
                ((string-match "^\\(.+\\)\\.[^.]+$" filename)
                 (match-string 1 filename))))
         (moved 0))
    (unless stem
      (user-error "Cannot determine file stem from %s" filename))
    (unless (file-directory-p destination)
      (make-directory destination :parents))
    ;; Find and move all files matching the stem
    (dolist (file (directory-files base-dir t (format "^%s\\." (regexp-quote stem))))
      (let ((new-file (expand-file-name (file-name-nondirectory file) destination)))
        (rename-file file new-file t)
        (cl-incf moved)))
    ;; Update imports for the main .ts file
    (let ((old-import (expand-file-name stem base-dir))
          (new-import (expand-file-name stem destination)))
      (angular-update-import-paths old-import new-import))
    (message "Moved %d files to %s" moved (abbreviate-file-name destination))))

(defun angular-rename-component ()
  "Rename an Angular component (files, class, selector, imports)."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (components (angular-list-components root))
         (old-name (completing-read "Component to rename: " components nil t))
         (new-name (read-string "New name (kebab-case): "))
         (prefix (angular-get-prefix))
         (old-selector (format "%s-%s" prefix old-name))
         (new-selector (format "%s-%s" prefix new-name))
         (old-dir (let ((files (angular--project-files root
                                (format "^%s\\.component\\.ts$" (regexp-quote old-name)))))
                    (when files (directory-file-name (file-name-directory (car files))))))
         (new-dir (when old-dir
                    (concat (file-name-directory (directory-file-name old-dir)) new-name))))
    (unless old-dir
      (user-error "Component '%s' not found" old-name))
    ;; Rename directory
    (when (not (string= old-dir new-dir))
      (rename-file old-dir new-dir)
      (angular-update-import-paths old-dir new-dir))
    ;; Update class name, selector, templateUrl, styleUrls in .ts files
    (dolist (file (directory-files-recursively
                   new-dir (format "%s\\.component\\(\\.spec\\)?\\.ts$" (regexp-quote old-name))))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward (format "\\b%sComponent\\b" (angular-pascal-case old-name)) nil t)
          (replace-match (format "%sComponent" (angular-pascal-case new-name))))
        (goto-char (point-min))
        (while (re-search-forward (format "selector:\\s-*['\"]%s['\"]" (regexp-quote old-selector)) nil t)
          (replace-match (format "selector: '%s'" new-selector)))
        (goto-char (point-min))
        (while (re-search-forward (format "%s\\.component" (regexp-quote old-name)) nil t)
          (replace-match (format "%s.component" new-name)))
        (write-region (point-min) (point-max) file)))
    ;; Rename files
    (dolist (file (directory-files new-dir t (regexp-quote old-name)))
      (let ((new-file (replace-regexp-in-string (regexp-quote old-name) new-name file)))
        (unless (string= file new-file)
          (rename-file file new-file))))
    ;; Update imports referencing the component
    (angular-update-import-paths (format "%s.component" old-name)
                                 (format "%s.component" new-name))
    (message "Renamed '%s' to '%s'" old-name new-name)))

(defun angular-list-components (root)
  "List all component names in the Angular project at ROOT."
  (let ((files (angular--project-files root "\\.component\\.ts$")))
    (delq nil
          (mapcar (lambda (file)
                    (when (string-match "\\([a-z0-9-]+\\)\\.component\\.ts$"
                                        (file-name-nondirectory file))
                      (match-string 1 (file-name-nondirectory file))))
                  files))))

(defun angular-pascal-case (kebab-case-name)
  "Convert KEBAB-CASE-NAME to PascalCase."
  (mapconcat #'capitalize (split-string kebab-case-name "-") ""))

;;; ============================================================
;;; Namespace / Barrel Exports
;;; ============================================================

(defun angular-create-barrel ()
  "Create or update an index.ts barrel export for a directory.
Scans for .ts files and exports their public symbols."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (dir (read-directory-name "Create barrel for: "
                                   (expand-file-name "src/app" root)))
         (index-file (expand-file-name "index.ts" dir))
         (ts-files (cl-remove-if
                    (lambda (f)
                      (let ((name (file-name-nondirectory f)))
                        (or (string= name "index.ts")
                            (string-suffix-p ".spec.ts" name)
                            (string-suffix-p ".module.ts" name))))
                    (directory-files dir nil "\\.ts$")))
         (exports '()))
    (dolist (file ts-files)
      (let ((stem (file-name-sans-extension file)))
        (push (format "export * from './%s';" stem) exports)))
    (setq exports (sort exports #'string<))
    (with-temp-file index-file
      (insert (string-join exports "\n") "\n"))
    (message "Barrel created: %s (%d exports)" (abbreviate-file-name index-file) (length exports))
    (find-file index-file)))

(defun angular-create-barrel-recursive ()
  "Create barrel exports for a directory and all subdirectories.
Each subdirectory gets its own index.ts, parent re-exports children."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (dir (read-directory-name "Create barrels for: "
                                   (expand-file-name "src/app" root)))
         (count 0))
    (angular--create-barrels-in dir)
    ;; Count how many index.ts files were created
    (dolist (f (directory-files-recursively dir "^index\\.ts$"))
      (cl-incf count))
    (message "Created %d barrel files under %s" count (abbreviate-file-name dir))))

(defun angular--create-barrels-in (dir)
  "Recursively create barrel exports in DIR."
  (let ((subdirs (cl-remove-if-not
                  #'file-directory-p
                  (mapcar (lambda (f) (expand-file-name f dir))
                          (cl-remove-if
                           (lambda (f) (or (string= f ".") (string= f "..")
                                           (string-prefix-p "." f)))
                           (directory-files dir)))))
        (ts-files (cl-remove-if
                   (lambda (f)
                     (let ((name (file-name-nondirectory f)))
                       (or (string= name "index.ts")
                           (string-suffix-p ".spec.ts" name))))
                   (directory-files dir nil "\\.ts$")))
        (exports '()))
    ;; Recurse into subdirectories first
    (dolist (subdir subdirs)
      (angular--create-barrels-in subdir)
      ;; Re-export subdirectory if it has an index.ts
      (when (file-exists-p (expand-file-name "index.ts" subdir))
        (push (format "export * from './%s';" (file-name-nondirectory subdir))
              exports)))
    ;; Export local .ts files
    (dolist (file ts-files)
      (push (format "export * from './%s';" (file-name-sans-extension file))
            exports))
    ;; Write index.ts if there's anything to export
    (when exports
      (let ((index-file (expand-file-name "index.ts" dir)))
        (with-temp-file index-file
          (insert (string-join (sort exports #'string<) "\n") "\n"))))))

(defun angular-convert-to-barrel-imports ()
  "Convert deep imports to barrel imports in the current file.
Replaces './auth/auth.component' with './auth' when an index.ts exists."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (import-regex "from\\s-+\\(['\"]\\)\\([^'\"]+\\)\\1")
         (file-dir (file-name-directory (or buffer-file-name default-directory)))
         (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward import-regex nil t)
        (let* ((quote-char (match-string 1))
               (import-path (match-string 2))
               (expanded (expand-file-name import-path file-dir))
               ;; Resolve to the directory containing the imported file
               (parent-dir (cond
                            ;; If it resolves to a directory with index.ts, that's the barrel
                            ((file-exists-p (expand-file-name "index.ts" expanded))
                             nil) ; already a barrel import
                            ;; Otherwise get the parent directory
                            ((file-name-directory (concat expanded ".ts"))))))
          (when (and (not (string-prefix-p "@" import-path))
                     parent-dir
                     (file-exists-p (expand-file-name "index.ts" parent-dir)))
            ;; Check if the barrel re-exports what we need
            (let ((barrel-path (file-relative-name
                                (directory-file-name parent-dir) file-dir)))
              (unless (string-prefix-p "." barrel-path)
                (setq barrel-path (concat "./" barrel-path)))
              (unless (string= import-path barrel-path)
                (replace-match (format "from %s%s%s" quote-char barrel-path quote-char))
                (cl-incf count)))))))
    (message "Converted %d imports to barrel imports" count)))

(defun angular-add-path-alias ()
  "Add a path alias to tsconfig.json (e.g. @shared/* → src/app/shared/*)."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (tsconfig (expand-file-name "tsconfig.json" root))
         (alias (read-string "Alias (e.g. @shared): " "@"))
         (target-dir (read-directory-name "Maps to: "
                                          (expand-file-name "src/app" root)))
         (relative (file-relative-name target-dir root))
         (path-value (concat (directory-file-name relative) "/*"))
         (alias-key (concat alias "/*")))
    (unless (file-exists-p tsconfig)
      (user-error "No tsconfig.json found"))
    (with-current-buffer (find-file-noselect tsconfig)
      (goto-char (point-min))
      (if (re-search-forward "\"paths\"\\s-*:\\s-*{" nil t)
          ;; Add to existing paths
          (progn
            (end-of-line)
            (insert (format "\n      \"%s\": [\"%s\"]," alias-key path-value)))
        ;; Create paths section inside compilerOptions
        (goto-char (point-min))
        (if (re-search-forward "\"compilerOptions\"\\s-*:\\s-*{" nil t)
            (progn
              (end-of-line)
              (insert (format "\n    \"paths\": {\n      \"%s\": [\"%s\"]\n    },"
                              alias-key path-value)))
          (user-error "No compilerOptions found in tsconfig.json")))
      (save-buffer)
      (message "Added alias %s → %s" alias-key path-value))))

(defun angular-convert-to-alias-imports ()
  "Convert relative imports to alias imports in the current file.
Uses path aliases from tsconfig.json."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (aliases (angular--parse-path-aliases root))
         (file-dir (file-name-directory (or buffer-file-name default-directory)))
         (import-regex "from\\s-+\\(['\"]\\)\\([^'\"]+\\)\\1")
         (count 0))
    (unless aliases
      (user-error "No path aliases found in tsconfig.json"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward import-regex nil t)
        (let* ((quote-char (match-string 1))
               (import-path (match-string 2)))
          (when (and (not (string-prefix-p "@" import-path))
                     (or (string-prefix-p "./" import-path)
                         (string-prefix-p "../" import-path)))
            (let* ((expanded (expand-file-name import-path file-dir))
                   (rel-to-root (file-relative-name expanded root))
                   (best-alias nil)
                   (best-remainder nil))
              ;; Find the best matching alias
              (dolist (alias aliases)
                (let ((alias-name (car alias))
                      (alias-path (cdr alias)))
                  (when (string-prefix-p alias-path rel-to-root)
                    (let ((remainder (substring rel-to-root (length alias-path))))
                      (when (or (null best-alias)
                                (> (length alias-path) (length (cdr best-alias))))
                        (setq best-alias (cons alias-name alias-path)
                              best-remainder remainder))))))
              (when best-alias
                (let ((new-import (concat (replace-regexp-in-string "/\\*$" "" (car best-alias))
                                          "/" (string-remove-prefix "/" best-remainder))))
                  ;; Remove trailing .ts extension if present
                  (setq new-import (replace-regexp-in-string "\\.ts$" "" new-import))
                  (replace-match (format "from %s%s%s" quote-char new-import quote-char))
                  (cl-incf count))))))))
    (message "Converted %d imports to alias imports" count)))

(defun angular--parse-path-aliases (root)
  "Parse path aliases from tsconfig.json in ROOT.
Returns alist of (alias-pattern . directory-relative-to-root)."
  (let ((tsconfig (expand-file-name "tsconfig.json" root))
        (aliases '()))
    (when (file-exists-p tsconfig)
      (with-temp-buffer
        (insert-file-contents tsconfig)
        (goto-char (point-min))
        (when (re-search-forward "\"paths\"\\s-*:\\s-*{" nil t)
          (let ((paths-start (point)))
            (when (re-search-forward "}" nil t)
              (let ((paths-text (buffer-substring-no-properties paths-start (point))))
                (with-temp-buffer
                  (insert paths-text)
                  (goto-char (point-min))
                  (while (re-search-forward
                          "\"\\([^\"]+\\)\"\\s-*:\\s-*\\[\\s-*\"\\([^\"]+\\)\"" nil t)
                    (let ((alias (match-string 1))
                          (path (match-string 2)))
                      ;; Convert "src/app/shared/*" to "src/app/shared/"
                      (setq path (replace-regexp-in-string "/\\*$" "/" path))
                      (push (cons alias path) aliases))))))))))
    (nreverse aliases)))

(defun angular-create-feature ()
  "Scaffold a feature namespace directory with barrel export.
Creates the directory, an index.ts, and optionally generates components."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (name (read-string "Feature name (kebab-case): "))
         (parent (read-directory-name "Parent directory: "
                                      (expand-file-name "src/app" root)))
         (feature-dir (expand-file-name name parent))
         (generate (y-or-n-p "Generate a component for this feature? ")))
    (make-directory feature-dir t)
    ;; Create barrel
    (with-temp-file (expand-file-name "index.ts" feature-dir)
      (insert (format "// %s feature\n" name)))
    ;; Optionally generate component
    (when generate
      (compile (format "%s generate component %s/%s --flat"
                       (angular-cli-executable) name name)))
    ;; Offer to add path alias
    (when (y-or-n-p (format "Add @%s path alias? " name))
      (let ((alias (format "@%s" name))
            (target (file-relative-name feature-dir root)))
        (angular-add-path-alias-internal root alias target)))
    (find-file (expand-file-name "index.ts" feature-dir))
    (message "Feature '%s' created" name)))

(defun angular-add-path-alias-internal (root alias target-dir)
  "Add path ALIAS → TARGET-DIR to tsconfig.json in ROOT."
  (let ((tsconfig (expand-file-name "tsconfig.json" root))
        (path-value (concat (directory-file-name target-dir) "/*"))
        (alias-key (concat alias "/*")))
    (with-current-buffer (find-file-noselect tsconfig)
      (goto-char (point-min))
      (if (re-search-forward "\"paths\"\\s-*:\\s-*{" nil t)
          (progn
            (end-of-line)
            (insert (format "\n      \"%s\": [\"%s\"]," alias-key path-value)))
        (goto-char (point-min))
        (when (re-search-forward "\"compilerOptions\"\\s-*:\\s-*{" nil t)
          (end-of-line)
          (insert (format "\n    \"paths\": {\n      \"%s\": [\"%s\"]\n    },"
                          alias-key path-value))))
      (save-buffer))))

;;; ============================================================
;;; Module → Standalone Migration
;;; ============================================================

(defun angular-convert-to-standalone ()
  "Convert a module-based component to standalone.
Adds standalone: true and imports array to @Component decorator.
Removes the component from its NgModule declarations."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (components (angular-list-components root))
         (name (completing-read "Convert to standalone: " components nil t))
         (files (angular--project-files root
                  (format "^%s\\.component\\.ts$" (regexp-quote name))))
         (component-file (car files)))
    (unless component-file
      (user-error "Component '%s' not found" name))
    ;; Add standalone: true to @Component
    (with-current-buffer (find-file-noselect component-file)
      (goto-char (point-min))
      (if (re-search-forward "@Component(\\s-*{" nil t)
          (let ((decorator-start (point)))
            ;; Check if already standalone
            (if (save-excursion
                  (re-search-forward "standalone\\s-*:" nil t))
                (message "%s is already standalone" name)
              ;; Add standalone: true after the opening brace
              (insert "\n  standalone: true,")
              ;; Check if imports array exists, add empty one if not
              (goto-char decorator-start)
              (unless (save-excursion
                        (re-search-forward "\\bimports\\s-*:" nil t))
                (goto-char decorator-start)
                (insert "\n  imports: [],"))
              (save-buffer)
              ;; Remove from module declarations
              (angular--remove-from-module-declarations root name)
              (message "Converted '%s' to standalone" name)))
        (user-error "No @Component decorator found in %s" component-file)))))

(defun angular--remove-from-module-declarations (root component-name)
  "Remove COMPONENT-NAME from NgModule declarations in ROOT."
  (let* ((class-name (concat (angular-pascal-case component-name) "Component"))
         (module-files (angular--project-files root "\\.module\\.ts$"))
         (removed 0))
    (dolist (file module-files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((original (buffer-string)))
          ;; Find declarations array and remove the component from it
          (goto-char (point-min))
          (when (re-search-forward "declarations\\s-*:\\s-*\\[" nil t)
            (let ((decl-start (point)))
              (when (re-search-forward "\\]" nil t)
                (let ((decl-end (point)))
                  (goto-char decl-start)
                  (while (re-search-forward
                          (format ",?\\s-*%s\\s-*,?" (regexp-quote class-name))
                          decl-end t)
                    (let ((match-text (match-string 0)))
                      (replace-match
                       (if (and (string-prefix-p "," match-text)
                                (string-suffix-p "," match-text))
                           ","
                         "")))
                    (cl-incf removed))))))
          ;; Remove the import statement if it only imports this component
          (goto-char (point-min))
          (while (re-search-forward
                  (format "import\\s-+{\\s-*%s\\s-*}\\s-+from\\s-+['\"][^'\"]+['\"];?\n?"
                          (regexp-quote class-name))
                  nil t)
            (replace-match ""))
          (when (not (equal (buffer-string) original))
            (write-region (point-min) (point-max) file)))))
    (when (> removed 0)
      (message "Removed %s from %d module declaration(s)" class-name removed))))

;;; ============================================================
;;; CLI Commands
;;; ============================================================

(defun angular-serve ()
  "Start the Angular dev server."
  (interactive)
  (let ((default-directory (or (angular-find-project-root)
                               (user-error "Not in an Angular project"))))
    (compile (format "%s serve" (angular-cli-executable)))))

(defun angular-build ()
  "Build the Angular project."
  (interactive)
  (let ((default-directory (or (angular-find-project-root)
                               (user-error "Not in an Angular project"))))
    (compile (format "%s build" (angular-cli-executable)))))

(defun angular-test ()
  "Run Angular tests."
  (interactive)
  (let ((default-directory (or (angular-find-project-root)
                               (user-error "Not in an Angular project"))))
    (compile (format "%s test" (angular-cli-executable)))))

(defun angular-lint ()
  "Run Angular linter."
  (interactive)
  (let ((default-directory (or (angular-find-project-root)
                               (user-error "Not in an Angular project"))))
    (compile (format "%s lint" (angular-cli-executable)))))

;;; ============================================================
;;; Route Navigation
;;; ============================================================

(defun angular-open-routes ()
  "Open the app routes file."
  (interactive)
  (let* ((root (or (angular-find-project-root)
                   (user-error "Not in an Angular project")))
         (candidates (append
                      (angular--project-files root "app\\.routes\\.ts$")
                      (angular--project-files root "app-routing\\.module\\.ts$")
                      (angular--project-files root "routes\\.ts$"))))
    (if candidates
        (if (= (length candidates) 1)
            (find-file (car candidates))
          (let* ((src (expand-file-name "src" root))
                 (labels (mapcar (lambda (f) (file-relative-name f src)) candidates))
                 (choice (completing-read "Routes file: " labels nil t)))
            (find-file (expand-file-name choice src))))
      (message "No routes file found"))))

;;; ============================================================
;;; Console Log Helpers
;;; ============================================================

(defun angular-console-log-thing-at-point ()
  "Insert console.log for expression at point."
  (interactive)
  (let ((expr ""))
    (save-excursion
      (while (or (looking-back "\\(?:\\sw\\|\\s_\\|\\.\\)" 1)
                 (looking-back ")" 1))
        (backward-char))
      (let ((start (point)))
        (while (looking-at "\\(?:\\sw\\|\\s_\\|\\.\\)+")
          (forward-sexp))
        (when (looking-at "(")
          (forward-sexp))
        (setq expr (buffer-substring-no-properties start (point)))))
    (when (not (string-empty-p expr))
      (save-excursion
        (end-of-line)
        (newline-and-indent)
        (insert (format "console.log('%s', %s);" expr expr))))))

(defun angular-remove-all-console-logs ()
  "Delete all lines containing console.log in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "\\bconsole\\.log\\s-*(" nil t)
        (beginning-of-line)
        (kill-whole-line)
        (cl-incf count))
      (message "Removed %d console.log statements" count))))

;;; ============================================================
;;; Transient Menu
;;; ============================================================

;;;###autoload
(defun angular-transient ()
  "Angular transient menu."
  (interactive)
  (require 'transient)
  (transient-define-prefix angular-menu ()
    "Angular"
    [["Navigate"
      ("TAB" "Cycle ts/html/scss/spec" angular-cycle)
      ("j c" "Jump → Component" angular-jump-to-component)
      ("j t" "Jump → Template" angular-jump-to-template)
      ("j v" "Jump → Stylesheet" angular-jump-to-stylesheet)
      ("j x" "Jump → Test" angular-jump-to-test)]
     ["Open"
      ("o c" "Component" angular-open-component)
      ("o t" "Template" angular-open-component-template)
      ("o v" "Stylesheet" angular-open-component-stylesheet)
      ("o s" "Service" angular-open-service)
      ("o d" "Directive" angular-open-directive)
      ("o p" "Pipe" angular-open-pipe)
      ("o g" "Guard" angular-open-guard)
      ("o m" "Module" angular-open-module)
      ("o f" "Interface" angular-open-interface)]
     ["Refactor"
      ("g" "Generate" angular-generate)
      ("r" "Rename Component" angular-rename-component)
      ("m d" "Move Directory" angular-move-directory)
      ("m f" "Move File" angular-move-file)
      ("S" "→ Standalone" angular-convert-to-standalone)
      ("c" "Console.log" angular-console-log-thing-at-point)
      ("d" "Remove Logs" angular-remove-all-console-logs)]
     ["Namespace"
      ("n f" "New Feature" angular-create-feature)
      ("n b" "Create Barrel" angular-create-barrel)
      ("n B" "Barrels (recursive)" angular-create-barrel-recursive)
      ("n i" "→ Barrel Imports" angular-convert-to-barrel-imports)
      ("n a" "Add Path Alias" angular-add-path-alias)
      ("n @" "→ Alias Imports" angular-convert-to-alias-imports)]]
    [["Run"
      ("R s" "Serve" angular-serve)
      ("R b" "Build" angular-build)
      ("R t" "Test" angular-test)
      ("R l" "Lint" angular-lint)]
     ["Project"
      ("p" "angular.json" angular-project-config)
      ("P" "Routes" angular-open-routes)
      ("h d" "API Docs" angular-lookup-word)
      ("h s" "Search Docs" angular-search-word)]])
  (angular-menu))

;;; ============================================================
;;; Angular Template Mode
;;; ============================================================

;;;###autoload
(if (fboundp 'html-ts-mode)
    (define-derived-mode angular-template-mode html-ts-mode "Angular Template"
      "Major mode for Angular template files with LSP support.")
  (define-derived-mode angular-template-mode mhtml-mode "Angular Template"
    "Major mode for Angular template files with LSP support."))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.component\\.html\\'" . angular-template-mode))

;;; ============================================================
;;; Minor Mode
;;; ============================================================

;;;###autoload
(define-minor-mode angular-mode
  "Minor mode for working with Angular CLI."
  :lighter " Angular"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a a") 'angular-transient)
            (define-key map (kbd "C-c a TAB") 'angular-cycle)
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
            (define-key map (kbd "C-c a o f") 'angular-open-interface)
            (define-key map (kbd "C-c a j c") 'angular-jump-to-component)
            (define-key map (kbd "C-c a j t") 'angular-jump-to-template)
            (define-key map (kbd "C-c a j v") 'angular-jump-to-stylesheet)
            (define-key map (kbd "C-c a j x") 'angular-jump-to-test)
            (define-key map (kbd "C-c a c") 'angular-console-log-thing-at-point)
            (define-key map (kbd "C-c a d") 'angular-remove-all-console-logs)
            (define-key map (kbd "C-c a r") 'angular-rename-component)
            (define-key map (kbd "C-c a m d") 'angular-move-directory)
            (define-key map (kbd "C-c a m f") 'angular-move-file)
            (define-key map (kbd "C-c a S") 'angular-convert-to-standalone)
            (define-key map (kbd "C-c a n f") 'angular-create-feature)
            (define-key map (kbd "C-c a n b") 'angular-create-barrel)
            (define-key map (kbd "C-c a n B") 'angular-create-barrel-recursive)
            (define-key map (kbd "C-c a n i") 'angular-convert-to-barrel-imports)
            (define-key map (kbd "C-c a n a") 'angular-add-path-alias)
            (define-key map (kbd "C-c a n @") 'angular-convert-to-alias-imports)
            (define-key map (kbd "C-c a R s") 'angular-serve)
            (define-key map (kbd "C-c a R b") 'angular-build)
            (define-key map (kbd "C-c a R t") 'angular-test)
            (define-key map (kbd "C-c a R l") 'angular-lint)
            (define-key map (kbd "C-c a P") 'angular-open-routes)
            map)
  (when angular-mode
    (add-hook 'xref-backend-functions #'angular-xref-backend nil t)))

(defun angular-mode-maybe ()
  "Enable `angular-mode' if the current file is in an Angular project."
  (when (and buffer-file-name
             (locate-dominating-file buffer-file-name "angular.json"))
    (angular-mode 1)))

(define-globalized-minor-mode global-angular-mode angular-mode
  angular-mode-maybe)

(provide 'angular-mode)
;;; angular-mode.el ends here
