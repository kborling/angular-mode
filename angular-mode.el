;;; angular-mode.el --- Emacs Minor Mode for Working with Angular CLI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Kevin Borling <kborling@protonmail.com>
;;
;; Author: Kevin Borling <kborling@protonmail.com>
;; Created: October 16, 2023
;; Version: 0.2.0
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
  "List files matching PATTERN under ROOT/src, using fd if available."
  (let ((src-dir (expand-file-name "src" root)))
    (when (file-directory-p src-dir)
      (if (executable-find "fd")
          (let ((default-directory src-dir))
            (mapcar (lambda (f) (expand-file-name f src-dir))
                    (split-string
                     (shell-command-to-string
                      (format "fd --type f %s" (shell-quote-argument pattern)))
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
     ["Actions"
      ("g" "Generate" angular-generate)
      ("r" "Rename Component" angular-rename-component)
      ("m d" "Move Directory" angular-move-directory)
      ("m f" "Move File" angular-move-file)
      ("c" "Console.log" angular-console-log-thing-at-point)
      ("d" "Remove Logs" angular-remove-all-console-logs)]
     ["Project"
      ("p" "angular.json" angular-project-config)
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
            map)
  (when angular-mode
    (add-hook 'xref-backend-functions #'angular-xref-backend nil t)))

(define-globalized-minor-mode global-angular-mode angular-mode
  (lambda () (angular-mode 1)))

(provide 'angular-mode)
;;; angular-mode.el ends here
