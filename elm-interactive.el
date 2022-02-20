;;; elm-interactive.el --- Run an interactive Elm session. -*- lexical-binding: t -*-

;; Copyright (C) 2015, 2016  Bogdan Popa

;; Author: Bogdan Popa
;; URL: https://github.com/jcollard/elm-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:
(require 'ansi-color)
(require 'comint)
(require 'compile)
(require 'cl-lib)
(require 'elm-font-lock)
(require 'elm-util)
(require 'f)
(require 'json)
(require 'let-alist)
(require 'rx)
(require 's)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url)

(defvar elm-interactive--seen-prompt nil
  "Non-nil represents the fact that a prompt has been spotted.")
(make-variable-buffer-local 'elm-interactive--seen-prompt)

(defvar elm-interactive--current-project nil)
(defvar elm-interactive--process-name "elm")
(defvar elm-interactive--buffer-name "*elm*")
(defvar elm-reactor--buffer-name "*elm-reactor*")

(defcustom elm-interactive-command '("elm" "repl")
  "The Elm REPL command."
  :type '(repeat string)
  :group 'elm)

(defcustom elm-interactive-arguments '()
  "Command line arguments to pass to the Elm REPL command."
  :type '(repeat string)
  :group 'elm)

(defvar elm-interactive-prompt-regexp "^[>|] "
  "Prompt for `elm-interactive'.")

(defcustom elm-reactor-command '("elm" "reactor")
  "The Elm Reactor command."
  :type '(repeat string)
  :group 'elm)

(defcustom elm-reactor-port 8000
  "The Elm Reactor port."
  :type '(integer)
  :group 'elm)

(defcustom elm-reactor-address "127.0.0.1"
  "The Elm Reactor address."
  :type '(string)
  :group 'elm)

(defcustom elm-reactor-arguments `((:eval (format "--port=%s" elm-reactor-port)))
  "Command line arguments to pass to the Elm Reactor command.
Args are expanded using `elm--expand-args'."
  :type '(repeat string)
  :group 'elm)

(defvar elm-compile--buffer-name "*elm-make*")

(defcustom elm-compile-command '("elm" "make")
  "The Elm compilation command."
  :type '(repeat string)
  :group 'elm)

(defcustom elm-compile-arguments '("--output=elm.js")
  "Command line arguments to pass to the Elm compilation command."
  :type '(repeat string)
  :group 'elm)

(defvar elm-compile-error-regexp-alist-alist
  '((elm-file "-- [^-]+ -+ \\(.+\\)$" 1 nil)
    (elm-line "^\\([0-9]+\\)|" nil 1))
  "Regexps to match Elm compiler errors in compilation buffer.")

(defvar elm-compile-error-regexp-alist '(elm-line elm-file))

(dolist (alist elm-compile-error-regexp-alist-alist)
  (add-to-list 'compilation-error-regexp-alist-alist alist))

(dolist (symbol elm-compile-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist symbol))

(defcustom elm-create-package-command "elm make --yes"
  "The command that is used to initialize a new package definition."
  :type '(string)
  :group 'elm)

(defvar elm-package--contents nil
  "The contents of the Elm package catalog.")

(defvar elm-package--dependencies nil
  "The package dependencies for the current Elm package.")

(defvar elm-package--cache nil
  "A cache for extended package information.")

(defvar elm-package--marked-contents nil)

(defvar elm-package--working-dir nil)

(defvar elm-package-compile-buffer-name "*elm-package-compile*")

(defvar elm-package-buffer-name "*elm-package*")

(defcustom elm-package-command '("elm")
  "The Elm package command."
  :type '(repeat string)
  :group 'elm)

(defcustom elm-package-arguments '("install")
  "Command line arguments to pass to the Elm package command."
  :type '(repeat string)
  :group 'elm)

(defcustom elm-package-catalog-root
  "http://package.elm-lang.org/"
  "The root URI for the Elm package catalog."
  :type '(string)
  :group 'elm)

(defvar elm-package-catalog-format
  [(" " 1 nil)
   ("Name" 30 t)
   ("Version" 7 nil)
   ("Status" 10 t)
   ("Summary" 80 nil)]
  "The format of the package list header.")

(defvar elm-interactive-mode-map
  (let ((map (make-keymap)))
    (define-key map "\t" #'completion-at-point)
    (define-key map (kbd "C-a") #'elm-interactive-mode-beginning)
    (define-key map (kbd "C-c C-z") #'elm-repl-return-to-origin)
    map)
  "Keymap for Elm interactive mode.")

(defvar elm-repl--origin nil
  "Marker for buffer/position from which we jumped to this repl.")

(defcustom elm-sort-imports-on-save nil
  "Controls whether or not imports should be automaticaly reordered on save."
  :type 'boolean
  :group 'elm)

(defvar elm-package-mode-map
  (let ((map (make-keymap)))
    (define-key map "g" #'elm-package-refresh)
    (define-key map "n" #'elm-package-next)
    (define-key map "p" #'elm-package-prev)
    (define-key map "v" #'elm-package-view)
    (define-key map "m" #'elm-package-mark)
    (define-key map "i" #'elm-package-mark)
    (define-key map "u" #'elm-package-unmark)
    (define-key map "x" #'elm-package-install)
    map)
  "Keymap for Elm package mode.")

(defun elm-interactive-mode-beginning ()
  "Go to the start of the line."
  (interactive)
  (beginning-of-line)
  (goto-char (+ 2 (point))))

(defun elm-interactive--get-process-buffer ()
  "Get the REPL process buffer."
  (get-buffer-process elm-interactive--buffer-name))

(defun elm-interactive--spot-prompt (_string)
  "Spot the prompt, _STRING is ignored."
  (let ((proc (elm-interactive--get-process-buffer)))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))
        (if (re-search-backward comint-prompt-regexp
                                (line-beginning-position) t)
            (setq elm-interactive--seen-prompt t))))))

(defun elm-interactive--wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt or TIMEOUT.
The process PROC should be associated to a comint buffer.

Stolen from ‘haskell-mode’."
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or elm-interactive--seen-prompt
                      (setq elm-interactive--seen-prompt
                            (re-search-forward comint-prompt-regexp nil t))
                      (not (accept-process-output proc timeout))))))
    (unless elm-interactive--seen-prompt
      (error "Can't find the prompt"))))

(defun elm-interactive--send-command (command)
  "Send a COMMAND to the REPL."
  (let ((proc (elm-interactive--get-process-buffer)))
    (with-current-buffer (process-buffer proc)
      (elm-interactive--wait-for-prompt proc 10)
      (goto-char (process-mark proc))
      (insert-before-markers command)
      (move-marker comint-last-input-end (point))
      (setq elm-interactive--seen-prompt nil)
      (comint-send-string proc command))))

(defun elm-interactive-kill-current-session ()
  "Stop the current REPL session and delete its buffer."
  (interactive)
  (when (and elm-interactive--current-project
             (get-buffer-process elm-interactive--buffer-name)
             (not (equal elm-interactive--current-project (elm--find-dependency-file-path)))
             (y-or-n-p "This will kill your existing REPL session.  Continue? "))
    (delete-process elm-interactive--buffer-name)
    (kill-buffer elm-interactive--buffer-name)))

;;;###autoload
(define-derived-mode elm-interactive-mode comint-mode "Elm Interactive"
  "Major mode for `elm-interactive'.

\\{elm-interactive-mode-map}"

  (setq-local comint-prompt-regexp elm-interactive-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-use-prompt-regexp t)

  (add-hook 'comint-output-filter-functions #'elm-interactive--spot-prompt nil t)

  (elm--font-lock-enable))

;;;###autoload
(defun elm-interactive ()
  "Run an inferior instance of `elm-repl' inside Emacs."
  (interactive)
  (elm-interactive-kill-current-session)
  (let* ((default-directory (elm--find-dependency-file-path))
         (origin (point-marker))
         (cmd (append (elm--ensure-list elm-interactive-command) elm-interactive-arguments)))

    (setq elm-interactive--current-project default-directory)
    (let ((buffer (get-buffer-create elm-interactive--buffer-name)))
      (apply #'make-comint-in-buffer elm-interactive--process-name buffer
             (car cmd) nil (cdr cmd))
      (with-current-buffer buffer
        (elm-interactive-mode)
        (setq-local elm-repl--origin origin))
      (pop-to-buffer buffer))))

;;;###autoload
(define-obsolete-function-alias 'run-elm-interactive 'elm-interactive "2020-04")

(defun elm-repl-return-to-origin ()
  "Jump back to the location from which we last jumped to the repl."
  (interactive)
  (let* ((pos elm-repl--origin)
         (buffer (get-buffer (marker-buffer pos))))
    (when buffer
      (pop-to-buffer buffer)
      (goto-char pos))))

;;;###autoload
(defun elm-repl-load ()
  "Load an interactive REPL if there isn't already one running.
Changes the current root directory to be the directory with the closest
package json if one exists otherwise sets it to be the working directory
of the file specified."
  (interactive)
  (save-buffer)
  (let ((import-statement (elm--build-import-statement)))
    (elm-interactive)
    (elm-interactive--send-command ":reset\n")
    (elm-interactive--send-command import-statement)))

;;;###autoload
(defun elm-repl-push (beg end)
  "Push the region from BEG to END to an interactive REPL."
  (interactive "r")
  (let* ((to-push (buffer-substring-no-properties beg end))
         (lines (split-string (s-trim-right to-push) "\n")))
    (elm-interactive)
    (dolist (line lines)
      (elm-interactive--send-command (concat line " \\\n")))
    (elm-interactive--send-command "\n")))

;;;###autoload
(defun elm-repl-push-decl ()
  "Push the current top level declaration to the REPL."
  (interactive)
  (let ((lines (elm--get-decl)))
    (elm-interactive)
    (dolist (line lines)
      (elm-interactive--send-command (concat line " \\\n")))
    (elm-interactive--send-command "\n")))

(defun elm--ensure-list (v)
  "Return V if it is a list, otherwise a single-element list containing V."
  (if (consp v)
      v
    (list v)))

(defun elm--expand-args (args)
  "Expand any `(:eval ...)' entries in ARGS by evaluating them."
  (mapcar (lambda (arg)
            (pcase arg
              (`(:eval ,sexp) (eval sexp))
              (_ arg)))
          args))

;;; Reactor:
;;;###autoload
(defun elm-reactor ()
  "Run the Elm reactor process."
  (interactive)
  (let ((default-directory (elm--find-dependency-file-path))
        (cmd (elm--expand-args (append (elm--ensure-list elm-reactor-command) elm-reactor-arguments))))
    (with-current-buffer (get-buffer-create elm-reactor--buffer-name)
      (comint-mode)
      (ansi-color-for-comint-mode-on)
      (let ((proc (get-buffer-process (current-buffer))))
        (if (and proc (process-live-p proc))
            (progn
              (message "Restarting elm-reactor")
              (delete-process proc))
          (message "Starting elm-reactor")))

      (let ((proc (apply #'start-process "elm reactor" elm-reactor--buffer-name
                         (car cmd) (cdr cmd))))
        (when proc
          (set-process-filter proc 'comint-output-filter))))))

;;;###autoload
(define-obsolete-function-alias 'run-elm-reactor 'elm-reactor "2020-04")

(defun elm-reactor--browse (path &optional debug)
  "Open (reactor-relative) PATH in browser with optional DEBUG.

Runs `elm-reactor' first."
  (elm-reactor)
  (browse-url (format "http://localhost:%s/%s%s" elm-reactor-port path (if debug "?debug" ""))))

;;;###autoload
(defun elm-preview-buffer (debug)
  "Preview the current buffer using Elm reactor (in debug mode if DEBUG is truthy)."
  (interactive "P")
  (let* ((fname (buffer-file-name))
         (deppath (elm--find-dependency-file-path))
         (path (f-relative fname deppath)))
    (elm-reactor--browse path debug)))

;;;###autoload
(defun elm-preview-main (debug)
  "Preview the main elm file using Elm reactor (in debug mode if DEBUG is truthy)."
  (interactive "P")
  (elm-reactor--browse (elm--find-main-file) debug))


;;; Make:
(defun elm-compile--command (file &optional output json)
  "Generate a command that will compile FILE into OUTPUT, with or without JSON reporting."
  (let ((elm-compile-arguments
         (if output
             (append (cl-remove-if (apply-partially #'string-prefix-p "--output=") elm-compile-arguments)
                     (list (concat "--output=" (expand-file-name output))))
           elm-compile-arguments)))
    (s-join " "
            (append (elm--ensure-list elm-compile-command)
                    (mapcar 'shell-quote-argument
                            (append (list file)
                                    elm-compile-arguments
                                    (when json
                                      (list "--report=json"))))))))

(defun elm-compile--filter ()
  "Filter function for compilation output."
  (ansi-color-apply-on-region (point-min) (point-max)))

(define-compilation-mode elm-compilation-mode "Elm-Compile"
  "Elm compilation mode."
  (progn
    (add-hook 'compilation-filter-hook 'elm-compile--filter nil t)))

(defun elm-compile--file (file &optional output)
  "Compile FILE into OUTPUT."
  (let ((default-directory (elm--find-dependency-file-path)))
    (with-current-buffer
        (compilation-start
         (elm-compile--command file output)
         'elm-compilation-mode
         (lambda (_) elm-compile--buffer-name)))))

(defun elm-compile--file-json (file &optional output)
  "Compile FILE into OUTPUT and return the JSON report.
The report is a list of elements sorted by their occurrence order
in the file."
  (let* ((default-directory (elm--find-dependency-file-path))
         (report (shell-command-to-string
                  (elm-compile--command file output t))))
    (when (string-prefix-p "[" report)
      (let ((json (json-read-from-string report)))
        (cl-flet ((start-line (o) (let-alist o .region.start.line)))
          (cl-sort (append json nil) (lambda (o1 o2) (< (start-line o1) (start-line o2)))))))))

(defun elm-compile--temporary ()
  "Get a compilation report for the current buffer."
  (let* ((input (make-temp-file "elm-comp-in-" nil ".elm"))
         (output (make-temp-file "elm-comp-out-" nil ".js")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) input)
          (elm-compile--file-json input output))
      (delete-file output)
      (delete-file input))))


;;;###autoload
(defun elm-compile-buffer (&optional output)
  "Compile the current buffer into OUTPUT."
  (interactive
   (when current-prefix-arg
     (list (read-file-name "Output to: "))))
  (save-some-buffers)
  (elm-compile--file (elm--buffer-local-file-name) output))

;;;###autoload
(defun elm-compile-main (&optional output)
  "Compile the main elm file into OUTPUT."
  (interactive
   (when current-prefix-arg
     (list (read-file-name "Output to: "))))
  (elm-compile--file (elm--find-main-file) output))

(defun elm-compile--ensure-saved ()
  "Ensure the current buffer has been saved."
  (when (buffer-modified-p)
    (if (y-or-n-p "Save current buffer? ")
        (save-buffer)
      (error "You must save your changes first"))))

;;;###autoload
(defun elm-compile-clean-imports (&optional prompt)
  "Remove unused imports from the current buffer, PROMPT optionally before deleting."
  (interactive "P")
  (let* ((report (elm-compile--temporary))
         (line-offset 0))
    (dolist (ob report)
      (let-alist ob
        (when (equal .tag "unused import")
          (save-excursion
            (goto-char 0)
            (forward-line (- .region.start.line 1 line-offset))
            (when (or (not prompt) (y-or-n-p "Delete this import? "))
              (dotimes (_ (1+ (- .region.end.line
                                 .region.start.line)))
                (kill-whole-line)
                (setq line-offset (1+ line-offset))))))))))

(defconst elm-import--pattern
  (let* ((upper-word '(seq upper (0+ alnum)))
         (lower-word '(seq lower (0+ alnum)))
         (ws '(or space "\n"))
         (exposing-item `(or ,lower-word
                             (seq ,upper-word
                                  (opt (0+ ,ws) "(" (0+ ,ws)
                                       (or ".." (seq ,upper-word (0+ (0+ ,ws) "," (0+ ,ws) ,upper-word)))
                                       (0+ ,ws) ")"))))
         (exposing-list `(seq ,exposing-item (0+ (0+ ,ws) "," (0+ ,ws) ,exposing-item))))
    ;; TODO: we don't yet allow for comments on lines within an import statement
    (rx-to-string
     `(seq line-start
           "import" (1+ ,ws) (group ,upper-word (0+ "." ,upper-word))
           (opt (1+ ,ws) "as" (1+ ,ws) (group ,upper-word))
           (opt (1+ ,ws) "exposing" (1+ ,ws) "(" (0+ ,ws) (group (or ".." ,exposing-list)) (0+ ,ws) ")")
           (0+ space)
           line-end)) )
  "Regex to match elm import (including multiline).
Import consists of the word \"import\", real package name, and optional
\"as\" part, and \"exposing\" part, which must occur in that (standard) order.
Each is captured as a group.")

;;;###autoload
(defun elm-sort-imports ()
  "Sort the import list in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward elm-import--pattern nil t)
      ;; Sort block of contiguous imports, separated by empty lines
      (let ((start (match-beginning 0)))
        (forward-char 1) ;; Move past newline
        (while (looking-at elm-import--pattern)
          (goto-char (1+ (match-end 0))))
        (let ((end (point)))
          (sort-regexp-fields nil elm-import--pattern "\\1" start end))))))


;;;###autoload
(defun elm-compile-add-annotations (&optional prompt)
  "Add missing type annotations to the current buffer, PROMPT optionally before inserting."
  (interactive "P")
  (let* ((report (elm-compile--temporary))
         (line-offset 0))
    (dolist (ob report)
      (let-alist ob
        (when (equal .tag "missing type annotation")
          ;; Drop the first 2 lines from .details since they contain the warning itself.
          (let ((annotation (s-join "\n" (cdr (cdr (s-split "\n" .details))))))
            (goto-char 0)
            (forward-line (+ line-offset (1- .region.start.line)))
            (setq line-offset (1+ line-offset))
            (when (or (not prompt) (y-or-n-p (format "Add annotation '%s'? " annotation)))
              (princ (format "%s\n" annotation) (current-buffer)))))))))

;;; Package:
;;;###autoload
(defun elm-create-package ()
  "Generate a new package definition in the current directory."
  (interactive)
  (when (elm--has-dependency-file)
    (error "Elm-package.json already exists"))
  (let* ((default-directory (elm--find-dependency-file-path)))
    (message "Creating elm package definition. This might take a minute...")
    (shell-command elm-create-package-command)))

(defun elm-package--build-uri (&rest segments)
  "Build a URI by combining the package catalog root and SEGMENTS."
  (concat elm-package-catalog-root (s-join "/" segments)))

(defun elm-package--format-entry (entry index)
  "Format a package '(INDEX ENTRY) for display in the package listing."
  (let-alist entry
    (let ((mark (if (member index elm-package--marked-contents)
                    "*"
                  ""))
          (button (list .name . ()))
          (status (if (member .name elm-package--dependencies)
                      "dependency"
                    "available")))
      (list index (vector mark button (elt .versions 0) status .summary)))))

(defun elm-package--entries ()
  "Return the formatted package list."
  (seq-map-indexed #'elm-package--format-entry elm-package--contents))

(defun elm-package--get-marked-packages ()
  "Get packages that are marked for installation."
  (mapcar (lambda (id)
            (let-alist (nth id elm-package--contents)
              .name))
          elm-package--marked-contents))

(defun elm-package--get-marked-install-commands ()
  "Get a list of the commands required to install the marked packages."
  (mapcar (lambda (package)
            (s-join " " (append (elm--ensure-list elm-package-command) elm-package-arguments (list package))))
          (elm-package--get-marked-packages)))

(defun elm-package--read-dependencies ()
  "Read the current package's dependencies."
  (setq elm-package--working-dir (elm--find-dependency-file-path))
  (let-alist (elm--read-dependency-file)
    (setq elm-package--dependencies
          (mapcar (lambda (dep) (symbol-name (car dep)))
                  (if (consp .dependencies.direct)
                      (append .dependencies.direct .dependencies.indirect)
                    .dependencies)))))

(defun elm-package--read-json (uri)
  "Read a JSON file from a URI."
  (with-current-buffer (url-retrieve-synchronously uri)
    (goto-char (point-min))
    (re-search-forward "^ *$")
    (json-read)))

(defun elm-package--read-package ()
  "Read a package from the minibuffer."
  (completing-read "Package: " elm-package--dependencies nil t))

(defun elm-package--read-module (package)
  "Read a module from PACKAGE from the minibuffer."
  (completing-read "Module: " (elm-package-modules package) nil t))

(defun elm-package--read-module-definition (package module)
  "Read a definition from PACKAGE and MODULE from the minibuffer."
  (completing-read "Definition: " (elm-package-definitions package module) nil t))

(defun elm-package-refresh-package (package version)
  "Refresh the cache for PACKAGE with VERSION."
  (let ((documentation-uri
         (elm-package--build-uri "packages" package version "docs.json")))
    (setq elm-package--cache
          (cons `(,package . ,(elm-package--read-json documentation-uri))
                elm-package--cache))))

(defun elm-package-latest-version (package)
  "Get the latest version of PACKAGE."
  (let ((entry (assoc (intern-soft package) elm-package--contents)))
    (if (not entry)
        (error "Package not found")
      (let ((versions (cdr entry)))
        (elt versions 0)))))

(defun elm-package--ensure-cached (package)
  "Ensure that PACKAGE has been cached."
  (unless (assoc package elm-package--cache)
    (elm-package-refresh-package package (elm-package-latest-version package))))

(defun elm-package-modules (package)
  "Get PACKAGE's module list."
  (elm-package--ensure-cached package)
  (sort
   (mapcar (lambda (module)
             (let-alist module .name))
           (cdr (assoc package elm-package--cache)))
   #'string<))

(defun elm-package--select-module (package module-name)
  "Select a PACKAGE's MODULE-NAME from the cache."
  (elm-package--ensure-cached package)
  (elt (cl-remove-if-not
        (lambda (module)
          (let-alist module (equal module-name .name)))
        (cdr (assoc package elm-package--cache))) 0))

(defun elm-package-definitions (package module-name)
  "Get all of PACKAGE's MODULE-NAME's definitions."
  (let-alist (elm-package--select-module package module-name)
    (let* ((extract (lambda (x)
                      (let ((name (cdr (assq 'name x))))
                        (cons name nil))))
           (aliases (mapcar extract .aliases))
           (types (mapcar extract .types))
           (values (mapcar extract .values)))
      (append aliases types values))))

(defun elm-package-definition (package module-name definition-name)
  "Get documentation from PACKAGE's MODULE-NAME for DEFINITION-NAME."
  (let-alist (elm-package--select-module package module-name)
    (elt (cl-remove-if-not
          (lambda (x)
            (equal definition-name (cdr (assq 'name x))))
          (vconcat .aliases .types .values))
         0)))

(defun elm-package-refresh ()
  "Refresh the package catalog's contents."
  (interactive)
  (with-current-buffer elm-package-buffer-name
    (elm-package--read-dependencies)
    (tabulated-list-print :remember-pos)))

(defun elm-package-prev (&optional n)
  "Goto (Nth) previous package."
  (interactive "p")
  (elm-package-next (- n))
  (forward-line 0)
  (forward-button 1))

(defun elm-package-next (&optional n)
  "Goto (Nth) next package."
  (interactive "p")
  (dotimes (_ (abs n))
    (let ((d (cl-signum n)))
      (forward-line (if (> n 0) 1 0))
      (when (eobp)
        (forward-line -1))
      (forward-button d))))

(defun elm-package-mark ()
  "Mark the package at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (setq elm-package--marked-contents (cons id elm-package--marked-contents))
      (elm-package-next 1)
      (elm-package-refresh))))

(defun elm-package-unmark ()
  "Unmark the package at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (setq elm-package--marked-contents
            (seq-remove (lambda (x) (= id x))
                        elm-package--marked-contents))
      (elm-package-next 1)
      (elm-package-refresh))))

(defun elm-package-view ()
  "View the package at point in a browser."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (let-alist (nth id elm-package--contents)
        (browse-url (elm-package--build-uri "packages" .name (elt .versions 0)))))))

(defun elm-package--install-sentinel (_proc _msg)
  "Refreshes the package buffer on _PROC exit, ignoring _MSG."
  (elm-package-refresh))

(defun elm-package-install ()
  "Install the marked packages."
  (interactive)
  (unless elm-package--marked-contents
    (error "Nothing to install"))
  (let* ((and (elm--shell-and-command))
         (command-to-run (s-join and (elm-package--get-marked-install-commands))))
    (when (yes-or-no-p (concat "Install " (s-join ", " (elm-package--get-marked-packages)) " ?"))
      (let* ((default-directory elm-package--working-dir)
             (compilation-buffer-name-function (lambda (_) elm-package-compile-buffer-name))
             (compilation-buffer (compile command-to-run)))
        (setq elm-package--marked-contents nil)
        (set-process-sentinel (get-buffer-process compilation-buffer)
                              #'elm-package--install-sentinel)))))

;;;###autoload
(defun elm-package-catalog (refresh)
  "Show the package catalog, refreshing the list if REFRESH is truthy."
  (interactive "P")
  (elm--assert-dependency-file)
  (when (or refresh (not elm-package--contents))
    (elm-package-refresh-contents))
  (let ((buffer (get-buffer-create elm-package-buffer-name)))
    (pop-to-buffer buffer)
    (elm-package--read-dependencies)
    (elm-package-mode)))

;;;###autoload
(defun elm-package-refresh-contents ()
  "Refresh the package list."
  (interactive)
  (elm--assert-dependency-file)
  (let* ((all-packages (elm-package--build-uri "all-packages")))
    (with-current-buffer (url-retrieve-synchronously all-packages)
      (goto-char (point-min))
      (re-search-forward "^ *$")
      (setq elm-package--marked-contents nil)
      (setq elm-package--contents
            (cl-loop for (name . versions) in (json-read)
                     collect `((name . ,(symbol-name name))
                               (versions . ,(nreverse versions))
                               (summary . "")))))))

;;;###autoload
(defun elm-import (refresh)
  "Import a module, refreshing if REFRESH is truthy."
  (interactive "P")
  (elm--assert-dependency-file)
  (when (or refresh (not elm-package--contents))
    (elm-package-refresh-contents))
  (elm-package--read-dependencies)
  (let* ((package (elm-package--read-package))
         (module (elm-package--read-module package))
         (statement (concat "import " module))
         (statement (read-string "Import statement: " statement)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward "^import " nil t)
          (beginning-of-line)
        (forward-line 1)
        (insert "\n"))
      (insert (concat statement "\n"))))
  (elm-sort-imports))


(defun elm-imports--list (buffer)
  "Find all imports in the current BUFFER.
Return an alist of (FULL_NAME . ('as AS 'exposing EXPOSING), where
EXPOSING"
  (with-current-buffer buffer
    (save-excursion
      (save-match-data
        (let ((matches ()))
          (goto-char (point-min))
          (while (re-search-forward elm-import--pattern nil t)
            (let ((full (substring-no-properties (match-string 1)))
                  (as (match-string 2))
                  (exposing (match-string 3)))
              (push (list full
                          (cons 'as (if as (substring-no-properties as) full))
                          (cons 'exposing exposing))
                    matches)))
          matches)))))

(defun elm-imports--aliased (imports-list name full-name)
  "Given IMPORTS-LIST, return the local name for function with NAME and FULL-NAME."
  (let* ((suffix (concat "." name))
         (module-name (s-chop-suffix suffix full-name))
         (imports-entry (cl-assoc module-name imports-list :test 'string-equal)))
    (let-alist imports-entry
      (if (or (string-equal "Basics" module-name)
              (when .exposing
                (or (string-equal .exposing "..")
                    (cl-find name (s-split " *, *" .exposing) :test 'string-equal))))
          name
        (concat (or .as module-name) suffix)))))

;;;###autoload
(defun elm-expose-at-point ()
  "Exposes identifier at point."
  (interactive)
  (save-excursion
    ;; If already at the beginning of defun then
    ;; elm-beginning-of-defun will go to previous defun.  Thus we go
    ;; to the beginning of next defun and come back to make sure we
    ;; will arrive at correct place.
    (elm-end-of-defun)
    (elm-beginning-of-defun)
    (let* (case-fold-search
           (expose (cond
                    ((looking-at (rx "type" (+ space) "alias" (+ space)))
                     (goto-char (match-end 0))
                     (word-at-point))
                    ((looking-at (rx "type" (+ space)))
                     (goto-char (match-end 0))
                     (concat (word-at-point)
                             (if (y-or-n-p "Expose constructors? ")
                                 "(..)"
                               "")))
                    ((or (looking-at (rx (or "port" "module" "import") (+ space)))
                         (null (word-at-point)))
                     (user-error "No identifier at point"))
                    (t (word-at-point)))))
      (goto-char (point-min))
      (if (re-search-forward (rx bol "module" (+ (or space))
                                 upper (* (or word (syntax symbol)))
                                 (+ (any space ?\n)) "exposing" (+ (any space ?\n)) "(")
                             nil t)
          (progn
            (goto-char (match-end 0))
            (insert expose)
            (when (looking-at (rx (* (any space ?\n)) word))
              (insert ", ")))
        (error "Couldn't find module declaration")))))

(defun elm-documentation--show (documentation)
  "Show DOCUMENTATION in a help buffer."
  (let-alist documentation
    (help-setup-xref (list #'elm-documentation--show documentation) nil)
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (point-min)
          (insert (propertize .name 'face 'font-lock-function-name-face))
          (when .args
            (insert (concat " " (s-join " " .args))))
          (when .cases
            (let ((first t))
              (mapc
               (lambda (case)
                 (if first
                     (insert "\n  = ")
                   (insert "\n  | "))
                 (insert (propertize (elt case 0) 'face 'font-lock-function-name-face))
                 (insert (concat " " (s-join " " (elt case 1))))
                 (setq first nil))
               .cases)))
          (when .type
            (insert " : ")
            (insert (propertize .type 'face 'font-lock-type-face)))
          (insert (concat "\n\n" (s-trim-left .comment)))))))  )

;;;###autoload
(defun elm-documentation-lookup (refresh)
  "Lookup the documentation for a function, refreshing if REFRESH is truthy."
  (interactive "P")
  (elm--assert-dependency-file)
  (when (or refresh (not elm-package--contents))
    (elm-package-refresh-contents))
  (elm-package--read-dependencies)
  (let* ((package (elm-package--read-package))
         (module (elm-package--read-module package))
         (definition (elm-package--read-module-definition package module))
         (documentation (elm-package-definition package module definition)))
    (elm-documentation--show documentation)))

;;;###autoload
(define-derived-mode elm-package-mode tabulated-list-mode "Elm Package"
  "Special mode for elm-package.

\\{elm-package-mode-map}"

  (buffer-disable-undo)

  (setq truncate-lines t

        tabulated-list-format elm-package-catalog-format
        tabulated-list-entries #'elm-package--entries)

  (tabulated-list-init-header)
  (tabulated-list-print))


;;;###autoload
(defun elm-test-project ()
  "Run the elm-test command on the current project."
  (interactive)
  (let ((default-directory (elm--find-elm-test-root-directory))
        (compilation-buffer-name-function (lambda (_) "*elm-test*")))
    (compile "elm-test")))


(provide 'elm-interactive)
;;; elm-interactive.el ends here
