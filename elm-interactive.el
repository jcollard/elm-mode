;;; elm-interactive.el --- Run an interactive Elm session.

;; Copyright (C) 2015  Bogdan Popa

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
(require 'elm-font-lock)
(require 'elm-util)
(require 'f)
(require 'json)
(require 'let-alist)
(require 's)
(require 'tabulated-list)
(require 'url)
(eval-when-compile
  (require 'auto-complete nil t))

(defvar elm-interactive--seen-prompt nil
  "Non-nil represents the fact that a prompt has been spotted.")
(make-variable-buffer-local 'elm-interactive--seen-prompt)

(defvar elm-interactive--process-name "elm")
(defvar elm-interactive--buffer-name "*elm*")
(defvar elm-reactor--process-name "elm-reactor")
(defvar elm-reactor--buffer-name "*elm-reactor*")

(defvar elm-interactive-command "elm-repl"
  "The Elm REPL command.")

(defvar elm-interactive-arguments '()
  "Command line arguments to pass to the Elm REPL command.")

(defvar elm-interactive-prompt-regexp "^[>|] "
  "Prompt for `run-elm-interactive'.")

(defvar elm-reactor-command "elm-reactor"
  "The Elm Reactor command.")

(defvar elm-reactor-port "8000"
  "The Elm Reactor port.")

(defvar elm-reactor-address "127.0.0.1"
  "The Elm Reactor address.")

(defvar elm-reactor-arguments `("-p" ,elm-reactor-port "-a" ,elm-reactor-address)
  "Command line arguments to pass to the Elm Reactor command.")

(defvar elm-compile--buffer-name "*elm-make*")

(defvar elm-compile-command "elm-make"
  "The Elm compilation command.")

(defvar elm-compile-arguments '("--yes" "--warn" "--output=elm.js")
  "Command line arguments to pass to the Elm compilation command.")

(defvar elm-compile-error-regexp-alist-alist
  '((elm-file "-- [^-]+ -+ \\(.+\\)$" 1 nil)
    (elm-line "^\\([0-9]+\\)│" nil 1))
  "Regexps to match Elm compiler errors in compilation buffer.")

(defvar elm-compile-error-regexp-alist '(elm-line elm-file))

(dolist (alist elm-compile-error-regexp-alist-alist)
  (add-to-list 'compilation-error-regexp-alist-alist alist))

(dolist (symbol elm-compile-error-regexp-alist)
  (add-to-list 'compilation-error-regexp-alist symbol))

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

(defvar elm-package-command "elm-package"
  "The Elm package command.")

(defvar elm-package-arguments '("install" "--yes")
  "Command line arguments to pass to the Elm package command.")

(defvar elm-package-catalog-root
  "http://package.elm-lang.org/"
  "The root URI for the Elm package catalog.")

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
    map)
  "Keymap for Elm interactive mode.")

(defvar elm-oracle-command "elm-oracle"
  "The Elm Oracle command.")

(defconst elm-oracle--pattern
  "\\(?:[^A-Za-z0-9_.']\\)\\(\\(?:[A-Za-z_][A-Za-z0-9_']*[.]\\)?[A-Za-z0-9_']*\\)"
  "The prefix pattern used for completion.")

(defvar elm-oracle--completion-cache (make-hash-table :test #'equal)
  "A cache for Oracle-based completions by prefix.")

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

(defun elm-interactive--spot-prompt (string)
  "Spot the prompt, STRING is ignored."
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

Stolen from haskell-mode."
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

(defun elm-interactive--initialize ()
  "Helper function to initialize the Elm REPL."
  (setq comint-use-prompt-regexp t))

;;;###autoload
(define-derived-mode elm-interactive-mode comint-mode "Elm Interactive"
  "Major mode for `run-elm-interactive'.

\\{elm-interactive-mode-map}"

  (setq comint-prompt-regexp elm-interactive-prompt-regexp)
  (setq comint-prompt-read-only t)

  (add-hook 'comint-output-filter-functions #'elm-interactive--spot-prompt nil t)

  (turn-on-elm-font-lock))

(add-hook 'elm-interactive-mode-hook #'elm-interactive--initialize)

;;;###autoload
(defun run-elm-interactive ()
  "Run an inferior instance of `elm-repl' inside Emacs."
  (interactive)
  (let* ((default-directory (elm--find-dependency-file-path))
         (prog elm-interactive-command)
         (buffer (comint-check-proc elm-interactive--process-name)))

    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'elm-interactive-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer elm-interactive--buffer-name))
       (current-buffer)))

    (unless buffer
      (apply #'make-comint-in-buffer elm-interactive--process-name buffer
             elm-interactive-command nil elm-interactive-arguments)
      (elm-interactive-mode))))

;;;###autoload
(defun elm-repl-load ()
  "Load an interactive REPL if there isn't already one running.
Changes the current root directory to be the directory with the closest
package json if one exists otherwise sets it to be the working directory
of the file specified."
  (interactive)
  (let ((import-statement (elm--build-import-statement)))
    (run-elm-interactive)
    (elm-interactive--send-command ":reset\n")
    (elm-interactive--send-command import-statement)))

;;;###autoload
(defun elm-repl-push (beg end)
  "Push the region from BEG to END to an interactive REPL."
  (interactive "r")
  (let* ((to-push (buffer-substring-no-properties beg end))
         (lines (split-string (s-trim-right to-push) "\n")))
    (run-elm-interactive)
    (dolist (line lines)
      (elm-interactive--send-command (concat line " \\\n")))
    (elm-interactive--send-command "\n")))

;;;###autoload
(defun elm-repl-push-decl ()
  "Push the current top level declaration to the REPL."
  (interactive)
  (let ((lines (elm--get-decl)))
    (run-elm-interactive)
    (dolist (line lines)
      (elm-interactive--send-command (concat line " \\\n")))
    (elm-interactive--send-command "\n")))


;;; Reactor:
;;;###autoload
(defun run-elm-reactor ()
  "Run the Elm reactor process."
  (interactive)
  (let ((default-directory (elm--find-dependency-file-path))
        (process (get-process elm-reactor--process-name)))

    (when process
      (delete-process process))

    (apply #'start-process elm-reactor--process-name elm-reactor--buffer-name
           elm-reactor-command elm-reactor-arguments)))

(defun elm-reactor--browse (path &optional debug)
  "Open (reactor-relative) PATH in browser with optional DEBUG.

Runs `elm-reactor' first."
  (run-elm-reactor)
  (let ((qs (if debug "?debug" "")))
    (browse-url (concat "http://" elm-reactor-address ":" elm-reactor-port "/" path qs))))

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
  "Preview the Main.elm file using Elm reactor (in debug mode if DEBUG is truthy)."
  (interactive "P")
  (elm-reactor--browse (elm--find-main-file) debug))


;;; Make:
(defun elm-compile--command (file &optional output)
  "Generate a command that will compile FILE into OUTPUT."
  (let ((output-command (if output (concat " --output=" output) "")))
    (concat elm-compile-command " " file output-command " " (s-join " " elm-compile-arguments))))

(defun elm-compile--colorize-compilation-buffer ()
  "Handle ANSI escape sequences in compilation buffer."
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(add-hook 'compilation-filter-hook #'elm-compile--colorize-compilation-buffer)

(defun elm-compile--file (file &optional output)
  "Compile FILE into OUTPUT."
  (let ((default-directory (elm--find-dependency-file-path))
        (compilation-buffer-name-function (lambda (_) elm-compile--buffer-name)))
    (compile (elm-compile--command file output))))

;;;###autoload
(defun elm-compile-buffer (&optional output)
  "Compile the current buffer into OUTPUT."
  (interactive
   (when current-prefix-arg
     (list (read-file-name "Output to: "))))
  (elm-compile--file (elm--buffer-local-file-name) output))

;;;###autoload
(defun elm-compile-main (&optional output)
  "Compile the Main.elm file into OUTPUT."
  (interactive
   (when current-prefix-arg
     (list (read-file-name "Output to: "))))
  (elm-compile--file (elm--find-main-file) output))


;;; Package:
(defun elm-package--build-uri (&rest segments)
  "Build a URI by combining the package catalog root and SEGMENTS."
  (concat elm-package-catalog-root (s-join "/" segments)))

(defun elm-package--format-entry (index entry)
  "Format a package '(INDEX ENTRY) for display in the package listing."
  (let-alist entry
    (let ((mark (if (-contains? elm-package--marked-contents index)
                    "*"
                  ""))
          (button (list .name . ()))
          (status (if (-contains? elm-package--dependencies .name)
                      "dependency"
                    "available")))
      (list index (vector mark button (elt .versions 0) status .summary)))))

(defun elm-package--entries ()
  "Return the formatted package list."
  (-map-indexed #'elm-package--format-entry elm-package--contents))

(defun elm-package--get-marked-packages ()
  "Get packages that are marked for installation."
  (-map (lambda (id)
          (let-alist (nth id elm-package--contents)
            (concat .name " " (elt .versions 0))))
        elm-package--marked-contents))

(defun elm-package--get-marked-install-commands ()
  "Get a list of the commands required to install the marked packages."
  (-map (lambda (package)
          (concat elm-package-command " " (s-join " " elm-package-arguments) " " package))
        (elm-package--get-marked-packages)))

(defun elm-package--read-dependencies ()
  "Read the current package's dependencies."
  (setq elm-package--working-dir (elm--find-dependency-file-path))
  (let-alist (elm--read-dependency-file)
    (setq elm-package--dependencies (-map (lambda (dep) (symbol-name (car dep)))
                                          .dependencies))))

(defun elm-package--read-package ()
  "Read a package from the minibuffer."
  (completing-read "Package: " elm-package--dependencies nil t))

(defun elm-package--read-module (package)
  "Read a module from PACKAGE from the minibuffer."
  (completing-read "Module: " (elm-package-modules package) nil t))

(defun elm-package-refresh-package (package version)
  "Refresh the cache for PACKAGE with VERSION."
  (let* ((description (elm-package--build-uri "description"))
         (package-uri (concat description "?name=" package "&version=" version)))
    (with-current-buffer (url-retrieve-synchronously package-uri)
      (goto-char (point-min))
      (re-search-forward "^ *$")
      (setq elm-package--cache
            (cons `(,package . ,(json-read))
                  elm-package--cache)))))

(defun elm-package-latest-version (package)
  "Get the latest version of PACKAGE."
  (let ((package (-find (lambda (p)
                          (let-alist p
                            (equal .name package)))
                        elm-package--contents)))

    (if (not package)
        (error "Package not found")
      (let-alist package
        (elt .versions 0)))))

(defun elm-package-modules (package)
  "Get PACKAGE's module list."
  (unless (assoc package elm-package--cache)
    (elm-package-refresh-package package (elm-package-latest-version package)))
  (let-alist (cdr (assoc package elm-package--cache))
    (append .exposed-modules nil)))

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
            (-reject (lambda (x) (= id x))
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

(defun elm-package--install-sentinel (proc msg)
  "Refreshes the package buffer on PROC exit, ignoring MSG."
  (elm-package-refresh))

(defun elm-package-install ()
  "Install the marked packages."
  (interactive)
  (unless elm-package--marked-contents
    (error "Nothing to install"))
  (let ((command-to-run (s-join " && " (elm-package--get-marked-install-commands))))
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
      (setq elm-package--contents (append (json-read) nil)))))

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
      (forward-line 1)
      (insert (concat statement "\n")))))

;;;###autoload
(defun elm-documentation-lookup (refresh)
  "Lookup the documentation for a function, refreshing if REFRESH is truthy."
  (interactive "P")
  (elm--assert-dependency-file)
  (when (or refresh (not elm-package--contents))
    (elm-package-refresh-contents))
  (elm-package--read-dependencies)
  (let* ((package (elm-package--read-package))
         (version (elm-package-latest-version package))
         (module (elm-package--read-module package))
         (module (s-replace "." "-" module))
         (function (read-string "Function: " (thing-at-point 'word t)))
         (uri (elm-package--build-uri "packages" package version module))
         (uri (concat uri "#" function)))
    (browse-url uri)))

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


(autoload 'popup-make-item "popup")


(defun elm-oracle--completion-prefix-at-point ()
  "Return the completions prefix found at point."
  (save-excursion
    (let* ((_ (re-search-backward elm-oracle--pattern nil t))
           (beg (1+ (match-beginning 0)))
           (end (match-end 0)))
      (s-trim (buffer-substring-no-properties beg end)))))


(defun elm-oracle--completion-namelist (completions)
  "Extract a list of identifier names from the COMPLETIONS alists."
  (-map (lambda (candidate)
          (let-alist candidate
            .fullName))
        completions))

(defun elm-oracle--completions-select (completions selection)
  "Search COMPLETIONS for SELECTION and return it."
  (aref (remove-if-not (lambda (cand)
                         (let-alist cand
                           (equal .fullName selection)))
                       completions) 0))

(defun elm-oracle--completion-docbuffer (completions selection)
  "Search COMPLETIONS for SELECTION and return its documentation."
  (company-doc-buffer
    (let-alist (elm-oracle--completions-select completions selection)
      (format "%s\n\n%s" .signature .comment))))

(defun elm-oracle--completion-signature (completions selection)
  "Search COMPLETIONS for SELECTION and return its signature."
  (let-alist (elm-oracle--completions-select completions selection)
    (format "%s : %s" selection .signature)))

(defun elm-oracle--get-completions-cached (prefix)
  "Cache and return the cached elm-oracle completions for PREFIX."
  (when (and prefix (not (equal "" prefix)))
    (or (gethash prefix elm-oracle--completion-cache)
        (let* ((default-directory (elm--find-dependency-file-path))
               (current-file (or (buffer-file-name) (elm--find-main-file)))
               (command (s-join " " (list elm-oracle-command
                                          (shell-quote-argument current-file)
                                          (shell-quote-argument prefix))))
               (candidates (json-read-from-string (shell-command-to-string command))))
          (puthash prefix candidates elm-oracle--completion-cache)))))

(defun elm-oracle--get-completions (prefix &optional popup)
  "Get elm-oracle completions for PREFIX with optional POPUP formatting."
  (let* ((candidates (elm-oracle--get-completions-cached prefix))
         (candidates
          (-map (lambda (candidate)
                  (let-alist candidate
                    (if popup
                        (popup-make-item .fullName
                                         :document (concat .signature "\n\n" .comment)
                                         :summary .signature)
                      .fullName)))
                candidates)))
    candidates))

(defun elm-oracle--get-first-completion (item)
  "Get the first completion for ITEM."
  (let* ((default-directory (elm--find-dependency-file-path))
         (current-file (buffer-file-name))
         (command (s-join " " (list elm-oracle-command current-file item)))
         (candidates (json-read-from-string (shell-command-to-string command))))
    (if (> (length candidates) 0)
        (elt candidates 0)
      nil)))

;;;###autoload
(defun elm-oracle-type-at-point ()
  "Print the type of the function at point to the minibuffer."
  (interactive)
  (save-excursion
    (forward-word)
    (let* ((_ (re-search-backward elm-oracle--pattern nil t))
           (beg (1+ (match-beginning 0)))
           (end (match-end 0))
           (item (s-trim (buffer-substring-no-properties beg end)))
           (completion (elm-oracle--get-first-completion item)))
      (if completion
          (let-alist completion
            (message .signature))
        (message "Unknown type")))))

;;;###autoload
(defun elm-oracle-completion-at-point-function ()
  "Completion at point function for elm-oracle."
  (save-excursion
    (let* ((_ (re-search-backward elm-oracle--pattern nil t))
           (beg (1+ (match-beginning 0)))
           (end (match-end 0))
           (prefix (s-trim (buffer-substring-no-properties beg end)))
           (completions (elm-oracle--get-completions prefix)))
      (list beg end completions :exclusive 'no))))

;;;###autoload
(defun elm-oracle-setup-completion ()
  "Set up standard completion.
Add this function to your `elm-mode-hook' to enable an
elm-specific `completion-at-point' function."
  (add-hook 'completion-at-point-functions
            #'elm-oracle-completion-at-point-function
            nil t))


(eval-after-load 'auto-complete
  '(ac-define-source elm
     `((candidates . (elm-oracle--get-completions ac-prefix t))
       (prefix . ,elm-oracle--pattern))))

;;;###autoload
(defun elm-oracle-setup-ac ()
  "Set up auto-complete support.
Add this function to your `elm-mode-hook'."
  (add-to-list 'ac-sources 'ac-source-elm))


;;;###autoload
(defun company-elm (command &optional arg &rest ignored)
  "Set up a company backend for elm."
  (interactive (list 'interactive))
  (case command
    (init t)
    (interactive (company-begin-backend 'company-elm))
    (t
     (let* ((prefix (elm-oracle--completion-prefix-at-point))
            (completions (elm-oracle--get-completions-cached prefix)))
       (case command
         (prefix prefix)
         (doc-buffer (elm-oracle--completion-docbuffer completions arg))
         (candidates (elm-oracle--completion-namelist completions))
         (meta (elm-oracle--completion-signature completions arg)))))))


(provide 'elm-interactive)
;;; elm-interactive.el ends here
