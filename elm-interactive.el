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
(require 'comint)
(require 'elm-font-lock)
(require 'elm-util)
(require 's)

(defvar elm-interactive--seen-prompt nil
  "Non-nil represents the fact that a prompt has been spotted.")
(make-variable-buffer-local 'elm-interactive--seen-prompt)

(defvar elm-interactive--buffer-simple-name "elm")
(defvar elm-interactive--buffer-name "*elm*")

(defvar elm-interactive-command "elm-repl"
  "The Elm REPL command.")

(defvar elm-interactive-arguments '()
  "Command line arguments to pass to the Elm REPL command.")

(defvar elm-interactive-prompt-regexp "^[>|] "
  "Prompt for `run-elm-interactive'.")

(defvar elm-interactive-mode-map
  (let ((map (make-keymap)))
    (define-key map "\t" #'completion-at-point)
    (define-key map (kbd "C-a") #'elm-interactive-mode-beginning)
    map)
  "Keymap for Elm interactive mode.")

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
         (buffer (comint-check-proc elm-interactive--buffer-simple-name)))

    (pop-to-buffer
     (if (or buffer (not (derived-mode-p 'elm-interactive-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer elm-interactive--buffer-name))
       (current-buffer)))

    (unless buffer
      (apply #'make-comint-in-buffer elm-interactive--buffer-simple-name buffer
             elm-interactive-command nil elm-interactive-arguments)
      (elm-interactive-mode))))

;;;###autoload
(defun load-elm-repl ()
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
(defun push-elm-repl ()
  "Push the selected region to an interactive REPL."
  (interactive)
  (let* ((to-push (buffer-substring-no-properties (mark) (point)))
         (lines (split-string (s-trim-right to-push) "\n")))
    (run-elm-interactive)
    (dolist (line lines)
      (elm-interactive--send-command (concat line " \\\n")))
    (elm-interactive--send-command "\n")))

;;;###autoload
(defun push-decl-elm-repl ()
  "Push the current top level declaration to the REPL."
  (interactive)
  (let ((lines (elm--get-decl)))
    (run-elm-interactive)
    (dolist (line lines)
      (elm-interactive--send-command (concat line " \\\n")))
    (elm-interactive--send-command "\n")))

(provide 'elm-interactive)
;;; elm-interactive.el ends here
