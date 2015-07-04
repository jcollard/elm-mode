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

(defvar elm-interactive--buffer-simple-name "elm")
(defvar elm-interactive--buffer-name "*elm*")

(defvar elm-interactive-command "elm-repl"
  "The Elm REPL command.")

(defvar elm-interactive-arguments '()
  "Command line arguments to pass to the Elm REPL command.")

(defvar elm-interactive-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-elm-interactive'.")

(defvar elm-interactive-prompt-regexp "^> *"
  "Prompt for `run-elm-interactive'.")

(defun elm-interactive--initialize ()
  "Helper function to initialize the Elm REPL."
  (setq comint-use-prompt-regexp t))

(define-derived-mode elm-interactive-mode comint-mode "Elm Interactive"
  "Major mode for `run-elm-interactive'.

\\<elm-interactive-mode-map>"
  nil "Elm Interactive"

  (setq comint-prompt-regexp elm-interactive-prompt-regexp)
  (setq comint-prompt-read-only t)

  (turn-on-elm-font-lock))

(add-hook 'elm-interactive-mode-hook 'elm-interactive--initialize)

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
      (apply 'make-comint-in-buffer elm-interactive--buffer-simple-name buffer
             elm-interactive-command elm-interactive-arguments)
      (elm-interactive-mode))))

(defun load-elm-repl ()
  "Load an interactive REPL if there isn't already one running.
Changes the current root directory to be the directory with the closest
package json if one exists otherwise sets it to be the working directory
of the file specified."
  (interactive)
  (let ((import-statement (elm--get-open-import (elm--get-module-name))))
    (run-elm-interactive)
    (comint-send-string (current-buffer) ":reset\n")
    (comint-send-string (current-buffer) import-statement)))

(defun push-elm-repl ()
  "Push the selected region to an interactive REPL."
  (interactive)
  (let* ((to-push (buffer-substring-no-properties (mark) (point)))
         (format-tp (replace-regexp-in-string "\n" "\\\\\n" to-push)))
    (run-elm-interactive)
    (comint-send-string (current-buffer) format-tp)
    (comint-send-string (current-buffer) "\n")))

(provide 'elm-interactive)
;;; elm-interactive.el ends here
