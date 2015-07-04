;;; elm-repl.el --- Run Elm repl

;; Copyright (C) 2013, 2014  Joseph Collard
;; Copyright (C) 2015  Bogdan Popa

;; Author: Joseph Collard
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
(require 'elm-util)

(defconst elm--repl-command "elm-repl")
(defconst elm--repl-buffer "*elm-repl*")

(defun run-elm-repl ()
  "Run an Elm REPL."
  (interactive)
  (let* ((default-directory (elm--find-dependency-file-path))
         (buffer (get-buffer-create elm--repl-buffer))
         (target-window (get-buffer-window buffer))
         (current-window (selected-window)))

    ;; If the target window does not exist, create it and set the buffer
    ;; then select that window so elm-repl will be running there
    (if target-window
	(select-window target-window)
      (let ((split-window (elm--intelligent-split-window)))
	(set-window-buffer split-window buffer)
	(select-window split-window)))

    ;; Start elm-repl if it is not already running
    (make-comint elm--repl-command elm--repl-command)

    ;; Switch focus back to the originally selected window
    (select-window current-window)))

(defun load-elm-repl ()
  "Load an interactive version elm-repl if there isn't already one running.
Changes the current root directory to be the directory with the closest
package json if one exists otherwise sets it to be the working directory
of the file specified."
  (interactive)
  (run-elm-repl)
  (let ((elm-repl (get-process elm--repl-command)))
    (comint-send-string elm-repl ":reset\n")
    (comint-send-string elm-repl (elm--get-open-import (elm--get-module-name)))))

(defun push-elm-repl ()
  "Push the selected region to elm-repl."
  (interactive)
  (run-elm-repl)
  (let* ((elm-repl (get-process elm--repl-command))
         (to-push (buffer-substring-no-properties (mark) (point)))
	 (format-tp (replace-regexp-in-string "\n" "\\\\\n" to-push)))
    (comint-send-string elm-repl format-tp)
    (comint-send-string elm-repl "\n")))

;; TODO: Add an load-elm-repl that searches for the nearest
;; elm_dependencies.json. It should use that directory as the root directory
;; for loading the current buffer. This requires elm-repl to have a
;; `cd` command for switching directories. elm-repl should also use the
;; specified elm_dependencies.json to determine a default set of src-dirs to
;; use when launched.

;; TODO: When a command is sent to the elm-repl process, it would be nice if
;; it was echoed in the process buffer

(provide 'elm-repl)
;;; elm-repl.el ends here
