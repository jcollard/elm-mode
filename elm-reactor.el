;;; elm-reactor.el --- Wrapper around `elm-reactor'.

;; Copyright (C) 2015  Bogdan Popa

;; Author: Bogdan Popa <popa.bogdanp@gmail.com>
;; URL: https://github.com/Bogdanp/elm-mode

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
(require 'f)

(defvar elm-reactor-command "elm-reactor"
  "The Elm Reactor command.")

(defvar elm-reactor-port "8000"
  "The Elm Reactor port.")

(defvar elm-reactor-arguments `("-p" ,elm-reactor-port)
  "Command line arguments to pass to the Elm Reactor command.")

(defvar elm-reactor--process-name "elm-reactor")
(defvar elm-reactor--buffer-name "*elm-reactor*")

;;;###autoload
(defun run-elm-reactor ()
  "Run the Elm reactor process."
  (interactive)
  (let ((default-directory (elm--find-dependency-file-path))
        (process (get-process elm-reactor--process-name)))

    (when (not process)
      (apply #'start-process elm-reactor--process-name elm-reactor--buffer-name
             elm-reactor-command elm-reactor-arguments))))

(defun elm-preview-buffer ()
  "Preview the current buffer using Elm reactor."
  (interactive)
  (let* ((fname (buffer-file-name))
         (deppath (elm--find-dependency-file-path))
         (path (f-relative fname deppath)))
    (run-elm-reactor)
    (browse-url (concat "http://127.0.0.1:" elm-reactor-port "/" path))))

(provide 'elm-reactor)
;;; elm-reactor.el ends here
