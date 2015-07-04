;;; elm-compile.el --- Elm compilation sub-mode

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
(require 'elm-util)

(defvar elm-compiler-command
  "elm-make"
  "The command that is used to compile Elm files.")

(defun elm--compile-command (file &optional output)
  "Generate a command that will compile FILE into OUTPUT."
  (let ((output-command (if output (concat " --output=" output) "")))
    (concat elm-compiler-command " " file output-command " --yes")))

(defun elm-compile (file &optional output)
  "Compile FILE into OUTPUT."
  (let* ((d-file (elm--find-dependency-file-path))
	 (default-directory (if d-file d-file (elm--get-buffer-dirname)))
	 (command (elm--compile-command file output)))
    (print (shell-command-to-string command))))

(defun elm-compile-buffer (&optional output)
  "Compile the current buffer into OUTPUT."
  (interactive)
  (elm-compile (elm--buffer-local-file-name) output))

(provide 'elm-compile)
;;; elm-compile.el ends here
