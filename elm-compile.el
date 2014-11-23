;;; elm-compile.el --- Elm compilation sub-mode

;; Copyright (C) 2013, 2014  Joseph Collard

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

;; Compilation
(defvar elm-compiler
  "elm-make")

(defun elm-compile-command (file &optional output)
  (let* ((output-command (if output (concat " --output=" output) ""))
         (ls (list elm-compiler " " file output-command " --yes")))
    (reduce 'concat ls)))

(defun elm-compile (file &optional output)
  (let* ((d-file (find-dependency-file-path))
	 (default-directory (if d-file d-file (get-file-directory)))
	 (command (elm-compile-command file output)))
    (print (shell-command-to-string command))))

(defun elm-compile-buffer (&optional output)
  (interactive)
  (elm-compile (buffer-local-file-name) output))

(provide 'elm-compile)

;;; elm-compile.el ends here
