;;; elm-format.el --- Automatically format an Elm buffer.

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
(defcustom elm-format-on-save nil
  "Controls whether or not `elm-format' should be run on the current buffer on save."
  :group 'elm-format
  :type 'boolean)

(defvar elm-format-command "elm-format"
  "The name of the `elm-format' command.")


;;;###autoload
(defun elm-mode-format-buffer ()
  "Apply `elm-format' to the current buffer."
  (interactive)
  (let* ((column (current-column))
         (line (line-number-at-pos))
         (in-file (buffer-file-name))
         (err-file (make-temp-file "elm-format"))
         (out-file (make-temp-file "elm-format"))
         (retcode (call-process elm-format-command
                                nil (list (current-buffer) err-file)
                                nil
                                in-file
                                "--output" out-file
                                "--yes")))

    (if (/= retcode 0)
        (message "Error: elm-format failed on current buffer")
      (insert-file-contents out-file nil nil nil t)
      (goto-char (point-min))
      (forward-line (1- line))
      (goto-char (+ column (point))))

    (delete-file err-file)
    (delete-file out-file)))


(provide 'elm-format)
;;; elm-format.el ends here
