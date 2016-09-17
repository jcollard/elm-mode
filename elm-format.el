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

(defcustom elm-format-elm-version "0.17"
  "The version of Elm against which code should be formatted."
  :group 'elm-format
  :type '(choice (const :tag "Default: 0.17" "0.17")
                 (const :tag "0.16" "0.16")))

(defvar elm-format-command "elm-format"
  "The name of the `elm-format' command.")


;;;###autoload
(defun elm-mode-format-buffer ()
  "Apply `elm-format' to the current buffer."
  (interactive)
  (let* ((column (current-column))
         (line (line-number-at-pos))
         ;;; elm-format requires that the file have a .elm extension
         (in-file (make-temp-file "elm-format" nil ".elm"))
         (err-file (make-temp-file "elm-format"))
         (out-file (make-temp-file "elm-format"))
         (contents (buffer-substring-no-properties (point-min) (point-max)))
         (_ (with-temp-file in-file (insert contents)))
         (retcode
          (with-temp-buffer
            (call-process elm-format-command
                          nil (list (current-buffer) err-file)
                          nil
                          in-file
                          "--output" out-file
                          "--elm-version" elm-format-elm-version
                          "--yes"))))

    (if (/= retcode 0)
        (with-temp-buffer
          (insert-file-contents err-file nil nil nil t)
          (message "Error: elm-format failed on current buffer.\n\n%s" (buffer-string)))
      (insert-file-contents out-file nil nil nil t)
      (goto-char (point-min))
      (forward-line (1- line))
      (goto-char (+ column (point))))

    (delete-file err-file)
    (delete-file out-file)))


(provide 'elm-format)
;;; elm-format.el ends here
