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

(require 'ansi-color)

(defcustom elm-format-on-save nil
  "Controls whether or not `elm-format' should be run on the current buffer on save."
  :group 'elm-format
  :type 'boolean)

(defcustom elm-format-elm-version "0.18"
  "The version of Elm against which code should be formatted."
  :group 'elm-format
  :type '(choice (const :tag "Default: 0.18" "0.18")
                 (const :tag "0.17" "0.17")
                 (const :tag "0.16" "0.16")))

(defcustom elm-format-command "elm-format"
  "The name of the `elm-format' command."
  :group 'elm-format
  :type 'string)

(defun elm-format--handle-success (out-file error-buffer-name is-call-interactive)
  "Replaces the current buffer's contents with the output of a successful call to elm-format."
  (let ((error-buffer (get-buffer error-buffer-name)))
    (when error-buffer
      (with-current-buffer error-buffer
        (read-only-mode 0)
        (erase-buffer)
        (insert "elm-format applied successfully.")
        (read-only-mode 1)))

    (insert-file-contents out-file nil nil nil t)

    (when is-call-interactive
      (message "elm-format applied"))))

(defun elm-format--handle-error (err-file error-buffer-name is-call-interactive)
  "Handles error for calls to elm-format.
  If elm-format was run interactively by the user, displays a read-only buffer with the error.
  Otherwise, just displays a message informing that the call failed."

  (let ((error-buffer (get-buffer-create error-buffer-name)))
    (with-current-buffer error-buffer
      (read-only-mode 0)
      (insert-file-contents err-file nil nil nil t)
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode t))

    (if is-call-interactive
        (display-buffer error-buffer)
      (message (concat "elm-format failed: see " error-buffer-name)))))

;;;###autoload
(defun elm-mode-format-buffer (&optional is-call-interactive)
  "Apply `elm-format' to the current buffer."
  (interactive "p")
  (let* (;; elm-format requires that the file have a .elm extension
         (in-file (make-temp-file "elm-format" nil ".elm"))
         (err-file (make-temp-file "elm-format"))
         (out-file (make-temp-file "elm-format"))
         (contents (buffer-substring-no-properties (point-min) (point-max)))
         (_ (with-temp-file in-file (insert contents))))

    (unwind-protect
        (let* ((command elm-format-command)
               (version elm-format-elm-version)
               (error-buffer-name "*elm-format errors*")
               (retcode
                (with-temp-buffer
                  (call-process command
                                nil (list (current-buffer) err-file)
                                nil
                                in-file
                                "--output" out-file
                                "--elm-version" version
                                "--yes"))))
          (if (not (eq retcode 0))
              (elm-format--handle-error err-file error-buffer-name is-call-interactive)
            (elm-format--handle-success out-file error-buffer-name is-call-interactive)))
      (delete-file in-file)
      (delete-file err-file)
      (delete-file out-file))))


(provide 'elm-format)
;;; elm-format.el ends here
