;;; elm-preview.el ---

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
(require 'elm-compile)


;; Compiles the current buffer and opens it in the default browser
(defun elm-preview-buffer ()
  (interactive)
  (let* ((d-file (find-dependency-file-path))
	 (dir (if d-file d-file (get-file-directory)))
	 (path-html (concat dir "preview.html")))
    (elm-compile-buffer "preview.html")
    (browse-url (concat "file:///" path-html))))
  
(provide 'elm-preview)

;;; elm-preview.el ends here
