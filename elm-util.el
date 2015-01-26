;;; elm-utils.el --- General utility functions used by Elm mode modules

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

;; Provides useful utility functions

;; TODO: should be based on the OS.

;;; Code:
(defvar directory-seperator 
  "/")


(defvar elm-dependencies-file-name
  "elm-package.json")

;; If splitting right would not half the width of the current
;; buffer, splits right. Otherwise, splits below
(defun intelligent-split-window ()
  (if (not (fboundp 'window-total-width)) (split-window)
    (let ((width (window-total-width))
	  (height (window-total-height)))
      (if (> (/ width 2) height)
	  (split-window-right)
        (split-window-below)))))

(defun intercalate (separator list)
  (mapconcat #'identity list separator))

;; Returns the name of the module in the current buffer based on
;; its filename and relative location to the nearest `elm-dependencies-file-name`
(defun get-module-name ()
  (let* ((m-d-path (elm-find-dependency-file-path))
	 (d-path (or m-d-path default-directory))
	 (d-split (split-string d-path directory-seperator))
	 (f-path (buffer-file-name))
	 (f-split (split-string f-path directory-seperator))
	 (mod-split (remove-matching d-split f-split))
	 (mod (intercalate "." mod-split))
	 (mod-split2 (butlast (split-string mod "\\.")))
	 (mod2 (intercalate "." mod-split2)))
    mod2))

(defun buffer-local-file-name ()
  (let* ((m-d-path (elm-find-dependency-file-path))
	 (d-path (or m-d-path default-directory))
	 (d-split (split-string d-path directory-seperator))
	 (f-path (buffer-file-name))
	 (f-split (split-string f-path directory-seperator))
	 (mod-split (remove-matching d-split f-split))
	 (mod (intercalate directory-seperator mod-split)))
    mod))
  

(defun remove-matching (ls0 ls1)
  (if (null ls0) ls1
    (let ((h0 (car ls0))
	  (h1 (car ls1)))
      (if (equal h0 h1) (remove-matching (cdr ls0) (cdr ls1))
	ls1))))

(defun elm-find-dependency-file-path ()
  "Return the closest directory containing `elm-dependencies-file-name'.
If neither the current directory of this buffer nor any parent
directory contains such a file, return nil."
  (locate-dominating-file default-directory elm-dependencies-file-name))

(provide 'elm-util)

;;; elm-util.el ends here
