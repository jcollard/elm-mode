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


(defvar dependencies-file-name
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

(defun intercalate (seperator list)
  (let ((helper (lambda (x y) (concat x (concat seperator y)))))
    (reduce helper list)))

(defun merge-path (dir-path-list)
  (let ((helper (lambda (x y) (concat x (concat y directory-seperator)))))
    (reduce helper (cons "" dir-path-list))))

;; Get the current working directory for the file being worked on
(defun get-file-directory ()
  (let* ((file-path (buffer-file-name))
	 (split-file-path (split-string file-path directory-seperator))
	 (dir-path-list (butlast split-file-path))
	 (dir-path (merge-path dir-path-list)))
    dir-path))

(defun get-file-path-directory (file-path)
  (let* ((split-file-path (split-string file-path directory-seperator))
	 (dir-path-list (butlast split-file-path))
	 (dir-path (merge-path dir-path-list)))
    dir-path))


;; Returns the name of the module in the current buffer based on
;; its filename and relative location to the nearest `dependencies-file-name`
(defun get-module-name ()
  (let* ((m-d-path (find-dependency-file-path))
	 (d-path (if m-d-path m-d-path (get-file-directory)))
	 (d-split (split-string d-path directory-seperator))
	 (f-path (buffer-file-name))
	 (f-split (split-string f-path directory-seperator))
	 (mod-split (remove-matching d-split f-split))
	 (mod (intercalate "." mod-split))
	 (mod-split2 (butlast (split-string mod "\\.")))
	 (mod2 (intercalate "." mod-split2)))
    mod2))

(defun buffer-local-file-name ()
  (let* ((m-d-path (find-dependency-file-path))
	 (d-path (if m-d-path m-d-path (get-file-directory)))
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

;; Returns the directory of the current buffer if it contains 
;; `dependencies-file-name`. Otherwise, it returns the nearest parent 
;; directory that contains `dependencies-file-name`. If no parent folder
;; contains `dependencies-file-name` `nil` is returned
(defun find-dependency-file-path ()
  (find-dependency-file-path-helper (get-file-directory)))

;; Returns `dir-path` if it contains `dependencies-file-name`.
;; Otherwise, it checks the parent directory. If the root directory
;; is reached `nil` is returned
(defun find-dependency-file-path-helper (dir-path)
  (if (not dir-path) nil
    (if (contains-dependency-file dir-path) dir-path
      (find-dependency-file-path-helper (up-dir dir-path)))))

;; Returns the parent directory of `dir-path` or `nil` if there is no parent directory
(defun up-dir (dir-path)
  (let* ((dir-path-clean (file-name-as-directory dir-path))
	 (split-file-path (split-string dir-path-clean directory-seperator))
	 (up-dir-path-list (butlast split-file-path 2))
	 (up-dir-path (merge-path up-dir-path-list)))
    (if (eq up-dir-path "") nil up-dir-path)))

;; Returns true if the `dir-path` contains `dependencies-file-name` and `nil` otherwise
(defun contains-dependency-file (dir-path)
  (let ((ls (directory-files-and-attributes dir-path nil dependencies-file-name)))
    (not (null ls))))



(provide 'elm-util)

;;; elm-util.el ends here
