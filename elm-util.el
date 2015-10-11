;;; elm-utils.el --- General utility functions used by Elm mode modules.

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
(require 'f)
(require 'json)
(require 'let-alist)
(require 's)

(require 'haskell-decl-scan nil 'noerror)
(require 'inf-haskell nil 'noerror)

(defconst elm-package-json
  "elm-package.json"
  "The name of the package JSON configuration file.")

(defun elm--get-module-name ()
  "Return the qualified name of the module in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "module +\\([A-Z][A-Za-z0-9.]*\\)" nil t)
      (error "Module declaration not found"))
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun elm--get-decl ()
  "Return the current declaration.

Relies on `haskell-mode' stuff."
  (unless (fboundp #'haskell-ds-backward-decl)
    (error "This functionality requires haskell-mode"))

  (save-excursion
    (goto-char (1+ (point)))
    (let* ((start (or (haskell-ds-backward-decl) (point-min)))
           (end (or (haskell-ds-forward-decl) (point-max)))
           (raw-decl (s-trim-right (buffer-substring start end)))
           (lines (split-string raw-decl "\n"))
           (first-line (car lines)))

      (inferior-haskell-flash-decl start end)
      (if (string-match-p "^[a-z].*:" first-line)
          (cdr lines)
        lines))))

(defun elm--build-import-statement ()
  "Generate a statement that will import the current module."
  (concat "import " (elm--get-module-name) " exposing (..) \n"))

(defun elm--get-buffer-dirname ()
  "Return the absolute dirname of the current buffer."
  (concat (f-dirname (buffer-file-name)) "/"))

(defun elm--buffer-local-file-name ()
  "Return the current file name relative to the dependency file."
  (let ((dirname (buffer-file-name))
        (deppath (elm--find-dependency-file-path)))
    (f-relative dirname deppath)))

(defun elm--find-dependency-file-path ()
  "Recursively search for a directory containing a package JSON file."
  (let* ((path (f-traverse-upwards
                (lambda (path)
                  (f-exists? (f-expand elm-package-json path)))))
         (path (if (not path)
                   (f-dirname (buffer-file-name))
                 path)))

    (concat path "/")))

(defun elm--has-dependency-file ()
  "Check if a dependency file exists."
  (f-exists? (f-join (elm--find-dependency-file-path) elm-package-json)))

(defun elm--assert-dependency-file ()
  "Report an error unless there is a package file."
  (unless (elm--has-dependency-file)
    (error "Elm package file not found")))

(defun elm--read-dependency-file ()
  "Find and read the JSON dependency file into an object."
  (elm--assert-dependency-file)
  (let ((dep-file (f-join (elm--find-dependency-file-path) elm-package-json)))
    (json-read-file dep-file)))

(defun elm--find-main-file ()
  "Find the Main.elm file."
  (let-alist (elm--read-dependency-file)
    (let ((source-dir (aref .source-directories 0)))
      (if (equal "." source-dir)
          "Main.elm"
        (f-join source-dir "Main.elm")))))

(provide 'elm-util)
;;; elm-util.el ends here
