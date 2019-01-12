;;; elm-meta.el --- Meta data about Elm source and projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library contains functionality for getting metadata about the
;; current project, the code it contains, and its dependencies.

;; It is intended to form the basis for code navigation tools and
;; completion.

;;; Code:

(require 'cl-lib)
(require 'let-alist)
(require 'json)
(require 's)
(require 'dash)

;; TODO: Elm version might be a dep range, e.g.
;; TODO: Match dependency versions
;; TODO: when "exposed-modules" list exists, limit searched modules sometimes?

(cl-defstruct (elm-meta-package
               (:constructor elm-meta-package--new))
  "Information about an Elm package.

Slots:

`root'
     Directory in which elm.json or elm-package.json is found

`package-file'
     Full path to elm.json or elm-package.json

`elm-version'
     Elm version spec, e.g. \"0.19.0\", or \"0.19.0 <= v < 0.20.0\"

`dependencies'
     Dependencies as a list of (package-name . version-spec) string pairs

`source-dirs'
     Full paths to all source dirs in this package"
  root package-file elm-version dependencies source-dirs)

(defun elm-meta--locate-package-file (file)
  "Search upwards from FILE and return the path of the Elm package json file, or NIL."
  (or
   (-when-let (root (locate-dominating-file file "elm.json"))
     (expand-file-name "elm.json" root))
   (-when-let (root (locate-dominating-file file "elm-package.json"))
     (expand-file-name "elm-package.json" root))))

(defun elm-meta-package-info (dir)
  "Retrieve an `elm-meta-package' struct about the Elm package containing DIR."
  (-when-let (package-file (elm-meta--locate-package-file dir))
    (let ((elm-json-dir (file-name-directory package-file)))
      (let-alist (let ((json-array-type 'list))
                   (json-read-file package-file))
        (elm-meta-package--new
         :root elm-json-dir
         :package-file package-file
         :elm-version .elm-version
         :dependencies (mapcar (lambda (dep) (cons (symbol-name (car dep)) (cdr dep)))
                               (if (consp .dependencies.direct)
                                   (append .dependencies.direct .dependencies.indirect)
                                 .dependencies))
         :source-dirs (mapcar (lambda (d) (expand-file-name d elm-json-dir))
                              (or .source-directories '("src"))))))))

(defun elm-meta-dependency-info (package-info dependency)
  "Given PACKAGE-INFO, find the package info of its cached DEPENDENCY package.
DEPENDENCY is a (name . version-spec) string pair."
  (assert (consp dependency))
  ;; ~/.elm/0.19.0/package/elm/core/1.0.0
  (let* ((dep-name (car dependency))
         (dep-version (cdr dependency))
         (package-cache-dir
          (expand-file-name (file-name-as-directory (concat (elm-meta-package-elm-version package-info) "/package/" dep-name)) "~/.elm/")))
    (unless (file-directory-p package-cache-dir)
      (error "Missing package cache directory: %s" package-cache-dir))
    ;; TODO: consider only versions satisfying `dep-version'
    (let* ((versions (seq-filter (lambda (f) (string-match-p "^[0-9]" f))
                                 (directory-files package-cache-dir)))
           (highest (car (nreverse (seq-sort 'version< versions)))))
      (unless highest
        (error "No cached versions for dependency package %s in %s" dependency package-cache-dir))
      (elm-meta-package-info (expand-file-name highest package-cache-dir)))))

(defun elm-meta-find-module-source (package-info module)
  "Given PACKAGE-INFO, find the path of the Elm file defining MODULE.
If the package itself does not define module, also search its dependencies."
  (or (elm-meta--find-module-source-in-package package-info module)
      (cl-block nil
        (dolist (dep (elm-meta-package-dependencies package-info))
          (-when-let (found (elm-meta--find-module-source-in-package
                             (elm-meta-dependency-info package-info dep)
                             module))
            (cl-return found))))))

(defun elm-meta--find-module-source-in-package (package-info module)
  "Given PACKAGE-INFO, find the path of the Elm file defining MODULE."
  (let ((module-as-sub-path (concat (s-replace "." "/" module) ".elm")))
    (cl-block nil
      (dolist (dir (elm-meta-package-source-dirs package-info))
        (let ((elm-file (expand-file-name module-as-sub-path dir)))
          (when (file-exists-p elm-file)
            (cl-return elm-file)))))))


(provide 'elm-meta)
;;; elm-meta.el ends here
