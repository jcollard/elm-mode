;;; elm-font-lock.el --- Font locking module for Elm mode

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

(require 'font-lock)
(with-no-warnings (require 'cl))


;; Reserved Keywords

(defconst keywords 
  '("let" "case" "in" "if" "of"
    "then" "else" "otherwise" "module" 
    "import" "as" "type" "where"
    "alias" "port" "infixr" "infixl")
  "Special Keywords")

(defconst regexp-keywords         
  (concat (concat "\\<" (regexp-opt keywords t)) "\\>"))

(defconst elm-font-lock-keywords
   (cons regexp-keywords font-lock-keyword-face)
  "Highlighting for keywords")

;; the syntax propertize function for setting single line comments
(defun elm-syntax-propertize (start end)
  (goto-char start)
  (funcall
   (syntax-propertize-rules
           ("^[[:space:]]*\\(--\\).*\\(
\\)"
            (1 "< b")
            (2 "> b")
            )
           )
   start end))

(defvar elm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?{ "(} 1n" st)
    (modify-syntax-entry ?- ". 23n" st)
    (modify-syntax-entry ?} "){ 4n" st)
   st))

;; Function names
(defconst regexp-function
  "^[a-z][^[:space:][:punct:]]*")
;  "^[a-z][^[:space:]]*")

(defconst elm-font-lock-functions
   (cons regexp-function font-lock-function-name-face)
  "Highlighting for function names")

;; Types and Modules
(defconst regexp-type
  "\\<[A-Z][^[:space:].]*\\>")

(defconst elm-font-lock-types
   (cons regexp-type font-lock-type-face)
  "Highlighting for module names and types")

(defconst elm-font-lock-highlighting
  (list (list
         elm-font-lock-keywords
         elm-font-lock-functions
         elm-font-lock-types
      ; nil turns off string/comment highlighting
      ; t turns off capital letter matching
         ) nil nil))

(defun turn-on-elm-font-lock ()
  (setq font-lock-multiline t)
  (set-syntax-table elm-mode-syntax-table)
  (set (make-local-variable 'syntax-propertize-function) #'elm-syntax-propertize)
  (set (make-local-variable 'font-lock-defaults) elm-font-lock-highlighting))


(provide 'elm-font-lock)

;;; elm-font-lock.el ends here
