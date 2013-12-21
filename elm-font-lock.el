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

(require 'font-lock)
(with-no-warnings (require 'cl))


;; Reserved Keywords

(defconst keywords 
  '("data" "let" "case" "in" "if" 
    "then" "else" "otherwise" "module" 
    "import" "open" "as" "type"
    "foreign")
  "Special Keywords")

(defconst regexp-keywords         
  (concat (concat "\\<" (regexp-opt keywords t)) "\\>"))

(defconst elm-font-lock-keywords
  (list
   (cons regexp-keywords font-lock-keyword-face))
  "Highlighting for keywords")

;; Comments

;; TODO: Comments like the following
;;   -- "literal string"
;; fail due to the "string"
(defconst regexp-single-line-comment
  "\\(\-\-[^\n]*\\)")

(defvar elm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?{ ". 1n" st)
    (modify-syntax-entry ?- ". 23n" st)
    (modify-syntax-entry ?} ". 4n" st)
   st))

(defconst elm-font-lock-comments
  (list
   (cons regexp-single-line-comment font-lock-comment-face))
  "Highlighting for comments")

;; Function names
(defconst regexp-function
  "^[a-z][^ ]*")

(defconst elm-font-lock-functions
  (list
   (cons regexp-function font-lock-function-name-face))
  "Highlighting for function names")

;; Types and Modules
(defconst regexp-type
  "[A-Z][^ \n]*")

(defconst elm-font-lock-types
  (list
   (cons regexp-type font-lock-type-face))
  "Highlighting for module names and types")

(defconst elm-font-lock-highlighting
  (append
   elm-font-lock-comments
   elm-font-lock-keywords
   elm-font-lock-functions
   elm-font-lock-types))

(defun turn-on-elm-font-lock ()
  (setq font-lock-multiline t)
  (set-syntax-table elm-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(elm-font-lock-highlighting)))


(provide 'elm-font-lock)
