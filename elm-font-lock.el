;;; elm-font-lock.el --- Font locking module for Elm mode.

;; Copyright (C) 2013, 2014 Joseph Collard
;; Copyright (C) 2015 Bogdan Popa

;; Authors: Joseph Collard
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
(require 'rx)

(defgroup elm-font-lock nil
  "Font locking for Elm code."
  :group 'faces)

(defface elm-font-lock-multiline-list-delimiters
  '((t :inherit font-lock-keyword-face))
  "The default face used to highlight brackets and commas in multiline lists."
  :group 'elm-font-lock)

(defcustom elm-font-lock-multiline-list-delimiters-face 'elm-font-lock-multiline-list-delimiters
  "The face used to highlight brackets and commas in multilist lists.
To disable this highlighting, set this to nil."
  :type '(choice (const nil)
                 face)
  :group 'elm-font-lock)

(defconst elm--keywords
  '("let" "case" "in" "if" "of" "then" "else" "otherwise"
    "module" "import" "as" "exposing"  "type" "where"
    "alias" "port" "infixr" "infixl")
  "Reserved keywords.")

(defconst elm--regexp-keywords
  (concat (concat "\\<" (regexp-opt elm--keywords t)) "\\>")
  "A regular expression representing the reserved keywords.")

(defconst elm--font-lock-keywords
  (cons elm--regexp-keywords font-lock-keyword-face)
  "Highlighting for keywords.")

(defun elm--syntax-propertize (start end)
  "The syntax propertize function for setting single line comments over START to END."
  (let ((case-fold-search nil))
    (goto-char start)
    (funcall
     (syntax-propertize-rules
      ;;; Syntax rule for -- comments
      ((rx (and (0+ " ") (group "--")
                (0+ any) (group "\n")))
       (1 "< b")
       (2 "> b"))

      ;;; Syntax rule for char literals
      ((rx (and (1+ " ")
                (group "'")
                (optional "\\") any
                (group "'")))
       (1 "\"")
       (2 "\"")))
     start end)))

(defvar elm--syntax-table
  (let ((st (make-syntax-table)))
    ;;; Syntax entry for {- -} type comments.
    (modify-syntax-entry ?{ "(} 1n" st)
    (modify-syntax-entry ?- ". 23n" st)
    (modify-syntax-entry ?} "){ 4n" st)
    st))

(defconst elm--regexp-function
  "^[a-z][^[:space:][:punct:]]*"
  "A regular expression representing function names.")

(defconst elm--font-lock-functions
  (cons elm--regexp-function font-lock-function-name-face)
  "Highlighting for function names.")

(defconst elm--regexp-type
  "\\<[A-Z][^[:space:].]*\\>"
  "A regular expression representing modules and types.")

(defconst elm--font-lock-types
  (cons elm--regexp-type font-lock-type-face)
  "Highlighting for module names and types.")

(defconst elm--regexp-multiline-list-comma-closing-brackets
  (concat "^[[:space:]]*" (regexp-opt '("," "]" "}") t))
  "A regular expression representing commas and closing brackets in multiline lists and records.")

(defconst elm--font-lock-multiline-list-comma-closing-brackets
  (cons elm--regexp-multiline-list-comma-closing-brackets
        '(1 elm-font-lock-multiline-list-delimiters-face))
  "Highlighting for commas and closing brackets in multiline lists and records.")

(defun elm--match-multiline-list-opening-bracket (limit)
  "Highlighting search function for opening brackets in multiline lists and records.
Also highlights opening brackets without a matching bracket."
  (when (elm--search-forward-opening-bracket limit)
    (let ((opening (point))
          (eol (line-end-position))
          (closing (elm--search-forward-closing-bracket)))
      (if (or (= closing opening) (> closing eol))
          (progn
            (set-match-data (match-data))
            (goto-char (+ 1 opening))
            t)
        (elm--match-multiline-list-opening-bracket limit)))))

(defun elm--search-forward-opening-bracket (limit)
  "Go to the next opening bracket up to LIMIT."
  (if (search-forward-regexp (regexp-opt '("[" "{")) limit t)
      (progn
        (backward-char)
        t)))

(defun elm--search-forward-closing-bracket ()
  "Go to the next matching bracket, assuming that the cursor is on an opening bracket."
  (ignore-errors
    (save-match-data
      (forward-sexp)))
  (point))

(defconst elm--font-lock-multiline-list-opening-brackets
  '(elm--match-multiline-list-opening-bracket (0 elm-font-lock-multiline-list-delimiters-face))
  "Highlighting for opening brackets in multiline lists and records.")

(defconst elm--font-lock-highlighting
  (list (list elm--font-lock-keywords
              elm--font-lock-functions
              elm--font-lock-types
              elm--font-lock-multiline-list-comma-closing-brackets
              elm--font-lock-multiline-list-opening-brackets)
        nil nil))

(defun turn-on-elm-font-lock ()
  "Turn on Elm font lock."
  (setq font-lock-multiline t)
  (set-syntax-table elm--syntax-table)
  (set (make-local-variable 'syntax-propertize-function) #'elm--syntax-propertize)
  (set (make-local-variable 'font-lock-defaults) elm--font-lock-highlighting))

(provide 'elm-font-lock)
;;; elm-font-lock.el ends here
