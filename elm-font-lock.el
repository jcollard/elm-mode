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

(defconst keywords 
  '("data" "let" "case" "in" "if" 
    "then" "else" "otherwise" "module" 
    "import" "open" "as" "type"
    "foreign")
  "Special Keywords")

(defconst regexp-keywords         
  (concat (concat "\\<" (regexp-opt keywords t)) "\\>"))

(defconst regexp-single-line-comment
  "\-\-[^\n]*")

;; TODO: Not sure why this only works on loading of an elm file
(defconst regexp-multi-line-comment
  "\{\-[[:unibyte:]]+?\-\}")

  
(defconst elm-font-lock-keywords
  (list
   (cons regexp-keywords font-lock-keyword-face)
   )
  "Highlighting for keywords")

(defconst elm-font-lock-comments
  (list
   (cons regexp-single-line-comment font-lock-comment-face)
   (cons regexp-multi-line-comment font-lock-comment-face))
  "Highlighting for comments")

(defconst elm-font-lock-highlighting
  (append
   elm-font-lock-comments
   elm-font-lock-keywords))

(defun turn-on-elm-font-lock ()
  (setq font-lock-multiline t)
  (set (make-local-variable 'font-lock-defaults) '(elm-font-lock-highlighting)))



(provide 'elm-font-lock)
