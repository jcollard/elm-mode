;;; elm-indentation.el -- indentation module for Elm Mode

;; Copyright (C) 2009  Kristof Bastiaensen

;; Author: Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>

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

;; Installation:
;;
;; To turn indentation on for all Elm buffers under Elm mode
;; <http://www.elm.org/elm-mode/> add this to .emacs:
;;
;;    (add-hook elm-mode-hook 'turn-on-elm-indentation)
;;
;; Otherwise, call `elm-indentation-mode'.

;;; Code:

(require 'syntax)
(with-no-warnings (require 'cl))

(defvar elm-literate nil
  "*If not nil, the current buffer contains a literate Elm script.
Possible values are: `bird' and `tex', for Bird-style and LaTeX-style
literate scripts respectively.  Set by `elm-mode' and
`literate-elm-mode'.  For an ambiguous literate buffer -- i.e. does
not contain either \"\\begin{code}\" or \"\\end{code}\" on a line on
its own, nor does it contain \">\" at the start of a line -- the value
of `elm-literate-default' is used.")
(make-variable-buffer-local 'elm-literate)
(put 'elm-literate 'safe-local-variable 'symbolp)
;; Default literate style for ambiguous literate buffers.
(defcustom elm-literate-default 'bird
  "Default value for `elm-literate'.
Used if the style of a literate buffer is ambiguous.  This variable should
be set to the preferred literate style."
  :group 'elm
  :type '(choice (const bird) (const tex) (const nil)))

;; Dynamically scoped variables.
(defvar following-token)
(defvar current-token)
(defvar left-indent)
(defvar starter-indent)
(defvar current-indent)
(defvar layout-indent)
(defvar parse-line-number)
(defvar possible-indentations)
(defvar indentation-point)
(defvar elm-literate)

(defgroup elm-indentation nil
  "Elm indentation."
  :link '(custom-manual "(elm-mode)Indentation")
  :group 'elm
  :prefix "elm-indentation-")

(defcustom elm-indentation-cycle-warn t
  "Warn before moving to the leftmost indentation, if you tab at the rightmost one."
  :type 'boolean
  :group 'elm-indentation)

(defcustom elm-indentation-delete-backward-indentation t
  "Delete backward removes indentation."
  :type 'boolean
  :group 'elm-indentation)

(defcustom elm-indentation-delete-backward-jump-line nil
  "Delete backward jumps to the previous line when removing last indentation."
  :type 'boolean
  :group 'elm-indentation)

(defcustom elm-indentation-delete-indentation t
  "Delete removes indentation."
  :type 'boolean
  :group 'elm-indentation)

(defcustom elm-indentation-layout-offset 2
  "Extra indentation to add before expressions in a elm layout list."
  :type 'integer
  :group 'elm-indentation)

(defcustom elm-indentation-starter-offset 1
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'elm-indentation)

(defcustom elm-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'elm-indentation)

(defcustom  elm-indentation-ifte-offset 2
  "Extra indentation after the keywords `if' `then' or `else'."
  :type 'integer
  :group 'elm-indentation)

(defcustom elm-indentation-where-pre-offset 2
  "Extra indentation before the keyword `where'."
  :type 'integer
  :group 'elm-indentation)

(defcustom elm-indentation-where-post-offset 2
  "Extra indentation after the keyword `where'."
  :type 'integer
  :group 'elm-indentation)

(defcustom elm-indentation-birdtrack-extra-space t
  "Append a space after every birdtrack in literate mode."
  :type 'boolean
  :group 'elm-indentation)


;; Avoid a global bogus definition (which the original run-time
;; `defun' made), and support Emacs 21 without the syntax.el add-on.
(eval-when-compile
  (unless (fboundp 'syntax-ppss)
    (defsubst syntax-ppss (&rest pos)
      (parse-partial-sexp (point-min) (or pos (point))))))

(defconst elm-indentation-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [?\r] 'elm-newline-and-indent)
    (define-key keymap [backspace] 'elm-indentation-delete-backward-char)
    (define-key keymap [?\C-d] 'elm-indentation-delete-char)
    keymap))

;; Used internally
(defvar elm-indent-last-position)

;;;###autoload
(define-minor-mode elm-indentation-mode
  "Elm indentation mode that deals with the layout rule.
It rebinds RET, DEL and BACKSPACE, so that indentations can be
set and deleted as if they were real tabs.  It supports
autofill-mode."
  :lighter " Ind"
  :keymap elm-indentation-mode-map
  (kill-local-variable 'indent-line-function)
  (kill-local-variable 'normal-auto-fill-function)
  (when elm-indentation-mode
    (setq max-lisp-eval-depth (max max-lisp-eval-depth 600)) ;; set a higher limit for recursion
    (set (make-local-variable 'indent-line-function)
         'elm-indentation-indent-line)
    (set (make-local-variable 'normal-auto-fill-function)
         'elm-indentation-auto-fill-function)
    (set (make-local-variable 'elm-indent-last-position)
         nil)))

;;;###autoload
(defun turn-on-elm-indentation ()
  "Turn on the elm-indentation minor mode."
  (interactive)
  (elm-indentation-mode t))

(put 'parse-error
     'error-conditions
     '(error parse-error))
(put 'parse-error 'error-message "Parse error")

(defun parse-error (&rest args)
  (signal 'parse-error (apply 'format args)))

(defmacro on-parse-error (except &rest body)
  `(condition-case parse-error-string
       (progn ,@body)
     (parse-error
      ,except
      (message "%s" (cdr parse-error-string)))))

(defun elm-current-column ()
  "Compute current column according to elm syntax rules,
  correctly ignoring composition."
  (save-excursion
    (let ((start (point))
          (cc 0))
      (beginning-of-line)
      (while (< (point) start)
        (if (= (char-after) ?\t)
            (setq cc (* 8 (+ 1 (/ cc 8))))
          (incf cc))
        (forward-char))
      cc)))

(defun kill-indented-line (&optional arg)
  "`kill-line' for indented text.
Preserves indentation and removes extra whitespace"
  (interactive "P")
  (let ((col (elm-current-column))
        (old-point (point)))
    (cond ((or (and (numberp arg) (< arg 0))
               (and (not (looking-at "[ \t]*$"))
                    (or (not (numberp arg)) (zerop arg))))
                                        ;use default behavior when calling with a negative argument
                                        ;or killing (once) from the middle of a line
           (kill-line arg))
          ((and (skip-chars-backward " \t") ;always true
                (bolp)
                (save-excursion
                  (forward-line arg)
                  (not (looking-at "[ \t]*$"))))
                                        ; killing from an empty line:
                                        ; preserve indentation of the next line
           (kill-region (point)
                        (save-excursion
                          (forward-line arg)
                          (point)))
           (skip-chars-forward " \t")
           (if (> (elm-current-column) col)
               (move-to-column col)))
          (t                            ; killing from not empty line:
                                        ; kill all indentation
           (goto-char old-point)
           (kill-region (point)
                        (save-excursion
                          (forward-line arg)
                          (skip-chars-forward " \t")
                          (point)))))))

(defun elm-indentation-auto-fill-function ()
  (when (> (elm-current-column) fill-column)
    (while (> (elm-current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((auto-fill-function nil)
          (indent (car (last (elm-indentation-find-indentations)))))
      (newline)
      (when (eq elm-literate 'bird)
        (insert ">"))
      (indent-to indent)
      (end-of-line))))

(defun elm-indentation-reindent (col)
  (beginning-of-line)
  (delete-region (point)
                 (progn
                   (when (and (eq elm-literate 'bird)
                              (eq (char-after) ?>))
                     (forward-char))
                   (skip-syntax-forward "-")
                   (point)))
  (when (eq elm-literate 'bird)
    (insert ">"))
  (indent-to col))

(defun elm-indentation-current-indentation ()
  (if (eq elm-literate 'bird)
      (save-excursion
        (beginning-of-line)
        (forward-char)
        (skip-syntax-forward "-")
        (current-column))
    (current-indentation)))

(defun elm-indentation-outside-bird-line ()
  (and (eq elm-literate 'bird)
       (or (< (current-column) 2)
           (save-excursion
             (beginning-of-line)
             (not (eq (char-after) ?>))))))

(defun elm-newline-and-indent ()
  (interactive)
  (if (elm-indentation-outside-bird-line)
      (progn
        (delete-horizontal-space)
        (newline))
    (on-parse-error
     (newline)
     (let* ((cc (elm-current-column))
            (ci (elm-indentation-current-indentation))
            (indentations (elm-indentation-find-indentations)))
       (skip-syntax-forward "-")
       (if (prog1 (and (eolp)
                       (not (= (elm-current-column) ci)))
             (delete-horizontal-space)
             (if (not (eq elm-literate 'bird))
                 (newline)
               (when elm-indentation-birdtrack-extra-space
                 (indent-to 2))
               (newline)
               (insert "> ")))
           (elm-indentation-reindent
            (max (elm-indentation-butlast indentations)
                 (elm-indentation-matching-indentation
                  ci indentations)))
         (elm-indentation-reindent (elm-indentation-matching-indentation
                                        cc indentations)))))))

(defun elm-indentation-one-indentation (col indentations)
  (let* ((last-pair (last indentations)))
    (cond ((null indentations)
           col)
          ((null (cdr indentations))
           (car indentations))
          ((<= col (car last-pair))
           col)
          (t (car last-pair)))))

(defun elm-indentation-butlast (indentations)
  (when (consp (cdr indentations))
    (while (cddr indentations)
      (setq indentations (cdr indentations))))
  (car indentations))

(defun elm-indentation-next-indentation (col indentations)
  "Find the lefmost indentation which is greater than COL."
  (catch 'return
    (while indentations
      (if (or (< col (car indentations))
              (null (cdr indentations)))
          (throw 'return (car indentations))
        (setq indentations (cdr indentations))))
    col))

(defun elm-indentation-previous-indentation (col indentations)
  "Find the rightmost indentation which is less than COL."
  (and indentations
       (> col (car indentations))
       (catch 'return
         (while indentations
           (if (or (null (cdr indentations))
                   (<= col (cadr indentations)))
               (throw 'return (car indentations))
             (setq indentations (cdr indentations))))
         col)))

(defun elm-indentation-matching-indentation (col indentations)
  "Find the leftmost indentation which is greater than or equal to COL."
  (catch 'return
    (while indentations
      (if (or (<= col (car indentations))
              (null (cdr indentations)))
          (throw 'return (car indentations))
        (setq indentations (cdr indentations))))
    col))

(defun elm-indentation-indent-line ()
  (when (save-excursion
          (beginning-of-line)
          (not (nth 8 (syntax-ppss))))
    (let ((ci (elm-indentation-current-indentation))
          (start-column (elm-current-column)))
      (cond ((> (elm-current-column) ci)
             (save-excursion
               (move-to-column ci)
               (elm-indentation-reindent
                (elm-indentation-one-indentation
                 ci (elm-indentation-find-indentations)))))

            ((= (elm-current-column) ci)
             (elm-indentation-reindent
              (elm-indentation-next-indentation
               ci (elm-indentation-find-indentations))))

            (t (move-to-column ci)
               (elm-indentation-reindent
                (elm-indentation-matching-indentation
                 ci (elm-indentation-find-indentations)))))
      (cond ((not (= (elm-current-column) start-column))
             (setq elm-indent-last-position nil))
            ((not elm-indentation-cycle-warn)
             (elm-indentation-reindent
              (elm-indentation-next-indentation
               -1
               (elm-indentation-find-indentations))))
            ((not (equal (point) elm-indent-last-position))
             (message "Press TAB again to go to the leftmost indentation")
             (setq elm-indent-last-position (point)))
            (t
             (elm-indentation-reindent
              (elm-indentation-next-indentation
               -1
               (elm-indentation-find-indentations))))))))

(defun elm-indentation-delete-backward-char (n)
  (interactive "p")
  (on-parse-error
   (delete-char (- n))
   (cond
    ((elm-indentation-outside-bird-line)
     (delete-char (- n)))
    ((and (use-region-p)
          delete-active-region
          (not (= (point) (mark))))
     (delete-region (mark) (point)))
    ((or (= (elm-current-column) 0)
         (> (elm-current-column) (elm-indentation-current-indentation))
         (nth 8 (syntax-ppss)))
     (delete-char (- n)))
    (elm-indentation-delete-backward-indentation
     (let* ((ci (elm-indentation-current-indentation))
            (pi (elm-indentation-previous-indentation
                 ci (elm-indentation-find-indentations))))
       (save-excursion
         (cond (pi
                (move-to-column pi)
                (delete-region (point)
                               (progn (move-to-column ci)
                                      (point))))
               (t
                (if (not elm-indentation-delete-backward-jump-line)
                    (delete-char (- 1))
                  (beginning-of-line)
                  (delete-region (max (point-min) (- (point) 1))
                                 (progn (move-to-column ci)
                                        (point)))))))))
    (t (delete-char (- n))))))

(defun elm-indentation-delete-char (n)
  (interactive "p")
  (if (elm-indentation-outside-bird-line)
      (delete-char n)
    (on-parse-error (delete-char n)
                    (cond
                     ((and delete-selection-mode
                           mark-active
                           (not (= (point) (mark))))
                      (delete-region (mark) (point)))
                     ((and (eq elm-literate 'bird)
                           (looking-at "\n> "))
                      (delete-char (+ n 2)))
                     ((or (eolp)
                          (>= (elm-current-column) (elm-indentation-current-indentation))
                          (nth 8 (syntax-ppss)))
                      (delete-char n))
                     (elm-indentation-delete-indentation
                      (let* ((ci (elm-indentation-current-indentation))
                             (pi (elm-indentation-previous-indentation
                                  ci (elm-indentation-find-indentations))))
                        (save-excursion
                          (if (and pi (> pi (elm-current-column)))
                              (move-to-column pi))
                          (delete-region (point)
                                         (progn (move-to-column ci)
                                                (point))))))
                     (t (delete-char (- n)))))))

(defun elm-indentation-goto-least-indentation ()
  (beginning-of-line)
  (if (eq elm-literate 'bird)
      (catch 'return
        (while t
          (when (not (eq (char-after) ?>))
            (forward-line)
            (forward-char 2)
            (throw 'return nil))
          (let ((ps (nth 8 (syntax-ppss))))
            (when ps ;; inside comment or string
              (goto-char ps)
              (beginning-of-line)))
          (when (and (>= 2 (elm-indentation-current-indentation))
                     (not (looking-at ">\\s-*$")))
            (forward-char 2)
            (throw 'return nil))
          (when (bobp)
            (forward-char 2)
            (throw 'return nil))
          (forward-line -1)))
    ;; not bird style
    (catch 'return
      (while (not (bobp))
        (forward-comment (- (buffer-size)))
        (beginning-of-line)
        (let ((ps (nth 8 (syntax-ppss))))
          (when ps ;; inside comment or string
            (goto-char ps)))
        (when (= 0 (elm-indentation-current-indentation))
          (throw 'return nil))))
    (beginning-of-line)
    (when (bobp)
      (forward-comment (buffer-size)))))

(defun elm-indentation-parse-to-indentations ()
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
          (layout-indent 0)
          (parse-line-number 0)
          (current-indent elm-indentation-layout-offset)
          (starter-indent elm-indentation-layout-offset)
          (left-indent elm-indentation-layout-offset)
          (case-fold-search nil)
          current-token
          following-token
          possible-indentations)
      (elm-indentation-goto-least-indentation)
      (if (<= indentation-point (point))
          (elm-indentation-first-indentation)
        (setq current-token (elm-indentation-peek-token))
        (catch 'parse-end
          (elm-indentation-toplevel)
          (unless (eq current-token 'end-tokens)
            (parse-error "Illegal token: %s" current-token)))
        possible-indentations))))

(defun elm-indentation-first-indentation ()
  (if (eq elm-literate 'bird) '(2) '(0)))

(defun elm-indentation-find-indentations ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (elm-indentation-first-indentation))
     ((nth 4 ppss)
      (if (save-excursion
            (and (skip-syntax-forward "-")
                 (eolp)
                 (not (> (forward-line 1) 0))
                 (not (nth 4 (syntax-ppss)))))
          (elm-indentation-parse-to-indentations)
        (elm-indentation-first-indentation)))
     (t
      (elm-indentation-parse-to-indentations)))))

(defconst elm-indentation-unicode-tokens
  '(("→" . "->")     ;; #x2192 RIGHTWARDS ARROW
    ("∷" . "::")     ;; #x2237 PROPORTION
    ("←" . "<-")     ;; #x2190 LEFTWARDS ARROW
    ("⇒" . "=>")     ;; #x21D2 RIGHTWARDS DOUBLE ARROW
    ("∀" . "forall") ;; #x2200 FOR ALL
    ("↢" . "-<")     ;; #x2919 LEFTWARDS ARROW-TAIL
    ("↣" . ">-")     ;; #x291A RIGHTWARDS ARROW-TAIL
    ("⤛" . "-<<")    ;; #x291B LEFTWARDS DOUBLE ARROW-TAIL
    ("⤜" . ">>-")    ;; #x291C RIGHTWARDS DOUBLE ARROW-TAIL
    ("★" . "*"))     ;; #x2605 BLACK STAR
  "Translation dictionary from UnicodeSyntax tokens to their ASCII representation.")

(defconst elm-indentation-toplevel-list
  '(("module" . elm-indentation-module)
    ("data" . (lambda () (elm-indentation-statement-right #'elm-indentation-data)))
    ("type" . (lambda () (elm-indentation-statement-right #'elm-indentation-data)))
    ("newtype" . (lambda () (elm-indentation-statement-right #'elm-indentation-data)))
    ("class" . elm-indentation-class-declaration)
    ("instance" . elm-indentation-class-declaration )))

(defconst elm-indentation-type-list
  '(("::"    . (lambda () (elm-indentation-with-starter
                           (lambda () (elm-indentation-separated #'elm-indentation-type "->" nil)) nil)))
    ("("     . (lambda () (elm-indentation-list #'elm-indentation-type
                                                    ")" "," nil)))
    ("["     . (lambda () (elm-indentation-list #'elm-indentation-type
                                                    "]" "," nil)))
    ("{"     . (lambda () (elm-indentation-list #'elm-indentation-type
                                                    "}" "," nil)))))

(defconst elm-indentation-expression-list
  '(("data" . elm-indentation-data)
    ("type" . elm-indentation-data)
    ("newtype" . elm-indentation-data)
    ("if"    . (lambda () (elm-indentation-phrase
                           '(elm-indentation-expression
                             "then" elm-indentation-expression
                             "else" elm-indentation-expression))))
    ("let"   . (lambda () (elm-indentation-phrase
                           '(elm-indentation-declaration-layout
                             "in" elm-indentation-expression))))
    ("do"    . (lambda () (elm-indentation-with-starter
                           #'elm-indentation-expression-layout nil)))
    ("mdo"   . (lambda () (elm-indentation-with-starter
                           #'elm-indentation-expression-layout nil)))
    ("rec"   . (lambda () (elm-indentation-with-starter
                           #'elm-indentation-expression-layout nil)))
    ("case"  . (lambda () (elm-indentation-phrase
                           '(elm-indentation-expression
                             "of" elm-indentation-case-layout))))
    ("\\"    . (lambda () (elm-indentation-with-starter
                           #'elm-indentation-lambda-maybe-lambdacase nil)))
    ("proc"  . (lambda () (elm-indentation-phrase
                           '(elm-indentation-expression
                             "->" elm-indentation-expression))))
    ("where" . (lambda () (elm-indentation-with-starter
                           #'elm-indentation-declaration-layout nil t)))
    ("::"    . (lambda () (elm-indentation-with-starter
                           (lambda () (elm-indentation-separated #'elm-indentation-type "->" nil)) nil)))
    ("="     . (lambda () (elm-indentation-statement-right #'elm-indentation-expression)))
    ("<-"    . (lambda () (elm-indentation-statement-right #'elm-indentation-expression)))
    ("("     . (lambda () (elm-indentation-list #'elm-indentation-expression
                                                    ")" '(list "," "->") nil)))
    ("["     . (lambda () (elm-indentation-list #'elm-indentation-expression
                                                    "]" "," "|")))
    ("{"     . (lambda () (elm-indentation-list #'elm-indentation-expression
                                                    "}" "," nil)))))

(defun elm-indentation-expression-layout ()
  (elm-indentation-layout #'elm-indentation-expression))

(defun elm-indentation-declaration-layout ()
  (elm-indentation-layout #'elm-indentation-declaration))

(defun elm-indentation-case-layout ()
  (elm-indentation-layout #'elm-indentation-case))

;; After a lambda (backslash) there are two possible cases:
;;   - the new lambdacase expression, that can be recognized by the
;;     next token being "case",
;;   - or simply an anonymous function definition in the form of
;;     "expression -> expression".
(defun elm-indentation-lambda-maybe-lambdacase ()
  (if (string= current-token "case")
      (elm-indentation-with-starter
       #'elm-indentation-case-layout nil)
    (elm-indentation-phrase-rest
     '(elm-indentation-expression "->" elm-indentation-expression))))

(defun elm-indentation-fundep ()
  (elm-indentation-with-starter
   (lambda () (elm-indentation-separated
               #'elm-indentation-fundep1 "," nil))
   nil))

(defun elm-indentation-fundep1 ()
  (let ((current-indent (elm-current-column)))
    (while (member current-token '(value "->"))
      (elm-indentation-read-next-token))
    (when (and (eq current-token 'end-tokens)
               (member following-token '(value "->")))
      (elm-indentation-add-indentation current-indent))))

(defun elm-indentation-toplevel ()
  (elm-indentation-layout
   (lambda ()
     (let ((parser (assoc current-token elm-indentation-toplevel-list)))
       (if parser
           (funcall (cdr parser))
         (elm-indentation-declaration))))))

(defun elm-indentation-type ()
  (let ((current-indent (elm-current-column)))
    (catch 'return
      (while t
        (cond
         ((member current-token '(value operator "->"))
          (elm-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (when (member following-token
                        '(value operator no-following-token
                                "->" "(" "[" "{" "::"))
            (elm-indentation-add-indentation current-indent))
          (throw 'return nil))

         (t (let ((parser (assoc current-token elm-indentation-type-list)))
              (if (not parser)
                  (throw 'return nil)
                (funcall (cdr parser))))))))))

(defun elm-indentation-data ()
  (elm-indentation-with-starter
   (lambda ()
     (when (string= current-token "instance")
       (elm-indentation-read-next-token))
     (elm-indentation-type)
     (cond ((string= current-token "=")
            (elm-indentation-with-starter
             (lambda () (elm-indentation-separated #'elm-indentation-type "|" "deriving"))
             nil))
           ((string= current-token "where")
            (elm-indentation-with-starter
             #'elm-indentation-expression-layout nil))))
   nil))

(defun elm-indentation-class-declaration ()
  (elm-indentation-with-starter
   (lambda ()
     (elm-indentation-type)
     (when (string= current-token "|")
       (elm-indentation-fundep))
     (when (string= current-token "where")
       (elm-indentation-with-starter
        #'elm-indentation-expression-layout nil)))
   nil))

(defun elm-indentation-module ()
  (elm-indentation-with-starter
   (lambda ()
     (let ((current-indent (elm-current-column)))
       (elm-indentation-read-next-token)
       (when (string= current-token "(")
         (elm-indentation-list
          #'elm-indentation-module-export
          ")" "," nil))
       (when (eq current-token 'end-tokens)
         (elm-indentation-add-indentation current-indent)
         (throw 'parse-end nil))
       (when (string= current-token "where")
         (elm-indentation-read-next-token)
         (when (eq current-token 'end-tokens)
           (elm-indentation-add-layout-indent)
           (throw 'parse-end nil))
         (elm-indentation-layout #'elm-indentation-toplevel))))
   nil))

(defun elm-indentation-module-export ()
  (cond ((string= current-token "module")
         (let ((current-indent (elm-current-column)))
           (elm-indentation-read-next-token)
           (cond ((eq current-token 'end-tokens)
                  (elm-indentation-add-indentation current-indent))
                 ((eq current-token 'value)
                  (elm-indentation-read-next-token)))))
        (t (elm-indentation-type))))

(defun elm-indentation-list (parser end sep stmt-sep)
  (elm-indentation-with-starter
   `(lambda () (elm-indentation-separated #',parser
                                              ,sep
                                              ,stmt-sep))
   end))

(defun elm-indentation-with-starter (parser end &optional where-expr?)
  (let ((starter-column (elm-current-column))
        (current-indent current-indent)
        (left-indent (if (= (elm-current-column) (elm-indentation-current-indentation))
                         (elm-current-column) left-indent)))
    (elm-indentation-read-next-token)
    (when (eq current-token 'end-tokens)
      (if (equal following-token end)
          (elm-indentation-add-indentation starter-column)
        (if where-expr?
            (elm-indentation-add-where-post-indent left-indent)
          (elm-indentation-add-indentation
           (+ left-indent elm-indentation-left-offset))))
      (throw 'parse-end nil))
    (let* ((current-indent (elm-current-column))
           (starter-indent (min starter-column current-indent))
           (left-indent (if end (+ current-indent elm-indentation-starter-offset)
                          left-indent)))
      (funcall parser)
      (cond ((eq current-token 'end-tokens)
             (when (equal following-token end)
               (elm-indentation-add-indentation starter-indent))
             (when end (throw 'parse-end nil))) ;; add no indentations
            ((equal current-token end)
             (elm-indentation-read-next-token)) ;; continue
            (end (parse-error "Illegal token: %s" current-token))))))

(defun elm-indentation-case ()
  (elm-indentation-expression)
  (cond ((eq current-token 'end-tokens)
         (elm-indentation-add-indentation current-indent))
        ((string= current-token "|")
         (elm-indentation-with-starter
          (lambda () (elm-indentation-separated #'elm-indentation-case "|" nil))
          nil))
        ((string= current-token "->")
         (elm-indentation-statement-right #'elm-indentation-expression))
        ;; otherwise fallthrough
        ))

(defun elm-indentation-statement-right (parser)
  (elm-indentation-read-next-token)
  (when (eq current-token 'end-tokens)
    (elm-indentation-add-indentation
     (+ left-indent elm-indentation-left-offset))
    (throw 'parse-end nil))
  (let ((current-indent (elm-current-column)))
    (funcall parser)))

(defun elm-indentation-simple-declaration ()
  (elm-indentation-expression)
  (cond ((string= current-token "=")
         (elm-indentation-statement-right #'elm-indentation-expression))
        ((string= current-token "::")
         (elm-indentation-statement-right #'elm-indentation-type))
        ((and (eq current-token 'end-tokens)
              (string= following-token "="))
         (elm-indentation-add-indentation current-indent)
         (throw 'parse-end nil))))

(defun elm-indentation-declaration ()
  (elm-indentation-expression)
  (cond ((string= current-token "|")
         (elm-indentation-with-starter
          (lambda () (elm-indentation-separated #'elm-indentation-expression "," "|"))
          nil))
        ((eq current-token 'end-tokens)
         (when (member following-token '("|" "=" "::" ","))
           (elm-indentation-add-indentation current-indent)
           (throw 'parse-end nil)))))

(defun elm-indentation-layout (parser)
  (if (string= current-token "{")
      (elm-indentation-list parser "}" ";" nil)
    (elm-indentation-implicit-layout-list parser)))

(defun elm-indentation-expression-token (token)
  (member token '("if" "let" "do" "case" "\\" "(" "[" "::"
                  value operator no-following-token)))

(defun elm-indentation-expression ()
  (let ((current-indent (elm-current-column)))
    (catch 'return
      (while t
        (cond
         ((memq current-token '(value operator))
          (elm-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (cond ((string= following-token "where")
                 (elm-indentation-add-where-pre-indent))
                ((elm-indentation-expression-token following-token)
                 (elm-indentation-add-indentation
                  current-indent)))
          (throw 'return nil))

         (t (let ((parser (assoc current-token elm-indentation-expression-list)))
              (when (null parser)
                (throw 'return nil))
              (funcall (cdr parser))
              (when (and (eq current-token 'end-tokens)
                         (string= (car parser) "let")
                         (= elm-indentation-layout-offset current-indent)
                         (elm-indentation-expression-token following-token))
                ;; inside a layout, after a let construct
                (elm-indentation-add-layout-indent)
                (throw 'parse-end nil))
              (unless (member (car parser) '("(" "[" "{" "do" "case"))
                (throw 'return nil)))))))))

(defun elm-indentation-test-indentations ()
  (interactive)
  (let ((indentations (save-excursion (elm-indentation-find-indentations)))
        (str "")
        (pos 0))
    (while indentations
      (when (>= (car indentations) pos)
        (setq str (concat str (make-string (- (car indentations) pos) ?\ )
                          "|"))
        (setq pos (+ 1 (car indentations))))
      (setq indentations (cdr indentations)))
    (end-of-line)
    (newline)
    (insert str)))

(defun elm-indentation-separated (parser separator stmt-separator)
  (catch 'return
    (while t
      (funcall parser)
      (cond ((if (listp separator) (member current-token separator) (equal current-token separator))
             (elm-indentation-at-separator))

            ((equal current-token stmt-separator)
             (setq starter-indent (elm-current-column))
             (elm-indentation-at-separator))

            ((eq current-token 'end-tokens)
             (cond ((or (equal following-token separator)
                        (equal following-token stmt-separator))
                    (elm-indentation-add-indentation starter-indent)
                    (throw 'parse-end nil)))
             (throw 'return nil))

            (t (throw 'return nil))))))

(defun elm-indentation-at-separator ()
  (let ((separator-column
         (and (= (elm-current-column) (elm-indentation-current-indentation))
              (elm-current-column))))
    (elm-indentation-read-next-token)
    (cond ((eq current-token 'end-tokens)
           (elm-indentation-add-indentation current-indent)
           (throw 'return nil))
          (separator-column ;; on the beginning of the line
           (setq current-indent (elm-current-column))
           (setq starter-indent separator-column)))))

(defun elm-indentation-implicit-layout-list (parser)
  (let* ((layout-indent (elm-current-column))
         (current-indent (elm-current-column))
         (left-indent (elm-current-column)))
    (catch 'return
      (while t
        (let ((left-indent left-indent))
          (funcall parser))
        (cond ((member current-token '(layout-next ";"))
               (elm-indentation-read-next-token))
              ((eq current-token 'end-tokens)
               (when (or (elm-indentation-expression-token following-token)
                         (string= following-token ";"))
                 (elm-indentation-add-layout-indent))
               (throw 'return nil))
              (t (throw 'return nil))))))
  ;; put elm-indentation-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (elm-indentation-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun elm-indentation-phrase (phrase)
  (elm-indentation-with-starter
   `(lambda () (elm-indentation-phrase-rest ',phrase))
   nil))

(defun elm-indentation-phrase-rest (phrase)
  (let ((starter-line parse-line-number))
    (let ((current-indent (elm-current-column)))
      (funcall (car phrase)))
    (cond
     ((eq current-token 'end-tokens)
      (cond ((null (cdr phrase))) ;; fallthrough
            ((equal following-token (cadr phrase))
             (elm-indentation-add-indentation starter-indent)
             (throw 'parse-end nil))
            ((string= (cadr phrase) "in")
             (when (= left-indent layout-indent)
               (elm-indentation-add-layout-indent)
               (throw 'parse-end nil)))
            (t (throw 'parse-end nil))))

     ((null (cdr phrase)))

     ((equal (cadr phrase) current-token)
      (let* ((on-new-line (= (elm-current-column) (elm-indentation-current-indentation)))
             (lines-between (- parse-line-number starter-line))
             (left-indent (if (<= lines-between 0)
                              left-indent
                            starter-indent)))
        (elm-indentation-read-next-token)
        (when (eq current-token 'end-tokens)
          (elm-indentation-add-indentation
           (cond ((member (cadr phrase) '("then" "else"))
                  (+ starter-indent elm-indentation-ifte-offset))
                 ((member (cadr phrase) '("in" "->"))
                  ;; expression ending in another expression
                  (if on-new-line
                      (+ left-indent elm-indentation-starter-offset)
                    left-indent))
                 (t (+ left-indent elm-indentation-left-offset))))
          (throw 'parse-end nil))
        (elm-indentation-phrase-rest (cddr phrase))))

     ((string= (cadr phrase) "in")) ;; fallthrough
     (t (parse-error "Expecting %s" (cadr phrase))))))

(defun elm-indentation-add-indentation (indent)
  (elm-indentation-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent elm-indentation-layout-offset)
     indent)))

(defun elm-indentation-add-layout-indent ()
  (elm-indentation-push-indentation layout-indent))

(defun elm-indentation-add-where-pre-indent ()
  (elm-indentation-push-indentation
   (+ layout-indent elm-indentation-where-pre-offset))
  (if (= layout-indent elm-indentation-layout-offset)
      (elm-indentation-push-indentation
       elm-indentation-where-pre-offset)))

(defun elm-indentation-add-where-post-indent (indent)
  (elm-indentation-push-indentation
   (+ indent elm-indentation-where-post-offset)))

(defun elm-indentation-push-indentation (indent)
  (when (or (null possible-indentations)
            (< indent (car possible-indentations)))
    (setq possible-indentations
          (cons indent possible-indentations))))

(defun elm-indentation-token-test ()
  (let ((current-token nil)
        (following-token nil)
        (layout-indent 0)
        (parse-line-number 0)
        (indentation-point (mark)))
    (elm-indentation-read-next-token)))

(defun elm-indentation-read-next-token ()
  (cond ((eq current-token 'end-tokens)
         'end-tokens)
        ((eq current-token 'layout-end)
         (cond ((> layout-indent (elm-current-column))
                'layout-end)
               ((= layout-indent (elm-current-column))
                (setq current-token 'layout-next))
               ((< layout-indent (elm-current-column))
                (setq current-token (elm-indentation-peek-token)))))
        ((eq current-token 'layout-next)
         (setq current-token (elm-indentation-peek-token)))
        ((> layout-indent (elm-current-column))
         (setq current-token 'layout-end))
        (t
         (elm-indentation-skip-token)
         (if (>= (point) indentation-point)
             (progn
               (setq following-token
                     (if (= (point) indentation-point)
                         (elm-indentation-peek-token)
                       'no-following-token))
               (setq current-token 'end-tokens))
           (when (= (elm-current-column) (elm-indentation-current-indentation))
             ;; on a new line
             (setq current-indent (elm-current-column))
             (setq left-indent (elm-current-column))
             (setq parse-line-number (+ parse-line-number 1)))
           (cond ((> layout-indent (elm-current-column))
                  (setq current-token 'layout-end))
                 ((= layout-indent (elm-current-column))
                  (setq current-token 'layout-next))
                 (t (setq current-token (elm-indentation-peek-token))))))))

(defun elm-indentation-peek-token ()
  "Return token starting at point."
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|mdo\\|rec\\|do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alnum:]'_]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "[][(){}[,;]")
         (match-string-no-properties 0))
        ((looking-at "\\(\\\\\\|->\\|<-\\|::\\|=\\||\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "\\(→\\|←\\|∷\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (let ((tok (match-string-no-properties 1)))
           (or (cdr (assoc tok elm-indentation-unicode-tokens)) tok)))
        ((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
         'operator)
        (t 'value)))

(defun elm-indentation-skip-token ()
  "Skip to the next token."
  (let ((case-fold-search nil))

    (if (or (looking-at "'\\([^\\']\\|\\\\.\\)*'")
            (looking-at "\"\\([^\\\"]\\|\\\\.\\)*\"")
            (looking-at         ; Hierarchical names always start with uppercase
             "[[:upper:]]\\(\\sw\\|'\\)*\\(\\.\\(\\sw\\|'\\)+\\)*")
            (looking-at "\\sw\\(\\sw\\|'\\)*") ; Only unqualified vars can start with lowercase
            (looking-at "[0-9][0-9oOxXeE+-]*")
            (looking-at "[-:!#$%&*+./<=>?@\\\\^|~]+")
            (looking-at "[](){}[,;]")
            (looking-at "`[[:alnum:]']*`"))
        (goto-char (match-end 0))
      ;; otherwise skip until space found
      (skip-syntax-forward "^-"))
    (forward-comment (buffer-size))
    (while (and (eq elm-literate 'bird)
                (bolp)
                (eq (char-after) ?>))
      (forward-char)
      (forward-comment (buffer-size)))))

(provide 'elm-indentation)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; tab-width: 8
;; End:

;;; elm-indentation.el ends here
