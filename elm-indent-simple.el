;;; elm-indent-simple.el --- "stupid" indentation module for Elm Mode  -*- lexical-binding: t; -*-

;; Copyright 2021  Theodor Thornhill

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

(require 'cl-lib)

;;; Indentation

(defcustom elm-indent-simple-offset 4
  "Indent Elm code by this number of spaces."
  :type 'integer
  :group 'elm
  :safe #'integerp)

(defun elm-indent-simple--find-indentation-of-list ()
  (save-excursion
    (backward-up-list 1)
    (+ (- (current-column) (current-indentation))
       (current-indentation))))

(defun elm-indent-simple--find-indentation-of-tokens (tokens)
  (save-excursion
    (re-search-backward (regexp-opt tokens) (point-min) t nil)
    (+ (- (current-column) (current-indentation))
       (current-indentation))))

(defun elm-indent-simple--two-lines-same-token-p (token)
  "Checks if line and previous line start with same token."
  (and (looking-at token)
       (save-excursion
         (forward-line -1)
         (back-to-indentation)
         (looking-at token))))

(defun elm-indent-simple--previous-line-ends-with (tokens)
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (forward-comment (- (point)))
    (looking-back (regexp-opt tokens) nil)))

(defun elm-indent-simple--previous-line-starts-with (tokens)
  (save-excursion
    (forward-line -1)
    (back-to-indentation)
    (looking-at-p (regexp-opt tokens))))

(defun elm-indent-simple-compute-indentation ()
  "Return a column to indent to.

The numbers we get are the positions we can determine from the
given context. When we cannot find a context to indent to, we
default to the indentation level of previous line."
  (let* ((indent-level-previous-line
          (save-excursion
            (forward-line -1)
            (current-indentation)))
         (positive-offset (+ indent-level-previous-line elm-indent-simple-offset)))
    (save-excursion
      (back-to-indentation)
      ;; Now we are positioned at start of indentation.
      ;; Logic below assumes this is true.
      (cond
       ((elm-indent-simple--previous-line-ends-with '("=" "<-" "[" "{" "of" "if" "else" ":" "->" "exposing")) positive-offset)
       ((looking-at-p (regexp-opt '("{-" "-}"))) 0)
       ((and (= indent-level-previous-line 0) (looking-at-p "=")) positive-offset)
       ((elm-indent-simple--previous-line-starts-with '("type" "let")) positive-offset)
       ((looking-at-p ")") (elm-indent-simple--find-indentation-of-list))
       ((looking-at-p "}") (elm-indent-simple--find-indentation-of-list))
       ((looking-at-p "]") (elm-indent-simple--find-indentation-of-list))
       ((looking-at-p ",") (elm-indent-simple--find-indentation-of-list))
       ((looking-at-p "else") (elm-indent-simple--find-indentation-of-tokens '("if")))
       ((looking-at-p "then") (elm-indent-simple--find-indentation-of-tokens '("if")))
       (t (elm-indent-simple-lastly))))))

(defun elm-indent-simple-level-2-previous-lines ()
  "Returns indent level of the two previous lines."
  (save-excursion
    ;; Since we save-excursion above, we can go back two lines one by one, and
    ;; return the lines separately.
    (cl-values
     (progn
       (forward-line -1)
       (current-indentation))
     (progn
       (forward-line -1)
       (current-indentation)))))

(defun elm-indent-simple-lastly ()
  (interactive)
  (cl-multiple-value-bind (previous-line 2nd-previous-line)
      (elm-indent-simple-level-2-previous-lines)
    (cond
     ((and (zerop previous-line) (zerop 2nd-previous-line)) 0)
     ((zerop previous-line) 2nd-previous-line)
     (t
      previous-line))))

(defun elm-indent-simple-do-indent (indent)
  (if (<= (current-column) (current-indentation))
      (ignore-errors (indent-line-to indent))
    (save-excursion (ignore-errors (indent-line-to indent)))))

(defun elm-indent-simple-indent-line ()
  "Set indent levels for Elm source code.

Try to indent to the correct level.  If indent level is
ambiguous, multiple invocations will indent tabstops forward."
  (interactive)
  (elm-indent-simple-do-indent
   (elm-indent-simple-compute-indentation)))

(defun elm-indent-simple-indent-region (start end)
  "Apply `elm-indent-simple-indent-line' to every line between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (elm-indent-simple-indent-line)
      (forward-line))))

(defun elm-indent-simple-indent-forward (&optional arg)
  "Indent line to the next tabstop."
  (interactive "p")
  (elm-indent-simple-do-indent
   (indent-next-tab-stop (* (or arg 1) (current-indentation)))))

(defun elm-indent-simple-indent-backward (&optional _arg)
  "Indent backwards to the nearest tabstop."
  (interactive "p")
  (elm-indent-simple-do-indent
   (indent-next-tab-stop
    (save-excursion (back-to-indentation) (current-column)) t)))


(defvar elm-indent-simple-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c TAB") 'elm-indent-simple-indent-line)
    (define-key map (kbd "<tab>") 'elm-indent-simple-indent-forward)
    (define-key map (kbd "<backtab>") 'elm-indent-simple-indent-backward)
    map))

;;;###autoload
(define-minor-mode elm-indent-simple-mode
  "\"Stupid\" Elm indentation mode."
  :global nil
  :lighter " EIndent-simple"
  :map 'elm-indent-simple-mode-map
  
  (if elm-indent-simple-mode
      (progn
        (setq-local tab-width elm-indent-offset)
        (setq-local indent-line-function #'elm-indent-simple-indent-line)
        (setq-local indent-region-function nil))
    (kill-local-variable 'tab-width)
    (kill-local-variable 'indent-line-function)))

(provide 'elm-indent-simple)
;;; elm-indent-simple.el ends here
