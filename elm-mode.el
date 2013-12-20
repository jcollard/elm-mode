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

;; Elm mode hook for user defined functionality
(defvar elm-mode-hook nil)

;; Elm mode keyboard short cuts
(defvar elm-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Elm major mode")

(require 'elm-indent)
(require 'elm-indentation)
(require 'elm-font-lock)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))

;; Are we looking at a literate script?
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

(defun elm-mode ()
  "Major mode for editing Workflow Process Description Language files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map elm-mode-map)
  (elm-indent-mode)
  (turn-on-elm-font-lock)
  (setq major-mode 'elm-mode)
  (setq mode-name "Elm")
  (run-hooks 'elm-mode-hook))

(provide 'elm-mode)
