;;; elm-mode.el --- Major mode for Elm

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

(require 'elm-indent)
(require 'elm-indentation)
(require 'elm-font-lock)
(require 'elm-repl)
(require 'elm-map)
(require 'elm-compile)
(require 'elm-preview)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))


(defalias 'elm-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode elm-mode elm-parent-mode "Elm"
  "Major mode for editing Elm source code.

\\{elm-mode-map}"
  (setq indent-tabs-mode nil)

  ;; Set single line comments
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (elm-indent-mode)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (turn-on-elm-font-lock))

(provide 'elm-mode)

;;; elm-mode.el ends here
