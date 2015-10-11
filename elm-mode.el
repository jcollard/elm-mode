;;; elm-mode.el --- Major mode for Elm

;; Copyright (C) 2013, 2014  Joseph Collard
;; Copyright (C) 2015  Bogdan Popa

;; Author: Joseph Collard
;; Package-Requires: ((f "0.17") (let-alist "1.0.4") (s "1.7.0") (emacs "24"))
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
(require 'elm-interactive)
(require 'elm-font-lock)

(defun elm-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
ARG specifies the number of lines to comment or uncomment."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "--")
        (comment-end ""))
    (comment-dwim arg)))

(defvar elm-mode-map
  (let ((map (make-keymap)))
    (define-key map [remap comment-dwim] 'elm-comment-dwim)
    (define-key map "\C-c\C-l" 'elm-repl-load)
    (define-key map "\C-c\C-p" 'elm-repl-push)
    (define-key map "\C-c\C-e" 'elm-repl-push-decl)
    (define-key map "\C-c\C-c" 'elm-compile-buffer)
    (define-key map "\C-c\M-c" 'elm-compile-main)
    (define-key map "\C-c\M-k" 'elm-package-catalog)
    (define-key map "\C-c\C-n" 'elm-preview-buffer)
    (define-key map "\C-c\C-m" 'elm-preview-main)
    (define-key map "\C-c\C-d" 'elm-documentation-lookup)
    (define-key map "\C-c\C-i" 'elm-import)
    (define-key map "\C-c\C-t" 'elm-oracle-type-at-point)
    map)
  "Keymap for Elm major mode.")

;;;###autoload
(define-derived-mode elm-mode prog-mode "Elm"
  "Major mode for editing Elm source code."
  (setq-default indent-tabs-mode nil)

  ;; Elm is not generally suitable for electric indentation, since
  ;; there is no unambiguously correct indent level for any given
  ;; line.
  (when (boundp 'electric-indent-inhibit)
    (setq-local electric-indent-inhibit t))

  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")

  (turn-on-elm-font-lock)
  (turn-on-elm-indent))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))

(provide 'elm-mode)
;;; elm-mode.el ends here
