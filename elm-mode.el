;;; elm-mode.el --- Major mode for Elm

;; Copyright (C) 2013, 2014  Joseph Collard
;; Copyright (C) 2015  Bogdan Popa

;; Author: Joseph Collard
;; Package-Requires: ((f "0.17") (s "20140910.334") (emacs "24"))
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
(require 'elm-reactor)
(require 'elm-font-lock)
(require 'elm-map)
(require 'elm-compile)

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
