;;; elm-map.el --- Elm-mode keyboard mappings.

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

(require 'elm-compile)
(require 'elm-interactive)
(require 'elm-preview)

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
    (define-key map "\C-c\C-l" 'load-elm-repl)
    (define-key map "\C-c\C-p" 'push-elm-repl)
    (define-key map "\C-c\C-c" 'elm-compile-buffer)
    (define-key map "\C-c\C-n" 'elm-preview-buffer)
    map)
  "Keymap for Elm major mode.")

(define-key elm-mode-map [menu-bar] (make-sparse-keymap))

(let ((menu-map (make-sparse-keymap "Elm")))
    (define-key elm-mode-map [menu-bar elm] (cons "Elm" menu-map))
    (define-key menu-map [elm-repl]
      '("elm-repl: Load Buffer" . load-elm-repl))
    (define-key menu-map [elm-push]
      '("elm-repl: Push Region" . push-elm-repl))
    (define-key menu-map [elm-compile]
      '("Compile Buffer" . elm-compile-buffer))
    (define-key menu-map [elm-preview]
      '("Preview Buffer" . elm-preview-buffer)))

(provide 'elm-map)
;;; elm-map.el ends here
