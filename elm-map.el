;;; elm-map.el ---

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

(require 'elm-repl)
(require 'elm-compile)
(require 'elm-preview)

(defun elm-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
        (comment-start "--") (comment-end "")
        )
    (comment-dwim arg)))

;; Elm mode keyboard short cuts
(defvar elm-mode-map
  (let ((map (make-keymap)))
    (define-key map [remap comment-dwim] 'elm-comment-dwim)
    (define-key map "\C-c\C-l" 'load-elm-repl)
    (define-key map "\C-c\C-p" 'push-elm-repl)
    (define-key map "\C-c\C-c" 'elm-compile-buffer)
    (define-key map "\C-c\C-n" 'elm-preview-buffer)
    map)
  "Keymap for Elm major mode")

;; Elm Menu
(define-key elm-mode-map [menu-bar] (make-sparse-keymap))

(let ((menuMap (make-sparse-keymap "Elm")))
    (define-key elm-mode-map [menu-bar elm] (cons "Elm" menuMap))
    (define-key menuMap [elm-repl]
      '("elm-repl: Load Buffer" . load-elm-repl))
    (define-key menuMap [elm-push]
      '("elm-repl: Push Region" . push-elm-repl))
    (define-key menuMap [elm-compile]
      '("Compile Buffer" . elm-compile-buffer))
    (define-key menuMap [elm-preview]
      '("Preview Buffer" . elm-preview-buffer)))

(provide 'elm-map)

;;; elm-map.el ends here
