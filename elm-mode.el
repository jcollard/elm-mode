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

(require 'elm-indent)
(require 'elm-indentation)
(require 'elm-font-lock)
(require 'elm-repl)
(require 'elm-map)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))


(defun elm-mode ()
  "Major mode for editing Elm source code"
  (interactive)
  (setq indent-tabs-mode nil)
  (kill-all-local-variables)
  (use-local-map elm-mode-map)
  (elm-indent-mode)
  ;; TODO
  ;; This line makes tabs use spaces which is what
  ;; we need for elm. However, I think it overrides behavior
  ;; outside of elm-mode
  (setq-default indent-tabs-mode nil)
  (turn-on-elm-font-lock)
  (setq major-mode 'elm-mode)
  (setq mode-name "Elm")
  (run-hooks 'elm-mode-hook))

(provide 'elm-mode)
