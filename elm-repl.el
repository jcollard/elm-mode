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

(require 'comint)

(defconst elm-repl-buffer
  "*elm-repl*")

(defun run-elm-repl ()
  (interactive)
  (letrec 
      
      ;; The *elm-repl* buffer
      ((buffer (get-buffer-create elm-repl-buffer))
       ;; The window that should hold elm-repl
       (target_window (get-buffer-window buffer))
       ;; The current window
       (selected_window (selected-window)))
    
    ;; If the target window does not exist, create it and set the buffer
    ;; then select that window so elm-repl will be running there
    (if target_window      
	(select-window target_window)
      (let ((split_window (split-window)))
	(set-window-buffer split_window buffer)
	(select-window split_window)))
    
    ;; Start elm-repl if it is not already running
    (comint-run "elm-repl")
    
    ;; Switch focus back to the originally selected window
    (select-window selected_window)))


(provide 'elm-repl)
