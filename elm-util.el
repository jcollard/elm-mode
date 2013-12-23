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

;; Provides useful utility functions

;; If splitting right would not half the width of the current
;; buffer, splits right. Otherwise, splits below
(defun intelligent-split-window ()
  (let ((width (window-total-width))
	(height (window-total-height)))
    (if (> (/ width 2) height)
	(split-window-right)
        (split-window-below))))

(provide 'elm-util)
