;;; elm-defuns.el --- Find start/end of elm defuns  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Joseph Collard
;; Copyright (C) 2015, 2016  Bogdan Popa
;; Copyright (C) 2023  Steve Purcell

;; Author: Steve Purcell

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

(defun elm-beginning-of-defun (&optional arg)
  "Move backward to the beginning of an ELM \"defun\".
With ARG, do it that many times.  Negative arg -N means move
forward to Nth following beginning of defun.  Returns t unless
search stops due to beginning or end of buffer.

Find the roots of this function in the c-awk-mode."
  (interactive "p")
  (or arg (setq arg 1))
  (save-match-data
    (let ((found t) ; Has the most recent regexp search found b-of-defun?
          (regexp "^[^- \t\n\r]"))
      (if (>= arg 0)
          ;; Go back one defun each time round the following loop. (For +ve arg)
          (while (and found (< 0 arg) (not (eq (point) (point-min))))
            ;; Go back one "candidate" each time round the next loop until one
            ;; is genuinely a beginning-of-defun.

            (setq found (search-backward-regexp
                         regexp (point-min) 'stop-at-limit))
            (when found
              (while (and (forward-line -1)
                          (looking-at regexp)
                          (not (= (point-min) (point)))))
              (forward-line))
            (setq arg (1- arg)))
        ;; The same for a -ve arg.
        (if (not (eq (point) (point-max))) (forward-char 1))
        (while (and found (< arg 0) (not (eq (point) (point-max)))) ; The same for -ve arg.
          (setq found (search-forward-regexp
                       regexp (point-min) 'stop-at-limit))
          (setq arg (1+ arg))))
      (eq arg 0))))

(defun elm-end-of-defun (&optional arg)
  "Move forward to the end of an ELM \"defun\".
With ARG, do it that many times.  Negative arg -N means move
forward to Nth previous end of defun.  Returns t unless
search stops due to beginning or end of buffer.

Find the roots of this function in the c-awk-mode."
  (interactive "p")
  (or arg (setq arg 1))
  (save-match-data
    (let ((found t) ; Has the most recent regexp search found b-of-defun?
          (regexp "^\n\n"))
      (if (>= arg 0)
          ;; Go back one defun each time round the following loop. (For +ve arg)
          (while (and found (< 0 arg) (not (eq (point) (point-max))))
            ;; Go back one "candidate" each time round the next loop until one
            ;; is genuinely a beginning-of-defun.
            (setq found (search-forward-regexp
                         regexp
                         (point-max) 'stop-at-limit))
            (setq arg (1- arg)))
        ;; The same for a -ve arg.
        (if (not (eq (point) (point-min))) (forward-char 1))
        (while (and found (< arg 0) (not (eq (point) (point-min)))) ; The same for -ve arg.
          (setq found (search-backward-regexp
                       regexp (point-min) 'stop-at-limit))
          (setq arg (1+ arg)))
        (if found (goto-char (match-beginning 0))))
      (eq arg 0))))


(provide 'elm-defuns)
;;; elm-defuns.el ends here
