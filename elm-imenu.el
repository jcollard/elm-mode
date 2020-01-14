;;; elm-imenu.el --- imenu support for elm
;;; Commentary:
;;; Code:
(require 'imenu)

(defcustom elm-imenu-use-categories t
  "Group imenu entries by their type, e.g. functions, type aliases."
  :group 'elm
  :type 'boolean
  :safe 'booleanp)

(defun elm-imenu-create-index ()
  "Create an imenu index for the current buffer."
  (save-excursion
    (imenu--generic-function
     `((,(and elm-imenu-use-categories "Type") "^type \\([A-Z][^ \n]+\\)" 1)
       (,(and elm-imenu-use-categories "Type Alias") "^type alias \\([A-Z][^ \n]+\\)" 1)
       (,(and elm-imenu-use-categories "Port") "^port \\([^ ]+\\)" 1)
       (,(and elm-imenu-use-categories "Function") "^\\([^ ]+\\) :" 1)))))

(provide 'elm-imenu)
;;; elm-imenu.el ends here
