;;; popup-menu.el --- context menu for restautant -*- lexical-binding: t -*-

;; Copyright (C) 2016 Alexander aka 'CosmonauT' Vynnyk

;; Maintainer: cosmonaut.ok@zoho.com
;; Keywords: internal
;; Package: cosmonaut

;; This file is part of Cosmonaut.

;; Cosmonaut is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Cosmonaut is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Cosmonaut.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO:

;;; Code:

(defun choose-from-menu (menu-title menu-items)
  "Choose from a list of choices from a popup menu.
See `popup-commands' which calls this"
  (let ((item)
	(item-list))
    (while menu-items
      (setq item (car menu-items))
      (if (consp item)
	  (setq item-list (cons (cons (car item) (cdr item) ) item-list))
	(setq item-list (cons (cons item item) item-list)))
      (setq menu-items (cdr menu-items))
      )

    (x-popup-menu t (list menu-title (cons menu-title (nreverse item-list))))))

(defun right-popup ()
  "Show a popup menu of commands. See also `choose-from-menu'."
  (interactive)
  (eval-expression
   (car
    (read-from-string
     (choose-from-menu
      "Appetizer"
      (list
       (cons "Cut [C-w]" "(call-interactively 'kill-region)")
       (cons "Copy [M-w]" "(call-interactively 'kill-ring-save)")
       (cons "Paste (Yank) [C-y]" "(yank)")
       (cons "Paste from history" "(popup-menu 'yank-menu)")
       (cons "-" "")
       (cons "Open Terminal here" "(open-console)")
       (cons "-" "")
       (cons "Undo [C-x u]" "(undo)")
       (cons "Redo" "(call-interactively 'redo)")
       (cons "-" "")
       (cons "Refactor N/I" "")
       (cons "Folding N/I" "")
       (cons "-" "")
       (cons "Save Current File [C-x C-s]" "(call-interactively 'save-buffer)")
       (cons "-" "")
       (cons "Search [C-s]" "(call-interactively 'search-forward)")
       (cons "Search [C-s-1]" (cons "zzz" "(call-interactively 'search-forward)"))
       (cons "-" "")
       (cons "Open buffer in new frame (WM window) [C-x 5 2]"
	     "(make-frame-command)")
       (cons "Hide buffer [C-x 0]"  "(delete-window)")
       (cons "-" "")
       (cons "-" "")
       (cons "Close buffer [C-x k]"  "(kill-buffer)")
       ))))))


;; (x-popup-menu
;;  t
;;  '("Title"
;;    ("Title 1" ("Item 1-1" . 11) ("Item 1-2" . 12))
;;    ("Title 2" ("Item 2-1" . 21) ("Item 2-2" . 22))))

(global-set-key [mouse-3] 'right-popup)

;;;; popup-menu.el ends here
