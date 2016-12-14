;;; codebrowser.el --- make Cosmonaut bootstrap and early boot  -*- lexical-binding: t -*-

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

;; TODO

;;; Code:

;;;
;;; ecb
;;;
(when (not cosmonaut/code-browser-switch-to-simple)

  (require 'ecb)

  (defadvice ecb-activate (after ecb-activate-after activate)
    "Redraw layout after activation of ecb."
    (ecb-redraw-layout))

  ;;;; defining standard layouts
  (ecb-layout-define "cosmonaut-3-1" left-right
		     "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Methods     |                               | Directories |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |             |
   |              |                               |             |
   |  Sources     |                               |             |
   |              |                               |             |
   |--------------|                               |             |
   |              |                               |             |
   |  History     |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
		     (ecb-set-methods-buffer)
		     (ecb-split-ver 0.4)
		     (ecb-set-sources-buffer)
		     (ecb-split-ver 0.5)
		     (ecb-set-history-buffer)
		     (select-window (next-window (next-window)))
		     (ecb-set-directories-buffer)
		     (select-window (previous-window (selected-window) 0)))

  (ecb-layout-define "cosmonaut-2-2" left-right
		     "This function creates the following layout:

   --------------------------------------------------------------
   |              |                               |             |
   |  Methods     |                               | Directories |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |--------------|             Edit              |-------------|
   |              |                               |             |
   |  History     |                               |  Sources    |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   |              |                               |             |
   --------------------------------------------------------------
   |                                                            |
   |                    Compilation                             |
   |                                                            |
   --------------------------------------------------------------

If you have not set a compilation-window in `ecb-compile-window-height' then
the layout contains no persistent compilation window and the other windows get a
little more place."
		     (ecb-set-methods-buffer)
		     (ecb-split-ver 0.5)
		     (ecb-set-history-buffer)
		     (select-window (next-window (next-window)))
		     (ecb-set-directories-buffer)
		     (ecb-split-ver 0.66)
		     (ecb-set-sources-buffer)
		     (select-window (previous-window (previous-window (selected-window) 0) 0)))

  (custom-set-variables
   '(ecb-tip-of-the-day nil)
   '(ecb-options-version "2.40")
   '(ecb-layout-name "cosmonaut-2-2")
   '(ecb-windows-width 0.2)
   '(ecb-tip-of-the-day nil)
   '(ecb-compile-window-height 0.15)
   '(ecb-compile-window-width 'edit-window)
   '(ecb-compile-window-temporally-enlarge 'after-selection) ;; after-display after-selection both nil
   '(ecb-enlarged-compilation-window-max-height 0.6) ;; best, half, number
   '(ecb-create-layout-file (locate-user-config-file "ecb-user-layouts.el"))
   '(ecb-tip-of-the-day-file (locate-user-config-file "ecb-tip-of-day.el"))
   '(ecb-source-path (quote (("/" "Root"))))

   ;; ecb-windows-width 30
   ;; ecb-fix-window-size 'width
   ;; ecb-history-make-buckets 'mode
   ;; ecb-kill-buffer-clears-history 'auto
   ;; ecb-create-layout-frame-height 40
   ;; ecb-create-layout-frame-width 110
   '(ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
   ;; '(semantic-decoration-styles (list '("semantic-decoration-on-includes" . t)
   ;; 				      '("semantic-tag-boundary" . t))
   ;; 				;; ecb-create-layout-frame-height 40
   ;; 				;; ecb-create-layout-frame-width 110
   ;; 				))

   ;; (add-hook 'ecb-show-ecb-windows-before-hook
   ;;           'ecb-enlarge-frame-width-before-show)
   ;; (add-hook 'ecb-hide-ecb-windows-before-hook
   ;;           'ecb-shrink-frame-width-before-hide)
   ;; (add-hook 'ecb-deactivate-hook
   ;;           'ecb-shrink-frame-width-before-hide)
   ;; (add-hook 'ecb-activate-before-layout-draw-hook
   ;;           'ecb-enlarge-frame-width-before-activate)

   ;; (defun ecb-shrink-frame-width-before-hide ()
   ;;   "Shrink frame width before ecb hide layout."
   ;;   (if (and (not (ecb-windows-all-hidden))

   ;;            (not (eq (frame-pixel-width)
   ;;                     (display-pixel-width))))
   ;;       (if (< (- (frame-width) (+ ecb-windows-width 2)) initial-frame-width)
   ;;           (set-frame-width (selected-frame) initial-frame-width)
   ;;         (set-frame-width (selected-frame) (- (frame-width) (+ ecb-windows-width 2))))))
   ;; (defun ecb-enlarge-frame-width-before-activate ()
   ;;   "Enlarge frame width when ecb active and need it to."
   ;;   (let ((use-last-win-conf (and ecb-last-window-config-before-deactivation
   ;;                                 (equal ecb-split-edit-window-after-start
   ;;                                        'before-deactivation)
   ;;                                 (not (ecb-window-configuration-invalidp
   ;;                                       ecb-last-window-config-before-deactivation)))))
   ;;     (unless (or (and use-last-win-conf
   ;;                      (eq (nth 5 ecb-last-window-config-before-deactivation)
   ;;                          ecb-windows-hidden-all-value))
   ;;                 (> (+ (frame-pixel-width) (* (frame-char-width)
   ;;                                              (+ ecb-windows-width 2)))
   ;;                    (display-pixel-width)))
   ;;       (set-frame-width (selected-frame) (+ (frame-width) (+ ecb-windows-width 2))))))
   )

  (global-set-key (kbd "S-<f12>") 'ecb-toggle-compile-window)
  )

;; (setq ecb-create-layout-file (locate-user-config-file "ecb-user-layouts.el"))

(when (not cosmonaut/code-browser-switch-to-simple)
  (custom-set-variables
   '(ecb-layout-name "cosmonaut-2-2")))

(when cosmonaut/code-browser-switch-to-simple
  (require 'sr-speedbar)
  (custom-set-variables
   '(speedbar-use-images t)
   '(sr-speedbar-right-side nil)
   '(speedbar-show-unknown-files t))
  )

(defun toggle-code-browser ()
  (interactive)
  (if cosmonaut/code-browser-switch-to-simple
      (sr-speedbar-toggle)
    (or (ecb-toggle-ecb-windows)
	(call-interactively 'ecb-activate))))

(global-set-key (kbd "<f11>") 'toggle-code-browser)


(add-hook 'emacs-startup-hook '(lambda ()
				 (when (not noninteractive)
				   (if cosmonaut/code-browser-switch-to-simple
				       (sr-speedbar-open)
				     (call-interactively 'ecb-activate)))))

;;;;; config-ecb.el ends here ---
