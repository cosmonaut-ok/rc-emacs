;;; cosmonaut-common.el --- common cosmonaut features  -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'user-directories)
(require 'redo+)
(require 'f)
(require 'notify)
(require 'ansi-color)
;; (require 'github-notifier)
;; (require 'popup)
(require 'ssh)

;;;
;;; Global config
;;;

(defun cosmonaut/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;;;; Global default configuration parameters

(custom-set-variables
 '(yes-or-no-p 'y-or-n-p) ;replace y-e-s by y
 '(inhibit-startup-message t) ;no splash screen
 ;; backup
 '(make-backup-files cosmonaut/autobackup)
 '(use-backup-dir cosmonaut/autobackup) ;use backup directory
 '(version-control t) ;; Enable versioning with default values (keep five last versions, I think!)
 '(backup-by-copying t)
 '(delete-old-versions t)
 '(backup-directory-alist
   `((".*" . ,cosmonaut/backup-directory))) ; don't litter my fs tree
 '(auto-save-file-name-transforms
   `((".*" ,cosmonaut/backup-directory t)))
 `(auto-save-interval ,cosmonaut/autobackup-interval)
 ;; other
 '(query-replace-highlight t)           ;highlight during query
 '(search-highlight t)                  ;highlight incremental search
 '(ls-lisp-dirs-first t)                ;display dirs first in dired
 '(initial-frame-alist (quote ((fullscreen . maximized)))) ; start maximized
 )

;;;; visual mode
(global-visual-line-mode t) ;; wrap long lines visually to several lines
;; Remove visual line from buffer
(add-hook 'minibuffer-setup-hook '(lambda ()
                                   (visual-line-mode -1)))

;;;; enable cua mode
(defhooklet cosmonaut/cua-mode emacs-startup cosmonaut/familiar-copy-paste-cut
  ;; https://www.emacswiki.org/emacs/CuaMode
  (cua-mode t))

(defhooklet cosmonaut/cua-mode test-mode cosmonaut/familiar-copy-paste-cut
  (cua-mode t))

(defhooklet cosmonaut/cua-mode prog-mode cosmonaut/familiar-copy-paste-cut
  (cua-mode t))

;;;; Sync linux and eamcs clipboards
;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)

;;;; Move lines/regions up/down
(require 'drag-stuff)
(drag-stuff-global-mode 1)
;; Drag line
;; To drag a line up and down. Put the cursor on that line and press <M-up> and <M-down>.
;; Drag lines
;; To drag several lines up and down. Select the lines you want to drag and press <M-up> and <M-down>.
;; Drag region
;; A region can be dragged to the left and right. Select the region you want to drag and press <M-left> and <M-right>.
;; Drag word
;; To drag a word. Place the cursor on the word and press <M-left> and <M-right>.
;; For more information, see comments in drag-stuff.el.

;;;; Recently edited files in menu
(recentf-mode 1)

;;;; Dired customizations
(setq dired-listing-switches "-Al")

;;;; Word completion customizations
(setq dabbrev-always-check-other-buffers t)
;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

;; move semanticDB, srecode and ede to cache
(mkdir cosmonaut/user-cache-directory t)
(custom-set-variables
 '(ede-project-placeholder-cache-file (locate-user-cache-file "cosmonaut-ede-projects.el"))
 '(semanticdb-default-save-directory (locate-user-cache-file "cosmonaut-semanticdb"))
 '(srecode-map-save-file (locate-user-cache-file "cosmonaut-srecode-map.el")))

;; set semantic/imenu buffer autorescan
(custom-set-variables
 '(imenu-auto-rescan t))
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

;;;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)

;;;; Tabbar mode
(when cosmonaut/enable-tabbar
  (tabbar-mode 1)
  (setq tabbar-use-images nil)
  (global-set-key [S-left] 'tabbar-backward-tab)
  (global-set-key [S-right] 'tabbar-forward-tab))


;;;; Set visible bell
(setq visible-bell t)

;; ;; set frame (window in window-managers terms) size
;; (when window-system (set-frame-size (selected-frame) 190 96))

;;;; Set time format to display
(load-library "time")
(setq display-time-24hr-format t
      display-time-mail-file t
      display-time-form-list (list 'time 'load)
      display-time-day-and-date t)
(display-time)

;;;; ido
;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-ido.el
(require 'ido)
(require 'ido-vertical-mode)
(require 'ido-completing-read+)
;;
(ido-mode 1)
(ido-vertical-mode 1)
(ido-better-flex/enable)

(custom-set-variables
 '(ido-vertical-show-count t)
 '(ido-enable-flex-matching t)
 '(ido-vertical-define-keys 'C-n-C-p-up-and-down)
 )

(global-set-key "\M-x" (lambda ()
			 (interactive)
			 (call-interactively (intern (ido-completing-read
						      "M-x " (all-completions "" obarray 'commandp))))))

;; set ido-completing-read as default completing function
(setq ido-cr+-replace-completely t)
(setq-default completing-read-function 'ido-completing-read)

;;;; If we read a compressed file, uncompress it on the fly:
;;;; (this works with .tar.gz and .tgz file as well)
(custom-set-variables
 '(auto-compression-mode 1))

;;;; fill-column
(defun cosmonaut/fill-column ()
  (when cosmonaut/fill-column
    (setq-default fill-column cosmonaut/max-line-length)))

(add-hook 'prog-mode-hook 'cosmonaut/fill-column)

;;;; Uniquify buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;;
;;; Prog mode configuration
;;;

(defvar cosmonaut-max-line-length (concat "^[^\n]\\{"
					   (number-to-string cosmonaut/max-line-length)
					   "\\}\\(.*\\)$"))

;;;
;;; activate generic options
;;;
(require 'magit)
(require 'magit-gh-pulls)

(defhooklet cosmonaut/generic-prog-mode-improvements prog-mode t
  ;; highlight FIXME/TODO/BUG keywords
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t)))
  ;; auto-fill mode
  (auto-fill-mode -1)
  ;; projectile
  (require 'projectile)
  ;; magit
  (setq magit-completing-read-function 'magit-ido-completing-read)
  ;;
  (subword-mode 1)
  (font-lock-mode 1)
  ;; Drag and move selcted
  (drag-stuff-mode 1)
  ;; set global indent-tabs-mode
  (setq indent-tabs-mode cosmonaut/indent-tabs-mode)
  
  (local-set-key (kbd "C-c C-f") 'flash-cross)
  (local-set-key (kbd "RET") 'newline-and-indent)

  (message "Prog mode enabled. USE Shift+SPACE to show or hide blocks"))

;; Remove trailing whitespaces
(defhooklet cosmonaut/delete-trailing-whitespaces before-save t
  (delete-trailing-whitespace 1))

;;
(when (and cosmonaut/check-parens-before-save buffer-file-name)
  (add-hook 'before-save-hook
	    'check-parens
	    nil t))

;; magit-gh-pulls
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;;;
;;; indicate paren
;;;
(defhooklet cosmonaut/parenth-management prog-mode cosmonaut/indicate-parentheses
  (require 'paren)
  (require 'highlight-parentheses)
  ;; show pait to current parenth
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  ;; indicate other parentheses
  (highlight-parentheses-mode 1)
  ;; 'parenthesis of 'expression highlight just parens/highlight entire expression
  (setq show-paren-style 'parenthesis))

;;;
;;; linum
;;;
(defhooklet cosmonaut/linum prog-mode cosmonaut/linum-mode
    (require 'linum)
    (linum-mode 1))

;;;
;;; highlight-current-column
;;;
(defhooklet cosmonaut/highlight-current-column prog-mode cosmonaut/highlight-current-column
  (require 'col-highlight)
  (column-highlight-mode 1))

;;;
;;; highlight-current-line
;;;
(defhooklet cosmonaut/hl-line prog-mode cosmonaut/highlight-current-line
    (require 'hl-line+)
    (hl-line-mode 1))

;;;
;;; nyan
;;;
(defhooklet cosmonaut/nyan prog-mode cosmonaut/why-so-serious
    (nyan-mode 1))

;;;
;;; fill-column-indicator
;;;
(defvar cosmonaut-last-max-line-length cosmonaut/max-line-length)

(defhooklet cosmonaut/fill-column-indicator prog-mode cosmonaut/fill-column
  (require 'fill-column-indicator)
  (fci-mode 1)
  (setq fci-rule-column cosmonaut/max-line-length)
  ;; highlight too long lines
  (when cosmonaut/highlight-too-long-lines
    (let ((cosmonaut-max-line-length (concat "^[^\n]\\{"
					      (number-to-string cosmonaut/max-line-length)
					      "\\}\\(.*\\)$"))
	  (cosmonaut-previous-max-line-length (concat "^[^\n]\\{"
						       (number-to-string cosmonaut-last-max-line-length)
						       "\\}\\(.*\\)$")))
      (font-lock-remove-keywords nil (list (list (concat "^[^\n]\\{" cosmonaut-previous-max-line-length "\\}\\(.*\\)$") 1 font-lock-warning-face t)))
      (font-lock-add-keywords nil (list (list cosmonaut-max-line-length 1 font-lock-warning-face t)))))
  (setq cosmonaut-max-line-length cosmonaut/max-line-length))

;;;
;;; hideshow
;;;
(defhooklet cosmonaut/hideshow prog-mode cosmonaut/enable-hide-show-blocks
  (require 'hideshow)
  (require 'hideshowvis)
  (hs-minor-mode 1)
  (hideshowvis-enable)
  (hideshowvis-symbols)
  (hs-hide-initial-comment-block)
  (local-set-key (kbd "S-SPC") 'hs-toggle-hiding))

;;;
;;; ispell
;;;
;; (defhooklet cosmonaut/ispell prog-mode cosmonaut/enable-spell-checking
;;   (flyspell-prog-mode)) ;; Check strings for spelling errors

;;;
;;; projectile
;;;
(defhooklet cosmonaut/projectile prog-mode t
  (require 'projectile)
  (projectile-mode 1))

;;;
;;; require-final-newline
;;;
(defhooklet cosmonaut/require-final-newline prog-mode cosmonaut/require-final-newline
  (setq require-final-newline 1))

;;;
;;; indent-level
;;;
(defhooklet cosmonaut/indent-level prog-mode t
  (custom-set-variables
   '(standard-indent cosmonaut/indent-level)
   '(indent-tabs-mode nil)))

;;;
;;; ssh
;;;
(defhooklet cosmonaut/ssh ssh-mode t
  (setq ssh-directory-tracking-mode t)
  (shell-dirtrack-mode t)
  (setq dirtrackp nil))

;;;
;;; Ediff customizations
;;;
(setq ediff-ignore-similar-regions t)
(setq ediff-use-last-dir t)
(setq ediff-diff-options " -b ")

;;;
;;; common compilation options
;;;

;;;; Notify result from compilation buffer
(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

;;;
;;; helm-swoop
;;;
;; (require 'helm-swoop)
;; (global-set-key (kbd "C-M-S") 'helm-swoop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; set key to open-console
(global-set-key (kbd "<f12>") 'open-console)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; initialize package
;;;
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;;;
;;; automatically remove old backups
;;;
(when cosmonaut/clear-autobackups
  (message "Deleting old backup files...")
  (let ((week (* 60 60 24 7))
	(current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
		 (> (- current (float-time (fifth (file-attributes file))))
		    week))
	(message "%s" file)
	(delete-file file)))))

;;; cosmonaut-common.el ends here
