;;; rc-c-pp.el --- c/c++ language support  -*- lexical-binding: t -*-

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

;;;
;;; Generic
;;;

(require 'cc-mode)
(require 'ggtags)
;; (require 'semantic)

;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)
;; (global-semantic-stickyfunc-mode 1)

;; (semantic-mode 1)

(defhooklet cosmonaut/c-pp-init (c-mode-common c-mode c++-mode) t
  (hs-minor-mode 1)
  (c-set-style "bsd") ;; see below
  (setq c-basic-offset 2
	tab-width 2	; default to 4 visible spaces to display a tab
	indent-tabs-mode t)
  ;; GROUP: Editing -> Editing Basics
  (custom-set-variables
   '(global-mark-ring-max 5000)	; increase mark ring to contains 5000 entries
   '(mark-ring-max 5000) ; increase kill ring to contains 5000 entries
   '(kill-ring-max 5000) ; increase kill-ring capacity
   '(kill-whole-line t)	; if NIL, kill whole line and move the next line up
   ;;
   '(mode-require-final-newline t)      ; add a newline to end of file
   '(c-basic-offset 2)
   '(tab-width 2)	; default to 4 visible spaces to display a tab
   '(indent-tabs-mode t)
   '(gc-cons-threshold 100000000)
   '(show-trailing-whitespace 1)
   ;;
   ;; Available C style:
   ;; “gnu”: The default style for GNU projects
   ;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
   ;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
   ;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
   ;; “stroustrup”: What Stroustrup, the author of C++ used in his book
   ;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
   ;; “linux”: What the Linux developers use for kernel development
   ;; “python”: What Python developers use for extension modules
   ;; “java”: The default style for java-mode (see below)
   ;; “user”: When you want to define your own style
   '(c-default-style "bsd")
   ;;
   )
  (delete-selection-mode t)
  (ggtags-mode 1)
  )

(defhooklet cosmonaut/c-pp-cedet (c-mode-common c-mode c++-mode) t
  (local-set-key "\C-c\C-j" 'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" 'semantic-ia-show-summary))

;; Enable EDE only in C/C++
(require 'ede)
(global-ede-mode)

(defhooklet cosmonaut/c-pp-company (c-mode-common c-mode c++-mode) t
  (add-to-list 'company-backends 'company-c-headers))

;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-language-environment "UTF-8")
;; (prefer-coding-system 'utf-8)

;; show whitespace in diff-mode
(defhooklet cosmonaut/diff-mode diff-mode t
  (setq-local whitespace-style
	      '(face
		tabs
		tab-mark
		spaces
		space-mark
		trailing
		indentation::space
		indentation::tab
		newline
		newline-mark))
  (whitespace-mode 0))

;; Package: volatile-highlights
;; GROUP: Editing -> Volatile Highlights
;; (use-package volatile-highlights
;;   :init
;;   (volatile-highlights-mode t))

;; Package: clean-aindent-mode
(defhooklet cosmonaut/clean-aindent prog-mode t
  (clean-aindent-mode 1))

;; Package: dtrt-indent
;; (use-package dtrt-indent
;;   :init
;;   (dtrt-indent-mode 1)
;;   (setq dtrt-indent-verbosity 0))

;; Package: ws-butler
;; (use-package ws-butler
;;   :init
;;   (add-hook 'prog-mode-hook 'ws-butler-mode)
;;   (add-hook 'text-mode 'ws-butler-mode)
;;   (add-hook 'fundamental-mode 'ws-butler-mode))

;; PACKAGE: comment-dwim-2
;; (global-set-key (kbd "M-;") 'comment-dwim-2)

;; PACKAGE: anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
;; (use-package anzu
;;   :init
;;   (global-anzu-mode)
;;   (global-set-key (kbd "M-%") 'anzu-query-replace)
;;   (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp))

;; ;; PACKAGE: iedit
;; (use-package iedit
;;   :bind (("C-;" . iedit-mode))
;;   :init
;;   (setq iedit-toggle-key-default nil))

;; ;; Customized functions
;; (defun prelude-move-beginning-of-line (arg)
;;   "Move point back to indentation of beginning of line.

;; Move point to the first non-whitespace character on this line.
;; If point is already there, move to the beginning of the line.
;; Effectively toggle between the first non-whitespace character and
;; the beginning of the line.

;; If ARG is not nil or 1, move forward ARG - 1 lines first. If
;; point reaches the beginning or end of the buffer, stop there."
;;   (interactive "^p")
;;   (setq arg (or arg 1))

;;   ;; Move lines first
;;   (when (/= arg 1)
;;     (let ((line-move-visual nil))
;;       (forward-line (1- arg))))

;;   (let ((orig-point (point)))
;;     (back-to-indentation)
;;     (when (= orig-point (point))
;;       (move-beginning-of-line 1))))

;; (global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "When called interactively with no active region, copy a single
;; line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (message "Copied line")
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))

;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single
;;   line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))

;; ;; kill a line, including whitespace characters until next non-whiepsace character
;; ;; of next line
;; (defadvice kill-line (before check-position activate)
;;   (if (member major-mode
;;               '(emacs-lisp-mode scheme-mode lisp-mode
;;                                 c-mode c++-mode objc-mode
;;                                 latex-mode plain-tex-mode))
;;       (if (and (eolp) (not (bolp)))
;;           (progn (forward-char 1)
;;                  (just-one-space 0)
;;                  (backward-char 1)))))

;; ;; taken from prelude-editor.el
;; ;; automatically indenting yanked text if in programming-modes
;; (defvar yank-indent-modes
;;   '(LaTeX-mode TeX-mode)
;;   "Modes in which to indent regions that are yanked (or yank-popped).
;; Only modes that don't derive from `prog-mode' should be listed here.")

;; (defvar yank-indent-blacklisted-modes
;;   '(python-mode slim-mode haml-mode)
;;   "Modes for which auto-indenting is suppressed.")

;; (defvar yank-advised-indent-threshold 1000
;;   "Threshold (# chars) over which indentation does not automatically occur.")

;; (defun yank-advised-indent-function (beg end)
;;   "Do indentation, as long as the region isn't too large."
;;   (if (<= (- end beg) yank-advised-indent-threshold)
;;       (indent-region beg end nil)))

;; (defadvice yank (after yank-indent activate)
;;   "If current mode is one of 'yank-indent-modes,
;; indent yanked text (with prefix arg don't indent)."
;;   (if (and (not (ad-get-arg 0))
;;            (not (member major-mode yank-indent-blacklisted-modes))
;;            (or (derived-mode-p 'prog-mode)
;;                (member major-mode yank-indent-modes)))
;;       (let ((transient-mark-mode nil))
;;         (yank-advised-indent-function (region-beginning) (region-end)))))

;; (defadvice yank-pop (after yank-pop-indent activate)
;;   "If current mode is one of `yank-indent-modes',
;; indent yanked text (with prefix arg don't indent)."
;;   (when (and (not (ad-get-arg 0))
;;              (not (member major-mode yank-indent-blacklisted-modes))
;;              (or (derived-mode-p 'prog-mode)
;;                  (member major-mode yank-indent-modes)))
;;     (let ((transient-mark-mode nil))
;;       (yank-advised-indent-function (region-beginning) (region-end)))))

;; ;; prelude-core.el
;; (defun indent-buffer ()
;;   "Indent the currently visited buffer."
;;   (interactive)
;;   (indent-region (point-min) (point-max)))

;; ;; prelude-editing.el
;; (defcustom prelude-indent-sensitive-modes
;;   '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
;;   "Modes for which auto-indenting is suppressed."
;;   :type 'list)

;; (defun indent-region-or-buffer ()
;;   "Indent a region if selected, otherwise the whole buffer."
;;   (interactive)
;;   (unless (member major-mode prelude-indent-sensitive-modes)
;;     (save-excursion
;;       (if (region-active-p)
;;           (progn
;;             (indent-region (region-beginning) (region-end))
;;             (message "Indented selected region."))
;;         (progn
;;           (indent-buffer)
;;           (message "Indented buffer.")))
;;       (whitespace-cleanup))))

;; (global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; ;; add duplicate line function from Prelude
;; ;; taken from prelude-core.el
;; (defun prelude-get-positions-of-line-or-region ()
;;   "Return positions (beg . end) of the current line
;; or region."
;;   (let (beg end)
;;     (if (and mark-active (> (point) (mark)))
;;         (exchange-point-and-mark))
;;     (setq beg (line-beginning-position))
;;     (if mark-active
;;         (exchange-point-and-mark))
;;     (setq end (line-end-position))
;;     (cons beg end)))

;; ;; smart openline
;; (defun prelude-smart-open-line (arg)
;;   "Insert an empty line after the current line.
;; Position the cursor at its beginning, according to the current mode.
;; With a prefix ARG open line above the current line."
;;   (interactive "P")
;;   (if arg
;;       (prelude-smart-open-line-above)
;;     (progn
;;       (move-end-of-line nil)
;;       (newline-and-indent))))

;; (defun prelude-smart-open-line-above ()
;;   "Insert an empty line above the current line.
;; Position the cursor at it's beginning, according to the current mode."
;;   (interactive)
;;   (move-beginning-of-line nil)
;;   (newline-and-indent)
;;   (forward-line -1)
;;   (indent-according-to-mode))

;; (global-set-key (kbd "M-o") 'prelude-smart-open-line)
;; (global-set-key (kbd "M-o") 'open-line)

;; (provide 'setup-editing)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(defhooklet cosmonaut/gdb (c-mode-common c-mode c++-mode) t
  (gdb-enable-debug t)
  ;; use gdb-many-windows by default
  (gdb-many-windows t)
  (custom-set-variables
   ;; Non-nil means display source file containing the main routine at startup
   '(gdb-show-main t)
   )
  )

(defhooklet cosmonaut/c-pp-flycheck (c-mode-common c-mode c++-mode) t
  (custom-set-variables
   `(flycheck-gcc-include-path '(,default-directory))))

;; company
(delete 'company-semantic company-backends)
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; ;; Package zygospore
;; (use-package zygospore
;;   :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
;;          ("RET" .   newline-and-indent)))

;;   ; automatically indent when press RET

;; ;; activate whitespace-mode to view all whitespace characters
;; (global-set-key (kbd "C-c w") 'whitespace-mode)
;; (windmove-default-keybindings)

(dolist (map (list ggtags-mode-map dired-mode-map))
  (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key map (kbd "C-c g f") 'ggtags-find-file)
  (define-key map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key map (kbd "M-.") 'ggtags-find-tag-dwim)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map (kbd "C-c <") 'ggtags-prev-mark)
  (define-key map (kbd "C-c >") 'ggtags-next-mark))

;; (use-package ivy
;;   :init
;;   (progn
;;     (ivy-mode 1)
;;     (setq ivy-use-virtual-buffers t)
;;     (global-set-key (kbd "C-c s") 'swiper)))

;; (use-package counsel
;;   :bind
;;   (("M-x" . counsel-M-x)
;;    ("M-y" . counsel-yank-pop)
;;    ("C-c r" . counsel-recentf)
;;    ("C-x C-f" . counsel-find-file)
;;    ("<f1> f" . counsel-describe-function)
;;    ("<f1> v" . counsel-describe-variable)
;;    ("<f1> l" . counsel-load-library)
;;    ("C-h f" . counsel-describe-function)
;;    ("C-h v" . counsel-describe-variable)
;;    ("C-h l" . counsel-load-library)))

;; (use-package counsel-projectile
;;   :init
;;   (counsel-projectile-on))

;; (provide 'setup-ivy-counsel)
;; (require 'ivy)

;; (ivy-mode 1)

;; (setq ivy-re-builders-alist
;;       '((read-file-name-internal . ivy--regex-fuzzy)
;;         (t . ivy--regex-plus)))

;; (setq ivy-use-virtual-buffers t)

;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "M-y") 'counsel-yank-pop)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-load-library)
;; (global-set-key (kbd "C-h f") 'counsel-describe-function)
;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)
;; (global-set-key (kbd "C-h l") 'counsel-load-library)

;; (global-set-key (kbd "C-c s") 'swiper)

;; (provide 'setup-ivy-counsel)

;;; rc-c-pp.el ends here
