;;; rc-lisp.el --- lisp language support  -*- lexical-binding: t -*-

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

;;; Package --- Cosmonaut

;;; Commentary:

;;; Code:

;;; SLIME
(require 'inf-lisp)
(require 'slime)
(require 'hyperspec)

;; hyperspec root
(setq common-lisp-hyperspec-root (locate-source-file "share/HyperSpec"))
      
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(setq slime-net-coding-system 'utf-8-unix)

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy slime-asdf slime-banner slime-fuzzy slime-repl inferior-slime))
     (setq slime-complete-symbol*-fancy t) ;; TODO: ???
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (slime-mode t)))


(add-hook 'inferior-lisp-mode-hook
          (lambda () (inferior-slime-mode t)))

;; autocompletion for SLIME
(require 'slime-company)

(add-hook 'slime-mode-hook
          (lambda ()
            (slime-autodoc-mode 1)
            ;; (define-key slime-mode-map (kbd "<C-tab>") 'auto-complete)
            ;; (define-key slime-repl-mode-map (kbd "<C-tab>") 'auto-complete)
	    (slime-autodoc-mode t)
	    ))

;; (add-hook 'slime-mode-hook (lambda () (auto-complete-mode t)))
;; (add-hook 'slime-repl-mode-hook (lambda () (auto-complete-mode t)))

(add-hook 'slime-mode-hook (lambda () (slime-autodoc-mode t)))

(add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")  :coding-system utf-8-unix))

(setq slime-use-autodoc-mode nil)
(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy slime-asdf slime-banner slime-fuzzy slime-repl slime-company))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     (setq slime-repl-history-remove-duplicates t)
     (setq slime-repl-history-trim-whitespaces t)
     (setq slime-protocol-version 'ignore) ;; ignore version mismatch
     ))

(global-set-key [(control f11)] 'slime-selector)

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (font-lock-mode 1)))

;; (defun slime-repl-mode-end-of-defun ()
;;   (forward-sexp) t)

;; Slime-mode
(defun slime-description-fontify ()
  "Fontify sections of SLIME Description."
  (with-current-buffer "*SLIME Description*"
    (highlight-regexp
     (concat "^Function:\\|"
             "^Macro-function:\\|"
             "^Its associated name.+?) is\\|"
             "^The .+'s arguments are:\\|"
             "^Function documentation:$\\|"
             "^Its.+\\(is\\|are\\):\\|"
             "^On.+it was compiled from:$")
     'hi-green-b)))

(defadvice slime-show-description (after slime-description-fontify activate)
  "Fontify sections of SLIME Description."
  (slime-description-fontify))

;; kill SLIME correctly before exiting emacs
(defun slime-kill-emacs-kill ()
  (interactive)
  (when (slime-connected-p)
    (if (equal (slime-machine-instance)
	       (if (string-match "^\\([a-zA-Z0-9_-]+\\)\\." system-name)
		   (match-string 1 system-name)
		 system-name))
        (slime-quit-lisp)
      (slime-disconnect)))
  (slime-kill-all-buffers)
  (sleep-for 1) ;; just another hack. It's not working w/o sleeping
  (save-buffers-kill-emacs))

;; (add-hook 'kill-emacs-hook 'slime-kill)
(global-set-key (kbd "C-x C-c") 'slime-kill-emacs-kill) ;; dirty hack, but it's not working in other way

;; (defvar slime-apropos-anchor-regexp "^[^ ]")
;; (defun slime-apropos-next-anchor ()
;;   (interactive)
;;   (let ((pt (point)))
;;     (forward-line 1)
;;     (if (re-search-forward slime-apropos-anchor-regexp nil t)
;;         (goto-char (match-beginning 0))
;;       (goto-char pt)
;;       (error "anchor not found"))))

;; (defun slime-apropos-prev-anchor ()
;;   (interactive)
;;   (let ((p (point)))
;;     (if (re-search-backward slime-apropos-anchor-regexp nil t)
;;         (goto-char (match-beginning 0))
;;       (goto-char p)
;;       (error "anchor not found"))))

;; ;; SLIME local keymap
;; ;; *for apropos
;; (defvar slime-apropos-minor-mode-map (make-sparse-keymap))
;; (define-key slime-apropos-minor-mode-map "\C-m" 'slime-describe-symbol)
;; (define-key slime-apropos-minor-mode-map "l" 'slime-describe-symbol)
;; (define-key slime-apropos-minor-mode-map "j" 'slime-apropos-next-anchor)
;; (define-key slime-apropos-minor-mode-map "k" 'slime-apropos-prev-anchor)
;; (define-minor-mode slime-apropos-minor-mode "")

;; (defadvice slime-show-apropos (after slime-apropos-minor-mode activate)
;;   ""
;;   (when (get-buffer "*SLIME Apropos*")
;;     (with-current-buffer "*SLIME Apropos*" (slime-apropos-minor-mode 1))))
;; ;; *for balanced comments
;; (define-key slime-repl-mode-map (kbd "C-c ;")
;;   'slime-insert-balanced-comments)

;; (setq slime-auto-connect 'always)


;; ;; lookup information in hyperspec
;; (require 'info-look)
;; (info-lookup-add-help
;;  :mode 'lisp-mode
;;  :regexp "[^][()'\" \t\n]+"
;;  :ignore-case t
;;  :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

;; redshank
;; Add this to your Emacs configuration:
;;
(require 'redshank-loader) ;; "~/.emacs.d/share/redshank/redshank-loader.el")

(eval-after-load "redshank-loader"
  `(redshank-setup '(lisp-mode-hook
                     slime-repl-mode-hook) t))

(setq autopair-handle-action-fns
      (list 'autopair-default-handle-action
            '(lambda (action pair pos-before)
               (hl-paren-color-update))))

(add-to-list 'auto-mode-alist '("\\.lts$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lts$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . lisp-mode))

(defhooklet cosmonaut/lisp-generic (lisp-mode lisp-interaction-mode) t
  ;; (slime-mode 1)
  (abbrev-mode 1)
  ;; (yafolding-mode 1) ;; TODO: do we need a folding mode?
  (turn-on-eldoc-mode)
  ;; (paredit-mode -1)
  ;; (auto-complete-mode 1)
  (setq tab-width 2)
  ;; (when (not (slime-connected-p) (slime))) ;; TODO: check if ECB activated
  ;; Set some local keys
  (local-set-key "\C-c\]" 'slime-close-all-parens-in-sexp)
  (local-set-key "\C-h\C-l" 'hyperspec-lookup)
  (local-set-key [?\M-\t] 'slime-indent-and-complete-symbol)
  (local-set-key [(control c) tab] 'slime-complete-form)
  ;; (local-set-key [backtab] 'auto-complete)
  ;; (ecb-activate)
  (set (make-local-variable 'slime-lisp-implementations)
       (list (assoc 'sbcl slime-lisp-implementations))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lisp-mode hook

;; There is no prog-mode in emacs23
;; (add-hook 'lisp-mode-hook 'cosmonaut/lisp-mode-hook)
;; (add-hook 'lisp-interaction-mode-hook 'cosmonaut/lisp-mode-hook)
;; (add-hook 'lisp-interaction-mode-hook 'init-prog-mode)

