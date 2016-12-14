;;; tex.el ---

;; Copyright (C) 2013 Alexander Vynnyk
;;
;; Author: cosmonaut@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;; ;; ;;(load "~/.emacs.d/share/auctex/auctex.el" nil t t)
;; ;; ;;(load "~/.emacs.d/share/auctex/preview/preview-latex.el" nil t t)

;; ;; (autoload 'tex-math-preview "tex-math-preview" nil t)

;; ;; ;; (setenv "TEXINPUTS"
;; ;; ;;         (concat (getenv "TEXINPUTS")
;; ;; ;;                 ":/home/ott/tex/styles//:/home/ott/projects/fprog/journal-issues/class//"))

(require 'tex-site)
(setq-default TeX-master (locate-source-file "share/master"))
(setq TeX-parse-self t)
(setq TeX-auto-save t)
(setq TeX-save-query nil)
;; ;; (setq TeX-default-mode 'latex-mode)
(setq TeX-open-quote "``")
(setq TeX-close-quote "''")
;; (setq TeX-PDF-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
;; (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
;; (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
;; (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
;; (autoload 'turn-on-bib-cite "bib-cite")
;; (add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; ;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; (setq LaTeX-eqnarray-label "eq"
;; LaTeX-equation-label "eq"
;; LaTeX-figure-label "fig"
;; LaTeX-table-label "tab"
;; LaTeX-myChapter-label "chap"
;; TeX-auto-save t
;; TeX-newline-function 'reindent-then-newline-and-indent
;; TeX-style-path
;; '("style/" "auto/"
;; "/usr/share/emacs21/site-lisp/auctex/style/"
;; "/var/lib/auctex/emacs21/"
;; "/usr/local/share/emacs/site-lisp/auctex/style/")
;; LaTeX-section-hook
;; '(LaTeX-section-heading
;; LaTeX-section-title
;; LaTeX-section-toc
;; LaTeX-section-section
;; LaTeX-section-label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cosmonaut/texinfo-hook ()
  (local-set-key [delete]  'delete-char)
  (define-key texinfo-mode-map [f10] 'tex-math-preview)
  (setq delete-key-deletes-forward t))

(add-hook 'texinfo-mode-hook 'cosmonaut/texinfo-hook)

;; ;; ;; CDLaTeX mode
;; ;; ;(autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
;; ;; ;(autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)
;; ;; ;(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex) ; with AUCTeX LaTeX mode
;; ;; ;(add-hook 'latex-mode-hook 'turn-on-cdlatex)
;; ;;                                         ; with Emacs latex mode

;; (require 'flymake)

;; (defun flymake-get-tex-args (file-name)
;; (list "pdflatex"
;; (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;; (add-hook 'LaTeX-mode-hook 'flymake-mode)

(setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

(defun cosmonaut/TeX-keymap ()
  (local-set-key [(meta i)]
		 '(lambda ()
		    (interactive)
		    (insert "\n\\item[] ")))
  ;; (local-set-key "\\" 'TeX-electric-macro)
  (local-set-key "C-cc-f" 'TeX-electric-macro)
  )

(defun cosmonaut/tex-mode-hook ()
  (font-lock-mode 1)
  (outline-minor-mode 1)
  (setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else
  (flyspell-mode 1)
  (flyspell-buffer)
  (turn-on-bib-cite)
  (setq bib-cite-use-reftex-view-crossref t)
  (cosmonaut/TeX-keymap)
  (cdlatex-mode 1)
  (reftex-mode 1)
  (TeX-PDF-mode 1)
  (setq reftex-plug-into-AUCTeX t)
  (define-key yas-minor-mode-map [C-tab] 'yas-expand)
  (define-key yas-minor-mode-map [tab] nil)
  (local-unset-key (kbd "<tab>"))
  (local-unset-key (kbd "TAB"))
  (local-set-key "<tab>" 'cdlatex-tab)
  )

(add-hook 'TeX-mode-hook 'cosmonaut/tex-mode-hook)
(add-hook 'LaTeX-mode-hook 'cosmonaut/tex-mode-hook)
(add-hook 'latex-mode-hook 'cosmonaut/tex-mode-hook)
;;
(add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
(add-hook 'laTeX-mode-hook #'LaTeX-preview-setup)
