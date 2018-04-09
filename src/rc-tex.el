;;; tex.el ---

;; Copyright (C) 2013 Alexander Vynnyk
;;
;; Author: cosmonaut@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;;(load "~/.emacs.d/share/auctex/auctex.el" nil t t)
;;(load "~/.emacs.d/share/auctex/preview/preview-latex.el" nil t t)

;; (autoload 'tex-math-preview "tex-math-preview" nil t)

;; (setenv "TEXINPUTS"
;;         (concat (getenv "TEXINPUTS")
;;                 ":/home/ott/tex/styles//:/home/ott/projects/fprog/journal-issues/class//"))

(defvar system-auctex-styles (locate-source-file "el-get/auctex/style/"))
(defvar local-auctex-styles (locate-source-file "style/"))
(defvar project-auctex-styles nil)

(defhooklet cosmonaut/texlatex (tex-mode latex-mode LaTeX-mode) t
  (require 'tex-site)
  (require 'preview-latex)
  (require 'company-auctex)
  (require 'latex-templates)

  ;; (require 'latex-preview-pane)
  ;; (require 'flymake)

  (autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
  (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil)

  ;; create auto lisp directory
  (dolist (v (append TeX-auto-private TeX-style-private))
    (when (not (file-directory-p v))
      (mkdir v t)))

  (push
   (locate-source-file "lib/latex-templates/templates")
   latex-templates-private)

  (turn-on-cdlatex)
  (local-unset-key (kbd "<tab>"))
  (local-unset-key (kbd "TAB"))
  (local-set-key "<tab>" 'cdlatex-tab)

  ;; (flymake-mode 1)
  (font-lock-mode 1)
  (outline-minor-mode 1)
  (yas-minor-mode 1)
  (define-key yas-minor-mode-map [C-tab] 'yas-expand)
  (define-key yas-minor-mode-map [tab] nil)

  (company-auctex-init)
  ;; (latex-preview-pane-mode 1)
  (autoload 'tex-math-preview "tex-math-preview" nil t)
  (setq project-auctex-styles (concat (file-name-directory (buffer-file-name)) "style/"))

  (define-key TeX-mode-map [f10] 'tex-math-preview)

  (custom-set-variables
   '(TeX-auto-save t)
   '(TeX-parse-self t)
   ;; '(TeX-save-query nil)
   '(TeX-master nil)
   '(TeX-PDF-mode t)
   ;; '(TeX-master (locate-source-file "share/master"))
   '(TeX-default-mode 'latex-mode)
   '(TeX-open-quote "``")
   '(TeX-close-quote "''")
   ;; autoinsert math block closing
   '(TeX-electric-math (cons "$" "$"))
   ;; hooks list, with new section
   ;; '(LaTeX-section-hook '(LaTeX-section-heading LaTeX-section-title LaTeX-section-section LaTeX-section-label))
   '(LaTeX-section-hook
     '(LaTeX-section-heading
       LaTeX-section-title
       LaTeX-section-toc
       LaTeX-section-section
       LaTeX-section-label))
   ;; '(TeX-arg-item-label-p nil)    ; set t, if you want to be asked for item label every time with C-c C-j
   ;; ;; (LaTeX-eqnarray-label "eq")
   ;; ;; (LaTeX-equation-label "eq")
   ;; ;; (LaTeX-figure-label "fig")
   ;; ;; (LaTeX-table-label "tab")
   '(TeX-newline-function 'reindent-then-newline-and-indent)
   '(TeX-style-path
     (list
      system-auctex-styles
      local-auctex-styles
      project-auctex-styles))
   '(outline-minor-mode-prefix "\C-c \C-o") ; Or something else
   )

    (flyspell-mode 1)
    (flyspell-buffer)
    ;; (turn-on-bib-cite)
    ;; (setq bib-cite-use-reftex-view-crossref t)
    ;; (cosmonaut/TeX-keymap)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; USING reftex-mode: https://www.gnu.org/software/auctex/manual/reftex/RefTeX-in-a-Nutshell.html#RefTeX-in-a-Nutshell
;; (autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
;; (setq reftex-plug-into-AUCTeX t)
;; (autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
;; (autoload 'reftex-citation "reftex-cite" "Make citation" nil)
;; (autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
;; (autoload 'turn-on-bib-cite "bib-cite")
;; (add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; ;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defhooklet cosmonaut/texinfo-hook texinfo-mode t
  (local-set-key [delete]  'delete-char)
  (define-key texinfo-mode-map [f10] 'tex-math-preview)
  (setq delete-key-deletes-forward t))

(defun flymake-get-tex-args (file-name)
  (list "pdflatex"
        (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;; (setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
;; (setq ispell-dictionary "english") ; this can obviously be set to any language your spell-checking program supports

;; (defun cosmonaut/TeX-keymap ()
;;   (local-set-key [(meta i)]
;; 		 '(lambda ()
;; 		    (interactive)
;; 		    (insert "\n\\item[] ")))
;;   ;; (local-set-key "\\" 'TeX-electric-macro)
;;   (local-set-key "C-cc-f" 'TeX-electric-macro)
;;   )

;; (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
;; (add-hook 'laTeX-mode-hook #'LaTeX-preview-setup)
