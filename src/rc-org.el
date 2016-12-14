;;; rc-makefile.el --- makefile support  -*- lexical-binding: t -*-

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

(require 'org)
(require 'company-capf)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(add-hook 'org-mode-hook
	  (lambda ()
	    (setq org-todo-keywords
		  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
	    (font-lock-mode 1)
	    (setq org-log-done t)
	    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)
	    ;; (add-hook 'org-mode-hook #'my-org-mode-hook)
	    ))

;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
