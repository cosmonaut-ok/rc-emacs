;;; markdown.el --- markdown file format support  -*- lexical-binding: t -*-

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


(require 'markdown-mode)

(add-auto-mode 'markdown-mode
               "\\.md\\'" "\\.markdown\\'")

;; Google-flavored markdown
(add-auto-mode 'gfm-mode
	       "README\\.md\\'")

(defhooklet cosmonaut/markdown markdown-mode t
  ;; The Markdown files I write using IA Writer use newlines to separate
  ;; paragraphs. That's why I need Visual Line Mode. I also need to
  ;; disable M-q. If I fill paragraphs, that introduces unwanted
  ;; newlines.
  (local-set-key (kbd "M-q") 'ignore)
  (visual-line-mode t))

(defhooklet cosmonaut/githubize-markdown markdown-mode cosmonaut/enable-github-flavored-preview
  (custom-set-variables
   '(setq markdown-command (concat cosmonaut/source-directory "scripts/markdown-flavor.rb"))))

(defhooklet cosmonaut/markdown-preview markdown-mode t
  (markdown-preview-mode t))

;;; cosmonaut-markdown.el ends here
