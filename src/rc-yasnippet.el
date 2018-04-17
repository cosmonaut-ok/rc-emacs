;;; yasnippet.el ---  code sinppets support  -*- lexical-binding: t -*-

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

;; use popup menu for yas-choose-value

;;; Code:

(require 'yasnippet)
(require 'company-yasnippet)

;; (require 'yasnippets)

(yas-global-mode 1)

(add-to-list 'yas/snippet-dirs (concat cosmonaut/source-directory "/data/snippets/"))
(add-to-list 'yas/snippet-dirs (concat cosmonaut/source-directory "el-get/yasnippets/"))
(add-to-list 'yas/snippet-dirs (concat cosmonaut/source-directory "el-get/yasnippet/snippets/"))
(add-to-list 'yas/snippet-dirs (concat cosmonaut/source-directory "lib/rspec-mode/snippets/"))

(when (file-directory-p (concat cosmonaut/user-data-directory "/snippets/"))
  (add-to-list 'yas/snippet-dirs (concat cosmonaut/user-data-directory "/snippets/")))

;;;; Remove yasnippet from menu
(custom-set-variables
 '(yas-use-menu nil))

;; (yas-global-mode 1)
(yas-reload-all)

;; do not bind yasnippet to TAB key. Bind it to C-TAB
(define-key yas-minor-mode-map (kbd "<C-M-tab>") 'yas-ido-expand)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(global-set-key (kbd "<C-tab>") 'company-yasnippet)

(defhooklet cosmonaut/snippet-font-lock snippet-mode t
  (font-lock-mode 1))

;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Let you select (and expand) a yasnippet key."
  (interactive)
  (let ((original-point (point)))
    (while (and
	    (not (= (point) (point-min) ))
	    (not
	     (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
	   (word (buffer-substring init-word original-point))
	   (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
		  (lambda (s) (string-match (concat "^" word) s)) list)))
	(if (= (length key) 1)
	    (setq key (pop key))
	  (setq key (ido-completing-read "key: " list nil nil word)))
	(delete-char (- init-word original-point))
	(insert key)
	(yas-expand)))))

;; do not activate yas in term
(add-hook 'term-mode-hook (lambda()
			    (setq yas-dont-activate-functions t)))

;;;
;;; yas-minor-mode
;;;
(defhooklet cosmonaut/yas-minor-mode prog-mode
  (yas-minor-mode-on))

;; TODO: https://www.emacswiki.org/emacs/CompanyMode#toc10
;;; yasnippet.el ends here
