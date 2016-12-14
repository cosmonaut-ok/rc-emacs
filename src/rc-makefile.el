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

(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))
(add-to-list 'auto-mode-alist '("Makefile.*" . makefile-gmake-mode))

(defun cosmonaut/makefile-indent-line ()
  (save-excursion
    (forward-line 0)
    (cond
     ;; keep TABs
     ((looking-at "\t") t)
     ;; indent continuation lines to 4
     ((and (not (bobp))
	   (= (char-before (1- (point))) ?\\))
      (delete-horizontal-space)
      (indent-to 4))
     ;; delete all other leading whitespace
     ((looking-at "\\s-+")
      (replace-match "")))))


(defun cosmonaut/makefile-mode-hook ()
  (interactive)
  ;; (flyspell-prog-mode)
  ;; (flycheck-mode)
  ;; (linum-mode t)
  ;; (fci-mode)
  ;; (auto-complete-mode)
  ;; (subword-mode)
  (setq-local indent-tabs-mode t) ;; Turn On Tab Character
  (setq-local tab-width 4)
  (highlight-indentation-mode)
  ;; (setq-local indent-line-function 'cosmonaut/makefile-indent-line)
  )

(add-hook 'makefile-mode-hook 'cosmonaut/makefile-mode-hook)
