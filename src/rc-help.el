;;; cosmonaut-help.el --- help and versioning  -*- lexical-binding: t -*-

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

(defun cosmonaut-version (&optional short)
  (interactive)
  (let ((version "0.3.0")) ;; autoconf-anchor
    (if short
	(princ version)
      (princ (concat "Cosmonaut Chef IDE version: " version ".\nBuild with Emacs version:\n" (emacs-version) "\n")))))

(define-colored-compilation-mode selfdeps-installation-mode "Installing Cosmonaut required dependencies")

(defun bundle-install-selfdeps (&optional buffer result &rest rest)
  (interactive)
  (setq
   compilation-finish-functions
   (remove 'bundle-install-selfdeps compilation-finish-functions)) ; ``delete'' is not working properly sometimes
  (if (or (null result) (string-match "^finished" result))
      (progn
	(cd cosmonaut/source-directory)
	(compile
	 (concat
	  ". "
	  (expand-file-name "~/.rvm/scripts/rvm") ;; init required env. variables before start
	  "; bundle install ")
	 'selfdeps-installation-mode))
    (error "RVM installation failed. Can not continue.")))

(defun startup-wizard (ruby-version)
  (interactive "sInput ruby version to install: ")
  (async-shell-command (concat "/bin/sh " (locate-source-file "bootstrap.sh") " " ruby-version) "*Cosmonaut Bootstrap*"))

(defun show-md-doc-page (name)
  (browse-url (concat "https://github.com/cosmonaut-ide/cosmonaut/wiki/" name)))

;; (defun show-md-doc-page (name)
;;   (let ((file-pathname (locate-source-file name)))
;;     (if (file-exists-p file-pathname)
;; 	(let ((the-buffer (find-file-read-only file-pathname))
;; 	      (browse-url-browser-function 'xwidget-webkit-browse-url))
;; 	  (with-current-buffer the-buffer
;; 	    (markdown-preview)
;; 	    (kill-buffer the-buffer)))
;;       (error "No such file %s. Check your installation consistency" name))))


(defun cosmonaut/help ()
  (interactive)
  (show-md-doc-page "Home"))

(defun cosmonaut/faq ()
  (interactive)
  (show-md-doc-page "FAQ"))

(defun cosmonaut/cheat-sheet ()
  (interactive)
  (show-md-doc-page "cheatSheet"))

(defun cosmonaut/installation ()
  (interactive)
  (show-md-doc-page "Installation"))

;;; cosmonaut-help.el ends here
