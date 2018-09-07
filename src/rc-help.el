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
;;  (let ((the-buffer (find-file-read-only file-pathname))
;;        (browse-url-browser-function 'xwidget-webkit-browse-url))
;;    (with-current-buffer the-buffer
;;      (markdown-preview)
;;      (kill-buffer the-buffer)))
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

(defun cosmonaut/keybindings ()
  (interactive)
  (let ((msg
         (concat "Key   Description\n"
                 "___   ____________\n"
                 "\n"
                 "F3    Berks install/update\n"
                 "C-F3    Rvm use ruby@gem\n"
                 "S-F3    Bundle install/update\n"
                 "\n"
                 "F5    Check project with foodcritic\n"
                 "S-F5    Check current file with foodcritic\n"
                 "\n"
                 "F6    Check project with rubocop/cookstyle\n"
                 "S-F6    Check current file with rubocop/cookstyle\n"
                 "\n"
                 "F8    Run rspec/chefspec on project\n"
                 "C-F8    rspec/chefspec only on current file\n"
                 "C-S-F8  rspec/chefspec only on current line\n"
                 "\n"
                 "C-S-F9  Run `kitchen verify` with single VM\n"
                 "S-F9    Run `kitchen verify` with all VMs\n"
                 "F9    Run kitchen converge on single VM\n"
                 "C-F9    Run kitchen converge` on all VMs\n"
                 "\n"
                 "F11   Fullscreen editor\n"
                 "C-F11 Show/hide compile window\n"
                 "\n"
                 "F12   Open unix shell\n"
                 "C-F12 Open ruby shell with IRB/PRY\n"
                 "C-S-F12 Open native emacs shell\n"
                 "\n"
                 "F7/C-s  Search\n"
                 "S-F7    Highlight phrase\n"
                 "C-S-F7  Unhighlight phrase of regexp\n"
                 "M-F7  Search in whole project\n"
                 "\n"
                 "C-x C-s Save current file\n"
                 "C-x s Save all unsaved opened files\n"
                 )))
    (message-box msg)
    ))

(global-set-key (kbd "<S-f1>") 'cosmonaut/keybindings)

;;; rc-help.el ends here
