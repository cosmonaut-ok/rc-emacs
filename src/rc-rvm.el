;;; cosmonaut-rvm.el --- RVM support  -*- lexical-binding: t -*-

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

(require 'rvm)
(require 'url)
;; RVM installation hack
(defvar rvm-installation-url "https://get.rvm.io/")
(defvar rvm--gemset-default "cosmonaut")

(defvar rvm-installation-key "409B6B1796C275462A1703113804BB82D39DC0E3")

(setq rvm--gemset-default cosmonaut/rvm-default-gemset)

(define-colored-compilation-mode rvm-installation-mode "RVM installation")

(defcustom rvm-script
  (or (and (file-readable-p "~/.rvm/scripts/rvm") "~/.rvm/scripts/rvm")
      (and (file-readable-p "/usr/local/rvm/scripts/rvm") "/usr/local/rvm/scripts/rvm"))
  "Location of RVM script source."
  :group 'rvm
  :type 'file)

(defun rvm-install-rvm (ruby)
  (interactive "sWhich ruby do you want to install?: ")
  (let ((installator (locate-user-cache-file ".rvm.sh")))
    (progn (gpg-install-key rvm-installation-key)
	   (download-file rvm-installation-url installator)
	   (compile
	    (concat
	     "/bin/bash "
	     installator
	     " stable"
	     " --ruby="
	     ruby
	     " --auto-dotfiles"
	     (when cosmonaut/enable-bundler
	       " --gems=bundler"))
	    'rvm-installation-mode))))

(defun rvm-install-ruby (ruby)
  (interactive "sWhich ruby do you want to install?: ")
  (async-shell-command
   (concat
    "source "
    (expand-file-name rvm-script)
    "; rvm install "
    ruby)
   "*Ruby installation*"))

(defun rvm-generate-docs ()
  (interactive)
  (compile
   (concat
    "source "
    (expand-file-name rvm-script)
    "; rvm docs generate ")
   'rvm-installation-mode))

(defun rvm-use-as-default (new-ruby new-gemset)
  "switch the current ruby version to any ruby, which is installed with rvm and use it as default"
  (interactive
   (let* ((picked-ruby (rvm--completing-read "Ruby Version: "
					     (rvm/list)))
	  (picked-gemset (rvm--completing-read "Gemset: "
					       (rvm/gemset-list picked-ruby))))
     (list picked-ruby picked-gemset)))
  (when (rvm-working-p)
    (let* ((new-ruby-with-gemset (rvm--ruby-gemset-string new-ruby new-gemset))
	   (ruby-info (rvm/info new-ruby-with-gemset))
	   (new-ruby-binary (cdr (assoc "ruby" ruby-info)))
	   (new-ruby-gemhome (cdr (assoc "GEM_HOME" ruby-info)))
	   (new-ruby-gempath (cdr (assoc "GEM_PATH" ruby-info))))
      (setq rvm--current-ruby new-ruby)
      (setq rvm--current-gemset new-gemset)
      (setq rvm--gemset-default new-gemset)
      (rvm--set-ruby-default new-ruby new-gemset)
      (rvm--set-gemhome new-ruby-gemhome new-ruby-gempath new-gemset))
    (rvm--message (concat "Ruby: " new-ruby " Gemset: " new-gemset))))

(defun rvm--set-ruby-default (ruby gemset)
  (shell-command-to-string
   (concat
    "source "
    (expand-file-name rvm-script)
    "; rvm use "
    ruby
    rvm--gemset-separator
    gemset
    " --default;"
    )))

(defhooklet cosmonaut/rvm enh-ruby-mode t
  (local-set-key (kbd "<C-f3>") 'rvm-use))

;;; cosmonaut-rvm.el ends here
