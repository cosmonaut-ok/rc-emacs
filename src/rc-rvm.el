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

(defvar rvm-installation-key "409B6B1796C275462A1703113804BB82D39DC0E3")

(define-colored-compilation-mode rvm-installation-mode "RVM installation")

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
	     (when cosmonaut/use-bundler
	       " --gems=bundler"))
	    'rvm-installation-mode))))

(defun rvm-install-ruby (ruby)
  (interactive "sWhich ruby do you want to install?: ")
  (async-shell-command
   (concat
    "source "
    (expand-file-name "~/.rvm/scripts/rvm")
    "; rvm install "
    ruby)
   "*Ruby installation*"))

(defun rvm-generate-docs ()
  (interactive)
  (compile
   (concat
    "source "
    (expand-file-name "~/.rvm/scripts/rvm")
    "; rvm docs generate ")
   'rvm-installation-mode))

;;; cosmonaut-rvm.el ends here
