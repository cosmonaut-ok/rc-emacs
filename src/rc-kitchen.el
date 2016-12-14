;;; kitchen.el --- test kitchen support  -*- lexical-binding: t -*-

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

;;;
;;; test-kitchen
;;;

(defhooklet cosmonaut/chef-kitchen chef-mode
  (require 'test-kitchen))

;; patch for original test-kitchen.el. It does not supports automatic login
(defcustom test-kitchen-login-command "kitchen login"
  "The command used for converge project.")

(defun test-kitchen-login (instance)
  (interactive (list (completing-read "Kitchen instance to login: " (split-string (test-kitchen-list-bare)))))
  (let ((root-dir (test-kitchen-locate-root-dir)))
    (if root-dir
        (let ((default-directory root-dir))
	  (with-current-buffer (term "/bin/bash")
	    (term-send-raw-string (concat test-kitchen-login-command " " instance "\n"))))
      (error "Couldn't locate .kitchen.yml!"))))
;;
