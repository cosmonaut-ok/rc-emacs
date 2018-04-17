;;; rspec.el --- rspec support  -*- lexical-binding: t -*-

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

(require 'rspec-mode)

;;;
;;; rspec
;;;
(custom-set-variables
 '(compilation-scroll-output t)
 '(rspec-command-options ""))

;;;; add snippets after rspec loaded
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))

(defhooklet cosmonaut/rspec (chef-mode ruby-mode enh-ruby-mode eruby-mode html-erb-mode) t
  (local-set-key (kbd "<f8>") 'rspec-verify-all)
  (local-set-key (kbd "<C-f8>") 'rspec-verify)
  (local-set-key (kbd "<S-f8>") 'rspec-verify-single)
  )

;; rspec-mode patch
(defcustom rspec-use-chefdk-when-possible nil
  "When t and chefDK home defined correctly, run specs with 'chef exec'.
Disables running specs with bundle, Zeus of Spring."
  :type 'boolean
  :group 'rspec-mode)

(defcustom rspec-chefdk-home-directory "/opt/chefdk"
  "Use `chef exec` for rspec when it's possible"
  :type 'directory
  :group 'rspec-mode)

(defun rspec-chefdk-p ()
  (and rspec-use-chefdk-when-possible
       (cosmonaut-chefdk-command "chef")))

(defun rspec-runner ()
  "Return command line to run rspec."
  (let ((chefdk-command (if (rspec-chefdk-p) (concat (cosmonaut-chefdk-command "chef") " exec ")))
	(bundle-command (if (rspec-bundle-p) "bundle exec "))
        (zeus-command (if (rspec-zeus-p) "zeus " nil))
        (spring-command (if (rspec-spring-p) "spring " nil)))
    (concat (or chefdk-command zeus-command spring-command bundle-command)
            (if (rspec-rake-p)
                (concat rspec-rake-command " spec")
              rspec-spec-command))))

;;; cosmonaut-rspec.el ends here
