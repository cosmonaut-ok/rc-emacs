;;; chef.el --- make Cosmonaut bootstrap and early boot  -*- lexical-binding: t -*-

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

;; TODO

;;; Code:

(defvar cosmonaut/chef-keywords-list
  '("apt_repository"
    "ark"
    "at_exit" ;; ?
    "baseurl"
    "bash"
    "before"
    "chef_gem"
    "ChefSpec"
    "command"
    "context"
    "cookbook_file"
    "cron"
    "default"
    "define"
    "depends"
    "describe"
    "directory"
    "describe"
    "docker_container"
    "docker_image"
    "execute"
    "expect"
    "file"
    "fog_key_pair" ;; ?
    "gem_package"
    "git"
    "group"
    "hostsfile_entry"
    "htpasswd"
    "node"
    "include_recipe"
    "iptables_rule"
    "it"
    "jenkins_command"
    "jenkins_plugin"
    "jenkins_script"
    "link"
    "log"
    "logrotate_app"
    "mode"
    "mysql_client"
    "mysql_config"
    "mysql_database"
    "mysql_database_user"
    "mysql_service"
    "not_if"
    "notifies"
    "should"
    "subscribes"
    "ohai"
    "only_if"
    "override"
    "owner"
    "package"
    "php_fpm_pool"
    "python_pip"
    "python_service"
    "rackspace_dns_record"
    "rbenv_execute"
    "rbenv_gem"
    "rbenv_ruby"
    "remote_file"
    "rpm_package"
    "rsync_serve"
    "ruby_block"
    "service"
    "simple_iptables_rule"
    "source"
    "stub_command"
    "sudo"
    "swap_file"
    "sysctl_param"
    "tar_extract"
    "template"
    "user"
    "users_manage"
    "user_ulimit"
    "variables"
    "yum_package"
    "yum_repository"
    ))

(defhooklet cosmonaut/chef-mode-init enh-ruby-mode cosmonaut/enable-chef
  (require 'chef-mode)
  (chef-mode 1)
  ;;
  (dolist (res cosmonaut/chef-keywords-list)
    (pushnew res enh-ruby-extra-keywords :test 'string-equal))
  )

(require 'flycheck) ;; TODO: it's hack. Need to fix it
(eval-after-load "flycheck"
  '(when (and cosmonaut/enable-flycheck cosmonaut/enable-chef cosmonaut/enable-foodcritic)
     (flycheck-define-checker chef-foodcritic
			      "A Chef cookbooks syntax checker using Foodcritic.
See URL `http://acrmp.github.io/foodcritic/'."
			      :command ("foodcritic" (option-list "--tags" flycheck-foodcritic-tags) source)
			      :error-patterns
			      ((error line-start (message) ": " (file-name) ":" line line-end))
			      :modes (enh-ruby-mode ruby-mode)
			      :predicate
			      (lambda ()
				(let ((parent-dir (f-parent default-directory)))
				  (or
				   ;; Chef CookBook
				   ;; http://docs.opscode.com/chef/knife.html#id38
				   (locate-dominating-file parent-dir "recipes")
				   (locate-dominating-file parent-dir "resources")
				   (locate-dominating-file parent-dir "providers")
				   ;; Knife Solo
				   ;; http://matschaffer.github.io/knife-solo/#label-Init+command
				   (locate-dominating-file parent-dir "cookbooks"))))
			      :next-checkers ((warnings-only . ruby-rubocop)))))

;; yas-chef-mode
(defhooklet cosmonaut/chef-add-extra-snippets chef-mode t
  (yas-activate-extra-mode 'chef-mode))

;;; chef.el ends here
