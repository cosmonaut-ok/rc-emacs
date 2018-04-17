;;; cosmonaut-chef.el --- make Cosmonaut bootstrap and early boot  -*- lexical-binding: t -*-

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
  '(
    "a2enmod"
    "ark"
    "apt_package"
    "apt_preference"
    "apt_repository"
    "apt_update"

    "bash"
    "batch"
    "bff_package"
    "breakpoint"
    "build_essential"

    "cab_package"
    "chef_acl"
    "chef_client"
    "chef_container"
    "chef_data_bag"
    "chef_data_bag_item"
    "chef_environment"
    "chef_gem"
    "chef_group"
    "chef_handler"
    "chef_mirror"
    "chef_node"
    "chef_organization"
    "chef_role"
    "chef_user"
    "chocolatey_package"
    "common"
    "cookbook_file"
    "cron"
    "csh"

    "delete_lines"
    "deploy"
    "directory"
    "dmg_package"
    "dnf_package"
    "docker_container"
    "docker_image"
    "docker_registry"
    "dpkg_package"
    "dsc_resource"
    "dsc_script"

    "erlang_call"
    "examples"
    "execute"

    "file"
    "filebeat_prospector"
    "flywaydb"
    "freebsd_package"

    "gem_package"
    "git"
    "group"

    "homebrew_cask"
    "homebrew_package"
    "homebrew_tap"
    "hostname"
    "hostsfile_entry"
    "htpasswd"
    "http_request"

    "ifconfig"
    "ips_package"
    "iptables_rule"

    "jenkins_command"
    "jenkins_password_credentials"
    "jenkins_plugin"
    "jenkins_private_key_credentials"
    "jenkins_script"
    "jenkins_ssh_slave"
    "jenkins_user"

    "ksh"

    "launchd"
    "link"
    "load_balancer"
    "log"
    "logrotate_app"
    "logstash_forwarder"

    "machine"
    "machine_batch"
    "machine_execute"
    "machine_file"
    "machine_image"
    "macos_userdefaults"
    "macports_package"
    "mdadm"
    "mount"
    "msu_package"
    "mysql2_chef_gem"
    "mysql_client"
    "mysql_config"
    "mysql_database"
    "mysql_database_user"
    "mysql_service"

    "nfs_export"
    "nvm_install"

    "ohai"
    "ohai_hint"
    "openbsd_package"
    "openssl_dhparam"
    "openssl_rsa_private_key"
    "openssl_rsa_public_key"
    "osx_profile"

    "package"
    "pacman_package"
    "paludis_package"
    "perl"
    "php_fpm_pool"
    "phpmyadmin"
    "portage_package"
    "powershell_package"
    "powershell_script"
    "private_key"
    "public_key"
    "python"
    "python_pip"

    "rbenv_gem"
    "reboot"
    "reference"
    "registry_key"
    "remote_directory"
    "remote_file"
    "rhsm_errata"
    "rhsm_errata_level"
    "rhsm_register"
    "rhsm_repo"
    "rhsm_subscription"
    "route"
    "rpcbind"
    "rpm_package"
    "rsync_server"
    "ruby"
    "ruby_block"

    "script"
    "service"
    "smartos_package"
    "solaris_package"
    "subversion"
    "ssh_keygen"
    "sudo"
    "swap_file"
    "sysctl"
    "sysctl_param"
    "systemd_unit"

    "tar_extract"
    "template"
    "tomcat_install"
    "tomcat_service"

    "user"
    "user_ulimit"
    "users_manage"

    "windows_ad_join"
    "windows_auto_run"
    "windows_env"
    "windows_feature"
    "windows_feature_dism"
    "windows_feature_powershell"
    "windows_font"
    "windows_package"
    "windows_path"
    "windows_printer"
    "windows_printer_port"
    "windows_service"
    "windows_shortcut"
    "windows_task"

    "xml_edit"
    "xml_file"

    "yum"
    "yum_package"
    "yum_package_lock"
    "yum_repository"

    "zypper_package"
    "zypper_repository"
    ))

(require 'chef-mode)

(require 'flycheck) ;; TODO: it's hack. Need to fix it
(let ((chef-file-full-path (concat
          (file-name-as-directory cosmonaut/chefdk-home)
          (file-name-as-directory "bin")
          "chef")))
  (eval-after-load "flycheck"
    `(when (and cosmonaut/enable-flycheck cosmonaut/enable-chef cosmonaut/enable-foodcritic)
       (flycheck-define-checker chef-foodcritic
        "A Chef cookbooks syntax checker using Foodcritic.
See URL `http://acrmp.github.io/foodcritic/'."
        :command ,(if cosmonaut/enable-chefdk
                `(,chef-file-full-path "exec" "foodcritic" (option-list "--tags" flycheck-foodcritic-tags) source)
              '("foodcritic" (option-list "--tags" flycheck-foodcritic-tags) source))
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
        :next-checkers ((warnings-only . ruby-rubocop))))))

;; yas-chef-mode
(defhooklet cosmonaut/chef-add-extra-snippets chef-mode t
  (yas-activate-extra-mode 'chef-mode))

(defhooklet cosmonaut/imenu-replace chef-mode t
  (setq imenu-generic-expression
        '(("Ruby structures" "^\\( *\\(def\\|class\\|module\\) +.+\\)" 1)
          ("Actions and Resources" "^\\( *\\([a-zA-Z][a-zA-Z0-9]\\).+ +.+do\\)" 1)
          ("Included Receipes" "^\\( *\\(include_recipe\\) +.+\\)" 1)
          ))
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  )

(defhooklet cosmonaut/chef-mode-init enh-ruby-mode cosmonaut/enable-chef
  (dolist (res cosmonaut/chef-keywords-list)
    ;; (pushnew res enh-ruby-extra-keywords :test 'string-equal)
    (add-to-list 'enh-ruby-extra-keywords res)
    ;; (add-to-list 'enh-ruby-defun-beg-keywords res)
    )
  ;; add "action" to method-list
  ;; (add-to-list 'enh-ruby-defun-beg-keywords "action")

  (erm-reset) ;; `erm-reset'  will need to be called in order for any global changes to take effect.
  (chef-mode 1)
  )

(defun cosmonaut-chefdk-switch ()
  (custom-set-variables
   '(berkshelf-chefdk-home-directory cosmonaut/chefdk-home)
   '(test-kitchen-chefdk-home-directory cosmonaut/chefdk-home)
   '(rspec-chefdk-home-directory cosmonaut/chefdk-home))
  ;;
  (if cosmonaut/enable-chefdk
      (custom-set-variables
       '(berkshelf-use-chefdk-when-possible t)
       '(rspec-use-chefdk-when-possible t)
       '(test-kitchen-use-chefdk-when-possible t)
       ;; '(chef-knife-command (concat (cosmonaut-chefdk-command "chef") " exec knife"))
       ;;
       ;; '(knife-kitchen-use-chefdk-when-possible t)
       ;; '(foodcritic-use-chefdk-when-possible t)
       ;; '(bundler-use-chefdk-when-possible t)
       ;; '(gem-use-chefdk-when-possible t)
       ;; '(rubocop-use-chefdk-when-possible t)
       ;; '(bundler-use-chefdk-when-possible t)
       ;;
       )
    (custom-set-variables
     '(berkshelf-use-chefdk-when-possible nil)
     '(rspec-use-chefdk-when-possible nil)
     '(test-kitchen-use-chefdk-when-possible nil)
     ;; '(chef-knife-command "knife")
     ;;
     ;; '(knife-kitchen-use-chefdk-when-possible t)
     ;; '(foodcritic-use-chefdk-when-possible t)
     ;; '(bundler-use-chefdk-when-possible t)
     ;; '(gem-use-chefdk-when-possible t)
     ;; '(rubocop-use-chefdk-when-possible t)
     ;; '(bundler-use-chefdk-when-possible t)
     ;;
     )))

(defun cosmonaut-bundler-switch ()
  (if cosmonaut/enable-bundler
      (progn
  (custom-set-variables
   '(berkshelf-use-bundler-when-possible t)
   '(rspec-use-bundler-when-possible t)
   '(test-kitchen-use-bundler-when-possible t)
   ;;
   ;; '(knife-kitchen-use-bundler-when-possible t)
   ;; '(foodcritic-use-bundler-when-possible t)
   ;; '(bundler-use-bundler-when-possible t)
   ;; '(gem-use-bundler-when-possible t)
   ;; '(rubocop-use-bundler-when-possible t)
   ;;
   )
  ;; (if (not cosmonaut/enable-chefdk)
  ;;     (custom-set-variables
  ;;      '(chef-use-bundler t)))
        )
    (progn
      (custom-set-variables
       '(berkshelf-use-bundler-when-possible nil)
       '(rspec-use-bundler-when-possible nil)
       '(test-kitchen-use-bundler-when-possible nil)
       ;;
       ;; '(knife-kitchen-use-bundler-when-possible nil)
       ;; '(foodcritic-use-bundler-when-possible nil)
       ;; '(bundler-use-bundler-when-possible nil)
       ;; '(gem-use-bundler-when-possible nil)
       ;; '(rubocop-use-bundler-when-possible nil)
       ;;
       ;; '(chef-use-bundler nil)
       ))))

(defhooklet cosmonaut/chefdk-switcher prog-mode t
  (cosmonaut-chefdk-switch))

(defhooklet cosmonaut/bundler-switcher prog-mode t
  (cosmonaut-bundler-switch))
;;; cosmonaut-chef.el ends here
