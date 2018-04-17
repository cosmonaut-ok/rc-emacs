;;; bundler.el --- make Cosmonaut bootstrap and early boot  -*- lexical-binding: t -*-

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

(require 'bundler)

;; variable: cosmonaut/enable-chefdk
;; variable: cosmonaut/chefdk-home

(defun bundler-make-complete-command (cmd)
  (let ((chef-command (cosmonaut-chefdk-command "chef")))
    (if (and cosmonaut/enable-chefdk chef-command)
        (concat chef-command " exec " cmd)
      cmd)))

;; define bundler compilation mode
(define-compilation-mode bundler-compilation-mode "Bundler compilation"
  "Compilation mode for Bundler output."
  (add-hook 'compilation-filter-hook 'cosmonaut/colorize-compilation-buffer nil t))

(defun bundle-install-cosmonaut-gems ()
  (interactive)
  (when cosmonaut/enable-rvm
    (rvm-use-default))
  (bundle-gemfile (locate-source-file "Gemfile"))
  (bundle-install))

(defhooklet cosmonaut/bundle enh-ruby-mode t
  (local-set-key (kbd "<S-f3>") 'bundle-update))

;;;; patch for bundler.el to support chefdk

(defun bundle-console ()
  "Run an inferior Ruby process in the context of the current bundle."
  (interactive)
  (run-ruby (bundler-make-complete-command "bundle console")))

(defun bundle-version ()
  "Prints version information."
  (interactive)
  (shell-command (bundler-make-complete-command "bundle version")))

(define-colored-compilation-mode cosmonaut/bundler-compilation-mode "*Bundler*")

(defun bundle-command (cmd)
  "Run cmd in an async buffer."
  ;; (async-shell-command (bundler-make-complete-command cmd) "*Bundler*"))
  (compile (bundler-make-complete-command cmd) 'cosmonaut/bundler-compilation-mode))

(defun bundle-gem-location (gem-name)
  "Returns the location of the given gem, or 'no-gemfile if the
Gemfile could not be found, or nil if the Gem could not be
found."
  (let ((bundler-stdout
         (shell-command-to-string
          (bundler-make-complete-command
           (format "bundle show %s" (shell-quote-argument gem-name)))))
        (remote (file-remote-p default-directory)))
    (cond
     ((string-match "Could not locate Gemfile" bundler-stdout)
      'no-gemfile)
     ((string-match "Could not find " bundler-stdout)
      nil)
     (t
      (concat remote
              (replace-regexp-in-string
               "Resolving dependencies...\\|\n" ""
               bundler-stdout)
              "/")))))

(defun bundle-list-gems ()
  (save-excursion
    (let* ((cmd (bundler-make-complete-command "bundle list"))
           (bundle-out (shell-command-to-string cmd))
           (bundle-lines (split-string bundle-out "\n")))

      (defun parse-bundle-list-line (line)
        (cond
         ((string-match "^  \\* \\([^\s]+\\).*$" line)
          (match-string 1 line))
         ((string-match "Could not \\(find\\|locate\\)" line)
          (message line) nil)
         ((string-match "Gems included by the bundle:\\|^ *$" line)
          nil)
         (t
          (message "Warning: couldn't parse line from \"%s\":\n%s"
                   cmd line)
          nil)))

      (remq nil (mapcar 'parse-bundle-list-line bundle-lines)))))

(defun bundle-list-gem-paths ()
  (save-excursion
    (let* ((cmd (bundler-make-complete-command "bundle list --paths"))
           (bundle-out (shell-command-to-string cmd)))
      (split-string bundle-out "\n"))))

;;; cosmonaut-bundler.el ends here
