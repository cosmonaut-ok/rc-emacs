;;; cosmonaut-menubar.el --- cosmonaut menubar customizations  -*- lexical-binding: t -*-

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
;;; clearing default menu
;;;
(define-key global-map [menu-bar tools games] nil) ;; TODO: enable in why-so-serious mode :)
(define-key prog-mode-map [menu-bar hide-show] nil) ;; not working for some reason
(define-key global-map [menu-bar tools compare] nil)
(define-key global-map [menu-bar tools ediff-merge] nil)
(define-key global-map [menu-bar tools epatch] nil)
(define-key global-map [menu-bar tools compose-mail] nil)
(define-key global-map [menu-bar tools gnus-menu-item] nil)
(define-key global-map [menu-bar tools gnus] nil)
(define-key global-map [menu-bar tools rmail] nil)
(define-key global-map [menu-bar tools vc] nil)
(define-key global-map [menu-bar tools shell] nil)
(define-key global-map [menu-bar tools gdb] nil)
(define-key global-map [menu-bar tools compile] nil)
(define-key global-map [menu-bar tools semantic] nil)
(define-key global-map [menu-bar tools ede] nil)
;; (define-key global-map [menu-bar tools Projectile] nil)
(define-key global-map [menu-bar tools separator-prog] nil)
(define-key global-map [menu-bar tools Color\ Themes] nil)
(define-key global-map [menu-bar tools Start\ Code\ Browser\ \(ECB\)] nil)
(define-key global-map [menu-bar options package] nil) ;; TODO: do something with this

(define-key global-map [menu-bar help-menu emacs-news] nil)
(define-key global-map [menu-bar help-menu emacs-known-problems] nil)
(define-key global-map [menu-bar help-menu send-emacs-bug-report] nil)
(define-key global-map [menu-bar help-menu emacs-manual-bug] nil)
(define-key global-map [menu-bar help-menu search-documentation] nil)
(define-key global-map [menu-bar help-menu find-emacs-packages] nil)
(define-key global-map [menu-bar help-menu extra-packages] nil)
(define-key global-map [menu-bar help-menu external-packages] nil)
(define-key global-map [menu-bar help-menu external-packages] nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key-after prog-mode-map
  [menu-bar code]
  (cons "Code" (make-sparse-keymap "Code")) 'options)

(define-key prog-mode-map
  [menu-bar code activate-codebrowser]
  '("Start Code Browser (ECB)" . ecb-activate))

(define-key prog-mode-map
  [menu-bar code codebrowser-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar code toggle-codebrowser]
  '("Toggle Code Browser Windows" . toggle-code-browser))

(define-key prog-mode-map
  [menu-bar code codebrowser-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar code drag-stuff-up]
  '("Move Line/Selected Up" . drag-stuff-up))

(define-key prog-mode-map
  [menu-bar code drag-stuff-down]
  '("Move Line/Selected Down" . drag-stuff-down))

;; (define-key prog-mode-map
;;   [menu-bar code projectile]
;;   '("Project support" . projectile-mode-map))

;; Submap
(define-key prog-mode-map
  [menu-bar code compare]
  (cons "Compare" menu-bar-ediff-menu))

(define-key prog-mode-map
  [menu-bar code merge]
  (cons "Merge" menu-bar-ediff-merge-menu))

(define-key prog-mode-map
  [menu-bar code epatch]
  (cons "Apply Patch" menu-bar-epatch-menu))

;;;
;;; rubocop
;;;
;; (add-hook 'chef-mode-hook #'cosmonaut/chef-kitchen)

;; add rubocop/foodcritic menu

(define-key enh-ruby-mode-map
  [menu-bar code inspection]
  (cons "Inspect" (make-sparse-keymap "Inspect")))

(define-key enh-ruby-mode-map
  [menu-bar code inspection rubocop-check-project]
  '("Check Project" . rubocop-check-project))

(define-key enh-ruby-mode-map
  [menu-bar code inspection rubocop-check-directory]
  '("Check Directory" . rubocop-check-directory))

(define-key enh-ruby-mode-map
  [menu-bar code inspection rubocop-check-current-file]
  '("Check Current File" . rubocop-check-current-file))

(define-key prog-mode-map
  [menu-bar code inspection rubocop-separator]
  '("--" nil :visible))

(define-key enh-ruby-mode-map
  [menu-bar code inspection foodcritic-check-project]
  '("Foodcritic Project" . foodcritic-check-project))

(define-key enh-ruby-mode-map
  [menu-bar code inspection foodcritic-check-directory]
  '("Foodcritic Directory" . foodcritic-check-directory))

(define-key enh-ruby-mode-map
  [menu-bar code inspection foodcritic-check-current-file]
  '("Foodcritic File" . foodcritic-check-current-file))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction]
  (cons "Correct Syntax" (make-sparse-keymap "Correct Syntax")))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction autocorrect-current-file]
  '("Autocorrect Current File" . rubocop-autocorrect-current-file))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction autocorrect-directory]
  '("Autocorrect Directory" . rubocop-autocorrect-directory))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction autocorrect-project]
  '("Autocorrect Project" . rubocop-autocorrect-project))

(define-key prog-mode-map
  [menu-bar code format-selected]
  '("Format Selected" . indent-region))

(define-key prog-mode-map
  [menu-bar code expand]
  (cons "Expand" yas--minor-mode-menu))

(define-key prog-mode-map
  [menu-bar code complete]
  (cons "Complete" (make-sparse-keymap "Complete")))

(define-key prog-mode-map
  [menu-bar code comment-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar code comment/uncomment-selected]
  '("Comment/Uncomment Selected" . comment-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refactor
;;;
(define-key-after prog-mode-map
  [menu-bar refactor]
  (cons "Refactor" (make-sparse-keymap "Refactor")) 'options)

;;;; Ruby Refactor
(define-key enh-ruby-mode-map
  [menu-bar refactor ruby]
  (cons "Ruby Refactor" (make-sparse-keymap "Ruby Refactor")))

(define-key enh-ruby-mode-map
  [menu-bar refactor ruby ruby-refactor-add-parameter]
  '("Add Parameter" . ruby-refactor-add-parameter))

(define-key enh-ruby-mode-map
  [menu-bar refactor ruby ruby-refactor-extract-to-let]
  '("Extract To Let" . ruby-refactor-extract-to-let))

(define-key enh-ruby-mode-map
  [menu-bar refactor ruby ruby-refactor-extract-constant]
  '("Extract Constant" . ruby-refactor-extract-constant))

(define-key enh-ruby-mode-map
  [menu-bar refactor ruby ruby-refactor-extract-to-method]
  '("Extract To Method" . ruby-refactor-extract-to-method))

(define-key enh-ruby-mode-map
  [menu-bar refactor ruby ruby-refactor-remove-inline-temp]
  '("Remove Inline Temp" . ruby-refactor-remove-inline-temp))

(define-key enh-ruby-mode-map
  [menu-bar refactor ruby ruby-refactor-extract-local-variable]
  '("Extract LoCal Variable" . ruby-refactor-extract-local-variable))

(define-key enh-ruby-mode-map
  [menu-bar refactor ruby ruby-refactor-convert-post-conditional]
  '("Convert Post Conditional" . ruby-refactor-convert-post-conditional))

(define-key enh-ruby-mode-map
  [menu-bar refactor ruby ruby-refactor-separator]
  '("--" nil :visible))

(require 'ruby-tools)
;;;; Ruby Tools
(define-key prog-mode-map
  [menu-bar refactor ruby-tools-clear-string]
  '("Clear String" . ruby-tools-clear-string))

(define-key prog-mode-map
  [menu-bar refactor ruby-tools-to-symbol]
  '("Convert String To Symbol" . ruby-tools-to-symbol))

(define-key prog-mode-map
  [menu-bar refactor ruby-tools-to-single-quote-string]
  '("Double To Single Quote" . ruby-tools-to-single-quote-string))

(define-key prog-mode-map
  [menu-bar refactor ruby-tools-to-double-quote-string]
  '("Single To Double Quote" . ruby-tools-to-double-quote-string))

;;;; Ruby hash syntax
(define-key enh-ruby-mode-map
  [menu-bar refactor ruby-toggle-hash-syntax]
  '("Switch Between Old And New Ruby Hashtable Syntax" . ruby-toggle-hash-syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; VCS
;;;
(define-key prog-mode-map
  [menu-bar vcs]
  (cons "VCS" vc-menu-map))

;;;
;;; magit
;;;
;;;
(define-key prog-mode-map
  [menu-bar vcs git]
  (cons "Git" (make-sparse-keymap "Git")))

(define-key prog-mode-map
  [menu-bar vcs git clone]
  '("Clone Repository" . magit-clone))

(define-key prog-mode-map
  [menu-bar vcs git magit-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar vcs git push-to-upstream]
  '("Push Current Branch To Remote" . magit-push-current-to-pushremote))

(define-key prog-mode-map
  [menu-bar vcs git clone]
  '("Pull Current Branch From Remote" . magit-pull-from-pushremote))

(define-key prog-mode-map
  [menu-bar vcs git commit]
  '("Commit Changed" . magit-commit))

(define-key prog-mode-map
  [menu-bar vcs git show-log-current]
  '("Log Current Branch" . magit-log-current))

(define-key prog-mode-map
  [menu-bar vcs git add-changed]
  '("Manage Changed" . magit-diff-unstaged))

(define-key prog-mode-map
  [menu-bar vcs git show-status]
  '("Status" . magit-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key prog-mode-map
  [menu-bar vcs github]
  (cons "GitHub" (make-sparse-keymap "GitHub")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key prog-mode-map
  [menu-bar vcs github create-oauth-token]
  '("Create New Github OAuth Token" . gh-create-oauth-token))

(define-key prog-mode-map
  [menu-bar vcs github set-credentials]
  '("Set Github Username/Password" . gh-auth-set-credentials ))

(define-key prog-mode-map
  [menu-bar vcs github set-oauth-token]
  '("Set Github OAuth Token" . gh-auth-set-oauth-token))

(define-key prog-mode-map
  [menu-bar vcs github gh-separator-1]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar vcs github create-pullrequest]
  '("Create Pull Request From Branch" . magit-gh-pulls-create-pull-request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key magit-mode-map
  [menu-bar github]
  (cons "GitHub" (make-sparse-keymap "GitHub")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key magit-mode-map
  [menu-bar github add-changed]
  '("Create Pull Request Branch" . magit-gh-pulls-create-branch))

(define-key magit-mode-map
  [menu-bar github fetch-pr-commits]
  '("Fetch Pull Request Commits" . magit-gh-pulls-fetch-commits))

(define-key magit-mode-map
  [menu-bar github open-pr-in-browser]
  '("Open PR In Web Browser" . magit-gh-pulls-open-in-browser))

(define-key magit-mode-map
  [menu-bar github show-log-current]
  '("Reload Pull Request" . magit-gh-pulls-reload))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map
  [menu-bar options color-themes]
  '("Color Themes" . customize-themes))

(define-key global-map
  [menu-bar options cosmonaut/customize]
  '("Cosmonaut Customization Menu" . cosmonaut/customize))

;;;; reinit Cosmonaut
(define-key global-map
  [menu-bar options cosmonaut-reinit]
  '("Reload Cosmonaut configuration" . reinit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run
;;;
(define-key prog-mode-map
  [menu-bar run]
  (cons "Run" (make-sparse-keymap "Run")))

;;;
;;; test-kitchen
;;;

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen]
  (cons "Test Kitchen" (make-sparse-keymap "Test Kitchen")))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen destroy-all]
  '("Destroy All" . test-kitchen-destroy-all))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen destroy]
  '("Destroy" . test-kitchen-destroy))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen kitchen-separator-3]
  '("--" nil :visible))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen test-all]
  '("Test All" . test-kitchen-test-all))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen verify-all]
  '("Verify All" . test-kitchen-verify-all))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen converge-all]
  '("Converge All" . test-kitchen-converge-all))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen kitchen-separator-2]
  '("--" nil :visible))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen test]
  '("Test" . test-kitchen-test))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen verify]
  '("Verify" . test-kitchen-verify))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen login]
  '("Login" . test-kitchen-login))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen converge]
  '("Converge" . test-kitchen-converge))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen kitchen-separator-1]
  '("--" nil :visible))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen list]
  '("List" . test-kitchen-list))

;;;
;;; rspec
;;;
(define-key enh-ruby-mode-map
  [menu-bar run unit-testing]
  (cons "Unit Tests (RSpec)" (make-sparse-keymap "Unit Tests")))

(define-key enh-ruby-mode-map
  [menu-bar run unit-testing run-all-tests]
  '("Run All Tests" . rspec-verify-all))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen kitchen-separator]
  '("--" nil :visible))

(define-key enh-ruby-mode-map
  [menu-bar run unit-testing run-tests-current-file]
  '("Run All Tests On Current File" . rspec-verify))

(define-key enh-ruby-mode-map
  [menu-bar run unit-testing run-test-at-point]
  '("Run Test At Cursor" . rspec-verify-single))

;;;; GDB
(define-key prog-mode-map
  [menu-bar run gdb-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar run gdb]
  '("Debug (GDB)" . gdb))

(define-key prog-mode-map
  [menu-bar run gdb]
  '("Compile" . compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tools
;;;
;;;; shell command
(define-key prog-mode-map
  [menu-bar run shell-command-separator-1]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar run shell-command-on-selected]
  '("Selected As Shell Command" . shell-command-on-region))

(define-key prog-mode-map
  [menu-bar run shell-command]
  '("Shell Command" . shell-command))

(define-key prog-mode-map
  [menu-bar run shell-command-separator-2]
  '("--" nil :visible))

(define-key global-map
  [menu-bar tools ssh-session]
  '("SSH Session" . ssh))

;;;; ielm
(define-key global-map
  [menu-bar tools elisp-console]
  '("Interactive IDE Scripting Console" . ielm))

;;;; inf-ruby
(define-key global-map
  [menu-bar tools inf-ruby]
  '( "Interactive Ruby Console" . inf-ruby))

;;;
;;; package management
;;;
(define-key global-map
  [menu-bar tools additional-packages]
  (cons "Additional Packages" (make-sparse-keymap "Additional Packages")))

(define-key global-map
  [menu-bar tools additional-packages package-pm]
  (cons "Package.el Package Manager" (make-sparse-keymap "Package.el Package Manager")))

(define-key global-map
  [menu-bar tools additional-packages package-pm package-delete]
  '("Uninstall Package" . package-delete))

(define-key global-map
  [menu-bar tools additional-packages package-pm package-desc]
  '("Show Full Package Information (Not Implemented)" . package-desc-zzz))

(define-key global-map
  [menu-bar tools additional-packages package-pm package-install]
  '("Install Package" . package-install))

(define-key global-map
  [menu-bar tools additional-packages package-pm package-list-packages]
  '("List Packages" . package-list-packages))

;; el-get
(define-key global-map
  [menu-bar tools additional-packages el-get-pm]
  (cons "El-Get Package Manager (default)" (make-sparse-keymap "El-Get Package Manager (default)")))

(define-key global-map
  [menu-bar tools additional-packages el-get-pm el-get-remove]
  '("Uninstall Package" . el-get-remove))

(define-key global-map
  [menu-bar tools additional-packages el-get-pm el-get-describe]
  '("Show Full Package Information" . el-get-describe))

(define-key global-map
  [menu-bar tools additional-packages el-get-pm el-get-install]
  '("Install Package" . el-get-install))

(define-key global-map
  [menu-bar tools additional-packages el-get-pm el-get-list-packages]
  '("List Packages" . el-get-list-packages))

;;;
;;; rvm menu
;;;
(define-key global-map
  [menu-bar tools rvm]
  (cons "RVM" (make-sparse-keymap "RVM")))

(define-key global-map
  [menu-bar tools rvm rvm-install-rvm]
  '("Install RVM" . rvm-install-rvm))

(define-key global-map
  [menu-bar tools rvm rvm-generate-docs]
  '("Generate Ruby Documentation" . rvm-generate-docs))

(define-key global-map
  [menu-bar tools rvm rvm-use]
  '("Use Default Ruby For This Session" . rvm-use))

(define-key global-map
  [menu-bar tools rvm rvm-open-gem]
  '("Open Gem" . rvm-open-gem))

(define-key global-map
  [menu-bar tools rvm install-gem]
  '("Install Gem" . rvm-gem-install))

(define-key global-map
  [menu-bar tools rvm rvm-use]
  '("Switch Ruby And Gemset Version" . rvm-use))

;;;
;;; bundler Menu
;;;
(define-key global-map
  [menu-bar tools bundler]
  (cons "Bundler" (make-sparse-keymap "Bundler")))

(define-key global-map
  [menu-bar tools bundler bundle-exec]
  '("Exec" . bundle-exec))

(define-key global-map
  [menu-bar tools bundler bundle-show]
  '("Show" . bundle-show))

(define-key global-map
  [menu-bar tools bundler bundle-check]
  '("Check" . bundle-check))

(define-key global-map
  [menu-bar tools bundler bundle-update]
  '("Update" . bundle-update))

(define-key global-map
  [menu-bar tools bundler bundle-version]
  '("Version" . bundle-version))

(define-key global-map
  [menu-bar tools bundler bundle-gemfile]
  '("Gemfile" . bundle-gemfile))

(define-key global-map
  [menu-bar tools bundler bundle-install]
  '("Install" . bundle-install))

(define-key global-map
  [menu-bar tools bundler bundle-console]
  '("Console" . bundle-console))

(define-key global-map
  [menu-bar tools bundler bundle-outdated]
  '("Outdated" . bundle-outdated))

;;;
;;; berkshelf Menu
;;;

(define-key global-map
  [menu-bar tools berkshelf]
  (cons "Berkshelf" (make-sparse-keymap "Berkshelf")))

(define-key global-map
  [menu-bar tools berkshelf berks-viz]
  '("Build Dependencies Graph" . berks-viz))

(define-key global-map
  [menu-bar tools berkshelf berks-contingent]
  '("Show Dependencies For Cookbook" . berks-contingent))

(define-key global-map
  [menu-bar tools berkshelf berks-list]
  '("List" . berks-list))

(define-key global-map
  [menu-bar tools berkshelf berks-show]
  '("Info" . berks-info))

(define-key global-map
  [menu-bar tools berkshelf berks-verify]
  '("Verify" . berks-verify))

(define-key global-map
  [menu-bar tools berkshelf berks-outdated]
  '("Outdated" . berks-outdated))

(define-key global-map
  [menu-bar tools berkshelf berks-search]
  '("Search For Remote Cookbooks" . berks-search))

(define-key global-map
  [menu-bar tools berkshelf berks-delimiter-1]
  '("--" nil :visible))

(define-key global-map
  [menu-bar tools berkshelf berks-upload]
  '("Upload" . berks-upload))

(define-key global-map
  [menu-bar tools berkshelf berks-update]
  '("Update" . berks-update))

(define-key global-map
  [menu-bar tools berkshelf berks-install]
  '("Install" . berks-install))

;;;
;;; keyboard macros menu
;;;
(define-key global-map
  [menu-bar tools kmacro]
  (cons "Keyboard Macro" (make-sparse-keymap "keyboard Macro")))

;;;; Write keyboard macro
(define-key global-map
  [menu-bar tools kmacro kmacro-edit-macro]
  '("Edit Keyboard Macro" . kmacro-edit-macro))

(define-key global-map
  [menu-bar tools kmacro kmacro-call-macro]
  '("Call Keyboard Macro" . kmacro-call-macro))

(define-key global-map
  [menu-bar tools kmacro kmacro-end-macro]
  '("End Writing Keyboard Macro" . kmacro-end-macro))

(define-key global-map
  [menu-bar tools kmacro kmacro-start-macro]
  '("Start Writing Keyboard Macro" . kmacro-start-macro))

;;;
;;; JSON
;;;
(define-key json-mode-map
  [menu-bar code json-separator-1]
  '("--" nil :visible))

(define-key json-mode-map
  [menu-bar code json-reformat]
  '("Pretty Format JSON file" . json-reformat-region))

(define-key json-mode-map
  [menu-bar code json-snatcher]
  '("JSON Print Path" . jsons-print-path))

;;;
;;; Merkdown
;;;
(define-key markdown-mode-map
  [menu-bar markdown-tools]
  (cons "Markdown Tools" (make-sparse-keymap "Markdown Tools")))

(define-key markdown-mode-map
  [menu-bar markdown-tools markdown-preview]
  '("Preview Markdown File" . markdown-preview-open-browser))

(define-key markdown-mode-map
  [menu-bar markdown-tools markdown-toc]
  '("Generate Table of Contents" . markdown-toc-generate-toc))

(define-key gfm-mode-map
  [menu-bar markdown-tools]
  (cons "Markdown Tools" (make-sparse-keymap "Markdown Tools")))

(define-key gfm-mode-map
  [menu-bar markdown-tools markdown-preview]
  '("Preview Markdown file" . markdown-preview-open-browser))

(define-key gfm-mode-map
  [menu-bar markdown-tools markdown-toc]
  '("Generate Table of Contents" . markdown-toc-generate-toc))

;;;
;;; help menu
;;;
(define-key global-map
  [menu-bar help-menu cosmonaut-help-separator-1]
  '("--" nil :visible))

(define-key global-map
  [menu-bar help-menu install-required-dependencies]
  '("Install Cosmonaut Required Dependencies" . startup-wizard))

(define-key global-map
  [menu-bar help-menu cosmonaut-help-separator-2]
  '("--" nil :visible))

(define-key global-map
  [menu-bar help-menu cosmonaut-installation]
  '("Cosmonaut Installation (Online)" . cosmonaut/installation))

(define-key global-map
  [menu-bar help-menu cosmonaut-cheat-sheet]
  '("Cosmonaut Keybindings Cheat Sheet (Online)" . cosmonaut/cheat-sheet))

(define-key global-map
  [menu-bar help-menu cosmonaut-faq]
  '("Cosmonaut FAQ (Online)" . cosmonaut/faq))

(define-key global-map
  [menu-bar help-menu cosmonaut-help]
  '("Cosmonaut Help (Online)" . cosmonaut/help))

;;; cosmonaut-menubar.el ends here
