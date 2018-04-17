;;; cosmonaut-custom.el --- Cosmonaut customizations  -*- lexical-binding: t -*-

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

;; Cosmonaut customizations

;;; Code:

;;;
;;; toplevel
;;;
(defgroup cosmonaut nil
  "Customization for ``Cosmonaut`` IDE, based on Emacs."
  :group 'emacs)

;;;
;;; system
;;;
(defgroup cosmonaut/system nil
  "Color schemas, fonts etc"
  :group 'cosmonaut
  )


(defcustom cosmonaut/enable-verbose nil
  "Enable verbose loading."
  :type 'boolean
  :group 'cosmonaut/system
  )

;;;
;;; face
;;;
(defgroup cosmonaut/face nil
  "Color schemas, fonts etc"
  :group 'cosmonaut
  )

(defcustom cosmonaut/enable-tabbar t
  "Automatically activate code browser, when program file opens."
  :type 'boolean
  :group 'cosmonaut/face
  )

(defcustom cosmonaut/familiar-copy-paste-cut t
  "Bind familiar Control+c, Control+x and Control+v, instead Emacs default keybindings."
  :type 'boolean
  :group 'cosmonaut/face
  )

(defcustom cosmonaut/fill-column t
  "Enable special highlighting of long lines."
  :type 'boolean
  :group 'cosmonaut/face
  )

(defcustom cosmonaut/notify-on-build t
  "Notify on build, compile or test process finished."
  :type 'boolean
  :group 'cosmonaut/face
  )

(defcustom cosmonaut/enable-github-flavored-preview nil
  "Enable markdown preview, generated with github (requires internet connection). Experimental!"
  :type 'boolean
  :group 'cosmonaut/face
  )

(defcustom cosmonaut/why-so-serious t
  "Lets enable some fun."
  :type 'boolean
  :group 'cosmonaut/face
  )

(defcustom cosmonaut/highlight-symbol t
  "Automatically highlight all symbols in buffer, matched to selected."
  :type 'boolean
  :group 'cosmonaut/face
  )

;;;
;;; backup
;;;
(defgroup cosmonaut/backup nil
  "Backup parameters"
  :group 'cosmonaut
  )

(defcustom cosmonaut/autobackup t
  "Enable automatic file backups."
  :type 'boolean
  :group 'cosmonaut/backup
  )

(defcustom cosmonaut/clear-autobackups nil
  "Clear old autobackup files during startup."
  :type 'boolean
  :group 'cosmonaut/backup
  )

(defcustom cosmonaut/autobackup-interval 300
  "Set interval between autobackups."
  :type 'integer
  :group 'cosmonaut/backup
  )

(defcustom cosmonaut/backup-directory (locate-user-data-file "backups/")
  "Enable backups." ;; FIXME: not working yet
  :type 'directory
  :group 'cosmonaut/backup
  )

;;;
;;; programming mode
;;;
(defgroup cosmonaut/programming nil
  "Customization for ``Cosmonaut`` IDE, based on Emacs."
  :group 'cosmonaut)

(defcustom cosmonaut/max-line-length 80
  "Defines maximum recommended line length in program."
  :type 'integer
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/highlight-too-long-lines t
  "Highlight lines, longer than maximum recommended length as ``warning``."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/indent-level 2
  "Define number of spaces to indent code."
  :type 'integer
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/indent-tabs-mode nil
  "Indent code by tabs (``false`` switches to spaces)."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/indicate-parentheses t
  "Show pair to current parenth and highlight other parenthesis."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/linum-mode t
  "Add lines numbering."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/require-final-newline t
  "Defines maximum recommended line length in program."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/highlight-current-line t
  "Highlight current line."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/highlight-current-column nil
  "Highlight current column."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/enable-hide-show-blocks t
  "Enable possibility to hide/show code blocks, like functions, classes etc."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/code-browser-switch-to-simple nil
  "Switch to simple directory navigaton ``false`` enables standard cosmonaut code browser."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/enable-flycheck t
  "Enable on-the-fly style checking (used with robe, rubocop and foodcritic)."
  :type 'boolean
  :group 'cosmonaut/programming
  )

(defcustom cosmonaut/flycheck-disabled-checkers '(ruby-rubylint)
  "Disable some flycheck checkers for ruby files."
  :type 'list
  :group 'cosmonaut/programming
  )

;; (defcustom cosmonaut/enable-flymake t
;;   "Enable flymake support (on-the-fly syntax checking)."
;;   :type 'boolean
;;   :group 'cosmonaut/programming
;;   )

(defcustom cosmonaut/enable-spell-checking nil
  "Enable spell checking in code comments."
  :type 'boolean
  :group 'cosmonaut/programming
  )
;;;
;;; ruby
;;;
(defgroup cosmonaut/ruby nil
  "Ruby-specific options"
  :group 'cosmonaut
)

(defcustom cosmonaut/enable-electric t
  "Enable ruby electric (fast parenth and block completions)."
  :type 'boolean
  :group 'cosmonaut/ruby
  )

(defcustom cosmonaut/enable-robe t
  "Enable ROBE automatically, when open ruby file."
  :type 'boolean
  :group 'cosmonaut/ruby
  )

(defcustom cosmonaut/enable-ruby-tools t
  "Enable ruby tools automatically, when open ruby file."
  :type 'boolean
  :group 'cosmonaut/ruby
  )

(defcustom cosmonaut/enable-ruby-refactor t
  "Enable ruby refactor automatically, when open ruby file."
  :type 'boolean
  :group 'cosmonaut/ruby
  )

(defcustom cosmonaut/enable-rvm nil
  "Use RVM (if possible)."
  :type 'boolean
  :group 'cosmonaut/ruby
  )

(defcustom cosmonaut/rvm-default-gemset "global"
  "Default rvm default gemset."
  :type 'string
  :group 'cosmonaut/ruby
  )

(defcustom cosmonaut/enable-rubocop t
  "Enable rubocop, when open ruby file."
  :type 'boolean
  :group 'cosmonaut/ruby
  )

(defcustom cosmonaut/enable-ri t
  "Ruby RI documentation support (press F1 on symbol to get it)."
  :type 'boolean
  :group 'cosmonaut/ruby
  )

;;;
;;; chef
;;;
(defgroup cosmonaut/chef nil
  "Color schemas, fonts etc"
  :group 'cosmonaut
  )

(defcustom cosmonaut/enable-chef t
  "Enable chef extensions, when using ruby files."
  :type 'boolean
  :group 'cosmonaut/chef
  )

(defcustom cosmonaut/enable-foodcritic t
  "Enable flycheck foodcritic checker (reboot needed)."
  :type 'boolean
  :group 'cosmonaut/chef
  )

(defcustom cosmonaut/enable-chefdk t
  "Enable chef extensions, when using ruby files."
  :type 'boolean
  :group 'cosmonaut/chef
  )

(defcustom cosmonaut/chefdk-home "/opt/chefdk"
  "Set chefDK home directory."
  :type 'directory
  :group 'cosmonaut/chef
  )

(defcustom cosmonaut/enable-bundler t
  "Enable bundler, when using ruby files."
  :type 'boolean
  :group 'cosmonaut/chef
  )

;;;
;;; 3rd-party important modes
;;;
(defgroup cosmonaut/misc nil
  "Not Cosmonaut-related important ruby, chef etc. options"
  :group 'cosmonaut
  )

(defgroup rspec-mode nil
  "RSpec minor mode."
  :group 'cosmonaut/misc)

(defgroup enh-ruby nil
  "Ruby mode."
  :group 'cosmonaut/misc)

(defgroup chef-mode nil
  "Chef minor mode."
  :group 'cosmonaut/misc)

(defgroup github-notifier nil
  "Chef minor mode."
  :group 'cosmonaut/misc)

;; add button open terminal here
(defcustom cosmonaut/terminal-emulator (get-terminal-emulator)
  "Default terminal emulator."
  :type 'string
  :group 'cosmonaut/system)

(defcustom cosmonaut/use-external-terminal-emulator nil
  "User external terminal emulator, instead of standard cosmonaut's shell."
  :type 'boolean
  :group 'cosmonaut/system)

(defcustom cosmonaut/check-parens-before-save nil
  "Check if all parens are paired before file saving."
  :type 'boolean
  :group 'cosmonaut/system)

;;; cosmonaut-custom.el ends here
