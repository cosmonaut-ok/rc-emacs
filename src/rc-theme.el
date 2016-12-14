;;; theme.el --- themes support  -*- lexical-binding: t -*-

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

(require 'color-theme)
(color-theme-initialize)

(setq custom-theme-directory (concat cosmonaut/source-directory "/data/themes"))

(when (file-directory-p (concat cosmonaut/user-data-directory "/themes"))
  ;; TODO: add all themes in this folder
  (add-to-list 'custom-theme-load-path (concat cosmonaut/user-data-directory "/themes")))

;; (load-theme 'cosmonaut-classic t)

;;; cosmonaut-theme.el ends here
