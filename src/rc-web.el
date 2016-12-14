;;; web.el --- web mode support  -*- lexical-binding: t -*-

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

(require 'web-mode)

(setq web-mode-code-indent-offset cosmonaut/indent-level)
(setq web-mode-markup-indent-offset cosmonaut/indent-level)
(setq web-mode-css-indent-offset cosmonaut/indent-level)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(require 'company-web-html)

(defhooklet cosmonaut/company-web-mode web-mode
  (company-mode t)
  (cosmonaut/local-push-company-backend 'company-web-html))

;;; cosmonaut-web.el ends here
