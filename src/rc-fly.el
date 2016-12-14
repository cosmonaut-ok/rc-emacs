;;; cosmonaut-fly.el --- flycheck and flymake support  -*- lexical-binding: t -*-

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
;;; flycheck
;;;
(defhooklet cosmonaut/flycheck prog-mode cosmonaut/enable-flycheck
  (require 'flycheck)
  (require 'flycheck-pos-tip)
  (flycheck-mode 1)
  (flycheck-pos-tip-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(defun flycheck-print-current-checker (args)
  "Print checker for current buffer.
   ARGS is dummy."
  (interactive "P")
  (print (flycheck-get-checker-for-buffer)))

;;;
;;; flymake
;;;
(defhooklet cosmonaut/flymake prog-mode cosmonaut/enable-flymake
  (require 'flymake)
  (flymake-mode 1))

;;; cosmonaut-fly.el ends here
