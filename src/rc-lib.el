;;; lib.el --- common cosmonaut library  -*- lexical-binding: t -*-

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
;;; cosmonaut speciall functions
;;;
(require 'cl-lib)
(require 'url)

(defun cosmonaut/customize ()
  (interactive)
  (customize-group 'cosmonaut))

;; reload configuration
(defun reinit ()
  "Reload all cosmonaut configuration"
  (interactive)
  (let ((cosmonaut/do-bootstrap nil))
    (load-file user-init-file)))

(defun get-terminal-emulator ()
  (let ((terms '("konsole" "mate-terminal" "gnome-terminal" "terminator" "rxvt" "xterm")))
    (cl-labels ((get-first-existing-term (termlist)
					 (cond ((not termlist) nil)
					       ((call-process "which" nil nil nil (car termlist))
						(replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "which" " " (car termlist)))))
					       (t (get-first-existing-term (cdr termlist))))))
	       (get-first-existing-term terms))))

(defun open-console ()
  (interactive)
  (if cosmonaut/use-external-terminal-emulator
      (call-process cosmonaut/terminal-emulator)
    (term "/bin/bash")))

;;;; Common Functions
(defun add-auto-mode (mode &rest files)
  "Connect file type to some"
  (dolist (f files)
    (add-to-list 'auto-mode-alist (cons f mode))))

(defun flash-cross ()
  (interactive)
  (require 'hl-line)
  (require 'col-highlight)
  (col-highlight-flash)
  (hl-line-flash))

;;;
;;; common compilation options
;;;

(defun get-first-n-list-elements (n list)
  (cond ((null list) nil)
	 ((= n 0) nil)
	 (t (cons (car list) (get-first-n-list-elements (- n 1) (cdr list))))))

(defun notify-compilation-result (buffer result &rest rest)
  "Notify about the ended compilation in BUFFER.
  This function is intended to be used in
  `compilation-finish-functions'."
  (with-current-buffer buffer
    (unless (eq major-mode 'grep-mode)
      (let ((urgency) (status))
	(if (string-match "^finished" result)
	    (progn
	      (setq urgency 'normal)
	      (setq status "ok"))
	  (progn
	    (setq urgency 'critical)
	    (setq status "fail")))
	(when cosmonaut/notify-on-build
	  (notify (concat "Build: " (buffer-name buffer))
		  (format (concat "Command: %s\n"
				  "Result: %s"
				  )
			  ;;
			  (concat (get-first-n-list-elements
				   25 (string-to-list compile-command)) "...")
			  ;;
			  result
			  )
		  :urgency urgency
		  :icon (locate-source-file
			 (concat "data/icons/hicolor/48x48/status-" status ".png"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro verbose-message (&rest message)
  "Print MESSAGE only in verbose mode."
  `(when cosmonaut/enable-verbose (message ,@message)))

(defmacro defhooklet (name mode condition &rest body)
  "This is the test NAME, MODE, CONDITION, BODY."
  (cond ((null mode) nil)
	((atom mode)
	 `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
		    #'(lambda ()
			(when ,condition
			  (verbose-message "Launching ``%s'' hooklet on ``%s'' mode" ',name ',mode)
			  ,@body))))
	(t
	 `(progn
	    (add-hook ',(intern (concat (symbol-name (car mode)) "-hook"))
		    #'(lambda ()
			(when ,condition
			  (verbose-message "Launching ``%s'' hooklet on ``%s'' mode" ',name ',(car mode))
			  ,@body)))
	    (defhooklet ,name ,(cdr mode) ,condition ,body)))))

(defun cosmonaut/enable-verbose-messages ()
  (interactive)
  (custom-set-variables
   '(cosmonaut/enable-verbose t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-sublist-by-fist-element (name map)
  (cond ((null map) nil)
	((eq name (caar map))
	 (car map))
	(t (find-tool-bar-item-by-name name (cdr map)))))

(cl-defun tool-bar-delete-item (name &optional (map tool-bar-map))
  (let ((item (find-sublist-by-fist-element name (cdr map))))
    (delete item map)))

(defmacro tool-bar-add-item-for-mode (icon def key mode &rest rest)
  `(tool-bar-add-item ,icon ,def ,key :active-modes '(,mode) ,@rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cosmonaut/colorize-compilation-buffer ()
  "Colorize compile buffer output."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(defmacro define-colored-compilation-mode (name title &rest body)
  "Define compilation mode with ansi colors support."
  `(define-compilation-mode ,name ,title
     (progn
       (add-hook 'compilation-filter-hook 'cosmonaut/colorize-compilation-buffer nil t)
       ,@body)))

;; define simple colored installation mode
(define-colored-compilation-mode cosmonaut/colored-compilation-mode "*compile*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun download-file (url &optional download-name)
  (interactive "sEnter download URL:\nFEnter destination filename:")
  (let ((url (if (or
		  (string-match "^http://" url)
		  (string-match "^https://" url)
		  (string-match "^ftp" url))
		 url
	       (concat "http://" url))))
  (let ((download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      ;; we may have to trim the http response
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (write-file download-name)))))

(defun gpg-install-key (key)
  (if (executable-find "gpg")
      (let ((cmd (concat "gpg --keyserver hkp://keys.gnupg.net --recv-keys " key)))
	(compile cmd 'cosmonaut/colored-compilation-mode))
    (error "There is no ``gpg'' binary installed in system. Can not continue")))


;;; cosmonaut-lib.el ends here
