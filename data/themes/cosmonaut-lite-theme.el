;;; cosmonaut-lite-theme.el --- a soothing theme with a pastel color palette

;; Author: Brian Mastenbrook <brian@mastenbrook.net>
;; Keywords: themes
;; URL: https://github.com/bmastenbrook/cosmonaut-lite-theme-el
;; Version: 1.0
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;; This file was written by Brian Mastenbrook and is placed in the
;; public domain.

;;; Code:

(deftheme cosmonaut-lite
  "cosmonaut-lite, a soothing theme with a pastel color palette")

(custom-theme-set-faces
 'cosmonaut-lite
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(minibuffer-prompt ((t (:foreground "steelblue4"))))
 '(highlight ((t (:background "lightsteelblue2"))))
 '(shadow ((t (:foreground "grey50"))))
 '(font-lock-builtin-face ((t (:foreground "Maroon"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "Cyan4"))))
 '(font-lock-constant-face ((t (:foreground "DarkRed"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "DarkBlue"))))
 '(font-lock-keyword-face ((t (:foreground "Purple4"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-string-face ((t (:foreground "Gray30"))))
 '(font-lock-type-face ((t (:foreground "Aquamarine4"))))
 '(font-lock-variable-name-face ((t (:foreground "tomato"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "DarkRed"))))
 '(mode-line ((t (:box (:line-width -1 :color "grey70" :style nil) :foreground "grey0" :background "grey90"))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "grey50"))))
 '(default ((t (:background "#EAF0F0" :foreground "black"))))
 '(fringe ((t (:background "#EAF0F0")))))

;;;###autoload
(when load-file-name
  (add-to-list
   'custom-theme-load-path
   (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun cosmonaut-lite-theme ()
  "Load the cosmonaut-lite-theme."
  (interactive)
  (load-theme 'cosmonaut-lite t))

(provide-theme 'cosmonaut-lite)

;;; cosmonaut-lite-theme.el ends here
