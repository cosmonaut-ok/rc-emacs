;;; cosmonaut-classic-theme.el --- dark & minty fresh theme

;; Copyright (C) 2016 by Shaun Viguerie


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(deftheme cosmonaut-classic
  "cosmonaut-classic-theme")

(custom-theme-set-variables
 'cosmonaut-classic
 '(blink-cursor-mode 1)
 '(scroll-bar-mode -1)
 '(global-font-lock-mode t)
 '(scalable-fonts-allowed t)
 '(uniquify-buffer-name-style (quote forward))
 '(use-dialog-box -1)
 '(tool-bar-mode 1)
 '(menu-bar-mode 1)
 '(frame-background-mode 'dark)
 ;; We have CPU to spare; highlight all syntax categories.
 '(font-lock-maximum-decoration t)
 '(custom-buffer-done-kill t)
 ;; '(transient-mark-mode t)
 '(scroll-step 1) ;; Line-by-Line Scrolling
 )

(custom-theme-set-faces
 'cosmonaut-classic
 
 '(default ((t (:background "gray7" :foreground "green"))))
 '(mouse ((t (:foregound "goldenrod"))))
 '(fringe ((t (:background "black" :foreground "green"))))
 '(cursor ((t (:background "red" :foreground "black"))))
 '(border ((t (:foregound "blue"))))


 '(mode-line ((t (:background "black" :foreground "green"))))
 '(Man-overstrike-face ((t (:weight bold))))
 '(Man-underline-face ((t (:underline t))))
 '(apropos-keybinding-face ((t (:underline t))))
 '(apropos-label-face ((t (:italic t))))

 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 ;; '(font-lock-type-face ((t (:foreground "PaleGreen"))))
 '(font-lock-type-face ((t (:foreground "light slate blue"))))
 '(font-lock-comment-face ((t (:foreground "OrangeRed"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "cyan"))))
 '(font-lock-string-face ((t (:foreground "LightSalmon"))))
 ;; '(font-lock-string-face ((t (:foreground "deep pink"))))
 '(font-lock-doc-string-face ((t (:foreground "Orange")))) ;; LightSalmon
 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue")))) ;;LightSkyBlue
 ;; '(font-lock-function-name-face ((t (:foreground "VioletRed2"))))
 '(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
 ;; '(font-lock-variable-name-face ((t (:foreground "MediumPurple1"))))
 '(font-lock-constant-face ((t (:foreground "Aquamarine"))))
 '(font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
 '(font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))

 '(region ((t (:background "MidnightBlue"))))
 '(secondary-selection ((t (:background "dodger blue"))))
 
 '(mouse ((t (:foregound "goldenrod"))))
 '(highlight ((t (:background "blue" :foreground "orange"))))
 '(show-paren-match-face ((t (:background "turquoise" :foreground "coral"))))
 '(show-paren-mismatch-face ((t (:background "purple" :foreground "white"))))
 '(cursor ((t (:background "red"))))

 '(linum ((t (:background "gray7" :foreground "green"))))

 ;; flymake
 '(flymake-errline ((t (:inherit (default) :box (:line-width 2 :color "Dark Red")))))
 '(flymake-warnline ((t (:inherit (font-lock-warning-face)))))
  
 ;; hl-line
 '(hl-line ((t (:background "#130061"))))
 
 ;; col-highlight
 '(col-highlight ((t (:background "#040017"))))
 
 ;; ECB
 '(ecb-default-highlight-face ((t (:background "dark slate blue"))))
 
 ;; mode-line
 '(mode-line ((t (:background "grey7" :foreground "#96CBFE"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "#1d1f21" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "#7c7c7c" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))

 ;; isearch
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 
 ;; ido-mode
 '(ido-first-match ((t (:foreground "violet" :weight bold))))
 '(ido-only-match ((t (:foreground "#ff982d" :weight bold))))
 '(ido-subdir ((t (:foreground "#8AE234"))))
 '(ido-virtual ((t (:foreground "#7c7c7c"))))

 ;; diff-hl (https://github.com/dgutov/diff-hl)
 '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
 '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
 '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

 ;; dired-mode
 '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
 '(dired-flagged ((t (:inherit (diff-hl-delete)))))
 '(dired-symlink ((t (:foreground "#FD5FF1"))))

 ;; guide-key (https://github.com/kai2nenobu/guide-key)
 '(guide-key/highlight-command-face ((t (:inherit (cursor)))))
 '(guide-key/key-face ((t (:inherit (font-lock-warning-face)))))
 '(guide-key/prefix-command-face ((t (:inherit (font-lock-keyword-face)))))

 ;; flx-ido (https://github.com/lewang/flx)
 '(flx-highlight-face ((t (:inherit (link) :weight bold))))

 ;; markdown-mode (http://jblevins.org/projects/markdown-mode/)
 ;;
 ;; Note: Atom Dark Theme for Atom.io does not currently theme some things that markdown-mode does.  For cases where
 ;;       Atom.io does not provide theming, this theme will leave the theming done by markdown-mode as-is.  Where both
 ;;       Atom.io and markdown-mode provide theming, markdown-mode's theming will be changed to match that of Atom.io.
 '(markdown-blockquote-face ((t :foreground "#555")))
 '(markdown-header-face ((t :foreground "#eee")))
 '(markdown-header-delimiter-face ((t (:inherit (markdown-header-face)))))
 '(markdown-header-rule-face ((t (:inherit (font-lock-comment-face)))))

 ;; Js2-mode (https://github.com/mooz/js2-mode)
 '(js2-error ((t (:foreground "#c00"))))
 '(js2-external-variable ((t (:inherit (font-lock-builtin-face)))))
 '(js2-function-param ((t (:foreground "#C6C5FE"))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "#96CBFE"))))
 '(js2-jsdoc-html-tag-name ((t (:foreground "#96CBFE"))))
 '(js2-jsdoc-tag ((t (:inherit (font-lock-doc-face):weight bold))))
 '(js2-jsdoc-type ((t (:inherit (font-lock-type-face)))))
 '(js2-jsdoc-value ((t (:inherit (js2-function-param)))))

 ;; eruby-mode (https://github.com/petere/emacs-eruby-mode)
'(eruby-standard-face ((t (:inherit (default) :box (:line-width 2 :color "dark slate blue")))))
'(eruby-standard-face ((t (:inherit (default) :background "black" :box (:line-width 2 :color "dark slate blue")))))
 ;; '(eruby-comment-face ((t (:inherit font-lock-comment-face :background "gray20"))))
 ;; '(eruby-standard-face ((t (:background "dark slate blue"))))

 ;; minimap (https://github.com/dengste/minimap)
 '(minimap-active-region-background ((t (:inherit (highlight)))))

 ;; powerline (https://github.com/milkypostman/powerline)
 '(powerline-active2 ((t (:background "grey10"))))

 ;; speedbar
 '(speedbar-button-face ((t (:foreground "#AAAAAA"))))
 '(speedbar-directory-face ((t (:inherit (font-lock-keyword-face)))))
 '(speedbar-file-face ((t (:inherit (default)))))
 '(speedbar-highlight-face ((t (:inherit (highlight)))))
 '(speedbar-selected-face ((t (:background "#4182C4" :foreground "#FFFFFF"))))
 '(speedbar-separator-face ((t (:background "grey11" :foreground "#C5C8C6" :overline "#7C7C7C"))))
 '(speedbar-tag-face ((t (:inherit (font-lock-function-name-face)))))

 ;; whitespace
 '(whitespace-empty ((t (:foreground "#333333"))))
 '(whitespace-hspace ((t (:inherit (whitespace-empty)))))
 '(whitespace-indentation ((t (:inherit (whitespace-empty)))))
 '(whitespace-line ((t (:inherit (trailing-whitespace)))))
 '(whitespace-newline ((t (:inherit (whitespace-empty)))))
 '(whitespace-space ((t (:inherit (whitespace-empty)))))
 '(whitespace-space-after-tab ((t (:inherit (whitespace-empty)))))
 '(whitespace-space-before-tab ((t (:inherit (whitespace-empty)))))
 '(whitespace-tab ((t (:inherit (whitespace-empty)))))
 '(whitespace-trailing ((t (:inherit (trailing-whitespace)))))

 ;; hideshow
 '(hs-face ((t (:background "RoyalBlue4" :box 1))))
 )

(defvar cosmonaut-classic-theme-force-faces-for-mode t
  "If t, cosmonaut-classic-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Atom Dark
Theme from Atom.io as best as possible.

The reason this is required is because some modes (html-mode, yaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.

Current modes, and their faces, impacted by this variable:

* html-mode: font-lock-variable-name-face
* markdown-mode: default
* yaml-mode: font-lock-variable-name-face
")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Atom Dark Theme" for Emacs, we need to work around this so that these themes look
;; as much like "Atom Dark Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `cosmonaut-classic-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
(defun cosmonaut-classic-theme-change-faces-for-mode ()
  (interactive)
  (and (eq cosmonaut-classic-theme-force-faces-for-mode t)
       (cond
        ((member major-mode '(conf-mode conf-javaprop-mode html-mode yaml-mode))
         (face-remap-add-relative 'font-lock-variable-name-face '(:inherit (font-lock-keyword-face))))
        ((eq major-mode 'java-mode)
         (face-remap-add-relative 'font-lock-variable-name-face '(:inherit (js2-function-param))))
        ((eq major-mode 'markdown-mode)
         (face-remap-add-relative 'default '(:foreground "#999")))
        ((member major-mode '(javascript-mode js2-mode))
         (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))))))

(add-hook 'after-change-major-mode-hook 'cosmonaut-classic-theme-change-faces-for-mode)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'cosmonaut-classic)
;;; cosmonaut-classic-theme.el ends here
