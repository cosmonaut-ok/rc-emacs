;; check if ruby installed

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (not (call-process "which" nil nil nil "ruby"))
  (warn "WARNING! There is no ruby in system. Extended ruby/chef features are not supported"))

(defvar cosmonaut/source-directory (file-name-directory load-file-name))

(defvar cosmonaut/list-load-components
  '("lib" "custom" "common" "tabbar" "git" "yasnippet" "company"
    "python" "ruby" "rspec" "chef" "kitchen" "bundler" "berkshelf"
    "foodcritic" "rvm" "codebrowser" "markdown"
    "yaml" "json" "web" "erb" "fly" "popup-menu"
    "lisp" "c-pp" "makefile" "org" "tex"
    "shell-script" "menubar" "toolbar" "theme"
    "help"))

;; loading initial user-directories file
(let ((ud-file (concat cosmonaut/source-directory "src/rc-user-directories.el")))
  (if noninteractive
      (progn (load ud-file) (byte-compile-file ud-file))
    (if (file-exists-p (concat ud-file "c")) ;; searching for elc files
	(load (concat ud-file "c"))
      (load ud-file))))

(defvar cosmonaut/packages-installed-p (locate-source-file "build"))

(load (locate-source-file "src/bootstrap.el"))
(require 'el-get)
(el-get)

;; set init and custom file
(mkdir cosmonaut/user-config-directory t)

(setq user-init-file (locate-source-file "init.el")
      user-emacs-directory cosmonaut/user-config-directory
      custom-file (locate-user-config-file "rc.el")
      local-file (locate-user-config-file "rc.el"))
;; move semanticDB, srecode and ede to cache
(custom-set-variables
 '(ede-project-placeholder-cache-file (locate-user-cache-file "cosmonaut-ede-projects.el"))
 '(semanticdb-default-save-directory (locate-user-cache-file "cosmonaut-semanticdb"))
 '(srecode-map-save-file (locate-user-cache-file "cosmonaut-srecode-map.el"))
 ;; '(ido-save-directory-list-file (locate-user-cache-file "ido.last"))
 )

;; create custom file if it does not exists
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; load rc files
(dolist (file cosmonaut/list-load-components)
  (let* ((base (locate-source-file (concat "src" "/rc-" file)))
	 (elc-file (concat base ".elc"))
	 (el-file (concat base ".el")))
    (if noninteractive
	(progn
	  (load el-file)
	  (byte-compile-file el-file))
      (if (file-exists-p elc-file)
	  (load elc-file)
	(load el-file)))))

(when (file-exists-p local-file)
  (load local-file))

(when (not noninteractive)
  ;; enable default theme, if still none set
  (when (not custom-enabled-themes)
    (load-theme 'cosmonaut-atomic t))
  (message "Wellcome to the Cosmonaut. Please, choose your dishes from menu. Right click for Appetizer"))
;;; init.el ends here
