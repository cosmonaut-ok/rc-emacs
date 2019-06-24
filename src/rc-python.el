;; (require 'python-mode)

;; fix unexpected emacs behavior with "//" in pathname
;; (setq-default py-install-directory (replace-regexp-in-string "\\/$" "" py-install-directory))

;; ; use IPython
;; (setq-default py-shell-name "jupyter")
;; (setq-default py-which-bufname "jupyter console")
;; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;;       '("console -i --simple-prompt"))

;; (setq py-force-py-shell-name-p t)

;; ; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
;; (setq py-switch-buffers-on-execute-p t)
; don't split windows
;; (setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
;; (setq py-smart-indentation t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(mode-require-final-newline t)
 ;; '(py--imenu-create-index-p t)
 ;; '(py-auto-complete-p t)
 ;; '(py-auto-fill-mode t)
 ;; '(py-autofill-timer-delay 1)
 ;; '(py-autopair-mode t)
 ;; '(py-company-pycomplete-p t)
 ;; '(py-electric-colon-newline-and-indent-p t)
 ;; '(py-electric-comment-add-space-p t)
 ;; '(py-electric-comment-p t)
 ;; '(py-fast-completion-delay 0.2)
 ;; '(py-fast-process-p t)
 ;; '(py-hide-show-minor-mode-p t)
 ;; '(py-highlight-error-source-p t)
 ;; '(py-ipython-command "jupyter")
 ;; '(py-ipython-command-args "console -i --simple-prompt")
 ;; '(py-ipython-execute-delay 0.5)
 ;; '(py-load-pymacs-p t)
 ;; '(py-load-skeletons-p t)
)

(defhooklet cosmonaut/python-company python-mode t
  ;; (company-mode 1)
  (add-to-list 'company-backends 'company-jedi)
  )

