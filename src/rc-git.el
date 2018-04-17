;; TODO:

;; default auth is password-based
(setq-default gh-api-v3-authenticator 'gh-password-authenticator)

(defun gh-create-oauth-token ()
  (interactive)
  (browse-url "https://github.com/settings/tokens/new"))

(defun gh-auth-set-credentials (username password)
  (interactive (list
		(read-string "Input your github username: ")
		(read-passwd "Input your github password: " t)))
  (if (shell-command (concat "git config --global github.user " username " && git config --global github.password " password))
      (message "Github user added")
    (message "Failed to add Github user")))

(defun gh-auth-set-oauth-token (token)
  (interactive "sInput GitHub Api v3 Authenticator: ")
  (if (shell-command (concat "git config --global github.oauth-token " token))
      (message "Github auth token added")
    (message "Failed to add Github auth token")))

;; add G to git status map for github pulls menu
(define-key magit-status-mode-map (kbd "G") 'magit-gh-pulls-popup)

;; magit
;; github-browse-file.el
;; github-clone.el
;; github-notifier.el
;; magit-gh-pulls.el
;; yagist
