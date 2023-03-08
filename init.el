
(setq-default fill-column 90)
(setq custom-file "~/.emacs.d/custom.el")

(load-file "~/.emacs.d/elpaca-bootstrap.el")

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)
(use-package org)
(elpaca-wait)


;; basic UI setup. 
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq ring-bell-function #'ignore)

(when (display-graphic-p)
 (scroll-bar-mode -1)
 (set-fringe-mode 15)
 )
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; movement helpers. 
(global-set-key (kbd "C-c i") 'windmove-up)
(global-set-key (kbd "C-c k") 'windmove-down)
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)

;; UI Setup
(if (display-graphic-p) 
    (use-package doom-themes
      :init (load-theme 'doom-palenight t))
  (use-package doom-themes
    :init (load-theme 'tsdh-dark t)))
;; (M-x customize-themes)


;; utility packages
(use-package rainbow-delimiters)
(use-package no-littering)
(use-package helpful)
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
    (setq which-key-idle-delay 1))


;; Programming stuff. 
(use-package rust-mode)
(add-hook 'rust-mode-hook
	  (lambda () (setq indent-tabs-mode nil)
	    ))

(use-package cargo)
(use-package ripgrep)
(use-package yaml-mode)
(use-package cmake-mode)
(use-package toml-mode)


;;; lsp mode
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :hook ((rust-mode . lsp)
	 (python-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (lsp-mode . efs/lsp-mode-setup)
	 )
  :config
  (setq lsp-signature-auto-activate nil))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

;; c/c++
(defun my-c-mode-hook ()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Color handling for compilation

(add-hook 'shell-mode-hook 'ansi-color-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


(setq compilation-scroll-output t
      compilation-window-height 20)

;; Git related

(use-package magit
  :commands magit-status
  :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package git-timemachine)
(use-package forge
  :after magit)

;; Projectil setup
(use-package projectile
  :diminish
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; (when (boundp 'bp-default-project-path)
  ;;     (setq projectile-project-search-path '(bp-default-project-path)))
  (setq projectile-switch-project-action #'projectile-dired))

;; ;(projectile-register-project-type
;; ; 'conan '("conanfile.py" "CMakeLists.txt")
;; ; :project-file "conanfile.py"
;; ; :compile "conan install . -if build -b missing"
;; ; :run "conan build . -bf build"
;; ; )

(use-package vertico
  :init
  (vertico-mode))

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; org roam note taking

(use-package org-roam
  :custom
  (org-roam-directory "~/roam-notes")
  (org-roam-v2-act t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  )

(load-file (concat user-emacs-directory "/agenda.el"))

(defvar machine-settings-file
  (concat user-emacs-directory "box-specifics/" (downcase system-name) ".el")
  "Settings file for the box we are currently on")

(when (file-exists-p machine-settings-file)
  (load-file machine-settings-file))


;; testing uility stuff.

(use-package ascii-table)




;; (defun dired-run-at-point ()
;;   (interactive)
;;   (let ((process (dired-file-name-at-point)))
;;     (async-start-process (file-name-base process) process '(lambda (arg)))))

;; ;; look into this https://gitlab.com/jgkamat/rmsbolt
;; ;;; uses configurations from https://github.com/daviwil/emacs-from-scratch

;; (if (executable-find "pianobar")
;;     (use-package pianobar))
;; ;; doesn't seem to contain the messages that are added to it. 
;; ;; (setq initial-buffer-choice "*bootup-report*")
;; ;; bootup report helper functions. 
;; (defun bootup/message (msg)
;;   (with-current-buffer (get-buffer-create "*bootup-report*")
;;     (end-of-buffer)
;;     (insert (concat msg "\n"))))

;; (use-package elfeed
;;   :custom
;;   (when (boundp elfeed-my-custom-feeds)
;;     (elfeed-feeds
;;      elfeed-my-custom-feeds))
;;   (elfeed-sort-order 'ascending)
;;   )

;; (defun elfeed-eww-open (&optional use-generic-p)
;;   "open with eww"
;;   (interactive "P")
;;   (let ((entries (elfeed-search-selected)))
;;     (cl-loop for entry in entries
;; 	     do (elfeed-untag entry 'unread)
;; 	     when (elfeed-entry-link entry)
;; 	     do (eww-browse-url it))
;;     (mapc #'elfeed-search-update-entry entries)
;;     (unless (use-region-p) (forward-line))))

;; ;; (defun elfeed-w3m-open (&optional use-generic-p)
;; ;;   "open with w3m"
;; ;;   (interactive "P")
;; ;;   (let ((entries (elfeed-search-selected)))
;; ;;     (cl-loop for entry in entries
;; ;;              do (elfeed-untag entry 'unread)
;; ;;              when (elfeed-entry-link entry)
;; ;;              do (w3m-browse-url it))
;; ;;     (mapc #'elfeed-search-update-entry entries)
;; ;;     (unless (use-region-p) (forward-line))))


;; ;; (define-key elfeed-search-mode-map (kbd "t") 'elfeed-w3m-open)
;; (define-key elfeed-search-mode-map (kbd "w") 'elfeed-eww-open)

;; (use-package flyspell
;;   :defer t
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;     (add-hook 'text-mode-hook 'flyspell-mode)))


;; ;; hmm load user specifics customizations late or early? 
;; (when (file-readable-p machine-settings-file)
;;   (load-file machine-settings-file))


;; (use-package consult
;;   :demand t
;;   :bind (;;("C-s" . consult-line)
;; 	 ("C-M-l" . consult-imenu)
;; 	 ("C-x b" . consult-buffer)
;; 	 :map minibuffer-local-map
;; 	 ("C-r" . consult-history)
;; 	 )
;;   :custom
;;   (consult-project-root-function #'projectile-project-root)
;;   (completion-in-region-function #'consult-completion-in-region)
;;   )


;; (when (boundp 'bp-default-project-path)
;;   (message "%s" bp-default-project-path))


;; ;; mail server stuff
;; (setq send-mail-function 'smtpmail-send-it
;;    message-send-mail-function 'smtpmail-send-it
;;    smtpmail-starttls-credentials
;;    '(("smtp.gmail.com" 587 nil nil))
;;    smtpmail-auth-credentials
;;    (expand-file-name "~/.authinfo")
;;    smtpmail-default-smtp-server "smtp.gmail.com"
;;    smtpmail-smtp-server "smtp.gmail.com"
;;    smtpmail-smtp-service 587
;;    smtpmail-debug-info t
;;    starttls-extra-arguments nil
;;    smtpmail-warn-about-unknown-extensions t
;;    starttls-use-gnutls nil)


;; (use-package markdown-mode)
;; (defun markdown-html (buffer)
;;   (princ (with-current-buffer buffer
;;     (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
;;   (current-buffer)))

;; (setq backup-directory '(("." . ,(expand-file-name "tmp/backups" user-emacs-directory))))

;; (defun rscript ()
;;   (interactive)
;;   (message (shell-command-to-string (concat "rust-script.exe " (buffer-file-name)))))

;; ;; gpg helper funcs
;; (defun efs/lookup-password (&rest keys)
;;  (let ((result (apply #'auth-source-search keys)))
;;    (if result
;; 	(funcall (plist-get (car result) :secret))
;;      nil)))

;; (add-to-list 'tramp-connection-properties
;;              (list (regexp-quote "/ssh:aiur:")
;;                    "remote-shell" "/usr/bin/bash"))

