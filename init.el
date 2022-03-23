
(setq-default fill-column 90)



;; straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-check-for-modifications 'live)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; look into this https://gitlab.com/jgkamat/rmsbolt
;;; uses configurations from https://github.com/daviwil/emacs-from-scratch
(setq custom-file "~/.emacs.d/custom.el")

(if (executable-find "pianobar")
    (use-package pianobar))
;; doesn't seem to contain the messages that are added to it. 
;; (setq initial-buffer-choice "*bootup-report*")
;; bootup report helper functions. 
(defun bootup/message (msg)
  (with-current-buffer (get-buffer-create "*bootup-report*")
    (end-of-buffer)
    (insert (concat msg "\n"))))

(use-package ripgrep)

(defvar machine-settings-file
  (concat user-emacs-directory "box-specifics/" (downcase system-name) ".el")
  "Settings file for the box we are currently on")

; for elfeed.
;; https://fasterthanli.me/index.xml

(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)



;; basic UI setup. 
(when (display-graphic-p)
 (scroll-bar-mode -1)
 (set-fringe-mode 10)
 )
(tool-bar-mode -1)
(tooltip-mode -1)

(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)
(setq ring-bell-function 'ignore)

(use-package org-roam
  :custom
  (org-roam-directory "~/roam-notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(setq org-roam-v2-act t)

(use-package flyspell
  :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode)))
    


(use-package rainbow-delimiters)
(use-package ag)
(use-package no-littering)
(use-package yaml-mode)
(use-package ag)


;; hmm load user specifics customizations late or early? 
(when (file-readable-p machine-settings-file)
  (load-file machine-settings-file))

(load-file (concat user-emacs-directory "programming.el"))
(load-file (concat user-emacs-directory "agenda.el"))


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

;; todo: how to check this only for if emacs is launched with gui.
;; UI layout stuff. 

(if (display-graphic-p) 
    (use-package doom-themes
      :init (load-theme 'doom-palenight t))
  (use-package doom-themes
;;    :init (load-theme 'doom-challenger-deep t)))
    :init (load-theme 'tsdh-dark t)))
;;    :init (load-theme 'doom-Iosvkem t)))  something about this doesn't work well with agenda. 

;; (load-theme 'deeper-blue)
;; (load-theme 'tango-dark)
;;   :init (load-theme 'doom-Iosvkem t))

(when (boundp 'bp-default-project-path)
  (message "%s" bp-default-project-path))

(use-package ivy)



(use-package projectile
  :diminish
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; (when (boundp 'bp-default-project-path)
  ;;     (setq projectile-project-search-path '(bp-default-project-path)))
  (setq projectile-switch-project-action #'projectile-dired))

;(projectile-register-project-type
; 'conan '("conanfile.py" "CMakeLists.txt")
; :project-file "conanfile.py"
; :compile "conan install . -if build -b missing"
; :run "conan build . -bf build"
; )

;; (load-file (concat user-emacs-directory "custom_projectile.el"))

(use-package helpful)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
    (setq which-key-idle-delay 1))

;; mail server stuff
(setq send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   smtpmail-starttls-credentials
   '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
   (expand-file-name "~/.authinfo")
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587
   smtpmail-debug-info t
   starttls-extra-arguments nil
   smtpmail-warn-about-unknown-extensions t
   starttls-use-gnutls nil)


;; (global-set-key (kbd "C-c i") 'windmove-up)
;; (global-set-key (kbd "C-c k") 'windmove-down)
;; (global-set-key (kbd "C-c j") 'windmove-left)
;; (global-set-key (kbd "C-c l") 'windmove-right)

(use-package markdown-mode)
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
    (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
  (current-buffer)))


(setq backup-directory '(("." . ,(expand-file-name "tmp/backups" user-emacs-directory))))

(defun rscript ()
  (interactive)
  (message (shell-command-to-string (concat "rust-script.exe " (buffer-file-name)))))

;; gpg helper funcs
(defun efs/lookup-password (&rest keys)
 (let ((result (apply #'auth-source-search keys)))
   (if result
	(funcall (plist-get (car result) :secret))
     nil)))


(require 'tramp)
