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

;;; uses configurations from https://github.com/daviwil/emacs-from-scratch
(setq custom-file "~/.emacs.d/custom.el")

;; look into this https://gitlab.com/jgkamat/rmsbolt



;; doesn't seem to contain the messages that are added to it. 
;; (setq initial-buffer-choice "*bootup-report*")
;; bootup report helper functions. 
(defun bootup/message (msg)
  (with-current-buffer (get-buffer-create "*bootup-report*")
    (end-of-buffer)
    (insert (concat msg "\n"))))


(defvar machine-settings-file
  (concat user-emacs-directory "box-specifics/" (downcase system-name) ".el")
  "Settings file for the box we are currently on")

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


(defvar py_jira-dir (concat user-emacs-directory "py_jira"))

(when (file-directory-p py_jira-dir)
  (add-to-list 'load-path "~/.emacs.d/py_jira")
  (require 'py_jira))

(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)

;; basic UI setup. 
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)
(setq ring-bell-function 'ignore)

(column-number-mode)
(global-display-line-numbers-mode t)


(use-package rainbow-delimiters)
(use-package no-littering)

;; hmm load user specifics customizations late or early? 
(when (file-readable-p machine-settings-file)
  (load-file machine-settings-file))

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


;; git related stuff.
(use-package magit
  :commands magit-status
  :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package ivy)


;; todo: how to check this only for if emacs is launched with gui.
;; UI layout stuff. 
(if (display-graphic-p)  ;; if part
  (use-package doom-themes
    :init (load-theme 'doom-palenight t)) ;; then-part
  (use-package zenburn-theme
     :init (load-theme 'zenburn t)))

;; (load-theme 'deeper-blue)
;; (load-theme 'tango-dark)
;;   :init (load-theme 'doom-Iosvkem t))

(use-package projectile
  :diminish
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; (when (boundp 'bp-default-project-path)
  ;;   (setq projectile-project-search-path '(bp-default-project-path)))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package helpful)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
    (setq which-key-idle-delay 1))

;; (use-package which-key
;;   :init (which-key-mode)
;;   :diminish which-key-mode
;;   :config
;;   (setq which-key-idle-delay 1))

;; (smart-tabs-insinuate 'c 'c++ 'python)

;; compliation mode coloring 
(require 'ansi-color)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq c-default-style '((c-mode . "linux") (c++-mode . "linux")))

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

;; Rust handling. 
(when (and (executable-find "rustup") (executable-find "rust-script"))
  ;; how to only add this once? 
  (push '(rust . t) bp-org-babel-languages))

    
;; ;; todo: make this less os specific or something.
;; (setq org-default-notes-file "~/AppData/Roaming/agenda/refile.org")

;; ;; should be conditional on machine
;; (setq org-directory "~/AppData/Roaming/agenda")
;; (setq org-default-notes-file "~/AppData/Roaming/agenda/refile.org")

;; gpg helper funcs
(defun efs/lookup-password (&rest keys)
  (let ((result (apply #'auth-source-search keys)))
    (if result
	(funcall (plist-get (car result) :secret))
      nil)))


(when (file-directory-p "py_jira")
  (message "loading up py jira")
  (add-to-list 'load-path "~/.emacs.d/py_jira")
  (require 'py_jira))
