;;; uses configurations from https://github.com/daviwil/emacs-from-scratch
(setq custom-file "~/.emacs.d/custom.el")

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


(defvar py_jira-dir (concat user-emacs-directory "py_jira"))

(when (file-directory-p py_jira-dir)
  (add-to-list 'load-path "~/.emacs.d/py_jira")
  (require 'py_jira))


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
;; do this on some sort of daily or weekly time point?
;; such that melpa and stuff could still be reachable if not used in a long time
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-verbose t)
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package yaml-mode)

;; UI layout stuff. 
(use-package doom-themes)
(load-theme 'doom-palenight t)


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

(when (file-directory-p "py_jira")
  (message "loading up py jira")
  (add-to-list 'load-path "~/.emacs.d/py_jira")
  (require 'py_jira))

(use-package rainbow-delimiters)
(use-package no-littering)

;; hmm load user specifics customizations late or early? 
(when (file-readable-p machine-settings-file)
  (load-file machine-settings-file))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package cargo)

;; languages
(use-package cmake-mode)
(use-package lua-mode)
(use-package rust-mode)

(add-hook 'rust-mode-hook
	  (lambda () (setq indent-tabs-mode nil)))


;; git related stuff.
(use-package magit
  :commands magit-status
  :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

;;; lsp mode
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :hook ((rust-mode . lsp)
	 (lsp-mode . efs/lsp-mode-setup)
	 )
  :config
    (lsp-enable-which-key-integration t))

(use-package lsp-ui)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.2))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs 
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
	 :request "launch"
	 :name "LLDB::Run"
	 :gdbpath "rust-lldb"
	 :target nil
	 :cwd nil)))

;; (use-package dap-mode)
;; (require 'dap-gdb-lldb)
;; (dap-register-debug-template "Rust::GDB Run Configuration"
;;                              (list :type "gdb"
;;                                    :request "launch"
;;                                    :name "GDB::Run"
;;                            :gdbpath "rust-gdb"
;;                                    :target nil
;;                                    :cwd nil))
;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

;; Todo; check if rustup components are installed.
;; rls needs , rls, rust-src rust-analysis 

(if (executable-find "clangd")
    (bootup/message "Successfully found clangd")
  (bootup/message "Failed to find clangd"))

;; Rust handling. 
(if (executable-find "rustup")
    (progn
      (use-package rust-mode)
      (use-package ob-rust)
      (bootup/message "Succesfully found rustup"))
  ;;(setq components (shell-command-to-string "rustup component list")))
  (bootup/message "Failed to find rustup"))

(if (executable-find "cargo")
    (bootup/message "Succesfully found cargo")
  (bootup/message "Failed to find cargo"))

(if (executable-find "python")
    (bootup/message "Succesfully found python")
  (bootup/message "Failed to find python"))

;; replaced with vertico
;; (use-package counsel
;;   :bind (("M-x" . counsel-M-x)
;; 	 ("C-x b" . counsel-ibuffer)
;; 	 ("C-x C-f" . counsel-find-file)
;; 	 )
;;   )

;; todo: how to check this only for if emacs is launched with gui.
(use-package doom-themes
  :init (load-theme 'doom-palenight t))

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

(defun my-c-mode-hook ()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
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

(defun bp/org-mode-setup ()
  (org-indent-mode))

;; org mode
(use-package org
  :hook bp/org-mode-setup
  :config
  (setq org-ellipsis " ↓"))


(use-package org-roam)
;; todo move this to box specifics
(setq org-roam-directory "~/org-roam")

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defvar bp-org-babel-languages
  '((emacs-lisp . t)
    (python . t)))

(setq bp-org-babel-languages '((emacs-lisp . t) (python . t)))


(setq backup-directory '(("." . ,(expand-file-name "tmp/backups" user-emacs-directory))))


(defun rscript ()
  (interactive)
  (message (shell-command-to-string (concat "rust-script.exe " (buffer-file-name)))))

;; Rust handling. 
(when (and (executable-find "rustup") (executable-find "rust-script"))
  ;; how to only add this once? 
  (push '(rust . t) bp-org-babel-languages))

;; do we care about babel stuff? 
(org-babel-do-load-languages
 'org-babel-load-languages bp-org-babel-languages)

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))

;; agenda stuff
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

    
;; ;; todo: make this less os specific or something.
;; (setq org-default-notes-file "~/AppData/Roaming/agenda/refile.org")

;; ;; should be conditional on machine
;; (setq org-directory "~/AppData/Roaming/agenda")
;; (setq org-default-notes-file "~/AppData/Roaming/agenda/refile.org")


;; clocking
(setq org-log-done 'time)

(setq org-clock-into-drawer nil)


(global-set-key (kbd "<f12>") 'org-agenda)
(setq org-fast-tag-selection-include-todo nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "OTHER(o)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("OTHER" :foreground "forest green" :weight bold)))


(setq org-capture-templates
      (quote (("t" "todo" entry (file org-default-notes-file)
	       "* TODO %?\n" :clock-in t :clock-resume t))))

;; refile handling
(setq org-refile-use-outline-path nil)
(setq org-refile-targets '((org-agenda-files :maxlevel . 8)))

;; remove clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; (smart-tabs-insinuate 'c 'javascript)
;; httpd-start
;; httpd-serve-directory.

;; https://www.youtube.com/watch?v=_ZyD4n5zqxA
(defun bp/vid-dl (user_url)
  (interactive "sURL: ")
  (if (not (boundp 'video-dir))
      (message "video dir not found")
    
    (let ((url user_url) (default-directory video-dir))
      (async-start
       ;; What to do in the child process
       `(lambda ()
	  (message ,(concat " downloading: " url " to " default-directory))
	  (shell-command-to-string ,(concat "youtube-dl " url))
	  ,(format "This is a %s" url)
	  )

       ;; What to do when it finishes
       (lambda (r)
	 (message "Async process done, result should be 222: %s" r))))))



