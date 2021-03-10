;;; uses configurations from https://github.com/daviwil/emacs-from-scratch

;; doesn't seem to contain the messages that are added to it. 
;; (setq initial-buffer-choice "*bootup-report*")
;; bootup report helper functions. 
(defun bootup/message (msg)
  (with-current-buffer (get-buffer-create "*bootup-report*")
    (end-of-buffer)
    (insert (concat msg "\n"))))

(defvar machine-settings-file
  (concat user-emacs-directory "box-specifics/" (downcase system-name))
  "Settings file for the box we are currently on")

(defvar py_jira-dir (concat user-emacs-directory "py_jira"))

(when (file-directory-p py_jira-dir)
  (add-to-list 'load-path "~/.emacs.d/py_jira")
  (require 'py_jira))


;; set the custom file so emacs customization stuff goes there instead of here. 
(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; do this on some sort of daily or weekly time point?
;; such that melpa and stuff could still be reachable if not used in a long time
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package t))

(setq-default use-package-verbose t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)

(require 'use-package)
(setq use-package-always-ensure t)

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


;; keybindings and auto complete stuff
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 )
  )

(use-package rainbow-delimiters)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.4))


;; hmm load user specifics customizations late or early? 
(when (file-readable-p machine-settings-file)
  (load-file machine-settings-file))

(use-package impatient-mode)
(use-package helm)

(use-package smart-tabs-mode)

;; languages
(use-package cmake-mode)
(use-package markdown-mode)
(use-package lua-mode)

;; git related stuff.
(use-package magit)
(use-package forge
  :after magit)





;; eglot
;; (use-package eglot)

;; eglot c / c++ 

;;; lsp mode

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)

  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'

  :hook ((rust-mode . lsp)
	 (lsp-mode . efs/lsp-mode-setup)
	 )
  :config
  (lsp-enable-which-key-integration t))


(use-package lsp-ui)

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



;; silence the bell
(setq ring-bell-function 'ignore)


(use-package projectile
  :diminish
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (boundp 'bp-default-project-path)
    (setq projectile-project-search-path '(bp-default-project-path)))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package helpful)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key
  :config
  (setq which-key-idle-delay 1))
;; (require 'conan)


;; (smart-tabs-insinuate 'c 'c++ 'python)

;; compliation mode coloring 
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; (setq c-default-style "bsd")
;; (setq-default c-basic-offset 2)
;; (c-set-offset 'case-label '+)
;; (ido-mode t)

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

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defvar bp-org-babel-languages
  '((emacs-lisp . t)
    (python . t)))

(setq bp-org-babel-languages '((emacs-lisp . t) (python . t)))



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


(defun am_on_poxy ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package-20210106.2145")
  (add-to-list 'load-path "~/.emacs.d/elpa/bind-key-20200805.1727")
  (add-to-list 'load-path "~/.emacs.d/elpa/rust-mode")
  (add-to-list 'load-path "~/.emacs.d/elpa/cargo.el")
  (add-to-list 'load-path "~/.emacs.d/elpa/markdown-mode")
  (require 'use-package)
  (require 'rust-mode)
  (require 'cargo)
  )







