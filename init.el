;; doesn't seem to contain the messages that are added to it. 
;; (setq initial-buffer-choice "*bootup-report*")
;; bootup report helper functions. 
(defun bootup/message (msg)
  (with-current-buffer (get-buffer-create "*bootup-report*")
    (end-of-buffer)
    (insert (concat msg "\n"))))


(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package t))

(setq-default use-package-verbose t)
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


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(inhibit-startup-buffer-menu t)
;;  '(inhibit-startup-screen t)
;;  '
;;  '(package-selected-packages
;;    '(db-pg markdown-mode cmake-mode toml-mode magit helm python-mode yaml-mode)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;; agenda
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; ;; should be conditional on machine
;; (setq org-directory "~/AppData/Roaming/agenda")
;; (setq org-default-notes-file "~/AppData/Roaming/agenda/refile.org")

;; (setq org-agenda-files
;;       '("~/AppData/Roaming/agenda/refile.org" "~/AppData/Roaming/agenda/projects.org" "~/AppData/Roaming/agenda/product_support.org" "~/AppData/Roaming/agenda/process_improvement.org" "~/AppData/Roaming/agenda/others.org" "~/AppData/Roaming/agenda/no_wip.org" "~/AppData/Roaming/agenda/notes.org" "~/AppData/Roaming/agenda/meetings.org" "~/AppData/Roaming/agenda/matlab.org" "~/AppData/Roaming/agenda/mab.org" "~/AppData/Roaming/agenda/lum.org" "~/AppData/Roaming/agenda/lpa.org" "~/AppData/Roaming/agenda/hed_target.org" "~/AppData/Roaming/agenda/altecu.org" "~/AppData/Roaming/agenda/axis_sim.org" "~/AppData/Roaming/agenda/cl455.org" "~/AppData/Roaming/agenda/code_review.org" "~/AppData/Roaming/agenda/contacts.org" "~/AppData/Roaming/agenda/email.org" "~/AppData/Roaming/agenda/exodus.org" "~/AppData/Roaming/agenda/genesis.org" "~/AppData/Roaming/agenda/reviews.org" "~/AppData/Roaming/agenda/simulink_builder.org" "~/AppData/Roaming/agenda/time_off.org" "~/AppData/Roaming/agenda/work.org"))

;; clocking

(setq org-log-done 'time)

(setq org-clock-into-drawer nil)

;; tabs and spaces formatting. 


(add-to-list 'load-path "~/.emacs.d/py_jira")

;; (require 'py_jira)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

;;; (quelpa-use-package quelpa utop alert flycheck flycheck-clang-tidy flycheck-rust emacsql-sqlite3 use-package forge helm lsp-mode racer tramp  neotree rust-mode yaml-mode magit lua-mode))
;; todo: define a minimum set of useful packages and an extended to reduce startup time. 
(use-package impatient-mode)
(use-package helm)

(use-package company)
(use-package smart-tabs-mode)



;; languages
(use-package cmake-mode)
(use-package markdown-mode)
(use-package lua-mode)

;; git related stuff.
(use-package magit)
(use-package forge
  :after magit)

;; (ghub-request "GET" "/user" nil
;; 	      :forge 'github
;; 	      :host "api.github.com"
;; 	      :username "brandonphelps"
;; 	      :auth 'forge)



;; eglot
(use-package eglot)

;; eglot c / c++ 



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
    (bootup/message "Succesfully found rustup")
  ;;(setq components (shell-command-to-string "rustup component list")))
  (bootup/message "Failed to find rustup"))

(if (executable-find "cargo")
    (bootup/message "Succesfully found cargo")
  (bootup/message "Failed to find cargo"))

(if (executable-find "python")
    (bootup/message "Succesfully found python")
  (bootup/message "Failed to find python"))
    



(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 )
  )

(use-package doom-themes)


;; (load-theme 'deeper-blue)
;; (load-theme 'tango-dark)
(load-theme 'doom-palenight)


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(auth-source-save-behavior nil)
;;  '(c-offsets-alist '((substatement-open . +) (case-label . 0)))
;;  '(custom-enabled-themes '(deeper-blue))
;;  '(send-mail-function 'mailclient-send-it)
;;  '(tramp-remote-process-environment
;;    '("ENV=''" "TMOUT=0" "LC_CTYPE=''" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat" "autocorrect=" "correct=")))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;; (require 'conan)

;; agenda stuff

(global-set-key (kbd "<f12>") 'org-agenda)
(setq org-fast-tag-selection-include-todo nil)


;; programming stuff

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

;; agenda stuff

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "OTHER(o)")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("OTHER" :foreground "forest green" :weight bold)))

;; todo: make this less os specific or something.
(setq org-default-notes-file "~/AppData/Roaming/agenda/refile.org")


;; refile handling

(setq org-refile-use-outline-path nil)
(setq org-refile-targets '((org-agenda-files :maxlevel . 8)))

;; remove clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;;; httpd-start
;;; httpd-serve-directory.
