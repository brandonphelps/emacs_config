

;; ;; 'health checks'
;; ;; under development. 
;; ;; (if (executable-find "clangd")
;; ;;     (bootup/message "Successfully found clangd")
;; ;;   (bootup/message "Failed to find clangd"))

;; ;; ;; Rust handling. 
;; ;; (if (executable-find "rustup")
;; ;;     (progn
;; ;;       (use-package rust-mode)
;; ;;       (use-package ob-rust)
;; ;;       (bootup/message "Succesfully found rustup"))
;; ;;   ;;(setq components (shell-command-to-string "rustup component list")))
;; ;;   (bootup/message "Failed to find rustup"))

;; ;; (if (executable-find "cargo")
;; ;;     (bootup/message "Succesfully found cargo")
;; ;;   (bootup/message "Failed to find cargo"))

;; ;; (if (executable-find "python")
;; ;;     (bootup/message "Succesfully found python")
;; ;;   (bootup/message "Failed to find python"))


(use-package jinja2-mode)
(use-package ripgrep)

;; git related stuff.
(use-package magit
  :commands magit-status
  :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

; maybe consider taking in a port? 
(defun mdbook-serve ()
  (interactive)
  (let ((default-directory (concat (projectile-project-root) "docs/")))
    (format "%s" default-directory)
    (start-process "mdbook" "*mdbook*" "mdbook" "serve" "-n" "0.0.0.0" "-p" "2323")))

(use-package forge
  :after magit)

(use-package git-timemachine)

(use-package poetry)

(use-package lua-mode)

(use-package cmake-mode)

;; c/c++

(defun my-c-mode-hook ()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)


;;; Rust related items. 

(use-package rust-mode)
(add-hook 'rust-mode-hook
	  (lambda () (setq indent-tabs-mode nil)
	    ))

(use-package cargo)

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

(use-package lsp-ui)

;; lsp auto completion stuff. 


(use-package corfu
  :custom
  (corfu-auto t)
  :init (corfu-global-mode))

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-use-icons t))


(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.5))

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

;; not certain if this works or not. 
;; (use-package dap-mode
;;   :ensure
;;   :config
;;   (dap-ui-mode)
;;   (dap-ui-controls-mode 1)
;;   (require 'dap-lldb)
;;   (require 'dap-gdb-lldb)
;;   ;; installs 
;;   (dap-gdb-lldb-setup)
;;   (dap-register-debug-template
;;    "Rust::LLDB Run Configuration"
;;    (list :type "lldb"
;; 	 :request "launch"
;; 	 :name "LLDB::Run"
;; 	 :gdbpath "rust-lldb"
;; 	 :target nil
;; 	 :cwd nil)))




;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (setq c-default-style '((c-mode . "linux") (c++-mode . "linux")))



;; ;; compliation mode coloring 

;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style '((c-mode . "linux") (c++-mode . "linux")))



;; compliation mode coloring 




;;(use-package lsp-origami)
;;(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)


(use-package request)


(use-package toml-mode)
(use-package ansi-color)

(defun colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'colorize-compilation)

(setq compilation-scroll-output t
      compilation-window-height 20)








;; poetry items
(defun run-pylint ()
  (interactive)
  (projectile-with-default-dir (projectile-acquire-root)
    (compilation-start "poetry run pylint axis_audit")))


(use-package docker-tramp) 

;;(add-to-list 'load-path (expand-file-name "tlc" user-emacs-directory)
(straight-use-package '(tlc :repo "git@ssh.dev.azure.com:v3/brandonphelps/tlc/tlc"
			    :host nil
			    :branch "master")
		      )

(use-package tlc
  :straight t (tlc :type git :repo "git@ssh.dev.azure.com:v3/brandonphelps/tlc/tlc" :host nil :branch "master")
  :config ; loads up code after the package is initialized.
  (add-to-list 'auto-mode-alist '("\\.tlc\\'" . tlc-mode))
  
  )



; (require 'clang-format)
;; clang format integration
