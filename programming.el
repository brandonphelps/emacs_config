

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
	  (lambda () (setq indent-tabs-mode nil)))

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
	 (lsp-mode . efs/lsp-mode-setup)
	 )
)  ;; :config
  ;;   (lsp-enable-which-key-integration t))

;; (use-package lsp-ui)

;; lsp auto completion stuff. 

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


;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; (setq c-default-style '((c-mode . "linux") (c++-mode . "linux")))



;; ;; compliation mode coloring 
;; (use-package ansi-color)

;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region compilation-filter-start (point))
;;   (toggle-read-only))

;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq c-default-style '((c-mode . "linux") (c++-mode . "linux")))



;; compliation mode coloring 
(use-package ansi-color)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;(use-package lsp-origami)
;;(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)
