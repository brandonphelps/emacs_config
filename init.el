
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/elpa/yaml-mode")
(add-to-list 'load-path "~/.emacs.d/elpa/cargo.el")
(add-to-list 'load-path "~/.emacs.d/elpa/emacs-web-server")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(load-file "~/.emacs.d/elpa/rust-mode/rust-mode.el")
(load-file "~/.emacs.d/elpa/markdown-mode/markdown-mode.el")
(require 'markdown-mode)
(load-file "~/.emacs.d/elpa/cargo.el/cargo.el")
(load-file "~/.emacs.d/elpa/emacs-web-server/simple-httpd.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset (quote set-from-style))
 '(c-offsets-alist (quote ((inline-open . 0) (substatement-open . 0))))
 '(custom-enabled-themes (quote (tango)))
 '(package-archives (quote (("melpa" . "http://melpa.org/packages/"))))
 '(package-selected-packages (quote (eglot company web-mode helm magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


(defun my-c-mode-hook ()
  (local-set-key (kbd "C-M-p") 'c-beginning-of-defun)
  (local-set-key (kbd "C-M-n") 'c-end-of-defun)
  (setq c-basic-offset 2)
  (setq tab-width 2))

(setq c-default-style '((java-mode . "linux")
			(awk-mode . "awk")
			(c-mode . "linux")
			(c++-mode . "linux")))

(require 'eglot)

;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

(defun my-c++-mode-hook ()
  (setq c-basic-offset 2)
  (setq tab-width 2))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;(add-hook 'c-mode-hook 'eglot-ensure)
;;(add-hook 'c++-mode-hook 'eglot-ensure)
(require 'rust-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(smart-tabs-mode db-pg markdown-mode cmake-mode toml-mode magit helm python-mode yaml-mode)))


(smart-tabs-insinuate 'c 'javascript)
