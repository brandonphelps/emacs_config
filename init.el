

;; doesn't seem to contain the messages that are added to it. 
;; (setq initial-buffer-choice "*bootup-report*")
;; bootup report helper functions. 
(defun bootup/message (msg)
  (with-current-buffer (get-buffer-create "*bootup-report*")
    (end-of-buffer)
    (insert (concat msg "\n"))))


(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

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



(setq-default use-package-verbose t)
(setq-default use-package-always-ensure t)

;;; (quelpa-use-package quelpa utop alert flycheck flycheck-clang-tidy flycheck-rust emacsql-sqlite3 use-package forge helm lsp-mode eglot cargo racer tramp ggtags smart-tabs-mode neotree rust-mode yaml-mode magit lua-mode org-journal org-kanban))

(use-package rust-mode)

(use-package quelpa
  :ensure t)
(use-package quelpa-use-package
  :ensure t)
(use-package company
  :ensure t)

(use-package smart-tabs-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package forge
  :after magit)


;; eglot
(use-package eglot
  :ensure t)

;; eglot c / c++ 

;; (ghub-request "GET" "/user" nil
;; 	      :forge 'github
;; 	      :host "api.github.com"
;; 	      :username "brandonphelps"
;; 	      :auth 'forge)



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

;; Todo; check if rustup components are installed.
;; rls needs , rls, rust-src rust-analysis 





(use-package forge
  :ensure t)



;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

(setq org-agenda-files
      '("~/agenda/money.org" "~/agenda/video_games/league_of_legends.org" "~/agenda/projects/emacs_hacking.org" "~/agenda/japanese/wanikani.org" "~/agenda/emacs_learning.org" "~/agenda/food/food_to_try.org" "~/agenda/food/recipes.org" "~/agenda/car.org" "~/agenda/projects/westinsfriendsgame.org" "~/agenda/projects/gw2api.org" "~/agenda/refile.org"))

(load-theme 'deeper-blue)

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


(ido-mode t)

;; programming stuff

(tool-bar-mode -1)

(smart-tabs-insinuate 'c 'c++ 'python)


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

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun my-c-mode-hook ()
  (setq tab-width 2)
  )
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


;; example compose mail with filling out some stuff
;; (compose-mail "bwp44f@mst.edu" "Food recipe" '(("From" . "zaszrules@gmail.com") ("" . "Hello World")))


;; (global-set-key (kbd "C-c i") 'windmove-up)
;; (global-set-key (kbd "C-c k") 'windmove-down)
;; (global-set-key (kbd "C-c j") 'windmove-left)
;; (global-set-key (kbd "C-c l") 'windmove-right)



;; (defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
;;   "return the full path of tags file"
;;   (let ((dir (file-name-as-directory (file-truename SRC-DIR)) )
;;        file)
;;     (setq file (concat dir "GTAGS"))
;;     (setq new_dir (substring dir 0 (- (length dir) 2)))
;;     (when (or FORCE (not (file-exists-p file)))
;;       (message "Creating TAGS in %s ..." new_dir)
;;       (message "ctags -f %s -e -R %s" file dir)
;;       ;(shell-command
;; 					;(format "ctags -f %s -e -R %s" file (substring (replace-regexp-in-string "/" "\\\\" new_dir))))
;;       (ggtags-create-tags dir))
;;   file))

;; (my-create-tags-if-needed "C:\\Users\\Brandon\\Desktop\\cpp\\requests" 1)
;; (my-create-tags-if-needed "C:\\Users\\Brandon\\Desktop\\cpp\\gw2api" 1)
;; (setenv "GTAGSLIBPATH" "C:\\Users\\Brandon\\Desktop\\cpp\\requests")


;; (defun my-update-tags ()
;;   (interactive)
;;   (dolist (tag tags-table-list)
;;     (my-create-tags-if-needed (file-name-directory tag) t)))

;; (setq tags-table-list '("c:/Users/Brandon/Desktop/cpp/requests/TAGS" "c:/Users/Brandon/.conan/data/libcurl/7.64.1/dev/testing/source/source_subfolder/TAGS"))


;; (message "%s" (replace-regexp-in-string "/" "\\\\" "c:/Users/Brandon/Desktop/cpp/requests"))


;; (ggtags-mode 1)



;; ;; Added by Package.el.  This must come before configurations of
;; ;; installed packages.  Don't delete this line.  If you don't want it,
;; ;; just comment it out by adding a semicolon to the start of the line.
;; ;; You may delete these explanatory comments.
;; (package-initialize)

;; (add-to-list 'load-path "~/.emacs.d/elpa/yaml-mode")
;; (add-to-list 'load-path "~/.emacs.d/elpa/cargo.el")
;; (add-to-list 'load-path "~/.emacs.d/elpa/emacs-web-server")

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/") t)

;; (load-file "~/.emacs.d/elpa/rust-mode/rust-mode.el")
;; (load-file "~/.emacs.d/elpa/markdown-mode/markdown-mode.el")
;; (require 'markdown-mode)
;; (load-file "~/.emacs.d/elpa/cargo.el/cargo.el")
;; (load-file "~/.emacs.d/elpa/emacs-web-server/simple-httpd.el")


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(c-basic-offset (quote set-from-style))
;;  '(c-offsets-alist (quote ((inline-open . 0) (substatement-open . 0))))
;;  '(custom-enabled-themes (quote (tango)))
;;  '(package-archives (quote (("melpa" . "http://melpa.org/packages/"))))
;;  '(package-selected-packages (quote (eglot company web-mode helm magit))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;; (defun my-c-mode-hook ()
;;   (local-set-key (kbd "C-M-p") 'c-beginning-of-defun)
;;   (local-set-key (kbd "C-M-n") 'c-end-of-defun)
;;   (setq c-basic-offset 2)
;;   (setq tab-width 2))

;; (setq c-default-style '((java-mode . "linux")
;; 			(awk-mode . "awk")
;; 			(c-mode . "linux")
;; 			(c++-mode . "linux")))

;; (require 'eglot)

;; ;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

;; (defun my-c++-mode-hook ()
;;   (setq c-basic-offset 2)
;;   (setq tab-width 2))

;; (add-hook 'c-mode-hook 'my-c-mode-hook)
;; (add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; ;;(add-hook 'c-mode-hook 'eglot-ensure)
;; ;;(add-hook 'c++-mode-hook 'eglot-ensure)
;; (require 'rust-mode)


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(inhibit-startup-buffer-menu t)
;;  '(inhibit-startup-screen t)
;;  '(package-selected-packages
;;    '(smart-tabs-mode db-pg markdown-mode cmake-mode toml-mode magit helm python-mode yaml-mode)))


;; (smart-tabs-insinuate 'c 'javascript)
;; >>>>>>> 0ab611b... linux init.el
