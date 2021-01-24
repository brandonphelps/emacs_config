;; doesn't seem to contain the messages that are added to it. 
;; (setq initial-buffer-choice "*bootup-report*")
;; bootup report helper functions. 
(defun bootup/message (msg)
  (with-current-buffer (get-buffer-create "*bootup-report*")
    (end-of-buffer)
    (insert (concat msg "\n"))))


(setq custom-file "~/.emacs.d/custom.el")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(setq-default use-package-verbose t)
(setq-default use-package-always-ensure t)

(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)



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

;; should be conditional on machine
(setq org-directory "~/AppData/Roaming/agenda")
(setq org-default-notes-file "~/AppData/Roaming/agenda/refile.org")

(setq org-agenda-files
      '("~/AppData/Roaming/agenda/refile.org" "~/AppData/Roaming/agenda/projects.org" "~/AppData/Roaming/agenda/product_support.org" "~/AppData/Roaming/agenda/process_improvement.org" "~/AppData/Roaming/agenda/others.org" "~/AppData/Roaming/agenda/no_wip.org" "~/AppData/Roaming/agenda/notes.org" "~/AppData/Roaming/agenda/meetings.org" "~/AppData/Roaming/agenda/matlab.org" "~/AppData/Roaming/agenda/mab.org" "~/AppData/Roaming/agenda/lum.org" "~/AppData/Roaming/agenda/lpa.org" "~/AppData/Roaming/agenda/hed_target.org" "~/AppData/Roaming/agenda/altecu.org" "~/AppData/Roaming/agenda/axis_sim.org" "~/AppData/Roaming/agenda/cl455.org" "~/AppData/Roaming/agenda/code_review.org" "~/AppData/Roaming/agenda/contacts.org" "~/AppData/Roaming/agenda/email.org" "~/AppData/Roaming/agenda/exodus.org" "~/AppData/Roaming/agenda/genesis.org" "~/AppData/Roaming/agenda/reviews.org" "~/AppData/Roaming/agenda/simulink_builder.org" "~/AppData/Roaming/agenda/time_off.org" "~/AppData/Roaming/agenda/work.org"))

;; clocking

(setq org-log-done 'time)

(setq org-clock-into-drawer nil)

;; tabs and spaces formatting. 


(setq c-default-style '((c-mode . "linux") (c++-mode . "linux")))
(setq-default indent-tabs-mode nil)
(add-to-list 'load-path "~/.emacs.d/py_jira")

(require 'py_jira)






;;; (quelpa-use-package quelpa utop alert flycheck flycheck-clang-tidy flycheck-rust emacsql-sqlite3 use-package forge helm lsp-mode eglot cargo racer tramp ggtags smart-tabs-mode neotree rust-mode yaml-mode magit lua-mode org-journal org-kanban))

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

(if (executable-find "python")
    (bootup/message "Succesfully found python")
  (bootup/message "Failed to find python"))
    

;; Todo; check if rustup components are installed.
;; rls needs , rls, rust-src rust-analysis 

(use-package forge
  :ensure t)

;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

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

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun my-c-mode-hook ()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(add-hook 'c-mode-hook 'my-c-mode-hook)

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






