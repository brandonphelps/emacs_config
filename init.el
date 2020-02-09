(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(package-selected-packages
   (quote
    (ggtags helm-etags-plus helm helm-ag smart-tabs-mode neotree cmake-ide rust-mode yaml-mode magit lua-mode org-journal org-kanban))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; (setq c-default-style "bsd")
;; (setq-default c-basic-offset 2)
;; (c-set-offset 'case-label '+)
;; (ido-mode t)

;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (defun my-c-mode-hook ()
;;   (setq tab-width 2)
;;   (helm-mode 1)
;;   (ggtags-mode 1)
;;   )
;; (add-hook 'c++-mode-hook 'my-c-mode-hook)

;; (global-set-key (kbd "C-c i") 'windmove-up)
;; (global-set-key (kbd "C-c k") 'windmove-down)
;; (global-set-key (kbd "C-c j") 'windmove-left)
;; (global-set-key (kbd "C-c l") 'windmove-right)

;; (smart-tabs-insinuate 'c 'c++ 'python)

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


