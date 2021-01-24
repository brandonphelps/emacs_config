(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   '("~/AppData/Roaming/agenda/refile.org" "~/AppData/Roaming/agenda/projects.org" "~/AppData/Roaming/agenda/product_support.org" "~/AppData/Roaming/agenda/process_improvement.org" "~/AppData/Roaming/agenda/others.org" "~/AppData/Roaming/agenda/no_wip.org" "~/AppData/Roaming/agenda/notes.org" "~/AppData/Roaming/agenda/meetings.org" "~/AppData/Roaming/agenda/matlab.org" "~/AppData/Roaming/agenda/mab.org" "~/AppData/Roaming/agenda/lum.org" "~/AppData/Roaming/agenda/lpa.org" "~/AppData/Roaming/agenda/hed_target.org" "~/AppData/Roaming/agenda/altecu.org" "~/AppData/Roaming/agenda/axis_sim.org" "~/AppData/Roaming/agenda/cl455.org" "~/AppData/Roaming/agenda/code_review.org" "~/AppData/Roaming/agenda/contacts.org" "~/AppData/Roaming/agenda/email.org" "~/AppData/Roaming/agenda/exodus.org" "~/AppData/Roaming/agenda/genesis.org" "~/AppData/Roaming/agenda/reviews.org" "~/AppData/Roaming/agenda/simulink_builder.org" "~/AppData/Roaming/agenda/time_off.org" "~/AppData/Roaming/agenda/work.org"))
 '(package-selected-packages
   '(db-pg markdown-mode cmake-mode toml-mode magit helm python-mode yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; agenda


(global-set-key (kbd "<f12>") 'org-agenda)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-directory "~/AppData/Roaming/agenda")
(setq org-default-notes-file "~/AppData/Roaming/agenda/refile.org")

;; clocking

(setq org-log-done 'time)

;; (setq org-log-into-drawer nil)


;; tabs and spaces formatting. 
;; (smart-tabs-insinuate 'c 'c++)

(defun my-c-mode-hook ()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(smart-tabs-insinuate 'c)

(add-hook 'c-mode-hook 'my-c-mode-hook)

;; (add-hook 'c-mode-hook 'my-c-mode-hook)
(setq c-default-style '((c-mode . "linux") (c++-mode . "linux")))


(setq-default indent-tabs-mode nil)

(setq org-clock-into-drawer nil)
(add-to-list 'load-path "~/.emacs.d/py_jira")

(require 'py_jira)
