

(defun bp/org-mode-setup ()
  (org-indent-mode))

;; org mode
(use-package org
  :straight nil
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

;; do we care about babel stuff? 
(org-babel-do-load-languages
 'org-babel-load-languages bp-org-babel-languages)

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))

;; agenda stuff
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

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


;; Rust handling. 
(when (and (executable-find "rustup") (executable-find "rust-script"))
  ;; how to only add this once? 
  (push '(rust . t) bp-org-babel-languages))
