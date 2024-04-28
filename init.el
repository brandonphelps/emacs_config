
(column-number-mode)
(setq-default fill-column 90)
(setq custom-file "~/.emacs.d/custom.el")


(load-file "~/.emacs.d/elpaca-bootstrap.el")

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)
(use-package org)
(elpaca-wait)

(require 'tramp)


(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; look into this https://gitlab.com/jgkamat/rmsbolt
;;; uses configurations from https://github.com/daviwil/emacs-from-scratch

(if (executable-find "pianobar")
    (use-package pianobar))
;; doesn't seem to contain the messages that are added to it. 
;; (setq initial-buffer-choice "*bootup-report*")
;; bootup report helper functions. 
(defun bootup/message (msg)
  (with-current-buffer (get-buffer-create "*bootup-report*")
    (end-of-buffer)
    (insert (concat msg "\n"))))


(defvar machine-settings-file
  (concat user-emacs-directory "box-specifics/" (downcase system-name) ".el")
  "Settings file for the box we are currently on")

; for elfeed.
;; https://fasterthanli.me/index.xml

(when (file-exists-p machine-settings-file)
  (load-file machine-settings-file))

;; basic UI setup. 
(setq inhibit-startup-message t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(setq ring-bell-function #'ignore)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-fringe-mode 15)
  )
(tooltip-mode -1)
(menu-bar-mode -1)

;; movement helpers. 
(global-set-key (kbd "C-c i") 'windmove-up)
(global-set-key (kbd "C-c k") 'windmove-down)
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)

; (use-package rustic)

;; UI Setup
(if (display-graphic-p) 
    (use-package doom-themes
      :init (load-theme 'doom-palenight t))
  (use-package doom-themes
    :init (load-theme 'tsdh-dark t)))
;; (M-x customize-themes)


;; utility packages
(use-package rainbow-delimiters)
(use-package no-littering)
(use-package helpful)
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
    (setq which-key-idle-delay 1))


;; Programming stuff. 
(use-package rust-mode)
(add-hook 'rust-mode-hook
	  (lambda () (setq indent-tabs-mode nil)
	    ))

(elpaca-wait)

(use-package poetry)
(use-package cargo)
(use-package ripgrep)
(use-package yaml-mode)
(use-package cmake-mode)
(use-package toml-mode)

(elpaca-wait)

; (use-package ansi-color)



(defun colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'colorize-compilation)


;;; lsp mode
;; (defun efs/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
;;   :hook ((rust-mode . lsp)
;; 	 (python-mode . lsp)
;; 	 (c-mode . lsp)
;; 	 (c++-mode . lsp)
;; 	 (lsp-mode . efs/lsp-mode-setup)
;; 	 )
;;   :config
;;   (setq lsp-signature-auto-activate nil))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t))

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

;; c/c++
(defun my-c-mode-hook ()
  (setq c-basic-offset 2)
  (setq indent-tabs-mode t)
  (setq tab-width 2))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Color handling for compilation

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


(setq compilation-scroll-output t
      compilation-window-height 20)

;; Git related


(use-package magit
  :commands magit-status
  :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package git-timemachine)

;; Projectil setup
(use-package projectile
  :diminish
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; (when (boundp 'bp-default-project-path)
  ;;     (setq projectile-project-search-path '(bp-default-project-path)))
  (setq projectile-switch-project-action #'projectile-dired))

;; ;(projectile-register-project-type
;; ; 'conan '("conanfile.py" "CMakeLists.txt")
;; ; :project-file "conanfile.py"
;; ; :compile "conan install . -if build -b missing"
;; ; :run "conan build . -bf build"
;; ; )

(use-package vertico
  :init
  (vertico-mode))

;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; org roam note taking

(use-package org-roam
  :custom
  (org-roam-directory "~/roam-notes")
  (org-roam-v2-act t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  )

(load-file (concat user-emacs-directory "/agenda.el"))

;; Agenda loading

(setq org-agenda-files (directory-files "~/agenda" t ".org$"))

(defvar machine-settings-file
  (concat user-emacs-directory "box-specifics/" (downcase system-name) ".el")
  "Settings file for the box we are currently on")

(when (file-exists-p machine-settings-file)
  (message "Loading machine-settings-file")
  (load-file machine-settings-file))


;; testing uility stuff.

(use-package ascii-table)


; (require 'clang-format)

;; (defun dired-run-at-point ()
;;   (interactive)

;;   ;; Open org agenda without showing it in the current frame
;;   (save-window-excursion
;;     (org-agenda-list))
;;   ;; Create posframe with the org agenda buffer
;;   (let ((frame (posframe-show org-agenda-buffer
;;                               :poshandler 'posframe-poshandler-frame-center
;;                               :border-width 2
;;                               :border-color "gray")))
;;     ;; Focus org agenda frame to be able to use it's shorcuts
;;     (x-focus-frame frame)
;;     ;; Bring back the disappeared cursor
;;     (with-current-buffer org-agenda-buffer
;;       (setq-local cursor-type 'box))))


(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))

;;   (let ((process (dired-file-name-at-point)))
;;     (async-start-process (file-name-base process) process '(lambda (arg)))))

;; ;; look into this https://gitlab.com/jgkamat/rmsbolt
;; ;;; uses configurations from https://github.com/daviwil/emacs-from-scratch

;; (if (executable-find "pianobar")
;;     (use-package pianobar))
;; ;; doesn't seem to contain the messages that are added to it. 
;; ;; (setq initial-buffer-choice "*bootup-report*")
;; ;; bootup report helper functions. 
;; (defun bootup/message (msg)
;;   (with-current-buffer (get-buffer-create "*bootup-report*")
;;     (end-of-buffer)
;;     (insert (concat msg "\n"))))

;; (use-package elfeed
;;   :custom
;;   (when (boundp elfeed-my-custom-feeds)
;;     (elfeed-feeds
;;      elfeed-my-custom-feeds))
;;   (elfeed-sort-order 'ascending)
;;   )

;; ;; (define-key elfeed-search-mode-map (kbd "t") 'elfeed-w3m-open)
;; (define-key elfeed-search-mode-map (kbd "w") 'elfeed-eww-open)

;; (use-package flyspell
;;   :defer t
;;   :init
;;   (progn
;;     (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;     (add-hook 'text-mode-hook 'flyspell-mode)))


;; ;; hmm load user specifics customizations late or early? 
;; (when (file-readable-p machine-settings-file)
;;   (load-file machine-settings-file))


;; (use-package consult
;;   :demand t
;;   :bind (;;("C-s" . consult-line)
;; 	 ("C-M-l" . consult-imenu)
;; 	 ("C-x b" . consult-buffer)
;; 	 :map minibuffer-local-map
;; 	 ("C-r" . consult-history)
;; 	 )
;;   :custom
;;   (consult-project-root-function #'projectile-project-root)
;;   (completion-in-region-function #'consult-completion-in-region)
;;   )


;; (when (boundp 'bp-default-project-path)
;;   (message "%s" bp-default-project-path))


(load-file "~/.gnus")


;; (use-package markdown-mode)
;; (defun markdown-html (buffer)
;;   (princ (with-current-buffer buffer
;;     (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
;;   (current-buffer)))

;; (setq backup-directory '(("." . ,(expand-file-name "tmp/backups" user-emacs-directory))))

;; (defun rscript ()
;;   (interactive)
;;   (message (shell-command-to-string (concat "rust-script.exe " (buffer-file-name)))))

;; ;; gpg helper funcs
;; (defun efs/lookup-password (&rest keys)
;;  (let ((result (apply #'auth-source-search keys)))
;;    (if result
;; 	(funcall (plist-get (car result) :secret))
;;      nil)))

(defun decode-hex-string (hex-string)
  (let ((res nil))
    (dotimes (i (/ (length hex-string) 2) (apply #'concat (reverse res)))
      (let ((hex-byte (substring hex-string (* 2 i) (* 2 (+ i 1)))))
        (push (format "%c" (string-to-number hex-byte 16)) res)))))


(fset #'jsonrpc--log-event #'ignore)
(setq eglot-events-buffer-size 0)
(setq eglot-sync-connect nil)






;; capture templaes

(setq org-capture-templates
      '(
	("j" "Journal Entry"
	 entry (file+datetree "~/agenda/journal.org")
	 "* %?"
	 :empty-lines 1)
	;; order is important, e must come first. 
	("e" "Exercise entries")
	("ee" "Exercise Entry"
	 entry (file+olp+datetree "~/agenda/journal.org" "Exercise" )
	 "* %?\n%T")
	("er" "Run"
	 entry (file+olp+datetree "~/agenda/journal.org" "Exercise" )
	 "* Run: %?\n%T")
	("ep" "Push-ups"
	 entry (file+olp+datetree "~/agenda/journal.org" "Exercise" )
	 "* Push-ups: %?\n%T")
	("es" "Squats"
	 entry (file+olp+datetree "~/agenda/journal.org" "Exercise" )
	 "* Squats: %?\n%T")
	))


(custom-set-variables
 '(org-agenda-files '("~/agenda/journal.org")))
