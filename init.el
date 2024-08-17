
(column-number-mode)
(setq-default fill-column 90)
(setq custom-file "~/.emacs.d/custom.el")

(load-file "~/.emacs.d/elpaca-bootstrap.el")

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(use-package org)

(require 'tramp)

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


(defun colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'colorize-compilation)

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


(use-package transient)

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

;; (cl-defmethod org-roam-node-type ((node org-roam-node))
;;   "Return the TYPE of NODE."
;;   (condition-case nil
;;       (file-name-nondirectory
;;        (directory-file-name
;;         (file-name-directory
;;          (file-relative-name (org-roam-node-file node) org-roam-directory))))
;;     (error "")))

(use-package org-journal)
(use-package ox-hugo)
(use-package org-roam
  :custom
  (org-roam-directory "~/roam-notes")
  (org-roam-v2-act t)
  ;; (org-roam-node-display-template 
  ;;  (concat "${type:15}" " ${title:*} "))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n c" . org-roam-capture)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  )
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))

(defun roam-sitemap (title list)
  (concat "#+OPTIONS: ^:nil author:nil html-postamble:nil\n"
	  "#+SETUPFILE: ./simple_inline.theme\n"
	  "#+TITLE: " title "\n\n"
	  (org-list-to-org list) "\nfile:sitemap.svg"))

(setq my-publish-time 0)
(defun roam-publication-wrapper (plist filename pubdir)
  (org-roam-graph)
  (org-html-publish-to-html plist filename pubdir)
  (setq my-publish-time (cadr (current-time))))

(setq org-publish-project-alist
      '(("roam"
	 :base-directory "~/roam-notes"
	 :auto-sitemap t
	 :sitemap-function roam-sitemap
	 :sitemap-title "Roam notes"
	 :publishing-function roam-publication-wrapper
	 :publishing-directory "~/roam-export"
	 :style "<link rel=\"stylesheet\" href=\"../other/mystyle.cs\" type=\"text/css\">")))

(defun org-roam-custom-link-builder (node)
  (let ((file (org-roam-node-file node)))
    (concat (file-name-base file) ".html")))

(setq org-roam-graph-link-builder 'org-roam-custom-link-builder)

(add-hook 'org-roam-graph-generation-hook
          (lambda (dot svg) (if (< (- (cadr (current-time)) my-publish-time) 5)
                                (progn (copy-file svg "~/roam-export/sitemap.svg" 't)
                                       (kill-buffer (file-name-nondirectory svg))
                                       (setq my-publish-time 0)))))

(use-package org-roam-ui)

(defun publish-garden ()
  (interactive)
  (setq org-hugo-base-dir "~/roam-notes/quartz/")
  (mapc #'jethro/publish (directory-files "~/roam-notes" t ".org$")))
      
(setq org-roam-capture-templates
      '(("m" "main" plain
	 "%?"
	 :if-new (file+head "${slug}.org"
			    "#+title: ${title}\n")
	 :immediate-finish t
	 :unnarrowed t)))




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

(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))




(defun copy-selected-text (start end)
  (interactive "r")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties start end)))
            (shell-command (concat "echo '" text "' | clip.exe")))))


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

(use-package elfeed
  :custom
  (elfeed-sort-order 'ascending)
  )
(use-package elfeed-tube
  :config
  (elfeed-tube-setup))


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


(setq eldoc-echo-area-prefer-doc-buffer t)

(defun ii/decode-jwt (start end &optional jwt)
  "Decode JWT in region and print to help buffer."
  (interactive "r")
  (let* ((tok
	  (if jwt jwt
	    (buffer-substring start end)))
	 (data (s-split "\\." tok))
	 (header (car data))
	 (claims (cadr data)))
    (with-temp-buffer
      (insert (format "%s\n\n%s"
		      (base64-decode-string header t)
		      (base64-decode-string claims t)))
      (json-pretty-print-buffer)
      (with-output-to-temp-buffer "*JWT*" (princ (buffer-string)))))
  t) 

;; this should only happen on on 4k monitor setups or what not. 
(set-face-attribute 'default nil :height 200)

(use-package mood-line
  :config
  (mood-line-mode))
