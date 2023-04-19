
(defun bp/org-mode-setup ()
  (org-indent-mode))

;; (use-package org)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; agenda stuff
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; refileing

(setq org-reverse-note-order t)

;; clocking
(setq org-log-done 'time)
(setq org-clock-into-drawer nil)

(setq org-fast-tag-selection-include-todo nil)
(setq org-use-fast-todo-selection t)

(when (boundp 'bp-org-default-notes-files)
  (setq org-default-notes-file bp-org-default-notes-files))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "OTHER(o)")))

(setq org-capture-templates
      (quote (
	      ("t" "todo clock keep" entry (file org-default-notes-file)
	       "* TODO %?\n" :clock-in t :clock-keep t)
	      ("f" "todo clock resume" entry (file org-default-notes-file)
	       "* TODO %?\n" :clock-in t :clock-resume t))))


(setq org-refile-targets '((org-agenda-files :maxlevel . 8)))

;; refile handling
(setq org-refile-allow-creating-parent-nodes (quote confirm))
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

;; this generates some warning? 
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

(defvar ss/org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
 (todo . " %i %-12:c")
 (tags . " %i %-12:c")
 (search . " %i %-12:c")))

(setq bp/notes-custom '("N" "Notes" tags "NOTE"
	       ((org-agenda-overriding-header "Notes")
		(org-tags-match-list-sublevels t))))

;; works for tags / todos
;; (org-agenda-skip-function 'my/org-agenda-skip-func)
(defun my/org-agenda-skip-func ()
  (org-agenda-skip-entry-if 'scheduled 'deadline))

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
	       ((org-agenda-overriding-header "Notes")
		(org-tags-match-list-sublevels t)))
	      ;; ("I" "items only with timestamps" ((agenda ""))
	      ;;  ((org-agenda-ndays 1)
	      ;; 	(org-agenda-show-log nil)
	      ;; 	(org-agenda-entry-types '(:timestamp))
	      ;; 	(org-agenda-clockreport-mode t)
	      ;; 	(org-agenda-include-diary nil)
	      ;; 	(org-agenda-span 'day)
	      ;; 	(org-agenda-show-all-dates nil)
	      ;; 	))
	      ;; ("i" "only scheduled entries sorted by time" ((agenda ""))
	      ;;  ((org-agenda-ndays 1)
	      ;; 	(org-agenda-show-log t)
	      ;; 	(org-agenda-entry-type '())
	      ;; 	(org-agenda-clockreport-mode t)
	      ;; 	(org-agenda-span 'day)
	      ;; 	))
	      ("j" "Meetings" tags-todo "MEETING"
	       ((org-agenda-overriding-header "Meetings")
		(org-tags-match-list-sublevels t)
		))
	      ("Ps" "Product support" tags-todo "PRODUCT_SUPPORT"
	       ((org-agenda-overriding-header "Product support")
		(org-tags-match-list-sublevels t)
		))
	      (" " "Agenda"
	       ((agenda ""
			((org-agenda-ndays 1)
			 (org-agenda-show-log t)
			 (org-agenda-span 'day)))
		(tags "REFILE"
		      ((org-agenda-overriding-header "Refile")
		       (org-tags-match-list-sublevels t)
		       ))
		(tags-todo "-REFILE"
			   ((org-agenda-overriding-header "TODO")
			    (org-tags-match-list-sublevels t)))
		))
	      )))
;; =======
;; 	       ((agenda "")
;; 		(tags "REFILE"
;; 		      ((org-agenda-overriding-header "Refile")
;; 		       (org-tags-match-list-sublevels t)
;; 		       (org-agenda-sorting-strategy
;; 			'(todo-state-down time-down))
;; 		       (org-agenda-prefix-format ss/org-agenda-prefix-format))))
;; 	       )
;; 	      ))) 
;; >>>>>>> origin/feature/agenda

;; remove clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("OTHER" :foreground "forest green" :weight bold)))

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ; time specific items are already sorted first by org-agenda-sorting-strategy

     ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

     ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

     ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
    ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
    ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
    ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
    ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (message "%s" date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))
;; (setq org-agenda-sorting-strategy
;;       '((agenda time-down)
;; 	(todo priority-down time-down)))

(setq org-agenda-sort-agenda-notime-is-late nil)


;;(setq org-clock-history-length 100)

