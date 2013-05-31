;; Конфигурационный файл с моими настройками Org-mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(org-remember-insinuate)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-iswitchb)
(global-set-key "\C-cr" 'org-remember)
(setq org-log-done 'time)
(setq org-agenda-files (list "~/Dropbox/org/gtd.org"
			     "~/Dropbox/org/someday.org" 
			     "~/Dropbox/org/journal.org"
			     "~/Dropbox/org/weeklyreview.org" 
			     "~/Dropbox/org/council010202.org" 
			     "~/Dropbox/org/bodybuilding.org"
			     ))
(setq org-deadline-warning-days 5)
(setq org-mobile-directory "~/Dropbox/Public/MobileOrg/")
(setq org-directory "~/Dropbox/org/")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")

(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/Dropbox/org/gtd.org" "ЗАДАЧИ")
	("Call" ?c "* CALL %^{Brief Description} %^g\n%?\nAdded: %U" "~/Dropbox/org/gtd.org" "ЗАДАЧИ")
	("Outside" ?o "** %^{Brief Description} %^g\n%?\nAdded: %U" "~/Dropbox/org/gtd.org" "ЗАДАЧИ")
	("Journal" ?j "** %^{Head Line} %U %^g\n%i%?" "~/Dropbox/org/journal.org" "Заметки")
	("Someday" ?s "** %^{Someday Heading} %U\n%?\n" "~/Dropbox/org/someday.org" "Когда-нибудь/может быть")))

(setq org-agenda-custom-commands
      '(
	("w" "Список дел, которые можно сделать на работе"
	 ((agenda)
	  (tags-todo "@ИНСТИТУТ")
	  (tags-todo "@КОМПЬЮТЕР")
	  (tags-todo "@ВЫЕЗДЫ")))
	
	("h" "Список дел, которые можно сделать дома" 
	 ((agenda)
	  (tags-todo "@ДОМ") 
	  (tags-todo "@КОМПЬЮТЕР") 
	  (tags-todo "@ВЫЕЗДЫ")))
	
	("d" "Список дел на день"
	 ((agenda "" ((org-agenda-ndays 1)
		      (org-agenda-sorting-strategy
		       (quote ((agenda time-up priority-down tag-up))))
		      (org-deadline-warning-days 7)))))
	
	("P" "Printed agenda"
	 ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
		      (org-agenda-start-on-weekday nil)         ;; calendar begins today
		      (org-agenda-repeating-timestamp-show-all t)
		      (org-agenda-entry-types '(:timestamp :sexp))))
	  (agenda "" ((org-agenda-ndays 1)                      ;; daily agenda
		      (org-deadline-warning-days 7)             ;; 7 day advanced warning for deadlines
		      (org-agenda-todo-keyword-format "[ ]")
		      (org-agenda-scheduled-leaders '("" ""))
		      (org-agenda-prefix-format "%t%s")))
	  (todo "TODO"                                          ;; todos sorted by context
		((org-agenda-prefix-format "[ ] %T: ")
		 (org-agenda-sorting-strategy '(tag-up priority-down))
		 (org-agenda-todo-keyword-format "")
		 (org-agenda-overriding-header "\nЗадачи по контексту\n------------------\n"))))
	 ((org-agenda-compact-blocks t)
	  (org-agenda-remove-tags t)
	  (ps-number-of-columns 2)
	  (ps-landscape-mode t))
	 ("~/Dropbox/org/documents/A/agendas/agenda.ps")))
      )
  
(setq org-refile-targets (quote (("gtd.org" :maxlevel . 2)
				 ("someday.org" :level . 2))))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
     
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; implementation Pomodoro timer
(add-to-list 'org-modules 'org-timer) ;; Activate the org-timer module 
(setq org-timer-default-timer 25) ;; Set a default value for the timer, for example
(add-hook 'org-clock-in-hook 
	  '(lambda ()  
	     (if (not org-timer-current-timer) (org-timer-set-timer '(16)))
	     )
) 
(add-hook 'org-clock-out-hook
	  '(lambda ()  (setq org-mode-line-string nil)))

(add-hook 'org-timer-done-hook 
	  '(lambda () 
	     (start-process "orgmode" nil "~/Dropbox/org/scripts/pomodoro")
	     ) 
)

(org-mobile-pull) ;; run org-mobile-pull at startup

;; Включаем типографику в режиме org-mode
(add-hook 'org-mode-hook 'turn-on-typopunct-mode)