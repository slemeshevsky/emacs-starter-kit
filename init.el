;;; init.el --- Where all the magic begins
;;
;; part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;;Так как я использую автономную версию ~CEDET~, то необходимо выполнить 
;; загрузку пакета с помощью команды
;; #+begin_src emacs-lisp

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/bin/cedet/contrib/")
(load-file "~/bin/emacs/cedet/cedet-devel-load.el")
(load-file "~/bin/emacs/cedet/contrib/cedet-contrib-load.el")
(add-to-list  'Info-directory-list "~/bin/cedet/doc/info")
;;#+end_src 

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(setq sitelisp-dir (expand-file-name "src" dotfiles-dir))

;; Load up Org Mode and Babel
(require 'org)

;; load up the main file
(org-babel-load-file (expand-file-name "starter-kit.org" dotfiles-dir))
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#d9d9d9"))))
 '(company-scrollbar-fg ((t (:background "#cccccc"))))
 '(company-tooltip ((t (:inherit default :background "#c5c5c5"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(org-agenda-done ((t (:foreground "ForestGreen" :strike-through t))))
 '(org-done ((t (:foreground "ForestGreen" :strike-through t :weight bold))))
 '(org-level-2 ((t (:foreground "#8C5353")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-auto-light-when-set (quote all-in-buffer))
 '(bmkp-last-as-first-bookmark-file "~/Projects/Doconce/python-num/src/python-num.bmk")
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness (quote unless-remote))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-prefix-key "C-c g")
 '(helm-gtags-pulse-at-cursor t)
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-use-input-at-cursor t)
 '(org-agenda-todo-list-sublevels nil)
 '(package-selected-packages
   (quote
	(org-super-agenda web-mode smart-tabs-mode python-mode pydoc projectile-speedbar professional-theme persp-projectile org-pomodoro org-bullets org-beautify-theme material-theme magit ipython ibuffer-projectile helm-projectile helm-gtags helm-bibtex helm-ag fill-column-indicator company-cmake company-c-headers company-auctex column-marker coffee-mode cmake-mode bookmark+ auto-complete anti-zenburn-theme alect-themes)))
 '(safe-local-variable-values
   (quote
	((projectile-project-compilation-cmd . "ablog build && cp -rp _website/* 

../slemeshevsky.github.io")
	 (projectile-project-run-cmd . "~/Projects/C++/build/cds/cds")
	 (projectile-project-compilation-cmd . "cd ~/Projects/C++/build/cds && qmake ../../cds/gui/cds.pro -spec linux-g++-64 CONFIG+=debug CONFIG+=qml_debug && make")
	 (projectile-project-name . "cds")
	 (eval progn
		   (setq dir
				 (projectile-project-root))
		   (setq doconce-project-path dir)
		   (setq bookmark-default-file
				 (concat dir projectile-project-name ".bmk")))
	 (doconce-project-path expand-file-name)
	 (doconce-section-nickname . "fdd")
	 (doconce-chapter-nickname . "fdm-for-ode")
	 (projectile-project-name . "python-num")
	 (projectile-project-name . "project")
	 (projectile-project-name . "porject")
	 (doconce-section-nickname . "tree")
	 (doconce-section-nickname . "three")
	 (doconce-section-nickname . "two")
	 (doconce-chapter-nickname . "intro")
	 (doconce-section-nickname quote two)
	 (doconce-chapter-nickname quote intro)
	 (doconce-author-institute . "Институт математики НАН Беларуси")
	 (doconce-author-email . "sergey.lemeshevsky@gmail.com")
	 (eval progn
		   (setq dir
				 (projectile-project-root))
		   (setq doconce-project-path dir))
	 (doconce-project-path quote
						   (projectile-project-root))
	 (doconce-project-path directory-file-name
						   (projectile-project-root))
	 (doconce-project-path projectile-project-root)
	 (doconce-project-path)
	 (doconce-project-path expand-file-name "")
	 (doconce-project-path expand-file-name "" "chapters")
	 (doconce-author-name . "С.В. Лемешевский")
	 (doconce-author-name . "С.В.Л.")
	 (doconce-author . "С.В.Л.")
	 (projectile-project-compilation-cmd . "ablog build && cp -rp _website/* ../slemeshevsky.github.io")
	 (projectile-project-compilation-cmd . "ablog build && cp _website/* ../slemeshevsky.github.io")
	 (projectile-project-compilation-run . "ablog serv -r")
	 (projectile-project-compilation-cmd . "ablog build")
	 (projectile-project-compilation-cmd . "cd .. && make html"))))
 '(truncate-lines t))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
