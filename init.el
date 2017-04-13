;;; init.el --- Where all the magic begins
;;
;; part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;;Так как я использую автономную версию ~CEDET~, то необходимо выполнить 
;; загрузку пакета с помощью команды
;; #+begin_src emacs-lisp
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
 '(company-scrollbar-bg ((t (:background "#ffffff"))))
 '(company-scrollbar-fg ((t (:background "#ffffff"))))
 '(company-tooltip ((t (:inherit default :background "#ffffff"))))
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
 '(safe-local-variable-values
   (quote
	((eval progn
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
	 (projectile-project-run-cmd . "~/Projects/Cpp/build/cds/cds")
	 (projectile-project-compilation-cmd . "cd ~/Projects/Cpp/build/cds && qmake-qt4 ~/Projects/Cpp/cds/gui/cds.pro -r -spec linux-g++ CONFIG+=debug && make")
	 (projectile-project-compilation-cmd . "cd ~/Projects/build/cds && qmake-qt4 ~/Projects/cds/gui/cds.pro -r -spec linux-g++ CONFIG+=debug && make")
	 (projectile-project-compilation-cmd . "cd .. && make html")
	 (projectile-project-run-cmd . "~/Projects/build/cds/cds")
	 (projectile-project-compilation-cmd . "cd ~/Projects/build/cds && qmake-qt4 ~/Projects/cds/gui/cds.pro && make")
	 (projectile-project-compilation-cmd . "cd ~/Projects/build/cds/ && qmake-qt4 'CONFIG += debug' ../../cds/gui/cds.pro && make")
	 (projectile-project-compilation-cmd . "cd ~/Projects/build/cds/ && qmake-qt4 'CONFIG += debug' ../../cds/cds.pro && make"))))
 '(truncate-lines t))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
