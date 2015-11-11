;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
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
 '(org-agenda-done ((t (:foreground "ForestGreen" :strike-through t))))
 '(org-done ((t (:foreground "ForestGreen" :strike-through t :weight bold))))
 '(org-level-2 ((t (:foreground "#8C5353")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
 '(safe-local-variable-values (quote ((projectile-project-compilation-cmd . "cd ~/Projects/build/cds/ && qmake-qt4 'CONFIG += debug' ../../cds/gui/cds.pro && make") (projectile-project-compilation-cmd . "cd ~/Projects/build/cds/ && qmake-qt4 'CONFIG += debug' ../../cds/cds.pro && make")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
