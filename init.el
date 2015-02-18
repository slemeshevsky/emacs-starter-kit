;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;;Так как я использую автономную версию ~CEDET~, то необходимо выполнить 
;; загрузку пакета с помощью команды
;; #+begin_src emacs-lisp
(add-to-list 'load-path "~/projects/cedet-bzr/contrib/")
(load-file "~/bin/emacs/cedet-bzr/cedet-devel-load.el")
(load-file "~/bin/emacs/cedet-bzr/contrib/cedet-contrib-load.el")
(add-to-list  'Info-directory-list "~/projects/cedet-bzr/doc/info")
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
 '(custom-enabled-themes (quote (sanityinc-solarized-light)))
 '(ecb-layout-name "right1")
 '(ecb-options-version "2.40")
 '(ecb-prescan-directories-for-emptyness (quote unless-remote))
 '(org-agenda-todo-list-sublevels nil))
