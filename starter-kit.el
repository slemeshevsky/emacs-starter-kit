
(setq dotfiles-dir (file-name-directory
                     (or load-file-name (buffer-file-name))))
  
(add-to-list 'load-path dotfiles-dir)
(setq package-user-dir (concat dotfiles-dir "elpa"))

(require 'package)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(auctex reftex)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq system-specific-config (concat dotfiles-dir system-name ".el")
          system-specific-literate-config (concat dotfiles-dir system-name ".org")
          user-specific-config (concat dotfiles-dir user-login-name ".el")
          user-specific-literate-config (concat dotfiles-dir user-login-name ".org")
          user-specific-dir (concat dotfiles-dir user-login-name))
    (add-to-list 'load-path user-specific-dir)

(add-to-list 'load-path sitelisp-dir)

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(add-to-list 'load-path (expand-file-name "color-theme" sitelisp-dir))
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn (color-theme-initialize)))

(require 'typopunct)
(setq-default typopunct-buffer-language 'russian)
;; Функция включает typopunct-mode.
(defun turn-on-typopunct-mode ()
  (typopunct-mode t))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(global-font-lock-mode t)

(show-paren-mode 1)

(require 'ess-site)

(load-library "ps-mule") 
(setq bdf-directory-list '("/usr/share/emacs/fonts/bdf")) 
(setq ps-mule-font-info-database-default ps-mule-font-info-database-bdf) 
(setq ps-multibyte-buffer 'bdf-font-except-latin)

(if (file-exists-p sitelisp-dir)
  (let ((default-directory sitelisp-dir))
    (normal-top-level-add-subdirs-to-load-path)))
 (if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-literate-config)
    (org-babel-load-file user-specific-literate-config))
(when (file-exists-p user-specific-dir)
  (let ((default-directory user-specific-dir))
    (mapc #'load (directory-files user-specific-dir nil ".*el$"))
     (mapc #'org-babel-load-file (directory-files user-specific-dir nil ".*org$"))))
 (if (file-exists-p system-specific-config) (load system-specific-config))
 (if (file-exists-p system-specific-literate-config)
     (org-babel-load-file system-specific-literate-config))
