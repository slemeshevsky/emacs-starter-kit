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
 '(TeX-command-list
   (quote
	(("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
	  (plain-tex-mode texinfo-mode ams-tex-mode)
	  :help "Run plain TeX")
	 ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
	  (latex-mode doctex-mode)
	  :help "Run LaTeX")
	 ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
	  (texinfo-mode)
	  :help "Run Makeinfo with Info output")
	 ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
	  (texinfo-mode)
	  :help "Run Makeinfo with HTML output")
	 ("AmSTeX" "amstex %(PDFout) %(extraopts) %`%S%(mode)%' %t" TeX-run-TeX nil
	  (ams-tex-mode)
	  :help "Run AMSTeX")
	 ("ConTeXt" "%(cntxcom) --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
	  (context-mode)
	  :help "Run ConTeXt once")
	 ("ConTeXt Full" "%(cntxcom) %(extraopts) %(execopts)%t" TeX-run-TeX nil
	  (context-mode)
	  :help "Run ConTeXt until completion")
	 ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
	 ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
	 ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
	 ("Print" "%p" TeX-run-command t t :help "Print the file")
	 ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
	 ("File" "%(o?)dvips %d -o %f " TeX-run-dvips t t :help "Generate PostScript file")
	 ("Dvips" "%(o?)dvips %d -o %f " TeX-run-dvips nil t :help "Convert DVI file to PostScript")
	 ("Dvipdfmx" "dvipdfmx %d" TeX-run-dvipdfmx nil t :help "Convert DVI file to PDF with dvipdfmx")
	 ("Ps2pdf" "ps2pdf %f" TeX-run-ps2pdf nil t :help "Convert PostScript file to PDF")
	 ("Glossaries" "makeglossaries %s" TeX-run-command nil t :help "Run makeglossaries to create glossary file")
	 ("Index" "makeindex %s" TeX-run-index nil t :help "Run makeindex to create index file")
	 ("upMendex" "upmendex %s" TeX-run-index t t :help "Run upmendex to create index file")
	 ("Xindy" "texindy -M rueng %s.idx" TeX-run-command nil t :help "Run xindy to create index file")
	 ("Check" "lacheck %s" TeX-run-compile nil
	  (latex-mode)
	  :help "Check LaTeX file for correctness")
	 ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
	  (latex-mode)
	  :help "Check LaTeX file for common mistakes")
	 ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
	 ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
	 ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
	 ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-engine (quote xetex) t)
 '(bmkp-auto-light-when-set (quote all-in-buffer))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/stability-book.bmk")
 '(ebib-file-associations (quote (("pdf" . "evince") ("ps" . "evince"))))
 '(ebib-preload-bib-files
   (quote
	("~/Dropbox/org/bibliography/references.bib" "~/Dropbox/org/bibliography/mypub.bib")))
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
	(org-cliplink treemacs treemacs-projectile flatui-dark-theme flatui-theme spacemacs-theme mu4e-maildirs-extension mu4e-alert mu4e-conversation mu4e-jump-to-list wanderlust org-gnome calfw calfw-org realgud company-c-headers ggtags smartparens function-args common-lisp-snippets django-snippets el-autoyas helm-c-yasnippet yasnippet-snippets ebib org-ref github-theme org-super-agenda web-mode smart-tabs-mode python-mode pydoc projectile-speedbar professional-theme persp-projectile org-pomodoro org-bullets org-beautify-theme material-theme magit ipython ibuffer-projectile helm-projectile helm-gtags helm-bibtex helm-ag fill-column-indicator company-cmake company-auctex column-marker coffee-mode cmake-mode auto-complete anti-zenburn-theme alect-themes)))
 '(safe-local-variable-values
   (quote
	((doconce-section-nickname . "mop")
	 (doconce-chapter-nickname . "report")
	 (projectile-project-name . "nasb-rep")
	 (gud-gbd-comand-name
	  (concat gud-gbd-comand-name projectile-project-run-cmd))
	 (projectile-project-configuration-cmd . "cd ~/Projects/Cxx/build/cds-new && cmake ../../cds")
	 (projectile-project-configure-cmd . "cd ~/Projects/Cxx/build/cds-new && cmake ../../cds-new")
	 (gud-gbd-comand-name concat gud-gbd-comand-name projectile-project-run-cmd)
	 (projectile-project-configure-cmd . "cd ~/Projects/Cxx/build/cds-new && cmake ../cds-new")
	 (projectile-compilation-dir . "~/Projects/Cxx/build/cds-new")
	 (projectile-project-run-cmd . "~/Projects/Cxx/build/cds-new/bin/MainApp")
	 (projectile-project-run-cmd . "~/Projects/Cxx/build/cds/cds")
	 (projectile-project-compilation-cmd . "cd ~/Projects/Cxx/build/cds-new && make")
	 (projectile-project-name . "cds-new")
	 (projectile-project-compilation-cmd . "ablog build && cp -rp _website/* 

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
 '(send-mail-function (quote smtpmail-send-it))
 '(truncate-lines t))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
