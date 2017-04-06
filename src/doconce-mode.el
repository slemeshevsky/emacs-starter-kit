(defcustom doconce-mode-hook nil
  "Hook run when `doconce-mode' is turned on.
The hook for `text-mode' is run before this one."
  :group 'doconce
  :type '(hook))

;; Font lock.
(require 'font-lock)
(defgroup doconce-faces nil "Faces used in DocOnce Mode."
  :group 'doconce
  :group 'faces)

(defface doconce-code-face
  '((t :foreground "DodgerBlue4"))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-math-face
  '((t :foreground "cyan4"
	   :background "LightGrey"))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-comment-face
  '((t :inherit font-lock-comment-face))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-crossref-face
  '((t :inherit font-lock-reference-face
	   :foreground "SaddleBrown"
	   :background "LightGrey"))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-section-heading-face
  '((t :inherit bold
	   :foreground "DeepSkyBlue4"
	   :background "gray80"))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-paragraph-heading-face
  '((t :inherit bold
	   :foreground "magenta4"
	   :background "gray80"))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-bold-face
  '((t :inherit bold))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-emph-face
  '((t :inherit italic))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-env-face
  '((t :background "LightGrey"))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-func-face
  '((t :inherit font-lock-function-name-face
	   :weight bold
	   :background "gray76"))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-func-prm-face
  '((t :inerit italic
	   :foreground "SaddleBrown"
	   :background "gray81"))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defface doconce-table-face
  '((t :inherit font-lock-string-face))
  "Used in `doconce-mode'"
  :group 'doconce-faces)

(defvar doconce-font-lock-keywords
  '(("\\(`.+?`\\)" 1 'doconce-code-face)
	("\\\\[\\($+\\)\\\\]" 1 'doconce-math-face)
	("\\$\\([^$]+\\)\\$" 1 'doconce-math-face)
	("\\(\\*.+?\\*\\)" 1 'doconce-emph-face)
	("\\(\s+_.+?_\s+\\)" 1 'doconce-bold-face)
	("\\(==+ *.+\\(\n.+\\)* *+==\\)" 1 'doconce-section-heading-face)
	("\\(__+ *.+\\(\n.+\\)* *+__\\)" 1 'doconce-paragraph-heading-face)
	("\\(^!b.*\\|^!e.*\\)" 1 'doconce-keyword-face)
	("!b.+\n\\(.*[^$]+?\\)!e.+" 1 'doconce-env-face)
	("\\(^#.*\\)" 1 'doconce-comment-face)
	("\\(\\*\\)" 1 'doconce-bold-face)
	("\\(\to \\)" 1 'doconce-bold-face)
	("\\(.+[^fromto]:.+\n\\)" 1 'doconce-func-prm-face)
	("\\(|.+|\\)" 1 'doconce-table-face)
	("\\(^.+CODE .+\\)" 1 'doconce-code-face)))

(defun doconce-section (level)
  (interactive "nSection level: ")
  (setq a 7)
  (cond
   ((= level 1) (setq a 7))
   ((= level 2) (setq a 5))
   ((= level 3) (setq a 3)))
  (setq s "")
  (setq b (+ (point) 1))
  (dotimes (i a)
	(setq s (concat s  "=")))
  (if (= level 4)
	  (progn
		(setq s "")
		(setq s (concat s "__"))))
  
  (if (region-active-p)
	  (progn
		(kill-region (region-beginning) (region-end))
		(insert (concat s " "))
		(yank))
	(progn
	  (beginning-of-line)
	  (insert (concat s " "))))
  (end-of-line)
  (insert (concat " " s))
  (goto-char (+ b a)))

(defun doconce-item-list ()
  (interactive)
  (let* ((txt (buffer-substring (mark) (point)))
		 (lines (split-string txt "\n")))
	(kill-region (mark) (point))
	(insert
	 (mapconcat '(lambda (s) (concat "* " s))
				lines
				"\n"))))

(defun doconce-enum-list ()
  (interactive)
  (let* ((txt (buffer-substring (mark) (point)))
		 (lines (split-string txt "\n")))
	(kill-region (mark) (point))
	(insert
	 (mapconcat '(lambda (s) (concat "o " s))
				lines
				"\n"))))


(defcustom doconce-project-path (directory-file-name "~/Projects/Doconce/doc/src/")
  "Doconce project directory"
  :group 'doconce
  :type '(string))

(defun doconce-project-chapters-path ()
  (directory-file-name (concat (file-name-as-directory doconce-project-path) "chapters")))

(defun doconce-mako-file-path ()
  (concat (file-name-as-directory (doconce-project-chapters-path)) "mako_code.txt"))

(defun doconce-project-book-path ()
  (directory-file-name (concat (file-name-as-directory doconce-project-path) "book")))

(defun doconce-book-file-path ()
  (concat (file-name-as-directory (doconce-project-book-path)) "book.do.txt"))

(defcustom doconce-author-name  "С.В. Лемешевский"
  "Doconce main author name"
  :group 'doconce
  :type '(string))

(defcustom doconce-author-email "sergey.lemeshevsky (at) gmail.com"
  "Doconce main author name"
  :group 'doconce
  :type '(string))

(defcustom doconce-author-institute "Институт математики НАН Беларуси"
  "Doconce main author institution"
  :group 'doconce
  :type '(string))

(defcustom doconce-chapter-nickname "chapter"
  "Doconce chapter nickname"
  :group 'doconce
  :type '(string))

(defcustom doconce-section-nickname "section"
  "Doconce chapter nickname"
  :group 'doconce
  :type '(string))

(defun doconce-create-mako-code-file ()
  (let ((mako-file-content (concat "# -*- coding: utf-8 -*-"
								   "## Mako variables and functions common to a lot of files"
								   "## (this file is included in, e.g., flow/main_flow.do.txt)."
								   "<%\n"
								   "# Note that goo.gl and bitly URLs cannot have slashes and continuation,\n"
								   "# therefore we use tinyurl.\n\n"
								   "src_path = 'http://tinyurl.com/zfwmann'\n"
								   "doc_path = 'http://tinyurl.com/hgqc9he'\n\n"
								   "#doc_index = doc_path + '/index.html'\n\n"
								   "# All chapters: nickname -> title dict\n"
								   "chapters = {\n"
								   "}\n\n"
								   "%>\n")))
	(write-region mako-file-content "" (doconce-mako-file-path))))

(defun doconce-add-chapter-to-mako-code-file (chapter-nickname chapter-title)
  (interactive "sChapter nickname: \n sTitle: ")
  (let* ((appended-text (concat " '" chapter-nickname "': u'" chapter-title "',\n")))
	(if (file-exists-p (doconce-mako-file-path))
		()
	  (doconce-create-mako-code-file))
	(progn  
	  (find-file (doconce-mako-file-path))
	  (goto-char (- (re-search-forward "\\(chapters = {\\(.*\n\\)+}\\)") 1))
	  (insert appended-text)
	  (save-buffer))))

(defun doconce-main-chapter-file-content (chapter-nickname chapter-title)
  (concat "TITLE: " chapter-title "\n"
		  "AUTHOR: " doconce-author-name
		  " Email: " doconce-author-email
		  " at " doconce-author-institute "\n"
		  "DATE: today\n\n\n"
		  "# Common Mako variable and functions\n"
		  "# #include \"../mako_code.txt\"\n\n"
		  "# #include \"" chapter-nickname ".do.txt\""))

(defun doconce-create-book-file (book-title)
  (interactive "sBook titile: ")
  (let ((book-file-content (concat "TITLE: " book-title "\n"
								   "AUTHOR: " doconce-author-name
								   " Email: " doconce-author-email
								   " at " doconce-author-institute "\n"
								   "DATE: today\n\n"
								   "## Handy mako variables and functions for the preprocessing step\n"
								   "# #include \"../chapters/mako_code.txt\"\n\n")))
		(progn
		   (make-directory (doconce-project-book-path))
		   (write-region book-file-content "" (doconce-book-file-path)))))

(defun doconce-add-chapter-to-book (chapter-nickname chapter-title)
  (if (file-exists-p (doconce-book-file-path))
	  ()
	(doconce-create-book-file "Название книги"))
  (progn
		(find-file (doconce-book-file-path))
		(end-of-buffer)
		(insert (concat "!split\n"
						"========= " chapter-title " =========\n"
						"label{ch:" chapter-nickname "}\n\n"
						"# #include \"../chapters/" chapter-nickname "/" chapter-nickname ".do.txt\"\n\n"))
		(save-buffer)))

(defun doconce-new-chapter (chapter-nickname chapter-title)
  (interactive "sChapter nickname: \nsTitle: ")
  (let* ((chapter-path (concat (file-name-as-directory (doconce-project-chapters-path)) chapter-nickname))
		 (file-path (concat (file-name-as-directory chapter-path) chapter-nickname ".do.txt"))
		 (main-file-path (concat  (file-name-as-directory chapter-path) "main_" chapter-nickname ".do.txt")))
	(progn
	  (make-directory chapter-path)
	  (make-directory (concat (file-name-as-directory chapter-path) "fig-" chapter-nickname))
	  (make-directory (concat (file-name-as-directory chapter-path) "slides-" chapter-nickname))
	  (make-directory (concat (file-name-as-directory chapter-path) "src-" chapter-nickname))
	  	  (make-directory (concat (file-name-as-directory chapter-path) "mov-" chapter-nickname))
	  (write-region (concat "# Local Variables:\n"
							"# doconce-chapter-nickname: \"" chapter-nickname "\"\n"
							"# End:") "" file-path)
	  (write-region (concat
					 "# Use a common ../make.sh file or do customized build here.\n"
					 "bash -x ../make.sh main_" chapter-nickname) "" (concat (file-name-as-directory chapter-path) "make.sh"))
	  (write-region (doconce-main-chapter-file-content chapter-nickname chapter-title) "" main-file-path t)
	  (doconce-add-chapter-to-mako-code-file chapter-nickname chapter-title)
	  (doconce-add-chapter-to-book chapter-nickname chapter-title)
	  (find-file main-file-path)
	  (setq doconce-chapter-nickname chapter-nickname))))

(defun doconce-create-section-file (chapter-nickname section-nickname section-title)
  (interactive "sChapter nickname: \nsSection nickname: \nsSection title: ")
  (progn
	(write-region (concat "======= " section-title " =======\n"
						  "label{" chapter-nickname ":" section-nickname "}\n\n\n"
						  "# Local Variables:\n"
						  "# doconce-chapter-nickname: \"" chapter-nickname "\"\n"
						  "# doconce-section-nickname: \"" section-nickname "\"\n"
						  "# End:\n")
				  ""
				  (concat section-nickname ".do.txt"))))

(defun doconce-new-section (section-nickname section-title)
  (interactive "sSection nickname: \nsSection title: ")
  (progn
	(doconce-create-section-file doconce-chapter-nickname section-nickname section-title)
	(insert (concat "# #include \"" section-nickname ".do.txt\"\n"))
	(save-buffer)))

(defun doconce-open-file ()
  (interactive)
  (copy-region-as-kill (line-beginning-position) (line-end-position))
  (let ((file-name
		 (car (last (split-string
					 (substring-no-properties (car kill-ring)))))))
	(find-file (expand-file-name file-name))))

(defvar doconce-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c d s") 'doconce-section)
	(define-key map (kbd "C-c d i") 'doconce-item-list)
	(define-key map (kbd "C-c d e") 'doconce-enum-list)
	(define-key map (kbd "C-c d o") 'doconce-open-file)
	(define-key map (kbd "C-c d a c") 'doconce-new-chapter)
	(define-key map (kbd "C-c d a s") 'doconce-new-section)	
	map)
  "Key map for `doconce'.")

(define-derived-mode doconce-mode text-mode "DocOnce"
  "Major mode for editing DocOnce source text
\\<doconce-mode-map>

Turning on `doconce-mode' calls the normal hooks `text-mode-hook'
and `doconce-mode-hook'. This mode also support font-lock highlighting.

\\{doconce-mode-map}"
;  :syntax-table doconce-mode-syntax-table
  :group 'doconce
  (setq-local font-lock-defaults
			  '(doconce-font-lock-keywords
				t nil nil nil
				(font-lock-multiline . t)
				(font-lock-mark-block-function . mark-paragraph)))
  (setq-local jit-lock-contextually t)

  ; Comments 
  (setq-local comment-start "# ")
  (setq-local comment-continue "   ")
  (setq-local comment-multi-line t)
  (setq-local comment-use-syntax nil)
  ;; DocOnce has not really a comment ender but nil is not really a
  ;; permissible value.
  (setq-local comment-end "")
  (setq-local comment-ensd-kip nil))

(add-to-list 'auto-mode-alist (cons "\\.do.txt\\'" 'doconce-mode))

(defvar doconce-keyword-list
  '("ref" "label" "cite" "idx" "begin" "end" "if" "endif" "split" "fromto" "at"))

(defvar doconce-funcname-list
  '("FIGURE" "MOVIE" "AUTHOR" "TITLE" "BIBFILE" "TOC" "DATE" "@@@CODE"))

(defun doconce-add-keywords (keyword-list facename)
  (let* ((keyword-regexp (concat "\\("
								  (regexp-opt keyword-list)
								  "\\)")))
	(font-lock-add-keywords 'doconce-mode
							`((,keyword-regexp 1 ',facename )))))

(defun doconce-add-crossref (keyword-list facename)
  (let* ((keyword-regexp (concat (regexp-opt keyword-list)
								 "\\({.+?}\\)")))
	(font-lock-add-keywords 'doconce-mode
							`((,keyword-regexp 1 ',facename)))))

(defun doconce-add-funcnames (keyword-list facename)
  (let* ((keyword-regexp (concat "^\\("
								 (regexp-opt keyword-list)
								 "\\)")))
	(font-lock-add-keywords 'doconce-mode
							`((,keyword-regexp 1 ',facename)))))

(doconce-add-keywords doconce-keyword-list 'doconce-keyword-face)
(doconce-add-funcnames doconce-funcname-list 'doconce-func-face)
(doconce-add-crossref doconce-keyword-list 'doconce-crossref-face)

(provide 'doconce-mode)
