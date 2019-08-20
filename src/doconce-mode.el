(require 'font-lock)
(require 'outline)
(require 'button)
(require 'org)

(defconst doconce-mode-version "0.1"
  "Doconce mode version number.")

(defvar doconce-mode-hook nil
  "Hook run when entering Doconce mode.")

(defgroup doconce nil
  "Major mode for the Doconce markup language."
  :prefix "doconce-"
  :group 'wp)

;;;###autoload
(add-to-list 'auto-mode-alist (cons (purecopy "\\.do.txt\\'") 'doconce-mode))

;;; Font lock

(defvar doconce-emphasized-face 'doconce-emphasized-face
  "Face name to use for emphasized words.")

(defvar doconce-code-face 'doconce-code-face
  "Face name to use for code.")

(defvar doconce-inline-code-face 'doconce-inline-code-face
  "Face name to use for inline code.")

(defvar doconce-list-face 'doconce-list-face
  "Face name to use for lists.")

(defvar doconce-header-face 'doconce-header-face
  "Face name to use for level-1 headers.")

(defvar doconce-footnote-face 'doconce-footnote-face
  "Face name to use for footnote identifiers.")

(defvar doconce-link-face 'doconce-link-face
  "Face name to use for links.")

(defvar doconce-comment-face 'doconce-comment-face
  "Face name to use for comments.")

(defvar doconce-math-face 'doconce-math-face
  "Face name to use for LaTeX expressions.")

(defvar doconce-special-lines-face 'doconce-special-lines-face
  "Face name to use for special lines.")

(defvar doconce-admonition-face 'doconce-admonition-face
  "Face name to use for admonitions.")

(defgroup doconce-faces nil
  "Faces used in Doconce Mode"
  :group 'doconce
  :group 'faces)

(defface doconce-emphasized-face
  '((t (:slant italic)))
  "Face for emphasized words."
  :group 'doconce-faces)

(defface doconce-header-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Base face for headers."
  :group 'doconce-faces)

(defface doconce-list-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for list item markers."
  :group 'doconce-faces)

(defface doconce-code-face
  '((t (:inherit font-lock-constant-face)))
  "Face for inline code."
  :group 'doconce-faces)

(defface doconce-reference-face
  '((t (:inherit font-lock-type-face)))
  "Face for link references."
  :group 'doconce-faces)

(defface doconce-footnote-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for footnote markers."
  :group 'doconce-faces)

(defface doconce-link-face
  '((t (:inherit link)))
  "Face for links."
  :group 'doconce-faces)

(defface doconce-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for Doconce comments."
  :group 'doconce-faces)

(defface doconce-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'doconce-faces)

(defface doconce-special-lines-face
  '((t (:inherit doconce-comment-face :underline t)))
  "Face for admonitions."
  :group 'doconce-faces)

(defface doconce-admonition-face
  '((t (:inherit doconce-header-face)))
  "Face for admonitions."
  :group 'doconce-faces)

(defconst doconce-regex-emphasized
  "\\*\\(.*\\)\\*"
  "Regular expression for emphasized words.")

(defconst doconce-regex-footnote
  "\\[\\^.+?\\]"
  "Regular expression for a footnote marker [^fn].")

(defconst doconce-regex-header
  "^\\(========= .* =========\\|======= .* =======\\|===== .* =====\\|=== .* ===\\)$"
  "Regexp identifying Doconce headers.")

(defconst doconce-regex-admonitions
  "^__.+?[.:?]__"
  "Regexp identifying Doconce admonitions.")

(defconst doconce-regex-list
  "^[ \t]*\\([o*-]\\)[ \t]+"
  "Regular expression for matching lists.")

(defconst doconce-regex-code
  "\\(^!bc\\([^§]+?\\)!ec\\)"
  "Regular expression for matching code blocks.")

(defconst doconce-regex-code-file
  "^@@@CODE .+"
  "Regular expression for matching code from file.")

(defconst doconce-regex-inline-code
  "`.+?`"
  "Regular expression for matching inline code fragments.")

(defconst doconce-regex-email
  "<\\(\\(\\sw\\|\\s_\\|\\s.\\)+@\\(\\sw\\|\\s_\\|\\s.\\)+\\)>"
  "Regular expression for matching inline email addresses.")

(defconst doconce-regex-link
  "\\(\".*\"\\):[ \t\n]*\\(\".*\"\\)"
  "Regular expression for matching links.")

(defconst doconce-regex-comment
  "#.+$"
  "Regular expression for matching comments.")

(defconst doconce-regex-math-inline
  "\\(^\\|[^\\]\\)\\(\\$\\($\\([^\\$]\\|\\\\.\\)*\\$\\|\\([^\\$]\\|\\\\.\\)*\\)\\$\\)"
  "Regular expression for inline LaTeX.")

(defconst doconce-regex-math-display
  "\\(^!bt\\([^§]+?\\)!et\\)"
  "Regular expression for LaTeX equations.")

(defconst doconce-regex-special-lines
  "\\(^\\(FIGURE\\|MOVIE\\|AUTHOR\\|TITLE\\|DATE\\|BIBFILE\\):.+$\\)"
  "Regular expression for special lines.")

(defconst doconce-regex-reference
  "ref\{.+?\}"
  "Regular expression for references.")

(defvar doconce-keywords
  '("!bt" "!et" "!bc" "!ec" "!bwarning" "!ewarning" "!bquote" "!equote"
    "!bnotice" "!enotice" "!bsummary" "!esummary" "!bquestion" "!equestion"
    "!bblock" "!eblock" "!bbox" "!ebox" "!bsubex" "!esubex" "!bhint"
    "!ehint" "!bsol" "!esol" "!bans" "!eans" "!bremarks" "eremarks" "!bpop"
    "!epop" "!bslidecell" "!eslidecell" "idx" "TOC" "label" "cite" "ref"))

(defvar doconce-mode-font-lock-keywords-basic
  (list
   (cons doconce-regex-comment 'doconce-comment-face)
   (cons doconce-regex-header 'doconce-header-face)
   (cons doconce-regex-admonitions 'doconce-header-face)
   (cons doconce-regex-emphasized '(1 doconce-emphasized-face))
   (cons doconce-regex-math-inline '(2 doconce-code-face))
   (cons doconce-regex-math-display '(2 doconce-code-face))
   (cons doconce-regex-code '(2 doconce-code-face))
   (cons doconce-regex-code-file 'doconce-special-lines-face)
   (cons doconce-regex-inline-code 'doconce-code-face)
   (cons doconce-regex-email 'doconce-url-face)
   (cons doconce-regex-link '((1 doconce-math-face)
                              (2 doconce-link-face)))
   (cons doconce-regex-list '(1 doconce-footnote-face))
   (cons doconce-regex-footnote 'doconce-footnote-face)
   (cons doconce-regex-special-lines '(1 doconce-special-lines-face)))
  "Syntax highlighting for Doconce files.")

;;; Defuns:

(defun doconce-insert-link ()
  "An interactive function that inserts a link."
  (interactive)
  (let ((desc (read-string
               "Description: " nil 'minibuffer-history "description"))
        (link (read-string
               "Link: " nil 'minibuffer-history "http://"))
        (point (point)))
    (insert "\"" desc "\": \"")
    (insert-text-button link 'follow-link t)
    (insert "\"")))

(defun doconce-insert-heading ()
  "An interactive function that inserts a Doconce heading."
  (interactive)
  (let ((level (save-excursion
                 (condition-case nil
                     (and (outline-back-to-heading t)
                          (doconce-outline-level)) (error 2)))))
    (unless (= (point-at-bol) (point-at-eol))
      (beginning-of-line) (open-line 1))
    (insert (car (rassoc level outline-heading-alist)))
    (search-backward "  " (point-at-bol) t)))

(defun doconce-get-list-str ()
  "Returns the closest list-prefix or a string with white space
only. It calls itself recursively checking previous lines for
either a list item or a white space indicating a paragraph."
  (goto-char (point-at-bol))
  (cond ((bobp) "")
        ((and (not (looking-at doconce-regex-header))
              (not (looking-at doconce-regex-list))
              (not (looking-at "^[ \t]*$")))
         (previous-line)
         (let ((res (doconce-get-list-str)))
           (forward-line) res))
        (t (buffer-substring (match-beginning 0) (match-end 0)))))

(defun doconce-insert-list-item-or-header ()
  "Insert a new list item if the point is inside a list structure."
  (interactive)
  (let ((empty-line
         (string-match-p
          "^[ \t]*$" (buffer-substring (point-at-bol) (point-at-eol))))
        (pos (point)))
    ;; If the line is empty we could replace this with a list-item.
    (when empty-line (previous-line))
    (let* ((list-str (doconce-get-list-str))
           (list-str (replace-regexp-in-string
                      "=+ \\(.*\\) =+" "" list-str nil nil 1)))
      ;; If the recursion stopped at an empty line we're not in a list.
      (if (string-match-p "^[ \t]*$" list-str)
          (goto-char pos)
        (end-of-line)
        ;; Either we're replacing an empty line, or we make a new one.
        (if empty-line (forward-line) (newline))
        (insert list-str)))))

(defun doconce-outline-level ()
  "Function that takes no args to compute a header's nesting level in an
outline. It assumes the point is at the beginning of a header line and that
the match data reflects the `outline-regexp'."
  (save-excursion
    (let ((level-1 (make-string 9 ?=))
          (level-2 (make-string 7 ?=))
          (level-3 (make-string 5 ?=))
          (level-4 (make-string 3 ?=)))
      (or (and (search-forward level-1 (point-at-eol) t 2) 1)
          (and (search-forward level-2 (point-at-eol) t 2) 2)
          (and (search-forward level-3 (point-at-eol) t 2) 3)
          (and (search-forward level-4 (point-at-eol) t 2) 4)))))

(defun doconce-promote ()
  "Promote the current heading higher up the tree."
  (interactive)
  (outline-back-to-heading t)
  (let* ((head (buffer-substring-no-properties
                (point-at-bol) (point-at-eol)))
         (new-head (concat "==" head "==")))
    (when (string-match-p doconce-regex-header new-head)
      (delete-char (- (point-at-eol) (point-at-bol)))
      (insert new-head))))

(defun doconce-demote ()
  "Demote the current heading lower down the tree."
  (interactive)
  (outline-back-to-heading t)
  (let* ((head (buffer-substring-no-properties
                (point-at-bol) (point-at-eol)))
         (new-head (and (> (length head) 4) (substring head 2 -2))))
    (when (and new-head (string-match-p doconce-regex-header new-head))
      (delete-char (- (point-at-eol) (point-at-bol)))
      (insert new-head))))

(defun doconce-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `doconce-enter-key' or
`doconce-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `doconce-enter-key', by an initial call of
`doconce-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position.
Positions are calculated by `doconce-calc-indents'."
  (interactive)
  (let ((positions (doconce-calc-indents))
        (cur-pos (current-column)))
    (if (not (equal this-command 'doconce-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (indent-line-to
       (doconce-indent-find-next-position cur-pos positions)))))

(defun doconce-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (unless (looking-back "\n\n")
      (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
        (goto-char font-lock-end)
        (when (re-search-forward "\n\n" nil t)
          (setq font-lock-end (match-beginning 0))
          (setq font-lock-beg found))))))

(defun doconce-reload-extensions ()
  "Check settings, update font-lock keywords, and re-fontify buffer."
  (interactive)
  (when (eq major-mode 'doconce-mode)
    (setq doconce-mode-font-lock-keywords
          (append
           doconce-mode-font-lock-keywords-basic
           doconce-keywords))
    (setq font-lock-defaults (list doconce-mode-font-lock-keywords))
    (font-lock-refresh-defaults)))

;;; Outline

(defvar doconce-cycle-global-status 1)
(defvar doconce-cycle-subtree-status nil)

(defun doconce-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.
Derived from `org-end-of-subtree'."
  (outline-back-to-heading invisible-OK)
  (let ((first t)
        (level (funcall outline-level)))
    (while (and (not (eobp))
                (or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

(defun doconce-cycle (&optional arg)
  "Visibility cycling for Doconce mode.
If ARG is t, perform global visibility cycling.  If the point is
at an atx-style header, cycle visibility of the corresponding
subtree.  Otherwise, insert a tab using `indent-relative'.
Derived from `org-cycle'."
  (interactive "P")
  (cond
   ((eq arg t) ;; Global cycling
    (cond
     ((and (eq last-command this-command)
           (eq doconce-cycle-global-status 2))
      ;; Move from overview to contents
      (hide-sublevels 1)
      (message "CONTENTS")
      (setq doconce-cycle-global-status 3))

     ((and (eq last-command this-command)
           (eq doconce-cycle-global-status 3))
      ;; Move from contents to all
      (show-all)
      (message "SHOW ALL")
      (setq doconce-cycle-global-status 1))
     (t
      ;; Defaults to overview
      (hide-body)
      (message "OVERVIEW")
      (setq doconce-cycle-global-status 2))))
   ((save-excursion (beginning-of-line 1) (looking-at outline-regexp))
    ;; At a heading: rotate between three different views
    (outline-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (outline-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (outline-end-of-heading)   (setq eoh (point))
        (doconce-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ((= eos eoh)
        ;; Nothing is hidden behind this heading
        (message "EMPTY ENTRY")
        (setq doconce-cycle-subtree-status nil))
       ((>= eol eos)
        ;; Entire subtree is hidden in one line: open it
        (show-entry)
        (show-children)
        (message "CHILDREN")
        (setq doconce-cycle-subtree-status 'children))
       ((and (eq last-command this-command)
             (eq doconce-cycle-subtree-status 'children))
        ;; We just showed the children, now show everything.
        (show-subtree)
        (message "SUBTREE")
        (setq doconce-cycle-subtree-status 'subtree))
       (t
        ;; Default action: hide the subtree.
        (hide-subtree)
        (message "FOLDED")
        (setq doconce-cycle-subtree-status 'folded)))))
   (t
    (indent-for-tab-command))))

(defun doconce-shifttab ()
  "Global visibility cycling.
Calls `doconce-cycle' with argument t."
  (interactive)
  (doconce-cycle t))

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


(defun doconce-set-bookmark (level)
  (interactive "nLevel:")
  (setq s "+")
  (dotimes (i level)
	(setq s (concat s  "-")))
  (copy-region-as-kill (line-beginning-position) (line-end-position))
  (bookmark-set  (concat s (car (cdr (split-string (substring-no-properties (car kill-ring)) "=+"))))))

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
		      (concat section-nickname ".do.txt"))
	))

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
		 (car (cdr (split-string
			    (substring-no-properties (car kill-ring)) "\"")))))
	(find-file (expand-file-name file-name))))

(defvar doconce-mode-map
  (let ((map (make-keymap)))
    ;; Visibility cycling
    (define-key map (kbd "TAB") 'doconce-cycle)
    (define-key map (kbd "<tab>") 'doconce-cycle)
    (define-key map (kbd "<S-iso-lefttab>") 'doconce-shifttab)
    (define-key map (kbd "<S-tab>")  'doconce-shifttab)
    (define-key map (kbd "<backtab>") 'doconce-shifttab)

    (define-key map (kbd "M-<right>") 'doconce-promote)
    (define-key map (kbd "M-<left>") 'doconce-demote)

    (define-key map (kbd "M-<return>")  'doconce-insert-list-item-or-header)
    (define-key map (kbd "M-RET")  'doconce-insert-list-item-or-header)

    (define-key map (kbd "C-c C-l") 'doconce-insert-link)
    (define-key map (kbd "C-c C-o") 'org-open-at-point)

    (define-key map [remap outline-insert-heading] 'doconce-insert-heading)
    (define-key map [remap outline-promote] 'doconce-promote)
    (define-key map [remap outline-demote] 'doconce-demote)

    (define-key map (kbd "C-c d i") 'doconce-item-list)
    (define-key map (kbd "C-c d e") 'doconce-enum-list)
    (define-key map (kbd "C-c d o") 'doconce-open-file)
    (define-key map (kbd "C-c d a c") 'doconce-new-chapter)
    (define-key map (kbd "C-c d a s") 'doconce-new-section)
    (define-key map (kbd "C-c d s b") 'doconce-set-bookmark)

    map)
  "Keymap for Doconce major mode.")

(define-derived-mode doconce-mode outline-mode "Doconce"
  "Major mode for the Doconce markup language.
\\{doconce-mode-map}"
  
  (set (make-local-variable 'comment-start) "## ")
  (set (make-local-variable 'comment-start-skip) "##+\\s-*")

  (set (make-local-variable 'doconce-mode-font-lock-keywords) nil)
  (set (make-local-variable 'font-lock-defaults) nil)
  (set (make-local-variable 'font-lock-multiline) t)

  (set (make-local-variable 'outline-regexp) doconce-regex-header)
  (set (make-local-variable 'outline-level) 'doconce-outline-level)
  (setq outline-heading-alist '(("=======   =======" . 2)
                                ("=====   =====" . 3)
                                ("===   ===" . 4)
                                ("=========   =========" . 1)))
  (doconce-reload-extensions)
  (add-hook 'font-lock-extend-region-functions
            'doconce-font-lock-extend-region))

(provide 'doconce-mode)
