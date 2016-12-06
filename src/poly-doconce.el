(load-file "~/.emacs.d/src/doconce-mode.el")
(require 'polymode)

;; 1. Define hostmode object
(defcustom pm-host/doconce
  (pm-bchunkmode "DocOnce" :mode 'doconce-mode)
  "Doconce host chunkmode"
  :group 'hostmodes
  :type 'object)

;; 2. Define innermode object
(defcustom  pm-inner/latex
  (pm-hbtchunkmode-auto "latex"
				   :head-reg "\\(^!bt.*\n\\)"
				   :tail-reg "\\(^!et\\)"
				   :mode 'LaTeX-mode
				   :font-lock-narrow t)
  "Doconce latex chunk."
  :group 'innermodes
  :type 'object)

(defcustom  pm-inner/python
  (pm-hbtchunkmode-auto "python"
				   :head-reg "\\(^!bc pycod\n\\)"
				   :tail-reg "\\(^!ec\\)"
				   :mode 'python-mode
				   :font-lock-narrow t)
  "Doconce python chunk."
  :group 'innermodes
  :type 'object)

(defcustom  pm-inner/cpp
  (pm-hbtchunkmode-auto "cpp"
				   :head-reg "\\(^!bc cppcod\n\\)"
				   :tail-reg "\\(^!ec\\)"
				   :mode 'c++-mode
				   :font-lock-narrow t)
  "Doconce cpp chunk."
  :group 'innermodes
  :type 'object)

;; 3. Define polymode object
(defcustom pm-poly/doconce
  (pm-polymode-multi "DocOnce"
				   :hostmode 'pm-host/doconce
				   :innermodes '(pm-inner/latex pm-inner/python pm-inner/cpp))
  "Doconce typical configuration"
  :group 'polymodes
  :type 'object)

;; 4. Define polymode function
(define-polymode poly-doconce-mode pm-poly/doconce)
(paragraph-indent-minor-mode t)
(provide 'poly-doconce)
