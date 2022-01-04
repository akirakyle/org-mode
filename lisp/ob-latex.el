;;; ob-latex.el --- Babel Functions for LaTeX        -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2025 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; URL: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating LaTeX source code.
;;
;; Currently on evaluation this returns raw LaTeX code, unless a :file
;; header argument is given in which case small png or pdf files will
;; be created directly form the latex source code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'org-macs)

(declare-function org-create-formula-image "org" (string tofile options buffer &optional type))
(declare-function org-latex-compile "ox-latex" (texfile &optional snippet))
(declare-function org-latex-make-preamble "ox-latex" (info &optional template snippet?))
(declare-function org-splice-latex-header "org" (tpl def-pkg pkg snippets-p &optional extra))
(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-next-visible-heading "org" (arg))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("latex" . "tex"))

(defvar org-format-latex-header)	  ; From org.el
(defvar org-format-latex-options)	  ; From org.el
(defvar org-latex-default-packages-alist) ; From org.el
(defvar org-latex-packages-alist)	  ; From org.el
(defvar org-preview-latex-process-alist)  ; From org.el

(defvar org-babel-default-header-args:latex
  '((:results . "latex") (:exports . "results"))
  "Default arguments to use when evaluating a LaTeX source block.")

(defconst org-babel-header-args:latex
  '((border	  . :any)
    (fit          . :any)
    (imagemagick  . ((nil t)))
    (iminoptions  . :any)
    (imoutoptions . :any)
    (packages     . :any)
    (pdfheight    . :any)
    (pdfpng       . :any)
    (pdfwidth     . :any)
    (headers      . :any)
    (buffer       . ((yes no))))
  "LaTeX-specific header arguments.")

(defcustom org-babel-latex-htlatex "htlatex"
  "The htlatex command to enable conversion of LaTeX to SVG or HTML."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-latex-preamble
  (lambda (_)
    "\\documentclass[preview]{standalone}
\\def\\pgfsysdriver{pgfsys-tex4ht.def}
")
  "Closure which evaluates at runtime to the LaTeX preamble.

It takes 1 argument which is the parameters of the source block."
  :group 'org-babel
  :type 'function)

(defcustom org-babel-latex-begin-env
  (lambda (_)
    "\\begin{document}")
  "Function that evaluates to the begin part of the document environment.

It takes 1 argument which is the parameters of the source block.
This allows adding additional code that will be ignored when
exporting the literal LaTeX source."
  :group 'org-babel
  :type 'function)

(defcustom org-babel-latex-end-env
  (lambda (_)
    "\\end{document}")
  "Closure which evaluates at runtime to the end part of the document environment.

It takes 1 argument which is the parameters of the source block.
This allows adding additional code that will be ignored when
exporting the literal LaTeX source."
  :group 'org-babel
  :type 'function)

(defcustom org-babel-latex-pdf-svg-process
  "inkscape \
--pdf-poppler \
--export-area-drawing \
--export-text-to-path \
--export-plain-svg \
--export-filename=%O \
%f"
  "Command to convert a PDF file to an SVG file."
  :group 'org-babel
  :type 'string
  :package-version '(Org . "9.6"))

(defun org-babel-expand-body:latex (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapc (lambda (pair) ;; replace variables
          (setq body
                (replace-regexp-in-string
                 (regexp-quote (format "%S" (car pair)))
                 (if (stringp (cdr pair))
                     (cdr pair) (format "%S" (cdr pair)))
                 body t t)))
	(org-babel--get-vars params))
  (let ((prologue (cdr (assq :prologue params)))
        (epilogue (cdr (assq :epilogue params))))
    (org-trim
     (concat
      (and prologue (concat prologue "\n"))
      body
      (and epilogue (concat "\n" epilogue "\n"))))))

(defun org-babel-execute:latex (body params)
  "Execute LaTeX BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (setq body (org-babel-expand-body:latex body params))
  (if (cdr (assq :file params))
      (let* ((out-file (cdr (assq :file params)))
	     (extension (file-name-extension out-file))
	     (tex-file (org-babel-temp-file "latex-" ".tex"))
	     (border (cdr (assq :border params)))
	     (fit (or (cdr (assq :fit params)) border))
	     (height (and fit (cdr (assq :pdfheight params))))
	     (width (and fit (cdr (assq :pdfwidth params))))
	     (headers (cdr (assq :headers params)))
	     (in-buffer (not (string= "no" (cdr (assq :buffer params)))))
	     (imagemagick (cdr (assq :imagemagick params)))
	     (im-in-options (cdr (assq :iminoptions params)))
	     (im-out-options (cdr (assq :imoutoptions params)))
	     (org-latex-packages-alist
	      (append (cdr (assq :packages params)) org-latex-packages-alist))
	     (org-format-latex-header
	      (concat org-format-latex-header
		      (mapconcat #'identity (cdr (assq :headers params)) "\n")
		      (if fit "\n\\usepackage[active, tightpage]{preview}\n" "")
		      (if border
			  (format "\\setlength{\\PreviewBorder}{%s}" border) "")
		      (if height
			  (concat "\n" (format "\\pdfpageheight %s" height)) "")
		      (if width
			  (concat "\n" (format "\\pdfpagewidth %s" width)) "")))
	     (body (if fit 
		       (concat "\n\\begin{preview}\n" body "\n\\end{preview}\n")
		     body)))
        (cond
         ((and (string-suffix-p ".png" out-file) (not imagemagick))
          (let ((org-format-latex-header
		 (concat org-format-latex-header "\n"
			 (mapconcat #'identity headers "\n"))))
	   (org-create-formula-image
            body out-file org-format-latex-options in-buffer 'dvipng)))
         ((and (string= "svg" extension) (not imagemagick))
          (org-create-formula-image
           body out-file org-format-latex-options in-buffer 'dvisvgm))
         ((string-suffix-p ".tex" out-file)
	  (when (file-exists-p out-file) (delete-file out-file))
	  (with-temp-file out-file
	    (insert body)))
	 ((and (string= "html" extension) (not imagemagick)
	       (executable-find org-babel-latex-htlatex))
	  (let ((org-format-latex-header
		 (concat org-format-latex-header
			 "\\def\\pgfsysdriver{pgfsys-tex4ht.def}")))
	    (org-babel-latex-format-tex tex-file body))
	  (when (file-exists-p out-file) (delete-file out-file))
	  (let ((default-directory (file-name-directory tex-file)))
	    (shell-command (format "%s %s" org-babel-latex-htlatex tex-file)))
	  (cond
	   ((file-exists-p (concat (file-name-sans-extension tex-file) "-1.svg"))
	    (if (string-suffix-p ".svg" out-file)
		(progn
		  (shell-command "pwd")
                  (rename-file (concat (file-name-sans-extension tex-file) "-1.svg")
                               out-file t))
	      (error "SVG file produced but HTML file requested")))
	   ((file-exists-p (concat (file-name-sans-extension tex-file) ".html"))
	    (if (string-suffix-p ".html" out-file)
                (rename-file (concat (file-name-sans-extension tex-file) ".html")
                             out-file t)
              (error "HTML file produced but SVG file requested")))))
	 ((or (string= "pdf" extension) imagemagick)
	  (org-babel-latex-format-tex tex-file body)
	  (let ((default-directory (file-name-directory tex-file)))
	    (org-latex-compile tex-file))
	  (let ((transient-pdf-file
		 (concat (file-name-sans-extension tex-file) ".pdf")))
	    (when (file-exists-p out-file) (delete-file out-file))
	    (cond
	     ((string= "pdf" extension)
	      (rename-file transient-pdf-file out-file))
	     (imagemagick
	      (org-babel-latex-convert-pdf
	       transient-pdf-file out-file im-in-options im-out-options)))))
	 (t
	  (error "Can't create %s files, please specify a .tex, .pdf, .png, or .svg file, or try the :imagemagick header argument"
		 extension)))
        nil) ;; signal that output has already been written to file
    body))

(defun org-babel-latex-format-tex (tex-file body)
  "Generate a temporary tex file from execute params."
  (with-temp-file tex-file
    (insert
     (org-latex-make-preamble
      (org-export-get-environment (org-export-get-backend 'latex))
      org-format-latex-header)
     (concat "\n\\begin{document}\n" body "\n\\end{document}\n"))))

(defun org-babel-latex-convert-pdf (pdffile out-file im-in-options im-out-options)
  "Generate OUT-FILE from PDFFILE using imagemagick.
IM-IN-OPTIONS are command line options for input file, as a string;
and IM-OUT-OPTIONS are the output file options."
  (let ((cmd (concat "convert " im-in-options " " pdffile " "
		     im-out-options " " out-file)))
    (message "Converting pdffile file %s..." cmd)
    (shell-command cmd)))

(defun org-babel-prep-session:latex (_session _params)
  "Return an error because LaTeX doesn't support sessions."
  (error "LaTeX does not support sessions"))

(provide 'ob-latex)

;;; ob-latex.el ends here
