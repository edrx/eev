;;; eev-pdflike.el -- hyperlinks to documents made of pages.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012-2023 Free Software Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    20240125
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-pdflike.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-pdflike.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                 <http://anggtwu.net/eev-intros/find-pdf-like-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-pdf-like-intro)

;;; Commentary:

;; The main ideas in this file are explained here:
;;
;;   (find-pdf-like-intro)
;;
;;
;;
;; 1. DEFINING VIEWERS
;; ===================
;; Let's suppose that I have just installed a new PDF viewer, called
;; FOOPDF, on my machine. My old method for implementing support for
;; it on eev would be to define a function `find-FOOPDF-page' "by
;; hand", like this:
;; 
;;   (defun find-FOOPDF-page (fname &optional page &rest rest)
;;      (find-bgprocess (list "/usr/bin/FOOPDF" fname "--page" page)))
;; 
;; After a while I realized that it would be better to have several
;; functions associated to FOOPDF, all of them calling indirectly a
;; function called `ee-find-FOOPDF-page'; this `ee-find-FOOPDF-page'
;; would receive a file name and a page number and just return a list
;; with the name of the FOOPDF program followed by the arguments to be
;; given to it. I would define it as:
;; 
;;   (defun ee-find-FOOPDF-page (fname page)
;;     (list "/usr/bin/FOOPDF" fname "--page" page))
;; 
;; All the other `*FOOPDF*' functions would be "mass produced", as in
;; `code-c-d',
;; 
;;   (find-eev-quick-intro "9.1. `code-c-d'")
;;   (find-eev-quick-intro "9.1. `code-c-d'" "find-code-c-d")
;; 
;; but by a function called `code-pdfbackend'. To inspect the code
;; that a `code-pdfbackend' would execute, add a "find-" to it, like
;; this:
;; 
;;   (find-code-pdfbackend "FOOPDF-page")
;; 
;; Note that the `*FOOPDF*' functions are related like this:
;; 
;;                             find-FOOPDF-page --> ee-find-FOOPDF-page
;;                                    ^
;;                                    :
;;     code-FOOPDF-page --> ee-code-FOOPDF-page      
;;                                    ^
;;                                    |
;;                        find-code-FOOPDF-page
;; 
;; Here each arrow `-->' means "calls" and the vertical dotted arrow
;; means "produces elisp code mentioning".
;;
;; The function `code-pdftextbackend' is similar to `code-pdfbackend',
;; but for functions like `find-pdf-text'. Try:
;;
;;   (find-code-pdftextbackend    "SHOWTEXT-text")
;;
;; Note: `code-pdfbackend' and `code-pdftextbackend' are replacements
;; for these old functions, that were very badly documented and had
;; cryptic names:
;;
;;   (find-code-xxxpdf-family     "FOOPDF-page")
;;   (find-code-xxxpdftext-family "SHOWTEXT-page")
;;
;;
;;
;; 2. ALIASES
;; ==========
;; The main functions for opening PDFs with different viewers are
;; these ones:
;;
;;   find-xpdf-page
;;   find-pdftotext-text
;;
;; and these "generic" functions are aliased to them:
;;
;;   find-pdf-page
;;   find-pdf-text
;;
;; These aliases are also mass-produced. Try:
;;
;;   (find-code-pdfbackendalias "PDF-PAGE" "XPDF-PAGE")
;;
;; `code-pdfbackendalias' is a replacement for `code-xxxpdf-alias':
;;
;;   (find-code-xxxpdf-alias "PDF-TEXT" "PDFTOTEXT-TEXT")
;;
;;
;;
;; 3. OTHER VIEWERS
;; ================
;; This file also implement these other "viewers":
;;
;;   find-djview-page
;;   find-djvutxt-text
;;   find-evince-page
;;   find-gv-page
;;   find-pdftools-page
;;   find-texworkspdf-page
;;   find-xdvi-page
;;
;; Some of them work on other "PDF-like" formats, like DVI, PS, and
;; DJVU. My definition of "PDF-like" is explained here:
;;
;;   (find-pdf-like-intro "1. PDF-like documents")
;;
;;
;;
;; 4. A CONVENTION ON HYPHENS
;; ==========================
;; Note that if we run
;;
;;   (find-code-pdf-page "livesofanimals" "/tmp/Coetzee99.pdf")
;;   (find-code-pdf-text "livesofanimals" "/tmp/Coetzee99.pdf")
;;        (code-pdf-page "livesofanimals" "/tmp/Coetzee99.pdf")
;;        (code-pdf-text "livesofanimals" "/tmp/Coetzee99.pdf")
;;
;; this defines the functions `find-livesofanimalspage' and
;; `find-livesofanimalstext'. I am trying to follow a convention in
;; which function with two hyphens in their names, like
;; `find-xpdf-page', are basic hyperlink functions like the ones
;; defined in this file, and functions with only one hyphen in their
;; names, like `find-livesofanimalspage', are short hyperlinks, as in:
;;
;;   (find-eev-quick-intro "9.4. Shorter hyperlinks to PDF files")
;;   (find-pdf-like-intro "7. Shorter hyperlinks to PDF files")


;; «.find-sh-page»			(to "find-sh-page")
;; «.ee-goto-position-page»		(to "ee-goto-position-page")
;;
;; «.code-pdfbackend»			(to "code-pdfbackend")
;; «.code-pdftextbackend»		(to "code-pdftextbackend")
;; «.code-pdfbackendalias»		(to "code-pdfbackendalias")
;;
;; Obsolete functions, replaced by the `*backend*'s:
;;   «.code-xxxpdf-family»		(to "code-xxxpdf-family")
;;   «.code-xxxpdftext-family»		(to "code-xxxpdftext-family")
;;   «.code-xxxpdf-alias»		(to "code-xxxpdf-alias")
;;
;; «.find-pdf-page»			(to "find-pdf-page")
;;   «.change-default-viewer»		(to "change-default-viewer")
;; «.find-pdf-page-windows»		(to "find-pdf-page-windows")
;; «.find-pdf-text»			(to "find-pdf-text")
;;
;; «.find-xpdf-page»			(to "find-xpdf-page")
;; «.find-pdftotext-text»		(to "find-pdftotext-text")
;; «.find-pdftotext8-text»		(to "find-pdftotext8-text")
;; «.find-texworkspdf-page»		(to "find-texworkspdf-page")
;; «.find-pdftools-page»		(to "find-pdftools-page")
;; «.find-pdftoolsr-page»		(to "find-pdftoolsr-page")
;; «.find-xdvi-page»			(to "find-xdvi-page")
;; «.find-djview-page»			(to "find-djview-page")
;; «.find-evince-page»			(to "find-evince-page")
;; «.find-mupdfpage»			(to "find-mupdfpage")
;; «.find-okularpage»			(to "find-okularpage")
;; «.find-gv-page»			(to "find-gv-page")
;; «.find-djvutxt-text»			(to "find-djvutxt-text")
;; «.find-firefox-page»			(to "find-firefox-page")
;; «.find-googlechrome-page»		(to "find-googlechrome-page")
;;   «.ee-fname-page-to-url»		(to "ee-fname-page-to-url")
;;
;; «.aliases»				(to "aliases")
;; «.code-brxxxs»			(to "code-brxxxs")



(require 'eev-plinks)			; (find-eev "eev-plinks.el")
(require 'eev-brxxx)			; (find-eev "eev-brxxx.el")

;; Autoloads for external functions.
;; See: (find-elnode "Autoload")
;; See also my temporary hack in:
;;   (find-eev "eev-pdflike.el" "find-pdftools-page")
;;   (find-eev "eev-pdflike.el" "find-pdftools-page" "require")
;;(autoload 'pdf-loader-install "pdf-tools")



;;;                  _       _     _           
;;; __   ____ _ _ __(_) __ _| |__ | | ___  ___ 
;;; \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;;  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;;   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;
;; Global variables used by `M-h M-p'. See:
;;   (find-pdf-like-intro "11. How `M-h M-p' guesses everything")
;;   (find-efunction 'find-pdflike-page-links)
;;
(defvar ee-page-c      "{?}" "Internal, used by `find-pdflike-page-links'.")
(defvar ee-page-fname  "{?}" "Internal, used by `find-pdflike-page-links'.")
(defvar ee-page-offset   0   "Internal, used by `find-pdflike-page-links'.")




;;;   __ _           _           _                                  
;;;  / _(_)_ __   __| |      ___| |__        _ __   __ _  __ _  ___ 
;;; | |_| | '_ \ / _` |_____/ __| '_ \ _____| '_ \ / _` |/ _` |/ _ \
;;; |  _| | | | | (_| |_____\__ \ | | |_____| |_) | (_| | (_| |  __/
;;; |_| |_|_| |_|\__,_|     |___/_| |_|     | .__/ \__,_|\__, |\___|
;;;                                         |_|          |___/      
;;
;; «find-sh-page» (to ".find-sh-page")
(defun find-sh-page (program-and-args &rest pos-spec-list)
  "Like `find-sh', but interpreting the car of POS-SPEC-LIST as a page number."
  (interactive "sShell command: ")
  (find-eoutput-reuse
   (ee-unsplit program-and-args)
   `(insert (ee-pdftotext-replace-bad-ffs
	     (find-callprocess00 ,'program-and-args))))
  (apply 'ee-goto-position-page pos-spec-list))

;; «ee-goto-position-page» (to ".ee-goto-position-page")
;; See: (find-refining-intro "1. Pos-spec-lists")
;;
(defun ee-goto-position-page (&optional pos-spec &rest rest)
  "Like `ee-goto-position', but interpreting a number as a page number.
\(Note that POS-SPEC is only interpreted as a page if it is a number.)"
  (when pos-spec
    (cond ((numberp pos-spec)
	   (goto-char (point-min))
	   (re-search-forward "[\f]" nil nil (1- pos-spec)))
	  ((stringp pos-spec)
	   (goto-char (save-excursion	          ; This used to be just:
			(goto-char (point-min))	  ; (goto-char (point-min))
			(search-forward pos-spec) ; (search-forward pos-spec)
			(point))))		  ;
	  (t (error "This is not a valid pos-spec: %S" pos-spec)))
    (if rest (ee-goto-rest rest))))

(defun ee-pdftotext-replace-bad-ffs (bigstr)
"Convert formfeeds that are preceded by non-newline chars into something else.
Sometimes pdftotext return \"spurious formfeeds\" that correspond
not to page breaks but to special printable characters, and these
spurious formfeeds confuse `ee-goto-position-page'. This function
finds sequence of spurious formfeeds using a heuristic that works
in most cases - formfeeds following something that is not a
newline are spurious - and replaces them by \"(ff)\"."
  (replace-regexp-in-string  
   "\\([^\n\f]\\)\\(\f+\\)" "\\1(ff)" bigstr t))





;;;                _                 _                _                  _      
;;;   ___ ___   __| | ___      __/\_| |__   __ _  ___| | _____ _ __   __| |_/\__
;;;  / __/ _ \ / _` |/ _ \_____\    / '_ \ / _` |/ __| |/ / _ \ '_ \ / _` \    /
;;; | (_| (_) | (_| |  __/_____/_  _\ |_) | (_| | (__|   <  __/ | | | (_| /_  _\
;;;  \___\___/ \__,_|\___|       \/ |_.__/ \__,_|\___|_|\_\___|_| |_|\__,_| \/  
;;;                                                                             

;; «code-pdfbackend»  (to ".code-pdfbackend")
;; See: (find-eevfile "eev-pdflike.el" "1. DEFINING VIEWERS")
;; Test: (find-code-pdfbackend "FOOVIEW")
;;
(defun      code-pdfbackend (pdfbackend)
  (eval (ee-read      (ee-code-pdfbackend pdfbackend))))
(defun find-code-pdfbackend (pdfbackend)
  (find-estring-elisp (ee-code-pdfbackend pdfbackend)))
(defun   ee-code-pdfbackend (pdfbackend)
  (ee-template0 "\
;; (find-code-pdfbackend \"{pdfbackend}\")
;;      (code-pdfbackend \"{pdfbackend}\")
;; (find-efunction 'find-code-pdfbackend)
;;
;; This is similar to a `code-c-d' - see:
;;   (find-eev-quick-intro \"9.1. `code-c-d'\")
;;   (find-eev-quick-intro \"9.1. `code-c-d'\" \"find-code-c-d\")
;; but it defines the four functions in the left in the diagram below:
;;
;; The code here defines this structure:
;;
;;                           find-{pdfbackend} --> ee-find-{pdfbackend}
;;                                  ^
;;                                  :
;;   code-{pdfbackend} --> ee-code-{pdfbackend}      
;;                                  ^
;;                                  |
;;                      find-code-{pdfbackend}
;;
;; The function `find-{pdfbackend}' calls `ee-find-{pdfbackend}',
;; that needs to be defined externally.
;;
;; See:
;; (find-eev \"eev-pdflike.el\" \"code-pdfbackend\")
;; (find-eevfile \"eev-pdflike.el\" \"\\\"{pdfbackend}\\\"\")
;; (find-eevgrep \"grep --color=auto -nH -e '\\\"{pdfbackend}\\\"' eev-pdflike.el\")
;;
\(defun      find-{pdfbackend} (fname &optional page &rest rest)
  (interactive \"fPDF file: \")
  (find-bgprocess (ee-find-{pdfbackend} fname page)))

;; (find-code-{pdfbackend} \"C\" \"FNAME\")
;;      (code-{pdfbackend} \"C\" \"FNAME\")
;;
\(defun      code-{pdfbackend} (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-{pdfbackend} c fname rest))))
\(defun find-code-{pdfbackend} (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-{pdfbackend} c fname rest)))
\(defun   ee-code-{pdfbackend} (c fname &rest rest)
  (ee-template0 \"\\
  ;; (find-code-{pdfbackend} \\\"{<}c{>}\\\" \\\"{<}fname{>}\\\")
  ;;      (code-{pdfbackend} \\\"{<}c{>}\\\" \\\"{<}fname{>}\\\")
  ;;
  ;; See: (find-pdf-like-intro \\\"7. Shorter hyperlinks to PDF files\\\")
  ;;
  (setq ee-pdflike-last 'find-{<}c{>}page)
  (defun find-{<}c{>}page (&optional page &rest rest)
    (interactive)
    (setq ee-pdflike-last 'find-{<}c{>}page)
    (find-{pdfbackend} {<}(ee-pp0 fname){>} page))
  \"))
"))



;; «code-pdftextbackend»  (to ".code-pdftextbackend")
;; See: (find-eevfile "eev-pdflike.el" "1. DEFINING VIEWERS")
;; Test: (find-code-pdftextbackend "SHOWTEXT")
;;
(defun      code-pdftextbackend (pdftextbackend)
  (eval (ee-read      (ee-code-pdftextbackend pdftextbackend))))
(defun find-code-pdftextbackend (pdftextbackend)
  (find-estring-elisp (ee-code-pdftextbackend pdftextbackend)))
(defun   ee-code-pdftextbackend (pdftextbackend)
  (ee-template0 "\
;; (find-code-pdftextbackend \"{pdftextbackend}\")
;;      (code-pdftextbackend \"{pdftextbackend}\")
;; (find-eev \"eev-pdflike.el\" \"code-pdftextbackend\")
;; (find-eevfile \"eev-pdflike.el\" \"\\\"{pdftextbackend}\\\"\")
;; (find-eevgrep \"grep --color=auto -nH -e '\\\"{pdftextbackend}\\\"' eev-pdflike.el\")
;;
;; The code here defines this structure:
;;
;;                           find-{pdftextbackend} --> ee-find-{pdftextbackend}
;;                                  ^
;;                                  :
;;   code-{pdftextbackend} --> ee-code-{pdftextbackend}      
;;                                  ^
;;                                  |
;;                      find-code-{pdftextbackend}
;;
;; where `ee-find-{pdftextbackend}'
;; is an external function.
;;
\(defun      find-{pdftextbackend} (fname &optional page &rest rest)
  (apply 'find-sh-page (ee-find-{pdftextbackend} fname) page rest))

;; (find-code-{pdftextbackend} \"C\" \"FNAME\")
;;      (code-{pdftextbackend} \"C\" \"FNAME\")
;;
\(defun      code-{pdftextbackend} (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-{pdftextbackend} c fname rest))))
\(defun find-code-{pdftextbackend} (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-{pdftextbackend} c fname rest)))
\(defun   ee-code-{pdftextbackend} (c fname &optional offset)
  (setq offset (or offset 0))
  (ee-template0 \"\\
  ;; (find-code-{pdftextbackend} \\\"{<}c{>}\\\" \\\"{<}fname{>}\\\")
  ;;      (code-{pdftextbackend} \\\"{<}c{>}\\\" \\\"{<}fname{>}\\\")
  ;;
  ;; See: (find-pdf-like-intro \\\"7. Shorter hyperlinks to PDF files\\\")
  ;;
  (setq ee-page-c      {<}(ee-pp0 c){>})
  (setq ee-page-fname  {<}(ee-pp0 fname){>})
  (setq ee-page-offset {<}(ee-pp0 offset){>})
  (defun find-{<}c{>}text (&optional page &rest rest)
    (interactive)
    (setq ee-page-c      {<}(ee-pp0 c){>})
    (setq ee-page-fname  {<}(ee-pp0 fname){>})
    (setq ee-page-offset {<}(ee-pp0 offset){>})
    (apply 'find-{pdftextbackend} {<}(ee-pp0 fname){>} page rest))
\"))
"))



;; «code-pdfbackendalias»  (to ".code-pdfbackendalias")
;; See: (find-eevfile "eev-pdflike.el" "2. ALIASES")
;; Test: (find-code-pdfbackendalias "NEWPDFPROG-PAGE" "OLDPDFPROG-PAGE")
;;
(defun      code-pdfbackendalias (newbackend oldbackend)
  (eval (ee-read      (ee-code-pdfbackendalias newbackend oldbackend))))
(defun find-code-pdfbackendalias (newbackend oldbackend)
  (find-estring-elisp (ee-code-pdfbackendalias newbackend oldbackend)))
(defun   ee-code-pdfbackendalias (newbackend oldbackend)
  (ee-template0 "\
;; (find-code-pdfbackendalias \"{newbackend}\" \"{oldbackend}\")
;;      (code-pdfbackendalias \"{newbackend}\" \"{oldbackend}\")
;; (find-eev \"eev-pdflike.el\" \"code-pdfbackendalias\")
;; (find-eevfile \"eev-pdflike.el\" \"\\\"{newbackend}\\\"\")
;; (find-eevgrep \"grep --color=auto -nH  -e '\\\"{newbackend}\\\"' eev-pdflike.el\")
;;
\(defalias   'ee-find-{newbackend}   'ee-find-{oldbackend})
\(defalias      'find-{newbackend}      'find-{oldbackend})
\(defalias   'ee-code-{newbackend}   'ee-code-{oldbackend})
\(defalias      'code-{newbackend}      'code-{oldbackend})
\(defalias 'find-code-{newbackend} 'find-code-{oldbackend})
"))




;; ;;;   ___  _               _      _       
;; ;;;  / _ \| |__  ___  ___ | | ___| |_ ___ 
;; ;;; | | | | '_ \/ __|/ _ \| |/ _ \ __/ _ \
;; ;;; | |_| | |_) \__ \ (_) | |  __/ ||  __/
;; ;;;  \___/|_.__/|___/\___/|_|\___|\__\___|
;; ;;;                                       
;; ;; «code-xxxpdf-family» (to ".code-xxxpdf-family")
;; ;; See: (find-eevfile "eev-pdflike.el" "1. DEFINING VIEWERS")
;; ;; Test: (find-code-xxxpdf-family "XPDFPAGE")
;; 
;; (defun      code-xxxpdf-family (xxxpdf)
;;   (eval (ee-read      (ee-code-xxxpdf-family xxxpdf))))
;; (defun find-code-xxxpdf-family (xxxpdf)
;;   (find-estring-elisp (ee-code-xxxpdf-family xxxpdf)))
;; (defun   ee-code-xxxpdf-family (xxxpdf)
;;   (ee-template0 "\
;; ;; (find-code-xxxpdf-family {(ee-pp0 xxxpdf)})
;; ;;      (code-xxxpdf-family {(ee-pp0 xxxpdf)})
;; ;;
;; ;; To understand the code that a call to
;; ;;        (code-{xxxpdf} \"C\" \"FNAME\")
;; ;; would execute, run:
;; ;;   (find-code-{xxxpdf} \"C\" \"FNAME\")
;; 
;; \(defun      find-{xxxpdf} (fname &optional page &rest rest)
;;   (find-bgprocess (ee-find-{xxxpdf} fname page)))
;; 
;; \(defun      code-{xxxpdf} (c fname &rest rest)
;;   (eval (ee-read      (apply 'ee-code-{xxxpdf} c fname rest))))
;; \(defun find-code-{xxxpdf} (c fname &rest rest)
;;   (find-estring-elisp (apply 'ee-code-{xxxpdf} c fname rest)))
;; \(defun   ee-code-{xxxpdf} (c fname &rest rest)
;;   (ee-template0 \"\\
;; \(setq ee-pdflike-last 'find-{<}c{>}page)
;; \(defun find-{<}c{>}page (&optional page &rest rest)
;;   (setq ee-pdflike-last 'find-{<}c{>}page)
;;   (find-{xxxpdf} {<}(ee-pp0 fname){>} page))
;; \"))
;; "))
;; 
;; ;; «code-xxxpdftext-family» (to ".code-xxxpdftext-family")
;; ;; See: (find-eevfile "eev-pdflike.el" "1. DEFINING VIEWERS")
;; ;; Test: (find-code-xxxpdftext-family "PDFTOTEXT")
;; ;;
;; (defun      code-xxxpdftext-family (xxxpdf)
;;   (eval (ee-read      (ee-code-xxxpdftext-family xxxpdf))))
;; (defun find-code-xxxpdftext-family (xxxpdf)
;;   (find-estring-elisp (ee-code-xxxpdftext-family xxxpdf)))
;; (defun   ee-code-xxxpdftext-family (xxxpdf)
;;   (ee-template0 "\
;; ;; (find-code-xxxpdftext-family {(ee-pp0 xxxpdf)})
;; ;;      (code-xxxpdftext-family {(ee-pp0 xxxpdf)})
;; ;;
;; ;; To understand the code that a call to
;; ;;        (code-{xxxpdf} \"C\" \"FNAME\")
;; ;; would execute, run:
;; ;;   (find-code-{xxxpdf} \"C\" \"FNAME\")
;; 
;; \(defun      find-{xxxpdf} (fname &optional page &rest rest)
;;   (apply 'find-sh-page (ee-find-{xxxpdf} fname) page rest))
;; 
;; \(defun      code-{xxxpdf} (c fname &rest rest)
;;   (eval (ee-read      (apply 'ee-code-{xxxpdf} c fname rest))))
;; \(defun find-code-{xxxpdf} (c fname &rest rest)
;;   (find-estring-elisp (apply 'ee-code-{xxxpdf} c fname rest)))
;; \(defun   ee-code-{xxxpdf} (c fname &optional offset)
;;   (setq offset (or offset 0))
;;   (ee-template0 \"\\
;; \(setq ee-page-c      {<}(ee-pp0 c){>})
;; \(setq ee-page-fname  {<}(ee-pp0 fname){>})
;; \(setq ee-page-offset {<}(ee-pp0 offset){>})
;; \(defun find-{<}c{>}text (&optional page &rest rest)
;;   (setq ee-page-c      {<}(ee-pp0 c){>})
;;   (setq ee-page-fname  {<}(ee-pp0 fname){>})
;;   (setq ee-page-offset {<}(ee-pp0 offset){>})
;;   (apply 'find-{xxxpdf} {<}(ee-pp0 fname){>} page rest))
;; \"))
;; "))
;; 
;; ;; «code-xxxpdf-alias» (to ".code-xxxpdf-alias")
;; ;; See: (find-eevfile "eev-pdflike.el" "2. ALIASES")
;; ;; Test: (find-code-xxxpdf-alias "PDF-PAGE" "XPDF-PAGE")
;; ;;
;; (defun      code-xxxpdf-alias (xxxpdfnew xxxpdfold)
;;   (eval (ee-read      (ee-code-xxxpdf-alias xxxpdfnew xxxpdfold))))
;; (defun find-code-xxxpdf-alias (xxxpdfnew xxxpdfold)
;;   (find-estring-elisp (ee-code-xxxpdf-alias xxxpdfnew xxxpdfold)))
;; (defun   ee-code-xxxpdf-alias (xxxpdfnew xxxpdfold)
;;   (ee-template0 "\
;; ;; (find-code-xxxpdf-alias \"{xxxpdfnew}\" \"{xxxpdfold}\")
;; ;; (find-code-xxxpdf-alias \"xxxpdfnew\" \"xxxpdfold\")
;; ;; (find-code-xxxpdf-alias \"pdf-page\" \"xpdf-page\")
;; ;; (find-code-xxxpdf-alias \"pdfpage\" \"xpdf-page\")
;; ;;
;; \(defalias   'ee-find-{xxxpdfnew}   'ee-find-{xxxpdfold})
;; \(defalias      'find-{xxxpdfnew}      'find-{xxxpdfold})
;; \(defalias   'ee-code-{xxxpdfnew}   'ee-code-{xxxpdfold})
;; \(defalias      'code-{xxxpdfnew}      'code-{xxxpdfold})
;; \(defalias 'find-code-{xxxpdfnew} 'find-code-{xxxpdfold})
;; "))







;;;   __ _           _                 _  __                              
;;;  / _(_)_ __   __| |      _ __   __| |/ _|      _ __   __ _  __ _  ___ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _` | |_ _____| '_ \ / _` |/ _` |/ _ \
;;; |  _| | | | | (_| |_____| |_) | (_| |  _|_____| |_) | (_| | (_| |  __/
;;; |_| |_|_| |_|\__,_|     | .__/ \__,_|_|       | .__/ \__,_|\__, |\___|
;;;                         |_|                   |_|          |___/      
;;
;; «find-pdf-page» (to ".find-pdf-page")
;; (find-code-pdfbackend "pdf-page")
        (code-pdfbackend "pdf-page")

;; The defalias below makes all functions in the `find-pdf-page'
;; family use `find-xpdf-page' by default. Running this defalias will
;; OVERRIDE the `find-pdf-page' defined by the `code-pdfbackend'
;; above.
;;
(defalias 'find-pdf-page 'find-xpdf-page)



;;;  ____        __             _ _           _                        
;;; |  _ \  ___ / _| __ _ _   _| | |_  __   _(_) _____      _____ _ __ 
;;; | | | |/ _ \ |_ / _` | | | | | __| \ \ / / |/ _ \ \ /\ / / _ \ '__|
;;; | |_| |  __/  _| (_| | |_| | | |_   \ V /| |  __/\ V  V /  __/ |   
;;; |____/ \___|_|  \__,_|\__,_|_|\__|   \_/ |_|\___| \_/\_/ \___|_|   
;;;                                                                    
;; «change-default-viewer»  (to ".change-default-viewer")
;; To make `find-pdf-page' use another default viewer
;; you just have to use a `defalias', like this:
;;
;;   (defalias 'find-pdf-page 'find-xpdf-page)
;;   (defalias 'find-pdf-page 'find-evince-page)
;;   (defalias 'find-pdf-page 'find-pdftools-page)
;;   (defalias 'find-pdf-page 'find-texworkspdf-page)
;;   (defalias 'find-pdf-page 'find-googlechrome-page)
;;
;; You can inspect the current definition of `find-pdf-page' by
;; looking at its "function cell". Function cells are explained here:
;;
;;   (find-elisp-intro "6. Defining functions")
;;   (find-elisp-intro "11. Byte-compiled functions")
;;
;; When a function is defined with `defalias' its function cell
;; contains a symbol - the "name" of the function that it points to.
;; Try:
;;
;;   (find-efunctionpp                  'find-pdf-page)
;;                     (symbol-function 'find-pdf-page)
;;   (find-efunctionpp (symbol-function 'find-pdf-page))
;;   (find-efunctionpp                  'find-xpdf-page)
;;   (find-efunctionpp                  'find-evince-page)
;;   (find-efunctionpp                  'find-pdftools-page)
;;   (find-efunctionpp                  'find-texworkspdf-page)
;;   (find-efunctionpp                  'find-googlechrome-page)
;;
;; A VERY TECHNICAL DETAIL: apparently it would be cleaner to do
;; something like this,
;;
;;   (defalias 'ee-find-pdf-page 'ee-find-xpdf-page)
;;
;; instead of:
;;
;;   (defalias 'find-pdf-page 'find-xpdf-page)
;;
;; but this wouldn't work for some atypical backends, like
;; `find-pdftools-page'... (TODO: explain this!)



;;;  ____        __             _ _                  __        ___  
;;; |  _ \  ___ / _| __ _ _   _| | |_    ___  _ __   \ \      / / | 
;;; | | | |/ _ \ |_ / _` | | | | | __|  / _ \| '_ \   \ \ /\ / / __)
;;; | |_| |  __/  _| (_| | |_| | | |_  | (_) | | | |   \ V  V /\__ \
;;; |____/ \___|_|  \__,_|\__,_|_|\__|  \___/|_| |_|    \_/\_/ (   /
;;;                                                             |_| 
;; «find-pdf-page-windows»  (to ".find-pdf-page-windows")
;; (find-elnode "System Environment" "windows-nt")
;; (find-evardescr 'system-type)
;; (to "change-default-viewer")
;; (to "find-firefox-page")
;;
(if (eq system-type 'windows-nt)
    (defalias 'find-pdf-page 'find-firefox-page)
  )




;;;   __ _           _                 _  __       _            _   
;;;  / _(_)_ __   __| |      _ __   __| |/ _|     | |_ _____  _| |_ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _` | |_ _____| __/ _ \ \/ / __|
;;; |  _| | | | | (_| |_____| |_) | (_| |  _|_____| ||  __/>  <| |_ 
;;; |_| |_|_| |_|\__,_|     | .__/ \__,_|_|        \__\___/_/\_\\__|
;;;                         |_|                                     
;;
;; «find-pdf-text» (to ".find-pdf-text")
;; (find-code-pdfbackendalias "pdf-text" "pdftotext-text")
        (code-pdfbackendalias "pdf-text" "pdftotext-text")

(defalias 'find-pdf-text 'find-pdftotext-text)





;;; __  __          _  __ 
;;; \ \/ /_ __   __| |/ _|
;;;  \  /| '_ \ / _` | |_ 
;;;  /  \| |_) | (_| |  _|
;;; /_/\_\ .__/ \__,_|_|  
;;;      |_|              
;;
;; «find-xpdf-page»  (to ".find-xpdf-page")
;; Xpdf is our default PDF viewer (except on W$). See:
;;
;;   (find-pdf-like-intro "3. Hyperlinks to PDF files" "xpdf")
;;   https://en.wikipedia.org/wiki/Xpdf
;;   https://askubuntu.com/questions/1245518/how-to-install-xpdf-on-ubuntu-20-04

(defvar ee-find-xpdf-colon "")
(defun  ee-find-xpdf-colon ()
  "Return \":\" or \"\" according to the version of xpdf.
Some versions of xpdf need a \":\" before the page number -
they need be called as \"xpdf foo.pdf :42\" instead of as
\"xpdf foo.pdf 42\". In the future this function will try
to guess correctly if the \":\" is needed or not, but this
version just returns the value of the variable
`ee-find-xpdf-colon'."
  ee-find-xpdf-colon)

(defvar ee-find-xpdf-page-options '("-fullscreen"))
(defun  ee-find-xpdf-page (fname &optional page &rest rest)
  `("xpdf"
    ,@ee-find-xpdf-page-options
    ,fname
    ,@(if page `(,(format "%s%d" (ee-find-xpdf-colon) page)))
    ))

(defun find-xpdf-page (fname &optional page &rest rest)
  "This defun will be overridden by the `code-pdfbackend' below.
We define it just to make this work: (find-efunction 'find-xpdf-page)"
  (interactive "fPDF file: ")
  (find-bgprocess (ee-find-xpdf-page fname page)))

;; (find-code-pdfbackend "xpdf-page")
        (code-pdfbackend "xpdf-page")




;;;            _  __ _        _            _   
;;;  _ __   __| |/ _| |_ ___ | |_ _____  _| |_ 
;;; | '_ \ / _` | |_| __/ _ \| __/ _ \ \/ / __|
;;; | |_) | (_| |  _| || (_) | ||  __/>  <| |_ 
;;; | .__/ \__,_|_|  \__\___/ \__\___/_/\_\\__|
;;; |_|                                        
;;
;; «find-pdftotext-text»  (to ".find-pdftotext-text")
;; Pdftotext is our default way of converting PDFs to text. See:
;;
;;   (find-pdf-like-intro "3. Hyperlinks to PDF files" "pdftotext")
;;   https://en.wikipedia.org/wiki/Pdftotext

(defvar ee-pdftotext-program "pdftotext")

(defun ee-find-pdftotext-text (fname &rest rest)
  `(,ee-pdftotext-program "-layout" "-enc" "Latin1" ,(ee-expand fname) "-"))

(defun find-pdftotext-page (fname &optional page &rest rest)
  "This defun will be overridden by the `code-pdftextbackend' below.
We define it just to make this work: (find-efunction 'find-pdftotext-page)"
  (apply 'find-sh-page (ee-find-pdftotext-text fname) page rest))

;; (find-man "1 pdftotext")
;; (find-man "1 pdftotext" "-enc encoding-name")
;;
;; The option "-enc Latin1" above make pdftotext convert its output to
;; Latin1. If you prefer UTF-8, override the previous definition with
;; the one below, that it is commented out with a "'":

' (defun ee-find-pdftotext-text (fname &rest rest)
   `(,ee-pdftotext-program "-layout" ,(ee-expand fname) "-"))

;; (find-code-pdftextbackend "pdftotext-text")
        (code-pdftextbackend "pdftotext-text")



;;;            _  __ _        _            _   ___  
;;;  _ __   __| |/ _| |_ ___ | |_ _____  _| |_( _ ) 
;;; | '_ \ / _` | |_| __/ _ \| __/ _ \ \/ / __/ _ \ 
;;; | |_) | (_| |  _| || (_) | ||  __/>  <| || (_) |
;;; | .__/ \__,_|_|  \__\___/ \__\___/_/\_\\__\___/ 
;;; |_|                                             
;;
;; Some PDFs are encoded in a way that confuses pdftotext, and none of
;; these ways of calling pdftotext work very well:
;;
;;   pdftotext -layout -enc Latin1 foo.pdf -
;;   pdftotext -layout             foo.pdf -
;;
;; Then we can use a block like this,
;;
;;   http://angg.twu.net/acker/acker__ga_livro1_2019.pdf
;;   (code-pdf-page  "ackerGA1" "$S/http/angg.twu.net/acker/acker__ga_livro1_2019.pdf")
;;   (code-pdf-text  "ackerGA1" "$S/http/angg.twu.net/acker/acker__ga_livro1_2019.pdf" 16)
;;   (code-pdf-text8 "ackerGA1" "$S/http/angg.twu.net/acker/acker__ga_livro1_2019.pdf" 16)
;;   (find-ackerGA1page)
;;   (find-ackerGA1page 3 "Sumário")
;;   (find-ackerGA1text 3 "Sumário")
;;
;; that lets us switch between `code-pdf-text', that uses "pdftotext
;; -layout -enc Latin1", and `code-pdf-text8', that uses "pdftotext
;; -layout".
;;
;; «find-pdftotext8-text»  (to ".find-pdftotext8-text")
(defun ee-find-pdftotext8-text (fname &rest rest)
  `(,ee-pdftotext-program "-layout" ,(ee-expand fname) "-"))
(defun ee-find-pdf-text8       (fname &rest rest)
  `(,ee-pdftotext-program "-layout" ,(ee-expand fname) "-"))

(defun find-pdftotext8-text (fname &optional page &rest rest)
  "This defun will be overridden by the `code-pdftextbackend' below.
We define it just to make this work: (find-efunction 'find-pdftotext-page)"
  (apply 'find-sh-page (ee-find-pdftotext8-text fname) page rest))

(defun find-pdf-text8       (fname &optional page &rest rest)
  "This defun will be overridden by the `code-pdftextbackend' below.
We define it just to make this work: (find-efunction 'find-pdftotext-page)"
  (apply 'find-sh-page (ee-find-pdf-text8       fname) page rest))

;; (find-code-pdftextbackend "pdftotext8-text")
        (code-pdftextbackend "pdftotext8-text")
;; (find-code-pdftextbackend "pdf-text8")
        (code-pdftextbackend "pdf-text8")





;;;  _____   __  __                   _        
;;; |_   _|__\ \/ /_      _____  _ __| | _____ 
;;;   | |/ _ \\  /\ \ /\ / / _ \| '__| |/ / __|
;;;   | |  __//  \ \ V  V / (_) | |  |   <\__ \
;;;   |_|\___/_/\_\ \_/\_/ \___/|_|  |_|\_\___/
;;;                                            
;; «find-texworkspdf-page» (to ".find-texworkspdf-page")
;; At one point TeXworks was my default PDF viewer on W$s. See:
;;
;;   http://www.tug.org/texworks/
;;   https://tug.org/pipermail/tex-live/2019-March/043227.html
;;   https://tug.org/pipermail/tex-live/2019-March/043229.html
;;   http://anggtwu.net/find-pdf-page.html

(defvar ee-texworks-program "texworks")

(defun  ee-find-texworkspdf-page (fname &optional page)
  `(,ee-texworks-program
    ,@(if page `(,(format "--position=%d" page)))
    ,fname
    ))

;; (find-code-pdfbackend "texworkspdf-page")
        (code-pdfbackend "texworkspdf-page")




;;;            _  __       _              _     
;;;  _ __   __| |/ _|     | |_ ___   ___ | |___ 
;;; | '_ \ / _` | |_ _____| __/ _ \ / _ \| / __|
;;; | |_) | (_| |  _|_____| || (_) | (_) | \__ \
;;; | .__/ \__,_|_|        \__\___/ \___/|_|___/
;;; |_|                                         
;;
;; «find-pdftools-page» (to ".find-pdftools-page")
;; This backend uses the Emacs package `pdf-tools' - that is "a better
;; DocView" - to display a PDF in an Emacs buffer. See:
;;
;;   https://github.com/vedang/pdf-tools/
;;   (find-epackage-links 'pdf-tools)
;;   (find-epackage       'pdf-tools)
;;   (find-enode "Document View")
;;
;; Note that `*-pdftools-page' is an atypical backend. In all the
;; "normal" backends the function `ee-find-FOO-page' returns the name
;; of an external program and its command-line arguments, and
;; `find-FOO-page' uses `find-bgprocess' to call that external
;; program...
;;
;; In the `*-pdftools-page' family of functions the function
;; `ee-find-pdftools-page' does not exist, and the
;; `(defun find-pdftools-page ...)' below OVERRIDES the
;; `(defun find-pdftools-page ...)' executed by the
;; `(code-pdfbackend "pdftools-page")'.
;;
;; (find-code-pdfbackend "pdftools-page")
        (code-pdfbackend "pdftools-page")

(defun find-pdftools-page (pdffile &optional page &rest rest)
  "Open PDFFILE in the current window using pdf-tools.
If PAGE is given, go to that page; if PAGE is nil, stay in the
current page."
  (interactive "fPDF file: ")
  ;;
  ;; `pdf-loader-install' makes sure that pdf-tools is loaded and
  ;; that it has set up `auto-mode-alist' and `magic-mode-alist' to
  ;; make PDF files be opened in `pdf-view-mode' instead of in
  ;; `doc-view-mode'. See:
  ;;   (find-evardescr 'auto-mode-alist "pdf")
  ;;   (find-evardescr 'auto-mode-alist "pdf-view-mode")
  ;;   (find-evardescr 'auto-mode-alist "doc-view-mode")
  ;;   (find-efunction 'pdf-loader-install)
  ;;   (find-efunction 'pdf-tools-install)
  ;;
  ;; Old way: (pdf-tools-install)
  (require 'pdf-tools)
  (require 'pdf-loader)
  (pdf-loader-install)
  ;;
  ;; Open PDFFILE. You will get weird results if it is not a PDF.
  ;; Use two tricks with "revert" to reload the PDF if it has changed.
  (let ((revert-without-query '(".*")))
    (find-fline pdffile))
  (revert-buffer nil 'noconfirm)
  ;;
  ;; If PAGE is given, go to that page.
  (if page (pdf-view-goto-page page)))


;; «find-pdftoolsr-page»  (to ".find-pdftoolsr-page")
;; Test:
;;   (defalias 'find-pdf-page 'find-xpdf-page)
;;   (defalias 'find-pdf-page 'find-pdftoolsr-page)
;;   (find-pdf-page "~/Coetzee99.pdf" 1)
;;   (find-pdf-page "~/Coetzee99.pdf" 3)
(defun find-pdftoolsr-page (fname &optional page &rest rest)
  "Like `find-pdftools-page', but opens the PDF in the right window."
  (interactive "fPDF file: ")
  (find-2a nil '(find-pdftools-page fname page)))




;; The function `ee-pdftools-revert-all' below - from 2019 - was a
;; trick that I used to redisplay PDFs when I recompiled their LaTeX
;; source. In jan/2022 I added a `(revert-buffer ...)' to
;; `find-pdftools-page', and I _think_ that this will let me rewrite
;; my hacks that use `ee-pdftools-revert-all' in a much better way.
;;
;; My old notes are here: (find-es "emacs" "ee-pdftools-revert-all")
;; This function will probably be declared obsolete soon.
;;
(defun ee-pdftools-revert-all ()
"Run `revert-buffer' in all windows in which pdf-tools is showing PDFs.
This makes pdf-tools show the new version of these PDFs without
changing the page, the image size, the hscroll, and the vscroll
in each window."
  (interactive)
  (dolist (window (window-list-1))
    (with-selected-window window
      (if (eq major-mode 'pdf-view-mode)
	  (revert-buffer)))))





;;;          _       _ 
;;; __  ____| |_   _(_)
;;; \ \/ / _` \ \ / / |
;;;  >  < (_| |\ V /| |
;;; /_/\_\__,_| \_/ |_|
;;;                    
;; «find-xdvi-page» (to ".find-xdvi-page")
;; Xdvi is my favorite viewer for DVI files - note: it doesn't support
;; PDF files! See:
;;
;;   https://en.wikipedia.org/wiki/Xdvi

(defvar ee-find-xdvi-page-options '())
(defun  ee-find-xdvi-page (fname &optional page)
  `("xdvi"
    ,@ee-find-xdvi-page-options
    ,@(if page `(,(format "+%d" page)))
    ,fname))

;; (find-code-pdfbackend "xdvi-page")
        (code-pdfbackend "xdvi-page")




;;;      _  _       _               
;;;   __| |(_)_   _(_) _____      __
;;;  / _` || \ \ / / |/ _ \ \ /\ / /
;;; | (_| || |\ V /| |  __/\ V  V / 
;;;  \__,_|/ | \_/ |_|\___| \_/\_/  
;;;      |__/                       
;;
;; «find-djview-page» (to ".find-djview-page")
;; DjView is a viewer for DJVU files.
;; The page argument does not work well with djview - see the cleanup
;; function below. I hate djview.
;;
;;   http://djvu.sourceforge.net/djview4.html
;;   https://packages.debian.org/sid/djview4

(defun     find-djview-page (fname &optional page &rest rest)
  (ee-find-djview-cleanup fname)
  (find-bgprocess (ee-find-djview-page fname page)))
(defvar ee-find-djview-page-options '())
(defun  ee-find-djview-page (fname &optional page)
  `("djview"
    ,@ee-find-djview-page-options
    ,@(if page `(,(format "--page=%d" page)))
    ,fname))

(defun ee-find-djview-cleanup (&optional fname)
  "A hack: clean up djview's 'recentFiles=' line in the config file if needed.
When we visit a file \"/path/foo.djvu\" with djview, go to the
page 234 and close djview, djview stores the filename
\"/path/foo.djvu\" and the page number 234 in the file
\"~/.config/DjVuLibre/DjView.conf\", in a line starting with
\"recentFiles=\" - and the next time we ask djview to open that
file it will go to the page 234 OVERRIDING THE ARGUMENT
\"--page=%d\"! This function is currently just a stub, but you
may want to put here code that cleans up that page information.")

;; (find-code-pdfbackend "djview-page")
        (code-pdfbackend "djview-page")




;;;  _____       _                
;;; | ____|_   _(_)_ __   ___ ___ 
;;; |  _| \ \ / / | '_ \ / __/ _ \
;;; | |___ \ V /| | | | | (_|  __/
;;; |_____| \_/ |_|_| |_|\___\___|
;;;                               
;; «find-evince-page» (to ".find-evince-page")
;; Evince is a document viewer that supports several file formats. The
;; last time I tried it confused page labels and page indices and it
;; was slow. I haven't used it much. See:
;;
;;   https://en.wikipedia.org/wiki/Evince

(defun     find-evince-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-evince-page fname page)))
(defvar ee-find-evince-page-options '())
(defun  ee-find-evince-page (fname &optional page)
  `("evince"
    ,@ee-find-evince-page-options
    ;; ,@(if page `(,(format "--page-label=%d" page)))
    ,@(if page `(,(format "--page-index=%d" page)))
    ,fname))

;; (find-code-pdfbackend "evince-page")
        (code-pdfbackend "evince-page")




;;;                            _  __ 
;;;  _ __ ___  _   _ _ __   __| |/ _|
;;; | '_ ` _ \| | | | '_ \ / _` | |_ 
;;; | | | | | | |_| | |_) | (_| |  _|
;;; |_| |_| |_|\__,_| .__/ \__,_|_|  
;;;                 |_|              
;;
;; «find-mupdfpage»  (to ".find-mupdfpage")
;; By Erich Ruff. See:
;; https://lists.gnu.org/archive/html/eev/2021-06/msg00008.html

(defun     find-mupdf-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-mupdf-page fname page)))
(defvar ee-find-mupdf-page-options '())
(defun  ee-find-mupdf-page (fname &optional page &rest rest)
  `("mupdf"
    ,@ee-find-mupdf-page-options
    ,fname
    ,@(if page `(,(format "%s" page)))
    ))

;; (find-code-pdfbackend "mupdf-page")
        (code-pdfbackend "mupdf-page")



;;;        _          _            
;;;   ___ | | ___   _| | __ _ _ __ 
;;;  / _ \| |/ / | | | |/ _` | '__|
;;; | (_) |   <| |_| | | (_| | |   
;;;  \___/|_|\_\\__,_|_|\__,_|_|   
;;;                                
;; «find-okularpage»  (to ".find-okularpage")
;; Test: (find-pdf-like-intro "2. Preparation")
;;       (find-okular-page "~/Coetzee99.pdf" 3)
;;
(defun     find-okular-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-okular-page fname page)))
(defvar ee-find-okular-page-options '())
(defun  ee-find-okular-page (fname &optional page &rest rest)
  `("okular"
    ,@ee-find-okular-page-options
    ,@(if page `("-p" ,(format "%s" page)))
    ,fname
    ))

;; (find-code-pdfbackend "okular-page")
        (code-pdfbackend "okular-page")





;;;   ____       
;;;  / ___|_   __
;;; | |  _\ \ / /
;;; | |_| |\ V / 
;;;  \____| \_/  
;;;              
;; «find-gv-page» (to ".find-gv-page")
;; GhostView is a viewer for PostScript and PDF files.
;; I haven't used it in ages. See:
;;
;;   https://www.gnu.org/software/gv/
;;   https://en.wikipedia.org/wiki/Ghostscript

(defvar ee-find-gv-page-options '())
(defun  ee-find-gv-page (fname &optional page)
  `("gv"
    ,@ee-find-gv-page-options
    ,@(if page `(,(format "--page=%d" page)))
    ,fname))

;; (find-code-pdfbackend "gv-page")
        (code-pdfbackend "gv-page")



;;;      _  _             _        _   
;;;   __| |(_)_   ___   _| |___  _| |_ 
;;;  / _` || \ \ / / | | | __\ \/ / __|
;;; | (_| || |\ V /| |_| | |_ >  <| |_ 
;;;  \__,_|/ | \_/  \__,_|\__/_/\_\\__|
;;;      |__/                          
;;
;; «find-djvutxt-text» (to ".find-djvutxt-text")
;; Djvutxt is a pdftotext-like program that works on DJVU files
;; instead of on PDF files. See:
;;
;;   https://en.wikipedia.org/wiki/DjVu
;;   http://djvu.sourceforge.net/doc/man/djvutxt.html

(defun ee-find-djvutxt-text (fname)
  (format "djvutxt '%s'" fname))

;; (find-code-pdftextbackend "djvutxt-text")
        (code-pdftextbackend "djvutxt-text")





;;;  ____                                      
;;; | __ ) _ __ _____      _____  ___ _ __ ___ 
;;; |  _ \| '__/ _ \ \ /\ / / __|/ _ \ '__/ __|
;;; | |_) | | | (_) \ V  V /\__ \  __/ |  \__ \
;;; |____/|_|  \___/ \_/\_/ |___/\___|_|  |___/
;;;                                            
;; «find-firefox-page»       (to ".find-firefox-page")
;; «find-googlechrome-page»  (to ".find-googlechrome-page")
;;   «ee-fname-page-to-url»  (to ".ee-fname-page-to-url")
;;
;; Some browsers can open a PDF file "/tmp/foo.pdf" at page 42 if we
;; add a "#page=42" to its URL - i.e., if we ask them to open the URL
;; "file:///tmp/foo.pdf#page=42"...

;; See: (find-efunction 'ee-fname-to-url)
;;      (find-eev "eev-plinks.el" "find-firefox")
;;      (find-eev "eev-plinks.el" "find-googlechrome")
;; Tests: (ee-fname-page-to-url "~/Coetzee99.pdf")
;;        (ee-fname-page-to-url "~/Coetzee99.pdf" 4)
;;        (ee-fname-page-to-url   "$S/http/foo/bar/Coetzee99.pdf" 4)
;;        (ee-fname-page-to-url "file:///home/edrx/Coetzee99.pdf" 4)
;;        (ee-fname-page-to-url   "https://foo/bar/Coetzee99.pdf" 4)
;;        (ee-fname-page-to-url    "http://foo/bar/Coetzee99.pdf" 4)
;;
(defun ee-fname-page-to-url (fname &optional page)
  "Convert FNAME to a \"file://\" url (after running `ee-expand' on it).
If FNAME is already a url then don't convert it.
If PAGE is non-nil append a \"#page=nnn\" to the result."
  (let ((baseurl (if (string-match "^\\(https?\\|file\\)://" fname)
		     fname
		   (format "file://%s" (ee-expand fname))))
	(pagestr (if page (format "#page=%s" page) "")))
    (concat baseurl pagestr)))

(defun ee-find-firefox-page (fname &optional page)
  `(,ee-firefox-program ,(ee-fname-page-to-url fname page)))

(defun ee-find-googlechrome-page (fname &optional page)
  `(,ee-googlechrome-program ,(ee-fname-page-to-url fname page)))

;; Tests: (find-firefox-page      "~/Coetzee99.pdf")
;;        (find-firefox-page      "~/Coetzee99.pdf" 3)
;;        (find-googlechrome-page "~/Coetzee99.pdf")
;;        (find-googlechrome-page "~/Coetzee99.pdf" 3)
;;
;; (find-code-pdfbackend "firefox-page")
        (code-pdfbackend "firefox-page")
;;
;; (find-code-pdfbackend "googlechrome-page")
        (code-pdfbackend "googlechrome-page")



;;;                _            _                               
;;;   ___ ___   __| | ___      | |__  _ ____  ____  ____  _____ 
;;;  / __/ _ \ / _` |/ _ \_____| '_ \| '__\ \/ /\ \/ /\ \/ / __|
;;; | (_| (_) | (_| |  __/_____| |_) | |   >  <  >  <  >  <\__ \
;;;  \___\___/ \__,_|\___|     |_.__/|_|  /_/\_\/_/\_\/_/\_\___/
;;;                                                             
;; «code-brxxxs» (to ".code-brxxxs")
;; See: (find-brxxx-intro "6. `code-brfile'")
;;
;; (find-code-brfile 'find-pdf-page    :local 'brpdfl      :dired 'brpdfd)
        (code-brfile 'find-pdf-page    :local 'brpdfl      :dired 'brpdfd)
;; (find-code-brfile 'find-pdf-text    :local 'brpdftextl  :dired 'brpdftextd)
        (code-brfile 'find-pdf-text    :local 'brpdftextl  :dired 'brpdftextd)
;; (find-code-brfile 'find-pdf-text8   :local 'brpdftxt8l  :dired 'brpdftxt8d)
        (code-brfile 'find-pdf-text8   :local 'brpdftxt8l  :dired 'brpdftxt8d)

;; (find-code-brfile 'find-djvu-page   :local 'brdjvul     :dired 'brdjvud)
        (code-brfile 'find-djvu-page   :local 'brdjvul     :dired 'brdjvud)
;; (find-code-brfile 'find-djvu-text   :local 'brdjvutextl :dired 'brdjvutextd)
        (code-brfile 'find-djvu-text   :local 'brdjvutextl :dired 'brdjvutextd)
;; (find-code-brfile 'find-evince-page :local 'brevincel   :dired 'brevinced)
        (code-brfile 'find-evince-page :local 'brevincel   :dired 'brevinced)
;; (find-code-brfile 'find-xdvi-page   :local 'brxdvil     :dired 'brxdvid)
        (code-brfile 'find-xdvi-page   :local 'brxdvil     :dired 'brxdvid)
;; (find-code-brfile 'find-xpdf-page   :local 'brxpdfl     :dired 'brxpdfd)
        (code-brfile 'find-xpdf-page   :local 'brxpdfl     :dired 'brxpdfd)



;;;     _    _ _                     
;;;    / \  | (_) __ _ ___  ___  ___ 
;;;   / _ \ | | |/ _` / __|/ _ \/ __|
;;;  / ___ \| | | (_| \__ \  __/\__ \
;;; /_/   \_\_|_|\__,_|___/\___||___/
;;;                                  
;; «aliases» (to ".aliases")
;; These aliases will be deleted - or commented out - soon.
;; They are here mostly to support some very old e-scripts that I
;; wrote before establishing this convention:
;;
;;   (find-eevfile "eev-pdflike.el" "4. A CONVENTION ON HYPHENS")

;; (find-code-pdfbackendalias "djvu"       "djview-page")
        (code-pdfbackendalias "djvu"       "djview-page")
;; (find-code-pdfbackendalias "djvu-text"  "djvutxt-text")
        (code-pdfbackendalias "djvu-text"  "djvutxt-text")
;; (find-code-pdfbackendalias "djvupage"   "djview-page")
        (code-pdfbackendalias "djvupage"   "djview-page")
;; (find-code-pdfbackendalias "dvi"        "xdvi-page")
        (code-pdfbackendalias "dvi"        "xdvi-page")
;; (find-code-pdfbackendalias "evince"     "evince-page")
        (code-pdfbackendalias "evince"     "evince-page")
;; (find-code-pdfbackendalias "evincepage" "evince-page")
        (code-pdfbackendalias "evincepage" "evince-page")
;; (find-code-pdfbackendalias "pdf"        "xpdf-page")
        (code-pdfbackendalias "pdf"        "xpdf-page")
;; (find-code-pdfbackendalias "pspage"     "gv-page")
        (code-pdfbackendalias "pspage"     "gv-page")
;; (find-code-pdfbackendalias "xdvi"       "xdvi-page")
        (code-pdfbackendalias "xdvi"       "xdvi-page")
;; (find-code-pdfbackendalias "xdvipage"   "xdvi-page")
        (code-pdfbackendalias "xdvipage"   "xdvi-page")
;; (find-code-pdfbackendalias "xpdf"       "xpdf-page")
        (code-pdfbackendalias "xpdf"       "xpdf-page")
;; (find-code-pdfbackendalias "xpdfpage"   "xpdf-page")
        (code-pdfbackendalias "xpdfpage"   "xpdf-page")









(provide 'eev-pdflike)




;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
