;;; eev-pdflike.el -- hyperlinks to documents made of pages.

;; Copyright (C) 2012-2019 Free Software Foundation, Inc.
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
;; Version:    2019jul11
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-pdflike.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-pdflike.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-pdf-like-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-pdf-like-intro)

;;; Commentary:

;; 2019jan25: I rewrote most of this file. The comments below are new.
;; There may be bugs. The old version of this file has renamed to
;; "eev-pdflike-old.el". See:
;;
;;   (find-eev "eev-pdflike-old.el")
;;
;; The main ideas are explained here:
;;
;;   (find-eev-quick-intro "9.3. Hyperlinks to PDF files")
;;
;; The functions whose names end with a <suffix> like "xpdf-page" call
;; one another according to this diagram:
;;
;;                         find-<suffix> --> ee-find-<suffix>
;;                                ^
;;                                :
;;    code-<suffix> --> ee-code-<suffix>      
;;                                ^
;;                                |
;;                    find-code-<suffix>
;;
;; note that "->" means "calls", and "..>" means "produces elisp code
;; that calls". The main <suffix>es are:
;;
;;   xpdf-page
;;   pdf-page
;;   pdf-text
;;
;; there are also these suffixes, for programs that I use much less:
;;
;;   xdvi-page
;;   djvu-page
;;   ps-page
;;   evince-page
;;
;; FAMILIES. We call the five functions ending in "xpdf-page" the
;; "xpdf-page family" - and the same for the other suffixes. The code
;; for the xpdf-page family is very similar to the code for the
;; pdf-text family. We use the templates in the "meta-tools" section
;; below to generate the block of code for each family - except for
;; the function `ee-find-<suffix>'. For example:
;;
;;   (find-code-xxxpdf-family "xpdf-page")
;;
;; CONVENTION ON HYPHENS. Note that if we run
;;
;;   (code-pdf-page         "livesofanimals" "/tmp/Coetzee99.pdf")
;;   (code-pdf-text         "livesofanimals" "/tmp/Coetzee99.pdf")
;;   ;; (find-code-pdf-page "livesofanimals" "/tmp/Coetzee99.pdf")
;;   ;; (find-code-pdf-text "livesofanimals" "/tmp/Coetzee99.pdf")
;;
;; this defines the functions `find-livesofanimalspage' and
;; `find-livesofanimalstext'. I am trying to follow a convention in
;; which function with two hyphens in their names, like
;; `find-xpdf-page', are basic hyperlink functions like the ones
;; defined in this file, and functions with only one hyphen in their
;; names, like `find-livesofanimalspage', are short hyperlinks, as in:
;;
;;   (find-eev-quick-intro "9.4. Shorter hyperlinks to PDF files")
;;
;; ALIASES. This convention on hyphens only occurred to me a few years
;; ago. To keep compatibility with the previous version there are some
;; calls to `code-xxxpdf-alias' at the end of this file that define
;; functions that I used in old versions of eev that do not follow the
;; convention.
;;
;; SOME TESTS:
;;
;;        (find-code-xxxpdftext-family "pdf-text")
;;             (code-xxxpdftext-family "pdf-text")
;; (find-code-pdf-text "loa" "/tmp/Coetzee99.pdf")
;; (find-code-pdf-text "loa" "/tmp/Coetzee99.pdf" -110)
;;      (code-pdf-text "loa" "/tmp/Coetzee99.pdf" -110)
;;      (find-loatext)
;;      (find-loatext (+ -110 145))
;;      (find-loatext (+ -110 145) "squirrel doing its thinking")
;;      (find-loatext              "squirrel doing its thinking")



;; «.ee-goto-position-page»		(to "ee-goto-position-page")
;; «.find-sh-page»			(to "find-sh-page")
;; «.code-xxxpdf-family»		(to "code-xxxpdf-family")
;; «.code-xxxpdftext-family»		(to "code-xxxpdftext-family")
;; «.code-xxxpdf-alias»			(to "code-xxxpdf-alias")
;;
;; «.find-xpdf-page»			(to "find-xpdf-page")
;; «.find-pdf-page»			(to "find-pdf-page")
;; «.find-pdf-text»			(to "find-pdf-text")
;;
;; «.find-texworkspdf-page»		(to "find-texworkspdf-page")
;; «.find-pdftools-page»		(to "find-pdftools-page")
;; «.find-xdvi-page»			(to "find-xdvi-page")
;; «.find-djview-page»			(to "find-djview-page")
;; «.find-evince-page»			(to "find-evince-page")
;; «.find-gv-page»			(to "find-gv-page")
;; «.find-djvutxt-text»			(to "find-djvutxt-text")
;;
;; «.aliases»				(to "aliases")
;; «.aliases-windows»			(to "aliases-windows")
;; «.code-brxxxs»			(to "code-brxxxs")


(require 'eev-brxxx)			; (find-eev "eev-brxxx.el")


;;;                  _       _     _           
;;; __   ____ _ _ __(_) __ _| |__ | | ___  ___ 
;;; \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;;  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;;   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;
;; Used by `M-h M-p'. See:
;;   (find-efunction 'find-pdflike-page-links)
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
;; «ee-goto-position-page» (to ".ee-goto-position-page")
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

;; «find-sh-page» (to ".find-sh-page")
(defun find-sh-page (program-and-args &rest pos-spec-list)
  "Like `find-sh', but interpreting the car of POS-SPEC-LIST as a page number."
  (interactive "sShell command: ")
  (find-eoutput-reuse
   (ee-unsplit program-and-args)
   `(insert (ee-pdftotext-replace-bad-ffs
	     (find-callprocess00 ,'program-and-args))))
  (apply 'ee-goto-position-page pos-spec-list))




;;;                 _              _              _     
;;;  _ __ ___   ___| |_ __ _      | |_ ___   ___ | |___ 
;;; | '_ ` _ \ / _ \ __/ _` |_____| __/ _ \ / _ \| / __|
;;; | | | | | |  __/ || (_| |_____| || (_) | (_) | \__ \
;;; |_| |_| |_|\___|\__\__,_|      \__\___/ \___/|_|___/
;;;                                                     
;; «code-xxxpdf-family» (to ".code-xxxpdf-family")
;; Test: (find-code-xxxpdf-family "XPDFPAGE")
;;
(defun      code-xxxpdf-family (xxxpdf)
  (eval (ee-read      (ee-code-xxxpdf-family xxxpdf))))
(defun find-code-xxxpdf-family (xxxpdf)
  (find-estring-elisp (ee-code-xxxpdf-family xxxpdf)))
(defun   ee-code-xxxpdf-family (xxxpdf)
  (ee-template0 "\
;; (find-code-xxxpdf-family {(ee-pp0 xxxpdf)})
;;      (code-xxxpdf-family {(ee-pp0 xxxpdf)})
;; (find-code-{xxxpdf} \"C\" \"FNAME\")

\(defun      find-{xxxpdf} (fname &optional page &rest rest)
  (find-bgprocess (ee-find-{xxxpdf} fname page)))

\(defun      code-{xxxpdf} (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-{xxxpdf} c fname rest))))
\(defun find-code-{xxxpdf} (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-{xxxpdf} c fname rest)))
\(defun   ee-code-{xxxpdf} (c fname &rest rest)
  (ee-template0 \"\\
\(setq ee-pdflike-last 'find-{<}c{>}page)
\(defun find-{<}c{>}page (&optional page &rest rest)
  (setq ee-pdflike-last 'find-{<}c{>}page)
  (find-{xxxpdf} {<}(ee-pp0 fname){>} page))
\"))
"))

;; «code-xxxpdftext-family» (to ".code-xxxpdftext-family")
;; Tests: (find-code-xxxpdftext-family "XPDFTEXT")
;;
(defun      code-xxxpdftext-family (xxxpdf)
  (eval (ee-read      (ee-code-xxxpdftext-family xxxpdf))))
(defun find-code-xxxpdftext-family (xxxpdf)
  (find-estring-elisp (ee-code-xxxpdftext-family xxxpdf)))
(defun   ee-code-xxxpdftext-family (xxxpdf)
  (ee-template0 "\
;; (find-code-xxxpdftext-family {(ee-pp0 xxxpdf)})
;;      (code-xxxpdftext-family {(ee-pp0 xxxpdf)})
;; (find-code-{xxxpdf} \"C\" \"FNAME\")
;; (ee-find-{xxxpdf} \"FNAME\")

\(defun      find-{xxxpdf} (fname &optional page &rest rest)
  (apply 'find-sh-page (ee-find-{xxxpdf} fname) page rest))

\(defun      code-{xxxpdf} (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-{xxxpdf} c fname rest))))
\(defun find-code-{xxxpdf} (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-{xxxpdf} c fname rest)))
\(defun   ee-code-{xxxpdf} (c fname &optional offset)
  (setq offset (or offset 0))
  (ee-template0 \"\\
\(setq ee-page-c      {<}(ee-pp0 c){>})
\(setq ee-page-fname  {<}(ee-pp0 fname){>})
\(setq ee-page-offset {<}(ee-pp0 offset){>})
\(defun find-{<}c{>}text (&optional page &rest rest)
  (setq ee-page-c      {<}(ee-pp0 c){>})
  (setq ee-page-fname  {<}(ee-pp0 fname){>})
  (setq ee-page-offset {<}(ee-pp0 offset){>})
  (apply 'find-{xxxpdf} {<}(ee-pp0 fname){>} page rest))
\"))
"))

;; «code-xxxpdf-alias» (to ".code-xxxpdf-alias")
;; Test: (find-code-xxxpdf-alias "pdfpage" "xpdf-page")
;;
(defun      code-xxxpdf-alias (xxxpdfnew xxxpdfold)
  (eval (ee-read      (ee-code-xxxpdf-alias xxxpdfnew xxxpdfold))))
(defun find-code-xxxpdf-alias (xxxpdfnew xxxpdfold)
  (find-estring-elisp (ee-code-xxxpdf-alias xxxpdfnew xxxpdfold)))
(defun   ee-code-xxxpdf-alias (xxxpdfnew xxxpdfold)
  (ee-template0 "\
;; (find-code-xxxpdf-alias \"xxxpdfnew\" \"xxxpdfold\")
;; (find-code-xxxpdf-alias \"pdf-page\" \"xpdf-page\")
;; (find-code-xxxpdf-alias \"pdfpage\" \"xpdf-page\")
;;
\(defalias   'ee-find-{xxxpdfnew}   'ee-find-{xxxpdfold})
\(defalias      'find-{xxxpdfnew}      'find-{xxxpdfold})
\(defalias   'ee-code-{xxxpdfnew}   'ee-code-{xxxpdfold})
\(defalias      'code-{xxxpdfnew}      'code-{xxxpdfold})
\(defalias 'find-code-{xxxpdfnew} 'find-code-{xxxpdfold})
"))




;;;   __ _           _                     _  __                              
;;;  / _(_)_ __   __| |    __  ___ __   __| |/ _|      _ __   __ _  __ _  ___ 
;;; | |_| | '_ \ / _` |____\ \/ / '_ \ / _` | |_ _____| '_ \ / _` |/ _` |/ _ \
;;; |  _| | | | | (_| |_____>  <| |_) | (_| |  _|_____| |_) | (_| | (_| |  __/
;;; |_| |_|_| |_|\__,_|    /_/\_\ .__/ \__,_|_|       | .__/ \__,_|\__, |\___|
;;;                             |_|                   |_|          |___/      
;;
;; «find-xpdf-page» (to ".find-xpdf-page")
;; (find-pdflikedef-links "xpdf" "c fname")
;; (find-code-xxxpdf-family "xpdf-page")
        (code-xxxpdf-family "xpdf-page")

(defvar ee-find-xpdf-page-options '("-fullscreen"))
(defun  ee-find-xpdf-page (fname &optional page &rest rest)
  `("xpdf"
    ,@ee-find-xpdf-page-options
    ,fname
    ,@(if page `(,(format "%d" page)))
    ))



;;;   __ _           _                 _  __                              
;;;  / _(_)_ __   __| |      _ __   __| |/ _|      _ __   __ _  __ _  ___ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _` | |_ _____| '_ \ / _` |/ _` |/ _ \
;;; |  _| | | | | (_| |_____| |_) | (_| |  _|_____| |_) | (_| | (_| |  __/
;;; |_| |_|_| |_|\__,_|     | .__/ \__,_|_|       | .__/ \__,_|\__, |\___|
;;;                         |_|                   |_|          |___/      
;;
;; «find-pdf-page» (to ".find-pdf-page")
;; (find-code-xxxpdf-alias "pdf-page" "xpdf-page")
        (code-xxxpdf-alias "pdf-page" "xpdf-page")


;;;   __ _           _                 _  __       _            _   
;;;  / _(_)_ __   __| |      _ __   __| |/ _|     | |_ _____  _| |_ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _` | |_ _____| __/ _ \ \/ / __|
;;; |  _| | | | | (_| |_____| |_) | (_| |  _|_____| ||  __/>  <| |_ 
;;; |_| |_|_| |_|\__,_|     | .__/ \__,_|_|        \__\___/_/\_\\__|
;;;                         |_|                                     
;;
;; «find-pdf-text» (to ".find-pdf-text")
;; (find-code-xxxpdftext-family "pdf-text")
        (code-xxxpdftext-family "pdf-text")

;; (defun ee-find-pdf-text (fname)
;;   (format "pdftotext -layout -enc Latin1 '%s' -" (ee-expand fname)))
;; 
;; (defun ee-find-pdftotext-text (fname)
;;   (format "pdftotext -layout -enc Latin1 '%s' -" (ee-expand fname)))

(defvar ee-pdftotext-program "pdftotext")

(defun ee-find-pdf-text (fname &rest rest)
  `(,ee-pdftotext-program "-layout" "-enc" "Latin1" ,(ee-expand fname) "-"))

(defun ee-find-pdftotext-text (fname &rest rest)
  `(,ee-pdftotext-program "-layout" "-enc" "Latin1" ,(ee-expand fname) "-"))








;;;   ___  _   _                                           
;;;  / _ \| |_| |__   ___ _ __   _ __  _ __ ___   __ _ ___ 
;;; | | | | __| '_ \ / _ \ '__| | '_ \| '__/ _ \ / _` / __|
;;; | |_| | |_| | | |  __/ |    | |_) | | | (_) | (_| \__ \
;;;  \___/ \__|_| |_|\___|_|    | .__/|_|  \___/ \__, |___/
;;;                             |_|              |___/     
;;
;; «find-texworkspdf-page» (to ".find-texworkspdf-page")
;; TODO: make TeXworks the default PDF viewer on W$s and Macs.
;; Tests: (find-es "texworks" "find-texworkspdf-page")
;; See: https://tug.org/pipermail/tex-live/2019-March/043227.html
;;      https://tug.org/pipermail/tex-live/2019-March/043229.html
;; (find-code-xxxpdf-family "texworkspdf-page")
        (code-xxxpdf-family "texworkspdf-page")

(defvar ee-texworks-program "texworks")

(defun  ee-find-texworkspdf-page (fname &optional page)
  `(,ee-texworks-program
    ,@(if page `(,(format "--position=%d" page)))
    ,fname
    ))



;; «find-pdftools-page» (to ".find-pdftools-page")
;; New (2019mar08). Requires: (find-epackage 'pdf-tools)
;; See: http://angg.twu.net/find-pdf-page.html
;; (find-code-xxxpdf-family "pdftools-page")
        (code-xxxpdf-family "pdftools-page")

(defun find-pdftools-page (fname &optional page &rest rest)
  (pdf-tools-install)
  (find-fline fname)
  (if page (pdf-view-goto-page (or page 1))))



;; «find-xdvi-page» (to ".find-xdvi-page")
;; (find-code-xxxpdf-family "xdvi-page")
        (code-xxxpdf-family "xdvi-page")

(defvar ee-find-xdvi-page-options '())
(defun  ee-find-xdvi-page (fname &optional page)
  `("xdvi"
    ,@ee-find-xdvi-page-options
    ,@(if page `(,(format "+%d" page)))
    ,fname))


;; «find-djview-page» (to ".find-djview-page")
;; (find-code-xxxpdf-family "djview-page")
        (code-xxxpdf-family "djview-page")

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


;; «find-evince-page» (to ".find-evince-page")
;; (find-code-xxxpdf-family "evince-page")
        (code-xxxpdf-family "evince-page")

(defun     find-evince-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-evince-page fname page)))
(defvar ee-find-evince-page-options '())
(defun  ee-find-evince-page (fname &optional page)
  `("evince"
    ,@ee-find-evince-page-options
    ;; ,@(if page `(,(format "--page-label=%d" page)))
    ,@(if page `(,(format "--page-index=%d" page)))
    ,fname))


;; «find-gv-page» (to ".find-gv-page")
;; (find-code-xxxpdf-family "gv-page")
        (code-xxxpdf-family "gv-page")

(defvar ee-find-gv-page-options '())
(defun  ee-find-gv-page (fname &optional page)
  `("gv"
    ,@ee-find-gv-page-options
    ,@(if page `(,(format "--page=%d" page)))
    ,fname))


;; «find-djvutxt-text» (to ".find-djvutxt-text")
;; (find-code-xxxpdftext-family "djvutxt-text")
        (code-xxxpdftext-family "djvutxt-text")

(defun ee-find-djvutxt-text (fname)
  (format "djvutxt '%s'" fname))



;;;     _    _ _                     
;;;    / \  | (_) __ _ ___  ___  ___ 
;;;   / _ \ | | |/ _` / __|/ _ \/ __|
;;;  / ___ \| | | (_| \__ \  __/\__ \
;;; /_/   \_\_|_|\__,_|___/\___||___/
;;;                                  
;; «aliases» (to ".aliases")
;; For compatibility the with previous versions - that were a mess.
;; At some point these aliases will all be commented out, and if you
;; depend on them what you I suggest you to do is:
;;   1) copy them (uncommented) to your .emacs
;;   2) rename your calls to aliases functions to standard functions
;;   3) comment one of your calls to `code-xxxpdf-alias'; start emacs
;;      again to check if your .emacs really didn't depend on these
;;      functions; comment another call to `code-xxxpdf-alias'; etc,
;;      etc; wash, rinse, repeat.

        (code-xxxpdf-alias "djvupage"   "djview-page")
        (code-xxxpdf-alias "djvu-text"  "djvutxt-text")
        (code-xxxpdf-alias "evincepage" "evince-page")
        (code-xxxpdf-alias "pspage"     "gv-page")
        (code-xxxpdf-alias "xdvipage"   "xdvi-page")
        (code-xxxpdf-alias "xpdfpage"   "xpdf-page")
        (code-xxxpdf-alias "xpdf"       "xpdf-page")
        (code-xxxpdf-alias "xdvi"       "xdvi-page")
        (code-xxxpdf-alias "dvi"        "xdvi-page")
        (code-xxxpdf-alias "djvu"       "djview-page")
        (code-xxxpdf-alias "pdf"        "xpdf-page")
        (code-xxxpdf-alias "evince"     "evince-page")

;; (find-code-xxxpdf-alias "djvu-text"  "djvutxt-text")
;; (find-code-xxxpdf-alias "djvupage"   "djview-page")
;; (find-code-xxxpdf-alias "evincepage" "evince-page")
;; (find-code-xxxpdf-alias "pspage"     "gv-page")
;; (find-code-xxxpdf-alias "xdvipage"   "xdvi-page")
;; (find-code-xxxpdf-alias "xpdfpage"   "xpdf-page")
;; (find-code-xxxpdf-alias "xpdf"       "xpdf-page")
;; (find-code-xxxpdf-alias "xdvi"       "xdvi-page")
;; (find-code-xxxpdf-alias "dvi"        "xdvi-page")
;; (find-code-xxxpdf-alias "djvu"       "djview-page")
;; (find-code-xxxpdf-alias "pdf"        "xpdf-page")
;; (find-code-xxxpdf-alias "evince"     "evince-page")



;; «aliases-windows»  (to ".aliases-windows")
;; (find-elnode "System Environment")
;; (find-evardescr 'system-type)
(if (eq system-type 'windows-nt)
    (code-xxxpdf-alias "pdf-page" "texworkspdf-page"))



;; «code-brxxxs» (to ".code-brxxxs")

(code-brfile 'find-pdf-text    :local 'brpdftextl  :dired 'brpdftextd)
(code-brfile 'find-djvu-text   :local 'brdjvutextl :dired 'brdjvutextd)
(code-brfile 'find-xpdf-page   :local 'brxpdfl     :dired 'brxpdfd)
(code-brfile 'find-evince-page :local 'brevincel   :dired 'brevinced)
(code-brfile 'find-xdvi-page   :local 'brxdvil     :dired 'brxdvid)
(code-brfile 'find-djvu-page   :local 'brdjvul     :dired 'brdjvud)




(provide 'eev-pdflike)




;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
