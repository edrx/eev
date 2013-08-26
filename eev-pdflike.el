;;; eev-pdflike.el -- hyperlinks to documents made of pages.

;; Copyright (C) 2012,2013 Free Software Foundation, Inc.
;;
;; This file is (not yet?) part of GNU eev.
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
;; Version:    2013jun16
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




;; (code-brfile 'find-fline                  :local 'brfl)
;; (code-brfile 'find-xpdfpage               :local 'brxpdf :dired 'brxpdfd)
;; (code-brurl  'find-firefox  :remote 'brm  :local 'brml   :dired 'brmd)


(require 'eev-brxxx)

;;;                  _       _     _           
;;; __   ____ _ _ __(_) __ _| |__ | | ___  ___ 
;;; \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;;  \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;;   \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;
(defvar ee-page-c      "{?}")
(defvar ee-page-fname  "{?}")
(defvar ee-page-offset 0)





;;;  ____  ____  _____     _ _ _        
;;; |  _ \|  _ \|  ___|   | (_) | _____ 
;;; | |_) | | | | |_ _____| | | |/ / _ \
;;; |  __/| |_| |  _|_____| | |   <  __/
;;; |_|   |____/|_|       |_|_|_|\_\___|
;;;                                     
;; See: (find-pdf-like-intro)


(defun ee-code-pdftext-rest (rest)
  (ee-template0 "
;; {(ee-S `(ee-code-pdftext-rest ,@rest))}
"))


;;;                 _  __ 
;;; __  ___ __   __| |/ _|
;;; \ \/ / '_ \ / _` | |_ 
;;;  >  <| |_) | (_| |  _|
;;; /_/\_\ .__/ \__,_|_|  
;;;      |_|              
;;
;; (find-pdflikedef-links "xpdf" "c fname")
;;
;; find-xpdfpage
;; find-xpdf-page
;; code-xpdf
;;
(defalias 'find-xpdfpage
          'find-xpdf-page)
(defun     find-xpdf-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-xpdf-page fname page)))
(defvar ee-find-xpdf-page-options '())
(defun  ee-find-xpdf-page (fname &optional page)
  `("xpdf"
    ,@ee-find-xpdf-page-options
    ,fname
    ,@(if page `(,(format "%d" page)))
    ))

(defun      code-xpdf (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-xpdf c fname rest))))
(defun find-code-xpdf (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-xpdf c fname rest)))
(defun   ee-code-xpdf (c fname &rest rest)
  (concat (ee-template0 "\
;; {(ee-S `(find-code-xpdf ,c ,fname ,@rest))} 
;;
\(setq ee-pdflike-last 'find-{c}page)
\(defun find-{c}page (&optional page &rest rest)
  (setq ee-pdflike-last 'find-{c}page)
  (find-xpdf-page {(ee-pp0 fname)} page))
")  (ee-code-pdftext-rest rest)))

(code-brfile 'find-xpdf-page :local 'brxpdfl :dired 'brxpdfd)



;;;            _  __ 
;;;  _ __   __| |/ _|
;;; | '_ \ / _` | |_ 
;;; | |_) | (_| |  _|
;;; | .__/ \__,_|_|  
;;; |_|              
;;
(defalias 'find-pdfpage  'find-xpdfpage)
(defalias 'find-pdf-page 'find-xpdf-page)
(defalias      'code-pdf      'code-xpdf)
(defalias 'find-code-pdf 'find-code-xpdf)



;;;             _                
;;;   _____   _(_)_ __   ___ ___ 
;;;  / _ \ \ / / | '_ \ / __/ _ \
;;; |  __/\ V /| | | | | (_|  __/
;;;  \___| \_/ |_|_| |_|\___\___|
;;;                              
;;
;; (find-pdflikedef-links "evince" "c fname")
;; (find-man "1 evince")
;;
;; find-evincepage
;; find-evince-page
;; code-evince
;;
(defalias 'find-evincepage
          'find-evince-page)
(defun     find-evince-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-evince-page fname page)))
(defvar ee-find-evince-page-options '())
(defun  ee-find-evince-page (fname &optional page)
  `("evince"
    ,@ee-find-evince-page-options
    ,@(if page `(,(format "--page-label=%d" page)))
    ,fname))

(defun      code-evince (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-evince c fname rest))))
(defun find-code-evince (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-evince c fname rest)))
(defun   ee-code-evince (c fname &rest rest)
  (concat (ee-template0 "\
\(defun find-{c}page (&optional page &rest rest)
  (find-evince-page {(ee-pp0 fname)} page))
{(ee-code-pdftext-rest rest)}
")  (ee-code-pdftext-rest rest)))

(code-brfile 'find-evince-page :local 'brevincel :dired 'brevinced)



;;;          _       _ 
;;; __  ____| |_   _(_)
;;; \ \/ / _` \ \ / / |
;;;  >  < (_| |\ V /| |
;;; /_/\_\__,_| \_/ |_|
;;;                    
;;
;; (find-pdflikedef-links "xdvi" "c fname")
;;
;; find-xdvipage
;; find-xdvi-page
;; code-xdvi
;;
(defalias 'find-xdvipage
          'find-xdvi-page)
(defun     find-xdvi-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-xdvi-page fname page)))
(defvar ee-find-xdvi-page-options '())
(defun  ee-find-xdvi-page (fname &optional page)
  `("xdvi"
    ,@ee-find-xdvi-page-options
    ,@(if page `(,(format "+%d" page)))
    ,fname))

(defun      code-xdvi (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-xdvi c fname rest))))
(defun find-code-xdvi (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-xdvi c fname rest)))
(defun   ee-code-xdvi (c fname &rest rest)
  (concat (ee-template0 "\
\(defun find-{c}page (&optional page &rest rest)
  (find-xdvi-page {(ee-pp0 fname)} page))
{(ee-code-pdftext-rest rest)}
")  (ee-code-pdftext-rest rest)))

(code-brfile 'find-xdvi-page :local 'brxdvil :dired 'brxdvid)

(defalias      'code-dvi      'code-xdvi)
(defalias 'find-code-dvi 'find-code-xdvi)



;;;      _  _             
;;;   __| |(_)_   ___   _ 
;;;  / _` || \ \ / / | | |
;;; | (_| || |\ V /| |_| |
;;;  \__,_|/ | \_/  \__,_|
;;;      |__/             
;;
;; (find-pdflikedef-links "djvu" "c fname")
;;
;; find-djvupage
;; find-djvu-page
;; code-djvu
;;
(defalias 'find-djvupage
          'find-djvu-page)
(defun     find-djvu-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-djvu-page fname page)))
(defvar ee-find-djvu-page-options '())
(defun  ee-find-djvu-page (fname &optional page)
  `("djview"
    ,@ee-find-djvu-page-options
    ,@(if page `(,(format "--page=%d" page)))
    ,fname))

(defun      code-djvu (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-djvu c fname rest))))
(defun find-code-djvu (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-djvu c fname rest)))
(defun   ee-code-djvu (c fname &rest rest)
  (concat (ee-template0 "\
\(defun find-{c}page (&optional page &rest rest)
  (find-djvu-page {(ee-pp0 fname)} page))
")  (ee-code-pdftext-rest rest)))

(code-brfile 'find-djvu-page :local 'brdjvul :dired 'brdjvud)




;;;            
;;;  _ __  ___ 
;;; | '_ \/ __|
;;; | |_) \__ \
;;; | .__/|___/
;;; |_|        
;;
;; (find-pdflikedef-links "ps" "c fname")
;;
;; find-pspage
;; find-ps-page
;; code-ps
;;
(defalias 'find-pspage
          'find-ps-page)
(defun     find-ps-page (fname &optional page &rest rest)
  (find-bgprocess (ee-find-ps-page fname page)))
(defvar ee-find-ps-page-options '())
(defun  ee-find-ps-page (fname &optional page)
  `("gv"
    ,@ee-find-ps-page-options
    ,@(if page `(,(format "--page=%d" page)))
    ,fname))

(defun      code-ps (c fname &rest rest)
  (eval (ee-read      (apply 'ee-code-ps c fname rest))))
(defun find-code-ps (c fname &rest rest)
  (find-estring-elisp (apply 'ee-code-ps c fname rest)))
(defun   ee-code-ps (c fname &rest rest)
  (concat (ee-template0 "\
\(defun find-{c}page (&optional page &rest rest)
  (find-ps-page {(ee-pp0 fname)} page))
")  (ee-code-pdftext-rest rest)))







;;;   __ _           _                            _            _   
;;;  / _(_)_ __   __| |    __  ____  ____  __    | |_ _____  _| |_ 
;;; | |_| | '_ \ / _` |____\ \/ /\ \/ /\ \/ /____| __/ _ \ \/ / __|
;;; |  _| | | | | (_| |_____>  <  >  <  >  <_____| ||  __/>  <| |_ 
;;; |_| |_|_| |_|\__,_|    /_/\_\/_/\_\/_/\_\     \__\___/_/\_\\__|
;;;                                                                

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

(defun find-sh-page (command &rest pos-spec-list)
  "Like `find-sh', but interpreting the car of POS-SPEC-LIST as a page."
  (interactive "sShell command: ")
  (find-eoutput-reuse
   command
   `(insert (shell-command-to-string ,command)))
  (apply 'ee-goto-position-page pos-spec-list))

;; find-pdf-text
;;
(defalias 'find-pdf-text
         'find-pdftotext-text)
(defun    find-pdftotext-text (fname &rest rest)
  (apply 'find-sh-page (ee-find-pdftotext-text fname) rest))
(defun ee-find-pdftotext-text (fname)
  (format "pdftotext -layout -enc Latin1 '%s' -" (ee-expand fname)))

(code-brfile 'find-pdf-text
         :local 'brpdftextl
         :dired 'brpdftextd)

;; find-djvu-text
;;
(defalias 'find-djvu-text
         'find-djvutxt-text)
(defun    find-djvutxt-text (fname &rest rest)
  (apply 'find-sh-page (ee-find-djvutxt-text fname) rest))
(defun ee-find-djvutxt-text (fname)
  (format "djvutxt '%s'" fname))

(code-brfile 'find-djvu-text
         :local 'brdjvutextl
         :dired 'brdjvutextd)



;; (find-pdflikedef-links "pdf" "c fname")

(defun      code-pdf-text (c fname &optional offset &rest rest)
  (eval (ee-read      (apply 'ee-code-pdf-text c fname offset rest))))
(defun find-code-pdf-text (c fname &optional offset &rest rest)
  (find-estring-elisp (apply 'ee-code-pdf-text c fname offset rest)))
(defun   ee-code-pdf-text (c fname &optional offset &rest rest)
  (setq offset (or offset 0))
  (concat (ee-template0 "\
;; {(ee-S `(find-code-pdf-text ,c ,fname ,offset ,@rest))}
\(defun find-{c}text (&optional page &rest rest)
  (setq ee-page-c      {(ee-pp0 c)})
  (setq ee-page-fname  {(ee-pp0 fname)})
  (setq ee-page-offset {(ee-pp0 offset)})
  (apply 'find-pdf-text {(ee-pp0 fname)} page rest))

;; Set the defaults now
;; See: (find-pdf-like-intro \"find-code-pdf-text\")
\(setq ee-page-c      {(ee-pp0 c)})
\(setq ee-page-fname  {(ee-pp0 fname)})
\(setq ee-page-offset {(ee-pp0 offset)})
")))


(defun      code-djvu-text (c fname &optional offset &rest rest)
  (eval (ee-read      (apply 'ee-code-djvu-text c fname offset rest))))
(defun find-code-djvu-text (c fname &optional offset &rest rest)
  (find-estring-elisp (apply 'ee-code-djvu-text c fname offset rest)))
(defun   ee-code-djvu-text (c fname &optional offset &rest rest)
  (setq offset (or offset 0))
  (concat (ee-template0 "\
\(defun find-{c}text (&optional page &rest rest)
  (setq ee-page-c      {(ee-pp0 c)})
  (setq ee-page-fname  {(ee-pp0 fname)})
  (setq ee-page-offset {(ee-pp0 offset)})
  (find-djvu-text {(ee-pp0 fname)} page))

;; Set the defaults now - see (find-pdf-like-intro \"find-code-pdf-text\")
\(setq ee-page-c      {(ee-pp0 c)})
\(setq ee-page-fname  {(ee-pp0 fname)})
\(setq ee-page-offset {(ee-pp0 offset)})
")))



;; Tests:
;;   (find-code-pdf-text "foo"   "/tmp/foo.pdf" 3)
;;        (code-pdf-text "foo"   "/tmp/foo.pdf" 3)
;;              (find-footext)
;;              (find-footext 2)


;; (find-efunction 'find-page-links)





;; Test:
;; (find-code-xpdf "{c}" "{fname}" :key "{foo}" :key "{bar}")
;; (find-xpdfpage "~/tmp/discussao_proifesgroups.pdf")

;; Garbage?
;; (defun ee-pspage (fname &optional page gvargs)
;;   `("gv" ,@gvargs ,@(if page (list (format "--page=%d" page))) ,fname))
;; (defun ee-xpdfpage (fname &optional page xpdfargs)
;;   `("xpdf" ,@xpdfargs ,fname ,(format "%s" (or page 1))))
;; (defun ee-djvupage (fname &optional page)
;;   `("djview" ,@(if page (list (format "--page=%d" page))) ,fname))







(provide 'eev-pdflike)




;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
