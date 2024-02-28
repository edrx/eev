;;; eev-wrap.el --- wrap the current line into a hyperlink  -*- lexical-binding: nil; -*-

;; Copyright (C) 2013,2016,2017,2019,2020,2023 Free Software Foundation, Inc.
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
;; Version:    20240220
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-wrap.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-wrap.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                 <http://anggtwu.net/eev-intros/find-wrap-intro.html>
;;                                               (find-eev-intro)
;;                                               (find-wrap-intro)

;;; Commentary:

;; 2019: all the `define-key's in this file are now commented out.
;; They were moved to:
;; (find-eevfile "eev-mode.el" "eewrap-anchor")


;; «.ee-template0»		(to "ee-template0")
;; «.ee-S»			(to "ee-S")
;; «.ee-this-line-wrapn»	(to "ee-this-line-wrapn")
;; «.eewrap-anchor»		(to "eewrap-anchor")
;; «.eewrap-escript-block»	(to "eewrap-escript-block")
;; «.eewrap-code-c-d»		(to "eewrap-code-c-d")
;; «.eewrap-debian»		(to "eewrap-debian")
;; «.eewrap-find-fline»		(to "eewrap-find-fline")
;; «.eewrap-eejump»		(to "eewrap-eejump")
;; «.eewrap-man»		(to "eewrap-man")
;; «.eewrap-pdflike»		(to "eewrap-pdflike")
;; «.eewrap-rm/mkdir/cd»	(to "eewrap-rm/mkdir/cd")
;; «.eewrap-sh»			(to "eewrap-sh")
;; «.eepitch»			(to "eepitch")
;; «.eewrap-audiovideo»		(to "eewrap-audiovideo")
;; «.eewrap-zsh»		(to "eewrap-zsh")
;; «.eewrap-two-eepitches»	(to "eewrap-two-eepitches")
;; «.ee-wrap-eewrap»		(to "ee-wrap-eewrap")
;; «.find-eewrap-links»		(to "find-eewrap-links")
;; «.find-eewraptest-links»	(to "find-eewraptest-links")


(require 'eev-template0)      ; (find-eev "eev-template0.el")


(defvar ee-hyperlink-prefix "# "
  "Hyperlinks created by `ee-HS' are prefixed with this.
The best way to change this variable interactively is by running
`M-x ee-hyperlink-prefix'.")

(defvaralias 'ee-H 'ee-hyperlink-prefix)



;;;                 ____  
;;;   ___  ___     / ___| 
;;;  / _ \/ _ \____\___ \ 
;;; |  __/  __/_____|__) |
;;;  \___|\___|    |____/ 
;;;                       
;;; «ee-S» (to ".ee-S")
;; `ee-S' and `ee-HS', for pretty-printing of sexps (mainly for use in
;; `ee-template0').
;; Tests:
;;   (find-estring (ee-S  '(foo   "bar\nplic")))
;;   (find-estring (ee-HS '(foo   "bar\nplic")))
;;   (find-estring (ee-H  "Some string"))
;;         `(setq a ,(ee-add-quote "foo"))
;;         `(setq a ,(ee-add-quote '(+ 1 2)))
;;   (ee-S `(setq a ,(ee-add-quote '(+ 1 2))))
;;
(defun ee-S (object)
  "Convert OBJECT (usually a sexp) into a string, for use in hyperlinks.
Quote newlines to make it fit in a single line.
The result of this function is always a string that can be `read' as Lisp.
The name of this function comes from the \"S\" in `(format \"%S\" <obj>)'."
  (let ((str (let ((print-escape-newlines t)
		   (print-escape-nonascii t) ; isn't escaping esc, \r, etc
		   (print-quoted t))
	       (prin1-to-string object))))
    (replace-regexp-in-string "\r" "\\\\r" str)))

(defun ee-HS (object) (concat ee-hyperlink-prefix (ee-S object)))

(defun ee-H (str) (format "%s%s" ee-hyperlink-prefix str))

(defun ee-needs-quote (obj)
  "Return t when OBJ needs a \"'\"."
  (not (or (numberp obj) (stringp obj) (eq obj nil) (eq obj t) (keywordp obj))))

(defun ee-add-quote (obj)
  "Return OBJ is OBJ is constant; else return 'OBJ."
  (if (ee-needs-quote obj)
      (list 'quote obj)
    obj))

(defun ee-Q (obj)
  (if (ee-needs-quote obj) (format "'%s" (ee-S obj)) (ee-S obj)))

(defun ee-Qrest (rest)
  (mapconcat (lambda (obj) (format " %s" (ee-Q obj))) rest ""))


(defalias 'ee-pp0 'ee-S)

(defun ee-ppp0 (list) (concat "(" (mapconcat 'ee-pp0 list "\n ") ")\n"))




;; «ee-template0» (to ".ee-template0")
;; Moved to: (find-eev "eev-template0.el")




;;;  _   _     _           _ _            
;;; | |_| |__ (_)___      | (_)_ __   ___ 
;;; | __| '_ \| / __|_____| | | '_ \ / _ \
;;; | |_| | | | \__ \_____| | | | | |  __/
;;;  \__|_| |_|_|___/     |_|_|_| |_|\___|
;;;                                       
;; «ee-this-line-wrapn» (to ".ee-this-line-wrapn")
;; The main function in this block is `ee-this-line-wrapn' -
;; all the `eewrap-*' functions defined below call it.

(defun ee-splitn (n str)
"Example: (ee-splitn 3 \"aa bb cc dd ee\")
             --> (\"aa\" \"bb\" \"cc dd ee\")"
  (if (= n 1) (list str)
    (if (string-match "^\\`[ \t]*\\([^ \t]+\\)[ \t]*" str)
	(cons (match-string 1 str)
	      (ee-splitn (- n 1) (substring str (match-end 0))))
      (cons "" (ee-splitn (- n 1) "")))))

(defun ee-this-line-extract ()
  "Delete the contents of the current line and return it as a string."
  (delete-and-extract-region (ee-bol) (ee-eol)))
(defun ee-this-line-extractn (n)
  "Delete the contents of the current line and return it as a list."
  (ee-splitn n (ee-no-properties (ee-this-line-extract))))
(defun ee-this-line-wrapn (n f)
  "Run F on the current line, after splitting it into N strings.
F is a function that receives N arguments and returns a string.
This function extracts the contents of the curren line, splits it,
runs F on the result of the splitting, inserts the result in the
place of what was deleted, and moves down one line.
If an error happens the original contents are not restored; you
have to run an \"undo\"."
  (insert (apply f (ee-this-line-extractn n)))
  (ee-next-line 1))




;;; --------------------
;;; All the standard wrapping functions, bound to M-UPPERCASE keys.
;;; Note that they are listed in alphabetical order below, and that in
;;; each section the higher level functions come first.





;;;  __  __         _                         _                
;;; |  \/  |       / \   _    __ _ _ __   ___| |__   ___  _ __ 
;;; | |\/| |_____ / _ \ (_)  / _` | '_ \ / __| '_ \ / _ \| '__|
;;; | |  | |_____/ ___ \ _  | (_| | | | | (__| | | | (_) | |   
;;; |_|  |_|    /_/   \_(_)  \__,_|_| |_|\___|_| |_|\___/|_|   
;;;                                                            
;; «eewrap-anchor»  (to ".eewrap-anchor")
;; See:  (find-eev-quick-intro "8.3. Creating index/section anchor pairs")
;;       (find-anchors-intro "Creating index/section anchor pairs")
;; Skel: (find-eewrap-links "A" "anchor" "prefix anchor")
;; Test: (find-eewraptest-links "anchor" "# <foo>")

(defun  eewrap-anchor () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-anchor))
(defun ee-wrap-anchor (line)
  "An internal function used by `eewrap-anchor'."
  (if (string-match "^\\(.*\\)<\\([^<>]*\\)>" line)
      (ee-wrap-anchor0 (match-string 1 line) (match-string 2 line))
    (error "Does not match")))
(defun ee-wrap-anchor0 (prefix anchor)
  "An internal function used by `ee-wrap-anchor'."
  (ee-template0 (ee-tolatin1 "\
{prefix}«.{anchor}»\t(to \"{anchor}\")
{prefix}«{anchor}»  (to \".{anchor}\")")))




;;;  __  __       _                            _       _     _     _ _    
;;; |  \/  |     | |__ _    ___  ___  ___ _ __(_)_ __ | |_  | |__ | | | __
;;; | |\/| |_____| '_ (_)  / _ \/ __|/ __| '__| | '_ \| __| | '_ \| | |/ /
;;; | |  | |_____| |_) |  |  __/\__ \ (__| |  | | |_) | |_  | |_) | |   < 
;;; |_|  |_|     |_.__(_)  \___||___/\___|_|  |_| .__/ \__| |_.__/|_|_|\_\
;;;                                             |_|                       
;; «eewrap-escript-block»  (to ".eewrap-escript-block")
;; See:   (find-eev-quick-intro "8.4. Creating e-script blocks")
;; Skel:  (find-eewrap-links "B" "escript-block" "anchor title")
;; Tests: (find-eewraptest-links "escript-block" "ANCHOR")
;;        (find-eewraptest-links "escript-block" "ANCHOR Some title")

;; (define-key eev-mode-map "\M-B" 'eewrap-escript-block)

(defun  eewrap-escript-block () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-escript-block))
(defun ee-wrap-escript-block (anchor title &optional date)
  "An internal function used by `ee-wrap-escript-block'."
  (setq date (or date (downcase (format-time-string "%Y%b%d"))))
  (if (equal title "") (setq title anchor))
  (ee-template0 (ee-tolatin1 "\
#####
#
# {title}
# {date}
#
#####

# «.{anchor}»\t(to \"{anchor}\")
# «{anchor}»  (to \".{anchor}\")")))





;;;  __  __        ____                   _                          _ 
;;; |  \/  |      / ___|_    ___ ___   __| | ___        ___       __| |
;;; | |\/| |_____| |   (_)  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |
;;; | |  | |_____| |___ _  | (_| (_) | (_| |  __/_____| (_|_____| (_| |
;;; |_|  |_|      \____(_)  \___\___/ \__,_|\___|      \___|     \__,_|
;;;                                                                    
;; «eewrap-code-c-d»  (to ".eewrap-code-c-d")
;; See:  (find-eev-quick-intro "`M-C'")
;; Skel: (find-eewrap-links "C" "code-c-d" "c d")
;; Test: (find-eewraptest-links "code-c-d" "CCC /DIR/")

(defun  eewrap-code-c-d () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-code-c-d))
(defun ee-wrap-code-c-d (c d)
  "An internal function used by `eewrap-code-c-d'."
  (ee-template0 "\
\(code-c-d \"{c}\" \"{d}\"\)
;; (find-{c}file \"\")"))


;;;  __  __       ____          _      _     _             
;;; |  \/  |     |  _ \ _    __| | ___| |__ (_) __ _ _ __  
;;; | |\/| |_____| | | (_)  / _` |/ _ \ '_ \| |/ _` | '_ \ 
;;; | |  | |_____| |_| |_  | (_| |  __/ |_) | | (_| | | | |
;;; |_|  |_|     |____/(_)  \__,_|\___|_.__/|_|\__,_|_| |_|
;;;                                                        
;; «eewrap-debian»  (to ".eewrap-debian")
;; See:  (find-wrap-intro "M-D")
;; Skel: (find-eewrap-links "D" "debian" "stem")
;; Test: (find-eewraptest-links "debian" "bash")

(defun  eewrap-debian () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-debian))
(defun ee-wrap-debian (stem)
  (ee-template0 "\
{ee-H}(find-status   \"{stem}\")
{ee-H}(find-vldifile \"{stem}.list\")
{ee-H}(find-udfile   \"{stem}/\")"))


;;;  __  __       _____     __ _ _      
;;; |  \/  |     |  ___|   / _(_) | ___ 
;;; | |\/| |_____| |_ (_) | |_| | |/ _ \
;;; | |  | |_____|  _| _  |  _| | |  __/
;;; |_|  |_|     |_|  (_) |_| |_|_|\___|
;;;                                     
;; «eewrap-find-fline»  (to ".eewrap-find-fline")
;; See: (find-wrap-intro "<M-F>")
;; Skel: (find-eewrap-links "F" "find-fline" "fname")
;; Test: (find-eewraptest-links "find-fline" "/tmp/foo")

(defun  eewrap-find-fline () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-find-fline))
(defun ee-wrap-find-fline (fname)
  "An internal function used by `eewrap-find-fline'."
  (ee-HS `(find-fline ,fname)))


;;;  __  __           _                _                       
;;; |  \/  |         | |_    ___  ___ (_)_   _ _ __ ___  _ __  
;;; | |\/| |_____ _  | (_)  / _ \/ _ \| | | | | '_ ` _ \| '_ \ 
;;; | |  | |_____| |_| |_  |  __/  __/| | |_| | | | | | | |_) |
;;; |_|  |_|      \___/(_)  \___|\___|/ |\__,_|_| |_| |_| .__/ 
;;;                                 |__/                |_|    
;;
;; «eewrap-eejump»  (to ".eewrap-eejump")
;; See: (find-eev-quick-intro "7.1. `eejump'")
;;      (find-eev-quick-intro "7.1. `eejump'" "meta-uppercase-j")
;; Old: (find-eejump-intro "Producing `eejump-nnn's and `eejump-nnn*'s")
;; Skel:  (find-eewrap-links "J" "eejump" "n sexp")
;; Tests: (find-eewraptest-links "eejump" "42   (find-fline \"~/TODO\")")
;;        (find-eewraptest-links "eejump" "todo (find-fline \"~/TODO\")")

(defun  eewrap-eejump () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-eejump))
(defun ee-wrap-eejump (n sexp)
  "An internal function used by `eewrap-eejump'."
  (if (string-match-p "^[0-9]+$" n)
    (if (equal sexp "")
        (ee-template0 "(defun eejump-{n}* () (find-efunction 'eejump-{n}*))")
      (ee-template0   "(defun eejump-{n} () {sexp})"))
    (ee-template0     "(defun {n} () (interactive) {sexp})")))



;;;  __  __       __  __                           
;;; |  \/  |     |  \/  |_   _ __ ___   __ _ _ __  
;;; | |\/| |_____| |\/| (_) | '_ ` _ \ / _` | '_ \ 
;;; | |  | |_____| |  | |_  | | | | | | (_| | | | |
;;; |_|  |_|     |_|  |_(_) |_| |_| |_|\__,_|_| |_|
;;;                                                
;; «eewrap-man»  (to ".eewrap-man")
;; See:  (find-wrap-intro "<M-M>")
;; Skel: (find-eewrap-links "M" "man" "str")
;; Test: (find-eewraptest-links "man" "1 tac")

(defun  eewrap-man () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-man))
(defun ee-wrap-man (str)
  "An internal function used by `eewrap-man'."
  (ee-HS `(find-man ,str)))



;;;  __  __       ____                _  __ _ _ _        
;;; |  \/  |     |  _ \ _   _ __   __| |/ _| (_) | _____ 
;;; | |\/| |_____| |_) (_) | '_ \ / _` | |_| | | |/ / _ \
;;; | |  | |_____|  __/ _  | |_) | (_| |  _| | |   <  __/
;;; |_|  |_|     |_|   (_) | .__/ \__,_|_| |_|_|_|\_\___|
;;;                        |_|                           
;;
;; «eewrap-pdflike»  (to ".eewrap-pdflike")
;; See:  (find-pdf-like-intro)
;;       (find-pdf-like-intro "`M-P'")
;; Skel: (find-eewrap-links "P" "pdflike" "stem fname")
;; Test: (find-eewraptest-links "pdflike" "o /tmp/o.pdf")

(defun  eewrap-pdflike () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-pdflike))
(defun ee-wrap-pdflike (stem fname)
  "An internal function used by `eewrap-pdflike'."
  (ee-template0 "\
;; (find-fline {(ee-S (file-name-directory fname))})
\(code-pdf-page \"{stem}\" \"{fname}\")
\(code-pdf-text \"{stem}\" \"{fname}\")
;; \(find-{stem}page)
;; \(find-{stem}text)
"))



;;;  __  __        ___                _  __ _ _ _              _ _       _    
;;; |  \/  |      / _ \ _   _ __   __| |/ _| (_) | _____      | (_)_ __ | | __
;;; | |\/| |_____| | | (_) | '_ \ / _` | |_| | | |/ / _ \_____| | | '_ \| |/ /
;;; | |  | |_____| |_| |_  | |_) | (_| |  _| | |   <  __/_____| | | | | |   < 
;;; |_|  |_|      \__\_(_) | .__/ \__,_|_| |_|_|_|\_\___|     |_|_|_| |_|_|\_\
;;;                        |_|                                                
;;
;; See: (find-pdf-like-intro)
;; (define-key eev-mode-map "\M-Q" 'eewrap-pdflike-link)
;; OBSOLETE.

(defun  eewrap-pdflike-link () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-pdflike-link))
(defun ee-wrap-pdflike-link (n text)
  "An internal function used by `eewrap-pdflike-link'."
  (format "%s\n%s"
	  (ee-wrap-pdflike-link1 "page" n text)
	  (ee-wrap-pdflike-link1 "text" n text)))
(defun ee-wrap-pdflike-link1 (what n text)
  "An internal function used by `eewrap-pdflike-link'."
  (ee-template0 
   "{ee-H}(find-{ee-page-c}{what} (+ {ee-page-offset} {n}) {(ee-S text)})"))





;;;  __  __       ____                        __         _       _ _      
;;; |  \/  |     |  _ \ _   _ __ _ __ ___    / / __ ___ | | ____| (_)_ __ 
;;; | |\/| |_____| |_) (_) | '__| '_ ` _ \  / / '_ ` _ \| |/ / _` | | '__|
;;; | |  | |_____|  _ < _  | |  | | | | | |/ /| | | | | |   < (_| | | |   
;;; |_|  |_|     |_| \_(_) |_|  |_| |_| |_/_/ |_| |_| |_|_|\_\__,_|_|_|   
;;;                                                                       
;; «eewrap-rm/mkdir/cd»  (to ".eewrap-rm/mkdir/cd")
;; See:  (find-escripts-intro "`M-R'")
;; Skel: (find-eewrap-links "R" "rm/mkdir/cd" "dir")
;; Test: (find-eewraptest-links "rm/mkdir/cd" "/tmp/foo/")

(defun  eewrap-rm/mkdir/cd () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-rm/mkdir/cd))
(defun ee-wrap-rm/mkdir/cd (dir)
  "An internal function used by `eewrap-rm/mkdir/cd'."
  (ee-template0 "\
# (find-fline \"{dir}\")
rm -Rv {dir}
mkdir  {dir}
cd     {dir}"))


;;;  __  __      ____      __ _           _           _     
;;; |  \/  |    / ___|_   / _(_)_ __   __| |      ___| |__  
;;; | |\/| |____\___ (_) | |_| | '_ \ / _` |_____/ __| '_ \ 
;;; | |  | |_____|__) |  |  _| | | | | (_| |_____\__ \ | | |
;;; |_|  |_|    |____(_) |_| |_|_| |_|\__,_|     |___/_| |_|
;;;                                                         
;; «eewrap-sh»  (to ".eewrap-sh")
;; See: (find-wrap-intro "<M-S>")
;; Skel: (find-eewrap-links "R" "sh" "str")
;; Test: (find-eewraptest-links "sh" "dict smop")

(defun  eewrap-sh () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-sh))
(defun ee-wrap-sh (str)
  "An internal function used by `eewrap-sh'."
  (ee-HS `(find-sh ,str)))

(defun  eewrap-sh0 () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-sh0))
(defun ee-wrap-sh0 (str)
  "An internal function used by `eewrap-sh0'."
  (ee-HS `(find-sh0 ,str)))


;;;  __  __     _____                    _ _       _     
;;; |  \/  |   |_   _|    ___  ___ _ __ (_) |_ ___| |__  
;;; | |\/| |_____| |(_)  / _ \/ _ \ '_ \| | __/ __| '_ \ 
;;; | |  | |_____| | _  |  __/  __/ |_) | | || (__| | | |
;;; |_|  |_|     |_|(_)  \___|\___| .__/|_|\__\___|_| |_|
;;;                               |_|                    
;;
;; «eepitch»  (to ".eepitch")
;; Moved to: (find-eev "eepitch.el" "eewrap-eepitch")



;;;  __  __    __     __                   _ _            _     _            
;;; |  \/  |   \ \   / /    __ _ _   _  __| (_) _____   _(_) __| | ___  ___  
;;; | |\/| |____\ \ / (_)  / _` | | | |/ _` | |/ _ \ \ / / |/ _` |/ _ \/ _ \ 
;;; | |  | |_____\ V / _  | (_| | |_| | (_| | | (_) \ V /| | (_| |  __/ (_) |
;;; |_|  |_|      \_/ (_)  \__,_|\__,_|\__,_|_|\___/ \_/ |_|\__,_|\___|\___/ 
;;;                                                                          
;; «eewrap-audiovideo»  (to ".eewrap-audiovideo")
;; See: (find-audiovideo-intro)
;; Skel: (find-eewrap-links "V" "audiovideo" "stem fname")
;; Test: (find-eewraptest-links "audiovideo" "ovideo /tmp/o.mp4")

(defun  eewrap-audiovideo () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-audiovideo))
(defun ee-wrap-audiovideo (stem fname)
  "An internal function used by `eewrap-audiovideo'."
  (ee-template0 "\
;; (find-fline {(ee-S (file-name-directory fname))})
;; (find-audio \"{fname}\")
;; (find-video \"{fname}\")
(code-audio \"{stem}\" \"{fname}\")
(code-video \"{stem}\" \"{fname}\")
;; (find-{stem})
;; (find-{stem} \"0:00\")
;; (find-{stem} t)
;; (eev-avadj-mode 1)
"))



;;;  __  __      _____     __ _           _              _     
;;; |  \/  |    |__  /_   / _(_)_ __   __| |     _______| |__  
;;; | |\/| |_____ / /(_) | |_| | '_ \ / _` |____|_  / __| '_ \ 
;;; | |  | |_____/ /_ _  |  _| | | | | (_| |_____/ /\__ \ | | |
;;; |_|  |_|    /____(_) |_| |_|_| |_|\__,_|    /___|___/_| |_|
;;;                                                            
;; «eewrap-zsh»  (to ".eewrap-zsh")
;; (define-key eev-mode-map "\M-Z" 'eewrap-zsh)
;; Skel: (find-eewrap-links "Z" "zsh" "str")
;; Test: (find-eewraptest-links "zsh" "echo $SHELL")

(defun  eewrap-zsh () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-zsh))
(defun ee-wrap-zsh (str)
  "An internal function used by `eewrap-zsh'."
  (ee-HS `(find-zsh ,str)))



;;;  __  __         _  _       ____                    _ _       _               
;;; |  \/  |      _| || |_ _  |___ \    ___  ___ _ __ (_) |_ ___| |__   ___  ___ 
;;; | |\/| |_____|_  ..  _(_)   __) |  / _ \/ _ \ '_ \| | __/ __| '_ \ / _ \/ __|
;;; | |  | |_____|_      _|_   / __/  |  __/  __/ |_) | | || (__| | | |  __/\__ \
;;; |_|  |_|       |_||_| (_) |_____|  \___|\___| .__/|_|\__\___|_| |_|\___||___/
;;;                                             |_|                              
;;
;; «eewrap-two-eepitches»  (to ".eewrap-two-eepitches")
;; See: (find-multiwindow-intro "4. Several eepitch targets")
;;      (find-multiwindow-intro "7. Eepitch blocks for two targets")
;; Skel: (find-eewrap-links "#" "two-eepitches" "b c")
;; Test: (find-eewraptest-links "two-eepitches" "shell python")

(defun  eewrap-two-eepitches () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-two-eepitches))
(defun ee-wrap-two-eepitches (b c)
  "An internal function used by `eewrap-two-eepitches'."
  (ee-template0 "\
 (find-3EE '(eepitch-{b}) '(eepitch-{c}))
 (find-3ee '(eepitch-{b}) '(eepitch-{c}))
 (eepitch-{b})
 (eepitch-{c})
"))






;;;                                    /\ ____  
;;;   ___  _____      ___ __ __ _ _ __|/\|___ \ 
;;;  / _ \/ _ \ \ /\ / / '__/ _` | '_ \    __) |
;;; |  __/  __/\ V  V /| | | (_| | |_) |  / __/ 
;;;  \___|\___| \_/\_/ |_|  \__,_| .__/  |_____|
;;;                              |_|            
;;
;; «ee-wrap-eewrap»  (to ".ee-wrap-eewrap")
;; See: (find-eev "eev-tlinks.el" "find-find-links-links-new")
;;      (find-wrap-intro "eewrap-eewrap")
;; This is somewhat similar to `find-find-links-links',
;; but it is MUCH more primitive - consider it a demo!

(defun  eewrap-eewrap () (interactive)
  "Obsolete! Superseded by `find-eewrap-links'!"
  (ee-this-line-wrapn 3 'ee-wrap-eewrap))

(defun ee-wrap-eewrap (C stem args)
  "An internal function used by `find-eewrap-links'."
  (let ((n (length (ee-split args))))
    (ee-template0 "
;; (find-efunction 'eewrap-{stem})

;; M-{C}: {stem}
;; (find-eev \"eev-mode.el\" \"eev-mode-map-set\")
;; (find-eev \"eev-mode.el\" \"eev-mode-map-set\" \"M-{C}\")
;; (find-eev \"eev-mode.el\" \"eev-mode-map-set\" \"M-{C}\" \"eewrap-{stem}\")
;; (define-key eev-mode-map \"\\M-{C}\" 'eewrap-{stem})

;; <eewrap-{stem}>
;; Skel: (find-eewrap-links \"{C}\" \"{stem}\" \"{args}\")
;; Test: (find-eewraptest-links \"{stem}\" \"{args}\")

(defun  eewrap-{stem} () (interactive)
  (ee-this-line-wrapn {n} 'ee-wrap-{stem}))
(defun ee-wrap-{stem} ({args})
  \"An internal function used by `eewrap-{stem}'.\"
  (ee-template0 \"\\
{<}(ee-HS `(find-{stem} ,{args})){>}\"))\n")))


;; «find-eewrap-links» (to ".find-eewrap-links")
;; A more standard way to create `eewrap-*' functions.
;; Skel: (find-find-links-links-new "eewrap" "C stem args" "n")
;; Test: (find-eewrap-links)
;;
(defun find-eewrap-links (&optional C stem args &rest pos-spec-list)
  "Visit a temporary buffer containing hyperlinks for eewrap."
  (interactive)
  (setq C    (or C    "{C}"))
  (setq stem (or stem "{stem}"))
  (setq args (or args "{args}"))
  (apply
   'find-elinks-elisp
   `((find-eewrap-links ,C ,stem ,args ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-eewrap-links)
     ,(ee-wrap-eewrap C stem args)
     )
   pos-spec-list))


;; «find-eewraptest-links»  (to ".find-eewraptest-links")
;; See:   (find-wrap-intro "6. All wrapping functions")
;; Skel:  (find-find-links-links-new "eewraptest" "stem line" "")
;; Tests: (find-eewraptest-links "eejump" "42   (find-fline \"~/TODO\")")
;;        (find-eewraptest-links "eejump" "todo (find-fline \"~/TODO\")")
;;
(defun find-eewraptest-links (&optional stem line &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for eewraptest."
  (interactive)
  (setq stem (or stem "{stem}"))
  (setq line (or line "{line}"))
  (apply
   'find-elinks-elisp
   `((find-eewraptest-links ,stem ,line ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-eewraptest-links)
     ""
     ,(ee-template0 "\
;; (eek \"2*<down> <<eewrap-{stem}>>\")
;; (find-efunction 'eewrap-{stem})
{line}
")
     )
   pos-spec-list))




;;;                                  _   
;;;   ___ ___  _ __ ___  _ __   __ _| |_ 
;;;  / __/ _ \| '_ ` _ \| '_ \ / _` | __|
;;; | (_| (_) | | | | | | |_) | (_| | |_ 
;;;  \___\___/|_| |_| |_| .__/ \__,_|\__|
;;;                     |_|              

;; (defalias    'ee-H 'ee-hyperlink-prefix)
;; (defalias    'ee-S 'ee-pp0)

(provide 'eev-wrap)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
