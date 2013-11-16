;;; eev-wrap.el --- wrap the current line into a hyperlink

;; Copyright (C) 2013 Free Software Foundation, Inc.
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
;; Version:    2013aug10
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-wrap.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-wrap.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-wrap-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-wrap-intro)

;;; Commentary:

;; This module installs new key bindings into eev-mode-keymap, so:
(require 'eev-mode)			; (find-eev "eev-mode.el")


(defvar ee-hyperlink-prefix "# "
  "Hyperlinks created by `ee-HS' are prefixed with this.
The best way to change this variable interactively is by running
`M-x ee-hyperlink-prefix'.")

(defvaralias 'ee-H 'ee-hyperlink-prefix)



;;;                  _                       _       _        ___  
;;;   ___  ___      | |_ ___ _ __ ___  _ __ | | __ _| |_ ___ / _ \ 
;;;  / _ \/ _ \_____| __/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \ | | |
;;; |  __/  __/_____| ||  __/ | | | | | |_) | | (_| | ||  __/ |_| |
;;;  \___|\___|      \__\___|_| |_| |_| .__/|_|\__,_|\__\___|\___/ 
;;;                                   |_|                          
;;
(defun ee-template00 (str)
"Replace substrings enclosed by `{}'s in STR by the result of evaluating them.
Examples:\n
  (ee-template00 \"a{(+ 2 3)}b\")
    -->  \"a5b\"\n
  (let ((hi \"Here:\") (a 22) (b 33))
    (ee-template00 \"{hi} {a} + {b} = {(+ a b)}\"))  
    -->  \"22 + 33 = 55\""
  (replace-regexp-in-string
   "{\\([^{}]+\\)}"
   (lambda (_code_) (format "%s" (eval (read (substring _code_ 1 -1)))))
   str 'fixedcase 'literal))

(defun ee-template0 (str)
"Replace substrings enclosed by `{}'s in STR by the result of evaluating them.
Substrings of the form `{<}' and `{>}' in STR are replaced by `{'
and `}' respectively; apart from that, this is the same as
`ee-template00'.
Example:  (ee-template0 \"{<} a{(+ 2 3)} {>}\")  -->  \"{ 5 }\""
  (let ((< "{") (> "}"))
    (ee-template00 str)))


;;;                 ____  
;;;   ___  ___     / ___| 
;;;  / _ \/ _ \____\___ \ 
;;; |  __/  __/_____|__) |
;;;  \___|\___|    |____/ 
;;;                       
;; ee-S and ee-HS, for pretty-printing of sexps
;; (mainly for use in ee-template0)
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

(defun ee-add-quote (obj)
  "Return OBJ is OBJ is constant; else return 'OBJ."
  (if (or (numberp obj) (stringp obj) (eq obj nil) (eq obj t) (keywordp obj))
      obj
    (list 'quote obj)))





;;;  _   _     _           _ _            
;;; | |_| |__ (_)___      | (_)_ __   ___ 
;;; | __| '_ \| / __|_____| | | '_ \ / _ \
;;; | |_| | | | \__ \_____| | | | | |  __/
;;;  \__|_| |_|_|___/     |_|_|_| |_|\___|
;;;                                       
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
;; See: (find-anchors-intro "Creating index/section anchor pairs")
(define-key eev-mode-map "\M-A" 'eewrap-anchor)

(defun  eewrap-anchor () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-anchor))
(defun ee-wrap-anchor (line)
  "An internal function used by `eewrap-anchor'."
  (if (string-match "^\\(.*\\)<\\([^<>]*\\)>" line)
      (ee-wrap-anchor0 (match-string 1 line) (match-string 2 line))
    (error "Does not match")))
(defun ee-wrap-anchor0 (prefix anchor)
  "An internal function used by `ee-wrap-anchor'."
  (ee-template0 "\
{prefix}«.{anchor}»\t(to \"{anchor}\")
{prefix}«{anchor}» (to \".{anchor}\")"))


;;;  __  __        ____                   _                          _ 
;;; |  \/  |      / ___|_    ___ ___   __| | ___        ___       __| |
;;; | |\/| |_____| |   (_)  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |
;;; | |  | |_____| |___ _  | (_| (_) | (_| |  __/_____| (_|_____| (_| |
;;; |_|  |_|      \____(_)  \___\___/ \__,_|\___|      \___|     \__,_|
;;;                                                                    
;; See: (find-code-c-d-intro)
(define-key eev-mode-map "\M-C" 'eewrap-code-c-d)

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
(define-key eev-mode-map "\M-D" 'eewrap-debian)

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
;; See: (find-wrap-intro "<M-F>")
(define-key eev-mode-map "\M-F" 'eewrap-find-fline)

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
;; See: (find-eejump-intro "Producing `eejump-nnn's and `eejump-nnn*'s")
(define-key eev-mode-map "\M-J" 'eewrap-eejump)

(defun  eewrap-eejump () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-eejump))
(defun ee-wrap-eejump (n sexp)
  "An internal function used by `eewrap-eejump'."
  (if (equal sexp "")
      (ee-template0 "(defun eejump-{n}* () (find-efunction 'eejump-{n}*))")
    (ee-template0   "(defun eejump-{n} () {sexp})")))


;;;  __  __       __  __                           
;;; |  \/  |     |  \/  |_   _ __ ___   __ _ _ __  
;;; | |\/| |_____| |\/| (_) | '_ ` _ \ / _` | '_ \ 
;;; | |  | |_____| |  | |_  | | | | | | (_| | | | |
;;; |_|  |_|     |_|  |_(_) |_| |_| |_|\__,_|_| |_|
;;;                                                
;; See: (find-wrap-intro "<M-M>")
(define-key eev-mode-map "\M-M" 'eewrap-man)

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
;; See: (find-pdf-like-intro)
(define-key eev-mode-map "\M-P" 'eewrap-pdflike)

(defun  eewrap-pdflike () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-pdflike))
(defun ee-wrap-pdflike (stem fname)
  "An internal function used by `eewrap-pdflike'."
  (ee-template0 "\
;; (find-fline {(ee-S (file-name-directory fname))})
\(code-xpdf     \"{stem}\" \"{fname}\")
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
;; See: (find-pdf-like-intro)
;; (define-key eev-mode-map "\M-Q" 'eewrap-pdflike-link)

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
(define-key eev-mode-map "\M-R" 'eewrap-rm/mkdir/cd)

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
;; See: (find-wrap-intro "<M-S>")
(define-key eev-mode-map "\M-S" 'eewrap-sh)

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
;; (define-key eev-mode-map "\M-T" 'eewrap-eepitch)
;; (find-eev "eepitch.el" "eepitch-wrap")



;;;  __  __    __     __                   _ _            _     _            
;;; |  \/  |   \ \   / /    __ _ _   _  __| (_) _____   _(_) __| | ___  ___  
;;; | |\/| |____\ \ / (_)  / _` | | | |/ _` | |/ _ \ \ / / |/ _` |/ _ \/ _ \ 
;;; | |  | |_____\ V / _  | (_| | |_| | (_| | | (_) \ V /| | (_| |  __/ (_) |
;;; |_|  |_|      \_/ (_)  \__,_|\__,_|\__,_|_|\___/ \_/ |_|\__,_|\___|\___/ 
;;;                                                                          
;; See: (find-audiovideo-intro)
(define-key eev-mode-map "\M-V" 'eewrap-audiovideo)

(defun  eewrap-audiovideo () (interactive)
  (ee-this-line-wrapn 2 'ee-wrap-audiovideo))
(defun ee-wrap-audiovideo (stem fname)
  "An internal function used by `eewrap-audiovideo'."
  (ee-template0 "\
;; (find-fline {(ee-S (file-name-directory fname))})
\(code-audio \"{stem}\" \"{fname}\")
\(code-video \"{stem}\" \"{fname}\")
;; \(find-{stem})
"))

;; 2013nov15:
(defun ee-wrap-audiovideo (stem fname)
  "An internal function used by `eewrap-audiovideo'."
  (ee-template0 "\
;; (find-fline {(ee-S (file-name-directory fname))})
;; (find-audio \"{fname}\")})
;; (find-video \"{fname}\")})
\(code-audio \"{stem}\" \"{fname}\")
\(code-video \"{stem}\" \"{fname}\")
;; \(find-{stem})
;; \(find-{stem} \"0:00\")
;; \(find-{stem} t)
;; \(eev-avadj-mode 1)
"))



;;;  __  __      _____     __ _           _              _     
;;; |  \/  |    |__  /_   / _(_)_ __   __| |     _______| |__  
;;; | |\/| |_____ / /(_) | |_| | '_ \ / _` |____|_  / __| '_ \ 
;;; | |  | |_____/ /_ _  |  _| | | | | (_| |_____/ /\__ \ | | |
;;; |_|  |_|    /____(_) |_| |_|_| |_|\__,_|    /___|___/_| |_|
;;;                                                            
(define-key eev-mode-map "\M-Z" 'eewrap-zsh)

(defun  eewrap-zsh () (interactive)
  (ee-this-line-wrapn 1 'ee-wrap-zsh))
(defun ee-wrap-zsh (str)
  "An internal function used by `eewrap-zsh'."
  (ee-HS `(find-zsh ,str)))





;;;                                    /\ ____  
;;;   ___  _____      ___ __ __ _ _ __|/\|___ \ 
;;;  / _ \/ _ \ \ /\ / / '__/ _` | '_ \    __) |
;;; |  __/  __/\ V  V /| | | (_| | |_) |  / __/ 
;;;  \___|\___| \_/\_/ |_|  \__,_| .__/  |_____|
;;;                              |_|            
;;
;; See: (find-eev "eev-tlinks.el" "find-find-links-links")
;;      (find-wrap-intro "eewrap-eewrap")
;; This is somewhat similar to `find-find-links-links',
;; but it is MUCH more primitive - consider it a demo!

(defun  eewrap-eewrap () (interactive)
  (ee-this-line-wrapn 3 'ee-wrap-eewrap))
(defun ee-wrap-eewrap (C stem args)
  (let ((n (length (ee-split args))))
    (ee-template0 "
;; M-{C}: {stem}
\(define-key eev-mode-map \"\\M-{C}\" 'eewrap-{stem})

\(defun  eewrap-{stem} () (interactive)
  (ee-this-line-wrapn {n} 'ee-wrap-{stem}))
\(defun ee-wrap-{stem} ({args})
  \"An internal function used by `eewrap-{stem}'.\"
  (ee-template0 \"\\
{<}(ee-HS `(find-{stem} ,{args})){>}\"))\n")))


;; A more standard way to create `eewrap-*' functions.
;; (find-find-links-links "<none>" "eewrap" "C stem args")
;;
(defun find-eewrap-links (&optional C stem args &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for foo."
  (interactive)
  (setq C    (or C    "{C}"))
  (setq stem (or stem "{stem}"))
  (setq args (or args "{args}"))
  (apply 'find-elinks-elisp
   `((find-eewrap-links ,C ,stem ,args ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-eewrap-links)
     ,(ee-wrap-eewrap C stem args)
     )
   pos-spec-list))

;; Test: (find-eewrap-links)





;;;                                  _   
;;;   ___ ___  _ __ ___  _ __   __ _| |_ 
;;;  / __/ _ \| '_ ` _ \| '_ \ / _` | __|
;;; | (_| (_) | | | | | | |_) | (_| | |_ 
;;;  \___\___/|_| |_| |_| .__/ \__,_|\__|
;;;                     |_|              

;; (defalias    'ee-H 'ee-hyperlink-prefix)
;; (defalias    'ee-S 'ee-pp0)

(defalias 'ee-pp0 'ee-S)		; compatibility


(provide 'eev-wrap)





;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun  %s "
;; no-byte-compile:   t
;; End:
