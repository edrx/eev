;;; eev-elinks.el --- `find-efunction-links' and other `find-e*-links'

;; Copyright (C) 2012,2013,2019 Free Software Foundation, Inc.
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
;; Version:    2019feb03
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-elinks.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-elinks.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-links-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-links-intro)

;;; Commentary:

;; See this for a (rough) classification of eev's hyperlink functions
;; into several classes:
;;
;;   (find-links-intro "6. Basic and non-basic hyperlinks")
;;
;; In this file we define `find-elinks' and several functions based on
;; it which generate relatively simple elisp hyperlinks buffers -
;; buffers which are mostly composed of elisp hyperlinks.
;;
;; The "more complex" `find-elinks'-based functions are the ones which
;; use `ee-template0'. They are defined here:
;;
;;   (find-eevfile "eev-tlinks.el")
;;
;; Here's one example of each function in this file:
;; [to be done]




;; «.find-elinks»		(to "find-elinks")
;; «.find-efunction-links»	(to "find-efunction-links")
;; «.find-evariable-links»	(to "find-evariable-links")
;; «.find-ekey-links»		(to "find-ekey-links")
;; «.find-elongkey-links»	(to "find-elongkey-links")
;; «.find-einfo-links»		(to "find-einfo-links")

;; «.ee-code-c-d-pairs-eval»	(to "ee-code-c-d-pairs-eval")
;; «.ee-find-xxxfile-sexps»	(to "ee-find-xxxfile-sexps")
;; «.find-file-links»		(to "find-file-links")
;; «.find-grep-links»		(to "find-grep-links")
;; «.find-ekbmacro-links»	(to "find-ekbmacro-links")
;; «.find-pdflike-page-links»	(to "find-pdflike-page-links")
;; «.ee-hyperlink-prefix»	(to "ee-hyperlink-prefix")
;; «.find-eface-links»		(to "find-eface-links")
;; «.find-color-links»		(to "find-color-links")

;; «.find-here-links»		(to "find-here-links")







;;;                                  _                   _       _   
;;;   __ _ _ __ ___  _   _ _ __   __| |      _ __   ___ (_)_ __ | |_ 
;;;  / _` | '__/ _ \| | | | '_ \ / _` |_____| '_ \ / _ \| | '_ \| __|
;;; | (_| | | | (_) | |_| | | | | (_| |_____| |_) | (_) | | | | | |_ 
;;;  \__,_|_|  \___/ \__,_|_| |_|\__,_|     | .__/ \___/|_|_| |_|\__|
;;;                                         |_|                      

;; (find-eapropos "around-point")

;; (find-elnode "Index" "* thing-at-point:")
;; (find-efunction        'thing-at-point)
;; (find-efile            "thingatpt.el")

(defun ee-stuff-around-point0 (chars)
  (interactive "MChars: \np")		; for tests
  (save-excursion
    (let* ((e (progn (skip-chars-forward  chars) (point)))
	   (s (progn (skip-chars-backward chars) (point))))
      (buffer-substring s e))))

(defun ee-stuff-around-point (chars)
  (interactive "MChars: \np")		; for tests
  (ee-no-properties (ee-stuff-around-point0 chars)))

(defun ee-debpkgname-around-point ()
"Return the name of the Debian package around point.
This function is not very smart."
  (ee-stuff-around-point "a-z0-9-+."))

(defun ee-debpkgname-ask (&optional prompt)
"Ask for the name of a Debian package; the default is the debpkgname at point."
  (read-string (or prompt "Debian package name: ")
	       (ee-debpkgname-around-point)))

(defun ee-manpagename-around-point ()  
"Return the manpagename around point.
This function is not very smart - it doesn't understand section names."
  (interactive)
  (ee-stuff-around-point "A-Za-z0-9-+_:."))

(defun ee-manpagename-ask (&optional prompt)
"Ask for the name of a manpage; the default is the manpage name at point."
  (interactive)
  (read-string (or prompt "Manpage: ")
	       (ee-manpagename-around-point)))




;;;   __ _           _            _ _       _        
;;;  / _(_)_ __   __| |       ___| (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____ / _ \ | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  __/ | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|      \___|_|_|_| |_|_|\_\___/
;;;                                                  
;; «find-elinks»  (to ".find-elinks")
;; A test:
;; (find-elinks '("a" nil (b c) (d "e")))

(defun ee-remove-nils (list)
  "Return a list like LIST, but without the `nil's."
  (let (newlist)
    (mapc (lambda (e) (if e (setq newlist (cons e newlist))))
	  list)
    (nreverse newlist)))

(defun ee-links-to-string0 (list)
  "Convert a LIST of strings and sexps to a big string."
  (mapconcat (lambda (o) (if (stringp o) o (ee-HS o)))
	     (ee-remove-nils list)
	     "\n"))

(defun ee-links-to-string (list)
  "Convert a LIST of strings and sexps to a big string (newline-terminated)."
  (concat (ee-links-to-string0 list)
	  "\n"))

(defun find-elinks (links &rest pos-spec-list)
  "Visit a temporary buffer containing LINKS converted to hyperlink lines."
  (let ((ee-buffer-name (or ee-buffer-name "*Elisp hyperlinks*")))
    (apply 'find-estring (ee-links-to-string links) pos-spec-list)))

(defun find-elinks-elisp (links &rest pos-spec-list)
  "Visit a temporary buffer containing LINKS converted to hyperlink lines.
The buffer is put in Emacs Lisp mode."
  (let ((ee-buffer-name (or ee-buffer-name "*Elisp hyperlinks*"))
	(ee-hyperlink-prefix ";; "))
    (apply 'find-estring-elisp (ee-links-to-string links) pos-spec-list)))





;;;   __ _           _             __                  _   _             
;;;  / _(_)_ __   __| |       ___ / _|_   _ _ __   ___| |_(_) ___  _ __  
;;; | |_| | '_ \ / _` |_____ / _ \ |_| | | | '_ \ / __| __| |/ _ \| '_ \ 
;;; |  _| | | | | (_| |_____|  __/  _| |_| | | | | (__| |_| | (_) | | | |
;;; |_| |_|_| |_|\__,_|      \___|_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|
;;;                                                                      
;; «find-efunction-links» (to ".find-efunction-links")
;; (find-find-links-links "\\M-f" "efunction" "f")
;; A test: (find-efunction-links 'next-line)
;;                  (eek "M-h M-f next-line")

(define-key eev-mode-map "\M-h\M-f" 'find-efunction-links)

(defun find-efunction-links (&optional f &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks related to the function F."
  (interactive (find-function-read))
  (apply 'find-elinks
   `((find-efunction-links ',f ,@pos-spec-list)
     ,@(ee-find-efunction-links f)
     )
   pos-spec-list))

(defun ee-find-efunction-links (f)
  "Return a list of hyperlinks for F (a function symbol).
This is an internal function used by `find-efunction-links' and
`find-ekey-links'."
  `((where-is ',f)
    (describe-function ',f)
    (find-efunctiondescr ',f)
    (find-efunction ',f)
    (find-efunctionpp ',f)
    (find-efunctiond ',f)
    ;; (find-eCfunction ',f)		; obsolete
    (find-estring (documentation ',f))
    (find-estring (documentation ',f t))
    (symbol-file ',f 'defun)
    (find-fline (symbol-file ',f 'defun))
    ""
    ,@(if (commandp f)
	  `((Info-goto-emacs-command-node ',f)
	    (find-enode "Command Index" ,(format "* %S:" f))
	    ))
    (find-elnode "Index" ,(format "* %S:" f))
    ))



;;;   __ _           _                            _       _     _      
;;;  / _(_)_ __   __| |       _____   ____ _ _ __(_) __ _| |__ | | ___ 
;;; | |_| | '_ \ / _` |_____ / _ \ \ / / _` | '__| |/ _` | '_ \| |/ _ \
;;; |  _| | | | | (_| |_____|  __/\ V / (_| | |  | | (_| | |_) | |  __/
;;; |_| |_|_| |_|\__,_|      \___| \_/ \__,_|_|  |_|\__,_|_.__/|_|\___|
;;;                                                                    
;; «find-evariable-links» (to ".find-evariable-links")
;; (find-find-links-links "\\M-v" "evariable" "var")
;; A test: (find-evariable-links 'line-move-visual)
;;                  (eek "M-h M-v line-move-visual")

(define-key eev-mode-map "\M-h\M-v" 'find-evariable-links)

(defun find-evariable-links (var &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for foo."
  (interactive (find-function-read 'variable))
  (apply 'find-elinks
   `((find-evariable-links ',var ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     ,var
     (describe-variable ',var)
     (find-evardescr ',var)
     (find-evariable ',var)
     (find-epp ,var)
     ""
     (find-enode "Variable Index" ,(format "* %S:" var))
     (find-elnode "Index" ,(format "* %S:" var))
     )
   pos-spec-list))


;;;   __ _           _            _              
;;;  / _(_)_ __   __| |       ___| | _____ _   _ 
;;; | |_| | '_ \ / _` |_____ / _ \ |/ / _ \ | | |
;;; |  _| | | | | (_| |_____|  __/   <  __/ |_| |
;;; |_| |_|_| |_|\__,_|      \___|_|\_\___|\__, |
;;;                                        |___/ 
;;
;; «find-ekey-links» (to ".find-ekey-links")
;; (find-find-links-links "\\M-k" "ekey" "key")
;; A test: (find-ekey-links "\C-x2")
;;             (eek "M-h M-k C-x 2")
(define-key eev-mode-map "\M-h\M-k" 'find-ekey-links)

(defun ee-format-kbd-macro (key)
  "Example: (ee-format-kbd-macro [down])  --> \"<down>  ;; next-line\""
  (replace-regexp-in-string "[ \t][ \t]+" "  " (format-kbd-macro key t)))

(defun find-ekey-links (key &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks related to the key sequence KEY."
  (interactive "kElisp hyperlinks for key: ")
  (let ((longkey     (format-kbd-macro key))
	(longkey+ (ee-format-kbd-macro key))
	(f                (key-binding key)))
    (apply 'find-elinks
	   `((find-ekey-links ,key ,@pos-spec-list)
	     ;; Convention: the first sexp always regenerates the buffer.
	     (find-ekey-links          ,key)
	     (eek ,(format "M-h M-k %s" longkey))
	     (eek ,(format "M-h M-k %s" longkey+))
	     ""
	     (find-elongkey-links      ,longkey)
	     (find-elongkey-links      ,longkey+)
	     (find-efunction-links    ',f)
	     ""
	     ,@(ee-find-eboundkey-links key f)
	     )
	   pos-spec-list)))

;; «find-elongkey-links» (to ".find-elongkey-links")
;; A test: (find-elongkey-links "C-x 2")
;;                 (eek "M-h M-k C-x 2")
(defun find-elongkey-links (longkey &rest pos-spec-list)
  "Like `find-ekey-links', but LONGKEY is a key sequence \"spelled out\".
Example: (find-elongkey-links \"M-h M-k\")
See `read-kbd-macro' and `edmacro-mode' for the format."
  (interactive "sElisp hyperlinks for key (long format): ")
  (let* ((key (read-kbd-macro longkey))
	 (f   (key-binding key)))
    (apply 'find-elinks
	   `((find-elongkey-links   ,longkey)
	     (find-ekey-links       ,key)
	     (find-efunction-links ',f)
	     ""
	     ,@(ee-find-eboundkey-links key f)
	     )
	   pos-spec-list)))

(defun ee-find-eboundkey-links (key f)
  "From KEY and its binding, F, produce a list of hyperlinks.
This is an internal function used by `find-ekey-links' and
`find-elongkey-links'."
  `((where-is ',f)
    (describe-function ',f)
    (find-efunctiondescr ',f)
    (find-efunction ',f)
    (find-efunctionpp ',f)
    (find-efunctiond ',f)
    (find-estring (documentation ',f))
    (find-estring (documentation ',f t))
    ""
    (describe-key ,key)
    (describe-key-briefly ,key)
    (find-ekeydescr ,key)
    (Info-goto-emacs-key-command-node ,key)
    (Info-goto-emacs-command-node ',f)
    (find-enode "Command Index" ,(format "* %S:" f))
    (find-elnode "Index" ,(format "* %S:" f))
    ""
    (key-description ,key)
    (format-kbd-macro ,key)
    (format-kbd-macro ,key t)
    (ee-format-kbd-macro ,key)
    (key-binding ,key)
    ))






;;;   __ _           _            _        __       
;;;  / _(_)_ __   __| |       ___(_)_ __  / _| ___  
;;; | |_| | '_ \ / _` |_____ / _ \ | '_ \| |_ / _ \ 
;;; |  _| | | | | (_| |_____|  __/ | | | |  _| (_) |
;;; |_| |_|_| |_|\__,_|      \___|_|_| |_|_|  \___/ 
;;;                                                 
;; «find-einfo-links» (to ".find-einfo-links")
;; (find-find-links-links "\\M-i" "einfo" "")
;; A test: (progn (find-enode "Lisp Eval") (find-einfo-links))
;;         (progn (find-enode "Lisp Eval") (eek "M-h M-i"))
(define-key eev-mode-map "\M-h\M-i" 'find-einfo-links)

(defvar ee-info-file "")

(defun ee-infop       () (get-buffer "*info*"))
(defun ee-info-node   () (with-current-buffer "*info*" Info-current-node))
(defun ee-info-book+  () (with-current-buffer "*info*" Info-current-file))
(defun ee-info-book-  () (file-name-nondirectory (ee-info-book+)))
(defun ee-info-file-  () (file-name-nondirectory  ee-info-file))
(defun ee-info-shortp () (string= (ee-info-book-) (ee-info-file-)))
(defun ee-info-shortf () (ee-intern "find-%snode" ee-info-code))
(defun ee-info-fullnode () (format "(%s)%s" (ee-info-book-) (ee-info-node)))

(defun ee-find-info-links ()
  `((info      ,(ee-info-fullnode))
    (find-node ,(ee-info-fullnode))
    ,(if (ee-info-shortp)
	 (list (ee-info-shortf) (ee-info-node)))))

(defun ee-intro-stem (&optional bufname)
  (setq bufname (or bufname (buffer-name (current-buffer))))
  (if (string-match "^\\*(find-\\(.*\\)-intro)\\*$" bufname)
      (match-string 1 bufname)))

;; (defun ee-find-intro-links (&optional stem)
;;   `((,(ee-intern "find-%s-intro" (or stem (ee-intro-stem))))))

(defun ee-find-intro-links (&optional stem)
  (setq stem (or stem (ee-intro-stem)))
  (let ((find-xxx-intro (ee-intern "find-%s-intro" stem))
	(url (format "http://angg.twu.net/eev-intros/find-%s-intro.html" stem)))
    `(,(ee-H url)
      (,find-xxx-intro)
      )))

  


;; A test: (ee-intro-stem "*(find-foo-intro)*")

(defun find-einfo-links (&optional intro &rest rest)
  "Visit a temporary buffer containing hyperlinks to the current info page.
When possible, try to produce also a shorter hyperlink, like the last one in:
  (info \"(bashref)Pipelines\")
  (find-node \"(bashref)Pipelines\")
  (find-bashnode \"Pipelines\")
The hack for generating the shorter hyperlink uses the global
variables `ee-info-code' and `ee-info-file' - see:
  (progn
   (find-code-c-d \"bash\" \"/usr/share/doc/bash/examples/\" \"bashref\")
   (ee-goto-position \"ee-info-code\"))

As an extra hack, if this function is called from a \"*(find-???-intro)*\"
buffer, also generate a link to that buffer."
  (interactive)
  (setq intro (or intro (ee-intro-stem (buffer-name (current-buffer)))))
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-einfo-links ,intro ,@rest)
    ;; Body:
    ""
    ,@(if (ee-infop)
	  (ee-find-info-links)
	'("[No \"*info*\" buffer]"))
    ""
    ,@(if intro
	 ;; (list (ee-intern "find-%s-intro" intro))
	 (ee-find-intro-links)
       ;; else: "[Not invoked from a \"*find-xxx-intro*\" buffer]"
       )
    ) rest))




;;;                _                          _                   _          
;;;   ___ ___   __| | ___        ___       __| |      _ __   __ _(_)_ __ ___ 
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |_____| '_ \ / _` | | '__/ __|
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |_____| |_) | (_| | | |  \__ \
;;;  \___\___/ \__,_|\___|      \___|     \__,_|     | .__/ \__,_|_|_|  |___/
;;;                                                  |_|                     
;;
;; «ee-code-c-d-pairs-eval» (to ".ee-code-c-d-pairs-eval")

(defun ee-filter (f list)
  "Return the elements of LIST for which (F elt) is true.
Actually return a list of `(F elt)'s."
  (ee-remove-nils (mapcar f list)))

(defun ee-prefixp (prefix str)
  "Return t if STR begins with PREFIX."
  (and (<= (length prefix) (length str))
       (equal prefix (substring str 0 (length prefix)))))

(defun ee-remove-prefix (prefix str)
  "Example: (ee-remove-prefix \"ab\" \"abcde\") --> \"cde\""
  (substring str (length prefix)))

(defun ee-replace-prefix0 (prefix newprefix fname)
  (if (ee-prefixp prefix fname)
      (concat newprefix (ee-remove-prefix prefix fname))))

(defun ee-replace-prefix (prefix newprefix fname)
  (ee-replace-prefix0 (ee-expand prefix) newprefix (ee-expand fname)))

(defun ee-intern (fmt &rest args)
  (intern (apply 'format fmt args)))

(defun ee-code-c-d-pairs-eval (fname code)
  "For each entry (C D) in `ee-code-c-d-pairs' for which D is a prefix of FNAME,
evaluate f in the context of a big `let', and return the result."
  (let ((efname (ee-expand fname)))
    (ee-filter (lambda (c-d)
		 (let* ((c (car c-d))
			(d (cadr c-d))
			(ed (ee-expand d)))
		   (if (ee-prefixp ed efname)
		       (let ((fname- (ee-remove-prefix ed efname)))
			 (eval code)))))
	       ee-code-c-d-pairs)))

;; «ee-find-xxxfile-sexps»  (to ".ee-find-xxxfile-sexps")
(defun ee-find-xxxfile-sexps (fname)
  (ee-code-c-d-pairs-eval
   fname
   '(list (ee-intern "find-%sfile" c) fname-)))



;;;   __ _           _        __ _ _            _ _       _        
;;;  / _(_)_ __   __| |      / _(_) | ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| |_| | |/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  _| | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|_|\___|     |_|_|_| |_|_|\_\___/
;;;                                                                
;; «find-file-links» (to ".find-file-links")
;; (find-find-links-links "f" "file" "fname")
;; A test: (find-file-links "~/tmp/foo")
(define-key eev-mode-map "\M-hf" 'find-file-links)

(defun ee-if-prefixp (d newd fname code)
  "An internal function used by `find-file-links'."
  (let* ((ed     (ee-expand d))
	 (efname (ee-expand fname)))
    (if (ee-prefixp ed efname)
	(let* ((fname- (ee-remove-prefix ed efname))
	       (fname+ (concat newd fname-)))
	  (eval code)))))

(defun ee-find-file-links (&optional fname)
  (setq fname (or fname (or (buffer-file-name) default-directory)))
  `(,(ee-if-prefixp "~/" "~/" fname '`(find-fline ,fname+))
    ,(ee-if-prefixp "$S/http/"   "http://" fname '(ee-H fname+))
    ,(ee-if-prefixp "$S/shttp/" "shttp://" fname '(ee-H fname+))
    ""
    (find-file ,fname)		; non-refinable
    (find-fline ,fname)		; refinable
    ,@(ee-find-xxxfile-sexps (ee-expand fname))
    ;;
    ,@(ee-find-file-extra-links fname) ; customizable by the user
    ))

(defun ee-find-file-extra-links (fname) ()) ; customize this

(defun find-file-links (fname &rest pos-spec-list)
  (interactive (list (or (buffer-file-name) default-directory)))
  (apply 'find-elinks
	 `((find-file-links ,fname ,@pos-spec-list)
	   ,@(ee-find-file-links fname)
	   )
	 pos-spec-list))



;;;   __ _           _                                  _ _       _        
;;;  / _(_)_ __   __| |       __ _ _ __ ___ _ __       | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____ / _` | '__/ _ \ '_ \ _____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____| (_| | | |  __/ |_) |_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|      \__, |_|  \___| .__/      |_|_|_| |_|_|\_\___/
;;;                          |___/         |_|                             
;;
;; «find-grep-links» (to ".find-grep-links")
;; (find-find-links-links "\\M-g" "grep" "")
;; Tests:
;;   (ee-find-grep-commands)
;;   (ee-find-grep-functions "~/eev-current/")
;;   (ee-find-grep-links '(find-agrep find-bgrep) '("grep a *" "grep b *"))
;;   (find-grep-links)
;;
(define-key eev-mode-map "\M-h\M-g" 'find-grep-links)

(defun ee-first-n-elements (n list)
  "Example: (ee-first-n-elements 2 '(a b c d e f))   ==> (a b)"
  (if (and (> n 0) list)
      (cons (car list)
	    (ee-first-n-elements (1- n) (cdr list)))))

(defun ee-find-grep-functions (dir)
  "An internal function used by `find-grep-links'."
  (ee-code-c-d-pairs-eval dir '(ee-intern "find-%sgrep" c)))

(defun ee-find-grep-commands ()
  "An internal function used by `find-grep-links'."
  (cons "grep -nH -e _ *" (ee-first-n-elements 4 grep-history)))

(defun ee-find-grep-links0 (find-xxxgreps grep-commands)
  "An internal function used by `find-grep-links'."
  (let (result)
    (dolist (head find-xxxgreps)
      (dolist (command grep-commands)
	(setq result (cons `(,head ,command) result))))
    (nreverse result)))

(defun ee-find-grep-links ()
  (ee-find-grep-links0
   (ee-find-grep-functions default-directory)
   (ee-find-grep-commands)))

(defun find-grep-links (&rest pos-spec-list)
"Visit a temporary buffer containing `find-xxxgrep' sexps."
  (interactive)
  (apply 'find-elinks
   `((find-grep-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-grep-links)
     ""
     ,@(ee-find-grep-links)
     )
   pos-spec-list))




;;;   __ _           _                                      
;;;  / _(_)_ __   __| |      _ __ ___   __ _  ___ _ __ ___  
;;; | |_| | '_ \ / _` |_____| '_ ` _ \ / _` |/ __| '__/ _ \ 
;;; |  _| | | | | (_| |_____| | | | | | (_| | (__| | | (_) |
;;; |_| |_|_| |_|\__,_|     |_| |_| |_|\__,_|\___|_|  \___/ 
;;;                                                         
;; «find-ekbmacro-links» (to ".find-ekbmacro-links")
;; (find-find-links-links "M" "macro" "")
;; (find-efunction 'find-ekbmacro-links)

(define-key eev-mode-map "\M-hM" 'find-ekbmacro-links)

(defun find-ekbmacro-links () (interactive)
  (find-elinks `(
    (find-ekbmacro-links)
    ""
    (format-kbd-macro last-kbd-macro)
    (setq last-kbd-macro (kbd ,(format-kbd-macro last-kbd-macro)))
    ""
    (find-enode "Keyboard Macros")
    (find-enode "Edit Keyboard Macro")
    (eek "M-h M-k C-x C-k C-e  ;; kmacro-edit-macro-repeat")
    (eek "        C-x C-k C-e  ;; kmacro-edit-macro-repeat")
    (eek "M-h M-k C-x C-k l    ;; kmacro-edit-lossage")
    (eek "        C-x C-k l    ;; kmacro-edit-lossage")
    )))



;;;            _  __ _ _ _                                     
;;;  _ __   __| |/ _| (_) | _____       _ __   __ _  __ _  ___ 
;;; | '_ \ / _` | |_| | | |/ / _ \_____| '_ \ / _` |/ _` |/ _ \
;;; | |_) | (_| |  _| | |   <  __/_____| |_) | (_| | (_| |  __/
;;; | .__/ \__,_|_| |_|_|_|\_\___|     | .__/ \__,_|\__, |\___|
;;; |_|                                |_|          |___/      

;; «find-pdflike-page-links» (to ".find-pdflike-page-links")
;; (find-efunction 'count-lines)
;;
(defun ee-count-formfeeds (start end)
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (save-match-data
	(let ((done 0))
	  (while (re-search-forward "[\f]" nil t 1)
	    (setq done (+ 1 done)))
	  done)))))

(defun ee-current-page ()
  (+ 1 (ee-count-formfeeds (point-min) (point))))

(defun ee-last-kill ()
  (if (stringp (car kill-ring))
    (ee-no-properties (car kill-ring))))

(defun ee-region ()
  (if (region-active-p)
      (buffer-substring-no-properties (point) (mark))))

(defun ee-region-or-last-kill ()
  (or (ee-region) (ee-last-kill)))

;; (find-find-links-links "\\M-p" "pdflike-page" "page bufname offset")

(define-key eev-mode-map "\M-h\M-p" 'find-pdflike-page-links)

(defun ee-pdflike-page-links (&optional page bufname offset)
  (setq page    (or page (ee-current-page)))
  (setq bufname (or bufname (buffer-name)))
  (setq offset  (or offset ee-page-offset))
  (let* ((c          ee-page-c)
	 (fname      ee-page-fname)
	 (find-cpage (ee-intern "find-%spage" c))
	 (find-ctext (ee-intern "find-%stext" c))
	 (kill       (or (ee-region-or-last-kill) ""))
	 (page-      (- page offset))
	 )
    `((,find-cpage ,page)
      (,find-ctext ,page)
      (,find-cpage (+ ,offset ,page-))
      (,find-ctext (+ ,offset ,page-))
      ""
      (,find-cpage ,page ,kill)
      (,find-ctext ,page ,kill)
      (,find-cpage (+ ,offset ,page-) ,kill)
      (,find-ctext (+ ,offset ,page-) ,kill)
      ""
      (code-pdf ,c ,fname)
      (code-pdf-text ,c ,fname ,offset)
      ,(ee-HS bufname)
      )))

(defun find-pdflike-page-links (&optional page bufname offset &rest rest)
"Visit a temporary buffer containing hyperlinks to a pdf-like document.
See: (find-pdf-like-intro)
     (find-pdf-like-intro \"refining hyperlinks to pages\")"
  (interactive)
  (setq page    (or page (ee-current-page)))
  (setq bufname (or bufname (buffer-name)))
  (setq offset  (or offset ee-page-offset))
  (let* ((c          ee-page-c)
	 (fname      ee-page-fname)
	 (find-cpage (ee-intern "find-%spage" c))
	 (find-ctext (ee-intern "find-%stext" c))
	 (kill       (or (ee-last-kill) ""))
	 (page-      (- page offset))
	 )
    ;;
    '(apply 'find-elinks `(
      (find-pdflike-page-links ,page ,bufname ,offset ,@rest)
      (find-efunction 'find-pdflike-page-links)
      ""
      (,find-cpage ,page)
      (,find-ctext ,page)
      (,find-cpage (+ ,offset ,page-))
      (,find-ctext (+ ,offset ,page-))
      ""
      (,find-cpage ,page ,kill)
      (,find-ctext ,page ,kill)
      (,find-cpage (+ ,offset ,page-) ,kill)
      (,find-ctext (+ ,offset ,page-) ,kill)
      ""
      (code-pdf ,c ,fname)
      (code-pdf-text ,c ,fname ,offset)
      ,(ee-HS bufname)
    ) rest)
    ;;
    (apply 'find-elinks `(
      (find-pdflike-page-links ,page ,bufname ,offset ,@rest)
      (find-efunction 'find-pdflike-page-links)
      ""
      ,@(ee-pdflike-page-links page bufname offset)
      ) rest)
    ))

;; (find-pdflike-page-links)
;; (find-angg ".emacs.papers" "kopkadaly")
;; (code-pdftotext "kopkadaly4" "~/books/__comp/kopka_daly__a_guide_to_latex_4th_ed.pdf" 12)
;; (find-code-pdftotext "kopkadaly4" "~/books/__comp/kopka_daly__a_guide_to_latex_4th_ed.pdf" 12)
;; (ee-page-parameters "kopkadaly4" 12)
;; (find-kopkadaly4page (+ 12 287) "13.1   The picture environment")
;; (find-kopkadaly4text            "13.1   The picture environment")
;; (find-kopkadaly4text)






;;;  _                           _ _       _                         __ _      
;;; | |__  _   _ _ __   ___ _ __| (_)_ __ | | __     _ __  _ __ ___ / _(_)_  __
;;; | '_ \| | | | '_ \ / _ \ '__| | | '_ \| |/ /____| '_ \| '__/ _ \ |_| \ \/ /
;;; | | | | |_| | |_) |  __/ |  | | | | | |   <_____| |_) | | |  __/  _| |>  < 
;;; |_| |_|\__, | .__/ \___|_|  |_|_|_| |_|_|\_\    | .__/|_|  \___|_| |_/_/\_\
;;;        |___/|_|                                 |_|                        
;;
;; «ee-hyperlink-prefix» (to ".ee-hyperlink-prefix")

(defun ee-hyperlink-prefix ()
  "A lispish interface for customizing the variable `ee-hyperlink-prefix'.
See the comments in the source code."
  (interactive)
  (find-elinks
   `((ee-hyperlink-prefix)
     ;; Convention: the first sexp always regenerates the buffer.
     (setq ee-hyperlink-prefix ,ee-hyperlink-prefix) ; current value
     ""
     (setq ee-hyperlink-prefix "# ")	; other common values
     (setq ee-hyperlink-prefix ";; ")
     (setq ee-hyperlink-prefix "-- ")
     (setq ee-hyperlink-prefix "// ")
     (setq ee-hyperlink-prefix "% ")
     )))




;;;   __ _           _             __                      _ _       _        
;;;  / _(_)_ __   __| |       ___ / _| __ _  ___ ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____ / _ \ |_ / _` |/ __/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  __/  _| (_| | (_|  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|      \___|_|  \__,_|\___\___|     |_|_|_| |_|_|\_\___/
;;;                                                                           
;; See: (find-links-intro)
;;      (find-templates-intro)

;; «find-eface-links» (to ".find-eface-links")
;; (find-find-links-links "\\M-s" "eface" "face-symbol")
;; A test: (find-eface-links 'bold)
(define-key eev-mode-map "\M-h\M-s" 'find-eface-links)

(defun find-eface-links (face-symbol &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks about FACE-SYMBOL."
  (interactive (list (or (face-at-point) 'default)))
  ;; (setq face-symbol (or face-symbol "{face-symbol}"))
  ;; (setq face-symbol (or face-symbol (face-at-point)))
  (apply 'find-elinks
   `((find-eface-links ',face-symbol ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-eface-links)
     ""
     (find-efacedescr ',face-symbol)
     (find-efaces ,(format "\n%S " face-symbol))
     (find-eface ',face-symbol)
     (customize-face ',face-symbol)
     (set-face-foreground ',face-symbol ,(face-foreground face-symbol))
     (set-face-background ',face-symbol ,(face-background face-symbol))
     (face-id ',face-symbol)
     (find-epp (mapcar (lambda (face) (cons (face-id face) face)) (face-list)))
     (find-ecolors)
     (find-efaces)
     (find-efaces ,(symbol-name face-symbol))
     )
   pos-spec-list))

;; Test: (find-eface-links 'eepitch-star-face)
;; (find-eevfile "eev.el" "\\M-h\\M-s")




;;;   __ _           _                     _                _ _       _        
;;;  / _(_)_ __   __| |      ___  ___ ___ | | ___  _ __    | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |____ / _ \/ __/ _ \| |/ _ \| '__|___| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |____|  __/ (_| (_) | | (_) | | |____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     \___|\___\___/|_|\___/|_|      |_|_|_| |_|_|\_\___/
;;;                                                                              
;; «find-color-links» (to ".find-color-links")
;; (find-find-links-links "c" "color" "initialcolor")
;; Tests:
;;   (find-ecolor-links)
;;   (find-ecolor-links "sienna")
;;
(define-key eev-mode-map "\M-hc" 'find-ecolor-links)

(defun find-ecolor-links (&optional initialcolor &rest pos-spec-list)
  "Visit a temporary buffer containing hyperlinks for the color INITIALCOLOR."
  (interactive)
  (setq initialcolor (or initialcolor "#123456"))
  (apply 'find-elinks
   `((find-ecolor-links ,initialcolor ,@pos-spec-list)
     ""
     (find-ecolor-links (ee-color-choose-tk ,(or initialcolor "gray")))
     (find-ecolor-links ,(or initialcolor "gray"))
     (find-ecolors)
     (find-ecolors ,initialcolor)
     ,`(insert (propertize " Sample " 'face '(:background ,initialcolor)))
     ,`(ee-color-values ,initialcolor)
     (kill-new ,initialcolor)
     (kill-new ,(ee-color-values initialcolor))
     (find-efunction 'find-ecolor-links)
     )
   pos-spec-list))

(defun ee-color-values (color)
  "Return the #RRGGBB representation for COLOR."
  (apply 'format "#%02x%02x%02x"
	 (mapcar (lambda (c) (lsh c -8)) (color-values color))))

(defun ee-color-choose-tk (&optional initialcolor)
  "Call Tcl/Tk to choose a color similar to INITIALCOLOR.
This needs a temporary directory; see: (find-prepared-intro)"
  (eetcl (format "puts [tk_chooseColor -initialcolor %s]; exit"
		 (or initialcolor "gray")))
  (find-sh0 (format "wish %s" ee-file-tcl)))




;;;   __ _           _       _                         _ _       _        
;;;  / _(_)_ __   __| |     | |__   ___ _ __ ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _ \ '__/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____| | | |  __/ | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|\___|_|  \___|     |_|_|_| |_|_|\_\___/
;;;                                                                       
;; «find-here-links» (to ".find-here-links")
;; See: (find-eev-quick-intro "`M-h M-h'")

;; (find-efunction 'find-grep-links)
;; (find-efunction 'find-einfo-links)
;; (find-efunction 'find-file-links)
;; (find-find-links-links "\\M-h" "here" "")
;; (find-efunction 'find-ecolors)

;; Moved the key binding to:
;;   (find-eevfile "eev-mode.el" "\\M-h\\M-h")
;; (define-key eev-mode-map "\M-h\M-h" 'find-here-links)

;; Some tools for detecting which kind of buffer we're in.
(defun ee-buffer-re (re)
  (if (string-match re (buffer-name))
      (match-string 1 (buffer-name))))
(defun ee-buffer-eq (str) (string= str (buffer-name)))

(defun ee-buffer-help0    () (ee-buffer-eq "*Help*"))
(defun ee-buffer-help-re0 (re n)
  (if (ee-buffer-help0)
      (save-excursion
	(goto-char (point-min))
	(if (looking-at re) (match-string n)))))

(defun ee-buffer-help (re n) (intern (or (ee-buffer-help-re0 re n) "nil")))

;; By major mode
(defun ee-grep-bufferp      () (eq major-mode 'grep-mode))
(defun ee-man-bufferp       () (eq major-mode 'Man-mode))
(defun ee-rcirc-bufferp     () (eq major-mode 'rcirc-mode))
(defun ee-info-bufferp      () (eq major-mode 'Info-mode))
(defun ee-dired-bufferp     () (eq major-mode 'dired-mode))
(defun ee-wdired-bufferp    () (eq major-mode 'wdired-mode))
(defun ee-w3m-bufferp       () (eq major-mode 'w3m-mode))
(defun ee-custom-bufferp    () (eq major-mode 'Custom-mode))
(defun ee-epackages-bufferp () (eq major-mode 'package-menu-mode))

;; By buffer name
(defun ee-intro-bufferp    () (ee-buffer-re "^\\*(find-\\(.*\\)-intro)\\*$"))
(defun ee-freenode-bufferp () (ee-buffer-re "^\\(.*\\).freenode\\.net"))
(defun ee-ecolors-bufferp  () (ee-buffer-eq "*Colors*"))
(defun ee-efaces-bufferp   () (ee-buffer-eq "*Faces*"))
(defun ee-pdftext-bufferp  () (ee-buffer-re "^pdftotext"))

;; By buffer name (when it is "*Help*")
(defvar ee-efunctiondescr-re "^\\([^ \t\n]+\\) is a[^\t\n]*function")
(defun  ee-efunctiondescr-bufferp () (ee-buffer-help ee-efunctiondescr-re 1))
(defun  ee-find-efunctiondescr-links ()
  (let ((f (ee-efunctiondescr-bufferp)))
    `((find-efunction-links ',f)
      (find-efunctiondescr ',f))))

(defvar ee-evardescr-re "^\\([^ \t\n]+\\) is a variable")
(defun  ee-evardescr-bufferp () (ee-buffer-help ee-evardescr-re 1))
(defun  ee-find-evardescr-links ()
  (let ((v (ee-evardescr-bufferp)))
    `((find-evariable-links ',v)
      (find-evardescr ',v))))

(defvar ee-efacedescr-re "^Face: \\([^ \t\n]+\\)")
(defun  ee-efacedescr-bufferp () (ee-buffer-help ee-efacedescr-re 1))
(defun  ee-find-efacedescr-links ()
  (let ((f (ee-efacedescr-bufferp)))
    `((find-eface-links ',f)
      (find-efacedescr ',f))))

(defvar ee-epackage-re "^\\([^ \t\n]+\\) is a[ -~]+ package")
(defun  ee-epackage-bufferp () (ee-buffer-help ee-epackage-re 1))
(defun  ee-find-epackage-links ()
  (let ((p (ee-epackage-bufferp)))
    `((find-epackages ,(format "\n  %s " p) t)
      (find-epackage ',p))))

;; By buffer name (when the mode is man)
(defvar ee-man-re "^\\*Man \\(.*\\)\\*$")
(defun  ee-find-man-links () 
  (let ((mp (ee-buffer-re ee-man-re)))
    `((find-man ,mp))))

(defvar ee-custom-re "^\\*Customize Group: \\(.*\\)\\*$")
(defun  ee-find-custom-links () 
  (let* ((name   (ee-buffer-re ee-custom-re))
	 (symbol (intern (downcase (replace-regexp-in-string " " "-" name)))))
    `((find-customizegroup ',symbol))))

;; Other cases
(defun ee-file-bufferp     () buffer-file-name)



(defun ee-find-efaces-links    () `((find-efaces)))
(defun ee-find-ecolors-links   () `((find-ecolors)))
(defun ee-find-epackages-links () `((find-epackages)))
(defun ee-find-pdftext-links   () (ee-pdflike-page-links))

;; to to:
;; ee-find-w3m-links
;; ee-find-ecolor-links
;; 

(defun ee-find-here-links ()
  (cond ;; by major mode
	((ee-info-bufferp)      (cons "" (ee-find-info-links)))      ; M-h M-i
	((ee-man-bufferp)       (cons "" (ee-find-man-links)))       ; ?
	((ee-grep-bufferp)      (cons "" (ee-find-grep-links)))	     ; M-h M-g
	((ee-w3m-bufferp)       (cons "" (ee-find-w3m-links)))	     ; M-h M-w
	((ee-dired-bufferp)     (cons "" (ee-find-file-links)))	     ; M-h f
	((ee-wdired-bufferp)    (cons "" (ee-find-file-links)))	     ; M-h f
	((ee-custom-bufferp)    (cons "" (ee-find-custom-links)))    ; ?
	((ee-epackages-bufferp) (cons "" (ee-find-epackages-links))) ; ?
	;; by buffer name
	((ee-intro-bufferp)     (cons "" (ee-find-intro-links)))     ; M-h M-i
	((ee-freenode-bufferp)  (cons "" (ee-find-freenode-links)))  ; ?
	((ee-ecolors-bufferp)   (cons "" (ee-find-ecolors-links)))   ; ?
	((ee-efaces-bufferp)    (cons "" (ee-find-efaces-links)))    ; ?
	((ee-pdftext-bufferp)   (cons "" (ee-find-pdftext-links)))   ; ?
	;; by buffer name, when it is "*Help*"
	((ee-efunctiondescr-bufferp) (cons "" (ee-find-efunctiondescr-links)))
	((ee-efacedescr-bufferp)     (cons "" (ee-find-efacedescr-links)))
	((ee-evardescr-bufferp)      (cons "" (ee-find-evardescr-links)))
	((ee-epackage-bufferp)       (cons "" (ee-find-epackage-links)))
	;; other cases
	((ee-file-bufferp)      (cons "" (ee-find-file-links)))	   ; M-h f
	(t (list "" "Not implemented!" "See:"
		 '(find-efunction 'ee-find-here-links)))
	))

(defun find-here-links-test (sexp)
"See: (find-links-intro \"`find-here-links'\")"
  (find-wset "13o_2o_o" sexp '(find-here-links)))

;; (find-man "1 cat")
;; (progn (find-man "1 cat") (buffer-name))
;; (find-eevfile "eev-rcirc.el")

(defun ee-find-here-links0 ()
  `(,(ee-H "See: ")
    (find-links-intro "`find-here-links'")
    (find-efunctiondescr 'eev-mode "M-h M-h")
    ))

;; (find-find-links-links "\\M-h" "here" "")
;;
(defun find-here-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks pointing to here."
  (interactive)
  (apply 'find-elinks
   `(;; The first line of a find-here-links buffer DOES NOT
     ;; regenerates the buffer - instead the first lines point to
     ;; help pages.
     ,@(ee-find-here-links0)
     ,@(ee-find-here-links)
     )
   pos-spec-list))

;; Test: (find-here-links)
;; (progn (find-enode "Screen") (find-here-links))







;; ------------------------------------------------------------
;; Old stuff:

;; The rest of this block of comments was cut & pasted straight from
;; eev-insert.el, but most of what they say still hold...
;;
;; This is the ugliest part of eev's code. It's being rewritten. Even
;; if work on it may seem stalled, it _is_ being rewritten. In some
;; sense.
;;
;; I got tired of writing all my hyperlinks by hand, so I created
;; these functions. The "new way of creating hyperlinks" (the first
;; block of this file) adds the following key bindings to
;; eev-mode-map:
;;
;;   M-h M-k  find-ekey-links
;;   M-h M-f  find-efunction-links
;;   M-h M-v  find-evariable-links
;;   M-h M-i  find-einfo-links
;;   M-h M-d  find-debpkg-links
;;   M-h f    find-file-links
;;   M-h m    find-last-manpage-links
;;   M-h M-m  find-manpage-links
;;
;; All of them work similarly. For example: type M-h M-k RET, and
;; `find-ekey-links' will create and display a buffer called "*Elisp
;; hyperlinks*", like this:
;;
;;    _____________________________________________________________ 
;;   |(find-ekey-links "\r")                                       |
;;   |(find-elongkey-links "RET")                                  |
;;   |(find-elongkey-links "RET  ;; newline")                      |
;;   |"RET  ;; newline"                                            |
;;   |                                                             |
;;   |(where-is 'newline)                                          |
;;   |(describe-function 'newline)                                 |
;;   |(find-efunctiondescr 'newline)                               |
;;   |(find-efunction 'newline)                                    |
;;   |(find-efunctionpp 'newline)                                  |
;;   |(find-efunctiond 'newline)                                   |
;;   |(find-eCfunction 'newline)                                   |
;;   |(find-estring (documentation 'newline))                      |
;;   |(find-estring (documentation 'newline t))                    |
;;   |                                                             |
;;   |(describe-key "\r")                                          |
;;   |(describe-key-briefly "\r")                                  |
;;   |(find-ekeydescr "\r")                                        |
;;   |(Info-goto-emacs-key-command-node "\r")                      |
;;   |(Info-goto-emacs-command-node 'newline)                      |
;;   |(find-enode "Command Index" "* newline:")                    |
;;   |(find-elnode "Index" "* newline:")                           |
;;   |                                                             |
;;   |(key-description "\r")                                       |
;;   |(format-kbd-macro "\r")                                      |
;;   |(format-kbd-macro "\r" t)                                    |
;;   |(key-binding "\r")                                           |
;;   |                                                             |
;;   |                                                             |
;;   |                                                             |
;;   |--:**  *Elisp hyperlinks*   All L28     (Fundamental)--------|
;;   |_____________________________________________________________|
;;
;;
;; That is, a lot of hyperlinks pointing to interesting pieces of
;; information about the key RET and the command (`newline') that is
;; bound to it. Then you may follow these hyperlinks by evaluating the
;; sexps or you may copy them to other files by copying their text.
;;
;; [To do: explain M-h M-y. There's an example in `eesteps' format in
;; the NEWS file.]

;; See: <http://angg.twu.net/eev-current/README.html>
;; and: <http://angg.twu.net/eev-current/NEWS.html>

;; The second part of this file contains some older functions that
;; insert Elisp hyperlinks at the current buffer -- like `inn', that
;; inserts a hyperlink to the info node currently being visited -- or
;; transform text -- for example, a series of lines, each one
;; containing the name of a Debian package -- into hyperliks.

'(

(defun ee-buffer-manpage-name (&optional bufname)
  "Return the name of the manpage in the buffer BUFNAME, or nil if none.
The default for BUFNAME is the name of the current buffer.
This function does a simple string matching and converts \"*Man
foo*\" to \"foo\"."
  (if (null bufname)
      (setq bufname (buffer-name)))
  (and bufname
       (string-match "^\\*\\(Wo\\)?Man \\(.*\\)\\*$" bufname)
       (match-string 2 bufname)))
  
(defun find-last-manpage-links (manpagename &rest rest)
  "Visit a temporary buffer containing hyperlinks related to a manpage.
Use this when you are in a manpage buffer and you want links to it."
  (interactive (list (ee-buffer-manpage-name)))
  (apply 'find-elinks
	 (list (ee-pph `(find-man-links ,manpagename))
	       ""
	       (ee-pph `(find-man ,manpagename)))
	 rest))

(defun find-manpage-links (manpagename &rest rest)
  "Visit a temporary buffer containing hyperlinks related to a manpage.
Use this when point is over a manpage name and you want links to that page."
  (interactive (list (ee-manpagename-ask)))
  (apply 'find-elinks
	 (list (ee-pph `(find-man-links ,manpagename))
	       ""
	       (ee-pph `(find-man ,manpagename)))
	 rest))

)

;; Creating temporary buffers with lots of elisp hyperlinks is an idea
;; that I only had relatively recently - in 2004, I think... before
;; that I used some functions that either inserted hyperlinks into the
;; current buffer or modified the text in the current buffer to
;; produce hyperlinks. For example, `M-x inn' inserted a link to an
;; info node, and `M-x dff' converted a line with the name of a debian
;; package into three lines, each one with a hyperlink to something
;; related to that debian package...






;; TODO: move these functions to another file (eev-video.el?)
;; (find-angg ".emacs" "mm:ss")
;; (find-angg ".emacs" "find-mplayer")
;; (find-angg ".emacs" "code-mplayer")

;; (find-man "1 mplayer" "  -ss ")
;; (find-man "1 mplayer" "  -fs ")
;; (find-man "1 mplayer" "  -osdlevel ")



;; Tests:
;; (find-upload-links "eev-current/eev-template.el")
;; (find-download-links "" "" "eev-current/eev-template.el")
;; (eevt-down "eev-current/" "emacs/eev/" "eev-template.el")
;






(provide 'eev-elinks)





;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
