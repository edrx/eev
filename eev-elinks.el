;;; eev-elinks.el --- `find-efunction-links' and other `find-e*-links'

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
;; Version:    2020jul22
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
;;   (find-links-conv-intro "3. Classification")
;;   (find-links-conv-intro "3. Classification" "`find-elinks'")
;;   (find-links-conv-intro "3. Classification" "`find-elinks'+")
;;
;; In this file we define `find-elinks' and several functions based on
;; it that generate relatively simple elisp hyperlinks buffers -
;; buffers which are mostly composed of elisp hyperlinks. They follow
;; these conventions:
;;
;;   (find-links-intro "3. Elisp hyperlinks buffers conventions")
;;
;; The "more complex" `find-elinks'-based functions are the ones which
;; use `ee-template0'. They are defined here:
;;
;;   (find-eevfile "eev-tlinks.el")
;;
;; Here's one example of each function in this file:
;; [to be done]




;; «.around-point»		(to "around-point")
;; «.find-elinks»		(to "find-elinks")
;; «.find-efunction-links»	(to "find-efunction-links")
;; «.find-evariable-links»	(to "find-evariable-links")
;; «.find-ekey-links»		(to "find-ekey-links")
;; «.find-elongkey-links»	(to "find-elongkey-links")
;; «.find-einfo-links»		(to "find-einfo-links")
;; «.find-eintro»		(to "find-eintro")

;; «.ee-code-c-d-filter»	(to "ee-code-c-d-filter")
;; «.ee-find-xxxfile-sexps»	(to "ee-find-xxxfile-sexps")
;; «.find-file-links»		(to "find-file-links")
;; «.find-grep-links»		(to "find-grep-links")
;; «.find-ekbmacro-links»	(to "find-ekbmacro-links")
;; «.find-pdflike-page-links»	(to "find-pdflike-page-links")
;; «.ee-hyperlink-prefix»	(to "ee-hyperlink-prefix")
;; «.find-eface-links»		(to "find-eface-links")
;; «.find-color-links»		(to "find-color-links")
;; «.find-epackage-links»	(to "find-epackage-links")
;; «.ee-package-dir»		(to "ee-package-dir")

;; «.find-code-pdf-links»	(to "find-code-pdf-links")
;; «.find-pdf-links»		(to "find-pdf-links")
;; «.find-code-audiovideo-links»  (to "find-code-audiovideo-links")



;; The functions in these sections were moved to:
;;   (find-eev "eev-hlinks.el")
;;
;; «.find-here-links»		(to "find-here-links")
;; «.find-here-links-beginner»	(to "find-here-links-beginner")
;; «.find-here-links-3»		(to "find-here-links-3")
;; «find-here-links»		(to ".find-here-links")
;; «find-here-links-beginner»	(to ".find-here-links-beginner")
;; «find-here-links-3»		(to ".find-here-links-3")








;;;                                  _                   _       _   
;;;   __ _ _ __ ___  _   _ _ __   __| |      _ __   ___ (_)_ __ | |_ 
;;;  / _` | '__/ _ \| | | | '_ \ / _` |_____| '_ \ / _ \| | '_ \| __|
;;; | (_| | | | (_) | |_| | | | | (_| |_____| |_) | (_) | | | | | |_ 
;;;  \__,_|_|  \___/ \__,_|_| |_|\__,_|     | .__/ \___/|_|_| |_|\__|
;;;                                         |_|                      
;;
;; «around-point» (to ".around-point")
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
;; `find-elinks' is an internal function used by all functions that
;; generate buffers with hyperlinks, like `find-efunction-links', and
;; by most functions that generate buffers with hyperlinks followed by
;; a templated string, like `find-latex-links'. It is a variant of
;; `find-estring', in the sense that
;;
;;    (find-elinks LIST POS-SPEC)
;;
;; is similar to
;;
;;    (find-estring (ee-links-to-string LIST) pos-spec).
;;
;; Here is how the conversion from LIST to a string works. Try:
;;
;;    (find-elinks '((a sexp) "a string"))
;;
;; In simple examples like the one above each element in LIST becomes
;; a line in the output string: sexps are prefixed by
;; `ee-hyperlink-prefix', and each string becomes a line. Try:
;;
;;    (find-elinks '((a sexp) (another) "" (sexp) "a string" "another"))
;;
;; The "" becomes an empty line. But things can be more complex: here,
;;
;;    (find-elinks '((str "foo\nbar") nil "foo\nbar"))
;;
;; the `(str "foo\nbar")' becomes a single line, with the `\n'
;; displayed as "\n"; the nil is dropped from the list by
;; `ee-remove-nils' and does not become a line, and the string
;; "foo\nbar" becomes two lines.

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
;; `find-efunction-links' is one of the simplest functions that
;; generate buffers with hyperlinks. It is mentioned in
;; `find-eev-quick-intro':
;; 
;;   (find-eev-quick-intro "4. Creating Elisp Hyperlinks")
;;   (find-eev-quick-intro "(find-efunction-links 'find-file)")
;;
;; Try: (find-efunction-links 'find-file)
;;               (eek "M-h M-f next-line")

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-h\M-f" 'find-efunction-links)

(defun find-efunction-links (&optional f &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks related to the function F."
  (interactive (find-function-read))
  (apply 'find-elinks
   `((find-efunction-links ',f ,@pos-spec-list)
     (eek ,(format "M-h M-f  %s" f))
     (find-eev-quick-intro "4.2. `find-ekey-links' and friends")
     ""
     ,@(ee-find-efunction-links f)
     )
   pos-spec-list))

(defun ee-find-efunction-links (f)
  "Return a list of hyperlinks for F (a function symbol).
This is an internal function used by `find-efunction-links' and
`find-ekey-links'."
  `((find-efunctiondescr ',f)
    (find-efunction ',f)
    (find-efunctionpp ',f)
    (find-efunctiond ',f)
    ""
    ,@(if (commandp f)
	  `((Info-goto-emacs-command-node ',f)
	    (find-enode "Command Index" ,(format "* %S:" f))
	    ))
    (find-elnode "Index" ,(format "* %S:" f))
    ""
    (where-is ',f)
    (symbol-file ',f 'defun)
    (find-fline (symbol-file ',f 'defun))
    (find-epp (assoc (symbol-file ',f 'defun) load-history))
    (find-estring (mapconcat 'identity (mapcar 'car load-history) "\n"))
    (find-estring (documentation ',f))
    (find-estring (documentation ',f t))
    (describe-function ',f)
    ;; (find-eCfunction ',f)		; obsolete
    ))



;;;   __ _           _                            _       _     _      
;;;  / _(_)_ __   __| |       _____   ____ _ _ __(_) __ _| |__ | | ___ 
;;; | |_| | '_ \ / _` |_____ / _ \ \ / / _` | '__| |/ _` | '_ \| |/ _ \
;;; |  _| | | | | (_| |_____|  __/\ V / (_| | |  | | (_| | |_) | |  __/
;;; |_| |_|_| |_|\__,_|      \___| \_/ \__,_|_|  |_|\__,_|_.__/|_|\___|
;;;                                                                    
;; «find-evariable-links» (to ".find-evariable-links")
;; Skel: (find-find-links-links-old "\\M-v" "evariable" "var")
;; A test: (find-evariable-links 'line-move-visual)
;;                 (eek "M-h M-v  line-move-visual")

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-h\M-v" 'find-evariable-links)

;; Test: (find-evariable-links 'line-move-visual)
(defun find-evariable-links (var &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks about a variable."
  (interactive (find-function-read 'variable))
  (apply 'find-elinks
   `((find-evariable-links ',var ,@pos-spec-list)
     (eek ,(format "M-h M-v  %s" var))
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
;; Skel: (find-find-links-links-old "\\M-k" "ekey" "key")
;;
;; The functions in this section generate buffers with hyperlinks
;; about a key sequence. Like this,
;;
;;   (find-eev-quick-intro "4. Creating Elisp Hyperlinks")
;;   (find-eev-quick-intro "(find-efunction-links 'find-file)")
;;
;; but for a key sequence instead of for a function. Try:
;;
;;    (eek "M-h M-k C-x 4 0")
;;    (find-elongkey-links "C-x 4 0")
;;    (find-ekey-links      "\C-x40")
;;    (find-ekey-links [?\C-x ?4 ?0])
;;    (read-key-sequence        "Key seq:")
;;    (read-key-sequence-vector "Key seq:")
;;
;; The high-level functions here are `find-ekey-links' and
;; `find-elongkey-links'. In the terminology of this section, a "key"
;; means a key sequence as a vector or a string, as described here,
;;
;;   (find-elnode "Key Sequence Input")
;;   (find-elnode "Strings of Events")
;;
;; and by "long key" we mean a key sequence in "keyboard macro" form,
;; with or without the ";; <comment>"s. This format is described here:
;;
;;   (find-efunctiondescr 'edmacro-mode "Format of keyboard macros")
;;
;; Here are some conversion functions:
;;
;;    (format-kbd-macro     "\C-x40")
;;    (format-kbd-macro     "\C-x40" t)
;;    (ee-format-kbd-macro  "\C-x40")
;;    (read-kbd-macro      "C-x 4 0")
;;
;; Note that the last hyperlinks in these buffers are conversions:
;;
;;    (find-ekey-links      "\C-x40" 2 "format-kbd-macro")
;;    (find-elongkey-links "C-x 4 0" 2 "format-kbd-macro")
;;
;; Only `find-ekey-links' is bound to a key sequence (`M-h M-k'),
;; because `interactive' can return a key but not a long key:
;;
;;   (find-efunctiondescr 'interactive "Key sequence")
;;
;; A good way to save to our notes hyperlinks about a key sequence is
;; by using the lines 4 and 5 in a `M-h M-k' buffer. For example, in
;; the case of `M-c', they are:
;;
;;   (eek "M-h M-k M-c  ;; capitalize-word")
;;   (find-efunctiondescr 'capitalize-word)
;;

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-h\M-k" 'find-ekey-links)

(defun ee-format-kbd-macro (key)
  "Example: (ee-format-kbd-macro [down])  --> \"<down>  ;; next-line\""
  (replace-regexp-in-string "[ \t][ \t]+" "  " (format-kbd-macro key t)))

;; Test: (find-ekey-links "\C-x40")
(defun find-ekey-links (key &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks related to the key sequence KEY.
See: (find-eev \"eev-elinks.el\" \"find-ekey-links\")"
  (interactive "kElisp hyperlinks for key: ")
  (let ((longkey     (format-kbd-macro key))
	(longkey+ (ee-format-kbd-macro key))
	(f                (key-binding key)))
    (apply 'find-elinks
	   `((find-ekey-links ,key ,@pos-spec-list)
	     ;; Convention: the first sexp always regenerates the buffer.
	     (find-elongkey-links      ,longkey)
	     (find-elongkey-links      ,longkey+)
	     (find-efunction-links    ',f)
	     (eek ,(format "M-h M-k  %s" longkey))
	     (eek ,(format "M-h M-k  %s" longkey+))
	     (eek ,(format "M-h M-f  %s" f))
	     ;; (find-efunctiondescr ',f)
	     ""
	     (find-eev-quick-intro "4.2. `find-ekey-links' and friends")
	     ""
	     ,@(ee-find-eboundkey-links key f)
	     )
	   pos-spec-list)))

;; «find-elongkey-links» (to ".find-elongkey-links")
;; Test: (find-elongkey-links "C-x 4 0")
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
	     (find-eev-quick-intro "4.2. `find-ekey-links' and friends")
	     ""
	     ,@(ee-find-eboundkey-links key f)
	     )
	   pos-spec-list)))

;; Test: (find-elinks (ee-find-eboundkey-links "\M-c" 'capitalize-word))
(defun ee-find-eboundkey-links (key f)
  "From KEY and its binding, F, produce a list of hyperlinks.
This is an internal function used by `find-ekey-links' and
`find-elongkey-links'."
  `((find-efunctiondescr ',f)
    (find-ekeydescr ,key)
    (find-efunction ',f)
    (find-efunctionpp ',f)
    (find-efunctiond ',f)
    ""
    (Info-goto-emacs-key-command-node ,key)
    (Info-goto-emacs-command-node ',f)
    (find-enode "Command Index" ,(format "* %S:" f))
    (find-elnode "Index" ,(format "* %S:" f))
    ""
    (describe-key-briefly ,key)
    (find-estring (documentation ',f))
    (find-estring (documentation ',f t))
    (describe-key ,key)
    (describe-function ',f)
    ""
    (where-is ',f)
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
;; Tests: (progn (find-enode "Lisp Eval") (find-einfo-links))
;;        (progn (find-enode "Lisp Eval") (eek "M-h M-i"))

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-h\M-i" 'find-einfo-links)

(defvar ee-info-file "")

(defun ee-infop          () (get-buffer "*info*"))
(defun ee-info-node      () (with-current-buffer "*info*" Info-current-node))
(defun ee-info-book+     () (with-current-buffer "*info*" Info-current-file))
(defun ee-info-book-     () (file-name-nondirectory (ee-info-book+)))
(defun ee-info-fullnode  () (format "(%s)%s" (ee-info-book-) (ee-info-node)))
(defun ee-info-fullnode+ () (format "(%s)%s" (ee-info-book+) (ee-info-node)))

(defun ee-info-file-     () (file-name-nondirectory ee-info-file))
(defun ee-info-shortp    () (string= (ee-info-book-) (ee-info-file-)))
(defun ee-info-shortf    () (ee-intern "find-%snode" ee-info-code))
(defun ee-info-shortlink () (list (ee-info-shortf) (ee-info-node)))

(defun ee-find-info-links ()
  `((info      ,(ee-info-fullnode))
    (find-node ,(ee-info-fullnode))
    ,(if (ee-info-shortp) (ee-info-shortlink))
    ))

(defun find-einfo-links (&optional fullnode &rest rest)
  "Visit a temporary buffer containing hyperlinks to the current info page.
When possible try to produce also a short hyperlink, like the last one in:

  (info \"(bashref)Pipelines\")
  (find-node \"(bashref)Pipelines\")
  (find-bashnode \"Pipelines\")

The short link is generated when the non-directory part of the
current value of the global variable `ee-info-file' - set by the
last call to a function of the form `find-XXXnode' - matches the
value of (ee-info-book-). This only works reliably for info
\"books\" that are in `Info-directory-list'."
  (interactive)
  (setq fullnode (or fullnode (ee-info-fullnode)))
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-einfo-links ,fullnode ,@rest)
    (find-einfo-links ,(ee-info-fullnode+) ,@rest)
    ""
    ,@(ee-find-info-links)
    ) rest))


;; A test: (ee-intro-stem "*(find-foo-intro)*")

;; Old version:

;; (defun find-einfo-links (&optional intro &rest rest)
;;   "Visit a temporary buffer containing hyperlinks to the current info page.
;; When possible, try to produce also a shorter hyperlink, like the last one in:
;;   (info \"(bashref)Pipelines\")
;;   (find-node \"(bashref)Pipelines\")
;;   (find-bashnode \"Pipelines\")
;; The hack for generating the shorter hyperlink uses the global
;; variables `ee-info-code' and `ee-info-file' - see:
;;   (progn
;;    (find-code-c-d \"bash\" \"/usr/share/doc/bash/examples/\" \"bashref\")
;;    (ee-goto-position \"ee-info-code\"))
;; 
;; As an extra hack, if this function is called from a \"*(find-???-intro)*\"
;; buffer, also generate a link to that buffer."
;;   (interactive)
;;   (setq intro (or intro (ee-intro-stem (buffer-name (current-buffer)))))
;;   (apply 'find-elinks `(
;;     ;; Convention: the first sexp always regenerates the buffer.
;;     (find-einfo-links ,intro ,@rest)
;;     ;; Body:
;;     ""
;;     ,@(if (ee-infop)
;; 	  (ee-find-info-links)
;;        '("[No \"*info*\" buffer]"))
;;     ""
;;     ,@(if intro
;; 	 ;; (list (ee-intern "find-%s-intro" intro))
;; 	 (ee-find-intro-links)
;;        ;; else: "[Not invoked from a \"*find-xxx-intro*\" buffer]"
;;        )
;;     ) rest))



;;;   __ _           _            _       _             
;;;  / _(_)_ __   __| |       ___(_)_ __ | |_ _ __ ___  
;;; | |_| | '_ \ / _` |_____ / _ \ | '_ \| __| '__/ _ \ 
;;; |  _| | | | | (_| |_____|  __/ | | | | |_| | | (_) |
;;; |_| |_|_| |_|\__,_|      \___|_|_| |_|\__|_|  \___/ 
;;;                                                     
;; «find-eintro» (to ".find-eintro")

;; Test: (ee-intro-stem "*(find-eev-quick-intro)*")
(defun ee-intro-stem (&optional bufname)
  "Convert a string of the form \"*(find-STEM-intro)*\" to \"STEM\".
When BUFNAME is nil use the name of the current buffer instead.
When BUFNAME is a string that is not of the right form return nil.
This can be used to test if the current buffer is an intro buffer."
  (setq bufname (or bufname (buffer-name)))
  (if (string-match "^\\*(find-\\(.*\\)-intro)\\*$" bufname)
      (match-string 1 bufname)))

;; Test: (find-elinks (ee-find-intro-links "FOO"))
(defun ee-find-intro-links (&optional stem)
  (setq stem (or stem (ee-intro-stem)))
  (let ((find-xxx-intro (ee-intern "find-%s-intro" stem))
	(url (format "http://angg.twu.net/eev-intros/find-%s-intro.html" stem)))
    `(,(ee-H url)
      (,find-xxx-intro)
      )))

;; Test: (find-eintro-links "eev-quick")
(defun find-eintro-links (&optional stem &rest rest)
  "Visit a temporary buffer containing hyperlinks to the current intro.
This only works reliably if either 1) the current buffer has a
name like \"*(find-STEM-intro)*\", or 2) STEM is given explicitly."
  (interactive)
  (setq stem (or stem (ee-intro-stem)))
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-eintro-links ,stem ,@rest)
    ""
    ,@(ee-find-intro-links stem)
    ) rest))


;; Note that eev-mode.el has this:
;; (define-key eev-mode-map "\M-h\M-i" 'find-eintro-or-einfo-links)
(defun find-eintro-or-einfo-links ()
  "Visit a temporary buffer containing hyperlinks to the current intro buffer.
If we're not in an intro buffer, visit a temporary buffer
containing hyperlinks to the current _info node_ instead. This is
a hack to let use use `M-h M-i' for both \"intro\" and \"info\"."
  (interactive)
  (if (ee-intro-stem)
      (find-eintro-links)
    (find-einfo-links)))





;;;                _                          _        __ _ _ _            
;;;   ___ ___   __| | ___        ___       __| |      / _(_) | |_ ___ _ __ 
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |_____| |_| | | __/ _ \ '__|
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |_____|  _| | | ||  __/ |   
;;;  \___\___/ \__,_|\___|      \___|     \__,_|     |_| |_|_|\__\___|_|   
;;;                                                                        
;; «ee-code-c-d-filter» (to ".ee-code-c-d-filter")
;; This is a trick to make `find-file-links' generate short hyperlinks.
;; See: (find-eev-quick-intro "10.1. Generating short hyperlinks to files")
;;
;; Each call to `(code-c-d C D)' generates an entry `(C D)' in the
;; alist `ee-code-c-d-pairs'. Try:
;;
;;   (code-c-d "foo"  "/FOO")
;;   (code-c-d "bar"  "/FOO/BAR/")
;;   (code-c-d "plic" "/FOO/BAR/PLIC/")
;;   (find-epp ee-code-c-d-pairs)
;;
;; You will see that the `ee-code-c-d-pairs' now begins with:
;;
;;   (("plic" "/FOO/BAR/PLIC/")
;;    ("bar" "/FOO/BAR/")
;;    ("foo" "/FOO")
;;    ...
;;    )
;;
;; And if you run
;;
;;   (find-file-links "/FOO/BAR/PLIC/ploc")
;;
;; Then the last links in the `find-file-links' buffer will be:
;;
;;   (find-plicfile          "ploc")
;;   (find-barfile      "PLIC/ploc")
;;   (find-foofile "/BAR/PLIC/ploc")
;;
;; They are generated by `ee-find-xxxfile-sexps'. Try:
;;
;;   (find-epp (ee-find-xxxfile-sexps "/FOO/BAR/PLIC/ploc"))
;;
;; To understand the implementation, try the sexps below. Note that
;; `find-code-c-d-filter-1' and `find-code-c-d-filter-2' are debugging
;; functions.
;;
;;   (find-epp ee-code-c-d-pairs)
;;   (find-code-c-d-filter-1 'c-d)
;;   (find-code-c-d-filter-1 'c)
;;   (find-code-c-d-filter-1 'd)
;;   (find-code-c-d-filter-1 'ed)
;;   (find-code-c-d-filter-2 "/FOO/BAR/PLIC/ploc" '(list c fname-))
;;   (find-code-c-d-filter-2 "/FOO/BAR/PLIC/ploc" '(ee-intern "find-%sfile" c))
;;
;; See: (find-evariable 'ee-code-c-d-pairs)
;;      (find-elnode "Association Lists")

(defun ee-filter (f list)
  "Return the elements of LIST for which (F elt) is true.
Actually return a list of `(F elt)'s."
  (ee-remove-nils (mapcar f list)))

(defun ee-code-c-d-filter-1 (code)
  "Run CODE on each `c-d' of `ee-code-c-d-pairs' and return a list of results.
This is a simpler version of `ee-code-c-d-filter-2', used only for debugging."
  (ee-filter
   (lambda (c-d)
     (let* ((c (car c-d))
	    (d (cadr c-d))
	    (ed (ee-expand d)))
       (eval code)))
   ee-code-c-d-pairs))

(defun ee-code-c-d-filter-2 (fname code)
  "Run CODE on each `c-d' of `ee-code-c-d-pairs' and return a list of results.
Only eval CODE when (ee-expand D) is a prefix of (ee-expand FNAME).
CODE is evaluated inside a `let' that sets the variables `c',
`d', `ed', `efname', and `fname-'. See the source for their meanings."
  (let ((efname (ee-expand fname)))
    (ee-filter
     (lambda (c-d)
       (let* ((c (car c-d))
	      (d (cadr c-d))
	      (ed (ee-expand d)))
	 (if (ee-prefixp ed efname)
	     (let ((fname- (ee-remove-prefix ed efname)))
	       (eval code)))))
     ee-code-c-d-pairs)))

(defun find-code-c-d-filter-1 (code)
  "For debugging. See the comments in the source."
  (find-epp (ee-code-c-d-filter-1 code)))

(defun find-code-c-d-filter-2 (fname code)
  "For debugging. See the comments in the source."
  (find-epp (ee-code-c-d-filter-2 fname code)))

;; ---

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
  "The result of (format FMT ARGS), converted to a symbol"
  (intern (apply 'format fmt args)))

;; «ee-find-xxxfile-sexps»  (to ".ee-find-xxxfile-sexps")
(defun ee-find-xxxfile-sexps (fname)
  "For each (C D) in ee-code-c-d-pairs test if D is a prefix of FNAME;
when this is true remove the prefix D from FNAME, and put the sexp
(find-Cfile \"FNAME-\") in the list of results. Return that list."
  (ee-code-c-d-filter-2
   fname
   '(list (ee-intern "find-%sfile" c) fname-)))





;;;   __ _           _        __ _ _            _ _       _        
;;;  / _(_)_ __   __| |      / _(_) | ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| |_| | |/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  _| | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|_|\___|     |_|_|_| |_|_|\_\___/
;;;                                                                
;; «find-file-links» (to ".find-file-links")
;; Skel: (find-find-links-links-old "f" "file" "fname")
;; A test: (find-file-links "~/tmp/foo")

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-hf" 'find-file-links)

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
    ,(ee-if-prefixp "$S/https/" "https://" fname '(ee-H fname+))
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
;; Skel: (find-find-links-links-old "\\M-g" "grep" "")
;; Tests:
;;   (ee-find-grep-commands)
;;   (ee-find-grep-functions "~/eev-current/")
;;   (ee-find-grep-links '(find-agrep find-bgrep) '("grep a *" "grep b *"))
;;   (find-grep-links)
;;

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-h\M-g" 'find-grep-links)

(defun ee-first-n-elements (n list)
  "Example: (ee-first-n-elements 2 '(a b c d e f))   ==> (a b)"
  (if (and (> n 0) list)
      (cons (car list)
	    (ee-first-n-elements (1- n) (cdr list)))))

(defun ee-find-grep-functions (dir)
  "An internal function used by `find-grep-links'."
  (ee-code-c-d-filter-2 dir '(ee-intern "find-%sgrep" c)))

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
;; Skel: (find-find-links-links-old "M" "macro" "")
;; (find-efunction 'find-ekbmacro-links)

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-hM" 'find-ekbmacro-links)

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

;; Skel: (find-find-links-links-old "\\M-p" "pdflike-page" "page bufname offset")

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-h\M-p" 'find-pdflike-page-links)

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
      (code-pdf-page ,c ,fname)
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
  (apply 'find-elinks `(
    (find-pdflike-page-links ,page ,bufname ,offset ,@rest)
    ;; (find-efunction 'find-pdflike-page-links)
    ;; (find-eev-quick-intro "10.4. Generating short hyperlinks to PDFs")
    ;; (find-eev-quick-intro "11.1. `find-pdf-links'")
    (find-pdf-like-intro "10. Generating a pair with the page number")
    (find-pdf-like-intro "11. How `M-h M-p' guesses everything")
    ""
    ,@(ee-pdflike-page-links page bufname offset)
    ) rest))

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
;; Skel: (find-find-links-links-old "\\M-s" "eface" "face-symbol")
;; A test: (find-eface-links 'bold)

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-h\M-s" 'find-eface-links)

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
;; Skel: (find-find-links-links-old "c" "color" "initialcolor")
;; Tests:
;;   (find-ecolor-links)
;;   (find-ecolor-links "sienna")
;;

;; Moved to eev-mode.el:
;; (define-key eev-mode-map "\M-hc" 'find-ecolor-links)

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



;;;   __ _           _                             _                          _ 
;;;  / _(_)_ __   __| |       ___ _ __   __ _  ___| | ____ _  __ _  ___      | |
;;; | |_| | '_ \ / _` |_____ / _ \ '_ \ / _` |/ __| |/ / _` |/ _` |/ _ \_____| |
;;; |  _| | | | | (_| |_____|  __/ |_) | (_| | (__|   < (_| | (_| |  __/_____| |
;;; |_| |_|_| |_|\__,_|      \___| .__/ \__,_|\___|_|\_\__,_|\__, |\___|     |_|
;;;                              |_|                         |___/              
;;
;; «find-epackage-links»  (to ".find-epackage-links")
;; Skel: (find-find-links-links-new "epackage" "pkg c d" "")
;; Test: (find-epackage-links 'lua-mode)
;;       (find-epackage-links 'tetris)
;;       (find-epackage-links 'foo)
;;
(defun find-epackage-links (&optional pkg c d &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for an Emacs package.
PKG must be a symbol; C and D are arguments for `code-c-d'.
If D is t then try to use `ee-package-dir' to get the directory."
  (interactive (list (symbol-at-point)))
  (setq pkg (or pkg "{pkg}"))
  (setq c (or c (replace-regexp-in-string "[-]" "" (symbol-name pkg))))
  (setq d (cond ((eq d t) (ee-package-dir pkg))
		((eq d nil) "{d}")
		(t d)))
  (apply
   'find-elinks
   `((find-epackage-links ,(ee-add-quote pkg) ,c ,d ,@pos-spec-list)
     (find-epackage-links ,(ee-add-quote pkg) ,c t ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-epackage-links)
     (find-elpafile "")
     ""
     ,(ee-find-epackage-links0 pkg c d)
     )
   pos-spec-list))

(defun ee-find-epackage-links0 (pkg c d)
  "This is an internal function used by `find-epackage-links'."
  (let* ((spkg (format "\n  %s " pkg))
	 (findelpafiles0 (ee-package-findelpafiles pkg))
	 (findelpafiles1 (reverse findelpafiles0))
	 (findelpafiles (mapconcat 'ee-HS findelpafiles1 "\n"))
	 )
    (ee-template0 "\
# (find-epackages {(ee-S spkg)})
# (find-epackage-links '{pkg})
# (find-epackage '{pkg})
{findelpafiles}

# (ee-package-dir '{pkg})
# (find-epp (ee-package-desc '{pkg}))

# (code-c-d \"{c}\" \"{d}\")
# (find-{c}file \"\")

# http://elpa.gnu.org/packages/{pkg}.html
# http://melpa.org/#/{pkg}
")))



;; Tests: (find-fline ee-elpadir)
;;        (find-fline "~/.emacs.d/elpa/" "lua-mode-")
;;        (ee-file-expand-wildcards-slash "~/.emacs.d/elpa/lua-mode-*")
;;        (ee-package-findelpafiles "lua-mode")
;;
(defun ee-package-findelpafiles (pkgname)
  "Convert a PKGNAME to a list of `(find-elpafile ...)' sexps."
  (let* ((pattern (format "%s%s-*" ee-elpadir pkgname))
	 (fnames (ee-file-expand-wildcards-slash pattern)))
    (mapcar (lambda (s) (list 'find-elpafile s)) fnames)))

(defun ee-file-expand-wildcards-slash (pattern)
"Like `file-expand-wildcards' but with `ee-file-name-nondirectory-slash' & sort."
  (let* ((fnames0 (file-expand-wildcards pattern))
	 (fnames1 (mapcar 'ee-file-name-nondirectory-slash fnames0))
	 (fnames2 (sort fnames1 'string<)))
    fnames2))

(defun ee-file-name-nondirectory-slash (fname)
"Like `file-name-nondirectory', but appends a / to FNAME if it is a directory."
  (concat (file-name-nondirectory fname)
	  (if (file-directory-p fname) "/" "")))




;; «ee-package-dir»  (to ".ee-package-dir")
;; This function converts a package name (a symbol) into the directory
;; in which that package was installed (or nil), using functions from
;; "package.el".
;;
;; Tests: (require 'package)
;;        (package-initialize)
;;        (ee-package-dir 'lua-mode)
;;        (ee-package-dir 'tetris)
;;        (ee-package-dir 'foo)
;;        (ee-package-desc 'lua-mode)
;;        (ee-package-desc 'tetris)
;;        (ee-package-desc 'foo)
;;
;; WARNING: the function `ee-package-dir' and its dependency
;; `ee-package-desc' use several functions from "package.el", and I
;; don't understand package.el well enough!
;;
;; See: (find-efile "emacs-lisp/package.el" "(cl-defstruct (package-desc")
;;      (find-efunction 'describe-package-1)
;;      (find-efunction 'describe-package-1 "(let* ((desc ")
;;
(defun ee-package-dir (pkg)
"Convert the name of the package PKG to the directory where it was installed."
  (let* ((desc (ee-package-desc pkg))
	 (dir (and desc (package-desc-dir desc))))
    (if (stringp dir)
	(replace-regexp-in-string
	 "\\([^/]\\)$" "\\1/"
	 (ee-shorten-file-name dir)))))

(defun ee-package-desc (pkg)
"An internal function used by `ee-package-dir'.
Convert PKG - a symbol - to a package-desc structure (or to nil)."
  (or (if (package-desc-p pkg) pkg)
      (cadr (assq pkg package-alist))
      (let ((built-in (assq pkg package--builtins)))
	(if built-in
	    (package--from-builtin built-in)
	  (cadr (assq pkg package-archive-contents))))))









;;;   __ _           _                     _                 _ _       _        
;;;  / _(_)_ __   __| |       ___ ___   __| | _____/\__     | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____ / __/ _ \ / _` |/ _ \    /_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____| (_| (_) | (_| |  __/_  _\_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|      \___\___/ \__,_|\___| \/       |_|_|_| |_|_|\_\___/
;;;                                                                             
;; «find-code-pdf-links»  (to ".find-code-pdf-links")
;; Tests:
;; (find-fline          "/usr/local/texlive/2018/texmf-dist/doc/latex/base/")
;; (find-code-pdf-links "/usr/local/texlive/2018/texmf-dist/doc/latex/base/source2e.pdf")
;; (find-code-pdf-links "/usr/local/texlive/2018/texmf-dist/doc/latex/base/source2e.pdf" "foo")

;; See: (find-efunction 'ee-if-prefixp)
(defun ee-shorten-file-name (fname)
  "Shorten FNAME if possible to make it start with \"$S/\" or \"~/\"."
  (or (ee-if-prefixp "$S/" "$S/" fname 'fname+)
      (ee-if-prefixp "~/"  "~/"  fname 'fname+)
      fname))

(defun find-code-pdf-links (&optional fname c &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks and `code-pdf-*'s to a PDF file."
  (interactive (list (and (eq major-mode 'dired-mode) (ee-dired-to-fname))))
  (if fname (setq fname (ee-shorten-file-name fname)))
  (setq fname (or fname "{fname}"))
  (setq c (or c "{c}"))
  (let ((dir (file-name-directory fname)))
    (apply 'find-elinks-elisp
     `((find-code-pdf-links ,fname ,c ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       ;;
       ;; (find-efunction 'find-code-pdf-links)
       ,(ee-template0 "\
;; See: (find-eev-quick-intro \"9.1. `code-c-d'\")
;;      (find-pdf-like-intro \"3. Hyperlinks to PDF files\")
;;      (find-pdf-like-intro \"7. Shorter hyperlinks to PDF files\")
;;      (find-pdf-like-intro \"9. Generating three pairs\")
;;      (find-pdf-like-intro \"9. Generating three pairs\" \"`M-h M-p'\")

;; (find-fline {(ee-S (file-name-directory fname))})
\(code-c-d \"{c}\" \"{(file-name-directory fname)}\")
;; (find-{c}file \"\")

;; (find-pdf-page \"{fname}\")
;; (find-pdf-text \"{fname}\")
\(code-pdf-page \"{c}\" \"{fname}\")
\(code-pdf-text \"{c}\" \"{fname}\")
;; (find-{c}page)
;; (find-{c}text)
")
       )
     pos-spec-list)))

;; «find-pdf-links»  (to ".find-pdf-links")
;;
(defun find-pdf-links ()
"Run either `find-code-pdf-links' or `find-pdflike-page-links'."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (find-code-pdf-links (ee-dired-to-fname))
    (find-pdflike-page-links)))



;; «find-code-audiovideo-links»  (to ".find-code-audiovideo-links")
;; Skel: (find-find-links-links-new "code-audiovideo" "fname c" "dir")
;;
(defun find-code-audiovideo-links (&optional fname c &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for code-audiovideo."
  (interactive (list (and (eq major-mode 'dired-mode) (ee-dired-to-fname))))
  (if fname (setq fname (ee-shorten-file-name fname)))
  (setq fname (or fname "{fname}"))
  (setq c (or c "{c}"))
  (let* ((dir (file-name-directory fname)))
    (apply
     'find-elinks-elisp
     `((find-code-audiovideo-links ,fname ,c ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       ;; (find-efunction 'find-code-audiovideo-links)
       ;; ""
       ,(ee-template0 "\
;; See: (find-eev-quick-intro \"9.1. `code-c-d'\")
;;      (find-pdf-like-intro \"9. Generating three pairs\" \"`M-h M-p'\")
;;      (find-audiovideo-intro \"2.1. `find-code-audiovideo-links'\")

;; Links to this directory:
;; (find-fline {(ee-S (file-name-directory fname))})
\(code-c-d \"{c}\" \"{(file-name-directory fname)}\")
;; (find-{c}file \"\")

;; Links to a PDF file:
;; (find-pdf-page \"{fname}\")
;; (find-pdf-text \"{fname}\")
\(code-pdf-page \"{c}\" \"{fname}\")
\(code-pdf-text \"{c}\" \"{fname}\")
;; (find-{c}page)
;; (find-{c}text)

;; Links to an audio file:
;; (find-audio \"{fname}\")
\(code-audio \"{c}audio\" \"{fname}\")
;; (find-{c}audio)
;; (find-{c}audio \"0:00\")

;; Links to a video file:
;; (find-video \"{fname}\")
\(code-video \"{c}video\" \"{fname}\")
;; (find-{c}video)
;; (find-{c}video \"0:00\")
;;
;; (eev-avadj-mode 0)
;; (eev-avadj-mode)

;; Links to an shell-like program (for eepitch):
;; (eepitch-comint \"{c}\" \"{fname}\")

(defun eepitch-{c} () (interactive)
  (eepitch-comint \"{c}\"
     \"{fname}\"))

;; Test:

 (eepitch-{c})
 (eepitch-kill)
 (eepitch-{c})
")
       )
     pos-spec-list)))

;; Tests:
;; (find-code-audiovideo-links "~/eev-videos/three-keys-2.mp4")




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
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
