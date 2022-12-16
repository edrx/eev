;;; eev-elinks.el --- `find-efunction-links' and other `find-e*-links'  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012-2022 Free Software Foundation, Inc.
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
;; Version:    20221216
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-elinks.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-elinks.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
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
;; «.find-pdflike-page-links»	(to "find-pdflike-page-links")
;; «.ee-hyperlink-prefix»	(to "ee-hyperlink-prefix")

;; «.find-eface-links»		(to "find-eface-links")
;; «.find-ecolor-links»		(to "find-ecolor-links")
;; «.find-epackage-links»	(to "find-epackage-links")
;;   «.ee-package-desc»		(to "ee-package-desc")
;;   «.ee-package-dir»		(to "ee-package-dir")
;; «.find-esetkey-links»	(to "find-esetkey-links")
;; «.find-ekbmacro-links»	(to "find-ekbmacro-links")
;; «.find-emajormode-links»	(to "find-emajormode-links")
;; «.find-eminormodes-links»	(to "find-eminormodes-links")
;; «.find-emodeline-links»	(to "find-emodeline-links")
;; «.find-emenubar-links»	(to "find-emenubar-links")

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
(defun ee-url-at-point ()
  (ee-no-properties (thing-at-point 'url)))

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

(defun ee-1stclassvideo-around-point-ask ()
"Ask for the name of a first-class video; the default is a string around point."
  (ee-1stclassvideo-ask (ee-stuff-around-point "A-Za-z0-9")))

;; See: (find-eev "eev-videolinks.el" "ee-1stclassvideos-info")
;; Tests: (ee-1stclassvideo-ask "foo")
;;        (ee-1stclassvideo-ask "eev2020")
;;        (ee-1stclassvideo-ask "eev2020video")
;;        (ee-1stclassvideos)
;;
(defun ee-1stclassvideo-ask (default0)
  "An internal function used by `ee-1stclassvideo-around-point-ask'."
  (let* ((default1 (replace-regexp-in-string "video$" "" default0))
	 (videos   (ee-1stclassvideos))
	 (default  (if (member default1 videos) default1))
	 (promptd  (if default (format " (default: %s)" default) ""))
	 (prompt   (format "Stem (\"c\") of the video:%s " promptd)))
    (completing-read prompt videos nil nil nil nil default)))

(defun ee-1stclassvideos ()
  (sort (mapcar 'car ee-1stclassvideos-info) 'string<))
  



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
;;   (find-eev-quick-intro  "(find-efunction-links 'find-file)")
;;   (find-eev-quick-intro "4.2. `find-ekey-links' and friends")
;;                           (find-efunction-links 'find-file)
;;                                    (eek "M-h M-f find-file")
;;
;; Key binding: (find-eev "eev-mode.el" "eev-mode-map-set" "M-h" "M-f")
;;
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
    (find-ealias ',f)
    (find-eppp (symbol-plist ',f))
    (find-hfunction ',f)
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
    (find-epp (assoc ,(symbol-file f 'defun) load-history))
    (find-eppp (mapcar 'car load-history))
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
;; Skel: (find-find-links-links-new "evariable" "var" "")
;; Tests: (find-evariable-links 'isearch-mode-map)
;;        (find-evariable-links 'line-move-visual)
;;                (eek "M-h M-v  line-move-visual")
;; Key binding: (find-eev "eev-mode.el" "eev-mode-map-set" "M-h" "M-v")
;;        
(defun find-evariable-links (var &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks about a variable."
  (interactive (find-function-read 'defvar))
  (apply
   'find-elinks
   `((find-evariable-links ,var ,@pos-spec-list)
     (eek ,(format "M-h M-v  %s" var))
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-evariable-links)
     ""
     ,var
     (describe-variable ',var)
     (find-evardescr ',var)
     (find-evariable ',var)
     (find-epp ,var)
     (find-eppp (symbol-plist ',var))
     (find-hvariable ',var)
     ""
     (find-enode "Variable Index" ,(format "* %S:" var))
     (find-elnode "Index" ,(format "* %S:" var))
     ""
     (keymapp ,var)
     (find-ekeymapdescr ,var)
     (describe-keymap ,var)
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

;; Test: (ee-format-kbd-macro [down])
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
;;
;; Key binding:
;; (find-eev "eev-mode.el" "eev-mode-map-set")
;; (find-eev "eev-mode.el" "eev-mode-map-set" "\\M-h\\M-i")
;;
;; See:
;; (find-eev "eev-hlinks.el" "ee-fhl-main-program")
;; (find-eev "eev-hlinks.el" "ee-fhl-main-program" "(ee-find-info-links)")
;; (find-eev-quick-intro "10.2. Generating short hyperlinks to info nodes")

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
;; Note that this code is very old and uses only very basic Lisp
;; functions. The functions in eev-kla.el do something similar to
;; this, but using cl-loop. See:
;;   (find-kla-intro)
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
;;   (find-eppp ee-code-c-d-pairs)
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
  (and (<= (length prefix) (length str))
       (equal prefix (substring str 0 (length prefix)))
       (substring str (length prefix))))

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
;; Skel: (find-find-links-links-new "grep" "" "")
;;
;; The functions `find-grep-links' and `ee-find-grep-links' are
;; similar to `find-file-links', described here,
;;
;;   (find-eev-quick-intro "10.1. Generating short hyperlinks to files")
;;
;; in the sense that they generate short hyperlinks to the default
;; directory and to its parent directories, but 1) they generate
;; `find-xxxgrep' links instead of `find-xxxfile' links, and 2) they
;; combine them with most recent elements in `grep-history'.
;;
;; Here's a micro-tutorial. Run `M-x grep', and complete the grep
;; command with a string to search for and a list of files, like this,
;;
;;   grep --color -nH --null -e
;;   -->
;;   grep --color -nH --null -e Punch *.el
;;
;; and hit RET. You should get a buffer named "*grep*" with the
;; results. If you type `M-h M-h' there the function `find-here-links'
;; will run `ee-find-grep-links' to generate hyperlinks to the result
;; of running that grep command, and one of those hyperlinks will be:
;;
;;   (find-eevgrep "grep --color -nH --null -e Punch *.el")
;;
(defun find-grep-links (&rest pos-spec-list)
"Visit a temporary buffer containing `find-xxxgrep' sexps."
  (interactive)
  (apply
   'find-elinks
   `((find-grep-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-grep-links)
     ""
     ,@(ee-find-grep-links)
     )
   pos-spec-list))

(defun ee-find-grep-links ()
  "An internal function used by `find-grep-links'."
  (append
   (ee-find-grep-links0
    (ee-find-grep-functions default-directory)
    (ee-find-grep-commands))
   (ee-find-grep-links1)))

;; Low-level functions used by `ee-find-grep-links'.
;; Tests:
;;   (find-elinks (ee-find-grep-links))
;;   (ee-find-grep-links)
;;   (ee-find-grep-links0 '(find-Agrep find-Bgrep) '("grep AA *" "grep BB *"))
;;     (ee-find-grep-functions ee-emacs-lisp-directory)
;;     (ee-find-grep-functions ee-eev-source-directory)
;;     (ee-find-grep-commands)
;;   (find-elinks (ee-find-grep-links1))
;;
(defun ee-find-grep-links0 (find-xxxgreps grep-commands)
  "An internal function used by `find-grep-links'."
  (let (result)
    (dolist (head find-xxxgreps)
      (dolist (command grep-commands)
	(setq result (cons `(,head ,command) result))))
    (nreverse result)))

(defun ee-find-grep-commands ()
  "An internal function used by `find-grep-links'."
  (cons "grep -nH -e _ *" (ee-first-n-elements 4 grep-history)))

(defun ee-first-n-elements (n list)
  "Example: (ee-first-n-elements 2 '(a b c d e f))   ==> (a b)"
  (if (and (> n 0) list)
      (cons (car list)
	    (ee-first-n-elements (1- n) (cdr list)))))

(defun ee-find-grep-functions (dir)
  "An internal function used by `find-grep-links'."
  (ee-code-c-d-filter-2 dir '(ee-intern "find-%sgrep" c)))

;; See:
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2021-02/msg00778.html
;;
(defun ee-find-grep-links1 ()
  "An internal function used by `find-grep-links'."
  (let ((dir (ee-shorten-file-name default-directory)))
    (list (ee-template0 "
(let ((default-directory {(ee-S dir)}))
  (grep {(ee-S (car grep-history))})
  )
"))))







;;;            _  __ _ _ _                                     
;;;  _ __   __| |/ _| (_) | _____       _ __   __ _  __ _  ___ 
;;; | '_ \ / _` | |_| | | |/ / _ \_____| '_ \ / _` |/ _` |/ _ \
;;; | |_) | (_| |  _| | |   <  __/_____| |_) | (_| | (_| |  __/
;;; | .__/ \__,_|_| |_|_|_|\_\___|     | .__/ \__,_|\__, |\___|
;;; |_|                                |_|          |___/      
;;
;; «find-pdflike-page-links» (to ".find-pdflike-page-links")
;; The function `find-pdflike-page-links' is called from
;; `find-pdf-links' (`M-h M-p') when you call it in a buffer that is
;; not in dired mode. See:
;;   (to "find-pdf-links")
;;   (find-pdf-like-intro "10. Generating a pair with the page number")
;;   (find-pdf-like-intro "11. How `M-h M-p' guesses everything")
;;
;; Skel: (find-find-links-links-new "pdflike-page" "page bufname offset" "")
;;
(defun find-pdflike-page-links (&optional page bufname offset &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks to a pdf-like document.
See: (find-pdf-like-intro)
     (find-pdf-like-intro \"refining hyperlinks to pages\")"
  (interactive)
  (setq page    (or page (ee-current-page)))
  (setq bufname (or bufname (buffer-name)))
  (setq offset  (or offset ee-page-offset))
  (apply
   'find-elinks
   `((find-pdflike-page-links ,page ,bufname ,offset ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-pdf-like-intro "10. Generating a pair with the page number")
     (find-pdf-like-intro "11. How `M-h M-p' guesses everything")
     ;; (find-efunction 'find-pdflike-page-links)
     ""
     ,@(ee-pdflike-page-links page bufname offset)
     )
   pos-spec-list))

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

;; These are internal functions used by `find-pdflike-page-links' and
;; `ee-pdflike-page-links'.

;; Based on: (find-efunction 'count-lines)
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
;; «find-eface-links» (to ".find-eface-links")
;; Skel: (find-find-links-links-new "eface" "face-symbol" "fg bg")
;; Test: (find-eface-links 'eepitch-star-face)
;; Key binding:
;;   (find-eev "eev-mode.el" "eev-mode-map-set")
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "M-h" "M-s" "find-eface-links")
;;
(defun find-eface-links (&optional face-symbol &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks about FACE-SYMBOL.
When called interactively generate hyperlinks about the face at point.
See the documentation for `ee-face-at-point' to understand what that
means."
  (interactive (list (ee-face-at-point current-prefix-arg)))
  (setq face-symbol (or face-symbol "{face-symbol}"))
  (let* ((fg (face-foreground face-symbol))
         (bg (face-background face-symbol)))
    (apply
     'find-elinks
     `((find-eface-links ',face-symbol ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-efunction 'find-eface-links)
       ""
       (find-enode "Faces")
       (find-elnode "Faces")
       ""
       (find-efacedescr     ',face-symbol)
       (find-efaces         ',face-symbol)
       (find-eface          ',face-symbol)
       (describe-face       ',face-symbol)
       (customize-face      ',face-symbol)
       (set-face-foreground ',face-symbol ,fg)
       (set-face-background ',face-symbol ,bg)
       ""
       (find-ecolor-links ,fg)
       (find-ecolor-links ,bg)
       (find-ecolors)
       ""
       (face-all-attributes ',face-symbol (selected-frame))
       (find-epp (face-all-attributes ',face-symbol (selected-frame)))
       ""
       (face-id ',face-symbol)
       (find-epp (mapcar (lambda (face) (cons (face-id face) face)) (face-list)))
       )
     pos-spec-list)))




(defun ee-face-at-point (&optional arg)
  "Return the face at point as a symbol; ARG determines what that means.
When ARG is nil this returns (face-at-point); in a font-locked
buffer this is typically something like `font-lock-comment-face'.
When ARG is 1 and the character at point is a glyph this returns
the face of that glyph - something like `eepitch-star-face'.
When ARG is anything else this returns (symbol-at-point); so if
the point is on the name of face this returns the face with that
name.

See the source code for `find-eface-links' and `find-efacedescr'
to understand how this is used. ARG is usually
`current-prefix-arg', so the default for it is to be nil."
  (if (eq arg nil)
      (or (face-at-point) 'default)
    (if (eq arg 1)
	(ee-face-of-glyph (char-after (point)))
      (symbol-at-point))))
  
;; Tests: (ee-face-of-glyph ?)
;;        (ee-face-of-glyph ?@)
(defun ee-face-of-glyph (char)
  "An internal function used by `ee-face-at-point'."
  (let* ((display-table standard-display-table)
         (disp-vector  (and display-table (aref display-table char)))
         (disp-vector0 (and disp-vector   (aref disp-vector 0)))
	 (face         (and disp-vector0  (glyph-face disp-vector0))))
    face))




;;;   __ _           _                     _                _ _       _        
;;;  / _(_)_ __   __| |      ___  ___ ___ | | ___  _ __    | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |____ / _ \/ __/ _ \| |/ _ \| '__|___| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |____|  __/ (_| (_) | | (_) | | |____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     \___|\___\___/|_|\___/|_|      |_|_|_| |_|_|\_\___/
;;;                                                                              
;; «find-ecolor-links» (to ".find-ecolor-links")
;; Skel: (find-find-links-links-new "color" "initialcolor" "")
;; Tests: (find-ecolor-links)
;;        (find-ecolor-links "sienna")
;;        (find-ecolor-links "#123456")
;;        (ee-color-values   "sienna")
;;        (ee-color-values   "#123456")
;;        (ee-color-choose-tk)
;;        (ee-color-choose-tk "sienna")
;;        (ee-color-choose-tk "#123456")
;;
(defun find-ecolor-links (&optional initialcolor &rest pos-spec-list)
  "Visit a temporary buffer containing hyperlinks for the color INITIALCOLOR."
  (interactive)
  (setq initialcolor (or initialcolor "#123456"))
  (apply
   'find-elinks
   `((find-ecolor-links ,initialcolor ,@pos-spec-list)
     (find-ecolor-links (read-color))
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-ecolor-links)
     ""
     (find-enode "Colors")
     (find-elnode "Color Names")
     ""
     (find-ecolor-links (ee-color-choose-tk ,(or initialcolor "gray")))
     (find-ecolor-links ,(or initialcolor "gray"))
     (find-ecolors)
     (find-ecolors ,initialcolor)
     ,`(insert (propertize " Sample " 'face '(:background ,initialcolor)))
     ,`(ee-color-values ,initialcolor)
     (kill-new ,initialcolor)
     (kill-new ,(ee-color-values initialcolor))
     )
   pos-spec-list))

(defun ee-color-values (color)
  "Return the #RRGGBB representation for COLOR."
  (apply 'format "#%02x%02x%02x"
	 (mapcar (lambda (c) (lsh c -8)) (color-values color))))

(defun ee-color-choose-tk (&optional initialcolor)
  "Call Tcl/Tk to choose a color similar to INITIALCOLOR.
The original version of this function used the directory $EEVTMPDIR.
This version uses the file /tmp/ee.tcl instead of $EEVTMPDIR/ee.tcl.
Don't use this in multi-user machines."
  (let ((ee-file-tcl "/tmp/ee.tcl")
	(tclcode (format "puts [tk_chooseColor -initialcolor %s]; exit\n"
			 (or initialcolor "gray"))))
    (ee-write-string tclcode ee-file-tcl)
    (find-sh0 (format "wish %s" ee-file-tcl))))



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
  (setq pkg (or pkg '{pkg}))
  (setq c (or c (ee-find-epackage-pkg-to-c pkg)))
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
  (setq c (or c (ee-find-epackage-pkg-to-c pkg)))
  (let* ((spkg (format "\n  %s " pkg))
	 (findelpafiles0 (ee-package-findelpafiles pkg))
	 (findelpafiles1 (reverse findelpafiles0))
	 (findelpafiles (mapconcat 'ee-HS findelpafiles1 "\n"))
	 )
    (ee-template0 "\
# (find-epackages '{pkg})
# (find-epackage-links '{pkg})
# (find-epackage '{pkg})
{findelpafiles}

# (ee-package-dir '{pkg})
# (find-epp     (ee-package-desc '{pkg}))
# (find-estruct (ee-package-desc '{pkg}))

# (package-initialize)
# (package-refresh-contents)
# (package-install '{pkg})
# (find-epackage   '{pkg})
# (ee-package-dir  '{pkg})
# (find-fline    (ee-package-dir '{pkg}))
# (find-epp     (ee-package-desc '{pkg}))
# (find-estruct (ee-package-desc '{pkg}))
# (kill-new      (ee-package-url '{pkg}))
# (insert \"\\n# \" (ee-package-url '{pkg}))
# (package-delete (ee-package-desc '{pkg}))

# (find-epackage-links '{pkg} \"{c}\" t)
# (find-epackage       '{pkg})
# (code-c-d \"{c}\" \"{d}\")
# (find-{c}file \"\")

# http://elpa.gnu.org/packages/{pkg}.html
# http://elpa.nongnu.org/nongnu/{pkg}.html
# http://melpa.org/#/{pkg}
")))

(defun ee-find-epackage-pkg-to-c (pkg)
  "Convert PKG (a symbol) to a \"c\" for a `code-c-d'."
  (replace-regexp-in-string "[-]" "" (symbol-name pkg)))



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




;; «ee-package-desc»  (to ".ee-package-desc")
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
(defun ee-package-desc (pkg)
"An internal function used by `ee-package-dir'.
Convert PKG - a symbol - to a package-desc structure (or to nil)."
  (or (if (package-desc-p pkg) pkg)
      (cadr (assq pkg package-alist))
      (let ((built-in (assq pkg package--builtins)))
	(if built-in
	    (package--from-builtin built-in)
	  (cadr (assq pkg package-archive-contents))))))

(defun ee-package-dir (pkg)
"Convert the name of the package PKG to the directory where it was installed."
  (let* ((desc (ee-package-desc pkg))
	 (dir (and desc (package-desc-dir desc))))
    (if (stringp dir)
	(replace-regexp-in-string
	 "\\([^/]\\)$" "\\1/"
	 (ee-shorten-file-name dir)))))

(defun ee-package-url (pkg)
  "Convert the name of the package PKG to its website."
  (let ((desc (ee-package-desc pkg)))
    (and desc (alist-get :url (package-desc-extras desc)))))







;;;   __ _           _                      _   _              
;;;  / _(_)_ __   __| |       ___  ___  ___| |_| | _____ _   _ 
;;; | |_| | '_ \ / _` |_____ / _ \/ __|/ _ \ __| |/ / _ \ | | |
;;; |  _| | | | | (_| |_____|  __/\__ \  __/ |_|   <  __/ |_| |
;;; |_| |_|_| |_|\__,_|      \___||___/\___|\__|_|\_\___|\__, |
;;;                                                      |___/ 
;;
;; «find-esetkey-links»  (to ".find-esetkey-links")
;; Skel: (find-find-links-links-new "esetkey" "key command" "longkey")
;; Test: (find-esetkey-links (kbd "M-o") 'other-window)
;;  See: (find-eevtemplvideo "14:20" "4. `find-esetkey-links'")
;;       (find-eevtemplvideo "14:45"   "if we just run M-x find-esetkey-links")
;;
(defun find-esetkey-links (&optional key command &rest pos-spec-list)
  "Visit a temporary buffer containing sexps for setting a key."
  (interactive
   (let* ((menu-prompting nil)
          (key (read-key-sequence "Set key: " nil t))
	  (longkey (format-kbd-macro key))
	  (command (ee-read-command)))
     (list key command)))
  (setq key (or key (kbd "M-o")))
  (setq command (or command 'other-window))
  (let* ((longkey (format-kbd-macro key)))
    (apply
     'find-elinks-elisp
     `((find-esetkey-links (kbd ,longkey) ',command ,@pos-spec-list)
       (find-esetkey-links (kbd "M-o") 'other-window ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-ekeydescr (kbd ,longkey))
       (find-efunctiondescr ',command)
       (find-efunction 'find-esetkey-links)
       ""
       (find-enode "Rebinding" "M-x global-set-key")
       (find-elnode "Changing Key Bindings" "Function: define-key")
       (find-efunctiondescr 'global-set-key)
       (find-efunctiondescr 'global-unset-key)
       (find-efunctiondescr 'local-set-key)
       (find-efunctiondescr 'local-unset-key)
       (find-efunctiondescr 'define-key)
       (find-efunctiondescr 'kbd)
       (find-enode "Misc Help" "describe-bindings")
       (eek "C-h b  ;; describe-bindings")
       ""
       ,(ee-template0 "\
(global-set-key   (kbd \"{longkey}\") '{command})
(global-unset-key (kbd \"{longkey}\"))

(local-set-key    (kbd \"{longkey}\") '{command})
(local-unset-key  (kbd \"{longkey}\"))

;; (find-ekeymapdescr eev-mode-map)
(define-key eev-mode-map (kbd \"{longkey}\") '{command})
(define-key eev-mode-map (kbd \"{longkey}\") nil)
")
       )
     pos-spec-list)))

(defun ee-read-command ()
  "An internal function used by `find-esetkey-links'."
  (let* ((cmd-at-pt (ee-command-at-point))
	 (prompt (if cmd-at-pt
		     (format "Command (default %s): " cmd-at-pt)
		   "Command: ")))
    (read-command prompt cmd-at-pt)))

(defun ee-command-at-point ()
  "An internal function used by `find-esetkey-links'."
  (let ((symbol (symbol-at-point)))
    (if (commandp symbol) symbol)))




;;;   __ _           _                                      
;;;  / _(_)_ __   __| |      _ __ ___   __ _  ___ _ __ ___  
;;; | |_| | '_ \ / _` |_____| '_ ` _ \ / _` |/ __| '__/ _ \ 
;;; |  _| | | | | (_| |_____| | | | | | (_| | (__| | | (_) |
;;; |_| |_|_| |_|\__,_|     |_| |_| |_|\__,_|\___|_|  \___/ 
;;;                                                         
;; «find-ekbmacro-links» (to ".find-ekbmacro-links")
;; Skel: (find-find-links-links-new "ekbmacro" "" "")
;; Test: (find-ekbmacro-links)
;;
;; The standard way to save and edit keyboard macros is with
;; the key sequences explained here,
;;   (find-enode "Keyboard Macros")
;;   (find-enode "Save Keyboard Macro")
;;   (find-enode "Edit Keyboard Macro")
;; but I prefer to use this function. It is bound to `M-h M':
;;   (find-eev "eev-mode.el" "eev-mode-map-set")
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "\\M-hM")
;;
(defun find-ekbmacro-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks on keyboard macros."
  (interactive)
  (apply
   'find-elinks
   `((find-ekbmacro-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-ekbmacro-links)
     ""
     (format-kbd-macro last-kbd-macro)
     (setq last-kbd-macro (kbd ,(format-kbd-macro last-kbd-macro)))
     (eek ,(format-kbd-macro last-kbd-macro))
     ""
     (find-enode "Keyboard Macros")
     (find-enode "Save Keyboard Macro")
     (find-enode "Edit Keyboard Macro")
     (find-efunctiondescr 'edmacro-mode)
     (eek "M-h M-k C-x C-k C-e  ;; kmacro-edit-macro-repeat")
     (eek "        C-x C-k C-e  ;; kmacro-edit-macro-repeat")
     (eek "M-h M-k C-x C-k l    ;; kmacro-edit-lossage")
     (eek "        C-x C-k l    ;; kmacro-edit-lossage")
     )
   pos-spec-list))



;; «find-emajormode-links»  (to ".find-emajormode-links")
;; Skel: (find-find-links-links-new "emajormode" "mode" "")
;; Test: (find-emajormode-links major-mode)
;;
(defun find-emajormode-links (&optional mode &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks about the major mode."
  (interactive (list major-mode))
  (setq mode (or mode "{mode}"))
  (apply
   'find-elinks
   `((find-emajormode-links ,mode ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-emajormode-links)
     ""
     (find-efunction-links ',mode)
     (find-efunctiondescr  ',mode)
     (find-efunction       ',mode)
     ""
     (find-estring (documentation ',mode))
     (find-estring (documentation ',mode t))
     )
   pos-spec-list))




;; «find-eminormodes-links»  (to ".find-eminormodes-links")
;; Skel: (find-find-links-links-new "eminormodes" "" "")
;; Test: (find-eminormodes-links)
;;
(defun find-eminormodes-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks about minor modes."
  (interactive)
  (apply
   'find-elinks
   `((find-eminormodes-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-eminormodes-links)
     ""
     (find-elnode "Minor Modes")
     (find-eppp minor-mode-list)
     (find-eppp local-minor-modes)
     (find-eppp global-minor-modes)
     ""
     (require 'dash)
     (find-eppp (--filter (not (boundp it)) minor-mode-list))
     (find-eppp (--filter (and (boundp it) (symbol-value it)) minor-mode-list))
     (find-elinks (--map `(find-efunctiondescr ',it) minor-mode-list))
     (find-elinks (--map `(find-efunction ',it) minor-mode-list))
     ""
     (find-elnode "Active Keymaps")
     (find-elnode "Standard Keymaps")
     (find-elnode "Searching Keymaps")
     (find-elnode "Controlling Active Maps")
     (find-elnode "Controlling Active Maps" "current-minor-mode-maps")
     (find-elnode "Controlling Active Maps" "minor-mode-key-binding")
     (find-eppp (current-minor-mode-maps))
     (find-eppp minor-mode-map-alist)
     (find-eppp (-map 'car minor-mode-map-alist))
     ""
     (find-efunctiondescr 'define-minor-mode)
     (find-efunction 'define-minor-mode)
     (find-eppm '(define-minor-mode ee-FOO-mode "no doc" :keymap ee-FOO-map))
     )
   pos-spec-list))




;; «find-emodeline-links»  (to ".find-emodeline-links")
;; Skel: (find-find-links-links-new "emodeline" "" "")
;; Test: (find-emodeline-links)
;;
(defun find-emodeline-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks about the mode line.
You can use this to understand how the mode line works."
  (interactive)
  (apply
   'find-elinks
   `((find-emodeline-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-emodeline-links)
     ""
     (find-elnode "Mode Line Format")
     (find-elnode "Emulating Mode Line")
     (find-evardescr 'mode-line-format)
     (find-epp mode-line-format)
     (find-epp (format-mode-line mode-line-format))
     (find-epp (format-mode-line mode-line-format 'header-line))
     (ee-no-properties (format-mode-line mode-line-format))
     (insert "\n# " (format-mode-line mode-line-format))
     ""
     (find-evardescr 'mode-line-front-space)
     (find-evardescr 'mode-line-mule-info)
     (find-evardescr 'mode-line-client)
     (find-evardescr 'mode-line-modified)
     (find-evardescr 'mode-line-remote)
     (find-evardescr 'mode-line-frame-identification)
     (find-evardescr 'mode-line-buffer-identification)
     (find-evardescr 'mode-line-position)
     (find-evardescr 'mode-line-modes)
     (find-evardescr 'mode-line-misc-info)
     (find-evardescr 'mode-line-end-spaces)
     )
   pos-spec-list))


;; «find-emenubar-links»  (to ".find-emenubar-links")
;; Skel: (find-find-links-links-new "emenubar" "" "")
;; Test: (find-emenubar-links)
;;
(defun find-emenubar-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks about the menu bar."
  (interactive)
  (apply
   'find-elinks
   `((find-emenubar-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-emenubar-links)
     ""
     ,(ee-template0 "\
# (find-enode  \"Menu Bar\")
# (find-elnode \"Menu Bar\")
# (find-efile  \"menu-bar.el\")
# (find-elnode \"Easy Menu\")
# (find-efile  \"easymenu.el\")
# (find-egrep \"grep --color=auto -nH --null -e easy-menu-define *.el */*.el\")

# (describe-bindings [(menu-bar)])
# (describe-bindings [(menu-bar) file])
# (describe-bindings [(menu-bar) edit])
# (describe-bindings [(menu-bar) options])
# (describe-bindings [(menu-bar) buffer])
# (describe-bindings [(menu-bar) tools])
# (describe-bindings [(menu-bar) help-menu])

# (find-ekeymapdescr menu-bar-file-menu)
# (find-ekeymapdescr menu-bar-edit-menu)
# (find-ekeymapdescr menu-bar-options-menu)
# (find-eapropos    \"menu-bar.*buffer\")
# (find-ekeymapdescr menu-bar-tools-menu)
# (find-ekeymapdescr menu-bar-help-menu)
# (find-evardescr   'menu-bar-final-items)

# (find-efunctiondescr 'menu-bar-mode)
# (call-interactively  'menu-bar-mode)
# (find-esetkey-links (kbd \"s-m\") 'menu-bar-mode)

# (load             \"menu-bar.el\")
# (find-efunctionpp 'menu-bar-mode)
# (menu-bar-mode 'toggle)
# (load             \"menu-bar.elc\")
")
     )
   pos-spec-list))







;;;   __ _           _                     _                 _ _       _        
;;;  / _(_)_ __   __| |       ___ ___   __| | _____/\__     | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____ / __/ _ \ / _` |/ _ \    /_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____| (_| (_) | (_| |  __/_  _\_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|      \___\___/ \__,_|\___| \/       |_|_|_| |_|_|\_\___/
;;;                                                                             
;; «find-code-pdf-links»  (to ".find-code-pdf-links")
;; See: (to "find-pdf-links")
;;      (find-pdf-like-intro "9. Generating three pairs" "find-code-pdf-links")

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
;; This function - usually bound to `M-h M-p' - behaves in one way
;; when invoked from dired buffers and in a totally different way when
;; invoked from other buffers. In a dired buffer it supposes that the
;; current line contains the name of a PDF, and it generates a buffer
;; whose main part is a pair `code-pdf-page'/`code-pdf-text' that lets
;; you define short hyperlinks to that PDF. See:
;;
;;   (find-pdf-like-intro "7. Shorter hyperlinks to PDF files")
;;   (find-pdf-like-intro "9. Generating three pairs" "`M-h M-p' in Dired mode")
;;
;; When the current buffer is not in dired mode this function supposes
;; that the buffer contains the "text" of a PDF, as explained here:
;;
;;   (find-pdf-like-intro "3. Hyperlinks to PDF files" "find-pdf-text")
;;
;; and it tries to generate short hyperlinks to the current page of
;; it, making lots of guesses, and often guessing everything wrong.
;; See:
;;
;;   (find-pdf-like-intro "10. Generating a pair with the page number")
;;   (find-pdf-like-intro "11. How `M-h M-p' guesses everything")
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
