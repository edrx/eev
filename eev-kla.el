;;; eev-kla.el -- kill link to anchor and friends.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.
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
;; Version:    20230127
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-kla.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-kla.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;; «.a-test»			(to "a-test")
;; «.more-tests»		(to "more-tests")
;; «.test-elsewhere»		(to "test-elsewhere")
;; «.the-video»			(to "the-video")
;;
;; «.ee-kl-expand»		(to "ee-kl-expand")
;; «.default-args»		(to "default-args")
;; «.best-lrcd»			(to "best-lrcd")
;; «.ee-kl-r-c-d»		(to "ee-kl-r-c-d")
;; «.shorter-fnames»		(to "shorter-fnames")
;; «.generate-sexps»		(to "generate-sexps")
;; «.ee-kl-kill»		(to "ee-kl-kill")
;; «.kill-sexps»		(to "kill-sexps")
;; «.ee-kl-insert»		(to "ee-kl-insert")
;; «.eekla2»			(to "eekla2")
;; «.aliases»			(to "aliases")




;;; Commentary:

;; «a-test»  (to ".a-test")
;; Try this:
;;
;;   M-x eekla
;;
;; You will see this message in the echo area:
;;
;;   Copied to the kill ring: (find-eev "eev-kla.el" "a-test")
;;
;; Congratulations! You have just "killed a link to an anchor"! =)
;; What happened in this test was that `eekla' has generated a link to
;; the anchor above and "killed it", in the sense of "copied it to the
;; kill ring"...
;;
;; Most of the docs about eev-kla.el are in this intro:
;;
;;   http://anggtwu.net/eev-intros/find-kla-intro.html
;;                                (find-kla-intro)
;;
;; but some tests can't be run from the intro because they need to be
;; run from a file "in which the conversion c,d<-filename works"; this
;; is explained in details in the intro.
;;
;; Let's see an example: this
;;
;;   (find-eev "eev-kla.el" "tests")
;;
;; is a hyperlink to this file, and to the anchor above - try it! The
;; function `find-eev' was defined by a call to `code-c-d' like the
;; one below,
;;
;;   ;; (find-code-c-d "eev" ee-eev-source-directory :anchor)
;;           (code-c-d "eev" ee-eev-source-directory :anchor)
;;
;; that was run from this module of eev:
;;
;;   (find-eev "eev-code.el" "code-c-d-s")
;;
;; That `code-c-d' defined the function `find-eev' in the "right" way,
;; and added an entry for "eev" in `ee-code-c-d-pairs'. You can see
;; that entry by running this,
;;
;;   (find-eppp ee-code-c-d-pairs "\"eev\"")
;;
;; or by running
;;
;;   (find-kla-links)
;;
;; and exploring the sexps in the temporary buffer that
;; `find-kla-links' generates.



;; «more-tests»  (to ".more-tests")
;; Now try:
;;
;;   M-x eeklf
;;   M-x eeklt
;;
;; You should get these messages in the echo area:
;;
;;   Copied to the kill ring: (find-eevfile "eev-kla.el")
;;   Copied to the kill ring: (to "more-tests")
;;
;; The results of the next tests will depend on what is in the region.
;; If you mark this "foo" - without the quotes - and try
;;
;;   M-x eeklas
;;   M-x eeklfs
;;   M-x eeklts
;;
;; you will get these messages in the echo area:
;;
;;   Copied to the kill ring: (find-eev "eev-kla.el" "more-tests" "foo")
;;   Copied to the kill ring: (find-eevfile "eev-kla.el" "foo")
;;   Copied to the kill ring: (to "more-tests" "foo")



;; «test-elsewhere»  (to ".test-elsewhere")
;; Now try to create a link to another file. Run this to open one of
;; the files in the Emacs sources, and to go to the first occurrence
;; of the string "build specific" in it:
;;
;;   (find-efile "comint.el" "build specific")
;;   (eek "<up> M-3 M-e")
;;
;; Then mark the string "build specific", and run:
;;
;;   M-x eeklf
;;   M-x eeklfs
;;
;; You should get these messages in the echo area:
;;
;;   Copied to the kill ring: (find-efile "comint.el")
;;   Copied to the kill ring: (find-efile "comint.el" "build specific")



;; «the-video»  (to ".the-video")
;; I recorded a video about this for the EmacsConf2022.
;; The video is here:
;;   (find-eev2022klavideo "0:00")
;; and the page about it is here:
;;   http://anggtwu.net/emacsconf2022-kla.html



;;;                  _    _                                       _ 
;;;   ___  ___      | | _| |       _____  ___ __   __ _ _ __   __| |
;;;  / _ \/ _ \_____| |/ / |_____ / _ \ \/ / '_ \ / _` | '_ \ / _` |
;;; |  __/  __/_____|   <| |_____|  __/>  <| |_) | (_| | | | | (_| |
;;;  \___|\___|     |_|\_\_|      \___/_/\_\ .__/ \__,_|_| |_|\__,_|
;;;                                        |_|                      
;; «ee-kl-expand»  (to ".ee-kl-expand")
;; See: (find-kla-intro "15. Symlinks")
;;
(defvar ee-kl-transforms nil
  "Set this if you need to support symlinks in eev-kla.el.
The value of this variable should be a list of pairs of this form:
(regexp replacement).")

(defun ee-kl-transform (fname)
  "Transform FNAME into a canonical form using regexps.
For each pair (regexp replacement) in `ee-kl-transforms' this
function replaces all occurrences of the regexp in FNAME by the
corresponding replacement."
  (cl-loop for (regexp repl) in ee-kl-transforms
           do (setq fname (replace-regexp-in-string regexp repl fname)))
  fname)

(defun ee-kl-expand (fname)
  "Expand FNAME using `ee-expand'.
This function also runs `ee-kl-transform' on the result, but
`ee-kl-transform' is usually a no-op."
  (ee-kl-transform (ee-expand fname)))



;;;  ____        __             _ _                         
;;; |  _ \  ___ / _| __ _ _   _| | |_    __ _ _ __ __ _ ___ 
;;; | | | |/ _ \ |_ / _` | | | | | __|  / _` | '__/ _` / __|
;;; | |_| |  __/  _| (_| | |_| | | |_  | (_| | | | (_| \__ \
;;; |____/ \___|_|  \__,_|\__,_|_|\__|  \__,_|_|  \__, |___/
;;;                                               |___/     
;; «default-args»  (to ".default-args")
;; See: (find-kla-intro "9. `cl-defun'")
;;      (find-kla-intro "10. The default `c', `d', and `r'")

(defun ee-kl-fname ()
  (or (buffer-file-name) default-directory))

(defun ee-kl-anchor ()
  (ee-preceding-tag-flash))

(defun ee-kl-region ()
  (buffer-substring-no-properties (point) (mark)))


;;;  ____            _     _                               _ 
;;; | __ )  ___  ___| |_  | |      _ __       ___       __| |
;;; |  _ \ / _ \/ __| __| | |_____| '__|____ / __|____ / _` |
;;; | |_) |  __/\__ \ |_  | |_____| | |_____| (_|_____| (_| |
;;; |____/ \___||___/\__| |_|     |_|        \___|     \__,_|
;;;                                                          
;; «best-lrcd»  (to ".best-lrcd")
;; These functions try to choose the "best" `c-d' for a filename. They
;; filter `ee-code-c-d-pairs' to find all the `c-d's that "match" that
;; filename, then they choose the best one, and they return it
;; converted to an `l-r-c-d'. The ideas and the terminology are
;; explained here:
;;   (find-kla-intro "7. The best `l-r-c-d'")
;;
;; Tests: (find-eppp (ee-kl-cds))
;;        (find-eppp (ee-kl-lrcds))
;;                   (ee-kl-lrcd)

(defun ee-kl-prefixp (prefix str)
  "If STR starts with PREFIX then return STR minus that prefix.
When STR doesn't start with PREFIX, return nil."
  (and (<= (length prefix) (length str))
       (equal prefix (substring str 0 (length prefix)))
       (substring str (length prefix))))

(defun ee-kl-cds ()
  "Return a copy of `ee-code-c-d-pairs' with all `d's ee-kl-expanded."
  (cl-loop for (c d) in ee-code-c-d-pairs
	   collect (list c (ee-kl-expand d))))

(cl-defun ee-kl-lrcds (&key fname)
  "Return all the `c-d's in (ee-kl-cds) that match FNAME.
Each matching `c-d' is converted to an `l-r-c-d'."
  (setq fname (or fname (ee-kl-fname)))
  (cl-loop for (c d) in (ee-kl-cds)
	   if (ee-kl-prefixp d fname)
	   collect (let* ((r (ee-kl-prefixp d fname))
			  (l (length r)))
		     (list l r c d))))

(cl-defun ee-kl-lrcd (&key fname)
  "Return the best lrcd in (ee-kl-lrcds FNAME).
If (ee-kl-lrcds FNAME) doesn't return any matching `lrcd's, return nil."
  (setq fname (or fname (ee-kl-fname)))
  (let* ((lrcds (ee-kl-lrcds :fname fname))
	 (l< (lambda (lrcd1 lrcd2) (< (car lrcd1) (car lrcd2))))
	 (lrcds-sorted (sort lrcds l<)))
    (car lrcds-sorted)))


;;;  ____        __             _ _                         _ 
;;; |  _ \  ___ / _| __ _ _   _| | |_   _ __    ___      __| |
;;; | | | |/ _ \ |_ / _` | | | | | __| | '__|  / __|    / _` |
;;; | |_| |  __/  _| (_| | |_| | | |_  | | _  | (__ _  | (_| |
;;; |____/ \___|_|  \__,_|\__,_|_|\__| |_|( )  \___( )  \__,_|
;;;                                       |/       |/         
;; «ee-kl-r-c-d»  (to ".ee-kl-r-c-d")
;; See: (find-kla-intro "10. The default `c', `d', and `r'")
;; Tests: (ee-kl-r)
;;        (ee-kl-c)
;;        (ee-kl-d)

(cl-defun ee-kl-r (&key fname)
  (setq fname (or fname (ee-kl-fname)))
  (nth 1 (ee-kl-lrcd :fname fname)))

(cl-defun ee-kl-c (&key fname)
  (setq fname (or fname (ee-kl-fname)))
  (nth 2 (ee-kl-lrcd :fname fname)))

(cl-defun ee-kl-d (&key fname)
  (setq fname (or fname (ee-kl-fname)))
  (nth 3 (ee-kl-lrcd :fname fname)))


;; «shorter-fnames»  (to ".shorter-fnames")
;; See: (find-kla-intro "6. The components")
;;      (find-kla-intro "6. The components" "living fossils")
;;
(cl-defun ee-kl-shortfname (&key fname c r)
  (setq fname (or fname (ee-kl-fname))
	r     (or r     (ee-kl-r :fname fname)))
  r)

(cl-defun ee-kl-shorterfname (&key fname c r)
  (setq fname (or fname (ee-kl-fname))
	r     (or r     (ee-kl-r :fname fname)))
  r)



;;;  ____                      
;;; / ___|  _____  ___ __  ___ 
;;; \___ \ / _ \ \/ / '_ \/ __|
;;;  ___) |  __/>  <| |_) \__ \
;;; |____/ \___/_/\_\ .__/|___/
;;;                 |_|        
;;
;; «generate-sexps»  (to ".generate-sexps")
;; Functions that generate sexps. Tests:
;;   (ee-kl-find-c)
;;   (ee-kl-find-cfile)
;;   (ee-kl-sexp-kla)
;;   (ee-kl-sexp-klas :region "foo")
;;   (ee-kl-sexp-klf)
;;   (ee-kl-sexp-klfs :region "foo")
;; See also:
;;   (find-kla-intro "12. The functions that generate sexps")
;;
(cl-defun ee-kl-find-c (&key fname c)
  "Generate a symbol of the form find-{c}."
  (setq fname  (or fname  (ee-kl-fname))
	c      (or c      (ee-kl-c :fname fname)))
  (intern (format "find-%s" c)))

(cl-defun ee-kl-find-cfile (&key fname c)
  "Generate a symbol of the form find-{c}file."
  (setq fname  (or fname  (ee-kl-fname))
	c      (or c      (ee-kl-c :fname fname)))
  (intern (format "find-%sfile" c)))

(cl-defun ee-kl-sexp-kla (&key fname c r anchor)
  "<K>ill <l>ink to <a>nchor - make sexp."
  (setq fname  (or fname  (ee-kl-fname))
	c      (or c      (ee-kl-c :fname fname))
	r      (or r      (ee-kl-r :fname fname))
	anchor (or anchor (ee-kl-anchor)))
  (list (ee-kl-find-c       :fname fname :c c)
	(ee-kl-shorterfname :fname fname :c c :r r)
	anchor))

(cl-defun ee-kl-sexp-klas (&key fname c r anchor region)
  "<K>ill <l>ink to <a>nchor and <s>tring - make sexp."
  (setq fname  (or fname  (ee-kl-fname))
	c      (or c      (ee-kl-c :fname fname))
	r      (or r      (ee-kl-r :fname fname))
	anchor (or anchor (ee-kl-anchor))
	region (or region (ee-kl-region)))
  (list (ee-kl-find-c       :fname fname :c c)
	(ee-kl-shorterfname :fname fname :c c :r r)
	anchor
	region))

(cl-defun ee-kl-sexp-klf (&key fname c r)
  "<K>ill <l>ink to <f>ile - make sexp."
  (setq fname (or fname (ee-kl-fname))
	c     (or c     (ee-kl-c :fname fname))
	r     (or r     (ee-kl-r :fname fname)))
  (list (ee-kl-find-cfile :fname fname :c c)
	(ee-kl-shortfname :fname fname :c c :r r)))

(cl-defun ee-kl-sexp-klfs (&key fname c r region)
  "<K>ill <l>ink to <f>ile and <s>tring - make sexp."
  (setq fname  (or fname  (ee-kl-fname))
	c      (or c      (ee-kl-c :fname fname))
	r      (or r      (ee-kl-r :fname fname))
	region (or region (ee-kl-region)))
  (list (ee-kl-find-cfile :fname fname :c c)
	(ee-kl-shortfname :fname fname :c c :r r)
	region))

(cl-defun ee-kl-sexp-klt (&key anchor)
  "<K>ill <l>ink to a (<t>o ...) - make sexp."
  (setq anchor (or anchor (ee-kl-anchor)))
  (list 'to anchor))

(cl-defun ee-kl-sexp-klts (&key anchor region)
  "<K>ill <l>ink to a (<t>o ... ...) - make sexp."
  (setq anchor (or anchor (ee-kl-anchor))
	region (or region (ee-kl-region)))
  (list 'to anchor region))



;;;                  _    _       _    _ _ _ 
;;;   ___  ___      | | _| |     | | _(_) | |
;;;  / _ \/ _ \_____| |/ / |_____| |/ / | | |
;;; |  __/  __/_____|   <| |_____|   <| | | |
;;;  \___|\___|     |_|\_\_|     |_|\_\_|_|_|
;;;                                          
;; «ee-kl-kill»  (to ".ee-kl-kill")
;; See: (find-kla-intro "13. Killing and inserting")
;; Tests: (ee-kl-link-to-string "(foo)\n")
;;        (ee-kl-link-to-string '(foo))

(defun ee-kl-kill (link)
  "Kill LINK and show a message.
Here \"kill\" means \"put it in the kill ring.\""
  (setq link (ee-kl-link-to-string link))
  (let ((link0 (replace-regexp-in-string "\n$" "" link)))
    (kill-new link)
    (message "Copied to the kill ring: %s" link0)))

(defun ee-kl-link-to-string (link)
  "Convert LINK to a string using `ee-S'.
If LINK is already a string, return it unchanged.
If LINK is a sexp, convert it to a string with `ee-S' and append
a newline to it."
  (if (stringp link)
      link
    (concat (ee-S link) "\n")))



;;;  _  ___ _ _     
;;; | |/ (_) | |___ 
;;; | ' /| | | / __|
;;; | . \| | | \__ \
;;; |_|\_\_|_|_|___/
;;;                 
;; «kill-sexps»  (to ".kill-sexps")
;; Commands that push sexps into the kill ring.
;;
(defun eekla ()
  "<K>ill <L>ink to <A>nchor.
Put in the kill ring a link to the preceding anchor."
  (interactive)
  (ee-kl-kill (ee-kl-sexp-kla)))

(defun eeklas ()
  "<K>ill <L>ink to <A>nchor and <S>tring.
Put in the kill ring a link to the preceding anchor."
  (interactive)
  (ee-kl-kill (ee-kl-sexp-klas)))

(defun eeklf ()
  "<K>ill <L>ink to <F>ile."
  (interactive)
  (ee-kl-kill (ee-kl-sexp-klf)))

(defun eeklfs ()
  "<K>ill <L>ink to <F>ile and <S>tring."
  (interactive)
  (ee-kl-kill (ee-kl-sexp-klfs)))

(defun eeklt ()
  "<K>ill <L>ink to a (<T>o ...)."
  (interactive)
  (ee-kl-kill (ee-kl-sexp-klt)))

(defun eeklts ()
  "<K>ill <L>ink to a (<T>o ... ...)."
  (interactive)
  (ee-kl-kill (ee-kl-sexp-klts)))



;;;                  _    _       _                     _   
;;;   ___  ___      | | _| |     (_)_ __  ___  ___ _ __| |_ 
;;;  / _ \/ _ \_____| |/ / |_____| | '_ \/ __|/ _ \ '__| __|
;;; |  __/  __/_____|   <| |_____| | | | \__ \  __/ |  | |_ 
;;;  \___|\___|     |_|\_\_|     |_|_| |_|___/\___|_|   \__|
;;;                                                         
;; «ee-kl-insert»  (to ".ee-kl-insert")
;; See: (find-kla-intro "13. Killing and inserting")
;; Tests: (ee-kl-comment-prefix)
;;        (ee-kl-insert "(foo)\n")
;;
(defun ee-kl-comment-prefix (&optional mode)
  "This a quick hack. Override it to add support for more languages."
  (let ((plist '(emacs-lisp-mode ";; "
		 haskell-mode    "-- "
		 lua-mode        "-- "
		 python-mode     "# "
		 agda2-mode      "-- "
		 latex-mode      "%% ")))
    (plist-get plist (or mode major-mode))))

(defun ee-kl-link-to-string-with-comment (link)
  (concat (or (ee-kl-comment-prefix) "# ")
	  (ee-kl-link-to-string link)))

(defun ee-kl-insert (&optional link)
  "Insert (ee-kl-comment-prefix) and then LINK."
  (interactive)
  (insert (ee-kl-link-to-string-with-comment (car kill-ring))))




;;;            _    _       ____  
;;;   ___  ___| | _| | __ _|___ \ 
;;;  / _ \/ _ \ |/ / |/ _` | __) |
;;; |  __/  __/   <| | (_| |/ __/ 
;;;  \___|\___|_|\_\_|\__,_|_____|
;;;                               
;; «eekla2»  (to ".eekla2")
;; See: (find-kla-intro "14. Bidirectional hyperlinks")
;; Based on:
;;   (find-eev "eev-flash.el" "specs")
;;   (find-eev "eev-tlinks.el" "ee-copy-rest" "eeflash-copy")
;; but lasts longer.
;;
(defvar ee-kla2-flash-spec '(highlight 2.0))

(defun ee-kla2-flash (pos1 pos2)
  "Highlight the region between POS1 and POS2 using `ee-kla2-flash-spec'."
  (eeflash pos1 (point) ee-kla2-flash-spec))

(defun ee-kla2-goto-bol ()
  "Move to the beginning of the line.
When not at BOL, move to the beginning of the next line."
  (when (not (= (ee-bol) (point)))	; when not at bol
    (move-beginning-of-line 2))		; do <down> C-a
  (point))

(defun ee-kla2-insert (link)
  "Move to the beginning of the line, insert LINK, and highlight it."
  (let* ((line (ee-kl-link-to-string-with-comment link))
	 (pos-before-line (ee-kla2-goto-bol)))
    (insert line)
    (ee-kla2-flash pos-before-line (point))))

(defun eekla2 ()
  "Insert a link \"to here\" \"there\" and a link \"to there\" \"here\"."
  (interactive)
  (let* ((link1 (ee-kl-sexp-kla))
	 (link2 (prog2 (other-window 1)
		    (ee-kl-sexp-kla)
		  (other-window -1))))
    (ee-kla2-insert link2)
    (other-window 1)
    (ee-kla2-insert link1)
    (other-window -1)))



;;;     _    _ _                     
;;;    / \  | (_) __ _ ___  ___  ___ 
;;;   / _ \ | | |/ _` / __|/ _ \/ __|
;;;  / ___ \| | | (_| \__ \  __/\__ \
;;; /_/   \_\_|_|\__,_|___/\___||___/
;;;                                  
;; «aliases»  (to ".aliases")
;; See: (find-kla-intro "3. Aliases")
;; I use these aliases:
;; (defalias 'kla  'eekla)
;; (defalias 'klas 'eeklas)
;; (defalias 'klf  'eeklf)
;; (defalias 'klfs 'eeklfs)
;; (defalias 'klt  'eeklt)
;; (defalias 'klts 'eeklts)
;; (defalias 'kli  'ee-kl-insert)
;; (defalias 'kla2 'eekla2)

(provide 'eev-kla)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
