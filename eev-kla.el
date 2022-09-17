;;; eev-kla.el -- kill link to anchor and friends.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.
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
;; Version:    20220917
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-kla.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-kla.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;; «.intro»			(to "intro")
;; «.variables»			(to "variables")
;;  «.test»			(to "test")
;; «.simple-defaults»		(to "simple-defaults")
;; «.other-defaults»		(to "other-defaults")
;; «.generate-sexps»		(to "generate-sexps")
;; «.kill-sexps»		(to "kill-sexps")
;; «.eekla2»			(to "eekla2")
;; «.demo»			(to "demo")
;; «.aliases»			(to "aliases")




;;; Commentary:

;; «intro»  (to ".intro")
;;
;; 0. Prerequisites
;; ================
;; This tool will only make sense to people who understand `code-c-d',
;; anchors, and `find-here-links' very well. See:
;;
;;   (find-eev-quick-intro "9.1. `code-c-d'")
;;   (find-eev-quick-intro "9.2. Extra arguments to `code-c-d'")
;;   (find-eev-quick-intro "9.2. Extra arguments to `code-c-d'" "to anchors")
;;   (find-eev-quick-intro "8. Anchors")
;;   (find-eev-quick-intro "4. Creating Elisp Hyperlinks")
;;   (find-eev-quick-intro "4.1. `find-here-links'")
;;
;;
;;
;; 1. The problem
;; ==============
;; This section of the main tutorial
;;
;;   (find-eev-quick-intro "10.1. Generating short hyperlinks to files")
;;
;; explains that after running these four `code-c-d's
;;
;;   (code-c-d "foo"  "/tmp/FOO/")
;;   (code-c-d "bar"  "/tmp/FOO/BAR/")
;;   (code-c-d "plic" "/tmp/FOO/BAR/PLIC/")
;;
;; all these sexp hyperlinks will point to the same file:
;;
;;   (find-file  "/tmp/FOO/BAR/PLIC/bletch")
;;   (find-fline "/tmp/FOO/BAR/PLIC/bletch")
;;   (find-foofile        "BAR/PLIC/bletch")
;;   (find-barfile            "PLIC/bletch")
;;   (find-plicfile                "bletch")
;;
;; So if we are in the file /tmp/FOO/BAR/PLIC/bletch then there isn't
;; an obvious choice for the best short hyperlink to that file, and
;; `find-here-links' will display all the choices above... Try this:
;;
;;   (eek "<down> M-3 M-e M-h M-h")
;;   (find-fline "/tmp/FOO/BAR/PLIC/bletch")
;;
;; It will show the "here links" to /tmp/FOO/BAR/PLIC/bletch at the
;; window at the right.
;;
;; The _standard_ way to create a link to /tmp/FOO/BAR/PLIC/bletch is
;; to run `find-here-links' - i.e., `M-h M-h' - and then choose one of
;; the hyperlinks, and do mark, copy, and paste. But sometimes we want
;; something faster.
;;
;;
;;
;; 2. Converting `c's to `d's
;; ==========================
;; Remember that the arguments for a `code-c-d' are called "c" (for
;; "code") and "d" (for "directory"),
;;
;;   (code-c-d "foo"  "/tmp/FOO/")
;;   (code-c-d "bar"  "/tmp/FOO/BAR/")
;;   (code-c-d "plic" "/tmp/FOO/BAR/PLIC/")
;;   ;;         ^`c's  ^`d's
;;
;; We can run this to understand the code that the first `code-c-d'
;; above runs,
;;
;;   (find-code-c-d "foo"  "/tmp/FOO/")
;;
;; and then we will see that there are two ways to convert the `c'
;; "foo" to its corresponding `d', "/tmp/FOO/":
;;
;;   ee-foodir
;;   ;; --> "/tmp/FOO/"
;;   (ee-foofile "BAR/PLIC/bletch")
;;   ;; --> "/tmp/FOO/BAR/PLIC/bletch"
;;
;;
;;
;; 3. Shortening filenames
;; =======================
;; Try:
;;
;;   (ee-kl-dir :c "foo")
;;   (ee-kl-dir :c "bar")
;;   (ee-kl-dir :c "plic")
;;   (ee-kl-shortfname :c "foo"  :fname "/tmp/FOO/BAR/PLIC/bletch")
;;   (ee-kl-shortfname :c "bar"  :fname "/tmp/FOO/BAR/PLIC/bletch")
;;   (ee-kl-shortfname :c "plic" :fname "/tmp/FOO/BAR/PLIC/bletch")
;;
;; The functions `ee-kl-dir' and `ee-kl-shortfname' _sort of_ perform
;; the opposite operations of `ee-{c}dir' and `ee-{c}file'. Try also
;; this:
;;
;;   (ee-kl-sexp-klf :c "foo"  :fname "/tmp/FOO/BAR/PLIC/bletch")
;;   (ee-kl-sexp-klf :c "bar"  :fname "/tmp/FOO/BAR/PLIC/bletch")
;;   (ee-kl-sexp-klf :c "plic" :fname "/tmp/FOO/BAR/PLIC/bletch")
;;
;; The three sexps above return these other sexps:
;;
;;   (find-foofile "BAR/PLIC/bletch")
;;   (find-barfile "PLIC/bletch")
;;   (find-plicfile "bletch")
;;
;; that are three different short hyperlinks to the file
;; /tmp/FOO/BAR/PLIC/bletch.
;;
;;
;;
;; 4. Defaults
;; ===========
;; When the functions `ee-kl-dir', `ee-kl-shortname', and
;; `ee-kl-sexp-klf' above are called without keyword arguments they
;; call certain functions to supply default values for those
;; arguments. For example, this call
;;
;;   (ee-kl-sexp-klf)
;;
;; is equivalent to:
;;
;;   (ee-kl-sexp-klf :c (ee-kl-c) :fname (ee-kl-fname))
;;
;; The call to (ee-kl-fname) returns the file name of this file -
;; nothing mysterious here - but the call to (ee-kl-c) returns the
;; value of the variable `ee-preferred-c'.
;;
;;
;;
;; 5. `ee-preferred-c'
;; ===================
;; In short: in the example that we are discussing there are three
;; possible sexp hyperlinks to this file:
;;
;;   /tmp/FOO/BAR/PLIC/bletch
;;
;; namely:
;;
;;   (find-foofile "BAR/PLIC/bletch")
;;   (find-barfile "PLIC/bletch")
;;   (find-plicfile "bletch")
;;
;; Each one is associated to a `c'. If `c' is "foo" we get the first
;; one, if `c' is "bar" we get the second, and if `c' is "plic" we get
;; the third one... and the value of the variavle `ee-preferred-c'
;; determines which is these short hyperlinks should be preferred.
;;
;; Try:
;;
;;   (find-evardescr 'ee-preferred-c)
;;
;; you will get an explanation like this:
;;
;;   ee-preferred-c is a variable defined in ‘eev-kla.el’.
;;
;;   Its value is "eev"
;;   Local in buffer eev-kla.el; global value is nil
;;
;;   See: (find-eev "eev-kla.el")
;;
;;     This variable’s value is directory-local, set by the file
;;     ‘/home/edrx/eev-current/.dir-locals.el’.
;;     This variable is safe as a file local variable if its value
;;     satisfies the predicate ‘stringp’.
;;
;; and if you visit the .dir-locals.el file file mentioned above,
;; with:
;;
;;   (find-eev ".dir-locals.el")
;;
;; you will see that it has a header, and then this:
;;
;;   ;; See: (find-eev "eev-kla.el" "intro")
;;   ;;      (find-enode "Directory Variables")
;;   ;;      (find-enode "Directory Variables" "a subdirectory (a string)")
;;   ;;
;;   (("" . ((nil . ((ee-preferred-c . "eev")))))
;;    )
;;
;;
;;
;; 6. `add-dir-local-variable'
;; ===========================
;; When I started created my own ".dir-locals.el" files I found their
;; syntax very hard to get right... but there's this:
;;
;;   (find-enode "Directory Variables" "M-x add-dir-local-variable")
;;   (find-enode "Directory Variables" "M-x delete-dir-local-variable")
;;   (find-efunctiondescr 'add-dir-local-variable)
;;   (find-efunctiondescr 'delete-dir-local-variable)
;;
;; The manual only explains how to run those functions with `M-x'.
;; People who prefer the run them from sexps can use this example as a
;; starting point:
;;
;;   (mkdir "/tmp/foo/bar/" t)
'    (find-2a nil ' (progn
                     (find-fline "/tmp/foo/bar/.dir-locals.el")
                     (add-dir-local-variable nil 'ee-preferred-c "foo")))
'    (find-2a nil ' (progn
                     (find-fline "/tmp/foo/bar/.dir-locals.el")
                     (add-dir-local-variable nil 'ee-preferred-c "bar")))
'    (find-2a nil ' (progn
                     (find-fline "/tmp/foo/bar/.dir-locals.el")
                     (delete-dir-local-variable nil 'ee-preferred-c)))
;;
;; Note that the three sexps starting with `find-2a's are three lines
;; long each, and they are commented out with a "'" at the beginning
;; of the line.
;;
;;
;;
;; 7. `eekla' and friends
;; ======================
;; At the moment this file implements these commands:
;;
;;   1) `M-x eekla', that <K>ills a <L>ink to an <A>nchor.
;;      See: (find-anchors-intro "3. The preceding tag" "M-1 M-h M-w")
;;           (find-refining-intro "5. Pointing to anchors")
;;
;;         «test»  (to ".test")
;;
;;      To test this, run `M-x eekla' here. It will highlight the
;;      anchor above ("test") for a fraction of a second and will show
;;      this message in the echo area:
;;
;;        Copied to the kill ring: (find-eev "eev-kla.el" "test")
;;
;;   2) `M-x eeklf', that <K>ills a <L>ink to a <F>ile.
;;       To test this, just run `M-x eeklf'. You will see this message
;;       in the echo area:
;;
;;         Copied to the kill ring: (find-eevfile "eev-kla.el")
;;
;;   3) `M-x eeklas'
;;
;;   4) `M-x eeklfs'
;;
;;   5) `M-x ee-preferred-c-show', that shows the current directory
;;      and the value of `ee-preferred-c' at the window at the right.
;;      This is useful to check if the ".dir-locals.el" file was
;;      correctly written. Try:
;;
;;        (ee-preferred-c-show)
;;
;; TODO: `M-x eeklt', that <K>ills a <L>ink made with (<T>o ...);
;; rewrite `ee-preferred-c-show' - I deleted the old version.
;;
;;
;;
;; 8. Aliases
;; ==========
;; TODO: write this.



;; «variables»  (to ".variables")
;;
(defvar ee-preferred-c nil
  "See: (find-eev \"eev-kla.el\")")

(defvar ee-kl-format "%s"
  "See: (find-eev \"eev-kla.el\")")

;;;###autoload
(put   'ee-preferred-c 'safe-local-variable #'stringp)

;;;###autoload
(put   'ee-kl-format 'safe-local-variable #'stringp)

(defun ee-kl-kill (sexp)
  (if (not (stringp sexp))
      (setq sexp (ee-S sexp)))
  (kill-new (format ee-kl-format sexp))
  (message "Copied to the kill ring: %s" sexp))



;; «simple-defaults»  (to ".simple-defaults")
;; "Simple defaults" - the functions that generate sexps, below, call
;; these functions when they don't receive keywords arguments. Tests:
;;   (ee-kl-c)
;;   (ee-kl-fname)
;;   (ee-kl-anchor)
;;   (ee-kl-region)
;;
(defun ee-kl-c ()
  (if (not ee-preferred-c)
      (error "`ee-preferred-c' is nil here!")
    ee-preferred-c))

(defun ee-kl-fname ()
  (or (buffer-file-name) (default-directory)))

(defun ee-kl-anchor ()
  (ee-preceding-tag-flash))

(defun ee-kl-region ()
  ;; (if (not (use-region-p)) (error "The region is not active!"))
  (buffer-substring-no-properties (point) (mark)))


;; «other-defaults»  (to ".other-defaults")
;; "Other defaults" - same as above, but these ones
;; accept keyword arguments. Tests:
;;   (ee-kl-dir)
;;   (ee-kl-shortfname)
;;   (ee-kl-find-c)
;;   (ee-kl-find-cfile)
;;
(cl-defun ee-kl-dir
    (&key (c (ee-kl-c)))
  (symbol-value (intern (format "ee-%sdir" c))))

(cl-defun ee-kl-shortfname
    (&key (c     (ee-kl-c))
	  (fname (ee-kl-fname)))
  (ee-remove-prefix (ee-expand (ee-kl-dir :c c))
		    (ee-expand fname)))

(cl-defun ee-kl-find-c
    (&key (c (ee-kl-c)))
  (intern (format "find-%s" c)))

(cl-defun ee-kl-find-cfile
    (&key (c (ee-kl-c)))
  (intern (format "find-%sfile" c)))


;; «generate-sexps»  (to ".generate-sexps")
;; Functions that generate sexps. Tests:
;;   (ee-kl-sexp-kla)
;;   (ee-kl-sexp-klas :region "foo")
;;   (ee-kl-sexp-klf)
;;   (ee-kl-sexp-klfs :region "foo")
;;
(cl-defun ee-kl-sexp-kla
    (&key (c      (ee-kl-c))
     &key (fname  (ee-kl-fname))
     &key (anchor (ee-kl-anchor)))
  (list (ee-kl-find-c :c c)
	(ee-kl-shortfname :c c :fname fname)
	anchor))

(cl-defun ee-kl-sexp-klas
    (&key (c      (ee-kl-c))
     &key (fname  (ee-kl-fname))
     &key (anchor (ee-kl-anchor))
     &key (region (ee-kl-region)))
  (list (ee-kl-find-c :c c)
	(ee-kl-shortfname :c c :fname fname)
	anchor
	region))

(cl-defun ee-kl-sexp-klf
    (&key (c     (ee-kl-c))
	  (fname (ee-kl-fname)))
  (list (ee-kl-find-cfile :c c)
	(ee-kl-shortfname :c c :fname fname)))

(cl-defun ee-kl-sexp-klfs
    (&key (c      (ee-kl-c))
	  (fname  (ee-kl-fname))
	  (region (ee-kl-region)))
  (list (ee-kl-find-cfile :c c)
	(ee-kl-shortfname :c c :fname fname)
	region))

;; «kill-sexps»  (to ".kill-sexps")
;; Commands that push sexps into the kill ring. Note that
;; they are "(interactive)" and can be invoked with `M-x'.
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

;; (eekla)




;;;            _    _       ____  
;;;   ___  ___| | _| | __ _|___ \ 
;;;  / _ \/ _ \ |/ / |/ _` | __) |
;;; |  __/  __/   <| | (_| |/ __/ 
;;;  \___|\___|_|\_\_|\__,_|_____|
;;;                               
;; «eekla2»  (to ".eekla2")
;; See: (find-kla-test-intro)
;;      (find-kla-test-intro "3. Run some tests")

(defun eekla2 ()
  "Insert a link \"to here\" \"there\" and a link \"to there\" \"here\".
Run `eekla' in this window, and save the result in `kla-here';
then run `eekla' in the next window, and save the result in
`kla-here'; then insert `kla-there' \"here\" and `kla-here'
\"there\"."
  (interactive)
  (let* ((kla-here  (progn (eekla)
			   (car kill-ring)))
         (kla-there (progn (other-window 1)
			   (eekla)
			   (other-window -1)
			   (car kill-ring))))
    (insert kla-there)
    (other-window 1)
    (insert kla-here)
    (other-window -1)))


;;;  ____                       
;;; |  _ \  ___ _ __ ___   ___  
;;; | | | |/ _ \ '_ ` _ \ / _ \ 
;;; | |_| |  __/ | | | | | (_) |
;;; |____/ \___|_| |_| |_|\___/ 
;;;                             
;; «demo»  (to ".demo")
;; See: (find-kla-test-intro)
;;      (find-kla-test-intro "2. Setup for a demo")
;; TODO: rewrite this.

(defun ee-kla-demo-write-file (fname contents)
  "See: (find-kla-test-intro)"
  (write-region contents nil fname))

(defun ee-kla-demo-write-three-files ()
  "See: (find-kla-test-intro)"
  ;;
  (ee-kla-demo-write-file "/tmp/eev-kla-test/dira/foo"
  "This file: /tmp/eev-kla-test/dira/foo
Index:
# «.a1»   (to \"a1\")
# «.a2»   (to \"a2\")\n
Body:
# «a1»    (to \".a1\")\n
# «a2»    (to \".a2\")\n\n")
  ;;
  (ee-kla-demo-write-file "/tmp/eev-kla-test/dirb/bar"
  "This file: /tmp/eev-kla-test/dirb/bar
Index:
-- «.b1»   (to \"b1\")
-- «.b2»   (to \"b2\")\n
Body:
-- «b1»    (to \".b1\")\n
-- «b2»    (to \".b2\")\n\n")
  ;;
  (ee-kla-demo-write-file "/tmp/eev-kla-test/.dir-locals.el"
  ";; This file: /tmp/eev-kla-test/.dir-locals.el
;;
(; (\"dira\" . ((nil . ((ee-preferred-c . \"klata\")))))
 (\"\"     . ((nil . ((ee-preferred-c . \"klat\")))))
 (\"dirb\" . ((nil . ((ee-preferred-c . \"klatb\")))))
 )")
  )




;; «aliases»  (to ".aliases")
;; I use these aliases:
;; (defalias 'kla  'eekla)
;; (defalias 'klas 'eeklas)
;; (defalias 'klf  'eeklf)
;; (defalias 'klfs 'eeklfs)
;; (defalias 'kla2 'eekla2)



(provide 'eev-kla)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
