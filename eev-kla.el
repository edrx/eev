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
;; Version:    20221115
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-kla.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-kla.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;; «.intro»			(to "intro")
;;  «.test»			(to "test")
;;
;; «.variables»			(to "variables")
;; «.ee-kl-format1»		(to "ee-kl-format1")
;; «.ee-kl-kill»		(to "ee-kl-kill")
;; «.ee-kl-format2»		(to "ee-kl-format2")
;; «.ee-kl-insert»		(to "ee-kl-insert")
;; «.best-lrcd»			(to "best-lrcd")
;; «.guess»			(to "guess")
;; «.simple-defaults»		(to "simple-defaults")
;; «.other-defaults»		(to "other-defaults")
;; «.generate-sexps»		(to "generate-sexps")
;; «.kill-sexps»		(to "kill-sexps")
;; «.eekla2»			(to "eekla2")
;; «.demo»			(to "demo")
;; «.aliases»			(to "aliases")




;;; Commentary:

;; WARNING (2022nov15): This is being rewritten!
;; One of my presentations at the EmacsConf2022 will be about
;; eev-kla.el. Its page is:
;;
;;   http://angg.twu.net/emacsconf2022-kla.html
;;
;; Right now - 2022nov15 - I am in the middle of rewriting both and
;; the code and the docs of eev-kla.el almost completely. The docs are
;; being moved to:
;;
;;   (find-kla-intro)
;;
;; And some concepts are changing. In my first versions of eev-kla.el
;; the "preferred `c'" for a file was always obtained by reading the
;; variable `ee-preferred-c', that was usually set using this:
;;
;;   (find-enode "Directory Variables")
;;
;; Then I added a way to guess the best `c' for a filename; then I
;; made guessing the default way, and `ee-preferred-c' secondary; then
;; I rewrote the algorithm for guessing, made it use this,
;;
;;   (find-kla-intro "4. The best `l-r-c-d'")
;;   (find-kla-intro "5. `cl-loop'")
;;
;; and started to move all the docs to `(find-kla-intro)'.


;;
;;   «intro»  (to ".intro")
;; 1. Very short introduction
;; ==========================
;; This:
;;
;;   (find-eev "eev-kla.el" "intro")
;;
;; is a short hyperlink to the anchor "intro" above. The standard way
;; to create a link like that is using `find-here-links', refining,
;; and shrinking, and that way needs many keys. It is explained here:
;;
;;   (find-saving-links-intro "2.3. The base case 3")
;;
;; This file implements another way to generate links like the one
;; in the beginning of this section. Try this:
;;
;;   M-x eekla
;;
;; You will see a message like this one
;;
;;   Copied to the kill ring: (find-eev "eev-kla.el" "intro")
;;
;; in the echo area. Then go to the your notes, paste that link with
;; `C-y', and follow it - it will take you back here.
;;
;;
;;
;; 2. The problem
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
;; 3. Converting `c's to `d's
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
;; 4. Shortening filenames
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
;; 5. Defaults
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
;; 6. `ee-preferred-c'
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
;; the third one... and the value of the variable `ee-preferred-c'
;; determines which is these short hyperlinks should be preferred.
;;
;; Try:
;;
;;   (find-evardescr 'ee-preferred-c)
;;
;; you will get an explanation like this - but the name of the
;; directory will be different:
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
;; and if you visit the .dir-locals.el file mentioned above, with:
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
;; 7. `add-dir-local-variable'
;; ===========================
;; When I started created my own ".dir-locals.el" files - by hand! - I
;; found their syntax very hard to get right... but there's this:
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
;; of the line. Also, note that you may need to save the dir-locals
;; file, and reload the other file, for the changes to take effect.
;;
;;
;;
;; 8. `eekla' and friends
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
;;   3) `M-x eeklas': <K>ill a <L>ink to an <A>nchor and <S>tring.
;;      This is similar to `M-x eekla', but if "refines" the link
;;      generated by `M-x eekla' by adding the region between the
;;      point and the mark. For example, is the region is this "blah"
;;      then `M-x eeklas' with show this message:
;;
;;        Copied to the kill ring: (find-eev "eev-kla.el" "test" "blah")
;;
;;   4) `M-x eeklfs': <K>ill a <L>ink to a <F>ile and <S>string.
;;      If the region is this "bleh" then `M-x eeklfs' will show
;;      this message:
;;
;;        Copied to the kill ring: (find-eevfile "eev-kla.el" "bleh")
;;
;;   5) `M-x eeklt': <K>ills a <L>ink made with (<T>o ...).
;;      To test this, run `M-x eeklt' here. You will see this message
;;      in the echo area:
;;
;;         Copied to the kill ring: (to "test")
;;
;;   6) `M-x ee-kl-insert' and `M-x eekla2', that are explained in the
;;      next section.
;;
;;
;;
;; 9. Aliases
;; ==========
;; The last section of this file - i.e.:
;;
;;   (find-eev "eev-kla.el" "aliases")
;;
;; suggests some aliases - for example, `M-x kla' for `M-x eekla'.
;; From here onwards I will suppose that these aliases are active.
;;
;;
;;
;; 10. Bidirectional links
;; =======================
;; This page
;;
;;    http://angg.twu.net/emacsconf2022-kla.html
;;
;; has one of my submissions to the EmacsConf2022; its title is
;; "Bidirectional links with eev", and that title refers to something
;; that I will show in the last part - probably the last minute - of
;; my video. The idea is that sometimes we have two windows displaying
;; different files, like this:
;;
;;    _____________________
;;   |          |          |
;;   | ;; «a»   | # «b»    |
;;   | _        | _        |
;;   |          |          |
;;   |__________|__________|
;;   |__foo.el__|__bar.py__|
;;
;; In the drawing above the first window is visiting a file called
;; "foo.el" (in Elisp) and the second one is visiting a file called
;; "bar.py" (in Python). In the first one the point is just after an
;; anchor whose tag is "a", and in the second one the point is after
;; an anchor whose tag is "b".
;;
;; We want to create a link from the "a" to the "b", and a link from
;; the "b" to the "a", with few keystrokes. Or, more precisely...
;;
;; An `M-x kla' in the first window generates a link like this,
;;
;;   (find-foo "foo.el" "a")
;;
;; and an `M-x kla' in the second window generates one like this:
;;
;;   (find-bar "bar.py" "b")
;;
;; We want a VERY VERY FAST WAY to put the first link on the second
;; window, and the second link in the first window, to get something
;; like this:
;;    ____________________________________________________________
;;   |                              |                             |
;;   | ;; «a»                       | # «b»                       |
;;   | ;; (find-bar "bar.py" "b")   | # (find-foo "foo.el" "a")   |
;;   | _                            | _                           |
;;   |                              |                             |
;;   |______________________________|_____________________________|
;;   |__foo.el______________________|__bar.py_____________________|
;;
;; we also want these links to be inserted in comments. In Elisp
;; comments start with ";;", and in Python they start with "#".
;;
;; In pseudocode, what we want is roughly this:
;;
;;   1. start at the first window
;;   2. generate a link to the "a" with `M-x kla'
;;   3. switch to the next window
;;   4. insert that link with a comment prefix and a newline
;;   5. generate a link to the "b" with `M-x kla'
;;   6. switch back to the first window
;;   7. insert that link with a comment prefix and a newline
;;
;; The command `M-x kla2' (or `M-x eekla2') does that.
;;
;; The _current implementation_ of `M-x kla2' - that will probably
;; change soon! - is based on the following ideas:
;;
;;   1. Some people prefer to make commands like `M-x kla' kill a sexp
;;      with a newline added at its end. These people can run this
;;
;;        (setq ee-kl-format1 "%s\n")
;; 
;;      to set that preference globally; other people will leave that
;;      variable as `nil'.
;;
;;   2. The comment prefix can only be added later, because it depends
;;      on the file in which the sexp will be inserted. The easiest
;;      way to define which comment prefix to use is to set the
;;      variable `ee-kl-format2' in the local variables section of
;;      that file.
;;
;;   3. The command `M-x kli' - or `ee-kl-insert' - is like `C-y', but
;;      it also adds the comment prefix.
;;
;;   4. The functions `ee-kl-format1' and `ee-kl-format2' - used,
;;      respectively, by `ee-kl-kill' and `ee-kl-insert' - could be
;;      defined like this,
;;
;;        (defun ee-kl-format1 (str)
;;          (format (or ee-kl-format1 "%s") str))
;;        (defun ee-kl-format2 (str)
;;          (format (or ee-kl-format2 "%s") str))
;;
;;      but we want to add a hack to the function `ee-kl-format2'. Its
;;      real definition is this one:
;;
;;        (defun ee-kl-format2 (str)
;;          (format (or ee-kl-format2 (ee-kl-format2-for-mode) "%s") str))
;;
;;      So: when the variable `ee-kl-format2' is nil we run the
;;      function `ee-kl-format2-for-mode' to try to get a default
;;      value for the comment prefix based on the major mode. The
;;      current definition of `ee-kl-format2-for-mode' is very
;;      simplistic, but it is intended to be overriden by the user.
;;
;;   5. My first implementation of `M-x kla2' ran `M-x kla' twice and
;;      `M-x kli' twice. The current implementation uses variants of
;;      `kla' and `kli' that do not change the kill ring and that
;;      always add the newlines - i.e., it ignores the value of the
;;      variable `ee-kl-format1'.
;;
;;
;; 11. Overridable functions
;; =========================
;; On my machine I have some "living fossils" - the ones mentioned
;; in `find-angg-es-links',
;;
;;   (find-angg-es-links)
;;   (find-angg-es-links 2 "living fossil")
;;   (find-eev "eev-tlinks.el" "find-angg-es-links")
;;   (find-eev "eev-tlinks.el" "find-angg-es-links" "living fossil")
;;
;; ...and a few other ones. My trick for making `kla' and `klas'
;; support them is to override `ee-kl-shorterfname', and redefine it
;; in my ~/.emacs by a function that deletes the suffixes.
;;
;; Also, a friend of mine uses Doom Emacs, that uses straight.el, and
;; he told me that in Doom Emacs this
;;
;;   (find-eev "eev-code.el" "code-c-d-s")
;;   (find-eev "eev-code.el" "code-c-d-s" "ee-eev-source-directory")
;;
;; points to a directory that only has .el files, and all these ".el"s
;; are symlinks. This breaks `ee-kl-shortfname', and I'm experimenting
;; with variants of `ee-kl-shortfname' - that are hacks, and that have
;; to be put in his ~/.emacs to override the original function - that
;; contain code to handle these symlinks correctly.
;;
;;
;; 12. Please test!
;; ================
;; ..and get in touch, either through the mailing list,
;;
;;   https://lists.gnu.org/archive/html/eev/
;;   https://lists.nongnu.org/mailman/listinfo/eev
;;
;; or by one of the ways listed here:
;;
;;   http://angg.twu.net/contact.html
;;
;; Thanks! =)




;; «variables»  (to ".variables")
;; See: (find-eev "eev-kla.el" "intro" "ee-preferred-c")
;;      (find-eev "eev-kla.el" "intro" "ee-kl-format1")
;;      (find-eev "eev-kla.el" "intro" "ee-kl-format2")
;;      (find-eev "eev-tlinks.el" "ee-copy-rest" "eeflash-copy")
;;
(defvar ee-preferred-c nil
  "See: (find-eev \"eev-kla.el\")")

(defvar ee-preferred-c-guess t
  "See: (find-eev \"eev-kla.el\")")

(defvar ee-kl-format1 nil
  "See: (find-eev \"eev-kla.el\")")

(defvar ee-kl-format2 nil
  "See: (find-eev \"eev-kla.el\")")

(defvar ee-kla2-flash-spec '(highlight 2.0))

;;;###autoload
(put 'ee-preferred-c 'safe-local-variable 'string-or-null-p)

;;;###autoload
(put 'ee-kl-format1 'safe-local-variable 'stringp)

;;;###autoload
(put 'ee-kl-insert2 'safe-local-variable 'stringp)

;; «ee-kl-format1»  (to ".ee-kl-format1")
;; «ee-kl-kill»  (to ".ee-kl-kill")
;;
(defun ee-kl-sexp-to-string (str-or-sexp)
  (if (stringp str-or-sexp)
      str-or-sexp
    (ee-S str-or-sexp)))

(defun ee-kl-format1 (str)
  (format (or ee-kl-format1 "%s") str))

(defun ee-kl-kill (str-or-sexp)
  (let ((str (ee-kl-sexp-to-string str-or-sexp)))
    (kill-new (ee-kl-format1 str))
    (message "Copied to the kill ring: %s" str)))

;; «ee-kl-format2»  (to ".ee-kl-format2")
;; «ee-kl-insert»  (to ".ee-kl-insert")
;; Test: (ee-kl-format2-for-mode)
;;
(defun ee-kl-format2-for-mode (&optional mode)
  (let ((plist '(emacs-lisp-mode ";; %s"
		 haskell-mode    "-- %s"
		 lua-mode        "-- %s"
		 python-mode     "# %s"
		 agda2-mode      "-- %s"
		 latex-mode      "%% %s")))
    (plist-get plist (or mode major-mode))))

(defun ee-kl-format2 (str)
  (format (or ee-kl-format2 (ee-kl-format2-for-mode) "%s") str))

;; Used by `M-x kli'
(defun ee-kl-insert (&optional str)
  (interactive)
  (let* ((str1 (or str (car kill-ring)))
	 (str2 (ee-kl-format2 str1)))
    (insert str2)))




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
;;   (find-kla-intro "4. The best `l-r-c-d'")

(defun ee-kl-expand (fname)
  (ee-expand fname))

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

(defun ee-kl-lrcds (fname)
  "Return all the `c-d's in (ee-kl-cds) that match FNAME.
Each matching `c-d' is converted to an `l-r-c-d'."
  (cl-loop for (c d) in (ee-kl-cds)
	   if (ee-kl-prefixp d fname)
	   collect (let* ((r (ee-kl-prefixp d fname))
			  (l (length r)))
		     (list l r c d))))

(defun ee-kl-lrcd (fname)
  "Return the best lrcd in (ee-kl-lrcds FNAME).
If (ee-kl-lrcds FNAME) doesn't return any matching `lrcd's, return nil."
  (let* ((lrcds (ee-kl-lrcds fname))
	 (l< (lambda (lrcd1 lrcd2) (< (car lrcd1) (car lrcd2))))
	 (lrcds-sorted (sort lrcds l<)))
    (car lrcds-sorted)))




;;;   ____                     
;;;  / ___|_   _  ___  ___ ___ 
;;; | |  _| | | |/ _ \/ __/ __|
;;; | |_| | |_| |  __/\__ \__ \
;;;  \____|\__,_|\___||___/___/
;;;                            
;; «guess»  (to ".guess")
;; This is an obscure hack that is only run when the variable
;; `ee-preferred-c-guess' is non-nil. Here's how it works...
;;
;; When the variable `ee-preferred-c' is nil, we can try to guess a
;; good "c" for the current directory by examining the entries in
;; `ee-code-c-d-pairs', filtering the pairs - the "c-d"s - that point
;; to this directory or to one of its parents/ancestors, then
;; filtering that list to pick up the entries that point closer to
;; where we are, and then choosing one of those entries, and returning
;; its "c". We always choose the first of these entries, but that's
;; not always the best choice... (TODO: explain this!)
;;
;; Tests:
;;                       (find-efile "subr.el")
;;                         (ee-efile "subr.el")
;;      (ee-kl-all-eds-for (ee-efile "subr.el"))
;;      (ee-kl-best-ed-for (ee-efile "subr.el"))
;;      (ee-kl-best-cs-for (ee-efile "subr.el"))
;; (find-code-c-d-filter-2 (ee-efile "subr.el") '(list c d ed))
;;
;; For more on `ee-code-c-d-pairs', see:
;;   (find-eev "eev-elinks.el" "ee-code-c-d-filter")

(defun ee-kl-all-eds-for (fname)
  (ee-code-c-d-filter-2 fname 'ed))

(defun ee-kl-best-ed-for (fname)
  (car (sort (ee-kl-all-eds-for fname) 'string>)))

(defun ee-kl-best-cs-for (fname)
  (let ((best-ed (ee-kl-best-ed-for fname)))
    (if best-ed
	(cl-loop for c-d in ee-code-c-d-pairs
		 as c = (car c-d)
		 as d = (cadr c-d)
		 as ed = (ee-expand d)
		 if (string= ed best-ed)
		 collect c))))

(defun ee-preferred-c-here-without-guess ()
  (if ee-preferred-c
      ee-preferred-c
    (error "`ee-preferred-c' is nil here!")))

(defun ee-preferred-c-here-with-guess ()
  (if ee-preferred-c
      ee-preferred-c
    (let ((pref-c (car (ee-kl-best-cs-for (ee-kl-fname)))))
      (if pref-c
	  pref-c
      (error "`ee-preferred-c' is nil here, and the guesses failed!")))))

(defun ee-preferred-c-here ()
  (if ee-preferred-c-guess
      (ee-preferred-c-here-with-guess)
    (ee-preferred-c-here-without-guess)))


;;;  ____  _                 _            _       __             _ _       
;;; / ___|(_)_ __ ___  _ __ | | ___    __| | ___ / _| __ _ _   _| | |_ ___ 
;;; \___ \| | '_ ` _ \| '_ \| |/ _ \  / _` |/ _ \ |_ / _` | | | | | __/ __|
;;;  ___) | | | | | | | |_) | |  __/ | (_| |  __/  _| (_| | |_| | | |_\__ \
;;; |____/|_|_| |_| |_| .__/|_|\___|  \__,_|\___|_|  \__,_|\__,_|_|\__|___/
;;;                   |_|                                                  
;;
;; «simple-defaults»  (to ".simple-defaults")
;; "Simple defaults" - the functions that generate sexps, below, call
;; these functions when they don't receive keywords arguments. Tests:
;;   (ee-kl-c)
;;   (ee-kl-fname)
;;   (ee-kl-anchor)
;;   (ee-kl-region)
;;
(defun ee-kl-c ()
  (ee-preferred-c-here))

(defun ee-kl-fname ()
  (or (buffer-file-name) default-directory))

(defun ee-kl-anchor ()
  (ee-preceding-tag-flash))

(defun ee-kl-region ()
  (buffer-substring-no-properties (point) (mark)))


;;;   ___  _   _                     _       __             _ _       
;;;  / _ \| |_| |__   ___ _ __    __| | ___ / _| __ _ _   _| | |_ ___ 
;;; | | | | __| '_ \ / _ \ '__|  / _` |/ _ \ |_ / _` | | | | | __/ __|
;;; | |_| | |_| | | |  __/ |    | (_| |  __/  _| (_| | |_| | | |_\__ \
;;;  \___/ \__|_| |_|\___|_|     \__,_|\___|_|  \__,_|\__,_|_|\__|___/
;;;                                                                   
;; «other-defaults»  (to ".other-defaults")
;; "Other defaults" - same as above, but these ones
;; accept keyword arguments. Tests:
;;   (ee-kl-dir)
;;   (ee-kl-shortfname)
;;   (ee-kl-shorterfname)
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

(cl-defun ee-kl-shorterfname
    (&key (c     (ee-kl-c))
	  (fname (ee-kl-fname)))
  (ee-kl-shorterfname :c c :fname fname))

(cl-defun ee-kl-find-c
    (&key (c (ee-kl-c)))
  (intern (format "find-%s" c)))

(cl-defun ee-kl-find-cfile
    (&key (c (ee-kl-c)))
  (intern (format "find-%sfile" c)))


;;;  ____                      
;;; / ___|  _____  ___ __  ___ 
;;; \___ \ / _ \ \/ / '_ \/ __|
;;;  ___) |  __/>  <| |_) \__ \
;;; |____/ \___/_/\_\ .__/|___/
;;;                 |_|        
;;
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
	(ee-kl-shorterfname :c c :fname fname)
	anchor))

(cl-defun ee-kl-sexp-klas
    (&key (c      (ee-kl-c))
     &key (fname  (ee-kl-fname))
     &key (anchor (ee-kl-anchor))
     &key (region (ee-kl-region)))
  (list (ee-kl-find-c :c c)
	(ee-kl-shorterfname :c c :fname fname)
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

(cl-defun ee-kl-sexp-klt
    (&key (anchor (ee-kl-anchor)))
  (list 'to anchor))


;;;  _  ___ _ _     
;;; | |/ (_) | |___ 
;;; | ' /| | | / __|
;;; | . \| | | \__ \
;;; |_|\_\_|_|_|___/
;;;                 
;; «kill-sexps»  (to ".kill-sexps")
;; Commands that push sexps into the kill ring. Note that
;; they can be invoked with `M-x'.
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




;;;            _    _       ____  
;;;   ___  ___| | _| | __ _|___ \ 
;;;  / _ \/ _ \ |/ / |/ _` | __) |
;;; |  __/  __/   <| | (_| |/ __/ 
;;;  \___|\___|_|\_\_|\__,_|_____|
;;;                               
;; «eekla2»  (to ".eekla2")
;; See: (find-eev "eev-kla.el" "intro" "10. Bidirectional links")
;;
(defun ee-kla2-bol ()
  (when (not (= (ee-bol) (point)))	; when not at bol
    (move-beginning-of-line 2))		; do <down> C-a
  (point))

(defun ee-kla2-flash (pos1 pos2)
  (eeflash pos1 (point) ee-kla2-flash-spec))

(defun ee-kla2-insert (sexp)
  (let* ((str1 (format "%s\n" (ee-S sexp)))
	 (str2 (ee-kl-format2 str1))
	 (pos1 (ee-kla2-bol)))
    (insert str2)
    (ee-kla2-flash pos1 (point))))

(defun eekla2 ()
  "Insert a link \"to here\" \"there\" and a link \"to there\" \"here\"."
  (interactive)
  (let* ((sexp1 (ee-kl-sexp-kla))
	 (sexp2 (prog2 (other-window 1)
		    (ee-kl-sexp-kla)
		  (other-window -1))))
    (ee-kla2-insert sexp2)
    (other-window 1)
    (ee-kla2-insert sexp1)
    (other-window -1)))


;;;  ____                       
;;; |  _ \  ___ _ __ ___   ___  
;;; | | | |/ _ \ '_ ` _ \ / _ \ 
;;; | |_| |  __/ | | | | | (_) |
;;; |____/ \___|_| |_| |_|\___/ 
;;;                             
;; «demo»  (to ".demo")
;; See: (find-kla-intro)
;;      (find-kla-intro "2. Setup for a demo")
;; TODO: How obsolete is this? Check and rewrite!

(defun ee-kla-demo-write-file (fname contents)
  "See: (find-kla-intro)"
  (write-region contents nil fname))

(defun ee-kla-demo-write-three-files ()
  "See: (find-kla-intro)"
  ;;
  (ee-kla-demo-write-file "/tmp/eev-kla/dira/foo"
  "This file: /tmp/eev-kla/dira/foo
Index:
# «.a1»   (to \"a1\")
# «.a2»   (to \"a2\")\n
Body:
# «a1»    (to \".a1\")\n
# «a2»    (to \".a2\")\n\n")
  ;;
  (ee-kla-demo-write-file "/tmp/eev-kla/dirb/bar"
  "This file: /tmp/eev-kla/dirb/bar
Index:
-- «.b1»   (to \"b1\")
-- «.b2»   (to \"b2\")\n
Body:
-- «b1»    (to \".b1\")\n
-- «b2»    (to \".b2\")\n\n")
  ;;
  (ee-kla-demo-write-file "/tmp/eev-kla/.dir-locals.el"
  ";; This file: /tmp/eev-kla/.dir-locals.el
;;
(; (\"dira\" . ((nil . ((ee-preferred-c . \"klata\")))))
 (\"\"     . ((nil . ((ee-preferred-c . \"klat\")))))
 (\"dirb\" . ((nil . ((ee-preferred-c . \"klatb\")))))
 )")
  )



;;;     _    _ _                     
;;;    / \  | (_) __ _ ___  ___  ___ 
;;;   / _ \ | | |/ _` / __|/ _ \/ __|
;;;  / ___ \| | | (_| \__ \  __/\__ \
;;; /_/   \_\_|_|\__,_|___/\___||___/
;;;                                  
;; «aliases»  (to ".aliases")
;; I use these aliases:
;; (defalias 'kla  'eekla)
;; (defalias 'klas 'eeklas)
;; (defalias 'klf  'eeklf)
;; (defalias 'klfs 'eeklfs)
;; (defalias 'klt  'eeklt)
;; (defalias 'kli  'ee-kl-insert)
;; (defalias 'kla2 'eekla2)

(provide 'eev-kla)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
