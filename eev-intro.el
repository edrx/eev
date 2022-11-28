;;; eev-intro.el --- sandboxed tutorials for eev, like (find-eev-quick-intro)  -*- lexical-binding: nil; -*-

;; Copyright (C) 2013-2022 Free Software Foundation, Inc.
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
;; Version:    20221128
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-intro.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-intro.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:
;;
;; Sometime around 2015 I realized that I could make a sandboxed
;; tutorial - (find-eev-quick-intro) - that would be THE starting
;; point of eev. It would be:
;;
;;   1) an interactive tutorial for beginners,
;;   2) the thing that emacs shows when it starts,
;;   3) a tutorial for advanced users,
;;   4) an index to the other sandboxed tutorials, that were mostly
;;      quite technical (and incomplete),
;;   5) a guide to the source files.
;;
;; In 2019 the work to make (find-eev-quick-intro) central was
;; completed. The only autoloaded function of eev is `eev-beginner',
;; that loads all the main modules of eev, turns eev-mode on, and runs
;; (find-eev-quick-intro). See:
;;
;;   (find-eevfile "eev-beginner.el" "defun eev-beginner ")

;; To use this, simply execute any of the sexps below:
;;   (find-eev-quick-intro)
;;   (find-eev-intro)


;; Quick index:
;; «.find-intro-dual»			(to "find-intro-dual")
;; «.find-eintro»			(to "find-eintro")
;;
;; «.find-eev-quick-intro»		(to "find-eev-quick-intro")
;; «.find-emacs-keys-intro»		(to "find-emacs-keys-intro")
;; «.find-eev-install-intro»		(to "find-eev-install-intro")
;; «.find-eev-intro»			(to "find-eev-intro")
;; «.find-here-links-intro»		(to "find-here-links-intro")
;; «.find-refining-intro»		(to "find-refining-intro")
;; «.find-saving-links-intro»		(to "find-saving-links-intro")
;;
;; «.find-eval-intro»			(to "find-eval-intro")
;; «.find-links-conv-intro»		(to "find-links-conv-intro")
;; «.find-links-intro»			(to "find-links-intro")
;; «.find-eepitch-intro»		(to "find-eepitch-intro")
;; «.find-wrap-intro»			(to "find-wrap-intro")
;; «.find-eejump-intro»			(to "find-eejump-intro")
;; «.find-anchors-intro»		(to "find-anchors-intro")
;; «.find-code-c-d-intro»		(to "find-code-c-d-intro")
;; «.find-pdf-like-intro»		(to "find-pdf-like-intro")
;; «.find-brxxx-intro»			(to "find-brxxx-intro")
;; «.find-psne-intro»			(to "find-psne-intro")
;;
;; «.find-audiovideo-intro»		(to "find-audiovideo-intro")
;; «.find-multiwindow-intro»		(to "find-multiwindow-intro")
;; «.find-rcirc-intro»			(to "find-rcirc-intro")
;; «.find-templates-intro»		(to "find-templates-intro")
;; «.find-prepared-intro»		(to "find-prepared-intro")
;; «.find-bounded-intro»		(to "find-bounded-intro")
;; «.find-channels-intro»		(to "find-channels-intro")
;; «.find-videos-intro»			(to "find-videos-intro")
;; «.find-video-links-intro»		(to "find-video-links-intro")

;; «.find-defun-intro»			(to "find-defun-intro")
;; «.find-emacs-intro»			(to "find-emacs-intro")
;; «.find-org-intro»			(to "find-org-intro")
;; «.find-escripts-intro»		(to "find-escripts-intro")
;; «.find-git-intro»			(to "find-git-intro")
;; «.find-windows-beginner-intro»	(to "find-windows-beginner-intro")
;; «.find-eev-exercises-intro»		(to "find-eev-exercises-intro")
;; «.find-kla-intro»			(to "find-kla-intro")
;; «.find-edit-index-intro»		(to "find-edit-index-intro")

;; Videos:
;; «.find-three-main-keys-intro»	(to "find-three-main-keys-intro")
;;
;; «.find-elisp-intro»			(to "find-elisp-intro")
;; «.find-lexical-intro»		(to "find-lexical-intro")


;; See: (find-anchors-intro)


;; For: (find-efile "info.el" "defface info-title-1")
(require 'info)



;; «find-intro-dual» (to ".find-intro-dual")
;; Below is a hack that I (edrx) use to edit these intros.
;; Note that it is sort of commented out.
;;
;; If we're editing the defun for `find-foo-intro' at a line that
;; contains "<a certain string>" then running `M-x fd' there creates a
;; window setup like this,
;;
;;    _______________________________________
;;   |                |                      |
;;   |    source:     |        intro:        |
;;   |  eev-intro.el  |  *(find-foo-intro)*  |
;;   |________________|______________________|
;;
;; with both windows centered on the lines with "<a certain string>".
;; If we're editing the buffer "*(find-foo-intro)*" and we're on a
;; line containing "<a certain string>" then running `M-x fd' also
;; creates the same window setup, with the source at the left and the
;; intro buffer at the right.

(defun find-intro-dual-define ()
  "Define `find-intro-dual' and `fd', that are hacks for editing `find-xxx-intro's."
  (interactive)
  ;; The hackish functions begin here and end at the line with a single ")".

;; Test: (ee-sexp-at "2)")
;;             (+  1  2)
(defun ee-sexp-at (re)
  (save-excursion (re-search-forward re) (ee-last-sexp)))
(setq ee-intro-sexp-end-re "\\(rest\\|pos-spec-list\\))))")
(defun ee-intro-sexp-here ()
  "Go to the end of the defun around point and `read' it.
Only works for \"(defun find-xxx-intro ...)s\".
Returns a list like this: (defun find-xxx-intro ...)."
  (read (ee-sexp-at ee-intro-sexp-end-re)))

(defun ee-bad-line (str) (string-match "[\\\"]" str))
(defun ee-this-line0 ()
  (buffer-substring-no-properties (ee-bol) (ee-eol)))
(defun ee-this-line ()
  (let ((line (ee-this-line0)))
    (if (ee-bad-line line)
	(error "Current line contains evil characters")
      line)))
(defun ee-intro-sourcep ()
  (equal (buffer-name) "eev-intro.el"))

(defun find-intro-intro ()
"If we're in the defun for `find-foo-intro' run (find-foo-intro (ee-this-line))."
  (interactive)
  (funcall (cadr (ee-intro-sexp-here)) (ee-this-line)))

(defun find-intro-source ()
"If we're in a buffer \"*(find-foo-intro)*\" go to the source for `find-foo-intro'.
Actually go to: (find-eev \"eev-intro.el\" \"find-foo-intro\" (ee-last-kill))."
  (interactive)
  (find-eev "eev-intro.el" (format "find-%s-intro" (ee-intro-bufferp))
	    (ee-this-line)))

;; (defun find-2a (a b) (find-wset "13_o_o" a b))
;; (defun find-2b (a b) (find-wset "13_o_"  a b))
(defun find-c2a (a b)   (find-wset "13_co_co" a b))
(defun find-c2b (a b)   (find-wset "13_co_c"  a b))

(defun find-intro-dual ()
  (interactive)
  (if (ee-intro-sourcep)
      (progn (eval (ee-intro-sexp-here))
	     (find-c2b nil '(find-intro-intro)))
    (find-c2a '(find-intro-source) nil)))

(defalias 'fs 'find-intro-source)
(defalias 'fi 'find-intro-intro)
(defalias 'fd 'find-intro-dual)

(message "`find-intro-dual' and `fd' are now defined.")

  ;; end of `find-intro-dual-define'
  )



;;;   __             _   _  __
;;;  / _| ___  _ __ | |_(_)/ _|_   _
;;; | |_ / _ \| '_ \| __| | |_| | | |
;;; |  _| (_) | | | | |_| |  _| |_| |
;;; |_|  \___/|_| |_|\__|_|_|  \__, |
;;;                            |___/
;;
;; «find-eintro» (to ".find-eintro")
;; 2019feb03: experimental. This is like `find-estring' but it runs
;; `ee-intro-fontify-maybe' to optionally fontify the titles;
;; `ee-intro-fontify-maybe' is similar to `Info-fontify-node'.
;; Compare:
;;   (find-fline "/usr/share/info/emacs-24/elisp.info.gz" "****")
;;   (find-elnode "Top" "Emacs Lisp\n**********")

;; This is practically a copy of:
;; (find-efile "info.el" "defun Info-fontify-node" ";; Fontify titles")
(defun ee-intro-fontify ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (re-search-forward
		 "\n\\([^ \t\n].+\\)\n\\(\\*\\*+\\|==+\\|--+\\|\\.\\.+\\)$"
		 nil t)
		;; Only consider it as an underlined title if the ASCII
		;; underline has the same size as the text.  A typical
		;; counter example is when a continuation "..." is alone
		;; on a line.
		(= (string-width (match-string 1))
		   (string-width (match-string 2))))
      (let* ((c (preceding-char))
	     (face (ee-intro-face c)))
	(put-text-property (match-beginning 1) (match-end 1)
			   'face face))
      ;; This is a serious problem for trying to handle multiple
      ;; frame types at once.  We want this text to be invisible
      ;; on frames that can display the font above.
      (when (memq (framep (selected-frame)) '(x pc w32 ns))
	(add-text-properties
	 (1- (match-beginning 2)) (match-end 2)
	 '(invisible t front-sticky nil rear-nonsticky t))))))

;; (find-elnode "Overlay Properties" "list of faces")
;; (find-efaces "info-title-1")
;; (find-efaces "underline")
(defun ee-intro-face (c)
  (cond ((= c ?*) '(info-title-1 underline))
	((= c ?=) '(info-title-2 underline))
	((= c ?-) '(info-title-3 underline))
	(t        'info-title-3)))

(defun ee-intro-fontify-maybe ())
(defun ee-intro-fontify-maybe () (ee-intro-fontify))

(defun find-eintro (bigstr &rest pos-spec-list)
  "Like `find-estring', but runs `ee-intro-fontify-maybe'."
  (find-estring bigstr)
  (ee-intro-fontify-maybe)
  (apply 'ee-goto-position pos-spec-list))

(defun find-eintro-latin1 (bigstr &rest pos-spec-list)
  "Like `find-eintro', but handles some strange chars in BIGSTR."
  (apply `find-eintro (ee-tolatin1 bigstr) pos-spec-list))




;;;                                   _      _         _       _
;;;   ___  _____   __      __ _ _   _(_) ___| | __    (_)_ __ | |_ _ __ ___
;;;  / _ \/ _ \ \ / /____ / _` | | | | |/ __| |/ /____| | '_ \| __| '__/ _ \
;;; |  __/  __/\ V /_____| (_| | |_| | | (__|   <_____| | | | | |_| | | (_) |
;;;  \___|\___| \_/       \__, |\__,_|_|\___|_|\_\    |_|_| |_|\__|_|  \___/
;;;                          |_|
;;
;; «find-eev-quick-intro» (to ".find-eev-quick-intro")
;; (find-intro-links "eev-quick")

(defun find-eev-quick-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-eev-quick-intro)*"))
    (apply 'find-eintro-latin1 "\
\(Re)generate: (find-eev-quick-intro)
Source code:  (find-efunction 'find-eev-quick-intro)
More intros:  (find-emacs-keys-intro)
              (find-eev-intro)
              (find-here-links-intro)
              (find-refining-intro)
              (find-eepitch-intro)
              (find-pdf-like-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.
The quickest way to open or recreate this is with `M-5 M-j'.


This is a tutorial for real beginners.
It supposes that you have Emacs installed.
For more material on eev, see:

  http://angg.twu.net/#eev




1. Installing eev
=================
The easiest way to install eev is with `M-x list-packages', as
explained in this video:

  http://angg.twu.net/2020-list-packages-eev-nav.html
  http://angg.twu.net/eev-videos/M-x-list-packages-eev-nav.mp4
  http://www.youtube.com/watch?v=kxBjiUo88_U

The three links in the \"Video links:\" block below

  [Video links:]
    (find-eevnavvideo \"0:31\" \"0.1. M-x package-initialize\")
    (find-eevnavvideo \"0:39\" \"0.2. M-x list-packages\")
    (find-eevnavvideo \"2:30\" \"0.3. M-x eev-beginner\")

point to positions in that video; to learn how to use links like
those, visit this URL:

  http://angg.twu.net/eev-intros/find-video-links-intro.html
                                (find-video-links-intro)

Installing eev does NOT activate eev-mode. To activate eev-mode
and open this tutorial, run `M-x eev-beginner'.

For other ways to install eev see this other tutorial, especially
its section 5.1:

  http://angg.twu.net/eev-intros/find-eev-install-intro.html#5.1
  http://angg.twu.net/eev-intros/find-eev-install-intro.html
  (find-eev-install-intro \"5.1. Using the tarball\")
  (find-eev-install-intro)

TIP FOR BEGINNERS: if you are a real beginner with, say, less
than 10 minutes of experience using Emacs, then you will probably
be in a stage in which you often get stuck in the middle of
something that you don't know how to leave - for example, you may
be in the middle of a \"complex command\", and Emacs may be
waiting for the keys that would complete the command. The easiest
way to handle that is to leave Emacs and then start it again, and
after starting it again you will need to type `M-x eev-beginner'
to activate eev again. For an explanation of this in video, see:

  [Video links:]
    (find-eevnavvideo \"3:46\" \"0.4. for the beginners: quitting and restarting\")
    (find-eevnavvideo \"4:06\"   \"the part of the sequence of keys\")
    (find-eevnavvideo \"4:21\"   \"go to the file menu, click quit\")
    (find-eevnavvideo \"4:46\"   \"enter emacs again, type M-x eev-beginner\")

Eventually you will learn how to get out of everything and how to undo
almost anything, _BUT THAT WILL NOT HAPPEN IN THE FIRST TEN MINUTES_.
This tutorial is intended to make you learn the most essential things
in the first ten minutes - including how to navigate in Emacs's
manuals.





2. Evaluating Lisp
==================
The most important idea in Emacs is that Lisp code can appear
anywhere, and you can evaluate a Lisp expression (a \"sexp\") by
placing the cursor (the \"point\") just after it and typing `C-x
C-e'; the result is then displayed in the echo area.

Note: `C-e' means control-E, `M-e' means alt-e, `M-E' means
alt-shift-e. If you have Caps Lock on then Emacs will receive an `M-E'
if you type alt-e, and `M-e' if you type alt-shift-e. Hint: avoid Caps
Lock!

You can try `C-x C-e' in the line below, with the point in the three
different indicated positions - you should get different results...

  (+ (* 2 3) (* 4 5))
            ^       ^^
            |       | \\
            6      20  26

...but `C-x C-e' is not beginner-friendly, and it even enters a
debugger that is hard to leave if it finds errors, so let's see
something better.

When you type `M-e' emacs moves the point to the end of the
current line, then runs a variant of `C-x C-e'. Try this on each
line of the block below:

  (+ (* 2 3)
     (* 4 5)
     )

`M-e' accepts several different numeric prefixes that alter its
behavior. We are only interested in one of them now - `M-0 M-e'
highlights the sexp for a fraction of a second instead of executing it.
Try it above.

In some rare occasions we might want to run something like `M-e'
but without moving to the end of the line first. Eev-mode
implements a key binding for that: `M-E' (meta-shift-e). As an
exercise, try to use `M-0 M-E' at several positions below, to
highlight the subsexps `(* 2 3)', `(* 4 5)', and `4'.

  (+ (* 2 3) (* 4 5))




3. Elisp hyperlinks
===================
Each one of the sexps below makes Emacs \"go somewhere\" if you execute
it with `M-e'. Executing sexps like those - we will call them \"elisp
hyperlinks\" - is like following a hyperlink in a browser.

In a browser you can \"go back\" after following a hyperlink because the
previous page is kept in the memory somehow. In Emacs+eev the easiest
way to \"go back\" is with `M-k', which runs a function called
`ee-kill-this-buffer'. If you follow one of the links below with
`M-e', it creates a new buffer and displays it. If you then type `M-k'
this new buffer is killed, and Emacs displays the buffer that was just
below it, which is this tutorial... try it! Here are some nice elisp
hyperlinks:

  (find-fline \"~/\")
  (find-efunctiondescr 'find-file)
  (find-eval-intro \"5. Going back\")
  (find-fline \"/tmp/\")
  (find-man \"date\")
  (find-sh  \"date\")
  (find-sh  \"date; sleep 1; date\")

Not all elisp hyperlinks \"go somewhere\"; some are like buttons that
perform an action, like the one below, that acts as if the user had
pressed a series of keys,

  (eek \"<down> C-a H E L L O ! <up> C-e\")

and some display their output in the echo area:

  (find-sh0 \"date\")

The following elisp hyperlinks may or may not work - try them too, but
be aware that they may show errors instead of opening a new buffer.
The first two of them open a page - actually a section, whose short
title is \"Lisp Eval\" - from the main Emacs manual. The third one
opens the file with the source code (in Lisp) for the function
`find-file'. The fourth one opens a directory that contains a part of
the source code of Emacs.

  (find-node \"(emacs)Lisp Eval\")
  (find-enode       \"Lisp Eval\")
  (find-efunction 'find-file)
  (find-efile \"\")

If they don't work that means that you don't have the Emacs manuals,
or the elisp source files, installed. The names for the packages which
have those things vary from one GNU/Linux distro to another. On Debian
something like

  sudo apt-get install emacs-el
  sudo apt-get install emacs-common-non-dfsg

may work - but for \"...-non-dfsg\" packages may need you to
enable access to the \"non-free\" repository... ask for help if
you need!

An important difference between elisp hyperlinks and browser
hyperlinks is discussed here:

  (find-links-conv-intro \"1. Security vs. transparency\")




3.1. Non-elisp hyperlinks
-------------------------
Emacs has ways to follow URLs, but the keys for that are totally
different from the ones for elisp hyperlinks. You can follow the URL
below by putting the point on it and typing `M-x browse-url':

  http://www.lua.org/start.html

This will make emacs invoke the default browser on that URL. See:

  (find-enode \"Browse-URL\")

Eev defines several functions similar to `browse-url'. These elisp
hyperlinks

  (find-firefox      \"http://www.lua.org/start.html\")
  (find-googlechrome \"http://www.lua.org/start.html\")

invoke \"firefox\" and \"google-chrome\" respectively on the given URL;
note that the \"firefox\" in a Debian-based system is usually a free
derivative of Firefox, and that \"google-chrome\" does not come
installed by default because it is \"gratis\" but not free. Also,

  M-x brff   -- runs `find-firefox' on the URL at point,
  M-x brg    -- runs `find-googlechrome' on the URL at point.

For more on the \"brxxx functions\" of eev, see:

  (find-brxxx-intro)





4. Creating Elisp Hyperlinks
============================
You can write elisp hyperlinks by hand, but that is hard. It is better
to generate hyperlinks automatically and then use cut and paste.

Eev has several functions that generate \"elisp hyperlinks\" buffers.
For example,

  (find-efunction-links 'find-file)

creates this buffer, and switches to it:
   ____________________________________________________________________
  |# (find-efunction-links 'find-file)                                 |
  |# (eek \"M-h M-f  find-file\")                                        |
  |# (find-eev-quick-intro \"4.2. `find-ekey-links' and  friends\")      |
  |                                                                    |
  |# (find-efunctiondescr 'find-file)                                  |
  |# (find-efunction 'find-file)                                       |
  |# (find-efunctionpp 'find-file)                                     |
  |# (find-efunctiond 'find-file)                                      |
  |                                                                    |
  |# (Info-goto-emacs-command-node 'find-file)                         |
  |# (find-enode \"Command Index\" \"* find-file:\")                       |
  |# (find-elnode \"Index\" \"* find-file:\")                              |
  |                                                                    |
  |# (where-is 'find-file)                                             |
  |# (symbol-file 'find-file 'defun)                                   |
  |# (find-fline (symbol-file 'find-file 'defun))                      |
  |# (find-estring (documentation 'find-file))                         |
  |# (find-estring (documentation 'find-file t))                       |
  |# (describe-function 'find-file)                                    |
  |                                                                    |
  |                                                                    |
  | -:**-  *Elisp hyperlinks*   All L1    (Fundamental eev)  ----------|
  |____________________________________________________________________|


[Video links:]
  (find-eev2020video \"29:39\" \"2. A tale of two `describe-key's\")
  (find-eev2020video \"31:31\"   \"the problem with the standard `describe-key'\")
  (find-eev2020video \"35:07\"   \"My variant: `find-ekey-links'\")
  (find-eev2020video \"37:00\"   \"how `find-ekey-links' generates its links\")
  (find-eev2020video \"37:14\"   \"hacker-friendly in the way that I wanted\")






4.1. `find-here-links'
----------------------
One standard way of using eev is:

  a) we keep our current notes in a a file - for example, \"~/TODO\"
  b) these notes are an \"executable log\" of what we did, including:
     c) hyperlinks to things we saw or visited
     d) commands issued to shells or shell-like programs (see sec. 6)

The quickest way of generating hyperlinks for (c) is with `M-h M-h'
\(`find-here-links'). When we type `M-h M-h' eev tries to generate an
elisp hyperlinks buffer containing some hyperlinks to \"here\" - and how
it does that depends on the major mode and on the name of the current
buffer. For example, typing `M-h M-h' here generates:

   ___________________________________________________________________
  |# See:                                                             |
  |# (find-eev-quick-intro \"4.1. `find-here-links'\")                  |
  |# (find-emacs-keys-intro \"1. Basic keys (eev)\" \"M-h M-h\")          |
  |# (find-here-links-intro \"4. `find-here-links-3'\")                 |
  |                                                                   |
  |# http://angg.twu.net/eev-intros/find-eev-quick-intro.html         |
  |# (find-eev-quick-intro)                                           |
  |                                                                   |
  |                                                                   |
  | -:**-  *Elisp hyperlinks*   All L1     (Fundamental eev)  --------|
  |___________________________________________________________________|

The elisp hyperlink

  # (find-eev-quick-intro)

at the end opens this tutorial.

The best way to learn how to create very quickly these
\"hyperlinks to things we saw or visited\" and to copy them to
our notes is explained in a separate tutorial:

  (find-here-links-intro)
  (find-here-links-intro \"3. `find-here-links'\")
  (find-here-links-intro \"3. `find-here-links'\" \"beginners\")

Cutting and pasting is explained briefly in section 5.2, below.
A way to go quickly to \"~/TODO\" is explained in section 7.1.
A way to \"refine\" hyperlinks to make them more precise is
explained here:

  (find-refining-intro \"2. Refining hyperlinks\")




4.2. `find-ekey-links' and friends
----------------------------------
Emacs is huge and you will probably want to save in your notes
many links about keys and functions that look interesting - using
`M-2 M-j' from time to time to visit

  (find-emacs-keys-intro)

and learning the keys listed there will not be enough.

Try the eek links below:

  (eek \"M-h M-k  C-x 4 0\")
  (eek \"M-h M-k  C-x 4 0  ;; kill-buffer-and-window\")
  (eek \"M-h M-k  C-s\")
  (eek \"M-h M-k  C-s  ;; isearch-forward\")
  (eek \"M-h M-f  isearch-forward\")

  (eek \"M-h M-k  M-h M-k  ;; find-ekey-links\")
  (eek \"M-h M-k  M-h M-f  ;; find-efunction-links\")

You will notice that:

  1. they create temporary buffers with lots of elisp hyperlinks,

  2. you can \"go back\" from these buffers with `M-k',

  3. the function name after the \";;\" is a comment and is
     ignored by Emacs (but is useful for humans),

  4. you can copy these hyperlinks to your \"~/TODO\",

  5. the \"M-h M-k\" and \"M-h M-f\" in the beginning are
     reminders that we can use `M-h M-k <key-sequence>' and `M-h
     M-f <function-name>' to create help buffers about other keys
     and functions,

  6. some of the hyperlinks use low-level representations for key
     sequences - you don't need to understand them. An example:

       (find-ekey-links \"\\^X40\")

  7. for _some_ keys and functions, but not all, the hyperlinks
     like

       (Info-goto-emacs-key-command-node \"\\C-s\")
       (Info-goto-emacs-command-node 'isearch-forward)
       (find-enode \"Command Index\" \"* isearch-forward:\")
       (find-elnode \"Index\" \"* defun:\")

     that appear near the middle of the elisp hyperlinks buffers,
     point to relevant points in the Emacs manuals - see the next
     section.

  8. `M-h M-k' can also handle some sequences of input events
     that are not key sequences. For example, if you click
     on \"Help\" in the menu bar and then click on \"Search
     Documentation\" and then on \"Emacs Terminology\" in the
     submenus this will open this page of the Emacs manual:

       (find-enode \"Glossary\")

     If you do `M-h M-k' and then click on Help -> Search
     Documentation -> Emacs Terminology in the menu bar then `M-h
     M-k' will generate a buffer in which one of the first lines
     will be:

       (eek \"M-h M-k  <menu-bar> <help-menu> <search-documentation> <emacs-terminology>\")

     For more information on that see:

       (find-enode  \"Menu Bar\")
       (find-enode  \"Glossary\" \"\\nInput Event\")
       (find-elnode \"Input Events\")
       (find-elnode \"Key Sequence Input\" \"menu-bar\")




5. Links to Emacs documentation
===============================
Try these links (some of them need the Emacs manuals installed):

  (find-emacs-keys-intro \"Cutting & pasting\")
  (find-node \"(emacs)Screen\")
  (find-efunctiondescr 'find-file)
  (find-efunction-links 'find-file)

This part of the eev tutorials has links to almost all the keys that
I've learned by heart after using Emacs for 20 years:

  (find-emacs-keys-intro \"2. Key sequences\")

They are not very many, because I use this a lot,

  (find-node \"(emacs)M-x\")

and I use elisp hyperlinks to create quick reminders for the keys that
I only need to remember when I am performing specific tasks.

Moral: when you want a quick reference of the main Emacs and eev keys,
type `M-2 M-j'.



5.1. Navigating the Emacs manuals
---------------------------------
The Emacs manuals are in \"info\" format, which means:

  a) they are divided into \"nodes\" - a top node, and chapters,
     sections, subsections, etc,

  b) the nodes in each manual in info format are organized as a tree,
     and they're all numbered except for the top node, the indexes and
     the appendixes. For example:

       top --.-- 1 --.-- 1.1
             |       `-- 1.2
             |-- 2
             |-- 3 ----- 3.1 --.-- 3.1.1
             |                 |-- 3.1.2
             |                 `-- 3.1.3
             |-- Appendix A
             `-- Index

  c) each node has both a short name and a long name (its title),
     and they may be different. For example, the hyperlinks below

       (find-node \"(emacs)Intro\")
       (find-node \"(emacs)Screen\")

     point to nodes whose titles are \"Introduction\" and \"The
     Organization of the Screen\",

  d) each manual also has a short name, also called its
     _filename_, and several kinds of long names and titles. The
     `find-node' links use the filename in parenthesis followed
     by the short node name. For example:

       Manual title                       elisp hyperlink
       ----------------------------------------------------------
       GNU Emacs Manual                 (find-node \"(emacs)\"Top)
       Emacs Lisp / GNU Emacs Lisp
         Reference Manual               (find-node \"(elisp)\"Top)
       An Introduction to
         Programming in Emacs Lisp      (find-node \"(eintr)\"Top)

  e) The \"Info directory\" lists all the installed info manuals.
     You can access it with:

       (find-node \"(dir)Top\")

     The main Emacs manuals appear grouped together there. Try:

       (find-node \"(dir)Top\" \"extensible self-documenting\")

     You will see something like this:

       Emacs
       * Emacs:               The extensible self-documenting text editor.
       * Emacs FAQ:           Frequently Asked Questions about Emacs.
       * Elisp:               The Emacs Lisp Reference Manual.
       * Emacs Lisp Intro:    A simple introduction to Emacs Lisp
                                programming.

  f) Emacs uses \"Info mode\" when displaying nodes of manuals in
     info format. In Info mode the tool bar displays icons
     meaning \"back\", \"forward\", \"previous\", \"next\",
     \"home\", etc, and you can click on these icons to navigate
     from the current node to other nodes. The main keys of Info
     mode are worth learning, though - the full list of keys can
     be found here,

       (find-efunctiondescr 'Info-mode)

     and the main ones are:

       q         exit             (go back to some other buffer)
       (arrows)  move point
       RET       follow link at point
       TAB       move to next link
       BACKTAB   move to prev link
       u         move \"up\" from this node
       n         move to the \"next\" node of this node
       p         move to the \"previous\" node of this node
       [         go backward one node, considering all nodes as
                 forming one sequence
       ]         go forward one node, considering all nodes as
                 forming one sequence

       d         go to the Info directory node.
       l         move back in history to the last node you were at.
       r         move forward in history to the node you returned from
                 after using `l'
       L         go to menu of visited nodes
       T         go to table of contents of the current Info file

    Try the keys above now - if you execute the `eek' sexp below
    it will split the window, keep these instructions in the left
    window and open and Info buffer at the right.

      (eek \"<down> M-3 M-e  ;; open the hyperlink below in the right window\")
      (find-node \"(emacs)Basic\")
      (find-node \"(emacs)Major Modes\")



5.2. Cutting and pasting
------------------------
You can do cut, copy and paste in a \"user-friendly\" way by using

  a) the rightmost icons in the toolbar, or
  b) the \"Edit\" menu in the menu-bar,

but the keys are very much worth learning:

  C-SPC   -- set-mark-command           (find-enode \"Setting Mark\")
  C-x C-x -- exchange-point-and-mark    (find-enode \"Setting Mark\" \"C-x C-x\")
  C-w     -- kill-region     (cut)      (find-enode \"Other Kill Commands\")
  M-w     -- kill-ring-save  (copy)     (find-enode \"Kill Ring\")
  C-y     -- yank            (paste)    (find-enode \"Kill Ring\")

The \"region\" where cut & paste operate is always what is between
the \"point\" and the \"mark\". See:

  (find-enode \"Point\")
  (find-enode \"Mark\")

Exercise: understand how the `eek' sexp below switches the two
lines just after it.

  (eek \"<down> C-a C-SPC <down> C-w <down> C-y 3*<up>\")
  First
  Second








6. Controlling shell-like programs
==================================
This is the second main feature of eev. The hyperlinks thing used the
keys `M-e', `M-k', and `M-h M-h', plus standard Emacs keys for cutting
and pasting. The module of eev that controls shell-like programs - it
is called \"eepitch\" - uses `<F8>' and `M-T'. Note that it is
`alt-shift-t', to not interfere with Emacs's `M-t'.

For more details see:

  (find-eepitch-intro)
  (find-wrap-intro \"2. <M-T>: produce an eepitch block\")

[Video links:]
  (find-eevnavvideo \"10:36\" \"if I type <f8> six times here\")
  (find-eevnavvideo \"10:50\" \"a shell running inside Emacs\")
  (find-eev2019video \"15:11\" \"Demo: the eepitch block (in red star lines)\")
  (find-eev2019video \"15:48\" \"Demo: eepitch on non-red star lines\")
  (find-eev2019video \"15:56\" \"Demo: eepitch in action\")
  (find-eevtestblsvideo \"2:33\" \"if I run f8 here I start a new Lua interpreter\")





6.1. The main key: <F8>
-----------------------
Emacs can run a shell in a buffer, and it can split its frame
into windows, like this:
   ___________________
  |         |         |
  |   our   |    a    |
  |  notes  |  shell  |
  |         |  buffer |
  |_________|_________|

The usual way to use a shell buffer is to move the cursor there
and type commands into its prompt; the eepitch-y way is to leave
the cursor at the \"notes\" buffer, write the commands for the
shell there, and send these commands to the shell with <F8>.

Here's what <F8> does:

  When we type <F8> on a line that starts with a red
  star (\"\"), it executes the rest of the line as Lisp, and
  moves down; when we type <F8> on a line that does not start
  with a \"\", it makes sure that the \"target buffer\" is being
  displayed (the \"target\" is usually the buffer called
  \"*shell*\"), it \"send\"s the current line to the target
  buffer, and moves down.

  \"Sending the current line to the target buffer\" means copying
  the contents of the current line to the target - as if the user
  had typed that line there by hand -, then \"typing\" a <RET> at
  the target buffet.

Please try that in the example after this paragraph, by typing
<F8> six times starting at the first line that says
\" (eepitch-shell)\". The three red star lines at the top will
create a target buffer, destroy it, and create it again; the
other three lines will send commands to the target shell.

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
echo \"We are at: $PWD\"
cd /tmp/
echo \"We changed to: $(pwd)\"




6.2. Other targets
------------------
Just like `(eepitch-shell)' creates a shell buffer and sets the
eepitch target to it, `(eepitch-python)' creates a buffer with a
Python interpreter and uses it as the eepitch target. Try:

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
def square (x):
    return x*x

print(square(5))

  We can use several targets at the time, alternating between them.
  For example:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
echo Hello... > /tmp/o

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
print(open(\"/tmp/o\").read())

 (eepitch-shell)
echo ...and bye >> /tmp/o

 (eepitch-python)
print(open(\"/tmp/o\").read())


  It is possible to display all the targets at the same time,
  using advanced features that are explained here:

    (find-multiwindow-intro \"4. Several eepitch targets\")
    (find-multiwindow-intro \"7. Eepitch blocks for two targets\")

  Here is a demo:

 (find-3EE '(eepitch-shell) '(eepitch-python))
 (eepitch-shell)
echo Hello... > /tmp/o

 (eepitch-python)
print(open(\"/tmp/o\").read())

 (eepitch-shell)
echo ...and bye >> /tmp/o

 (eepitch-python)
print(open(\"/tmp/o\").read())






6.3. Creating eepitch blocks: `M-T'
-----------------------------------
Write just \"shell\" or \"python\" in a line, then type
`M-T' (i.e., meta-shift-t) there. The line will be turned into
three - an \" (eepitch-xxx)\", an \" (eepitch-kill)\", and an
\" (eepitch-xxx)\". We call these blocks of three lines
\"eepitch blocks\". Try this below, converting the \"shell\" into
an eepitch block for starting a shell.

shell
pwd
cd /tmp/
pwd




6.4. Red stars
--------------
Eepitch.el sets the glyph for the char 15 to a red star in the
standard display table. In layman's terms: eepitch.el tells Emacs
that the character 15 should be displayed as a red star. The
character 15 corresponds to control-O, whose default
representation on screen would be \"^O\". You can enter a
literal ^O in a buffer by typing `C-q C-o'.

It is possible to make other characters play the role of the red
star, but to make that work you need to know a bit of Lisp. See:

  (find-red-star-links)
  (find-eev \"eev-tlinks.el\" \"find-red-star-links\")






7. Quick access to one-liners
=============================
[Video links:]
  (find-eevnavvideo \"6:28\"  \"M-j: you can forget practically everything...\")
  (find-eevnavvideo \"6:41\"  \"if you type just M-j\")
  (find-eevnavvideo \"6:48\"  \"has a header that is beginner-friendly\")
  (find-eevnavvideo \"8:48\"  \"most of the entries have hyperlinks to the manual\")
  (find-eevnavvideo \"9:48\"  \"M-j with numeric prefixes\")
  (find-eevnavvideo \"9:59\"  \"M-2 M-j runs (find-emacs-keys-intro)\")
  (find-eevnavvideo \"10:06\" \"M-5 M-j runs (find-eev-quick-intro)\")


7.1. `eejump'
-------------
Some key sequences in Emacs accept numeric arguments. For
example, try typing `M-9 a' (not `M-9 M-a'!) - this will insert 9
copies of the letter `a'. See:

  (find-enode \"Arguments\")

Eev binds the key `M-j' (`eejump') to a function that jumps to a
place that depends on the numeric argument. For example, `M-5
M-j' runs (find-eev-quick-intro), that reloads this intro and
goes to the top of it, and

  `M-2 M-j' runs: (find-emacs-keys-intro)
  `M-6 M-j' runs: (find-escripts-intro)
  `M-1 M-j' runs: (find-fline \"~/TODO\")

The way to associate a number to a place to jump to - or, more
precisely, to a single-line piece of Elisp code, i.e., a
\"one-liner\" - is through code like this:

  (defun eejump-5 () (find-eev-quick-intro))
  (defun eejump-2 () (find-emacs-keys-intro))
  (defun eejump-6 () (find-escripts-intro))
  (defun eejump-1 () (find-fline \"~/TODO\"))

The pattern is:

  (defun eejump-<nnn> () <one-liner associated to the argument nnn>)

`defun' is explained here:

  (find-elnode \"Defining Functions\" \"(defun foo () 5)\")
  (find-eval-intro \"10. More on functions\")
  (find-elisp-intro \"6. Defining functions\")

As a special case, a plain `M-j' without a prefix argument runs a
special function, `find-eejumps', that shows a help text followed
by all the `(defun eejump-<nnn> () ...)'s that are currently
active. So:

      `M-j' runs: (find-eejumps)

`find-eejumps' is explained in the next section.

Let's try to understand `M-j' from both a user's point of view and
from a technical point of view.

We may have elisp one-liners that we want to be able to execute very
quickly, and from anywhere. For example, I keep all my notes that I
have not organized yet in a file called \"~/TODO\", and if I type

  M-1 M-j

then I \"jump\" to \"~/TODO\" - the effect is the same as running this:

  (find-fline \"~/TODO\")

Note that `M-1 M-j' can be typed as:

  hold the meta key,
    type 1,
    type j,
  release the meta key.

instead of:

  hold the meta key,
    type 1,
  release the meta key,
  hold the meta key,
    type j,
  release the meta key;

There is no need to release and press again the meta key between
the `1' and the `j'.

Internally, what happens when you type `M-1 M-j' is this:

  `M-j' is bound to `eejump',
  `M-1 M-j' runs `eejump' with argument 1, i.e., (eejump 1),
  (eejump 1) runs (eejump-1),
  (eejump-1) has been defined with:

    (defun eejump-1 () (find-fline \"~/TODO\"))

So `M-1 M-j' runs the one-liner `(find-fline \"~/TODO\")'.
Similarly, `M-5 M-j' runs the one-liner `(find-eev-quick-intro)',
and so on.

We will sometimes refer to the one-liner associated to the
argument nnn as the \"eejump target associated to nnn\", or just
as the \"target associated to nnn\".





7.2. The list of eejump targets
-------------------------------
If you type `M-j' without a prefix argument it runs
`(find-eejumps)', that displays a temporary buffer with a header
with help and links and then a list of all the current eejump targets.
Try it:

  (eek \"M-j\")
  (find-eejumps)

The header is this:

  ;; Generated by: (find-eejumps)
  ;; See: (find-eev-quick-intro \"7.1. `eejump'\" \"`M-j'\")
  ;;      (find-eev-quick-intro \"7.2. The list of eejump targets\")
  ;;      (find-emacs-keys-intro \"1. Basic keys (eev)\")
  ;;      (find-emacs-keys-intro \"2. Key sequences\")
  ;; For example,
  ;;         M-5 M-j  runs:  (find-eev-quick-intro)
  ;;         M-2 M-j  runs:  (find-emacs-keys-intro)
  ;;         M-1 M-j  runs:  (find-fline \"~/TODO\")
  ;;     M-2 M-1 M-j  shows the file ~/TODO in the right window
  ;;     M-3 M-1 M-j  opens ~/TODO in the right window
  ;; Current eejump targets:

Note that after the \"See:\" we have four elisp hyperlinks to
sections of tutorials, and after the \"For example:\" we have
five examples of how to run `M-j' with numeric prefixes; the
first three of these examples are followed by the elisp hyperlink
whose action corresponds to running `M-j' with that prefix.

That header is very beginner-friendly, and if you are a beginner
who only knows how to use `M-e' and `M-j' you can, and should,
use that header as your main starting point: every time that you
feel lost you can type `M-j' to go back to that header, and you
can use its links to navigate to the documentation for Emacs and
eev.

That header is followed by a section that is very beginner
UN-friendly, that contains a series of defuns like these ones:

  (defun eejump-1 () (find-fline \"~/TODO\"))
  (defun eejump-5 () (find-eev-quick-intro))





7.3. Defining eejump targets
----------------------------
We can define new eejump targets, or overwrite the current ones, by
just running `defun's to define functions with names starting with
`eejump-'. Try:

  (defun eejump-9 () (find-eev-quick-intro \"7.2.\"))
  (defun eejump-9 () (find-eev-quick-intro \"7.3.\"))
  (fmakunbound 'eejump-9)
  (find-eejumps)

Note that if you type `M-J' (i.e., meta-uppercase-j,
`eewrap-eejump') on the line below then it will be converted into
the first \"(defun eejump- ...)\" above:

  9 (find-eev-quick-intro \"7.2.\")

An advanced feature: if you type `M-J' on a line that starts with
something that is not a number, you get a defun for a \"command
with a very short name\" like the ones that are described in the
next section. Try it now:

  (eek \"<down> M-J\")
  e (find-fline \"/tmp/foo.tex\")

[Video links:]
  (find-eev2020video \"26:49\" \"1.5.3. `M-J' (meta-uppercase-J)\")
  (find-eev2020video \"26:58\"   \"transforms the current line\")
  (find-eev2020video \"27:37\"   \"into a defun\")




7.4. Commands with very short names
-----------------------------------
Let's start with an example. If we are editing a LaTeX file, say
\"/tmp/foo.tex\", then it is convenient to have quick ways to:

  c) [c]ompile \"foo.tex\" into a \"foo.pdf\",
  d) [d]isplay the resulting \"foo.pdf\",
  e) jump to \"foo.tex\" from anywhere to [e]dit it.

If our \"/tmp/foo.tex\" starts with these lines

  % (defun c () (interactive) (find-sh \"cd /tmp/; pdflatex foo.tex\"))
  % (defun d () (interactive) (find-pdf-page \"/tmp/foo.pdf\"))
  % (defun e () (interactive) (find-fline \"/tmp/foo.tex\"))

and we execute these defuns, then from that point on `M-x c', `M-x d'
and `M-x e' will do \"compile\", \"display\" and \"edit\" on \"foo.tex\", as
described above.

For more on `M-x', and on why the defuns above need the
\"(interactive)\", see:

  (find-enode \"M-x\")
  (find-enode \"Commands\")
  (find-elnode \"Defining Commands\")

Remember that you can list all current eejump targets with:

  (find-eejumps)

you can list the current commands with very short names with:

  (find-eeshortdefs)

[Video links:]
  (find-eev2020video \"19:03\" \"commands with very short names\")
  (find-eev2020video \"21:07\" \"commands with very short numbers\")





7.5. `find-latex-links'
-----------------------
The easiest way to put the three defuns of the last section in
the header of a LaTeX file is with:

  (find-latex-links \"/tmp/foo\")

`find-latex-links' is just one of several template functions that
generate commands with very short names. Here's how to use it -
the other ones are similar.

  1) Run `M-x find-latex-links'. You will get a buffer whose top
     line is:

       # (find-latex-links \"{stem}\")

  2) Edit that, and change the \"{stem}\" to \"/tmp/foo\".

  3) Execute that top line, which is now:

       # (find-latex-links \"/tmp/foo\")

     You should get something like:

   _____________________________________________________________________
  |# (find-latex-links \"/tmp/foo\")                                      |
  |# (find-eev-quick-intro \"`find-latex-links'\")                        |
  |# (ee-copy-rest 1 '(find-fline \"/tmp/foo.tex\"))                      |
  |                                                                     |
  |% (defun c () (interactive) (find-sh \"pdflatex foo.tex\"))            |
  |% (defun d () (interactive) (find-pdf-page \"/tmp/foo.pdf\"))          |
  |% (defun e () (interactive) (find-fline \"/tmp/foo.tex\"))             |
  |%                                                                    |
  |\\documentclass{article}                                              |
  |\\begin{document}                                                     |
  |                                                                     |
  |\\end{document}                                                       |
  |                                                                     |
  |                                                                     |
  | -:**-  *Elisp hyperlinks*   All L1     (Fundamental)                |
  |_____________________________________________________________________|


  4) Execute the line with the \"(ee-copy-rest ...)\". You should get this -
     the window on the right is visiting the file \"/tmp/foo.tex\":

   ______________________________________________________________________
  |# (find-latex-links \"/tmp/foo\")   |                                  |
  |# (find-eev-quick-intro \"`find-lat|                                  |
  |# (ee-copy-rest 1 '(find-fline \"/t|                                  |
  |                                  |                                  |
  |% (defun c () (interactive) (find-|                                  |
  |% (defun d () (interactive) (find-|                                  |
  |% (defun e () (interactive) (find-|                                  |
  |%                                 |                                  |
  |\\documentclass{article}           |                                  |
  |\\begin{document}                  |                                  |
  |                                  |                                  |
  |\\end{document}                    |                                  |
  |                                  |                                  |
  |                                  |                                  |
  | -:**-  *Elisp hyperlinks*   All L| -:**-  foo.tex        All L9     |
  |_(Copied 8 lines to the kill ring - use C-y to paste)________________|


  5) Go to the window on the right and type `C-y'. You should get this,

   ______________________________________________________________________
  |# (find-latex-links \"/tmp/foo\")   |% (defun c () (interactive) (find-|
  |# (find-eev-quick-intro \"`find-lat|% (defun d () (interactive) (find-|
  |# (ee-copy-rest 1 '(find-fline \"/t|% (defun e () (interactive) (find-|
  |                                  |%                                 |
  |% (defun c () (interactive) (find-|\\documentclass{article}           |
  |% (defun d () (interactive) (find-|\\begin{document}                  |
  |% (defun e () (interactive) (find-|                                  |
  |%                                 |\\end{document}                    |
  |\\documentclass{article}           |                                  |
  |\\begin{document}                  |                                  |
  |                                  |                                  |
  |\\end{document}                    |                                  |
  |                                  |                                  |
  |                                  |                                  |
  | -:**-  *Elisp hyperlinks*   All L| -:**-  foo.tex        All L9     |
  |_____________________________________________________________________|

  and you can now save the file foo.tex (hint: use `C-x C-s'!),
  execute the three defuns for `c', `d', and `e', and jump to
  \"/tmp/foo.tex\" from anywhere with `M-x e'.

  A detailed explanation of `ee-copy-rest' can be found here:

    (find-eev \"eev-tlinks.el\" \"ee-copy-rest\")





8. Anchors
==========
[Video links:]
  (find-eevfherelvideo \"14:18\" \"4.1. Creating anchors\")
  (find-eevfherelvideo \"15:22\"  \"an index at the beginning of the file\")
  (find-eevfherelvideo \"15:47\"  \"the tutorial also explains ... `M-A'\")
  (find-eevfherelvideo \"16:07\"  \"`M-A' duplicates the line and...\")
  (find-eevfherelvideo \"16:19\"  \"`M-B' creates an e-script block\")


8.1. Introduction: `to'
-----------------------
A hyperlink like

  (to \"foo\")

jumps to the first occurrence of the string \"«foo»\" in the
current buffer. The function that wraps a string in `«»'s is
called `ee-format-as-anchor', and the sexp `(to \"foo\")'
is equivalent the second sexp below:

                    (ee-format-as-anchor \"foo\")
  (ee-goto-position (ee-format-as-anchor \"foo\"))

We will call strings in `«»'s _anchors_, and we will say
that `(to \"foo\")' jumps \"to the anchor `foo'\". The string
inside a `«»'s is called a _tag_.

In a situation like this,

  «one»     (to \"two\")
  «two»     (to \"three\")
  «three»   (to \"four\")
  «four»    (to \"one\")

we have four anchors, and typing `M-e' at the line with the
anchor \"one\" takes us to the line with the anchor \"two\",
typing `M-e' at the line with the anchor \"two\" takes us to the
line with the anchor \"three\", typing `M-e' again takes us to
the line with the anchor \"four\", and typing `M-e' again takes
us back to the line with the anchor \"one\". In a situation like
this we say that the anchors \"one\", \"two\", \"three\", and
\"four\" _point to one another_.

In a case like this,

  «.five»   (to \"five\")
   «five»  (to \".five\")

where the names of two anchors pointing to one another differ by
an initial dot, we will say that the anchor \".five\" is the
\"index anchor\", and the anchor \"five\" is the \"section
anchor\"; and one way to create an index for a file is to group
all the index anchors together. For an example, see:

  (find-eev \"eev-intro.el\" \".find-eev-intro\")



8.2. Creating anchors by hand
-----------------------------
One way to type the chars `«' and `»' is with `C-x 8 <' and
`C-x 8 >'. Try:

  (eek \"RET C-x 8 < t a g C-x 8 >\")




8.3. Creating index/section anchor pairs
----------------------------------------
Eev has several commands that transform the text in the current
line into something more complex. They are all called
`eewrap-(something)', and they are bound to
meta-uppercase-letters. The simplest examples are `M-F', `M-S'
and `M-M', that just \"wrap the text in the current line into an
elisp hyperlink\" by adding a prefix and a suffix; if you run
`M-F', `M-S' and `M-M' in the following lines

/tmp/
ls /tmp/
ls

they become this:

# (find-fline \"/tmp/\")
# (find-sh \"ls /tmp/\")
# (find-man \"ls\")

You can also try them by running the `eek' sexps below,

\(eek \"<down> M-F\")
/tmp/

\(eek \"<down> M-S\")
ls /tmp/

\(eek \"<down> M-M\")
ls

HINT: sometimes the eewrap commands don't do exactly what we
want, so learn how to use the \"undo\" command of Emacs. See:

  (find-emacs-keys-intro \"5. Undoing\")

The command `eewrap-anchor' (bound to `M-A') is similar to those
above, but it parses the current line in a more complex way -
everything between \"<>\" is the \"anchor\" and everything before
the \"<\" is the \"comment prefix\" - and it converts the current
line into two lines with `to's, each one pointing to the other
one. For example, `M-A' in the line below

  # <first-test>

yields this:

  # «.first-test»	(to \"first-test\")
  # «first-test» (to \".first-test\")

The line with the anchor \"«.first-test»\" is intended to be
moved - by hand, with cut and paste - to the index section at the
beginning of the file, as explained here:

  (find-escripts-intro)

Another way of moving the index line to the index section is
described here:

  (find-edit-index-intro)




8.4. Creating e-script blocks
-----------------------------
The key `M-B' (`eewrap-escript-block') is a variant of `M-A' that
converts the current line into nine (!) lines instead of two. If
we type `M-B' on the line below

  second-test Long description

it becomes this - the header of an \"e-script block\":

#####
#
# Long description
# 2018may22
#
#####

# «.second-test»	(to \"second-test\")
# «second-test» (to \".second-test\")

where again the line with the anchor \"«.second-test»\" is
intended to be moved to the index section at the beginning of the
file. The use of these \"e-script blocks\" is explained bere:

  (find-escripts-intro)




8.5. Hyperlinks to anchors in other files
-----------------------------------------
`find-anchor' is like `find-fline', but it interprets the first
argument after the file in a special way if it is present. These
hyperlinks are all equivalent:

  (find-anchor \"~/eev2/eev-blinks.el\" \"find-wottb\")
  (find-anchor \"~/eev2/eev-blinks.el\" (ee-format-as-anchor \"find-wottb\"))
  (find-fline  \"~/eev2/eev-blinks.el\" \"«find-wottb»\")

You can use this - or the shorter hyperlinks to anchors in
section 9.2 - to point to anchors or to e-script blocks in your
files.

[Video links:]
  (find-eevfherelvideo \"17:02\" \"4.2. The option :anchor for `code-c-d'\")






9. Shorter hyperlinks
=====================
See also: (find-code-c-d-intro)

[Video links:]
  (find-eev2020video  \"8:20\" \"`code-c-d' defines several new functions\")
  (find-eev2020video \"14:43\" \"1.4. How code-c-d is implemented\")
  (find-eev2020video \"17:20\"   \"find-code-c-d shows the code...\")
  (find-eev2019video \"17:07\" \"Demo: `code-c-d'\")



9.1. `code-c-d'
---------------
Sexps like

  (find-eevfile \"\")
  (find-eevfile \"eev-blinks.el\")
  (find-eevfile \"eev-blinks.el\" \"«find-sh»\")
  (find-udfile \"\")
  (find-udfile \"lua5.1-doc/\")
  (find-udfile \"lua5.1-doc/test/\")
  (find-udfile \"lua5.1-doc/test/fib.lua\")
  (find-udfile \"lua5.1-doc/test/fib.lua\" \"function fib(n)\")

work as abbreviations for

  (find-fline \"~/eev2/\")
  (find-fline \"~/eev2/eev-blinks.el\")
  (find-fline \"~/eev2/eev-blinks.el\" \"«find-sh»\")
  (find-fline \"/usr/share/doc/\")
  (find-fline \"/usr/share/doc/lua5.1-doc/\")
  (find-fline \"/usr/share/doc/lua5.1-doc/test/\")
  (find-fline \"/usr/share/doc/lua5.1-doc/test/fib.lua\")
  (find-fline \"/usr/share/doc/lua5.1-doc/test/fib.lua\" \"function fib(n)\")

They are \"mass-produced\", in the following sense. When we run this,

  (code-c-d \"ud\" \"/usr/share/doc/\")

the function `code-c-d' produces a big string using a template, and
evaluates that big string; the \"{c}\"s in the template are replaced by
the argument \"ud\" - called the \"code\" - and the \"{d}\"s in the template
are replaced by \"/usr/share/doc/\" - called the \"directory\". If we add
a \"find-\" before the `code-c-d', like this,

  (find-code-c-d \"ud\" \"/usr/share/doc/\")

we get a hyperlink to the code that `(code-c-d \"ud\" \"/usr/share/doc/\")'
would execute - i.e., to the result of substiting the \"{c}\"s and
\"{d}\"s in the template. This is useful for understanding how
`code-c-d' works; each call to `code-c-d' defines lots of functions,
some of them easier to explain, some harder. This, for example,

  (find-eevgrep \"grep --color -nH -e '(code-c-d ' *.el\")

greps for all calls to \"code-c-d\" in the source of eev.

By default, eev runs these `code-c-d's:

  (find-eevfile \"eev-code.el\" \"code-c-d \\\"e\\\"\")

You can add many more of them to your .emacs file.

An introduction to the ideas, details, innards and technicalities of
`code-c-d' can be found here:

  (find-code-c-d-intro)



9.2. Extra arguments to `code-c-d'
----------------------------------
If you compare the buffers generated by

  (find-code-c-d      \"CODE\" \"/DIR/\")
  (find-code-c-d      \"CODE\" \"/DIR/\" :info \"INFO\")
  (find-code-c-d-rest \"CODE\" \"/DIR/\" :info \"INFO\")

you will see that the `:info \"INFO\"' part adds some code to the end of
the generated string, and that the `find-code-c-d-rest' shows only
this extra code.

The most important extra arguments to `code-c-d' are:

  1) :info \"name-of-an-info-manual\"
  2) :gz
  3) :anchor

If the first extra argument is a string then `ee-code-c-d' adds an
`:info' before it, so these generate the same code:

  (find-code-c-d \"CODE\" \"/DIR/\"       \"INFO\")
  (find-code-c-d \"CODE\" \"/DIR/\" :info \"INFO\")

The eev source has this (in the file \"eev-code.el\"),

  (code-c-d \"e\"   ee-emacs-lisp-directory :info \"emacs\" :gz)
  (code-c-d \"eev\" ee-eev-source-directory :anchor)

and that code

  1) makes (find-enode \"\")
     work as an abbreviation for (find-node \"(emacs)\")

  2) makes (find-efile \"files.el\")
     run   (find-efile \"files.el.gz\")
     if the file \"files.el\" is not found,

  3) makes (find-eev     \"eev-blinks.el\"  \"find-wottb\")
     run:  (find-eevfile \"eev-blinks.el\" \"«find-wottb»\")
     or actually: (find-anchor (ee-eevfile \"eev-blinks.el\") \"find-wottb\")

Calls to `find-eev' are \"short hyperlinks to anchors\":

  (find-eev \"eev-blinks.el\" \"find-wottb\")
  (find-eev \"eev-blinks.el\" \"find-wottb\" \"defun find-wottb-call\")

For the technical details of the implementation, see here:

  (find-code-c-d-intro \"Extra arguments to `code-c-d'\")




9.3. Hyperlinks to PDF files
----------------------------
This section was moved to another tutorial! See:

  (find-pdf-like-intro \"2. Preparation\")
  (find-pdf-like-intro \"3. Hyperlinks to PDF files\")

Here is a very short summary. If you have run the preparation, by
executing the eepitch block below with <f8>s,

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  cd
  wget -nc https://tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf

then these sexps will be hyperlinks to a page of a PDF, and to
some string in it...

  (find-pdf-page \"~/Coetzee99.pdf\" 3 \"LECTURE I\")
  (find-pdf-text \"~/Coetzee99.pdf\" 3 \"LECTURE I\")

[Video links:]
  (find-eev2020video \"4:52\" \"`find-pdf-page' calls an external program\")
  (find-eev2020video \"5:26\" \"`find-pdf-text' converts the PDF to text and\")





9.4. Shorter hyperlinks to PDF files
------------------------------------
...and the `code-pdf-page' and `code-pdf-text' sexps below

  (code-pdf-page \"livesofanimals\" \"~/Coetzee99.pdf\")
  (code-pdf-text \"livesofanimals\" \"~/Coetzee99.pdf\" -110)

define the functions `find-livesofanimalspage' and
`find-livesofanimalstext', and the two sexps below

  (find-livesofanimalspage (+ -110 113) \"LECTURE I.\")
  (find-livesofanimalstext (+ -110 113) \"LECTURE I.\")

are now short hyperlinks to a page of a PDF, and to a string in
it.



9.5. Hyperlinks to audio and video files
----------------------------------------
Eev has some support for creating hyperlinks and short hyperlinks
to positions in audio files and video files, but it is not as
mature as the support for hyperlinks to positions in PDF files.
See:

  (find-audiovideo-intro)





10. Generating short hyperlinks
===============================

10.1. Generating short hyperlinks to files
------------------------------------------
If you run this

  (code-c-d \"foo\"  \"/tmp/FOO/\")
  (code-c-d \"bar\"  \"/tmp/FOO/BAR/\")
  (code-c-d \"plic\" \"/tmp/FOO/BAR/PLIC/\")

then these five links will all point to the same file:

  (find-file  \"/tmp/FOO/BAR/PLIC/bletch\")
  (find-fline \"/tmp/FOO/BAR/PLIC/bletch\")
  (find-foofile        \"BAR/PLIC/bletch\")
  (find-barfile            \"PLIC/bletch\")
  (find-plicfile                \"bletch\")

That file does not exist, but that is not important in the tests.
Note that the last three sexps are short hyperlinks. If you run

  (eek \"<down> M-3 M-e  ;; open the hyperlink below in the right window\")
  (find-file-links \"/tmp/FOO/BAR/PLIC/bletch\")

it will create an elisp hyperlinks buffer in which the last sexps
will be the three different short hyperlinks to
\"/tmp/FOO/BAR/PLIC/bletch\" above.

Remember that `find-here-links' - i.e., `M-h M-h' - can act in
several different ways depending on the context, i.e., depending
on what is \"here\". If you type `M-h M-h' in a buffer visiting a
file it runs a slight variation of `find-file-links' on that
file, and if you visit our test file with, say,

  (find-plicfile                \"bletch\")

and type `M-h M-h' there then one of the hyperlinks that will be
shown will be exactly the one with `find-plicfile'.

This works for all files. If you visit a file and type `M-h M-h'
then the last hyperlinks in the temporary buffer will be the
short hyperlinks to that file.




10.2. Generating short hyperlinks to info nodes
-----------------------------------------------
If you run this

  (code-c-d \"el\" ee-emacs-lisp-directory \"elisp\")

then these three hyperlinks will point to the same info node:

  (info      \"(elisp)Box Diagrams\")
  (find-node \"(elisp)Box Diagrams\")
  (find-elnode      \"Box Diagrams\")

Note that the last one is a short hyperlink. If you follow it and
then type `M-h M-h' you will see that the last hyperlink in the
\"*(find-here-links)*\" is exactly that short hyperlink. But try
this big sexp:

  (progn (find-elnode \"Box Diagrams\")
         (eek \"C-h r    ;; info-emacs-manual\")
         (eek \"M-h M-h  ;; find-here-links\")
         )

You will get a \"*(find-here-links)*\" buffer that points to a
page in the \"(emacs)\" manual instead of to one in the
\"(elisp)\" manual, and that does not have a short hyperlink at
the end. What happened?

The trick is that the code that produces that short hyperlink
uses two global variables and runs conditionally. When you run

  (find-elnode \"Box Diagrams\")

it sets these two variables:

  (setq ee-info-code \"el\")
  (setq ee-info-file \"elisp\")

You can check that in its source code:

  (find-efunctionpp 'find-elnode)
  (find-code-c-d \"el\" ee-emacs-lisp-directory \"elisp\")

The sub-function of `find-here-links' that is executed when
`find-here-links' detects that \"here\" is an info node only
generates the short hyperlink when the current \"info file\"
corresponds to the value saved in the global variable
`ee-info-file'.




10.3. Generating short hyperlinks to intros
-------------------------------------------
Let's see an example. If you follow this link and type `M-h M-h',

  (find-multiwindow-intro)

you will get an \"*Elisp hyperlinks*\" buffer whose last line
will be:

  # (find-multiwindow-intro)

which is a short hyperlink to the intro.




10.4. Generating short hyperlinks to PDFs
-----------------------------------------
This section was moved to:

  (find-pdf-like-intro \"9. Generating three pairs\")





10.5. Generating short hyperlinks to anchors
--------------------------------------------
See:
  (find-refining-intro \"5. Pointing to anchors\")







11. Generating `code-c-d's and friends
======================================
There are two main ways to generate lines like these

  (code-c-d      \"asy\" \"/usr/local/texlive/2019/texmf-dist/doc/asymptote/\")
  (code-pdf-page \"asy\" \"/usr/local/texlive/2019/texmf-dist/doc/asymptote/asymptote.pdf\")
  (code-pdf-text \"asy\" \"/usr/local/texlive/2019/texmf-dist/doc/asymptote/asymptote.pdf\")

without having to type much. The old way is with
`M-C' (`eewrap-code-c-d') and `M-P' (`eewrap-pdflike'), that
transform the current line in a way similar to
`M-T' (`eewrap-eepitch'). You can test these key sequences by
running the `eek' sexps below:

  (eek \"<down> M-C\")
  asy /usr/local/texlive/2019/texmf-dist/doc/asymptote/

  (eek \"<down> M-P\")
  asy /usr/local/texlive/2019/texmf-dist/doc/asymptote/asymptote.pdf

The new way is with `M-h M-e', that is explained here:

  (find-audiovideo-intro \"4.1. `find-extra-file-links'\")




11.1. `find-pdf-links'
----------------------
This section was moved to:

  (find-pdf-like-intro \"8. `find-pdf'-pairs\")
  (find-pdf-like-intro \"9. Generating three pairs\")





\(To be continued...)
" pos-spec-list)))

;; end of defun

;; (find-eev-quick-intro)




;;;                                      _
;;;   ___ _ __ ___   __ _  ___ ___      | | _____ _   _ ___
;;;  / _ \ '_ ` _ \ / _` |/ __/ __|_____| |/ / _ \ | | / __|
;;; |  __/ | | | | | (_| | (__\__ \_____|   <  __/ |_| \__ \
;;;  \___|_| |_| |_|\__,_|\___|___/     |_|\_\___|\__, |___/
;;;                                               |___/
;;
;; «find-emacs-keys-intro» (to ".find-emacs-keys-intro")
;; Skel: (find-intro-links "emacs-keys")

(defun find-emacs-keys-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-emacs-keys-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-emacs-keys-intro)
Source code:  (find-efunction 'find-emacs-keys-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-here-links-intro)
              (find-refining-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.
The quickest way to open or recreate this is with `M-2 M-j'.



1. Basic keys (eev)
===================
The most basic keys of eev are:
  M-e   - to follow a hyperlink.  Mnemonic: \"(e)valuate\"/\"(e)xecute\".
          See: (find-eev-quick-intro \"2. Evaluating Lisp\")
               (find-eev-quick-intro \"3. Elisp hyperlinks\")
  M-j   - to jump to certain predefined places.  In particular,
              `M-j' takes you to a buffer with basic help and a
                    list of jump targets. See:
                    (find-eev-quick-intro \"7.2. The list of eejump targets\")
          `M-2 M-j' takes you to this help page.
          `M-5 M-j' takes you to: (find-eev-quick-intro)
  M-k   - to go back.  Mnemonic: \"(k)ill buffer\".
          See: (find-eev-quick-intro \"3. Elisp hyperlinks\" \"M-k\")
  M-K   - to go back without killing the buffer.
          See: (find-eval-intro \"5. Going back\")
               (find-eval-intro \"5. Going back\" \"`M-K' instead of `M-k'\")
  <f8>  - See: (find-eev-quick-intro \"6. Controlling shell-like programs\")
  M-T   - See: (find-eev-quick-intro \"6.3. Creating eepitch blocks: `M-T'\")

The beginner's way of creating \"hyperlinks to here\" is with:
  M-h M-3   - (find-here-links-intro \"4. `find-here-links-3'\")
  M-h M-1   - (find-here-links-intro \"5. `find-here-links-1'\")
  M-h M-w   - (find-here-links-intro \"6. Copying the hyperlink\" \"M-h M-w\")

The main keys for creating buffers with elisp hyperlinks are:
  M-h M-h   - `find-here-links': (find-eev-quick-intro \"4.1. `find-here-links'\")
  M-h M-k   - (find-eev-quick-intro \"4.2. `find-ekey-links' and friends\")
  M-h M-f   - (find-eev-quick-intro \"4.2. `find-ekey-links' and friends\")
  M-h M-e   - (find-audiovideo-intro \"4.1. `find-extra-file-links'\")
  M-h M-p   - (find-pdf-like-intro \"9. Generating three pairs\")
              (find-pdf-like-intro \"9. Generating three pairs\" \"M-h M-p\")
    See also: (find-links-intro \"5. The first line regenerates the buffer\")

The main keys for refining hyperlinks are:
      M-h M-2   - `ee-duplicate-this-line'. See: (find-eval-intro \"M-h M-2\")
      M-h M-y   - `ee-yank-pos-spec'.       See: (find-eval-intro \"M-h M-y\")
  M-1 M-h M-w   - copy the preceding tag to the kill ring
      M-h M--   - shrink hyperlink to make it point to an anchor
            See also: (find-refining-intro \"2. Refining hyperlinks\")
                      (find-anchors-intro \"2. Shrinking\")
                      (find-anchors-intro \"3. The preceding tag\")



2. Key sequences and how to abort them
======================================
See: (find-enode \"Keys\" \"key sequence\")
     (find-enode \"User Input\" \"Control-a\" \"C-a\")
     (find-enode \"User Input\" \"<META> key\")
     (find-enode \"Completion\" \"<TAB>\")
     (find-enode \"Minibuffer History\" \"<UP>\" \"<DOWN>\")

<ESC> <ESC> <ESC>                (find-enode \"Quitting\")
C-g   keyboard-quit (\"get out\")  (find-enode \"Quitting\" \"C-g\")
M-x   execute-extended-command   (find-enode \"M-x\" \"Running Commands by Name\")

More about buffers:              (find-enode \"Buffers\")
More about the minibuffer:       (find-enode \"Minibuffer\")
More about TAB - for completion: (find-enode \"Completion\")
                for indentation: (find-enode \"Indentation\")
           in programming modes: (find-enode \"Basic Indent\")
More about modes:                (find-enode \"Major Modes\")
                                 (find-enode \"Minor Modes\")
                                 (find-enode \"Dired\")

See also: (find-enode \"Mode Line\" \"(MAJOR MINOR)\")
          (find-enode \"Mode Line\" \" MAJOR is the name\")
          (find-enode \"Mode Line\" \" MINOR is a list\")
          (find-enode \"Mode Line\" \" BUF \")
          (find-enode \"Mode Line\" \" BUF \" \"name of the buffer\")
          (find-enode \"Echo Area\")



3. Cutting & pasting
====================
The \"region\" where cut & copy operate is always what is between
the \"point\" and the \"mark\":

  (find-enode \"Point\")
  (find-enode \"Mark\")
  (find-enode \"Shift Selection\")

You can do cut, copy and paste by using the icons in the toolbar
or by using the menu bar (the relevant options are under
\"Edit\"), but the keys are worth learning:

  C-SPC   -- set-mark-command           (find-enode \"Setting Mark\")
  C-x C-x -- exchange-point-and-mark    (find-enode \"Setting Mark\" \"C-x C-x\")
  C-w     -- kill-region     (cut)      (find-enode \"Other Kill Commands\")
  M-w     -- kill-ring-save  (copy)     (find-enode \"Kill Ring\")
  C-y     -- yank            (paste)    (find-enode \"Kill Ring\")

See: (find-enode \"Tool Bars\")
     (find-enode \"Menu Bar\")
     (find-eev-quick-intro \"5.2. Cutting and pasting\")



4. Moving point
===============
C-a     -- beginning-of-line            (find-enode \"Moving Point\")
C-e     -- end-of-line                  (find-enode \"Moving Point\")
M-<     -- beginning-of-buffer          (find-enode \"Moving Point\")
M->     -- end-of-buffer                (find-enode \"Moving Point\")



5. Undoing
==========
C-/    -- undo    (find-enode \"Basic Undo\")
C-_    -- undo    (find-enode \"Basic Undo\")
                  (find-enode \"Undo\")


6. Windows
==========
See: (find-enode \"Windows\")
     (find-enode \"Frames\")

C-x o   -- other-window                          (find-enode \"Other Window\")
C-x 0   -- delete-window                         (find-enode \"Change Window\")
C-x 1   -- delete-other-windows     (\"1 window\") (find-enode \"Change Window\")
C-x 2   -- split-window-vertically (Above/Below) (find-enode \"Split Window\")
C-x 3   -- split-window-horizontally       (L|R) (find-enode \"Split Window\")
C-x 4 0 -- kill-buffer-and-window                (find-enode \"Change Window\")
C-x +   -- balance-windows                       (find-enode \"Change Window\")



7. Files and buffers
====================
C-x C-f -- find-file                    (find-enode \"Visiting\")
C-x C-s -- save-buffer                  (find-enode \"Saving\")
C-x C-c -- save-buffers-kill-emacs      (find-enode \"Saving\")
C-x b   -- switch-to-buffer             (find-enode \"Select Buffer\")
C-x k   -- kill-buffer                  (find-enode \"Kill Buffer\")
                                        (find-enode \"Dired\")


8. Search and replace
=====================
C-s     -- isearch-forward              (find-enode \"Incremental Search\")
C-r     -- isearch-backward             (find-enode \"Incremental Search\")
M-C-s   -- isearch-forward-regexp       (find-enode \"Regexp Search\")
M-C-r   -- isearch-backward-regexp      (find-enode \"Regexp Search\")
M-%     -- query-replace                (find-enode \"Replace\")



9. Macros
=========
C-x (   -- start-kbd-macro              (find-enode \"Keyboard Macros\")
C-x )   -- end-kbd-macro                (find-enode \"Keyboard Macros\")
C-x e   -- call-last-kbd-macro          (find-enode \"Keyboard Macros\")



10. Other keys (Emacs)
======================
M-q                  -- fill-paragraph       (find-enode \"Fill Commands\")
C-x r <SPC> <char>   -- point-to-register    (find-enode \"Position Registers\")
C-x r j <char>       -- jump-to-register     (find-enode \"Position Registers\")



11. Other keys (eev)
====================
M-F       -- eewrap-find-fline          (find-eev-quick-intro \"`M-F'\")
M-M       -- eewrap-man                 (find-eev-quick-intro \"`M-M'\")
M-S       -- eewrap-sh                  (find-eev-quick-intro \"`M-S'\")
M-A       -- eewrap-anchor              (find-eev-quick-intro \"`M-A'\")
M-B       -- eewrap-escript-block       (find-eev-quick-intro \"`M-B'\")
" pos-spec-list)))

;; (find-emacs-keys-intro)




;;;  _           _        _ _
;;; (_)_ __  ___| |_ __ _| | |
;;; | | '_ \/ __| __/ _` | | |
;;; | | | | \__ \ || (_| | | |
;;; |_|_| |_|___/\__\__,_|_|_|
;;;

;; «find-eev-install-intro» (to ".find-eev-install-intro")
;; Skel: (find-intro-links "eev-install")

(defun find-eev-install-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-eev-install-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-eev-install-intro)
Source code:  (find-efunction 'find-eev-install-intro)
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Note: this intro contains lots of very technical information!
If you're a beginner you should skip this - but if you use
Windows then this page may be relevant to you:
  http://angg.twu.net/installing-eev-on-windows.html




1. Beginners and experts
========================
When I teach Emacs and eev to beginners in workshops I install
Emacs and eev in their machines using a \"beginners setup\" like
the one described here,

  (find-eev-quick-intro \"1. Installing eev\")

that lets them start Emacs displaying the main eev tutorial and
with eev-mode on; then I teach them how to use `M-e' to follow
elisp hyperlinks, and I show them that if they type just `M-j'
they get a page with some nice & important hyperlinks at its
header... then usually in less that five minutes - with a bit of
help - they figure out how to navigate the documentation and how
to do the exercises.

When I show eev to long-time Emacs users I recommend them to
install eev using an \"expert setup\" that is very non-intrusive,
and that does not turn eev-mode on. With eev installed but
eev-mode off all elisp hyperlinks functions are still available,
even the ones that define \"shorter hyperlinks\", like the ones
described in these sections,

  (find-eev-quick-intro \"9. Shorter hyperlinks\")
  (find-pdf-like-intro \"7. Shorter hyperlinks to PDF files\")

and people can execute sexps with `C-x C-e'. People using the
expert setup can turn eev-mode on and off - i.e., activate and
disactivate its keybindings - with `M-x eev-mode', and when
`eev-mode' is on they can go to the main tutorials with:

  `M-5 M-j'  -  (find-eev-quick-intro)
  `M-2 M-j'  -  (find-emacs-keys-intro)

or by following the links at the header of the page displayed by
`M-j'. Try:

      `M-j'  -  (find-eejumps)

I try to assume that long-time Emacsers are too busy with their
other things, and that it's ok if they only spend 5 minutes per
month playing with the eev tutorials.





2. The expert setup
===================
The \"expert setup\" described above corresponds to downloading
eev and then running something equivalent to this:

  (add-to-list 'load-path \"~/path-to-the-eev-source/\")
  (require 'eev-load)

If you installed eev with `M-x list-packages' - see:

  (find-enode \"Packages\")

then the line that adjusts the load-path should not be needed.

Here's what `(require 'eev-load)' does:

  1) Several functions and variables become defined. They ALL
     have the prefixes \"find-\", \"ee\", \"code-\" or \"br\",
     except for one: \"to\".

  2) The standard-display-table is changed to make three
     characters be displayed as colored glyphs: \"\" (char 15),
     \"«\" (char 171), and \"»\" (char 187).

  3) The environment variables \"S\" and \"EEVDIR\" are set.

  4) An innocuous wrapper is installed around an internal
     function used by `man'. See:

       (find-eev \"eev-blinks.el\" \"find-man\")

Eev has very few autoloads, so eev-load.el loads all source files
except for eev-beginner.el. See:

  (find-eev \"eev-load.el\")
  (find-eev \"eev-load.el\" \"autoloads\")




3. The beginner setup
=====================
The \"beginner setup\" corresponds to the expert setup plus this:

  (eev-beginner)

where `eev-beginner' turns eev-mode on and runs:

  (find-eev-quick-intro)

See:

  (find-eev \"eev-beginner.el\")

If you have installed eev by following these instructions

  (find-eev-quick-intro \"1. Installing eev\")

then the script \"~/eev\" will run `(eev-beginner)' for you.

In the beginner setup for Windows people have to execute `M-x
eev-beginner' by hand when they start Emacs.




4. `eev-mode'
=============
Turning on eev-mode simply activates the eev-mode-map keymap, and
adds an \"eev\" to the mode line to remind you this. Turning off
eev-mode deactivates the keymap and the reminder.

See:

  (find-eev \"eev-mode.el\")
  (find-eev \"eev-mode.el\" \"eev-mode\")
  (find-efunctiondescr        'eev-mode)
  (find-eminormodekeymapdescr 'eev-mode)
  (find-efunctiondescr        'eev-avadj-mode)
  (find-eminormodekeymapdescr 'eev-avadj-mode)




5. Ways to download eev
=======================
These are the current ways to download and install eev:

  1. as an Emacs package, with `M-x list-packages'. See this
     video for _very detailed_ instructions:

       http://angg.twu.net/2020-list-packages-eev-nav.html

  2. as an Emacs package, by downloading a file named
     eev-YYYYMMDD.tar from either ELPA or angg.twu.net using
     links like these ones - but you'll have to correct the date:

       https://elpa.gnu.org/packages/eev.html
       https://elpa.gnu.org/packages/eev-20211205.tar

     and then running `M-x package-install-file'.

     If you're on M$ Windows and using Emacs 27.2 then you will
     probably need this method, because of this bug:

       https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-10/msg00482.html
       https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-10/msg00530.html
       https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-10/msg00556.html
       https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-10/msg02148.html

     There is a link to download Emacs28-pretest here:

       (find-windows-beginner-intro \"1. Download and install Emacs\")

  3. by using the script in section 5.1 below, that downloads a
     .tgz from

       http://angg.twu.net/eev-current/eev2.tgz ,

     unpacks it in the directory ~/eev2/ and creates a script
     called \"~/eev\" that starts Emacs loading eev and opening
     the main tutorial,

  4. by downloading the .tgz above and unpacking it, and then
     running these lines yourself:

       (add-to-list 'load-path \"~/path-to-the-eev-source/\")
       (require 'eev-load)
       (eev-mode 1)               ; optional

  5. by downloading the git repo from

       https://github.com/edrx/eev.git

     and then running this:

       (add-to-list 'load-path \"~/path-to-the-eev-source/\")
       (require 'eev-load)
       (eev-mode 1)               ; optional




5.1. Using the tarball
----------------------
This way of installing eev is recommended for people who can't or
who don't want to use Emacs's `M-x list-packages' (from
`package.el') and who are on a *NIX-based system.

To install eev using this method, you should copy and paste the
block of shell commands below into a terminal,

{
  rm -Rv ~/eev
  rm -Rv ~/eev2/
  mkdir  ~/eev2/
  cd     ~/eev2/
  rm -fv eev2.tgz
  wget http://angg.twu.net/eev-current/eev2.tgz
  tar -xvzf eev2.tgz
  {
    echo '#!/bin/sh'
    echo 'cd ~/eev2/ && emacs -l eev-beginner.el --eval=\"(find-eev-quick-intro)\" $*'
  } > ~/eev
  chmod 755 ~/eev
}

and then execute the commands in it. This is explained in this
outdated video (from 2016!):

  http://angg.twu.net/eev-videos/video-eev-quick-0.mp4

In short: you will have to open this URL in a browser,

  http://angg.twu.net/eev-intros/find-eev-install-intro.html#5.1

in a browser, and open a terminal running a shell; then mark the
multi-line \"{ ... }\" block above, copy it to the clipboard with
ctrl-C, and paste it into the shell to run its commands.

You will now have a shell script that you can invoke with

  ~/eev

that starts Emacs, loads eev, and opens the main tutorial - i.e.,
runs `M-x eev-beginner'. Note that if you install eev using this
method then all the files related to eev will be in the directory
~/eev2/, except for the script in ~/eev; if you start Emacs in
the default way then it will not know that it has to look for
stuff in ~/eev2/, and commands like `M-x eev-beginner' or sexps
like

  (require 'eev-load)
  (load \"eev-beginner\")

will not work.

Every time that Emacs gets stuck into something that you don't know
how to leave, or how to undo, you should kill the Emacs window and
start it again by typing \"~/eev\" again in the shell prompt.






5.2. Using the git repository
-----------------------------
The git repository for eev is at:

  https://github.com/edrx/eev.git

Try this:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  rm -Rfv /tmp/eev2
  mkdir   /tmp/eev2/
  cd      /tmp/eev2/ && git clone https://github.com/edrx/eev.git .
  cd      /tmp/eev2/
  # (find-gitk \"/tmp/eev2/\")
  {
    echo '#!/bin/sh'
    echo 'cd /tmp/eev2/ && emacs -l eev-readme.el --eval=\"(find-eev-quick-intro)\"'
  } > /tmp/eev
  chmod 755 /tmp/eev

  /tmp/eev

Note the \"cd ... && git clone URL .\". This is needed because if
we don't specify a directory after the URL in \"git clone\" then
git will create a directory /tmp/eev/, and that would be
incompatible with our convention of creating a script called
\"eev\" (\"/tmp/eev\" in this case).



5.3. Installation quirks
------------------------
There were a couple of situations around may/2019 which I was
helping friends who had installed eev on Windows with `M-x
list-packages' and we found out that we had to add
a `(package-initialize)' to their \".emacs\"s to make things
work... I still need to understand this. See:

  (find-es \"emacs\" \"package-initialize\")

If you have installed both an eev from ELPA and an eev from the
.tgz or from the git repo then one of them will be found first in
the load-path. Check which one!




5.4. `package-install-file'
---------------------------
If installing the latest version of eev from ELPA with `M-x
list-packages' doesn't work you can download the latest version
of eev as a .tar file directly from its ELPA page - here:

  http://elpa.gnu.org/packages/eev.html

and then run `M-x package-install-file' and give it the name of
the local copy of the .tar. See:

  (find-enode \"Package Files\" \"M-x package-install-file\")




5.5. `use-package'
------------------
Some people use non-default package managers for Emacs, like
straight.el and use-package. I have very little experience with
them, but it SEEMS that this is a good recipe for using eev with
`use-package':

;; From:
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00031.html
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00034.html
;; See: (find-eev-install-intro \"5.5. `use-package'\")
;;
(use-package eev
  :straight (:host github :repo \"edrx/eev\")
  :config (progn
           ;; See: (find-eev \"eev-load.el\" \"autoloads\")
           ;; http://angg.twu.net/eev-current/eev-load.el.html#autoloads
           (require 'eev-load)
           ;; (eev-mode 1)     ; optional
           ;; (eev-beginner)   ; optional
           ))





6. `find-eev-install-links'
===========================
There is this,

  (find-eev-install-links)

but I sort of forgot it, and I haven't updated it recently...
note that it is somehow similar to this,

  (find-eev-quick-intro \"7.5. `find-latex-links'\")

and follows most of the same conventions. See:

  (find-elnode \"Init File\" \".emacs\")
  (find-elnode \"Init Examples\")
  (find-elnode \"Init File Examples\")
  (find-eev-install-links \"~/eev2/\" \"~/eev\" \"\" 2 \".emacs\")




7. Eev as an ELPA/MELPA package
===============================
In march 2019 I prepared a first version of an emacs package for
eev to make it installable by `M-x list-packages' - see:

  (find-enode    \"Packages\")
  (find-efaqnode \"Packages that do not come with Emacs\")

and submitted it to the emacs-devel mailing list:

  http://lists.gnu.org/archive/html/emacs-devel/2019-03/msg00433.html

Stefan Monnier answered, and the rest of the discussion happened
off-list. Apparently eev could go into GNU ELPA, but some changes
and clean-ups were needed. I implemented most of what he
proposed/requested, but three of the things that he asked for
would demand changes that would make eev far less elegant and far
less useful for beginners... in rough terms, the code should 1)
be byte-compilable, 2) be compatible with lexical binding, and 3)
have all the autoloads. My reasons for not complying - or for not
complying NOW - are explained in the subsections below.

Btw: except for Stefan's e-mails ***100%*** the feedback that I
received about eev in the last three years came from beginners.
I am not willing to make changes that will make eev
beginner-UNfriendly.

UPDATE: in 2019apr14 eev became a part of ELPA even despite its
quirks!!! Its ELPA page is here:

  http://elpa.gnu.org/packages/eev.html




7.1. Byte-compilation
---------------------
In standard packages all elisp files should be byte-compilable
unless there is a very strong reason - but all eev source files
have a \"no-byte-compile: t\" in their local variables section.
See:

  (find-eevgrep \"grep --color -nH -e no-byte-compile: *.el\")
  (find-elnode \"Byte Compilation\" \"no-byte-compile: t\")

Here is why. Each call to a `code-*' function defines some
functions dynamically - for example, `(code-c-d \"e\" ...)'
defines `find-efile' - and the best way to inspect a function
defined in this way is by using `find-functionpp'. Try:

  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-eev-quick-intro \"9.1. `code-c-d'\" \"(find-code-c-d\")
  (find-code-c-d \"e\" ee-emacs-lisp-directory \"emacs\" :gz)
  (find-efunction-links 'find-efile)
  (symbol-function  'find-efile)
  (find-efunctionpp 'find-efile)
  (find-efunctionpp 'ee-efile)
  (find-evardescr 'ee-edir)

These functions are not byte-compiled, so the contents of their
function cells are lambda expressions, which are easy to read.
Compare:

  (symbol-function  'find-efile)
  (find-efunctionpp 'find-efile)
  (symbol-function  'find-file)
  (find-efunctionpp 'find-file)

The relevant sections in the manual are:

  (find-elnode \"What Is a Function\")
  (find-elnode \"What Is a Function\" \"lambda expression\")
  (find-elnode \"What Is a Function\" \"byte-code function\")
  (find-elnode \"Lambda Expressions\")
  (find-elnode \"Function Cells\")

If we don't byte-compile the modules of eev then beginners will
have a reasonable corpus of functions for which
`find-efunctionpp' shows something readable, which is good to
help them understand the innards of Emacs.

Here is an excerpt of one of my e-mails to Stefan:

  I have the impression - please correct me if I'm wrong - that
  you're proposing to replace the `find-efunctionpp' by something
  more modern. My view is that we have a \"new\" Emacs, that
  enforces byte-compilation and uses several data structures that
  are relatively opaque, built on top on an \"old\" Emacs that
  uses lots of simpler data structures, but in which many things
  are messier and more error-prone. I would love to add to eev
  functions to inspect these new data structures, but the \"old\"
  Emacs is the one that made me fell in love with Free Software
  and that made me spend years trying to convert people... and
  one of the underlying messages of eev is \"look, you can still
  use these simpler things\". Maybe I'm using \"simpler\" in a
  sense that is not very usual, so let me quote a paragraph from
  an article that I wrote about implementing a Forth in Lua:

    \"I've met many people over the years who have been Forth
    enthusiasts in the past, and we often end up discussing what
    made Forth so thrilling to use at that time - and what we can
    do to adapt its ideas to the computers of today. My personal
    impression is that Forth's main points were not the ones that
    I listed at the beginning of this section, and that I said
    that were easy to quantify; rather, what was most important
    was that nothing was hidden, there were no complex data
    structures around with \"don't-look-at-this\" parts (think on
    garbage collection in Lua, for example, and Lua's tables -
    beginners need to be convinced to see these things
    abstractly, as the concrete details of the implementation are
    hard), and everything - code, data, dictionaries, stacks -
    were just linear sequences of bytes, that could be read and
    modified directly if we wished to. We had total freedom,
    defining new words was quick, and experiments were quick to
    make; that gave us a sense of power that was totally
    different from, say, the one that a Python user feels today
    because he has huge libraries at his fingertips.\"

    (From: <http://angg.twu.net/miniforth-article.html>)




7.2. Dynamic binding
--------------------
Dependency on dynamic binding should be avoided - see:

  (find-elnode \"Dynamic Binding\")
  (find-elnode \"Dynamic Binding Tips\")
  (find-elnode \"Lexical Binding\")

but the _default_ function that eev uses for template strings is
intrinsically incompatible with lexical binding. See the comments
in its source file:

  (find-eev \"eev-template0.el\")
  (find-eev \"eev-template0.el\" \"lexical-binding\")

It is possible to use a replacement for it that works in lexical
binding, but I prefer to not make this replacement the default.
See:

  (find-eev \"eev-template0.el\" \"ee-template0-lex\")

See also this tutorial:

  (find-lexical-intro)

I decided to make all the elisp files in eev use dynamic binding
for simplicity, even though this is frowned upon.




7.3. Autoloads
--------------
I decided to mark only one function in eev as autoloadable -
instead of hundreds - and this is very non-standard. See the
comments in:

  (find-eev \"eev-load.el\")

and also:

  (find-eev \"README\")
  (find-eev \"eev-beginner.el\")

See:
  (find-eev \"eev-load.el\")
  (find-eev \"eev-mode.el\")

" pos-spec-list)))

;; (find-eev-install-intro)

;; (find-eev "eev-tlinks.el" "find-eev-update-links")
;; (find-eev "eev-tlinks.el" "find-eev-install-links")
;; (find-eev-update-links)





;;;
;;;   ___  _____   __
;;;  / _ \/ _ \ \ / /
;;; |  __/  __/\ V /
;;;  \___|\___| \_/
;;;
;; This works as an index.
;; Skel: (find-intro-links "eev")
;; «find-eev-intro»  (to ".find-eev-intro")

(defun find-eev-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-eev-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-eev-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-eev-intro\")
Main intros:  (find-eev-quick-intro)
              (find-emacs-keys-intro)
              (find-eepitch-intro)
              (find-here-links-intro)
              (find-refining-intro)
Index to the source files: (find-eev \"eev-load.el\")
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.
The quickest way to open or recreate this is with `M-5 M-0 M-j'.


Here is a list of all the available sandbox-y tutorials that
explain parts and concepts of eev, listed in (a kind of)
recommended reading order. These are the basic ones:

   0. (find-eev-quick-intro)
   1. (find-emacs-keys-intro)
   2. (find-eev-intro)
   3. (find-here-links-intro)
   4. (find-refining-intro)
   5. (find-saving-links-intro)
   6. (find-pdf-like-intro)
   7. (find-eepitch-intro)
   8. (find-audiovideo-intro)
   9. (find-video-links-intro)
  10. (find-videos-intro)
  11. (find-psne-intro)
  12. (find-rcirc-intro)
  13. (find-elisp-intro)
  14. (find-lexical-intro)
  15. (find-multiwindow-intro)
  16. (find-eev-install-intro)

These are things that I am using in workshops:

  17. (find-windows-beginner-intro)
  18. (find-eev-exercises-intro)

These ones explain ideas, conventions, and usage patterns:

  19. (find-escripts-intro)
  20. (find-links-conv-intro)

These are older and more technical versions of sections of the
eev-quick-intro:

  21. (find-eval-intro)
  22. (find-links-intro)
  23. (find-brxxx-intro)
  24. (find-wrap-intro)
  25. (find-eejump-intro)
  26. (find-anchors-intro)
  27. (find-code-c-d-intro)

These are etcs:

  28. (find-templates-intro)
  29. (find-org-intro)
  30. (find-git-intro)

These ones explain advanced features that require extra setup:

  31. (find-kla-intro)
  32. (find-edit-index-intro)
  33. (find-prepared-intro)
  34. (find-bounded-intro)
  35. (find-channels-intro)

This one was used in a video:

  36. (find-three-main-keys-intro)

These ones are obsolete:

  37. (find-emacs-intro)
  38. (find-defun-intro)

For an index of the videos, run:

    (find-1stclassvideos)





1. `eev-mode'
=============
To toggle eev-mode on and off, use `M-x eev-mode'.

The text below is a copy of:

  (find-eev-install-intro \"1. Installing eev by hand\")

Note that eev-mode.el has this commentary:

  ;; Turning on eev-mode simply activates the eev-mode-map keymap, and
  ;; adds an \"eev\" to the mode line to remind you this. Turning off
  ;; eev-mode deactivates the keymap and the reminder.

If you prefer to start with eev-mode off, omit the
line `(eev-mode 1)' above or change it to `(eev-mode 0)'. To
toggle eev-mode on and off, use `M-x eev'. The keybindings in
eev-mode are listed here:

  (find-eev \"eev-mode.el\" \"eev-mode-map-set\")

Years ago eev was a very invasive package that changed several
global settings; now it's the opposite. If you load eev the only
things that happen are:

  1) Several functions and variables become defined. They ALL
     have the prefixes \"find-\", \"ee\", \"code-\" or \"br\",
     except for one: \"to\".

  2) The standard-display-table is changed to make three
     characters be displayed as colored glyphs: \"\" (char 15),
     \"«\" (char 171), and \"»\" (char 187).

  3) The environment variables \"S\" and \"EEVDIR\" are set.

  4) An innocuous wrapper is installed around an internal
     function used by `man' (with `advice-add'). See:

       (find-eev \"eev-blinks.el\" \"find-man\")

TODO: update the text above! I rewrote most
of `(find-eev-install-intro)' in 2019sep29... See:

  (find-eev-install-intro \"1. Beginners and experts\")
  (find-eev-install-intro \"2. The expert setup\" \"Here's what\")
  (find-eev-install-intro \"4. `eev-mode'\")





3. The keybindings
==================
`eev-mode' defines its own meanings for lots of meta-shift-letter
key combinations - which are not normally used by Emacs - and,
besides that, only for:

  `M-e'    (find-eval-intro \"`M-e'\")
  `M-k'    (find-eval-intro \"`M-k'\")
  `M-j'    (find-eejump-intro \"\\neejump\\n\")
  `<f8>'   (find-eepitch-intro \"The main key: <F8>\")

and for several key sequences starting with `M-h'. The two
simplest ways to list the _main_ keys of eev are:

  1) click with the middle mouse button on the \"eev\" in the
     mode line - this is equivalent to:

       (find-efunctiondescr 'eev-mode)

  2) type `M-2 M-j' - this is equivalent to:

       (find-emacs-keys-intro)

These two ways are shown in this screenshot:

  http://angg.twu.net/IMAGES/eev-mode-help-and-M-2-M-j.png

To see _all_ the keybindings, run one of these sexps:

  (find-eev \"eev-mode.el\" \"eev-mode-map-set\")
  (find-ekeymapdescr       eev-mode-map)

If the keybindings in `eev-mode-map' interfere with other
keybindings that you use, the simplest solution is to define a
quick way to turn `eev-mode' on and off. If `M-x eev-mode' is too
long, you can try:

  (defalias 'em 'eev-mode)
  (global-set-key (kbd \"s-e\") 'eev-mode)

The `defalias' above makes `M-x em' equivalent to `M-x eev-mode',
and the `global-set-key' makes the key sequence `<Super>-e' run
`eev-mode'. If you don't know what is the super key, see:

  (find-enode \"Modifier Keys\")
  https://en.wikipedia.org/wiki/Super_key_(keyboard_button)

You can also modify `eev-mode-map' to make it define fewer
keybindings, but this is not so trivial to set up. One way to do
that is explained here:

  (find-eev \"eev-mode.el\" \"when-not-eev-mode-map\")





4. The prefix `find-'
=====================
Some people feel that the functions defined by eev should not use
the prefix `find-', that they should use `eefind-' instead...

The code below can be used to list all the `find-*' functions
defined by eev - including the `find-*' functions defined by
calls to `code-c-d', `code-pdf-page', and friends:

  (require 'dash)
  ;; See: https://github.com/magnars/dash.el#functions

  ;; Tests:
  ;; (find-epp        (assoc (symbol-file 'find-pdf-page 'defun) load-history))
  ;; (setq a-lh-entry (assoc (symbol-file 'find-pdf-page 'defun) load-history))
  ;; (find-epp                    a-lh-entry)
  ;; (find-epp (ee-lh-entry-finds a-lh-entry))
  ;;
  (defun ee-lh-entry-finds (lh-entry)
    \"Filter a load-history entry to keep only the `(defun . find-*)'s\"
    (let* ((a (--filter (consp it) lh-entry))
           (b (--filter (eq (car it) 'defun) a))
           (c (--filter (string-match \"^find-\" (symbol-name (cdr it))) b)))
      (cons (car lh-entry) c)))

  (defun ee-lh-eev-finds ()
    \"Filter the load-history - returns a stripped version with only
  the eev files and the `(defun . find-*)'s in them.\"
    (let* ((lh-eevs (--filter (string-match \"eev\" (car it)) load-history)))
      (-map 'ee-lh-entry-finds lh-eevs)))

  (defun ee-lh-eev-find-functions ()
    \"Return a list of all `find-*' functions defined by eev.\"
    (let* ((a (ee-lh-eev-finds))
           (b (-map 'cdr a))
	   (c (apply 'append b)))
        (-map 'cdr c)))

  ;; Tests:
  ;; (find-epp (ee-lh-eev-finds))
  ;; (find-eppp (ee-lh-eev-find-functions))

It should be possible to use that list of functions to produce an
experimental variant of eev in which all these `find-*' functions
become `eefind-*' functions, and in which there a function that
creates `find-*' aliases for all these `eefind-*' functions. I
will try to implement that a prototype for that in the first
months of 2021, but I am afraid that I won't use it much myself -
I think that this is ugly. If you you like to discuss, test, or
implement parts of this, please get in touch!

" rest)))

;; (find-eev-intro)



;;;  _                         _ _       _
;;; | |__   ___ _ __ ___      | (_)_ __ | | _____
;;; | '_ \ / _ \ '__/ _ \_____| | | '_ \| |/ / __|
;;; | | | |  __/ | |  __/_____| | | | | |   <\__ \
;;; |_| |_|\___|_|  \___|     |_|_|_| |_|_|\_\___/
;;;
;; «find-here-links-intro»  (to ".find-here-links-intro")
;; Skel: (find-intro-links "here-links")

(defun find-here-links-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-here-links-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-here-links-intro)
Source code:  (find-efunction 'find-here-links-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-refining-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



This intro will be merged with
  (find-refining-intro)
at some point...



Eev's central idea is that you can keep \"executable logs\" of
what you do, in a format that is reasonably readable and that is
easy to \"play back\" later, step by step and in any order. We
call these executable logs, or executable notes, \"e-scripts\".

[Video links:]
  (find-eev2019video \"5:41\" \"e-scripts\")
  (find-eev2019video \"12:54\" \"A demo\")





1. Alternating between \"task\" and \"notes\"
=========================================
In the old days log books were always made of paper, and there
was nothing automatic in taking notes with them. We would have to
decide what to write and how to write it, and we would have to
alternate between the \"task\" and \"taking notes\". After many
years of practice _some_ people would learn how to take notes
without distracting themselves much from the task at hand, and
they would learn how to make their notes at the same time concise
and readable enough.

Nowadays, with computers, there are _some_ ways to write logs
automatically - for example, most shells record the commands
given to them - but the output is of low quality.

Eev takes an intermediate stance between \"notes by hand\"
and \"automatic notes\". It is possible to do
\"task\"+\"notes\" with just a few more keystrokes than for
doing just \"task\", but that requires learning some tricks,
and having some practice.



1.1. Reading and writing
------------------------
Learning eev is like learning to read and write. We first learn
_how to read_, and we learn _how to write_ in a second stage,
when we have read a lot and we are able to read what we write.

Learning eev is also like learning to use paper notebooks. It is
much easier to understand the notes and ideas what we wrote
ourselves in our notebooks than to understand what other people
wrote in their notebooks... when we go back to what _we_ wrote we
are able to reconnect with what we were thinking, even when our
notes are quite terse because we did not write down all details -
and we can't do that with other people's notes.

So: we have to first learn how to _read_ executable notes in
order to be able to _write_ our own executable notes, but after
learning the basics we will usually find it much easier to read
our own executable notes than to read other people's notes...

This is very similar to what happens in programming. Programmers
usually takes years, and a lot of effort, to learn to write code
and comments that other people find readable, and what they do
after that is not that they write super-readable code all the
time - instead they _adjust_ the level of readability of their
code depending on the situation: they write the code that will be
used and read by other people in a cleaner, more readable style
with lots of comments, and they write \"throwaway code\" that
they will only run once in a very terse, and often messy, style.

Most of my executable notes are written - or rather, \"appear\" -
when I am learning something and I am switching rather mindlessly
between \"task\" and \"notes\" as I explained in the previous
section. What I write looks like throwaway code, only worse -
because I usually leave lots of unfinished parts intermixed with
the parts that run, without marking clearly which are which...
but when I go through my notes about a task again I usually clean
my notes a bit. For me going through my executable notes about a
task again always involves a bit of _rewriting_ - which is
something that programmers often do with their own code, but that
we don't do much with paper notebooks.

Anyway: be prepared to create executable notes that are almost
unreadable, even by you when you go back to them a few hours
later - that's normal, that's how things are, and you can, and
WILL, rewrite the most useful parts...

Note that this \"intro\" is about writing elisp hyperlinks. The
tricks for writing eepitch blocks and index-anchor pairs are
discussed elsewhere.





2. \"Here\"
=========
In this tutorial we will learn the basic technique for creating
an elisp hyperlink to \"here\" and copying it to our notes.
\"Here\" means the current Emacs buffer; we saw in the main
tutorial that elisp hyperlinks like

  (find-eev-quick-intro \"4. Creating Elisp Hyperlinks\")
  (find-emacs-keys-intro \"3. Cutting & pasting\")
  (find-fline \"~/\")
  (find-eevfile \"\")
  (find-eevfile \"eev-blinks.el\")
  (find-efunctiondescr 'find-file)
  (find-enode \"Modes\")
  (find-elnode \"Defining Functions\" \"(defun foo () 5)\")
  (find-man \"date\")

open eev tutorials (`find-xxx-intro's), directories, files,
descriptions of emacs functions, sections of manuals in \"info\"
format, and manpages. All the elisp hyperlinks above are of the
kind described in the first paragraphs of this section of the
main tutorial:

  (find-eev-quick-intro \"3. Elisp hyperlinks\")

They (usually) create a new buffer, and it is possible to \"go
back\" from that buffer with `M-k' of `M-K':

  (find-eval-intro \"5. Going back\")


[Video links:]
  (find-eev2020video \"2:25\" \"variants that were better behaved\")
  (find-eev2020video \"2:36\" \"in the same window as before\")





3. `find-here-links'
====================
Eev has a function, called `find-here-links' and bound to `M-h
M-h', that is able to distinguish several kinds of \"here\"s.
When we run it it creates a temporary buffer with lots of elisp
hyperlinks, and when we have enough practice we can spot in a
second which of its hyperlinks is the \"hyperlink to here\" that
we want to copy to our notes.

This tutorial is about a _variant_ of `find-here-links' that is
more suitable for beginners.




4. `find-here-links-3'
======================
Suppose that you are in a buffer with something interesting -
\"here\" -, and you want to generate a hyperlink to it and copy
that hyperlink to your notes. Some terminology:

  1. The target of that hyperlink will be the \"here\" buffer, so
     let's call the \"here\" buffer the \"target buffer\" from
     now on.

  2. `find-here-links' creates a temporary buffer with several
     elisp hyperlinks - let's call that buffer the \"elinks
     buffer\".

  3. Beginners start by putting all their (executable) notes in a
     single file, \"~/TODO\"; remember that `M-1 M-j' jumps to
     that file. The \"notes buffer\" is a buffer visiting the
     file \"~/TODO\".

The key sequence `M-h M-3' saves the current window configuration
in a variable called `ee-window-configuration-before-M-h-M-3',
creates a 3-window setting like this,

   _____________________
  |          |          |
  |          |  elinks  |
  |          |  buffer  |
  |  target  |__________|
  |  buffer  |          |
  |          |  notes   |
  |          |  buffer  |
  |__________|__________|

and puts the cursor at the elinks buffer.





5. `find-here-links-1'
======================
After creating the three windows described above we will usually
want to select a line from the elinks buffer - the right one,
i.e., the one with a hyperlink to the target buffer - and copy it
to the notes buffer; the next section explains how to do this.

After copying the hyperlink - or after deciding that we don't
want to copy it - we want to restore the original window
configuration that we had before typing `M-h M-3'. We can do that
by typing `M-h M-1' (`find-here-links-1'); I chose to use the
suffix \"1\" because in most cases the original window
configuration has a single window with the target buffer in it,
and the \"1\" is a reference to this:

  (find-emacs-keys-intro \"6. Windows\" \"C-x 1\")

Note that `M-h M-1' undoes what `M-h M-3' did. In a figure:

   _______________           _____________________           ________________
  |               |         |          |          |         |                |
  |               |         |          |  elinks  |         |                |
  |               |         |          |  buffer  |         |                |
  |    target     | M-h M-3 |  target  |__________| M-h M-1 |     target     |
  |    buffer     | ------> |  buffer  |          | ------> |     buffer     |
  |               |         |          |  notes   |         |                |
  |               |         |          |  buffer  |         |                |
  |_______________|         |__________|__________|         |________________|


[Video links:]
  (find-eevfherelvideo \"4:40\" \"2. The beginner's way\")
  (find-eevfherelvideo \"4:40\" \"2.1. The 3-window setting - and going back from it\")
  (find-eevfherelvideo \"6:47\"  \"`M-h M-3': three windows; `M-h M-1' goes back\")
  (find-eevfherelvideo \"7:29\"  \"the original configuration can be anything\")





6. Copying the hyperlink
========================
When you are a beginner, the easiest way to copy an elisp
hyperlink from the elinks buffer to the notes buffer is to put
the cursor on the line with the hyperlink, then type `M-h M-w'
(`ee-copy-this-line-to-kill-ring'), and then go to the notes
buffer and copy it to there with `C-y' or with the entry \"Edit
-> Paste\" in the menu bar. Note that in the three-window setting
copying a hyperlink from the elinks buffer to the notes buffer
means copying it from the upper right window to the lower right
window:

   _____________________
  |          |          |
  |          |  elinks  |
  |          |  buffer  |
  |  target  |____||____|
  |  buffer  |    \\/    |
  |          |  notes   |
  |          |  buffer  |
  |__________|__________|

When you become a slightly more advanced user the easiest way is
the one with the key sequences described here:

  (find-eev-quick-intro \"5.2. Cutting and pasting\")

[Video links:]
  (find-eevfherelvideo \"8:56\" \"2.2. Copying one link\")





7. Refining your hyperlinks
===========================
After learning the technique above, that was based on the keys:

  M-h M-3  -- find-here-links-3
  M-h M-w  -- ee-copy-this-line-to-kill-ring
  C-y      -- yank, i.e., paste; see: (find-enode \"Kill Ring\")
  M-h M-1  -- find-here-links-1

The next steps are to learn how:

  a) Refine hyperlinks. See:

       (find-refining-intro \"1. Pos-spec-lists\")
       (find-refining-intro \"2. Refining hyperlinks\")

  b) Work with a single window. See:

       (find-refining-intro \"3. Three buffers\")

  c) Use other keys that create buffers with hyperlinks. See:

       (find-emacs-keys-intro \"Some other keys that create\")



8. Debugging
============
The best way to understand the innards of `find-here-links' is to
call it in \"debug mode\". This is explained here:

  (find-eevfile \"eev-hlinks.el\" \"Debug mode\")
" pos-spec-list)))

;; (find-here-links-intro)




;;;            __ _       _
;;;  _ __ ___ / _(_)_ __ (_)_ __   __ _
;;; | '__/ _ \ |_| | '_ \| | '_ \ / _` |
;;; | | |  __/  _| | | | | | | | | (_| |
;;; |_|  \___|_| |_|_| |_|_|_| |_|\__, |
;;;                               |___/
;;
;; «find-refining-intro» (to ".find-refining-intro")
;; Skel: (find-intro-links "refining")

(defun find-refining-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-refining-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-refining-intro)
Source code:  (find-efunction 'find-refining-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-emacs-keys-intro)
              (find-here-links-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



This tutorial supposes that you already know how to
create \"hyperlinks to here\" - see:
  (find-here-links-intro)
  (find-emacs-keys-intro \"and refining them\")




1. Pos-spec-lists
=================
Most hyperlinks functions defined by eev can be \"refined\" by
the addition of extra arguments. These extra arguments are called
a \"pos-spec\" (or a \"pos-spec-list\") and they specify a
position in the target buffer. The first argument means a certain
line number, when it is a number, or the first occurrence of a
certain string, when it is a string. Try:

  (find-enode \"Command Index\")
  (find-enode \"Command Index\" \"eval-last-sexp\")

Further arguments mean either \"move down n lines\" or \"search
for the next occurrence of a string\", depending on whether they
are numbers or strings. Try:

  (find-sh \"seq 2095 2115\")
  (find-sh \"seq 2095 2115\" \"2100\")
  (find-sh \"seq 2095 2115\" \"2100\" \"9\")
  (find-sh \"seq 2095 2115\" \"2100\" 2)

In most cases an empty pos-spec-list, like this,

  (find-sh \"seq 2095 2115\")

means: \"if the target buffer already exists then just open it\"
- so that following that hyperlink would jump to the current
position of the point in that buffer.

Pos-spec-lists are usually interpreted by the function
`ee-goto-position'. The first argument is interpreted in a
special way, according to its type:

    string -> jump to the first occurrence of
              that string in the buffer
    number -> jump to the n-th line

and the other arguments are interpreted (recursively) by
`ee-goto-rest':

    string -> jump to the next occurrence of that string
    number -> move down n lines
    list   -> evaluate the list

Here is an example in which one of the arguments is a list.
Suggestion: execute it with `M-2 M-e'.

  (find-sh \"seq 2095 2115\" \"2100\" \"9\" '(eek \"!!!\"))

There are some variants of `ee-goto-position' in which the first
argument is interpreted in a different way, but we will not
discuss them here. See:

  (find-efunction 'ee-goto-anchor)
  (find-eev \"eev-pdflike.el\" \"ee-goto-position-page\")

If you want to add support for more complex pos-spec-lists, just
replace `ee-goto-rest' with your own extended version.

  [Video links:]
    (find-eev2019video \"1:08\" \"a string to search for\")
    (find-eev2020video \"2:25\" \"variants that were better behaved\")
    (find-eev2020video \"2:36\" \"in the same window\")
    (find-eev2020video \"2:53\" \"pos-spec-lists\")







2. Refining hyperlinks
======================
To _refine_ a hyperlink means to add items to its pos-spec-list
to make it point to a more precise location. For example, in the
links below the first one points to an \"intro\", the second
points to a section in it, and the third points to an important
idea in that section:

  (find-escripts-intro)
  (find-escripts-intro \"5. Tools for writing e-scripts\")
  (find-escripts-intro \"5. Tools for writing e-scripts\" \"alternate\")

The paragraph where the word \"alternate\" is talks about how to
alternate between a \"task\" and \"taking notes\" without losing
focus on the \"task\". Eev has some keys sequences for doing
that:

  M-h M-h   - `find-here-links'. See: (find-eev-quick-intro \"`M-h M-h'\")
  M-h M-2   - `ee-duplicate-this-line'
  M-h M-y   - `ee-yank-pos-spec'
  M-h M-w   - `ee-copy-this-line-to-kill-ring'

Let me explain them starting by the two middle ones. This sexp

  (kill-new \"Tools for\")

puts the string \"Tools for\" on the top of the kill ring; we can
use it to simulate what happens when the user marks a region
containing that string and runs `M-w'. See:

  (find-enode \"Kill Ring\")
  (find-emacs-keys-intro \"3. Cutting & pasting\")

Try the first two sexps below:

  (kill-new \"Tools for\")
  (eek \"<down>  M-h M-2  M-h M-y\")
  (find-escripts-intro)

The `eek' duplicates the line with \"(find-escripts-intro)\" and
then runs `M-h M-y' (`ee-yank-pos-spec') on it; `M-h M-y' is
based on `C-y' (`yank'),

  (find-enode \"Yanking\")

but `M-h M-y' \"yanks\" the text at the end of the sexp, in
quotes, as an new argument.

  [Video links:]
    (find-eevfherelvideo \"10:30\" \"2.3. Duplicating and refining\")
    (find-eevfherelvideo \"10:53\"  \"as a string at the end of the sexp\")




3. Three buffers
================
One very common usage pattern involves three buffers:

  1. a buffer where we are storing our notes (the \"notes
     buffer\").

  2. a buffer with something interesting that we want to create a
     link to (the \"target buffer\"),

  3. a temporary buffer with editable hyperlinks, usually created
     by `find-here-links' (the \"elinks buffer\").

Usually beginners use the file \"~/TODO\" - that can be accessed
with `M-1 M-j' - for all the e-script notes that they create;
learning to keep notes in several files is a skill that comes
later.

Let's take a concrete example. We found something interesting in
section 4 of `(find-escripts-intro)', and we want to keep a link
to that; we decided to use the string \"snip, snip\" to refine
the hyperlink -

  (find-escripts-intro)
  (find-escripts-intro \"4. How to read an e-script\")
  (find-escripts-intro \"snip, snip\")

and we want to produce the refined hyperlink - using the keys
from the previous section -, copy it to our notes buffer, and go
back to the target buffer. The keys to switch buffers are shown
below,

   ________           ________           ________           ________
  |        |         :        :         |        |         |        |
  | target | M-h M-h : elinks : M-1 M-j | notes  | M-K M-K | target |
  | buffer | ------> : buffer : ------> | buffer | ------> | buffer |
  |________|         :________:         |________|         |________|

but there are also some things that we need to do inside each one
of these buffers:

  a. In the target buffer: mark the string \"snip, snip\" and
     copy it to the kill ring with `M-w' (`kill-ring-save');

  b. In the elinks buffer: go to the line with the hyperlink that
     points to the target buffer - `(find-escripts-intro)' -,
     refine it with `M-h M-2 M-h M-y', copy its line to the kill
     ring with `M-h M-w' (`ee-copy-this-line-to-kill-ring');

  c. In the notes buffer: insert the refined hyperlink with
     `C-y' (`yank').

Note: `M-h M-w' (`ee-copy-this-line-to-kill-ring') is a key
sequence intended for beginners. I prefer to use something like
`C-a shift-<down> M-w'.

  [Video links:]
    (find-eevfherelvideo \"12:35\" \"3. Using a single window\")
    (find-eevfherelvideo \"13:19\"  \"go back usually by using\")






4. A tip for beginners
======================
TODO: Rewrite this section! It was mostly superseded by:

  (find-here-links-intro \"4. `find-here-links-3'\")

Some people find the instructions above hard to follow because
they force them to remember lots of things that are off-screen.
If you run the second sexp below,

  (define-key eev-mode-map \"\\M-h\\M-h\" 'find-here-links)
  (define-key eev-mode-map \"\\M-h\\M-h\" 'find-here-links-beginner)

it will rebind the key sequence `M-h M-h' to a variant of
`find-here-links' for beginners that creates this window
configuration when invoked as `M-3 M-h M-h':

   _____________________
  |          |          |
  |          |  elinks  |
  |          |  buffer  |
  |  target  |__________|
  |  buffer  |          |
  |          |  notes   |
  |          |  buffer  |
  |__________|__________|

You can then try to run the instructions in the last section with
all the three buffers visible, switching from one buffer to
another by clicking on their windows. Note that the order is:

   _____________________
  |          |          |
  |         ==> elinks  |
  |          |  buffer  |
  |  target  |____||____|
  |  buffer  |    \\/    |
  |          |  notes   |
  |         <== buffer  |
  |__________|__________|

and note also that, in the terminology of

  (find-escripts-intro \"alternate\" \"task\" \"notes\")

the left side is \"task\" and the right side is \"notes\".

To rebind `M-h M-h' to its original function, run this:

  (define-key eev-mode-map \"\\M-h\\M-h\" 'find-here-links)

or restart Emacs.

The big figure below shows all the key sequences:

   _______________________________________
  |                 |                     |
  |                 |       elinks        |
  |   target    `M-h M-h'   buffer:       |
  |   buffer:   ::::::::>   `M-h M-y      |
  |   `M-w'         |        M-h M-w'     |
  |                 |                     |
  |                 |        ::`M-1 M-j'  |
  |                 |_______ :: __________|
  |                 |        \\/           |
  |                 |                     |
  |             `M-K M-K'   notes         |
  |             <::::::::   buffer:       |
  |                 |       `C-y'         |
  |                 |                     |
  |_________________|_____________________|


[Video links:]
  (find-eevfherelvideo  \"8:56\" \"2.2. Copying one link\")
  (find-eevfherelvideo \"10:10\"  \"when people know a few more keys... C-w and M-w\")
  (find-eevfherelvideo \"10:21\"  \"that correspond to Cut, Copy, and Paste\")





5. Pointing to anchors
======================
We saw in

  (find-eev-quick-intro \"8. Anchors\")
  (find-eev-quick-intro \"8.1. Introduction: `to'\")
  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\")

how to create anchors and how to point to anchors in the same
file with `to', and we saw briefly in

  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\")
  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\" \"to anchors\")

that the extra argument `:anchor' in

  (code-c-d \"eev\" ee-eev-source-directory :anchor)

lets us abbreviate the link below, that points to an anchor,

  (find-eevfile \"eev-blinks.el\" \"«find-wottb»\")

to just this:

  (find-eev \"eev-blinks.el\" \"find-wottb\")

Let's now see how to put this all together. WARNING: this is an
advanced topic!

If you are in a file that has anchors you can use the key
sequence `M-1 M-h M-w' to copy the \"preceding tag\" to the kill
ring, and you can you use an `M-h M--' after the `M-h M-y' in the
hyperlinks buffer to shrink the first sexp below to the second
one:

  (find-eevfind \"eev-blinks.el\" \"find-wottb\")
  (find-eev \"eev-blinks.el\" \"find-wottb\")

The preceding tag and shrinking are explained here, with
exercises:

  (find-anchors-intro \"2. Shrinking\")
  (find-anchors-intro \"2. Shrinking\" \"`M-h M--'\")
  (find-anchors-intro \"3. The preceding tag\")
  (find-anchors-intro \"3. The preceding tag\" \"`M-1 M-h M-w'\")

Here is a diagram of key sequences:

        _____________________________________________
       |                      |                      |
       |                      |                      |
       |    target buffer     |       elinks         |
       |    with anchors      |       buffer         |
       |                  ::::::>                    |
       |     M-1 M-h M-w,     |       M-h M-2,       |
       |         M-h M-3  or  |    || M-h M-y,       |
       |         M-h M-h      |    || M-h M--,       |
       |                      |    || M-h M-w  or    |
       |                      |    ||     M-w        |
       |                      |    ||                |
       |                      |----||----------------|
       |                      |    ||                |
       |                      |    \\/  notes         |
       |                      |        buffer        |
       |                      |                      |
       |                      |           C-y        |
       |                      |                      |
       |                      |                      |
       |______________________|______________________|

If you are intending to learn this, here is a suggestion: do it
in two steps! Start by learning how to create hyperlinks to
anchors in the directory that has the source files of eev, i.e.,

  (find-eevfile \"\")

and only then do things like

  (code-c-d \"mnwa\" \"~/MYNOTESWITHANCHORS/\" :anchor)

but with better names, of course, and learn how to create links
to the anchors in the files in that directory.

NOTE: I use this so much that I got used to typing this sequence
of keys VERY quickly,

  M-1 M-h M-w
      M-h M-h
  (<down> several times)
      M-h M-2
      M-h M-y
      M-h M--
      M-h M-w

but I don't touch-type, and for me it became natural to hold the
meta key down with my left thumb while I type `M-1hwhh' and
`M-h2hyh-hw' _by moving my hands over the keyboard a lot_...

It would be nice to have a way to do this same series of actions using
keys that are good for touch typists. One possibilty is to use a
hydra; see the experimental code here:

  (find-eev \"eev-hydras.el\")

If you are interested in discussing how to make this more usable,
please get in touch!


[Video links:]
  (find-eevfherelvideo \"14:18\" \"4.1. Creating anchors\")
  (find-eevfherelvideo \"15:22\"  \"an index at the beginning of the file\")
  (find-eevfherelvideo \"15:47\"  \"the tutorial also explains ... `M-A'\")
  (find-eevfherelvideo \"16:07\"  \"`M-A' duplicates the line and...\")
  (find-eevfherelvideo \"16:19\"  \"use `M-B' to create something like this block here\")
  (find-eevfherelvideo \"17:02\" \"4.2. The option :anchor for `code-c-d'\")
  (find-eevfherelvideo \"17:21\"  \"here I have one of the source files\")
  (find-eevfherelvideo \"17:30\"  \"here we have an index\")
  (find-eevfherelvideo \"17:47\"  \"this sexp is equivalent to\")
  (find-eevfherelvideo \"18:12\"  \"not all functions created by code-c-d behave in that way\")
  (find-eevfherelvideo \"18:36\"  \"let me give the short explanation\")
  (find-eevfherelvideo \"18:57\"  \"is a button that defines new functions\")
  (find-eevfherelvideo \"19:04\"  \"and this sexp here shows the code\")
  (find-eevfherelvideo \"19:24\"  \"this defun that defines find-eev\")
  (find-eevfherelvideo \"20:04\" \"4.3. Shrinking hyperlinks\")
  (find-eevfherelvideo \"20:25\"  \"it has a demo here\")
  (find-eevfherelvideo \"21:07\" \"4.4. The preceding tag\")
  (find-eevfherelvideo \"21:35\"  \"a variant of `M-h M-w'\")
  (find-eevfherelvideo \"22:06\"  \"it also explains in the echo area what it does\")
  (find-eevfherelvideo \"22:43\"  \"in this example the refined hyperlink is not useful\")
  (find-eevfherelvideo \"22:48\"  \"but here is a demo that makes more sense\")



" pos-spec-list)))

;; (find-refining-intro)





;;;                  _               _ _       _        
;;;  ___  __ ___   _(_)_ __   __ _  | (_)_ __ | | _____ 
;;; / __|/ _` \ \ / / | '_ \ / _` | | | | '_ \| |/ / __|
;;; \__ \ (_| |\ V /| | | | | (_| | | | | | | |   <\__ \
;;; |___/\__,_| \_/ |_|_| |_|\__, | |_|_|_| |_|_|\_\___/
;;;                          |___/                      
;;
;; «find-saving-links-intro»  (to ".find-saving-links-intro")
;; Skel: (find-intro-links "saving-links")
;; Test: (find-saving-links-intro)

(defun find-saving-links-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-saving-links-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-saving-links-intro)
Source code:  (find-efunction 'find-saving-links-intro)
More intros:  (find-eev-quick-intro)
              (find-here-links-intro)
              (find-refining-intro)
              (find-eev-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.


\"Taking executable notes\" consists mainly on:
  a. recording commands sent to shell-like programs,
  b. saving elisp hyperlinks to everything interesting
     that we find.
\"(Generating and) saving links\" is the harder half.
This intro is about how to do that fluently.
It is based on ideas that I tested in this workshop:
  http://angg.twu.net/2021-workshop.html

THIS IS A WORK IN PROGRESS AND IS CURRENTLY A MESS.
This was split from:
  (find-eev-exercises-intro)
Pre-requisites:
  (find-here-links-intro)
  (find-refining-intro)
See also:
  (find-kla-intro)



1. Reading diagrams aloud
=========================
I will use 2D diagrams to represent sequences of actions. In
beginning of this tutorial these diagrams will have lots of
details, but they will become progressively more and more
streamlined; in the last exercises of this intro we will work
with diagrams in which only the details that are very hard to
infer are written down explicitly.

1.1. Key sequences
------------------
I will suppose that you are familiar with the first few sequences
of keys below, and that you are trying to become familiar with
the other ones. The pronounciations at the right are the ones
that we will use in the text of this intro and in its videos.

        Pronounciations
  C-w     cut
  M-w     copy,  or copy into the kill ring
  C-y     paste, or copy from the kill ring, or yank

  C-x 0   delete window
  C-x 1   one window

  M-k     kill buffer
  M-K     bury buffer

  M-j     meta-jay, or list of jumps
  M-1j    jump to notes
  M-2j    jump to emacs keys
  M-5j    jump to main tutorial
  M-21j   show notes at the right
  M-31j   jump to notes at the right

  M-h3    find here links beginner
  M-h1    go back (to the previous window configuration)

  M-hh    find here links
  M-hk    find key links
  M-hf    find function links
  M-he    find extra links

  M-h2    duplicate
  M-hy    refine, or yank into pos-spec
  M-h-    shrink

  M-hw    copy line
  M-1hw   copy last tag

Exercise: all the key sequences above are mentioned here:

  (find-emacs-keys-intro)

check that their pronounciations above make sense. Find the link
to the documentation of each key sequence, follow it, and read
the explanation. A few of the explanations have exercises that
show in practice how those key sequences work - do these exercises.



1.2. Windows and buffer names
-----------------------------
We will use two ways to explain what our abbreviations for buffer
names, like [EX], [EH], and [N], mean: pronounciations and sexps.
For example:

  [EX]    exercises     (find-saving-links-intro)
  [HL]    here-links    (find-here-links)
   [N]    notes         (find-fline \"~/TODO\")

A diagram like this

   _________________        ____________
  |        |        |      |            |
  |        |  [HL]  |      |            |
  |  [EX]  |        |      |    [EX]    |
  |        |________| ---> |            |
  |        |        |      |            |
  |        |  [N]   |      |            |
  |        |        |      |            |
  |________|________|      |____________|

We mean that we started in a window configuration with three
windows, with [EX] at the left half, [HR] at the upper right, and
[N] at the lower right, and then we switched to a window
configuration with a single window displaying [EX].




1.3. Adding keys
----------------
Look at this diagram:

   _________________        ____________
  |        |        |      |            |
  |  [EX]  |  [HL]  |      |    [EX]    |
  |   M-w  |  M-h2  |      |            |
  |        |  M-hy  |      |            |
  |        |  M-w   | M-h1 |            |
  |        |___::___| ---> |            |
  |        |   \\/   |      |            |
  |        |        |      |            |
  |        |  [N]   |      |            |
  |        |  C-y   |      |            |
  |________|________|      |____________|

We added key sequences to the diagram of the previous section,
and we did that in a way that lets us deduce from the diagram in
which buffer each key sequence was typed, and in which order.

For me these diagrams become easy to understand and to remember
if I translate mentally each key sequence to its pronounciation -
like this:

  In the buffer with exercises,
    copy a string to the kill ring;    (1)
  In the buffer with here-links,
    duplicate,
    refine,
    copy;                              (2)
  go to the buffer with notes, and
    paste;                             (3)
  go back to the previous
  window configuration.

This is somewhat similar to reading a score sheet.

In the steps (1), (2), and (3) some information hard to put in
words was omitted from the pronounciation. In (1) we copy to the
kill ring exactly the string that will be used in the refining
step; in (2) we copy to the kill ring two links, one \"original\"
and one that it is refined version; in the step (3) we copy these
two links to the right place in our notes, but the pronounciation
doesn't say where.






2. The base cases
=================
All the methods to create and save elisp hyperlinks that we will
see can be regarded as variants of the base cases 1, 2, and 3.
The base cases 1, 2, and 3 all follow this pattern:

   _______________        _____________________        ________________
  |               |      |          |          |      |                |
  |               |      |         ::> elinks  |      |                |
  |               |      |          |  buffer  |      |                |
  |    target     | M-h3 |  target  |____::____| M-h1 |     target     |
  |    buffer     | ---> |  buffer  |    \\/    | ---> |     buffer     |
  |               |      |          |  notes   |      |                |
  |               |      |          |  buffer  |      |                |
  |_______________|      |__________|__________|      |________________|

They only use two key sequences that change the window
configuration -

  M-h3  -   find here links beginner
  M-h1  -   go back (to the previous window configuration)

and they differ in the steps for editing the link in the elinks
buffer before copying it to the notes buffer.

  In the base case 1:    we do no editing, we just copy
                         one link to the notes buffer,

  In the base case 2:    we duplicate and refine, and we copy
                         two links to the notes buffer,

  In the base case 3:    we duplicate, refine, and shrink, and we copy
                         two links to the notes buffer.

The base cases 2 and 3 require us to copy to the kill ring a
string that will be used for the refining. The operation \"copy a
string from the target buffer to the kill ring\" consists of:

  In the base case 1:    nothing

  In the base case 2:    copy (with M-w)

  In the base case 3:    copy last tag (with M-1hw)

Let's see each one of the three base cases in detail and do one
exercise for each one.




2.1. The base case 1
--------------------
The base case 1 is just this:

   _____________        _____________________        _____________
  |             |      |          |          |      |             |
  |     [T]     |      |    [T]  ::>  [EH]   |      |     [T]     |
  |             |      |          |   M-hw   |      |             |
  |             | M-h3 |          |____::____| M-h1 |             |
  |             | ---> |          |    \\/    | ---> |             |
  |             |      |          |    [N]   |      |             |
  |             |      |          |    C-y   |      |             |
  |_____________|      |__________|__________|      |_____________|

In this case the arrow \"::>\" means just \"switch to the buffer
with elisp hyperlinks\". In the [EH] buffer we copy the line with
the hyperlink that points to the target to the kill ring with
`M-hw' (\"copy this line\"), and then we switch to the notes
buffer and we paste the hyperlink with `C-y' (\"yank\").

If we treat the steps `M-h3', `M-hw' (\"copy this line\"),
`C-y' (\"yank\"), and `M-h1' as \"obvious\" we can omit them from
the diagram, and we get this:

   _____________        _____________________        _____________
  |             |      |          |          |      |             |
  |     [T]     |      |    [T]  ::>  [EH]   |      |     [T]     |
  |             |      |          |          |      |             |
  |             |      |          |____::____|      |             |
  |             | ---> |          |    \\/    | ---> |             |
  |             |      |          |    [N]   |      |             |
  |             |      |          |          |      |             |
  |_____________|      |__________|__________|      |_____________|

One of my reasons for preferring this diagram with less
information is that now the step that means \"copy the hyperlink
to the kill ring\" does not say how to do that - and we can use
either `M-hw' or a more standard way to copy a line to the kill
ring, like `C-SPC <down> M-w'.

Exercise 2.1a:

  Treat this intro as the target buffer. Use the method above to
  generate a link to this target and to copy it to your notes.

Let me introduce another abbreviation that I will use. In the
exercises 2.1b, 2.2b, and 2.2c below we will use a target buffer
that is different from the buffer with the exercises, and instead
of representing what happens in the exercise with five window
configurations, as

   ________      _______      ____________      _______      ________
  |        |    |       |    |     |      |    |       |    |        |
  |  [EX]  | -> |  [T]  | -> | [T]::>[EH] | -> |  [T]  | -> |  [EX]  |
  |        |    |       |    |     |__::__|    |       |    |        |
  |        |    |       |    |     |  \\/  |    |       |    |        |
  |        |    |       |    |     |  [N] |    |       |    |        |
  |________|    |_______|    |_____|______|    |_______|    |________|

I will omit the second and the fourth window configurations and
draw something equivalent to this:

   ________                   ____________                   ________
  |        |                 |     |      |                 |        |
  |  [EX]  | --------------> | [T]::>[EH] | --------------> |  [EX]  |
  |        |                 |     |__::__|                 |        |
  |        |                 |     |  \\/  |                 |        |
  |        |                 |     |  [N] |                 |        |
  |________|                 |_____|______|                 |________|

or, even worse, I will simply expect that people will be able to
take the diagram of the previous exercise, i.e., of the
exercises 2.1a, 2.2a, and 2.3a, that are like this,

                 _______      ____________      _______                
                |       |    |     |      |    |       |               
                |  [T]  | -> | [T]::>[EH] | -> |  [T]  |              
                |       |    |     |__::__|    |       |              
                |       |    |     |  \\/  |    |       |              
                |       |    |     |  [N] |    |       |                
                |_______|    |_____|______|    |_______|              

and convert it mentally to a diagram with \"[EX]\" in the
extremities. So:

Exercise 2.1b:

  Follow this link:

    (find-enode \"Setting Mark\")

  it will open an info page. Treat that info page as the
  \"target\" and use the method above to create a link to that
  target and to copy it to your notes. After that come back to
  this intro using `M-K*' - i.e., \"bury buffer\" as many times
  as needed - or using any other method. Note that this exercise
  starts and ends with a window configuraton in which only the
  buffer \"[EX]\" is shown, so it uses implicitly the
  abbreviation that I've just described.

  [Video links:]
    (find-2021workshop1video \"0:22\" \"The base case 1 is described here\")
    (find-2021workshop1video \"0:52\"   \"The instructions are here\")




2.2. The base case 2
--------------------
The base case 2 is similar to the base case 1 but here we
duplicate and refine the hyperlink. Its diagram is:

   _____________        _____________________        _____________
  |             |      |          |          |      |             |
  |     [T]     |      |    [T]  ::>  [EH]   |      |     [T]     |
  |             |      |          |   M-h2   |      |             |
  |             |      |          |   M-hy   |      |             |
  |             |      |          |____::____|      |             |
  |             | ---> |          |    \\/    | ---> |             |
  |             |      |          |    [N]   |      |             |
  |             |      |          |          |      |             |
  |_____________|      |__________|__________|      |_____________|

Here many obvious (?!?!) steps were omitted. Here's how to read
it aloud including the omitted steps:

  find here links beginner
  In the target buffer:
    copy a string to the kill ring
  In the elisp hyperlinks buffer:
    find the hyperlink to the target
    duplicate
    refine
    copy the original and the refined links to the kill ring
  In the notes buffer:
    paste
  go back to the previous window configuration

Exercise 2.2a:

  Treat this intro as the target buffer and the \"(?!?!)\" as the
  string that will be used in the refinement. Use the method
  above to generate two links - one to this intro and one to the
  first occurrence of \"(?!?!)\" in it - and copy these two links
  to your notes.

Exercise 2.2b:

  Follow this link:

    (find-enode \"Setting Mark\" \"C-x C-x\")

  it will open an info page and search for the first occurence of
  \"C-x C-x\" in it. Treat that info page as the \"target\" and
  use the method above to create:

    1. a link to that target
    2. a refinement of that link that points to the \"C-x C-x\"

  then copy this pair of links to your notes. After that come
  back to this intro using `M-K*' - i.e., \"bury buffer\" as many
  times as needed - or using any other method. Note that here the
  initial and the final window configurations are just \"[EX]\",
  so we are using implicitly the abbreviation that omits some
  window configurations that was explained before the exercise
  2.1b.

  [Video links:]
    (find-2021workshop1video \"1:24\" \"The base case 2\")
    (find-2021workshop1video \"1:39\"   \"What I need to do is slightly\")
    (find-2021workshop1video \"1:55\"   \"This is not yet the link that I want\")



2.3. The base case 3
--------------------
In the base case 2 we edited the hyperlink by doing duplicate and
refine; in the base case 3 we will will edit it by doing
duplicate, refine, _and shrink_.

Very few other sections of this intro depend on this one - so you
may skip this.

The base case 3 will only make sense to people who understand:
anchors, short hyperlinks to anchors, using `M-1hw' to copy the
preceding tag to the kill ring, and using `M--' to shrink a
hyperlink to make it point to an anchor. You can learn these
extra prerequisites here:

  (find-eev-quick-intro \"8. Anchors\")
  (find-eev-quick-intro \"8.1. Introduction: `to'\")
  (find-eev-quick-intro \"8.5. Hyperlinks to anchors in other files\")
  (find-eev-quick-intro \"9. Shorter hyperlinks\")
  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\")
  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\" \":anchor\")
  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\" \":anchor\" \"3)\")
  (find-anchors-intro \"2. Shrinking\")
  (find-anchors-intro \"3. The preceding tag\")

Also, the base case 3 will only _look useful_ to people who can
imagine using anchors in their own files. The main techniques for
creating anchors are described in the two first links below, and
the other three links point to an experimental feature of eev
that I use to move the first half of an index/section pair to the
index section of a file with few keystrokes.

  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\")
  (find-refining-intro \"5. Pointing to anchors\")
  (find-refining-intro \"5. Pointing to anchors\" \"but I don't touch-type\")
  (find-eev \"eev-hydras.el\")

The diagram for the base case 3 is this one. Note - ta-da! - that
we are introducing another abbreviation: we are omitting the
window settings in the extremities.

         _____________________       
        |          |          |      
        |    [T]  ::>  [EH]   |      
        |   M-1hw  |   M-h2   |      
        |          |   M-hy   | 
   ---> |          |   M-h-   | ---> 
        |          |____::____|      
        |          |    \\/    |
        |          |    [N]   |      
        |          |          |      
        |__________|__________|      

One way to pronounce that diagram is:

  go to the target buffer
  find here links beginner
  in the target buffer:
    copy last tag
  in the elisp hyperlinks buffer:
    duplicate
    refine
    shrink
    copy the original and the refined links to the kill ring
  In the notes buffer:
    paste
  go back to the original window configuration

Exercise 2.3a:

  Omitted. Try to understand why! =)

Exercise 2.3b:

  This hyperlink

    (find-eev \"eev-videolinks.el\" \"ee-1stclassvideos-field\")

  points to an anchor in one of the source files of eev. Use it
  to go to that anchor, and pretend that you arrived there by
  accident, found that anchor interesting, and decided that you
  had to put a link to it in your notes. Use the method above to
  create

    1. a link to that file
    2. a link to that anchor in that file

  and then copy the two links to your notes.




The rest will be rewritten



[Video links:]
  (find-2021workshop1video \"0:22\" \"The base case 1 is described here\")
  (find-2021workshop1video \"0:52\" \"The instructions are here\")

[Video links:]
  (find-2021workshop1video \"1:24\" \"The base case 2\")
  (find-2021workshop1video \"1:39\" \"What I need to do is slightly\")
  (find-2021workshop1video \"1:55\" \"This is not yet the link that I want\")

3. Copy from left to right
==========================

3.1. Two basic exercises
------------------------
In the diagrams below the names of the buffers are abbreviated:

  [EX] - (find-eev-exercises-intro)
   [J] - (find-eejumps)
   [N] - notes, i.e., (find-fline \"~/TODO\")
  [EK] - (find-emacs-keys-intro)

   ________         _______          ______________               ________
  |        |       |       |        |       |      |             |        |
  |  [EX]  | M-j   |  [J]  | M-21j  |  [J]  |  [N] | C-x 1 M-K*  |  [EX]  |
  |        | ----> |       | -----> |  M-w ::> C-y | ----------> |        |
  |________|       |_______|        |_______|______|             |________|

   ________         _______          ______________               ________
  |        |       |       |        |       |      |             |        |
  |  [EX]  | M-2j  |  [EK] | M-21j  |  [EK] |  [N] | C-x 1 M-K*  |  [EX]  |
  |        | ----> |       | -----> |  M-w ::> C-y | ----------> |        |
  |________|       |_______|        |_______|______|             |________|

`M-21j' is `M-2 M-1 M-j' written in an abbreviated form -
        mnemonic: \"hold the meta key, type 21j, lift meta\" - and
 `M-K*' means \"type M-K as many times as needed\".

Watch this video and try to reproduce what happens in it:

  (find-2021workshop5video \"0:00\")




3.2. `find-extra-file-links'
----------------------------
Here you will need to understand `code-c-d' and
`find-extra-file-links'. See:

  (find-eev-quick-intro \"9. Shorter hyperlinks\")
  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-audiovideo-intro \"4.1. `find-extra-file-links'\")

The diagram will be this one:

   ________       _______	  _______          ______________               ________
  |        |     |       |	 |       |        |       |      |             |        |
  |  [EX]  |     |  [D]  | M-he  |  [EH] | M-21j  |  [EH] |  [N] | C-x 1 M-K*  |  [EX]  |
  |        | --> |       | ----> |       | -----> |  M-w ::> C-y | ----------> |        |
  |________|     |_______|       |_______|        |_______|______|             |________|

Where [D] is a dired buffer. Watch this video and try to
reproduce what happens in it:

  (find-2021workshop6video \"0:00\")

Do this twice. In the first time you should create elisp
hyperlinks pointing to this directory and this file,

  (find-efile \"play/\")
  (find-efile \"play/\" \"life.el\")

and in the second time you should create elisp hyperlinks to a
directory and a file that you find interesting.





3.3. Invisible text
-------------------
Run this sexp:

  (eek \"3*<up> C-a C-SPC C-e\")

and then use `M-w' to copy the region to the kill ring and `M-hy'
to yank it into this sexp:

  (insert \"\\n\")

Instead of getting the first sexp below you will get the second
one:

  (insert \"\\n\" \"2.3. Invisible text\")
  (insert \"\\n\" \"2.3. Invisible text\\n-------------------\")

That's because the title line contains some invisible text, and
the `M-hy' clears all the properties of the text that it inserts,
including the invisibility property. Invisible text is explained
here:

  (find-elnode \"Invisible Text\")

When you type `C-e' on the title of a section of an \"intro\" the
`C-e' takes you to the right of the line _after_ the invisible
text. If you type `<left> <right>' there this moves the point to
a position before the invisible text. So, if you want to copy the
title of a section of an intro to use in a refinement, use

  C-a C-SPC C-e <left> <right> M-w

instead of:

  C-a C-SPC C-e M-w

Exercise: create a pair of elisp hyperlinks, the first one
pointing to this intro and the second pointing to this section,
and copy the pair to your notes. You'll have to watch the video
to understand some tricky points and you will have to follow the
diagram below.

   ________         _______          _______________               ________
  |        |       |       |        |        |      |             |        |
  |  [EX]  | M-hh  |  [EH] | M-21j  |  [EH]  |  [N] | C-x 1 M-K*  |  [EX]  |
  |  M-w   | ----> |       | -----> |  M-h2 ::> C-y | ----------> |        |
  |        |       |       |        |  M-hy  |      |             |        |
  |________|       |_______|        |________|______|             |________|

[Video links:]
  (find-2021workshop4video \"0:00\")




3.3. Copy from `find-ekey-links'
--------------------------------

  1. use M-5j to go to (find-eev-quick-intro) - a.k.a. \"the main
     tutorial\",
  2. go to the section 4.2 in that main tutorial, and find where it
     says \"Try the eek links below\",
  3. run the eek link that says:

       (eek \"M-h M-k  C-s  ;; isearch-forward\")

  4. it should open a temporary buffer whose buffer name is \"*Elisp
     hyperlinks*\". Find the two lines in that buffer that say:

       # (Info-goto-emacs-command-node 'isearch-forward)
       # (find-enode \"Command Index\" \"* isearch-forward:\")

  5. mark and copy those lines,
  6. use `M-21j' or `M-31j' to open your notes buffer in the right
     window,
  7. paste those lines to your notes buffer.

In a diagram:

          __________         __________              ________________
         |          |       |          |            |        |       |
   M-5j  |   [MT]   |  M-e  |   [EH]   |    M-21j   |  [EH]  |  [N]  |
  -----> | sec. 4.2 | ----> |          | ---------> |       ::>      |
         |          |       |          |  or M-31j  |        |       |
         |__________|       |__________|            |________|_______|


3.4. A variant
--------------
Someone who knows how to use `M-h M-k' (`find-ekey-links') and
who doesn't need to split windows can do essentially the same as
we did in the previous section with fewer keystrokes. Let's see
how.

Exercise:

  1. Type `M-hk C-s' to open a temporary buffer with elisp
     hyperlinks on the key `C-s',
  2. mark, and copy with `M-w', the lines that say:

       # (Info-goto-emacs-command-node 'isearch-forward)
       # (find-enode \"Command Index\" \"* isearch-forward:\")

  3. use `M-1j' to go to your notes buffer,
  4. paste those lines to your notes buffer with `C-y'.

In a diagram:

              ___________          ___________
             |           |        |           |
   M-hk C-s  |    [EH]   |  M-1j  |    [N]    |
  ---------> | mark, M-w | -----> |    C-y    |
             |           |        |           |
             |___________|        |___________|





4. A two-window setting (hard)
==============================
Exercise: copy the diagram below to your notes,

   ____________         _____________          ____________        _____________
  |      |     |       |      |      |        |      |     |      |      |      |
  | [EX] | [T] | M-hh  | [EX] | [EH] | M-21j  | [EH] | [N] | M-K* | [EH] | [EX] |
  |      | M-w | ----> |      | M-h2 | -----> |      | C-y | ---> |      |      |
  |      |     |       |      | M-hy |        |      |     |      |      |      |
  |      |     |       |      | M-w  |        |      |     |      |      |      |
  |______|_____|       |______|______|        |______|_____|      |______|______|

then come back here and type `C-x 1 C-x 3' to split the window.
In the window at the right, go this directory,

  (find-efile \"\")

find the subdirectory \"play\", enter it, find the file
\"life.el\", and inside the file \"life.el\" find the line that
starts with this string, with a space at its end:

  \"(defun life \"

Then follow the instructions in the diagram above to create a
link to the file \"life.el\". Refine that link to make it point
to the first occurrence of the string \"(defun life \" inside the
file \"life.el\", and copy the original link and the refined one
to your notes.

[Video links:]
  (find-2021workshop2video \"0:00\")





1. Saving interesting links
===========================
The documentation of eev says at several places that people
should \"save all interesting links to their notes\". It is
easier to learn that if we split it into these tasks:

  a. save links to your notes
  b. test these links
  c. mark the links that you don't understand
  d. undestand what each link does (with M-h M-f)
  e. recognize which links are interesting


" pos-spec-list)))

;; (find-saving-links-intro)










;;;                  _
;;;   _____   ____ _| |
;;;  / _ \ \ / / _` | |
;;; |  __/\ V / (_| | |
;;;  \___| \_/ \__,_|_|
;;;
;; «find-eval-intro»  (to ".find-eval-intro")
;; Skel: (find-intro-links "eval")
;; (find-TH "eev-article" "hyperlinks")
;; (find-TH "eev-article" "forward-and-back")
;;      http://angg.twu.net/eev-article.html#hyperlinks
;;   file:///home/edrx/TH/L/eev-article.html#hyperlinks
;; (find-efunction 'ee-eval-last-sexp)

(defun find-eval-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-eval-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-eval-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-eval-intro\")
More intros:  (find-eev-quick-intro)
              (find-eepitch-intro)
              (find-eev-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.




Note: this intro is messy and VERY old!
TODO: integrate it with:
  (find-eev-quick-intro \"2. Evaluating Lisp\")
  (find-elisp-intro)




1. The standard way to evaluate Lisp: `C-x C-e'
===============================================
The most important idea in Emacs is that Lisp code can appear
anywhere, and you can evaluate a Lisp expression (a \"sexp\") by
placing the cursor (the \"point\") just after it and typing `C-x
C-e'; the result is then displayed in the echo area. Try it in
the line below, with the point in the three different indicated
positions - you should get different results.

  (+ (* 2 3) (* 4 5))
            ^       ^^
            |       | \\
            6      20  26



2. The end of line and `M-e'
============================
A common operation is to move the point to the end of the current
line, then run `C-x C-e'. That can be done with `C-e C-x C-e',
but eev-mode implements a shorthand for it: `M-e'. Try it here:

  (+ (* 2 3)
     (* 4 5)
     )

`M-e' accepts several different numeric prefixes that alter its
behavior. We are only interested in one of them now - `M-0 M-e'
highlights the sexp for a fraction of a second instead of
executing it. Try it above.

In some rare occasions we might want to run something like `M-e'
but without moving to the end of the line first. Eev-mode
implements a key binding for that: `M-E' (meta-shift-e). As an
exercise, try to use `M-0 M-E' at several positions below, to
highlight the subsexps `(* 2 3)', `(* 4 5)', and `4'.

  (+ (* 2 3) (* 4 5))





3. What to execute, and in what order
=====================================
Note that the order of evaluation may be important:

  (setq a 5)
  (setq a 6)
  (* a a)

By executing `(setq a 5)' and then `(* a a)' above we get 25,
by executing `(setq a 6)' and then `(* a a)' we get 36 - the
current value of `a' is the one of the last `setq' executed.

An exercise: edit the three sexps above to introduce a
`(setq a 22)', then use that sexp and the `(* a a)' to calculate
the square of 22.

Another exercise: just as `setq' sets variables and can override
their previous values, `defun' defines, and redefines, functions.
Execute the sexps below in different orders to obtain the results
25, 36, 50, and 60.

  (setq a 5)
  (setq a 6)
  (defun f (x) (* x x))
  (defun f (x) (* x 10))
  (f a)

MORAL: Elisp code can appear anywhere in any Emacs buffer, but it
is _passive by default_. It only gets executed if we move the
point to the right positions and type `C-x C-e', `M-e', or
similar keys. Sexps can be executed any number of times, in any
order, and can be edited and modified.




4. Elisp hyperlinks
===================
Some Emacs functions can be used as hyperlinks. When sexps like

  (find-file \"/tmp/\")
  (info \"(emacs)Lisp Eval\")
  (describe-function 'find-file)
  (find-function     'find-file)
  (man \"cat\")

are executed they \"open a new page\" - actually, they create a
new buffer, or reuse it if it already exists - and it is usually
possible to \"go back\" by killing the new buffer. However for
some functions, like `man', which by default open a manpage in
another window, \"going back\" would mean something different.

Eev defines several functions to let us use sexps as hyperlinks.
The main conventions on these functions are:

  1) their names start with \"find-\",

  2) calls to them can be \"refined\" with a pos-spec (this will
     be discussed below),

  3) they open the new buffer in the current window (to make it
     easier to \"go back\" after following them - see the next
     section),

  4) they don't display much output in the echo area,

  5) when they create temporary buffers with lots of sexps then:

     a) the first sexp in that buffer is one that can regenerate
        that buffer when executed,

     b) all the sexps are prefixed with the string stored in the
        variable `ee-hyperlink-prefix', to let these sexps be
        pasted into scripts as comments (see below).

Note that sometimes the most obvious name for a hyperlink
function starting with `find-' is already taken by Emacs - for
example, `find-file' and `find-function'. In those cases eev use
other names: `find-fline', `find-efunction', etc. Here are the
eev versions of the links above:

  (find-fline \"/tmp/\")
  (find-node \"(emacs)Lisp Eval\")
  (find-efunctiondescr 'find-file)
  (find-efunction      'find-file)
  (find-man \"cat\")




5. Going back
=============
Web browsers let you follow a hyperlink and then \"go back\".
There are different ways of going back - if you opened the new
page on a new window or tab, then going back means deleting the
new window or tab (or just switching to the old window/tab); if
you opened the new page on the same window/tab, then you need to
use the \"back\" button.

Eev-mode defines two keys for \"going back\": `M-k', that kills
the current buffer, and `M-K', that just hides it (\"buries\" it
in the bottom of the list of all buffers). Try following the link
below with `M-e', then deleting its buffer with `M-k' to go back:

  (find-node \"(emacs)Shell\")

In some cases we know that we may want to go \"forward\" again
after going back, and we may not want to delete the target buffer
- for example, because it would take a while to rebuild it again,
or because we would lose the position of the point there. Most
hyperlink functions in eev are able to reuse a buffer that
\"looks like\" the desired target buffer; the test for
lookalikeness is based on the name of the buffer only. Try to
follow the links below with `M-e', then come back to this buffer
with `M-k', then follow them again. Then try the same thing with
`M-K' instead of `M-k', to see the difference - in the `find-sh'
example below the \"sleep\" takes one second to run, so
revisiting the existing output buffer after a `M-K' is much
quicker than recreating it anew.

  (find-man \"1 bash\")
  (find-sh \"sleep 1; echo 'This was run at:'; date\")




6. Refining hyperlinks
======================
Note: this, and some of the following sections, were rewritten
and moved to:

  (find-refining-intro \"1. Pos-spec-lists\")
  (find-refining-intro \"2. Refining hyperlinks\")

Most hyperlinks functions defined by eev can be \"refined\" by
the addition of extra arguments. These extra arguments are called
a \"pos-spec\" (or a \"pos-spec-list\") and they specify a
position in the target buffer. The first argument means a certain
line number, when it is a number, or the first occurrence of a
certain string, when it is a string. Try:

  (find-node \"(emacs)Command Index\")
  (find-node \"(emacs)Command Index\" \"eval-last-sexp\")

Further arguments mean either \"move down n lines\" or \"search
for the next occurrence of a string\", depending on whether they
are numbers or strings. Try:

  (find-sh \"seq 2095 2115\")
  (find-sh \"seq 2095 2115\" \"2100\")
  (find-sh \"seq 2095 2115\" \"2100\" \"9\")
  (find-sh \"seq 2095 2115\" \"2100\" 2)



7. Pos-spec-lists
=================
[Moved to:] (find-refining-intro \"1. Pos-spec-lists\")

The optional arguments that refine a hyperlink form what we call
a \"pos-spec-list\". For example, the pos-spec-list here has two
elements,

  (find-sh \"seq 2095 2115\" \"2100\" \"9\")

and in most cases an empty pos-spec-list, like this,

  (find-sh \"seq 2095 2115\")

means: \"if the target buffer already exists then just open it\"
- so that following that hyperlink would jump to the current
position of the point in that buffer.

Pos-spec-lists are usually interpreted by the function
`ee-goto-position'. The first argument is interpreted in a
special way, according to its type:

    string -> jump to the first occurrence of
              that string in the buffer
    number -> jump to the n-th line

and the other arguments are interpreted (recursively) by
`ee-goto-rest':

    string -> jump to the next occurrence of that string
    number -> move down n lines
    list   -> evaluate the list

If you want to add support for more complex pos-spec-lists, just
replace `ee-goto-rest' with your own extended version.



8. Anchors and pages
====================
\[See:\] (find-anchors-intro)

Some hyperlink functions, like `find-efunction' and
`find-evariable', jump to specific positions in buffers - the
beginning of the definition of a function or a variable in the
source code - even when their pos-spec-lists are empty, so they
process all their extra arguments with just `ee-goto-rest'.

Other hyperlink functions transform the first argument of a
pos-spec-list in a special way it if is a string - for example,
in `find-available', which is based on `find-Package',

  (find-available \"bash\")
  (find-available \"bash\" \"bash-doc\")

the argument \"bash\" is converted to \"\\nPackage: bash\\n\",
and the two hyperlinks above jump to the description of the
package \"bash\" in the list of the available packages in a
Debian system.

The functions based on `find-anchor' transform an initial string
argument in the pos-spec-list by running `ee-format-as-anchor' on
it [TODO: document this], and the ones based on
`ee-goto-position-page' jump to the n-th \"page\" of a buffer if
the first argument of the pos-spec-list is a number, n; for
example, if n is 234 that will jump to the 233-th formfeed (233
and not 234 because the page 1 is before the first formfeed). For
more on \"pages\", see:

  (find-pdf-like-intro \"PDF-like documents as text\")




9. Producing and refining hyperlinks
====================================
If you are on an Info page, typing `M-h M-i' will create a
temporary buffer containing a header - which we will discuss
later - and several (possibly equivalent) links to that info
page. Something like this:
   ________________________________________________________
  |;; (find-einfo-links)                                   |
  |                                                        |
  |;; (info \"(emacs)Misc Buffer\")                          |
  |;; (find-node \"(emacs)Misc Buffer\")                     |
  |;; (find-enode \"Misc Buffer\")                           |
  |                                                        |
  |                                                        |
  |--:**-  *Elisp hyperlinks*   All L1     (Fundamental)---|
  |________________________________________________________|

These links are meant to be cut & pasted - possibly after
refining them to make them more precise. Let's look first at the
two key sequences that make refining much easier. Remember that
`M-w' (`kill-ring-save') is roughly correspondent to what is
called \"copy\" is most modern interfaces, and `C-y' (`yank') is
roughly correspondent to \"paste\". Both `M-w' and `C-y' operate
on Emacs's \"kill ring\", and to make our examples trivial to
follow we will first put a string on the kill ring:

  (kill-new \"C-y\")
  (car kill-ring)

Now let's see how to refine hyperlinks quickly. `M-h M-2'
duplicates the current line; we will use that to refine a copy of
a working hyperlink, instead of working directly on the original,
and risking breaking it. And `M-h M-y' refines the hyperlink on
the current line by adding a string - the top element in the kill
ring - to its sexp. Try this below; you should be able to convert

  (find-enode \"Kill Ring\")
  (find-enode \"Yanking\")

into

  (find-enode \"Kill Ring\")
  (find-enode \"Kill Ring\" \"C-y\")
  (find-enode \"Yanking\")
  (find-enode \"Yanking\" \"C-y\")

with few keystrokes, as you can leave the Meta key pressed. The
full key sequence for duplicating and refining is `M-h M-2 M-h
M-y', but we can think of it as `M-h2hy'.

Now try a more serious exercise: follow the `(find-enode ...)'
hyperlink below, copy a word or two from its contents to the kill
ring with `M-w', then generate the temporary buffer with
hyperlinks to that Info page with `M-h M-i', then duplicate one
of its hyperlinks with `M-h M-2', refine it with `M-h M-y', and
copy the result to this sandbox with `M-w' (or `C-w') and `C-y'.
As this is a long sequence of instructions, it is better to run
`C-x 1 C-x 2' or `C-x 1 C-x 3' before following the hyperlink, to
keep the instructions visible.

  (find-enode \"Command Index\")




10. More on functions
=====================
A symbol - for example `f' - can be both a variable and a
function; its \"value as a variable\" and its \"value as a
function\" are stored in different places. Try:

  (setq f 2)
  (setq f 5)
  (defun f (x) (* x x))
  (defun f (x) (* 10 x))
  (symbol-value    'f)
  (symbol-function 'f)

This is explained here:

  (find-elnode \"Symbol Components\")
  (find-elnode \"Symbol Components\" \"value cell\")
  (find-elnode \"Symbol Components\" \"function cell\")

The content of a \"function cell\" is _usually_ a lambda
expression. See:

  (find-elnode \"Lambda Expressions\")
  (find-elnode \"What Is a Function\")
  (find-elnode \"What Is a Function\" \"lambda expression\")
  (find-elnode \"What Is a Function\" \"byte-code function\")

Try:

  (setq f 2)
  (setq f 5)
  (set 'f 2)
  (set 'f 5)
  (fset 'f (lambda (x) (* x x)))
  (fset 'f (lambda (x) (* 10 x)))
  (defun f (x) (* 10 x))
  (defun f (x) (* x x))
  (symbol-value    'f)
  (symbol-function 'f)
  (f 4)
  (f f)

  ((lambda (x) (* x x))
   4)
  ((lambda (x) (* 10 x))
   4)



10.4. Quote and backquote
-------------------------
Eev uses backquote a lot and avoids macros.

  (find-elnode \"Backquote\")
  (find-elnode \"Macros\")






11. What else?
==============
Eev-mode defines several other key sequences similar to `M-h
M-i'. You can get the full list here:

  (find-efunctiondescr 'eev-mode)
  (find-efunctiondescr 'eev-mode \"M-h f\")

Try also this:

  (find-efunction-links 'eev-mode)

and for other tutorials like this one, try:

  (find-wrap-intro)
  (find-eepitch-intro)

\[To do: explain M-x ee-hyperlink prefix and how to embed
hyperlinks in scripts]
" rest)))

;; (find-eval-intro)





;;;  _ _       _                                             _   _
;;; | (_)_ __ | | _____        ___ ___  _ ____   _____ _ __ | |_(_) ___  _ __  ___
;;; | | | '_ \| |/ / __|_____ / __/ _ \| '_ \ \ / / _ \ '_ \| __| |/ _ \| '_ \/ __|
;;; | | | | | |   <\__ \_____| (_| (_) | | | \ V /  __/ | | | |_| | (_) | | | \__ \
;;; |_|_|_| |_|_|\_\___/      \___\___/|_| |_|\_/ \___|_| |_|\__|_|\___/|_| |_|___/
;;;
;; «find-links-conv-intro» (to ".find-links-conv-intro")
;; Skel: (find-intro-links "links-conv")

(defun find-links-conv-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-links-conv-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-links-conv-intro)
Source code:  (find-efunction 'find-links-conv-intro)
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



This intro explains some conventions on the names and behaviors
of the hyperlink functions of eev.





1. Security vs. transparency
============================
In a previous tutorial - here:

  (find-eev-quick-intro \"3. Elisp hyperlinks\")

we saw that several kinds of sexps can be used as hyperlinks. For
example, these ones:

  (find-fline \"/tmp/\")
  (find-node \"(emacs)Lisp Eval\")
  (find-efunctiondescr 'find-file)
  (find-efunction      'find-file)
  (find-man \"cat\")

Hyperlinks in a web browser _usually_ take us to a different
page, or to a different position in the same page, and in those
cases it is possible to go back to previous position from there;
but sometimes hyperlinks - or webpage buttons - are associated to
Javascript code, and \"following the link\" then means executing
that code. Web browsers try to make it impossible to have
hyperlinks or webpages that will send out your private
information, or that will put your system in a unusable state.
Security in web browsers is achieved by restricting what the
scripts in a page can do.

Sexp hyperlinks, in contrast, can do essentially anything - and,
instead of _security_, they have _transparency_. The code that a
sexp hyperlink will execute is visible, and users are supposed to
know that sexp hyperlinks with `find-fline', `find-node',
`find-efunctiondescr', etc, are very safe - but the ones that
start with `find-sh' may not be. It is possible to write
something like:

  (find-sh \"<code that deletes all your e-mails>\")

but it is not possible to hide that action behind an
innocent-looking button that says \"click for a larger image\".

So, _any_ elisp sexp can be _used_ as a sexp hyperlink; but
people are only going to follow a sexp hyperlink if they can more
or less predict (quickly!) what that hyperlink is going to do...
Readability is important, so let's take a look at the most common
kinds of hyperlinks.




2. Learning to read hyperlinks
==============================
How can we learn to recognize what each hyperlink sexp does? And
which ones are safe(r), and which ones are not? How do we
classify all hyperlink sexps?

We can start by dividing the hyperlink functions into a fixed set
of \"basic\" ones and an unbounded set of \"non-basic\" ones. In
the buffer generated by

  (find-efunction-links 'find-file)

these hyperlinks

  (find-efunctiondescr 'find-file)
  (find-efunction 'find-file)
  (find-efunctionpp 'find-file)
  (find-efunctiond 'find-file)
  (find-estring (documentation 'find-file))
  (find-estring (documentation 'find-file t))
  (find-fline (symbol-file 'find-file 'defun))

calls \"basic\" eev hyperlink functions, that are just interfaces
to Emacs function slightly tweaked into functions that follow
eev's conventions - they are refinable, use the current window,
etc. But these two,

  (find-enode \"Command Index\" \"* find-file:\")
  (find-elnode \"Index\" \"* find-file:\")

are generated by calls to `code-c-d', as explained here:

  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-eev-quick-intro \"9.1. `code-c-d'\" \"find-code-c-d\")

Check:

  (find-code-c-d \"e\"  ee-emacs-lisp-directory :info \"emacs\")
  (find-code-c-d \"el\" ee-emacs-lisp-directory :info \"elisp\")

The `code-*' functions define hyperlink functions whose names are
of the form `find-{stem}{suffix}', and each of these `code-*'
function has an associated `find-code-*' function that just
displays what the corresponding `code-*' would execute. So one
way to get acquainted to the most common of these suffixes is to
follow these hyperlinks:

  (find-code-c-d      \"CODE\" \"/DIR/\" :info \"INFO\")
  (find-code-pdf-page \"CODE\" \"FILE.pdf\")
  (find-code-pdf-text \"CODE\" \"FILE.pdf\")
  (find-code-audio    \"CODE\" \"FILE\")
  (find-code-video    \"CODE\" \"FILE\")

From these only the functions whose suffixes end with \"sh\" or
\"sh0\" and inherently dangerous; the others are usually safe if
no hacks had been done.

Some hyperlinks functions - for example the ones created by
`code-pdf-page', `code-audio', etc - invoke external programs,
and _may_ behave in bad ways when given unsafe arguments; these
functions are implemented using the low-level functions
`find-bgprocess' and `find-callprocess', which of course are
unsafe too.

Also, the functions `find-*-links', `find-*-intro' and
`find-code-*' simply create temporary buffers, and are thus very
safe - but, as always, think carefully before executing any code
that they generate.




3. Classification
=================
Here's one way to classify the hyperlink functions defined by
eev. It's far from perfect, but that's how they are divided in
the source files.

We start from some observations:

  a) The functions `code-c-d', `code-pdf-page' and
     `code-pdf-text' define new hyperlink functions; we called
     these new hyperlink functions \"short(er) hyperlinks\".

  b) Functions like `find-efunction-links' create temporary
     buffers with hyperlinks using `find-elinks', that is
     described here:

       (find-eev \"eev-elinks.el\" \"find-elinks\")

  c) Functions like `find-latex-links' create temporary buffers
     with hyperlinks plus templated text; they use `find-elinks'
     and `ee-template0', that is described here:

       (find-eev \"eev-wrap.el\" \"ee-template0\")

  d) Functions like `find-sh' and `find-pdf-page' call external
     processes.

  e) `code-c-d' has a debugging function associated to it:
     `find-code-c-d', that shows the \"templated code\" that the
     corresponding `code-c-d' would execute. `code-pdf-page' has
     `find-code-pdf-page' as its associated debugging function,
     and so on.

  f) `find-here-links' and its variants create temporary buffers
     that violate this convention:

       (find-links-intro \"5. The first line regenerates the buffer\")

  g) If we call the hyperlinks in the items above \"non-basic\"
     then we get - by exclusion! - a notion of what are \"basic
     hyperlinks\".

Here is the classification, with the class or idea at the left
and a hyperlink to the source file at the right:

  Basic hyperlinks:                 (find-eev \"eev-blinks.el\")
  External processes:               (find-eev \"eev-plinks.el\")
  `find-elinks':                    (find-eev \"eev-elinks.el\")
  `find-elinks'+`ee-template0':     (find-eev \"eev-tlinks.el\")
  `find-here-links':                (find-eev \"eev-hlinks.el\")
  `code-c-d' and `find-code-c-d':   (find-eev \"eev-code.el\")
  `code-pdf*' and `find-code-pdf*': (find-eev \"eev-pdflike.el\")





4. Elisp hyperlinks: some conventions
=====================================
One of the main ideas of eev is that elisp hyperlinks can be
\"followed\" with `M-e', and we can _usually_ \"go back\" with `M-k'.
We saw a few cases where `M-k' didn't work for going back:

  (find-eev-quick-intro \"3. Elisp hyperlinks\" \"eek\")
  (find-eev-quick-intro \"3. Elisp hyperlinks\" \"find-sh0\")
  (find-eev-quick-intro \"8.1. Introduction: `to'\")
  (find-eev-quick-intro \"9.3. Hyperlinks to PDF files\" \"find-pdf-page\")

Emacs has several functions that _sort_ of can be used as hyperlinks,
but that are kind of messy. For example, this one

  (man \"cat\")

may split the current window or even create another frame, and this
one

  (describe-function 'find-file)

displays a lot of output in the echo area. Compare them with their
eev-ish variants:

  (find-man \"cat\")
  (find-man \"cat\" \"-n, --number\")
  (find-efunctiondescr 'find-file)
  (find-efunctiondescr 'find-file \"RET\")

They:

  1) Have names that start with \"find-\" to indicate that they are from
     eev (practically all functions and variables defined in eev start
     with the prefixes \"find-\" or \"ee\"),

  2) they open the new buffer in the current window (to make it
     easier to go back),

  3) they don't display much output in the echo area,

  4) calls to them can be \"refined\" with a pos-spec.




4.1. Conventions on hyperlinks buffers
--------------------------------------
Emacs has several help commands, whose bindings start with `C-h',
that display their information in (temporary) \"help buffers\" -
and in many cases these help buffers have hyperlinks, that can be
followed by typing <RET> on them. They are explained here:

  (find-enode \"Help\")

An example:

  (find-enode \"Key Help\")
  (eek \"C-h k <down>\")

Eev has something similar, but using the prefix `M-h' and
following very different design decisions. Let's start with a
comparison, between Emacs's `C-h f' (`describe-function') and
eev's `M-h M-f' (`find-efunction-links'). Try:

  \"C-h f   find-file\" -> (describe-function    'find-file)
  \"C-h f   find-file\" -> (find-efunctiondescr  'find-file)
  \"M-h M-f find-file\" -> (find-efunction-links 'find-file)

`describe-function' and `find-efunctiondescr' are similar, but
the second one is better for use with `M-e'; we saw the reasons
in the previous section. `C-h f find-file' produces a buffer with
readable text and \"usual\" hyperlinks that can be followed by
typing RET on them, while `M-h M-f find-file' produces a buffer
like this:

   _____________________________________________________________
  |# (find-efunction-links 'find-file)                          |
  |# (where-is 'find-file)                                      |
  |# (describe-function 'find-file)                             |
  |# (find-efunctiondescr 'find-file)                           |
  |# (find-efunction 'find-file)                                |
  |# (find-efunctionpp 'find-file)                              |
  |# (find-efunctiond 'find-file)                               |
  |# (find-estring (documentation 'find-file))                  |
  |# (find-estring (documentation 'find-file t))                |
  |# (symbol-file 'find-file 'defun)                            |
  |# (find-fline (symbol-file 'find-file 'defun))               |
  |                                                             |
  |# (Info-goto-emacs-command-node 'find-file)                  |
  |# (find-enode \"Command Index\" \"* find-file:\")                |
  |# (find-elnode \"Index\" \"* find-file:\")                       |
  |                                                             |
  |                                                             |
  | -:**-  *Elisp hyperlinks*   All L1     (Fundamental)  ------|
  |_____________________________________________________________|

that looks very cryptic at first. It has lots of elisp
hyperlinks, some of them starting with \"find-\" and following
the conventions explained in section 2, some others not; some of
the sexps in it are easy to understand - try all of them! -,
while others are very technical.

This is an example of an \"elisp hyperlinks buffer\". The
functions that generate elisp hyperlinks buffers, like
`find-efunction-links', follow the convention (1)-(4) from
section 2 plus:

  5) The buffer name is \"*Elisp hyperlinks*\",

  6) The first sexp(s) in the elisp hyperlinks buffer regenerates
     the buffer,

  7) all the sexps are prefixed with the string stored in the
     variable `ee-hyperlink-prefix', to let these sexps be
     pasted into e-scripts as comments.

To understand the role of the first sexp let's look at the next
level of complexity.




4.2. Conventions on templated text
----------------------------------
Some functions, like `find-latex-links', generate buffers that
are composed of a series of elisp hyperlinks, as in the previous
section, followed by some \"templated text\". Try to execute the
two sexps below with `M-2 M-e':

  (find-latex-links)
  (find-latex-links \"/tmp/latextest\")
  (find-latex-links \"~/LATEX/foobar\")

Remember that `M-2 M-e' splits the window like this,
   __________ __________
  |          |          |
  |  source  |  target  |
  |          |          |
  |__________|__________|

where the \"source buffer\" is this one and the \"target buffer\" will be
this in the first case,
   ___________________________________________________________________
  |# (find-latex-links \"{stem}\")                                      |
  |# (find-latex-links \"/tmp/test\")                                   |
  |# (find-eev-quick-intro \"`find-latex-links'\")                      |
  |# (ee-copy-rest 1 '(find-fline \"{stem}.tex\"))                      |
  |                                                                   |
  |% (defun c () (interactive) (find-sh \"pdflatex {stem}.tex\"))       |
  |% (defun d () (interactive) (find-pdf-page \"{stem}.pdf\"))          |
  |% (defun e () (interactive) (find-fline    \"{stem}.tex\"))          |
  |% (defun w () (interactive) (find-texworks \"{stem}.tex\"))          |
  |%                                                                  |
  |\\documentclass{article}                                            |
  |\\begin{document}                                                   |
  |                                                                   |
  |\\end{document}                                                     |
  |                                                                   |
  |                                                                   |
  | -:**-  *Elisp hyperlinks*   All L1     (Fundamental)  ------------|
  |___________________________________________________________________|

In the second case all the \"{stem}\"s are substituted by
\"/tmp/latextest\", and in the third case they are substituted by
\"~/LATEX/foobar\". If you execute the second sexp in that
buffer, that is,

  # (find-latex-links \"/tmp/test\")

you get the buffer with the \"{stem}\"s substituted by \"/tmp/test\".

We can think that `find-latex-links' generates an \"*Elisp
hyperlinks*\" buffer from a template that depends on an argument
`stem', and:

  1) when `stem' is nil it is set to \"{stem}\",

  2) the first sexp corresponds to how `find-latex-links' was
     called (after the substitution of nils above),

  3) the second sexp shows a typical, useful, value for `stem',

  4) we can edit by hand the `stem's in the `find-latex-links'
     sexps in first two lines, and run them to regenerate the
     buffer with the new, hand-edited values.



4.3. Elisp hyperlinks buffers vs. Help buffers
----------------------------------------------
Let's refer to Emacs's help buffers as \"C-h buffers\" and to
eev's elisp hyperlink buffers as \"M-h buffers\". Here is a quick
list of the main differences and conventions; some of them will
be expanded later:

  1) C-h buffers are usually named \"*Help*\", while
     M-h buffers are usually named \"*Elisp Hyperlinks*\";

  2) C-h buffers are generated by functions called \"describe-*\",
     M-h buffers are generated by functions called \"find-*-links\";

  3) C-h buffers may contain \"usual-looking\" links, that can be
     followed by typing RET on them, and this is implemented via
     \"buttons\"; C-h buffers are \"ascii plus text properties\",
     while M-h buffers are plain ascii;

  4) C-h buffers are read-only, while
     M-h buffers are read-and-write. The idea is that we can not
     only follow the hyperlinks in a M-h buffer but also modify
     them - usually by \"refining\" them, like this,

       (find-eval-intro \"Refining hyperlinks\")

     then test the modified versions, and copy-and-paste those
     hyperlinks to other, more permanent places. This is much
     easier to do when we are working in plain ascii; the buttons
     in C-h buffers are non-trivial to create, to edit and to
     save.

  5) C-h buffers are _readable_, while
     M-h buffers may look like (technical) gibberish.

     This is intentional - M-h buffers have a do-it-yourself,
     \"I'm the result of a 5-minute hack\" feeling because most
     of them started just like that, as 5-minute hacks that
     turned out to be useful enough, and only suffered very minor
     changes later on. Try this:

       (find-find-links-links)

     Most `find-*-links' were created from that template - and it
     should be easy to create other ones.

  5) Many `M-h' commands, like `M-h f' and `M-h M-i', generate
     sexp hyperlinks that \"point to where we are now\"; but once
     we are in an M-h buffer this idea - whose basis is:
     from (almost) anywhere in Emacs it should to be easy to
     create a hyperlink to where we are now - changes to:

  6) The first line (the \"top sexp\") of an M-h buffer
     regenerates the buffer. And, at last,

  7) The elisp hyperlinks in M-h buffers are prefixed by the
     string in `ee-hyperlink-prefix'.


" pos-spec-list)))

;; (find-links-conv-intro)






;;;  _ _       _
;;; | (_)_ __ | | _____
;;; | | | '_ \| |/ / __|
;;; | | | | | |   <\__ \
;;; |_|_|_| |_|_|\_\___/
;;;
;; Skel: (find-intro-links "links")
;; «find-links-intro»  (to ".find-links-intro")

(defun find-links-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-links-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-links-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-links-intro\")
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Note: this intro is obsolete!
I need to move some parts of it to other intros and then delete it.
  See: (find-here-links-intro)
       (find-refining-intro)
       (find-templates-intro)
       (find-links-conv-intro \"3. Classification\")
       (find-links-conv-intro \"3. Classification\" \"regenerate\")




1. Functions for templated text
===============================
The function that is used to generate templated text from a
string is called `ee-template0'. The function that generates a
templated text from a list of sexps and strings is called
`ee-links-to-string'. The function that generates an \"*Elisp
hyperlinks*\" buffer from a list of sexps and strings is called
`find-elinks'. They are explained, with examples, in the source
code, at:

  (find-eev \"eev-wrap.el\" \"ee-template0\")
  (find-eev \"eev-elinks.el\" \"find-elinks\")
  (find-eev \"eev-elinks.el\" \"find-elinks\" \"ee-links-to-string\")

Some functions like `find-latex-links' generate buffers with
elisp hyperlinks at the top, some templated text meant to be
saved to a file at the bottom, and with a call to `ee-copy-rest'
separating the top part from the bottom part. The `ee-copy-rest'
is explained in detail here:

  (find-eev \"eev-tlinks.el\" \"ee-copy-rest\")




2. `find-here-links'
====================
The most important of the commands that generates buffers with elisp
hyperlinks - \"M-h commands\", in the terminology explained above - is
`find-here-links', which integrates most of the functionalities of
several more basic M-h commands. We will explain first what it _does_,
then its _usage_.

`find-here-links' is bound to `M-h M-h' to make it very easy to
invoke. If you are reading this then \"here\" means the buffer
\"*(find-links-intro)*\", in a certain position. Try to type `M-h M-h';
you will get a buffer like this,

   ____________________________________________________________
  |# See:                                                      |
  |# (find-links-intro \"`find-here-links'\")                    |
  |# (find-efunctiondescr 'eev-mode \"M-h M-h\")                 |
  |                                                            |
  |# (find-links-intro)                                        |
  |                                                            |
  |                                                            |
  |--:**-  *Elisp hyperlinks*   All L1     (Fundamental eev)  -|
  |____________________________________________________________|

which contains a 3-line header with help links, that we will explain
soon, and then a link to \"here\", i.e., to the buffer
\"*(find-links-intro)*\". Note that the link

  (find-links-intro)

can be \"refined\" to, for example,

  (find-links-intro \"which contains a 3-line header\")

using the tricks described in these sections:

  (find-eval-intro \"Refining hyperlinks\")
  (find-eval-intro \"Producing and refining hyperlinks\")
  (find-eval-intro \"Producing and refining hyperlinks\" \"`M-h M-2'\")
  (find-eval-intro \"Producing and refining hyperlinks\" \"`M-h2hy'\")

but `find-here-links' by itself only produces \"unrefined\" links - so
when we say that `find-here-links' produces links to \"here\" we mean
just \"to the current buffer\", not \"to the a certain position in the
current buffer\".

`find-here-links' works for several kinds of \"here\"s - it works for
intros like this one, for Info pages, for manpages, for files and
directories, for descriptions of Emacs functions and variables, and
for a few other cases. Try the sexps below:

  (find-here-links-test '(find-eval-intro))
  (find-here-links-test '(find-node \"(dir)Top\"))
  (find-here-links-test '(find-enode \"Lisp Eval\"))
  (find-here-links-test '(find-fline \"~/\"))
  (find-here-links-test '(find-eevfile \"eepitch.el\"))
  (find-here-links-test '(find-efunction 'ee-eval-last-sexp))

  (find-here-links-test '(find-efunctiondescr 'ee-eval-last-sexp))
  (find-here-links-test '(find-evardescr      'ee-hyperlink-prefix))
  (find-here-links-test '(find-efacedescr     'eepitch-star-face))

  (find-here-links-test '(find-ecolors \"\\nred\"))
  (find-here-links-test '(find-efaces  \"eepitch-star-face\"))
  (find-here-links-test '(find-customizegroup 'rcirc))

  (find-here-links-test '(find-man \"1 cat\"))
   [^ oops, this test doesn't work on multi-window settings...]

Each one of them tests a different case of `find-here-links',
creating a window setup like this:

   ______________________________________
  |               |                      |
  |               |      target of       |
  |               |        sexp          |
  |     this      |______________________|
  |     intro     |                      |
  |               |  result of running   |
  |               |  find-here-links on  |
  |               |   [target of sexp]   |
  |_______________|______________________|

The cursor is kept at the left window (\"this intro\"), so you
can compare the different cases using just <up>, <down>, and M-e.




3. `find-here-links': usage patterns
====================================
Typically what happens is this. We are putting our notes - eepitch
blocks, hyperlinks, etc - in a certain file; let's refer to it as the
\"e-script\". Then we start to navigate for information, and we find
something interesting. We want to add a link pointing to that
\"something interesting\" to our e-script notes - but for that we need
to produce that sexp hyperlink, and ideally we would like to do that
in a way that does not disturb much our attention. Consider this
diagram [note: it DOES NOT imply that the Emacs frame is split into three
windows - typically we will switch between buffers in a single window]:

   ______________________________________
  |               |                      |
  |               |      something       |
  |              ==>    interesting      |
  |   e-script    |_________||___________|
  |               |         \\/           |
  |               |  result of running   |
  |              <== find-here-links on  |
  |               |   [sthing intrstng]  |
  |_______________|______________________|

and this series of steps:

  0. We start navigating from the e-script buffer, and when we
  1. find something interesting [in another buffer], we
  2. run `find-here-links' at the \"something interesting\" buffer,
  3. identify among the sexps in the find-here-links buffer one that
     points to that \"something interesting\",
  4. copy that sexp to the kill-ring,
  5. go back to the e-script buffer,
  6. insert that sexp into the e-script buffer,
  7. execute that sexp to go back to the \"something interesting\",
  8. continue navigating & doing stuff.

At (8) we are essentially back to (1), but we have added to our
e-script buffer a link to \"something interesting\".

Note that to add refining we need to add a step before (2) and
another one after (3):

  1.9. select a string to search for and copy it to the kill-ring,
  3.1. refine that sexp using the \"string to search for\" (with M-h2hy)

\[TO DO: explain the keys that I normally use to perform all
this; explain why I am not the right person to optimize these
steps - because I can type these key sequences without thinking,
and step (3) sometimes gives several sexps for us to choose from]




4. ee-hyperlink-prefix
======================
`ee-hyperlink-prefix' is both a variable and a function that
helps us set that variable; it started to an experiment on how to
create an alternative to `M-x customize' and ended up becoming
the inspiration for all the `find-*-links' functions.

If you run `M-x ee-hyperlink-prefix' you should get a buffer like
this:

   ___________________________________________________________
  |# (ee-hyperlink-prefix)                                    |
  |# (setq ee-hyperlink-prefix \"# \")                          |
  |                                                           |
  |# (setq ee-hyperlink-prefix \"# \")                          |
  |# (setq ee-hyperlink-prefix \";; \")                         |
  |# (setq ee-hyperlink-prefix \"-- \")                         |
  |# (setq ee-hyperlink-prefix \"// \")                         |
  |# (setq ee-hyperlink-prefix \"% \")                          |
  |                                                           |
  |                                                           |
  |--:**-  *Elisp hyperlinks*   All L1     (Fundamental eev)--|
  |___________________________________________________________|

where the first line regenerates the buffer, the second line sets
the variable `ee-hyperlink-prefix' to its current value, and each
one of the lines after the first blank line sets
`ee-hyperlink-prefix' to one of several fixed common values. If
we change the value of `ee-hyperlink-prefix' with one of the
`setq's and execute the first line again we see that all the
prefixes, plus the argument \"# \" in the second line, change.
Try this, with `M-2 M-e' on each line:

  (progn (setq ee-hyperlink-prefix \"# \") (ee-hyperlink-prefix))
  (progn (setq ee-hyperlink-prefix \"% \") (ee-hyperlink-prefix))




5. The first line regenerates the buffer
========================================
Most of the \"M-h commands\" generate buffers with elisp
hyperlinks in which the the first line \"regenerates the
buffers\". This means two things:

  1. You can copy the first line to your notes, and it will work
     as a link to that buffer. Here are some examples of these
     first lines:

     (find-latex-links \"/tmp/mytest\")
     (find-latex-links \"~/latextest\")
     (find-code-pdf-links \"/usr/local/texlive/2019/texmf-dist/doc/asymptote/\" \"{c}\")
     (find-code-pdf-links \"/usr/local/texlive/2019/texmf-dist/doc/asymptote/\" \"asy\")

     A good way to compare the results of the two
     `find-latex-links' and the two `find-code-pdf-links' sexps
     above is to run them with `M-2 M-e'. The prefix `M-2' to
     `M-e' makes the \"target\" of a sexp be displayed in a
     second window without switching to it. See:

       (find-efunctiondescr 'ee-eval-sexp-eol)
       (find-multiwindow-intro \"find-2a\")

  2. You can change the arguments of the sexp in the first line
     and run it again, and this will regenerate the buffer with
     some modifications. This was explained here:

     (find-eev-quick-intro \"7.5. `find-latex-links'\" \"change the \\\"{stem}\\\"\")
     (find-eev-quick-intro \"11.1. `find-pdf-links'\" \"adjust by hand\")

\[To do: explain how this works in more complex templates.
Example:\]

  (find-find-links-links)
  (find-find-links-links \"\\\\M-u\")
  (find-find-links-links \"\\\\M-u\" \"USERTEST\")
  (find-find-links-links \"\\\\M-u\" \"USERTEST\" \"a\")
  (find-find-links-links \"\\\\M-u\" \"USERTEST\" \"a b\")
  (find-find-links-links \"\\\\M-u\" \"USERTEST\" \"a b c\")




6. Pointing to where we are now
===============================
Several of the `M-h' commands are mainly meant to help us
generate hyperlinks to \"where we are now\": to the current file,
to the current Info page, to the current `find-*-intro', to an
Emacs function or variable we are inspecting, to the current
buffer, and so on. They don't try to be very smart -

\[To do: write this\]

  (find-efunctiondescr 'eev-mode)
  (find-efunctiondescr 'eev-mode \"M-h f\")

                                  (eek \"M-h M-i\")

         (find-enode \"Lisp Eval\")
  (progn (find-enode \"Lisp Eval\") (eek \"M-h M-i\"))

  (eek \"M-h f    ;; find-file-links\")
  (eek \"M-h M-b  ;; find-ebuffer-links\")

  for example, `M-h M-i' generates links to
     the current \"intro\" buffer - like this one - _and_ to the
     current Info page (the \"i\" in `M-h M-i' has two meanings).
     Try:

       (eek \"M-h M-i\")

     you should get something like this:

      ___________________________________________________________
     |# (find-einfo-links \"links\")                               |
     |                                                           |
     |[No \"*info*\" buffer]                                       |
     |                                                           |
     |# (find-links-intro)                                       |
     |                                                           |
     |                                                           |
     |--:**-  *Elisp hyperlinks*   All L1     (Fundamental eev)--|
     |___________________________________________________________|



7. The rest of the buffer
=========================
Several elisp hyperlinks buffers are composed of two parts: a
series of links at the top, and then a template-generated text
that is mean to be copied to somewhere else. The canonical
example is

  (find-eev-update-links)

which ends with stuff that you can copy to your .emacs file to
make Emacs load eev by default. The end of the buffer generated
by `find-eev-update-links' looks more or less like this:

   ____________________________________________________________
  |# (ee-copy-rest 0 '(find-fline \"~/.emacs\"))                 |
  |                                                            |
  |;; Load eev2.                                               |
  |;; See:  (find-file \"~/eev/\")                               |
  |;;       (find-file \"~/eev/eev-readme.el\")                  |
  |;; Generated by: (find-eev-update-links \"~/eev/\")           |
  |;;                                                          |
  |(add-to-list 'load-path \"~/eev/\")                           |
  |(require 'eev2-all)      ; (find-eev \"eev2-all.el\")         |
  |(eev-mode 1)             ; (find-eev \"eev-mode.el\")         |
  |                                                            |
  |                                                            |
  |--:**-  *Elisp hyperlinks*   Bot L56    (Fundamental eev)---|
  |____________________________________________________________|

The line with `ee-copy-rest' is a hack. Its first argument is a
number, that we will call the \"skip\", and the second is
a (quoted) sexp hyperlink, that we will call the \"code\". The
rule that defines what is the \"rest of the buffer\" is this:

  Move to the beginning of the next line, then skip (i.e., move
  down) more SKIP lines. The rest of the buffer is everything
  from that point on.

A sexp like `(ee-copy-rest ...)' does several things:

  1) it highlights the rest of the buffer temporarily (like as
     with `M-0 M-e'),

  2) it copies the rest of the buffer to the kill ring (like as
     with `M-w'),

  3) it runs CODE to open its target in a window at the right
     side (like as with `M-3 M-e')

\[To do: add examples - including examples that let us create Lua
scripts etc\]


" rest)))

;; (find-links-intro)

;; (find-eevfile "eev-template.el" "defun find-efunction-links")





;;;                  _ _       _
;;;   ___  ___ _ __ (_) |_ ___| |__
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \
;;; |  __/  __/ |_) | | || (__| | | |
;;;  \___|\___| .__/|_|\__\___|_| |_|
;;;           |_|
;;
;; «find-eepitch-intro»  (to ".find-eepitch-intro")
;; Skel: (find-intro-links "eepitch")
;; (find-eev "eepitch.readme")

(defun find-eepitch-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-eepitch-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-eepitch-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-eepitch-intro\")
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-wrap-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial (for eepitch) and a sandbox.




This intro _complements_ the material in:
  (find-eev-quick-intro \"6. Controlling shell-like programs\")
For a good visual introduction to eepitch, see this page:
  http://angg.twu.net/eepitch.html

My video for the EmacsConf2019 has a simple demo of eepitch:
  https://www.youtube.com/watch?v=86yiRG8YJD0&t=956
  http://angg.twu.net/emacsconf2019.html

This (old) video shows a demo like the one in section 1.3:
  https://www.youtube.com/watch?v=Lj_zKC5BR64&t=16s
The relevant part is from t=16s to t=25s.

In this intro we suppose that the reader knows what is a terminal
and what is a shell. In Unix-like systems the terminal and the
shell are clearly different programs, and it's easy to understand
how a terminal can be used to run other programs that are not
shells (e.g., a Python interpreter; see \"REPL\" below); in
Windows most people don't know that the \"DOS window\" is in fact
a Windows console running cmd.exe. Some links:
  https://en.wikipedia.org/wiki/Pipeline_(Unix)
  https://en.wikipedia.org/wiki/Unix_philosophy
  https://en.wikipedia.org/wiki/Unix-like
  https://en.wikipedia.org/wiki/Shell_(computing)
  https://en.wikipedia.org/wiki/Shell_(computing)#Text_(CLI)_shells
  https://en.wikipedia.org/wiki/Shell_script
  https://en.wikipedia.org/wiki/Command-line_interface
  https://en.wikipedia.org/wiki/Command-line_interface#Command-line_interpreter
  https://en.wikipedia.org/wiki/Read-eval-print_loop (\"REPL\")
  https://en.wikipedia.org/wiki/Terminal_emulator
  https://en.wikipedia.org/wiki/Text_terminal
  https://en.wikipedia.org/wiki/MS-DOS#Windows_command-line_interface
  https://en.wikipedia.org/wiki/Windows_Console
  https://en.wikipedia.org/wiki/Cmd.exe





1. Some demos
=============
Let's start with the simplest case. If you put the cursor on the
first line that starts with a red star below and type the key
<f8> six times,

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
echo \"We are at: $PWD\"
cd /tmp/
echo \"We changed to: $(pwd)\"

you will notice that each <f8> does something with the current
line and move the cursor to the next line; first three <f8>s - on
the lines that start with red stars - create a window setting
like this,

   ________________________________
  |                |               |
  |     notes      |    target     |
  |     buffer     |    buffer     |
  |  (this intro)  |  (\"*shell*\")  |
  |                |               |
  |________________|_______________|

and the last three <f8>s - on \"non-red star lines\" - send the
lines

  echo \"We are at: $PWD\"
  cd /tmp/
  echo \"We changed to: $(pwd)\"

to the \"target buffer\", that in this case is the buffer with a
terminal running a shell; the shell behaves exactly is if the the
user had typed those three lines at its prompt.




1.1. Another target
-------------------
If you put the cursor at the first red star line below and type
<f8> six times you will get something very similar to the example
above,

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
1 + 2
print(\"Hello \" +
      \"world\")

but now the window setting will be like this:

   ________________________________
  |                |               |
  |     notes      |     target    |
  |     buffer     |     buffer    |
  |  (this intro)  |  (\"*python*\") |
  |                |               |
  |________________|_______________|

and the target buffer will be called \"*python*\", and it
contains a terminal running a Python interpreter.




1.2. Two targets
----------------
The demo below uses an advanced feature - the function
`find-3EE', explained at:

  (find-multiwindow-intro \"find-3EE\")

to create a 3-window setup like this:

   _______________________
  |          |            |
  |          |  *shell*   |
  |  notes   |____________|
  |  buffer  |            |
  |          |  *python*  |
  |__________|____________|

Some non-red star lines in it send the current line to the
\"*shell*\" buffer, and some send the current line to the
\"*python*\" buffer. The red star lines with \"(eepitch-shell)\"
set the target to \"*shell*\", and the red star lines with with
\"(eepitch-python)\" set the target to \"*python*\". Try it! Put
the cursor on the first red star line below, then type <f8>
twelve times:

 (find-3EE '(eepitch-shell) '(eepitch-python))
 (eepitch-shell)
echo Hello... > /tmp/o

 (eepitch-python)
print(open(\"/tmp/o\").read())

 (eepitch-shell)
echo ...and bye >> /tmp/o

 (eepitch-python)
print(open(\"/tmp/o\").read())




1.3. Two targets, two windows
-----------------------------
The demo below is similar to the one with three windows in the
previous section, but it uses just two windows - and the window
at the right alternates between \"*shell*\" and \"*python*\":

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
echo Hello... > /tmp/o

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
print(open(\"/tmp/o\").read())

 (eepitch-shell)
echo ...and bye >> /tmp/o

 (eepitch-python)
print(open(\"/tmp/o\").read())






2. How <f8> works
=================
The key <f8> works in one way when the cursor is on a line that
starts with a red star - it executes everything at the right of
the \"\" as Lisp code, and then moves down - and in a totally
different way on non-red star lines: on non-red star lines it
makes sure that the target buffer is being displayed, then sends
the current line to the target buffer \"as if the user had typed
it\", then moves down.




2.1. Eepitch blocks
-------------------
A block of three red star lines like

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

or

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)

is called an \"eepitch block\". The _final effect_ of typing <f8>
thrice on an eepitch block like this

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

is easy to describe: after the third <f8> we get a window setting
like this,

   ________________________
  |          |             |
  |  notes   |    target   |
  |  buffer  |    buffer   |
  |          | (\"*shell*\") |
  |          |             |
  |__________|_____________|

where the target buffer is running a _new_ shell...



2.2. `(eepitch-kill)'
---------------------
The effect of running <f8> on a line like

 (eepitch-kill)

is to kill the current target. More precisely, `(eepitch-kill)'
kills a buffer with the name stored in the variable
`eepitch-buffer-name', if a buffer with that name exists; in the
examples above the target buffer names are always either
\"*shell*\" or \"*python*\". If we are in a window setting like
this and the target is \"*shell*\"

   ________________________
  |          |             |
  |  notes   |    target   |
  |  buffer  |    buffer   |
  |          | (\"*shell*\") |
  |          |             |
  |__________|_____________|

and we run `(eepitch-kill)' the window setting becomes this:

   _____________________
  |          |          |
  |  notes   |   some   |
  |  buffer  |  other   |
  |          |  buffer  |
  |          |          |
  |__________|__________|

which may be confusing...




2.2. `(eepitch-shell)'
----------------------
The effect of running <f8> on a line like

 (eepitch-shell)

can be *roughly* described as:

  a) Set the name of the target buffer to \"*shell*\".

  b) If the target buffer does not exist, create it - by
     running `(shell)'.

  c) If the target buffer is not being displayed then display it
     - by creating a two-window setting with the target buffer at
     the right.

This is a simplification, though... the sexp

  (eepitch-shell)

runs this,

  (eepitch '(shell))

and the name of the target buffer is obtained from the
sexp `(shell)' by running it in a certain way.




2.3. `eepitch'
--------------
The documentation for `eepitch' says:

  (eepitch CODE)

  Set up a target for eepitch and make sure it is displayed in
  another window.
  The argument CODE must be a \"shell-like sexp\", i.e., one that
  when evaluated always switches to a buffer with a fixed name,
  and when that buffer does not exists it creates it.

For example, running `(shell)' switches to a buffer whose name is
\"*shell*\"; the name of the target buffer can obtained
from the sexp `(shell)' by running this:

  (save-window-excursion
    (shell)
    (setq eepitch-buffer-name
	  (buffer-name (current-buffer))))




2.4. `(eepitch-python)'
-----------------------
The effect of running <f8> on a line like

 (eepitch-python)

is very similar to `(eepitch-shell)', but it uses \"*python*\" as
the name of the target buffer. `(eepitch-python)' is defined as:

  (eepitch '(find-comintprocess \"python\" \"python\"))




2.5. `find-comintprocess'
-------------------------
The sexp

  (find-comintprocess \"buffer name\" \"program and args\")

switches to a buffer called \"*buffer name*\" and if that buffer
does not have an associated process then it runs \"program and
args\" there in comint mode. Comint is explained here:

  (find-enode \"Shell Mode\")

The sexp

  (eepitch-comint \"buffer name\" \"program and args\")

works as an abbreviation for:

  (eepitch '(find-comintprocess \"buffer name\" \"program and args\"))

Most `eepitch-<lang>' functions are defined using
`eepitch-comint'. See:

  (find-eev \"eepitch.el\" \"eepitch-langs\")
  (find-eev \"eepitch.el\" \"find-comintprocess\")
  (find-eev \"eepitch.el\" \"find-comintprocess\" \"defun eepitch-comint \")



2.6. `find-vtermprocess'
------------------------
Some programs don't run well inside comint buffers, but run well
inside other terminal emulators that are harder to set up but
that handle more escape sequences, like vterm:

  https://github.com/akermu/emacs-libvterm

Most `eepitch-<lang>' functions are defined using
`eepitch-comint' and `find-comintprocess'; the `eepitch-<lang>'
functions that need vterm are defined using `eepitch-vterm' and
`find-vtermprocess'. See:

  (find-eev \"eepitch.el\" \"other-terms\")
  (find-eev \"eepitch.el\" \"eepitch-langs-vterm\")
 




3. Test blocks
==============
Suppose that we have a file \"foo.py\" containing this (without
the indentation):

  def square (x):
      return x*x

  \"\"\"
   (eepitch-python)
   (eepitch-kill)
   (eepitch-python)
  execfile(\"foo.py\", globals())
  print(square(5))

  \"\"\"

Python treats everything between the first and the second
`\"\"\"'s as a multiline comment, and ignores it - but for us
this multiline comment contains an eepitch block that starts a
Python interpreter, then a line that loads \"foo.py\" in it, then
a line that tests the function \"square\" defined in foo.py. We
call the block between the `\"\"\"'s a \"test block\".

A \"test block\" is a multiline comment in a Python script, a Lua
script, or in a script in one of the other supported languages -
we call them the \"ambient script\" and the \"ambient language\"
- that contains at least:

  1) an eepitch block that runs an interpreter for the ambient
     language,

  2) a line that loads the ambient script in that interpreter,

  3) code that tests functions defined in the ambient script.

We can insert a test block in the current buffer by running `M-x
ee-insert-test', or `M-x eeit'. The current implementation of M-x
ee-insert-test' uses the name of the major mode to decide which
other function to call. If you are in a buffer in which the value
of the variable

  major-mode

is `FooBar-mode' then `M-x eeit' tries to run the function
`ee-insert-test-FooBar-mode', and yields an error if that
function does not exist. To add support for `FooBar-mode' to `M-x
eeit', just define a function with the right name. See the source
for examples:

  (find-eev \"eev-testblocks.el\" \"examples\")

My presentation at the EmacsConf2021 was about test blocks.
Links:

  Pages:  http://angg.twu.net/emacsconf2021.html
          https://emacsconf.org/2021/talks/test/
  Slides: http://angg.twu.net/LATEX/2021emacsconf.pdf
  Video:  https://emacsconf.org/2021/talks/test/
          (find-eev2021video \"0:00\")




3.1. `find-eeit-links'
----------------------
If you run this,

  (find-eeit-links 'lua-mode)

you will get a buffer with:

  a) links to inspect the current definition of
     `ee-insert-test-lua-mode',

  b) links that let you compare that with the `ee-insert-test-'s
     for other major modes,

  c) a barebones `(defun ...)' that lets you redefine
     `ee-insert-test-lua-mode'.

If you run `find-eeit-links' interactively with `M-x' then it
will run as:

  (find-eeit-links <current-major-mode>)

and you can use that to inspect the `ee-insert-test-' support for
the current major mode, or to implement it yourself.




3.2. Test blocks as documentation
---------------------------------
I found that test blocks are a really good way to document my
programs. Most people think that they look very alien at first,
but they understand them immediately when they see a demo - so
here are some demos. You need to have lua5.1 in your path to run
them; they use eepitch-lua51, that calls lua5.1. So try this
first:

 (eepitch-lua51)
 (eepitch-kill)
 (eepitch-lua51)
  print(\"Hello!\")
  for k,v in pairs(os) do print(k, v) end
  os.exit()

If it works then try the demo below. Note that eepitch treats the
lines with two red stars as comments; the sexps in \"\"-lines
are hyperlinks, and the ones in \"\"-lines are not.

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  rm -Rv /tmp/dednat6/
  mkdir  /tmp/dednat6/
  cd     /tmp/dednat6/
  wget http://angg.twu.net/dednat6-minimal.zip
  unzip dednat6-minimal.zip

 (code-c-d \"dn6lua\" \"/tmp/dednat6/dednat6/\" :anchor)
 (setenv \"LUA_INIT\" \"@/tmp/dednat6/dednat6/edrxlib.lua\")
 (find-dn6lua \"edrxlib.lua\")
 (find-dn6lua \"treetex.lua\" \"TreeNode-tests\" 3)
 (find-dn6lua \"rect.lua\" \"dedtorect-tests\" 3)




3.3. `eepitch-preprocess-line'
------------------------------
The key <f8> is bound to `eepitch-this-line'. You can see the
source code of that function by following these hyperlinks:

  (find-eev \"eepitch.el\" \"eepitch-this-line\")
  (find-efunction 'eepitch-this-line)

The source of `eepitch-this-line' contains this mysterious setq:

  (let ((line (buffer-substring (ee-bol) (ee-eol))))
    (setq line (eepitch-preprocess-line line))
    ...
    )

By default `eepitch-preprocess-line' is a no-op that simply
returns this argument unchanged. Its definition is just this:

  ;; See:
  ;; (find-eepitch-intro \"3.3. `eepitch-preprocess-line'\")
  (defun eepitch-preprocess-line (line) line)

Remember that the behavior of <f8> is usually described in
human-friendly terms as:

  \"lines starting with two red stars are treated as comments,
   lines starting with a red star are executed as lisp, 
   and other lines are sent to the target buffer.\"

The function `eepitch-preprocess-line' is a stub that lets us
change in arbitrary ways what is the \"line\" that is processed
in the sense above. Let's see a simple example. Try:

  (replace-regexp-in-string \"^abc\" \"\"    \"foo\")
  (replace-regexp-in-string \"^abc\" \"\" \"abcfoo\")
  (replace-regexp-in-string \"^abc\" \"\" \"abcfooabc\")
  (replace-regexp-in-string \"^abc\" \"\"    \"fooabc\")

A `(replace-regexp-in-string \"^abc\" \"\" ...)' deletes an
initial \"abc\" from a string if that string starts with \"abc\",
but returns other strings unchanged. So, if we redefine
`eepitch-preprocess-line' in this way,

  (setq eepitch-preprocess-regexp \"^#: \")
  (defun eepitch-preprocess-line (line)
    (replace-regexp-in-string eepitch-preprocess-regexp \"\" line))

then the

  (let ((line (buffer-substring (ee-bol) (ee-eol))))
    (setq line (eepitch-preprocess-line line))
    ...
    )

in the source of `eepitch-this-line' will first set `line' to the
string in the current line between the beginning-of-line and the
end-of-line, and then if `line' starts with \"#: \" that prefix
is deleted from it; and it is this \"line after removing the
prefix\" that is processed according the the rules of two red
stars/one red star/no red stars.

Now let's see a practical example. Gnuplot does not support
multiline comments, and using exactly the hack above I can make
<f8> ignore the prefix \"#: \". Then a block like this in a
Gnuplot file

#:  (eepitch-shell)
#:  (eepitch-kill)
#:  (eepitch-shell)
#: gnuplot
#: load \"foo.plt\"
#: plot sin(x)

works as a test block. Running

  (setq eepitch-preprocess-regexp \"^\")

or

  (defun eepitch-preprocess-line (line) line)

disables the hack. A similar technique for using test blocks in
makefiles is explained here:

  http://angg.twu.net/eev-make.html
  (find-2022eevmake0video)

Running `M-x eeit' in a makefile runs
`ee-insert-test-makefile-mode', that inserts a test block like
this:

  # See: (find-eepitch-intro \"3.3. `eepitch-preprocess-line'\")
  # (setq eepitch-preprocess-regexp \"^\")
  # (setq eepitch-preprocess-regexp \"^#T \")
  #
  #T  (eepitch-shell)
  #T  (eepitch-kill)
  #T  (eepitch-shell)
  #T make -f nameofthismakefile TARGET

The lines that start with just \"# \" serve as a reminder that
you need a special setup to make that test block work.

Some languages have syntaxes for comments that are much more
eepitch-unfriendly and test-blocks-unfriendly than this. An
extreme example is SmallTalk, in which comments are delimited by
double quotes and can't contain double quotes. It should be
possible to use `eepitch-preprocess-line' to add support for test
blocks in SmallTalk source files - but I haven't tried that yet.














-=-=-=-=-
Old stuff:



1. Motivation
=============
Suppose that we have to do some reasonably complex task using a
shell, and that we want to take notes of what we do because we
might have to do something similar later.

The two usual ways to interact with a shell are:

  1) through a _script_, that is, by preparing in advance all
     commands to be executed, putting them in a script file, and
     then running that file,

  2) _interactively_, by typing the commands one by one on a
     shell prompt.

Suppose that we have to discover which commands to run as we go;
that rules out preparing a script beforehand, so we need to use
the shell interactively. After issuing the right commands, the
two usual ways to retrieve what we did are:

  a) through the _shell history_, which records the last commands
     that the shell received,

  b) by looking at the full _transcript_ of our interaction with
     the shell.

The way (a) gets a list of commands, without comments, that can
be then saved into a text editor; the way (b) may require some
tricky editing to isolate the commands from their outputs.

Eepitch.el implements a simple alternative way of interacting
with shells (and other shell-like programs) while keeping notes.
It has only one essential key binding, <F8>, which is better
explained through the executable example in the next section, and
two unessential features, `M-T' and \"\", which will be
explained later.





2. The main key: <F8>
=====================
Emacs can run a shell in a buffer, and it can split its frame
into windows, like this:
   ___________________
  |         |         |
  |   our   |    a    |
  |  notes  |  shell  |
  |         |  buffer |
  |_________|_________|

The usual way to use a shell buffer is to move the cursor there
and type commands into its prompt; the eepitch-y way is to leave
the cursor at the \"notes\" buffer, write the commands for the
shell there, and send these commands to the shell with <F8>.

Here's what <F8> does:

  When we type <F8> on a line that starts with a red
  star (\"\"), it executes the rest of the line as Lisp, and
  moves down; when we type <F8> on a line that does not start
  with a \"\", it makes sure that the \"target buffer\" is being
  displayed (the \"target\" is usually the buffer called
  \"*shell*\"), it \"send\"s the current line to the target
  buffer, and moves down.

  \"Sending the current line to the target buffer\" means copying
  the contents of the current line to the target - as if the user
  had typed that line there by hand -, then \"typing\" a <RET> at
  the target buffet.

Please try that in the example after this paragraph, by typing
<F8> six times starting at the first line that says
\" (eepitch-shell)\". The three red star lines at the top will
create a target buffer, destroy it, and create it again; the
other three lines will send commands to the target shell.

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
echo \"We are at: $PWD\"
cd /tmp/
echo \"We changed to: $(pwd)\"




3. Other targets
================
Just like `(eepitch-shell)' creates a shell buffer and sets the
eepitch target to it, `(eepitch-python)' creates a buffer with a
Python interpreter and uses it as the eepitch target. Try:

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
def square (x):
    return x*x

print(square(5))

  We can use several targets at the time, alternating between them.
  For example:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
echo Hello... > /tmp/o

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
print(open(\"/tmp/o\").read())

 (eepitch-shell)
echo ...and bye >> /tmp/o

 (eepitch-python)
print(open(\"/tmp/o\").read())


  There is a (much) more advanced example of working with several
  targets here:

  (find-prepared-intro \"An `ee' for Python\")





4. More on eepitch-kill
=======================
Note that `(eepitch-kill)' kills the _current_ target, that may
or may not be a shell buffer, a Python interaction buffer, etc...
That explains the first line in blocks like:

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)

and:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

by running the first `(eepitch-python)' we can be sure that the
following `(eepitch-kill)' will kill the Python buffer, not the
shell buffer! And the last `(eepitch-python)' in the block of
three lines will then create a new Python interaction buffer,
erasing all definitions done in previous sessions.




5. Creating eepitch blocks: `M-T'
=================================
Write just \"shell\" or \"python\" in a line, then type
`M-T' (i.e., meta-shift-t) there. The line will be turned into
three - an \" (eepitch-xxx)\", an \" (eepitch-kill)\", and an
\" (eepitch-xxx)\". We call these blocks of three lines
\"eepitch blocks\". Try this below, converting the \"shell\" into
an eepitch block for starting a shell.

shell
pwd
cd /tmp/
pwd




6. Red stars
============
Eepitch.el sets the glyph for the char 15 to a red star in the
standard display table. In layman's terms: eepitch.el tells Emacs
that the character 15 should be displayed as a red star. The
character 15 corresponds to control-O, whose default
representation on screen would be \"^O\". You can enter a
literal ^O in a buffer by typing `C-q C-o'.



7. For more information
=======================
On hyperlinks:               (find-eval-intro)
On keys similar to `M-T':    (find-wrap-intro)
An older text about eepitch:
  (find-eev \"eepitch.readme\")
  (find-eev \"eepitch.readme\" \"the-trivial-case\")
  (find-eev \"eepitch.readme\" \"red-stars\")
  (find-eev \"eepitch.readme\" \"eepitch-blocks\")
  (find-eev \"eepitch.readme\" \"eepitch-blocks\")
Many functions like `eepitch-shell':
  (find-efunction 'eepitch-bash)
What functions can generate target buffers:
  (find-eevfile \"eepitch.el\" \"shell-like sexp\")
  (find-efunction 'eepitch)
" rest)))

;; (find-eepitch-intro)

;; (find-pytutnode "Methods of File Objects")




;;;
;;; __      ___ __ __ _ _ __
;;; \ \ /\ / / '__/ _` | '_ \
;;;  \ V  V /| | | (_| | |_) |
;;;   \_/\_/ |_|  \__,_| .__/
;;;                    |_|
;;
;; Skel: (find-intro-links "wrap")
;; «find-wrap-intro»  (to ".find-wrap-intro")

(defun find-wrap-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-wrap-intro)*"))
    (apply 'find-eintro-latin1 "\
\(Re)generate: (find-wrap-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-wrap-intro\")
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Note: this intro needs to be rewritten!
Ideally it should _complement_ the material in:
  (find-eev-quick-intro \"6.3. Creating eepitch blocks: `M-T'\")
  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\")





1. Eepitch and eev
==================
Eepitch defines only two keys - <F8> and <M-T> - and <M-T> is a
particular case of something more general: \"wrapping commands\", that
follow these conventions:

  1) they are bound to meta-shift-letter keys (M-T, M-F, M-M, ...),
  2) they transform the current line and then move down,
  3) they produce Lisp code meant to be executed with `M-e' or `F8',
  4) they are listed at:
       (find-efunctiondescr 'eev-mode \"M-F\")
  5) their keybindings are only available when eev-mode is turned on.

To understand how they work, please follow the instructions below and
try them here. Note that this buffer is a sandbox, and it can be
recreated by executing the sexp \"(find-wrap-intro)\" at the top.

Note that the wrapping commands are all bound to key sequences of
the form meta-SHIFT-letter - don't forget the shift!!!



2. <M-T>: produce an eepitch block
==================================
If you type <M-T> on a line containing just the word \"shell\" you get
three lines, like this:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

We call a block of three lines like this an \"eepitch block\", and
eepitch blocks can be used to set up interactions with external
programs. Try typing <M-T> on the lines that say \"shell\" and \"python\"
below, and use them to send some lines to bash and to a python
interpreter (with <F8>):

bash
export PS1='$PWD% '
cd /tmp/
function ee () { set -v; . /tmp/ee.sh; set +v; }
rm -v /tmp/ee.sh
cat > /tmp/ee.sh <<'%%%'
  echo Hello
  cd /etc/
%%%
cat   /tmp/ee.sh
bash  /tmp/ee.sh
ee

python
square = lambda x: x*x
square(5)



3. <M-F>: hyperlink to a file or a directory
============================================
If you type <M-F> on the lines below,

/etc/
/tmp/
~/
~/.emacs

you get hyperlinks like these:

# (find-fline \"/etc/\")
# (find-fline \"/tmp/\")
# (find-fline \"~/\")
# (find-fline \"~/.emacs\")



4. <M-S>: hyperlink to the output of a shell command
====================================================
If you type <M-S> on a line containing a shell command you get a
hyperlink that starts with `find-sh', and that when followed opens a
temporary buffer with the output of that shell command, like these:

  # (find-sh \"find --help\")
  # (find-sh \"find /etc | sort\")
  # (find-sh \"find /etc -type d | sort\")
  # (find-sh \"find /etc -type d -maxdepth 1 | sort\")
  # (find-sh \"find /etc -type d -maxdepth 2 | sort\")

Try it here:

dict smop
dict 'minor detail'

If you have the packages dict, dictd and dict-jargon installed
these hyperlinks will show you the meaning of the expressions
\"smop\" and \"minor detail\".

  # (find-sh \"dict smop\")
  # (find-sh \"dict 'minor detail'\")



5. <M-M>: hyperlink to a manpage
================================
Try <M-M> here:

1 tac



6. All wrapping functions
=========================
Below is a list of all wrapping functions, with tests and
hyperlinks:

  (eek \"2*<down> M-A <down> ;;; Test eewrap-anchor\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-anchor\")
;; <anchor>

  (eek \"2*<down> M-C <down> ;;; Test eewrap-code-c-d\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-code-c-d\")
foo /tmp/foobar/

  (eek \"2*<down> M-D <down> ;;; Test eewrap-debian\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-debian\")
bash

  (eek \"2*<down> M-F <down> ;;; Test eewrap-find-fline\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-find-fline\")
/tmp/foobar/

  (eek \"2*<down> M-J <down> ;;; Test eewrap-eejump\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-eejump\")
422 (find-eev-intro \"find-wrap-intro\")

  (eek \"2*<down> M-J <down> ;;; Test eewrap-eejump\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-eejump\")
42

  (eek \"2*<down> M-M <down> ;;; Test eewrap-man\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-man\")
1 tac

  (eek \"2*<down> M-P <down> ;;; Test eewrap-pdflike\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-pdflike\")
foopdf $S/http/foo.org/bar.pdf

  (eek \"2*<down> M-R <down> ;;; Test eewrap-rm/mkdir/cd\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-rm/mkdir/cd\")
/tmp/foo/

  (eek \"2*<down> M-S <down> ;;; Test eewrap-sh\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-sh\")
seq 1 20

  (eek \"2*<down> M-T <down> ;;; Test eewrap-eepitch\")
   Source:  (find-eev \"eepitch.el\"  \"eewrap-eepitch\")
python

  (eek \"2*<down> M-V <down> ;;; Test eewrap-audiovideo\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-audiovideo\")
slimetutorial /tmp/slime-tutorial.mp4

  (eek \"2*<down> M-Z <down> ;;; Test eewrap-zsh\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-zsh\")
echo $SHELL

  (eek \"2*<down> <<eewrap-eewrap>> <down> ;;; Test eewrap-eewrap\")
   Source:  (find-eev \"eev-wrap.el\" \"eewrap-eewrap\")
U user-defined a b c




7. Wrapping functions generate hyperlinks
=========================================
...this is a slogan - a huge idea, in a very shortened form. In its
full form, that would be:

  (Some) wrapping function provide one of the basic ways to produce
  elisp hyperlinks quickly; the second basic way, which is a bit more
  complex conceptually, is via Elisp hyperlinks buffers. This, and the
  whole rationale behind generating and using elisp hyperlinks, is
  explained here:

    (find-links-intro \"Elisp hyperlinks buffers\")

The \"some\" in beginning of the long version of the slogan, above, is
because a few of the wrapping commands, for example, <M-T> and <M-R>,
are used to produce things that are not hyperlinks - usually other
kinds of scripts.
" rest)))

;; (find-wrap-intro)




;;;             _
;;;   ___  ___ (_)_   _ _ __ ___  _ __
;;;  / _ \/ _ \| | | | | '_ ` _ \| '_ \
;;; |  __/  __/| | |_| | | | | | | |_) |
;;;  \___|\___|/ |\__,_|_| |_| |_| .__/
;;;          |__/                |_|
;;
;; (find-elnode "Defining Commands")
;; (find-enode "Arguments")
;;              (find-TH "emacs"    "eejump")
;;    http://angg.twu.net/emacs.html#eejump
;; file:///home/edrx/TH/L/emacs.html#eejump
;; «find-eejump-intro»  (to ".find-eejump-intro")

(defun find-eejump-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-eejump-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-eejump-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-eejump-intro\")
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Note: this intro needs to be rewritten!
Ideally it should _complement_ the material in:
  (find-eev-quick-intro \"7.1. `eejump'\")
See the comments in:
  (find-eev \"eejump.el\")




1. The problem
==============
Suppose that we have several files that we are working on, and we
want a quick way to jump to (i.e., to visit) any of them with
very few keystrokes; moreover,

  1) we want our list of files to be preserved between one Emacs
  session and another,

  2) we know that each \"visit a file\" command will correspond
  to an elisp hyperlink.

One quick solution would be to put the list of elisp hyperlinks
in a file, and make the key `M-j' open that file. But then
jumping to a file in that list becomes a two-step process: type
`M-j', move the point to the right line, type `M-e'. This would
be similar to what happens when we use one of the `find-e*'
commands, for example `find-efunction':

  (find-efunction 'find-efunction)
     (eek \"M-h M-f find-efunction\")

Those intermediate steps - seeing the list, locating visually the
right line, moving to it - are distracting, so we want to add new
items to our wishlist:

  3) it should be possible to jump straight to any of the files
  in the list, and with very few keystrokes,

  4) the list should be stored in a format that lets us see
  quickly which are the keystrokes for accessing each item - so
  that we won't need to memorize anything,

  5) the list should be easy to modify,

  6) it should be possible to assign shorter key sequences to
  files we visit more often,

  7) the source code must be very simple.



2. A miniature
==============
My original solution was this: I used only one keybinding, `M-j',
that acted differently when invoked with different numeric
prefixes; when invoked as `M-1 M-j' it opened a certain file,
when invoked with `M-2 M-j' it opened another, and so on, and
when it was invoked with an unrecognized prefix or with no prefix
it jumped to its definition in my ~/.emacs. Its code was like
this (NOTE: do not execute these defuns):

  ;; eejump-simplified (`M-j'):
  ;; M-1 M-j opens a certain file,
  ;; M-2 M-j opens another file,
  ;; when the argument is 11, 22, 33 or 44 do something special,
  ;; like changing the font;
  ;; with no argument or with an unrecognized argument jump to the
  ;; definition of eejump in ~/.emacs; then we can see which numbers
  ;; correspond to which actions (the source is the documentation!), and
  ;; we can change the definition if needed - just run `M-e' at the
  ;; right place to make the changes apply.
  ;;
  \(global-set-key (kbd \"M-j\") 'eejump-simplified)
  \(defun eejump-simplified (arg) (interactive \"P\")
    (cond ((eq arg 1) (find-file \"~/NOTES\"))
          ((eq arg 2) (find-file \"~/otherfile.txt\"))
          ;;
          ((eq arg 11) (set-frame-font \"fixed\"))
          ((eq arg 22) (set-frame-font \"terminus-16\"))
          ((eq arg 33) (set-frame-font \"terminus-bold-16\"))
          ((eq arg 44) (set-frame-font \"10x20\"))
          (t (find-function 'eejump-simplified))))

except that my definition became huge with time as I added to it
more entries for files (and other actions!) that I used often,
and also entries that were used not so often...

All the \"options\" - i.e., all the `(eq arg nnn)' lines - had to
be together in a single very big defun, and there was no way to
add new options temporarily...




3. Families
===========
Let's use a shorthand for key sequences: for example, `M-123j'
instead of `M-1 M-2 M-3 M-j'.

I tend to assign related numbers to related files. For example, I
use the prefix \"5\" for things that are Emacs-related: `M-5j'
visits my .emacs, `M-555j' visits the directory with all of eev's
elisp files, and `M-51j', `M-52j', etc, visit specific eev source
files that I happen to be working on. Also, I use the prefix
\"7\" for things related to LaTeX. So, the \"5*\" family is
composed of Emacs-related files, and the \"7*\" family of
LaTex-related files.

The definition of `eejump-simplified' given above does not
satisfy these two (new!) wishlist items:

  8) it should be possible to jump to the definition of the
  \"5*\" family by typing something like `M-5678j', where
  \"5678\" is a non-assigned number that starts with the \"5*\"
  prefix,

  9) it should be possible to convert a number/hyperlink pair
  very easily into to the code that assigns that elisp hyperlink
  as the desired behavior for that number - and it should be
  possible to do that both permanently (think in changing the
  definition of `eejump-simplified' in your .emacs) and
  temporarily (i.e., for the current Emacs session only).



4. `eejump'
===========
The definition of `eejump' that comes with eev is a bit more
complex than the one given above, and it will not be shown
here (it involves a tricky recursive function) but it satisfies
the 9 wishlist items above. It works in this way: if you type,
say, `M-123j', then:

  a) if `eejump-123' is defined, then execute it;
  b) otherwise, if `eejump-12*' is defined, execute it;
  c) otherwise, if `eejump-1*' is defined, execute it;
  d) otherwise, if `eejump-*' is defined, execute it,

and if `eejump-*' also is not defined, you get an error.

Here is a block of \"defun\"s that defines (trivial) meanings for
\"91\", \"92\", \"991\", and \"992\", plus targets for the \"9*\"
family and for the \"99*\" family; it also has two tests in
comments that will be very important for an explanation below.
Let's refer as that, in this section and the next ones, as \"the
block of six defuns (plus four tests)\".

  (defun eejump-9* () (find-efunction 'eejump-9*))
  (defun eejump-91 () (message \"M-91j\"))
  (defun eejump-92 () (message \"M-92j\"))
  (defun eejump-99* () (find-efunction 'eejump-99*))
  (defun eejump-991 () (message \"M-991j\"))
  (defun eejump-992 () (message \"M-992j\"))
  ;; (find-function-noselect 'eejump-9*)
  ;; (find-function-noselect 'eejump-99*)
  ;; (find-efunction 'eejump-9*)
  ;; (find-efunction 'eejump-99*)

Try to evaluate each of the sexps above with `M-e', then try to
run things like `M-92j' and `M-992j' - they should work - and
then something like `M-99876j'; that will not work, you'll get an
error like \"Don't know where `eejump-99*' is defined\"...



5. eejump blocks
================
Let's a call a sequence of defuns for eejumps with the same
prefix, like this, starting with a `(defun eejump-<prefix>* ...)',

  (defun eejump-99* () (find-efunction 'eejump-99*))
  (defun eejump-991 () (message \"M-991j\"))
  (defun eejump-992 () (message \"M-992j\"))

an \"eejump block\".

There are two sample eejump blocks in eejump.el, for the prefixes
\"\" and \"5\", starting at:

  (find-eev \"eejump.el\" \"eejump-*\")
  (find-eev \"eejump.el\" \"eejump-5*\")

You should probably copy them to your .emacs, and then start
modifying them.




6. Making an `eejump-nn*' work
==============================
If you execute a line like

  (defun eejump-9* () (find-efunction 'eejump-9*))

then Emacs will only record that `eejump-9*' has been defined in
this buffer - and thus will be able to jump to its definition
when you type something like `M-987j' - if two conditions are
met:

  a) the defun is executed with `M-x eval-region', `M-x
     eval-buffer', or some variant of `load' or `require' (`M-e'
     will not do!),

  b) the buffer with the definition is associated to a file; see
     these two pages of the Emacs manuals

       (find-enode \"Buffers\" \"visiting\")
       (find-elnode \"Buffer File Name\")

    if that concept is not totally familiar to you.

So, as an experiment, copy the block with six defuns and four
tests above to some buffer associated to a file, mark it, and
execute it with `M-x eval-region'. Now the tests should work -
and key sequences like `M-987j' should also work, and should jump
to the right places. See also:

  (find-elnode \"Where Defined\")



7. Producing `eejump-nnn's and `eejump-nnn*'s
=============================================
Look again to the block of six \"defun\"s above. Now type `M-J'
on each of the six lines below:

  9
  91 (message \"M-91j\")
  92 (message \"M-92j\")
  99
  991 (message \"M-991j\")
  992 (message \"M-992j\")

you will notice that you've just generated a block of defuns like
the one in the previous section! `M-J' works like this: it tries
to split the current line into \"words\" separated by whitespace,
but producing a maximum of two \"words\" (the 2nd, 3rd, etc
\"words\" as treated as a single \"word\"); if the second word is
empty, then `M-J' produces a definition for an `eejump-nnn*'; if
it is not empty, then `M-J' produces a definition for an
`eejump-nnn', treating the second \"word\" as a sexp.

Note that `M-J' is quite dumb - it doesn't check if the first
\"word\" is a number, nor if the second is a sexp. Use it with
care! Try using `M-J' on the \"a b ...\" lines below - you will
get useless definitions.

  a  b  c  d
  a  b  c
  a  b
  a




8. Permanent and temporary
==========================
If you create a block like the block of six defuns above in your
.emacs file then you'll be attributing a \"permanent\" meaning to
`M-91j', ..., `M-992j', and if you create it in a file that is
not evaluated in every Emacs session (and execute it, of course),
then you'll be attributing just a \"temporary\" meaning to
`M-91j', ..., `M-992j'.
" rest)))

;; (find-eejump-intro)




;;;                   _
;;;   __ _ _ __   ___| |__   ___  _ __ ___
;;;  / _` | '_ \ / __| '_ \ / _ \| '__/ __|
;;; | (_| | | | | (__| | | | (_) | |  \__ \
;;;  \__,_|_| |_|\___|_| |_|\___/|_|  |___/
;;;
;; «find-anchors-intro» (to ".find-anchors-intro")
;; Skel: (find-intro-links "anchors")

(defun find-anchors-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-anchors-intro)*"))
    (apply 'find-eintro-latin1 "\
\(Re)generate: (find-anchors-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-anchors-intro\")
More intros:  (find-eev-quick-intro)
              (find-here-links-intro)
              (find-refining-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Notes: this is an advanced tutorial!
And it is very incomplete at the moment!




1. Introduction
===============
These sections of the main tutorial explain what anchors are, and
explain two simple ways of creating index/section anchor pairs:

  (find-eev-quick-intro \"8. Anchors\")
  (find-eev-quick-intro \"8.1. Introduction: `to'\")
  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\")
  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\" \"`M-A'\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\" \"`M-B'\")

and these other sections explain briefly how hyperlinks to
anchors in other files work,

  (find-eev-quick-intro \"8.5. Hyperlinks to anchors in other files\")
  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\")
  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\" \":anchor)\")
  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\" \"makes (find-eev\")

but they stop right before explaining how to use them in a
practical way, i.e., with few keystrokes. This intro is about
this.





2. Shrinking
============
We saw in

  (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\" \"makes (find-eev\")

that these two hyperlinks are equivalent:

  (find-eevfile \"eev-blinks.el\" \"«find-wottb»\")
  (find-eev     \"eev-blinks.el\"  \"find-wottb\")

The first one searches for a string in \"eev-blinks.el\" in the
normal way; the second one treats the \"find-wottb\" as a tag,
wraps it in `«»'s, and then searches for the anchor
\"«find-wottb»\" in the file \"eev-blinks.el\".

We will refer to the operation that converts the hyperlink

  (find-eevfile \"eev-blinks.el\")

to

  (find-eev \"eev-blinks.el\")

as _shrinking_ the hyperlink. Eev has a key sequence that does
that, and for simplicity its behavor is just this: it looks at
first element of the sexp at eol (the \"head\" of the sexp), and
if it is a symbol that ends with \"file\" then rewrite the sexp
replacing the head symbol by it minus its suffix \"file\". That
key sequence is `M-h M--' (`ee-shrink-hyperlink-at-eol'), and its
source code is here:

  (find-eev \"eev-edit.el\" \"ee-shrink-hyperlink-at-eol\")

Try it on the two lines below:

  (find-eevfile  \"eev-edit.el\"  \"ee-shrink-hyperlink-at-eol\")
  (find-eev      \"eev-edit.el\"  \"ee-shrink-hyperlink-at-eol\")




3. The preceding tag
====================
The key sequence `M-h M-w' copies the current line to the kill
ring, highlights it for a fraction of a second, and shows the
message

  \"Copied the current line to the kill ring - use C-y to paste\"

in the echo area. Here are links to its source code and to a
section of a tutorial that mentions it:

  (find-eev \"eev-edit.el\" \"ee-copy-this-line-to-kill-ring\")
  (find-refining-intro \"3. Three buffers\" \"M-h M-w\")

When we run `M-h M-w' with a numeric argument - for example, as
`M-1 M-h M-w' - it highlights and copies to the kill ring the
\"preceding tag\" instead of the current line; the \"preceding
tag\" is the string between `«»'s in the anchor closest to the
point if we search backwards. As an exercise, type `M-1 M-h M-w'
at some point below, and then use `M-h M-y' (`ee-yank-pos-spec')
to add it to the hyperlink with `find-anchors-intro' below the
anchors.

  «first-anchor»
  «second-anchor»
  «third-anchor»

  (find-anchors-intro)



[TO DO: write the other sections!]


" rest)))

;; (find-anchors-intro)




;;;                _                          _
;;;   ___ ___   __| | ___        ___       __| |
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |
;;;  \___\___/ \__,_|\___|      \___|     \__,_|
;;;
;; «find-code-c-d-intro»  (to ".find-code-c-d-intro")

(defun find-code-c-d-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-code-c-d-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-code-c-d-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-code-c-d-intro\")
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Note: this intro needs to be rewritten!
Ideally it should _complement_ the material in:
  (find-eev-quick-intro \"9. Shorter hyperlinks\")
  (find-eev-quick-intro \"9.1. `code-c-d'\")



1. Avoiding full path names
===========================
Suppose that you have downloaded (\"psne\"-ed) this URL,

  http://www.lua.org/ftp/lua-5.1.4.tar.gz

with `M-x brep' - see:

  (find-psne-intro)

and you unpacked that tarball into the directory ~/usrc/ (I
prefer to use that instead of /usr/src/) with:

  tar -C ~/usrc/ -xvzf $S/http/www.lua.org/ftp/lua-5.1.4.tar.gz

Now you can access some directories and files of the unpacked
tarball with:

  (find-fline \"~/usrc/lua-5.1.4/\")
  (find-fline \"~/usrc/lua-5.1.4/src/\")
  (find-fline \"~/usrc/lua-5.1.4/src/lstrlib.c\")
  (find-fline \"~/usrc/lua-5.1.4/test/\")
  (find-fline \"~/usrc/lua-5.1.4/test/README\")
  (find-fline \"~/usrc/lua-5.1.4/doc/\")
  (find-w3m   \"~/usrc/lua-5.1.4/doc/contents.html\")

but it's a bit clumsy to have to use the \"~/usrc/lua-5.1.4/\"
every time, so eev provides a nice way to define shorthands. We
want to be able to write just this instead of the sexps above,

  (find-lua51file \"\")
  (find-lua51file \"src/\")
  (find-lua51file \"src/lstrlib.c\")
  (find-lua51file \"test/\")
  (find-lua51file \"test/README\")
  (find-lua51file \"doc/\")
  (find-lua51w3m  \"doc/contents.html\")

and here the directory \"~/usrc/lua-5.1.4/\" became a mnemonic
\"lua51\" in the middle of the names of some functions.

We will call these sexps with \"lua51\" \"shorter hyperlinks\".



2. Shorter hyperlinks
=====================
How can we generate the definitions for `find-lua51file' and
`find-lua51w3m' from just the strings \"lua51\" and
\"~/usrc/lua-5.1.4/\"? Try this:

  (find-code-c-d \"lua51\" \"~/usrc/lua-5.1.4/\")

you will get a temporary buffer with a lot of Lisp code -
including a definition for `find-lua51file' and another one for
`find-lua51w3m'. That Lisp code has not been executed yet; the
function `find-code-c-d' is just for debugging, and we can regard
it as a hyperlink to the code that this sexp would execute:

  (code-c-d \"lua51\" \"~/usrc/lua-5.1.4/\")

So, to define a family of functions including `find-lua51file'
and `find-lua51w3m', for a given \"mnemonic\" - \"lua51\" in this
case - and a given \"directory\" - \"~/usrc/lua-5.1.4/\" - we run
this:

  (code-c-d \"lua51\" \"~/usrc/lua-5.1.4/\")

which generates a block of Lisp code, as a string, and evaluates
it. Note: the original (and rather confusing) terminology for the
\"mnemonic\" was \"code\"; that's why the \"c\" in `code-c-d'.



3. Extra arguments to `code-c-d'
================================
`code-c-d' supports extra arguments - for example, this works:

  (find-code-c-d \"el\" \"~/usrc/emacs/lisp/\" :info \"elisp\")

Look at the end of the generated code and you will see that it
has a definition for `find-elnode' - such that

  (find-elnode \"Constant Variables\")

is a shorthand (a \"shorter hyperlink\") for:

  (find-node \"(elisp)Constant Variables\")

What is important to understand here is how these definitions
with extra arguments are structured - so that you will be able to
understand the source code when you need to. Both `code-c-d' and
`find-code-c-d' are defined with a `&rest' in their argument
lists, like this (NOTE: do not execute these defuns!):

  (defun      code-c-d (c d &rest rest) ...)
  (defun find-code-c-d (c d &rest rest) ...)

and they both invoke `ee-code-c-d', which does all the template
work and returns a big string; `ee-code-c-d' passes its `rest'
argument to a recursive function called `ee-code-c-d-rest', and
for each one of the supported keywords there is a corresponding
function, also recursive; for `:info' it is called
`ee-code-c-d-:info'. Their specifications are like this:

  (defun   ee-code-c-d (c d &rest rest) ...)
  (defun   ee-code-c-d-rest      (rest) ...)
  (defun   ee-code-c-d-:info (manual &rest rest) ...)

and one very non-obvious trick is used to make the code short.
When `ee-code-c-d-rest' and `ee-code-c-d-:info' are run they can
access the values the `c' and the `d' that were passed to
`ee-code-c-d' (due to dynamic scoping), so `c' and `d' do not
need to be passed down explicitly as arguments.

Try:

  (find-code-c-d      \"CODE\" \"/DIR/\" :info \"INFO\")




4. Other similar functions
==========================
See: (find-brxxx-intro)
     (find-pdf-like-intro)
     (find-audiovideo-intro)

Try: (find-code-pdf      \"CODE\" \"FILE.pdf\")
     (find-code-pdf-text \"CODE\" \"FILE.pdf\")
     (find-code-audio    \"CODE\" \"FILE.mp3\")
     (find-code-video    \"CODE\" \"FILE.mp4\")
" rest)))

;; (find-TH "eev-article")
;; (find-TH "eev-article" "shorter-hyperlinks")
;; (find-code-c-d-intro)





;;;            _  __       _ _ _
;;;  _ __   __| |/ _|     | (_) | _____
;;; | '_ \ / _` | |_ _____| | | |/ / _ \
;;; | |_) | (_| |  _|_____| | |   <  __/
;;; | .__/ \__,_|_|       |_|_|_|\_\___|
;;; |_|
;;
;; «find-pdf-like-intro»  (to ".find-pdf-like-intro")

(defun find-pdf-like-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-pdf-like-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-pdf-like-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-pdf-like-intro\")
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-here-links-intro)
              (find-refining-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Note: you will need a basic understanding of eepitch and
code-c-d to understand parts of this intro. See:

  (find-eev-quick-intro \"6.1. The main key: <F8>\")
  (find-eev-quick-intro \"9. Shorter hyperlinks\")
  (find-eev-quick-intro \"9.1. `code-c-d'\")





1. PDF-like documents
=====================
Let's introduce a bit of (improvised!) terminology: we will say
that a document is \"PDF-like\" when it is in a format like PDF,
PostScript, DVI or DJVU - i.e., divided into pages. Emacs has a
standard mode for viewing PDF-like documents,

  (find-enode \"Document View\")

but we will see a more eev-like way of pointing to pages of
PDF-like documents.




2. Preparation
==============
We need to start by downloading a PDF file to use in our
examples. If you run this e-script

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  cd
  wget -nc https://tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf

you will download a local copy of J.M. Coetzee's \"The Lives of
Animals\" into your home directory. To check that the PDF has been
downloaded, use:

  (find-fline \"~/\")
  (find-fline \"~/\"  \"Coetzee99.pdf\")
  (find-sh0 \"ls -l ~/Coetzee99.pdf\")

Eev also implements another way, called \"psne\", to download
local copies of files from the internet.\"Psne-ing\" a URL like

  https://tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf

downloads it to a local file with a name like:

       $S/https/tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf
  ~/snarf/https/tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf

that is _much_ longer that just \"~/Coetzee99.pdf\"; this has the
advantage of preserving more information about the URL from which
the file came, but sometimes these longer names feels clumsy.
Psne-ing is discussed a more advanced tutorial:

  (find-psne-intro)

In this tutorial we will use the home directory and the shorter
file name.




3. Hyperlinks to PDF files
==========================
If you have xpdf installed then this sexp

  (find-pdf-page \"~/Coetzee99.pdf\")

should work as a \"hyperlink to the PDF\": it calls xpdf as an
external program - like we did with browsers in the main tutorial -

  (find-eev-quick-intro \"3.1. Non-elisp hyperlinks\")
  (find-eev-quick-intro \"3.1. Non-elisp hyperlinks\" \"find-firefox\")

to display the PDF file that we downloaded.

The main keys of xpdf are:

  q         quit xpdf
  PageDown  scroll down/go to next page
  PageUp    scroll up/go to previous page
  arrows    scroll within the current page
  +         zoom in one step
  -         zoom out out step
  0         set zoom to 125%
  alt-f     toggle full-screen; use twice to fit document to page

Note that `q' \"goes back to Emacs\".

If you have the program pdftotext installed - hint: \"apt-get install
poppler-utils\"! - then you can also display PDFs in another way. This
sexp

  (find-pdf-text \"~/Coetzee99.pdf\")

work as a \"hyperlink to the _text_ of the PDF\": it extracts the text
from the PDF using the program \"pdftotext\" and displays that in an
Emacs buffer.




4. Hyperlinks to pages of PDF files
===================================
It is possible to create hyperlinks that point to a specific page in a
PDF file. Compare what happens when you run these sexps:

  (find-pdf-page \"~/Coetzee99.pdf\")
  (find-pdf-page \"~/Coetzee99.pdf\" 1)
  (find-pdf-page \"~/Coetzee99.pdf\" 1 \"The Lives of Animals\")
  (find-pdf-page \"~/Coetzee99.pdf\" 3)
  (find-pdf-page \"~/Coetzee99.pdf\" 3 \"LECTURE I\")
  (find-pdf-page \"~/Coetzee99.pdf\" 3 \"LECTURE I\" \"[113]\")

The top three sexps open the PDF at page 1 - the default. The bottom
three sexps open it at page 3. The arguments after the number are
ignored by Emacs - they are there to make these links more expressive
for humans.

The hyperlinks to the text of a PDF interpret the numeric number as a
page number and the following arguments as strings to search for. Try:

  (find-pdf-text \"~/Coetzee99.pdf\" 1)
  (find-pdf-text \"~/Coetzee99.pdf\" 1 \"The Lives of Animals\")
  (find-pdf-text \"~/Coetzee99.pdf\" 3)
  (find-pdf-text \"~/Coetzee99.pdf\" 3 \"LECTURE I\")
  (find-pdf-text \"~/Coetzee99.pdf\" 3 \"LECTURE I\" \"[113]\")

For more information about these string arguments, see:

  (find-refining-intro \"1. Pos-spec-lists\")

A pair of sexps like this, in which both point to the same
position of a PDF,

  (find-pdf-page \"~/Coetzee99.pdf\" 3 \"LECTURE I\" \"[113]\")
  (find-pdf-text \"~/Coetzee99.pdf\" 3 \"LECTURE I\" \"[113]\")

will be called a `find-pdf'-pair.

[Video links:]
  (find-eev2020video \"4:52\" \"`find-pdf-page' calls an external program\")
  (find-eev2020video \"5:26\" \"`find-pdf-text' converts the PDF to text and\")





5. A convention on page numbers
===============================
The `(+ -110 113)'s in

  (find-livesofanimalspage (+ -110 113) \"LECTURE I.\")
  (find-livesofanimalstext (+ -110 113) \"LECTURE I.\")

are a bit mysterious at first sight.

We are accessing a PDF that is an excerpt of a book. The third
page of the PDF has a \"[113]\" at its footer to indicate that it
is the page 113 of the book. Let's use the terms _page number_
and _page label_ to distinguish the two numberings: in this case,
the page whose page number is 3 is the page whose page label is
113. These two sexps

  (find-livesofanimalspage (+ -110 113))
  (find-livesofanimalspage 3)

are equivalent, but the first one is more human-friendly: the 113
is a page label, and the -110 is an adjustment (we call it the
\"offset\") to convert the 113 that humans prefer to see into
the 3 that xpdf needs to receive.





6. How the external programs are called
=======================================
Both `find-pdf-page' and `find-pdf-text' invoke external programs -
but how, exactly? Let's take a look at a hack that shows this. If
you prepend an \"ee-\" to `find-pdf-page' and `find-pdf-text' sexps,
like in:

  (ee-find-pdf-page \"~/Coetzee99.pdf\")
  (ee-find-pdf-page \"~/Coetzee99.pdf\" 3)
  (ee-find-pdf-text \"~/Coetzee99.pdf\")
  (ee-find-pdf-text \"~/Coetzee99.pdf\" 3)

you will get sexps that stop just before invoking the external
programs - they just show how these externals programs _would be_
invoked, i.e., they show the options that would be passed to them. The
results of the sexps above will be lists like these:

  (\"xpdf\" \"-fullscreen\" \"~/Coetzee99.pdf\")
  (\"xpdf\" \"-fullscreen\" \"~/Coetzee99.pdf\" \"3\")
  (\"pdftotext\" \"-layout\" \"-enc\" \"Latin1\" \"~/Coetzee99.pdf\" \"-\")
  (\"pdftotext\" \"-layout\" \"-enc\" \"Latin1\" \"~/Coetzee99.pdf\" \"-\")

Note that `ee-find-pdf-text' does not pass the argument \"3\" to
\"pdftotext\". A sexp like

  (find-pdf-text \"~/Coetzee99.pdf\" 3)

first produces the conversion to text of the full PDF, and then
finds the page 3 in it by counting formfeeds, as described here:

  (find-enode \"Pages\" \"formfeed\")





7. Shorter hyperlinks to PDF files
==================================
If you run these sexps

  (code-pdf-page \"livesofanimals\" \"~/Coetzee99.pdf\")
  (code-pdf-text \"livesofanimals\" \"~/Coetzee99.pdf\" -110)

they will define the functions `find-livesofanimalspage' and
`find-livesofanimalstext', and then these hyperlinks should work:

  (find-livesofanimalspage)
  (find-livesofanimalstext)
  (find-livesofanimalspage (+ -110 113))
  (find-livesofanimalstext (+ -110 113))
  (find-livesofanimalspage (+ -110 113) \"LECTURE I.\")
  (find-livesofanimalstext (+ -110 113) \"LECTURE I.\")
  (find-livesofanimalspage (+ -110 127) \"wrong thoughts\")
  (find-livesofanimalstext (+ -110 127) \"wrong thoughts\")
  (find-livesofanimalspage (+ -110 132) \"into the place of their victims\")
  (find-livesofanimalstext (+ -110 132) \"into the place of their victims\")
  (find-livesofanimalspage (+ -110 133) \"To write that book I had to think\")
  (find-livesofanimalstext (+ -110 133) \"To write that book I had to think\")
  (find-livesofanimalspage (+ -110 134) \"woke up haggard in the mornings\")
  (find-livesofanimalstext (+ -110 134) \"woke up haggard in the mornings\")
  (find-livesofanimalspage (+ -110 143) \"Babies have no self-consciousness\")
  (find-livesofanimalstext (+ -110 143) \"Babies have no self-consciousness\")
  (find-livesofanimalspage (+ -110 145) \"squirrel doing its thinking\")
  (find-livesofanimalstext (+ -110 145) \"squirrel doing its thinking\")
  (find-livesofanimalspage (+ -110 147) \"Rilke's panther\")
  (find-livesofanimalstext (+ -110 147) \"Rilke's panther\")
  (find-livesofanimalspage (+ -110 162) \"a grasp of the meaning\")
  (find-livesofanimalstext (+ -110 162) \"a grasp of the meaning\")
  (find-livesofanimalspage (+ -110 164) \"last common ground\")
  (find-livesofanimalstext (+ -110 164) \"last common ground\")

Hyperlinks like

  (find-livesofanimalspage (+ -110 113) \"LECTURE I.\")
  (find-livesofanimalstext (+ -110 113) \"LECTURE I.\")

behave roughly as abbreviations for:

  (find-pdf-page \"~/Coetzee99.pdf\" (+ -110 113) \"LECTURE I.\")
  (find-pdf-text \"~/Coetzee99.pdf\" (+ -110 113) \"LECTURE I.\")

[Video links:]
  (find-eev2020video \"10:22\" \"1.3. Shorter hyperlinks to PDFs and videos\")
  (find-eev2020video \"10:45\"   \"`code-pdf-page' creates short hyperlink functions\")
  (find-eev2020video \"11:38\"   \"let's try...\")
  (find-eev2020video \"11:55\"   \"`find-fongspivatext'\")





8. `find-pdf'-pairs
===================
Let's introduce some terminology. Remember that we call a pair of
sexps like

  (find-pdf-page \"~/Coetzee99.pdf\" (+ -110 113) \"LECTURE I.\")
  (find-pdf-text \"~/Coetzee99.pdf\" (+ -110 113) \"LECTURE I.\")

a \"`find-pdf'-pair\"; a pair like

  (find-livesofanimalspage (+ -110 113) \"LECTURE I.\")
  (find-livesofanimalstext (+ -110 113) \"LECTURE I.\")

will be called a \"short `find-pdf'-pair\", as in:

  (find-eev-quick-intro \"9. Shorter hyperlinks\")

and a pair like

  (code-pdf-page \"livesofanimals\" \"~/Coetzee99.pdf\")
  (code-pdf-text \"livesofanimals\" \"~/Coetzee99.pdf\" -110)

will be called a `code-pdf'-pair.

The \"livesofanimals\" will the called the _stem_. The \"-110\"
will be called the _offset_.




9. Generating three pairs
=========================
Eev has a high-level function that generates at once, for a
single PDF file, a `find-pdf'-pair, a `code-pdf'-pair, and a
short `find-pdf'-pair. To see what it produces, try:

  (find-code-pdf-links \"~/Coetzee99.pdf\")
  (find-code-pdf-links \"~/Coetzee99.pdf\" \"livesofanimals\")

The second link above produces a temporary buffer containing this:

  ;; (find-pdf-page \"~/Coetzee99.pdf\")
  ;; (find-pdf-text \"~/Coetzee99.pdf\")
  (code-pdf-page \"livesofanimals\" \"~/Coetzee99.pdf\")
  (code-pdf-text \"livesofanimals\" \"~/Coetzee99.pdf\")
  ;; (find-livesofanimalspage)
  ;; (find-livesofanimalstext)

`find-code-pdf-links' is somewhat similar to `find-latex-links',
in this aspect:

  (find-eev-quick-intro \"7.5. `find-latex-links'\" \"change the \\\"{stem}\\\"\")

If you run just

  (find-code-pdf-links \"~/Coetzee99.pdf\")

it will generate a buffer that has \"{c}\"s in several places and
that follows the convention that \"the first line regenerates the
buffer\". If you substitute the \"{c}\" in the top sexp by
\"livesofanimals\" and type `M-e' the buffer will be recreated
with each \"{c}\" replaced by \"livesofanimals\".

The user-friendly way to run `find-code-pdf-links' is by typing
`M-h M-p' in Dired mode. If you want to generate the three pairs
for a file \"~/foo/bar/story.pdf\" then visit the directory
\"~/foo/bar/\", put the cursor on the line that lists the file
\"story.pdf\", and type `M-h M-p'. Try it with our test file:

  (find-fline \"~/\" \"Coetzee99.pdf\")



10. Generating a pair with the page number
==========================================
If you type `M-h M-p' and you're not in Dired mode then `M-h M-p'
will try to generate a short `find-pdf'-pair pointing to the
current position in the current page of the current PDF
file (converted to text). The function bound to `M-h M-p' tries
to guess four things: the stem, the offset, the page number, and
the string to the be used as a pos-spec. Let's see first a
situation where everything works. Run the four sexps below and
type `M-h M-p':

  (code-pdf-page \"livesofanimals\" \"~/Coetzee99.pdf\")
  (code-pdf-text \"livesofanimals\" \"~/Coetzee99.pdf\" -110)
  (kill-new \"wrong thoughts\")
  (find-livesofanimalstext (+ -110 127) \"wrong thoughts\")

You will get an elisp hyperlinks buffer whose middle links are
four short `find-pdf'-pairs, all pointing to the current page:

  # (find-livesofanimalspage 17)
  # (find-livesofanimalstext 17)
  # (find-livesofanimalspage (+ -110 127))
  # (find-livesofanimalstext (+ -110 127))

  # (find-livesofanimalspage 17 \"wrong thoughts\")
  # (find-livesofanimalstext 17 \"wrong thoughts\")
  # (find-livesofanimalspage (+ -110 127) \"wrong thoughts\")
  # (find-livesofanimalstext (+ -110 127) \"wrong thoughts\")

The second and the fourth pairs use \"(+ -110 127)\" instead of
\"17\" as the page number; the third and the fourth pairs point
to the string \"wrong thoughts\" in the page.




11. How `M-h M-p' guesses everything
====================================
The method that `M-h M-p' uses to guess the stem, the offset, the
page and the pos-spec is so error-prone and gives unexpected
results so often that it's worth to describe it in detail.

  1. The stem is taken from the global variable `ee-page-c'.

  2. Every call to a function like `find-xxxtext' sets
     `ee-page-c' to \"xxx\" - for example, a call to
     `find-livesofanimalstext' sets `ee-page-c' to
     \"find-livesofanimalstext\". So `ee-page-c' usually holds
     the stem of the last function of the form `find-xxxtext'
     that was run.

  3. The offset is taken from the global variable
     `ee-page-offset'.

  4. A call to, say, `find-livesofanimalstext', sets
     `ee-page-offset' to the offset that was declared here:

       (code-pdf-text \"livesofanimals\" \"~/Coetzee99.pdf\" -110)

     So `ee-page-offset' usually holds the offset of the last
     function of the form `find-xxxtext' that was run.

  5. The page number is obtained by counting the number of
     formfeeds between the beginning of the buffer and the
     current position. If there are 16 formfeeds then the current
     page is 17.

  6. The pos-spec - \"wrong thoughts\" in the example - is the
     string in the top of the kill ring. See:

       (find-refining-intro \"2. Refining hyperlinks\" \"kill-new\")

If you want to see an example where `M-h M-p' guesses everything
wrong you can type `M-h M-p' here... as we're not in Dired mode
`M-h M-p' will think that we're in the conversion to text of
\"livesofanimals\", in page 1, and it will generate hyperlinks to
that page of the book!




12. Other ways to generate `code-pdf'-pairs
===========================================
The easiest way is with `M-h M-e'. See:

  (find-audiovideo-intro \"4.1. `find-extra-file-links'\" \"M-h M-e\")

There is also `M-P', that is a \"wrapping function\" that
transforms the current line, like `M-B' - see:

  (find-eev-quick-intro \"8.4. Creating e-script blocks\" \"M-B\")

`M-P' parses the current line as a short string and a file name,
and then deletes the current line and inserts in its place a
block of five lines containing a `code-pdf'-pair and some
comments. Try:

  (eek \"<down> M-P  ;; eewrap-pdflike\")
  livesofanimals ~/Coetzee99.pdf

" rest)))

;; (find-pdf-like-intro)




;;;  _
;;; | |__  _ ____  ____  ____  __
;;; | '_ \| '__\ \/ /\ \/ /\ \/ /
;;; | |_) | |   >  <  >  <  >  <
;;; |_.__/|_|  /_/\_\/_/\_\/_/\_\
;;;
;; «find-brxxx-intro»  (to ".find-brxxx-intro")

(defun find-brxxx-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-brxxx-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-brxxx-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-brxxx-intro\")
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-psne-intro)
              (find-pdf-like-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



This intro expands an idea that was mentioned briefly at:
  (find-eev-quick-intro \"3.1. Non-elisp hyperlinks\")
and combines it with the idea of \"local copies\" from:
  (find-psne-intro \"the second way\")
  (find-psne-intro \"1. Local copies of files from the internet\")
  (find-psne-intro \"5. `browse-url' and friends\")

At this moment the best explanation of these ideas in here:
  (find-eev \"eev-brxxx.el\" \";;; Commentary:\")
in the source code. I need to rewrite this intro!

[Video links:]
  (find-eevtemplvideo  \"9:17\" \"3. The function that defines brep\")
  (find-eevtemplvideo  \"9:38\"  \"`code-brurl' is a variant of `code-c-d'\")
  (find-eevtemplvideo \"10:07\"  \"find-code-url shows the code instead of executing it\")
  (find-eevtemplvideo \"11:26\"  \"this is explained in the main tutorial\")
  (find-eevtemplvideo \"12:12\"  \"accept extra arguments\")
  (find-eevtemplvideo \"12:34\"  \"if we run just this\")
  (find-eevtemplvideo \"12:40\"  \"one of the reasons for using text: comments\")
  (find-eevtemplvideo \"13:03\"  \"if we run just this with extra arguments\")
  (find-eevtemplvideo \"14:10\"  \"code-brurl executes this code here\")





1. Introduction
===============
We saw in

  (find-eev-quick-intro \"3.1. Non-elisp hyperlinks\")

that eev defines some functions with names starting with `br'
that are similar to `browse-url', and we saw in

  (find-psne-intro \"the second way\")
  (find-psne-intro \"1. Local copies of files from the internet\")
  (find-psne-intro \"3. The new way: M-x brep\")

how to create local copies of files; after downloading a local
copy of, say,

  http://www.gnu.org/software/emacs/emacs-paper.html

into

  $S/http/www.gnu.org/software/emacs/emacs-paper.html

you can open the local copy by running `M-x browse-url', `M-x brg'
or `M-x brff' on the \"file:///\" URL below,

  file:///home/edrx/snarf/http/www.gnu.org/software/emacs/emacs-paper.html

but note that the \"file:///\" URL has an \"edrx\" - my username
- in the middle of the file name, so this only works without
changes if you use \"edrx\" as your username...




2. The `l' variants
===================
After creating `brg' and `brff' I created variants of them that
would open the local copy of the URL at point instead of the
original URL - or, more precisely, that would open the result of
applying `ee-url-to-local-url' to the original URL. Try:

  (ee-url-to-local-url
   \"http://www.gnu.org/software/emacs/emacs-paper.html\")

These variants were called `brgl' and `brffl' - I used the
convention that the suffix `l' meant \"use the local copy\".



3. The `d' variants
===================
After creating `brgl' and `brffl' I realized that it would be
easy to create variants of them that would work in dired mode.
If we visit a directory - for example, this one,

  (find-fline \"$S/http/www.gnu.org/software/emacs/\")

and we put the point in a line with an HTML on it - for example,
on the line with the \"emacs-paper.html\" - then typing `M-x
brgd' there converts the full pathname of the file at point to a
\"file:///\" URL, like this,

                          $S/http/www.gnu.org/software/emacs/emacs-paper.html
  -> file:///home/edrx/snarf/http/www.gnu.org/software/emacs/emacs-paper.html

and opens the resulting \"file:///\" url with `brg'.

The suffix `d' means \"use the file in this line in dired\".




4. `brxxx'-functions
====================
`browse-url' has several variants, with names like
`browse-url-firefox' and `browse-url-chromium', that open the URL
at point using specific programs. See:

  (find-epackage 'browse-url)
  (find-enode \"Browse-URL\")

We say that `brg', `brgl' and `brgd' are \"`brxxx'-functions\"
with \"base function\" `find-googlechrome'; `brgl' is the `l' (or
\"local\") variant, and `brgd' is the `d' (or \"dired\") variant;
`brg' is sometimes called the \"remote\" variant.




5. `code-brurl'
===============
Remember that `code-c-d' generates lisp code and executes it, and
that `find-code-c-d' generates the same lisp code as `code-c-d'
but displays it instead of executing it; this was explained, with
examples, here:

  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-eev-quick-intro \"9.1. `code-c-d'\" \"find-code-c-d\")

Eev has a function called `code-brurl' that works like `code-c-d'
and that creates several `brxxx'-functions with the same base
function. To understand what the `code-brurl' sexp below does,

  ;; From:
  ;; (find-eev \"eev-brxxx.el\" \"code-brxxxs\")
  ;; (find-eev \"eev-brxxx.el\" \"code-brxxxs\" \"brg\")

  (code-brurl 'find-googlechrome  :remote 'brg  :local 'brgl  :dired 'brgd)

We run:

  (find-code-brurl 'find-googlechrome  :remote 'brg  :local 'brgl  :dired 'brgd)

Note that the base function in this example is
`find-googlechrome', that is a function that expects a URL.




6. `code-brfile'
================
We saw how to create `brxxx'-functions using `find-googlechrome'
as the base function; remember that `find-googlechrome' is a
function that expects a URL.

If we download a local copy of a PDF, like we did here,

  (find-pdf-like-intro \"2. Preparation\")

      https://tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf
  -> $S/https/tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf

then it makes sense to have a `brxxx'-function, called `brpdfl',
that we can run on the \"https://\" URL above, and that will open
the \"$S/https/\" file corresponding to the URL using
`find-pdf-page'... but `find-pdf-page' is a function that expects
a filename, not a URL, so `code-brurl' wouldn't work...

What we want can be done by the sexp below:

  ;; From:
  ;; (find-eev \"eev-pdflike.el\" \"code-brxxxs\")
  ;; (find-eev \"eev-pdflike.el\" \"code-brxxxs\" \"brpdfl\")
  (code-brfile 'find-pdf-page  :local 'brpdfl  :dired 'brpdfd)

To understand what it does, run:

  (find-code-brfile 'find-pdf-page  :local 'brpdfl  :dired 'brpdfd)

and read the docstrings.

Note that in the previous section we had a \":remote 'brg\", that
defined a remote variant. It doesn't make sense to apply
`find-pdf-page' to a remote URL, so we don't have a \":remote\"
here.




Old stuff:
(TODO: revise it!)




7. Old introduction
===================
We saw in

  (find-psne-intro)
  (find-psne-intro \"M-x brep\")
  (find-psne-intro \"M-x brfl\")
  (find-psne-intro \"5. `browse-url' and friends\")

that we can use `M-x brep' to download local copies of files from
the internet, and that `M-x brfl' on a URL runs `find-fline' on
the local copy of that URL. `brep' and `brfl' are
\"`browse-url'-like functions\" defined by eev; we will refer to
them, and to other such functions, as \"brxxx-functions\". Every
brxxx-function is an interactive interface to some \"base
function\"; for `brep' and `brfl' we have:

    brxxx-function   base function
    --------------   -------------
         brep        find-psne-links
         brfl        find-fline

What we will see here is how `code-brfile' and `code-brurl' -
which are somewhat similar to `code-c-d' - can be used to define
brxxx-functions from base functions.




8. A first example
==================
Let's define two trivial base functions, one that expects a URL,
and another one that expects a file name:

  (defun foo-url  (url)      (format \"Got URL: %s\"      url))
  (defun foo-file (filename) (format \"Got filename: %s\" filename))

Note that they don't do much - they just return explanatory
strings.

These two calls,

  (code-brurl  'foo-url  :remote 'brshowu :local 'brshowul)
  (code-brfile 'foo-file                  :local 'brshowfl)

define three brxxx-functions: `brshowu' and `brshowul' for the
base function `foo-url', and `brshowfl' for the base function
`foo-file'. You can inspect the definitions by running these
sexps,

  (find-code-brurl  'foo-url  :remote 'brshowu :local 'brshowul)
  (find-code-brfile 'foo-file                  :local 'brshowfl)

and you can test what `foo-url', `foo-file', `brshowu',
`brshowul', and `brshowfl' do by running the sexps below.

  (foo-url \"http://a/b\")
    => \"Got URL: http://a/b\"

  (foo-file \"/c/d/e/f\")
    => \"Got filename: /c/d/e/f\"

  (brshowu  \"http://a/b\")
    => `(foo-url \"http://a/b\") -> \"Got URL: http://a/b\"'

  (brshowul \"http://a/b\")
    => `(foo-url \"file:///home/edrx/snarf/http/a/b\") ->
        \"Got URL: file:///home/edrx/snarf/http/a/b\"'

  (brshowfl \"http://a/b\")
    => `(foo-file \"/home/edrx/snarf/http/a/b\") ->
        \"Got filename: /home/edrx/snarf/http/a/b\"'

Now let's go to what matters. Put the point on the URL below, and
run `M-x brshowu', `M-x brshowul' and `M-x brshowfl':

  http://a/b

you will see that `brshowu', `brshowul', and `brshowfl' can be
called interactively, and when they are called interactively they
use as their argument either the URL around point, or something
obtained from it - the local file name or a local URL associated
to that URL.




9. The conversions
==================
One underlying idea behind all this is that we have two
conversion functions, one from URLs to file names, and another
from (absolute) file names to URLs starting with \"file:///\".
They work like this:

  http://a/b  ->  $S/http/a/b  ->  file:///home/edrx/snarf/http/a/b
                       /tmp/c  ->  file:///tmp/c

try:

  (ee-url-to-fname \"http://a/b\")
  (ee-fname-to-url \"/tmp/c\")
  (ee-url-to-local-url \"http://a/b\")

Now execute the sexps below (with `M-2 M-e') to examine the code
that calls to `code-brurl' and `code-brfile' generate and
execute:

  (find-code-brurl  'foo-url  :remote 'brshowu :local 'brshowul)
  (find-code-brfile 'foo-file                  :local 'brshowfl)






10. Naming conventions for brxxx-functions
==========================================
By convention, each name for a brxxx-function is composed of a
prefix, a stem, and a suffix. The prefix is always \"br\", the
stem is a mnemonic for the base function, and the suffix is
either \"\", \"l\", or \"d\", meaning:

  \"\"   - use the URL without changes
  \"l\"  - use the local copy
  \"d\"  - dired variation (see below)

Here are the stems for some of the brxxx-functions defined by
eev:

  Base function       receives   stem
  -------------       --------   ----
  find-psne-links     URL        \"ep\"
  browse-url-firefox  URL        \"m\"
  find-googlechrome   URL        \"g\"
  find-w3m   	      URL        \"w\"
  find-fline          file name  \"f\"
  find-audio          file name  \"audio\"
  find-video          file name  \"video\"
  find-xpdf-page      file name  \"xpdf\"
  find-evince-page    file name  \"evince\"
  find-xdvi-page      file name  \"xdvi\"
  find-djvu-page      file name  \"djvu\"
  find-pdf-text       file name  \"pdftext\"
  find-djvu-text      file name  \"djvutext\"

In our example with `foo-url' and `foo-file' we had:

  Base function       receives   stem
  -------------       --------   ----
  foo-url             URL        showu
  foo-file            file name  showf




11. Calling `code-brurl' and `code-brfile'
==========================================

  (code-brurl '<U-function>
                   :remote 'br<stem>   :local 'br<stem>l   :dired 'br<stem>d)
                   \\---------------/   \\---------------/   \\----------------/
                       optional              optional             optional

  (code-brfile '<F-function>           :local 'br<stem>l   :dired 'br<stem>d)
                                       \\---------------/   \\----------------/
                                             optional             optional

This, like many other parts of eev, is a hack with a very concise
calling syntax - so we will see an example first, and then
dissect it to understand precisely how it works. If you are
curious about the inspirations behind it, here they are:

  (find-code-c-d-intro)
  (find-code-c-d-intro \"find-code-c-d\")
  (find-code-c-d-intro \"Extra arguments\")
  (find-enode \"Browse-URL\")



12. The dired variation
=======================

In dired mode each line corresponds to a file


" rest)))

;; (find-brxxx-intro)




;;;
;;;  _ __  ___ _ __   ___
;;; | '_ \/ __| '_ \ / _ \
;;; | |_) \__ \ | | |  __/
;;; | .__/|___/_| |_|\___|
;;; |_|
;;
;; «find-psne-intro»  (to ".find-psne-intro")
;; (find-TH "eev-article" "local-copies")
;; (find-angg ".emacs" "brep")
;; (find-eev "eev-browse-url.el" "find-psne-links")
;; (find-eev "eev-browse-url.el" "brep")

(defun find-psne-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-psne-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-psne-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-psne-intro\")
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.




We mentioned briefly in

  (find-pdf-like-intro \"2. Preparation\")

that there are two \"natural\" ways to store a local copy of a
file from the internet... here we will discuss the second way, in
which the conversion from URL to a local file name works like
this:

      https://tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf
  -> $S/https/tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf





1. Local copies of files from the internet
==========================================
Emacs knows how to fetch files from the internet, but for most
purposes it is better to use local copies. Suppose that the
environment variable $S is set to ~/snarf/; then running this

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  mkdir -p $S/http/www.gnu.org/software/emacs/
  cd       $S/http/www.gnu.org/software/emacs/
  wget      http://www.gnu.org/software/emacs/emacs-paper.html
  echo     'http://www.gnu.org/software/emacs/emacs-paper.html' >> ~/.psne.log

  # (find-fline \"$S/http/www.gnu.org/software/emacs/emacs-paper.html\")
  # (find-eww   \"$S/http/www.gnu.org/software/emacs/emacs-paper.html\")

creates a local copy of `emacs-paper.html' inside ~/snarf/http/
and appends the URL to the file ~/.psne.log. The two lines in
comments are hyperlinks to the local copy; The `find-fline' opens
it as a file in the obvious way, and `find-eww' opens it \"as
HTML\", using a text-mode web browser called eww that runs
entirely inside Emacs.



2. The old way: psne
====================
A long time ago eev used to include a shell function called
`psne' that ran all that with a single command. This:

  psne http://www.gnu.org/software/emacs/emacs-paper.html

would run the `mkdir', the `cd', the `wget' and the `echo' above.

If psne were just a shell script then it wouldn't be able to
change the current directory for the calling shell, so it had to
be defined as shell function instead of a script, and the user
had to patch his ~/.bashrc (or ~/.zshrc, or whatever) to install
the definition for psne and make it available. That was VERY
clumsy.

From now on we will use \"psne\" as a verb: to psne a URL means
to download a local copy of it into the right place, change to
its directory and save its name into the file \"~/.psne.log\".




3. The new way: `M-x brep'
==========================
Try to run this:

  (find-psne-links \"http://www.gnu.org/software/emacs/emacs-paper.html\")

or, equivalently, put the point on the URL below and then run
`M-x brep':

  http://www.gnu.org/software/emacs/emacs-paper.html

You will get a temporary buffer for psne-ing the URL above. It
will contain a `mkdir', a `cd', a `wget' and an `echo', plus an
eepitch block and some elisp hyperlinks, and it can be executed
with `F8's. Moral of the story: the \"new\" way to download a
local copy of a url is to put the point on it, then run `M-x
brep', then execute the resulting e-script. This does not require
any patching of rcfiles, as the shell-function version of `psne'
used to do.




4. The environment variable $S
==============================
If when eev is loaded by Emacs the environment variable $S is
unset, it will be set to a default value - namely, to the
expansion of \"$HOME/snarf\". Processes started from Emacs, such
as shells created with `eepitch-shell' or `find-sh', or external
terminals created by sexps like

  (find-bgprocess \"xterm\")
  (find-bgprocess \"gnome-terminal\")
  (find-bgprocess \"eterm\")

will then inherit that value. Try it:

  (getenv \"S\")
  (find-sh0 \"echo $S\")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
echo $S

Try also to create an external shell not from Emacs - for
example, from your window manager's list of available
applications, or from a text-mode login - and run \"echo $S\"
there: you will notice that $S is unset.

Old versions of eev used to require the user to run a script that
would patch his rcfiles (i.e., ~/.bashrc, ~/.zshrc, etc) to set
$S on startup. That turned out to be unreliable - it was better
to teach people how to distinguish those processes that inherit
$S from Emacs from those that don't, and let the experts patch
their rcfiles by hand.



5. `browse-url' and friends
===========================
If you place the point on the URL below

  http://www.gnu.org/software/emacs/emacs-paper.html

and run `M-x browse-url', Emacs will make an external browser
visit the remote version of that URL. One (bad) way to visit the
local copy of that URL is to modify the URL above by hand to
adjust it to your value of $S, until you obtain something like
this:

  file:///home/edrx/snarf/http/www.gnu.org/software/emacs/emacs-paper.html

and then run `M-x browse-url' on it.

One - rather primitive - way of visiting the local copy of that
URL with find-file is to modify the URL by hand, replacing its
\"http://\" with n \"$S/http/\", and then visit that file. For
example:

                http://www.gnu.org/software/emacs/emacs-paper.html
  (find-fline \"$S/http/www.gnu.org/software/emacs/emacs-paper.html\")

If you put the point on the URL and run `M-x brfl' on it you will
visit the local copy \"as a file\", with `find-file' /
`find-fline'. Visiting URLs - or their local copies - is
something that we do so frequently that we need ways to do that
with few keystrokes, which is why `brfl' has a short - and
cryptic - name. The conventions are:

  \"br\" is the common prefix for all the browse-url-like
       functions in eev,
  \"f\"  means to use `find-fline' (or, equivalently, `find-file'),
  \"l\"  is an optional suffix meaning to use the local copy.

The details on how to create these \"brxxx functions\" are here:

  (find-brxxx-intro)





6. `ee-flip-psne-ness'
======================
Converting a \"non-psne URL\" to a \"psne URL\" by hand, like this,

      https://tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf
  -> $S/https/tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf

is error-prone and boring.

Eev implements a command to do that, that works in both directions -
it is called `ee-flip-psne-ness', and it searches for the next
non-psne-or-psne URL and it \"flips its psne-ness\": it converts
non-psne URLs to psne URLs and psne URLs to non-psne URLs.

To try it you will have to run this:

  (define-key eev-mode-map \"\\M-s\" 'ee-flip-psne-ness)

because most people prefer to use the key `M-s' for their other
things. Then try it by putting the cursor here and typing `M-s' four
times. Watch the four psne-nesses below flip.

   https://tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf
  $S/https/tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf
   http://www.gnu.org/software/emacs/emacs-paper.html
  $S/http/www.gnu.org/software/emacs/emacs-paper.html





7. A historical note
====================
I wrote the first versions of \"psne\" in the late 1990s. At that point
I was using a program called \"snarf\" to fetch files from the internet
using FTP and HTTP, and I thought that it was natural to store the
files downloaded with snarf into the directory \"~/snarf/\". Later I
changed from snarf to wget, but I kept the directory as \"~/snarf/\".

I tried using several languages for the part that converted a url into
a directory. I still have the notes from my attempts to use Tcl and
Awk to do that - but at one point I managed to write a script in Perl
that was good enough and I stuck to that.

The shell function that I could call as

  psne      $S/http/www.gnu.org/software/emacs/emacs-paper.html

and it would run the four steps in

  mkdir -p http://www.gnu.org/software/emacs/
  cd       http://www.gnu.org/software/emacs/
  wget      $S/http/www.gnu.org/software/emacs/emacs-paper.html
  echo     'http://www.gnu.org/software/emacs/emacs-paper.html' >> ~/.psne.log

was called \"psne\" because it used a Perl script to obtain the
directory name, then it ran \"snarf\" (later \"wget\"), and it \"echo\"ed
the URL to the end of a log file. So \"p-sn-e\".





" rest)))

;; (find-enode "Command Index" "browse-url")
;; (find-efunction 'browse-url)
;; (find-elnode "System Environment")
;; (find-enode "Environment")
;; (find-eevfile \"eev.el\" \"$HOME/snarf\")

;; (find-psne-intro)








;;;                  _ _         __      _     _
;;;   __ _ _   _  __| (_) ___   / /_   _(_) __| | ___  ___
;;;  / _` | | | |/ _` | |/ _ \ / /\ \ / / |/ _` |/ _ \/ _ \
;;; | (_| | |_| | (_| | | (_) / /  \ V /| | (_| |  __/ (_) |
;;;  \__,_|\__,_|\__,_|_|\___/_/    \_/ |_|\__,_|\___|\___/
;;;
;; «find-audiovideo-intro» (to ".find-audiovideo-intro")
;; Skel: (find-intro-links "audiovideo")

(defun find-audiovideo-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-audiovideo-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-audiovideo-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-audiovideo-intro\")
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-videos-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Prerequisite:
  (find-pdf-like-intro)
This intro is being rewritten.




1. Time offsets
===============
Links to audio and video files are similar to links to pdf-like
documents, but instead of page numbers we use \"time offsets\" to
refer to positions. Time offsets are strings like 1:23, 12:34, or
1:23:45. The sexp hyperlinks below should all work if you have the
files that they refer to, and if you have mpv and xterm installed:

  (find-audio \"/tmp/mysong.mp3\")
  (find-audio \"/tmp/mysong.mp3\" \"1:23\")
  (find-audio \"/tmp/mysong.mp3\" \"1:23\" \"comments are ignored\")
  (find-video \"/tmp/myvideo.mp4\")
  (find-video \"/tmp/myvideo.mp4\" \"1:23\")
  (find-video \"/tmp/myvideo.mp4\" \"1:23\" \"comments are ignored\")

Note that they work by invoking an external player - mpv, by
default - and its error messages appear here:

  (find-ebuffer \"*Messages*\")

[Video links:]
  (find-eev2020video \"6:25\" \"`find-video'\")




2. `eev-avadj-mode'
===================
\"avadj-mode\" is a shorthand for \"audio/video adjust mode\".
When `eev-avadj-mode' is active we get keys for adjusting time
offsets quickly and for playing again the default audio or video
file at a given time offset, all of this without moving the
point. The keys are:

  M--    decrease the time offset by one second
  M-+    increase the time offset by one second
  M-=    same as M-+, for convenience
  M-p    play the default audio/video file at a time offset

You can toggle eev-avadj-mode on and off with `M-x
eev-avadj-mode', or with these sexps:

  (eev-avadj-mode 0)
  (eev-avadj-mode)

When it is on you will see an \"avadj\" at the mode line. Let's
examine `M--' and `M-+' first. With eev-avadj-mode on, try typing
several `M--'s and `M-+'s (or `M-='s) on the line below:

  This time offset - 9:59 - will change

Now, as an exercise, try to use `M--'s and `M-+'s/`M-='s, plus
`M-h M-2' (`ee-duplicate-this-line') and other more standard
editing commands, to convert this line

  (find-exampleaudio \"0:00\")

into:

  (find-exampleaudio \"0:00\")
  (find-exampleaudio \"0:12\" \"blah\")
  (find-exampleaudio \"0:30\" \"bleh\")

That should give you an idea of how to index audio or video files
- by creating elisp hyperlinks, with comments, to specific
positions in them. Of course in a real-world situation we would
execute these sexps occasionally to check if they are really
pointing to the right places, and then make further adjustments;
we are not doing that yet.

The idea of a \"default audio/video file\" will be explained in
section 4.4.




3. The time-from-bol
====================
All the keys in eev-avadj-mode operate on the \"time-from-bol\"
of the current line: the first occurrence, in the current line,
of a string that looks like a time offset. Note that the search
starts from the beginning of the line (\"-from-bol\"), and if
there are several possibilities, the first one is chosen.

Remember that `M-e' has a variant that just highlights what would
be executed, instead of evaluating a sexp:

  (find-eval-intro \"`M-0 M-e'\")

`M-p' also has something like this: `M-0 M-p' highlights the
time-from-bol and displays in the echo area the sexp that it
would execute to invoke a player - instead of running that sexp.
Try to evaluate these sexps:

  (code-audio \"sunwillset\" \"~/Zoe_Keating/Sun_Will_Set.ogg\")
  (find-sunwillset)
  ;; ^ don't worry if this fails - we are only calling it
  ;;   to set `ee-audiovideo-last'

and now try `M-0 M-p' on these lines:

  ;; 4:19 blah
  ;; 2:19

For more realistic examples, see:

  (find-videos-intro)





4. Short hyperlinks to audio and video files
============================================
This sexp

  (code-video \"ec2020video\" \"~/eev-videos/emacsconf2020.mp4\")

defines a function `find-ec2020video'. The function `code-video'
is similar to the functions `code-c-d' and `code-pdf-page', that
we saw in:

  (find-eev-quick-intro \"9. Shorter hyperlinks\")
  (find-pdf-like-intro \"7. Shorter hyperlinks to PDF files\")

After running the `(code-video ...)' above, this sexp

  (find-ec2020video \"8:20\" \"defines several functions\")

becomes a shorthand for:

  (find-video \"~/eev-videos/emacsconf2020.mp4\" \"8:20\")

Note that the string \"defines several functions\" is treated as a
comment, and is ignored - as in `find-pdf-page'.

If we run the second sexp below instead of the first one,

       (code-video \"ec2020video\" \"~/eev-videos/emacsconf2020.mp4\")
  (find-code-video \"ec2020video\" \"~/eev-videos/emacsconf2020.mp4\")

we get a temporary buffer with the code that the
sexp `(code-video ...)' would execute. Try it - and note that the
definition of `find-ec2020video' in the temporary buffer
contains a line like this:

  (setq ee-audiovideo-last 'find-ec2020video)

This line will be explained in the section 4.4.

[Video links:]
  (find-eev2020video \"12:54\" \"This block is a kind of an index for that video\")
  (find-eev2020video \"13:30\" \"we can index video tutorials\")




4.1. `find-extra-file-links'
----------------------------
The easiest way to produce `code-audio' and `code-video'
hyperlinks is with `M-h M-e', that runs `find-extra-file-links'.
This an experimental feature whose behavior may change soon, but
here is how it works now.

If you run

  (find-extra-file-links \"/tmp/foo.mp4\")

you will get a temporary buffer whose first line is

  ;; (find-extra-file-links \"/tmp/foo.mp4\" \"{c}\")

and that contains several blocks like this one:

  ;; Links to a video file:
  ;; (find-video \"/tmp/foo.mp4\")
  (code-video \"{c}video\" \"/tmp/foo.mp4\")
  ;; (find-{c}video)
  ;; (find-{c}video \"0:00\")

If you change the \"{c}\" in the first line to \"FOO\" and
execute it you will get a buffer generated from the same
template, but with all the \"{c}\"s replaced by \"FOO\"s. In that
buffer the block above will become this:

  ;; Links to a video file:
  ;; (find-video \"/tmp/foo.mp4\")
  (code-video \"FOOvideo\" \"/tmp/foo.mp4\")
  ;; (find-FOOvideo)
  ;; (find-FOOvideo \"0:00\")

The typical way of using `find-extra-file-links' is from dired,
by placing the cursor on the line of a file that you want to
create links to, and then typing `M-h M-e'. `M-h M-e' is similar
to the \"dired half\" of `M-h M-p' - see:

  (find-pdf-like-intro \"9. Generating three pairs\")
  (find-pdf-like-intro \"9. Generating three pairs\" \"M-h M-p\")

but `M-h M-e' produces many more links.

[Video links:]
  (find-eevtemplvideo \"28:12\" \"6. `find-here-links' and `find-extra-file-links'\")
  (find-eevtemplvideo \"30:18\"  \"`M-h M-e' runs `find-extra-file-links'\")
  (find-eevtemplvideo \"30:42\"  \"here is an example in Lisp\")
  (find-eevtemplvideo \"31:06\"  \"and I can change this {c}\")
  (find-eevtemplvideo \"31:22\"  \"Let me show a more realistic example\")
  (find-eevtemplvideo \"31:28\"  \"let's go to the directory with the video file\")
  (find-eevtemplvideo \"31:45\"  \"this file is a video file\")





4.2. `eewrap-audiovideo'
------------------------
And older, and clumsier, way of creating

If you type `M-V' (`eewrap-audiovideo') on a line containing a
shorthand word and a file name of an audio or video file, for
example, here,

  sunwillset  ~/Zoe_Keating/Sun_Will_Set.ogg

you will get something like this:

  ;; (find-fline \"~/Zoe_Keating/\")
  (code-audio \"sunwillset\" \"~/Zoe_Keating/Sun_Will_Set.ogg\")
  (code-video \"sunwillset\" \"~/Zoe_Keating/Sun_Will_Set.ogg\")
  ;; (find-sunwillset)
  ;; (find-sunwillset \"0:00\")

you should delete the line with the wrong sexp by hand - in this
case the wrong one is the one with `code-video', as we are
working with a sound file - and execute the other one; this will
define a function called `find-sunwillset', that plays the audio
file with `find-audio'. Run this this sexp to inspect its code:

  (find-code-audio \"sunwillset\" \"/tmp/Zoe_Keating__Sun_Will_Set.ogg\")

you will notice that running `find-sunwillset' sets a variable,
with:

  (setq ee-audiovideo-last 'find-sunwillset)

As we shall see soon, some operations play again the default
audio or video file, starting from some given time offset. The
default is always what is stored in `ee-audiovideo-last', and
each call to a short hyperlink of the form `find-xxxaudio' or
`find-xxxvideo' sets that variable.



4.3. A demo
-----------
Here's some code to test `find-video' and `code-video'. Make sure
that you have mpv installed, and run this escript block:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  # http://www.youtube.com/watch?v=K6LmZ0A1s9U
  # http://angg.twu.net/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4
  mkdir ~/eev-videos/
  cd    ~/eev-videos/
  wget -nc http://angg.twu.net/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4

It will download a copy of a video from youtube; I prepared the
.mp4 by running \"youtube-dl -f 18\" on the youtube URL and
renaming the result.

Then try:

  (find-video \"~/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4\")
  (code-video \"punchandjudyvideo\" \"~/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4\")
  (find-punchandjudyvideo)
  (find-punchandjudyvideo \"0:00\")
  (find-punchandjudyvideo \"0:10\" \"calls the baby\")
  (find-punchandjudyvideo \"0:40\" \"where's the baby\")
  (find-punchandjudyvideo \"1:04\" \"right position\")
  (find-punchandjudyvideo \"1:17\" \"he will sing the baby to sleep\")
  (find-punchandjudyvideo \"1:33\" \"1-2-3\")
  (find-punchandjudyvideo \"1:48\" \"baby downstairs\")
  (find-punchandjudyvideo \"3:12\" \"slaps\")
  (find-punchandjudyvideo \"3:50\" \"1-2-3\")
  (find-punchandjudyvideo \"4:34\" \"you keep an eye on mr Punch\")
  (find-punchandjudyvideo \"4:46\" \"hat\")
  (find-punchandjudyvideo \"5:03\" \"hat\")
  (find-punchandjudyvideo \"5:25\" \"did you see him?\")
  (find-punchandjudyvideo \"5:55\" \"clown\")
  (find-punchandjudyvideo \"6:14\" \"slaps\")
  (find-punchandjudyvideo \"6:52\" \"sausages\")
  (find-punchandjudyvideo \"7:24\" \"crocodile\")
  (find-punchandjudyvideo \"8:07\" \"crocodile + sausages\")
  (find-punchandjudyvideo \"8:32\" \"another scene\")
  (find-punchandjudyvideo \"8:39\" \"fight\")
  (find-punchandjudyvideo \"9:03\" \"clown\")
  (find-punchandjudyvideo \"9:45\" \"mr punch\")



4.4. The default audio/video file
---------------------------------
One of the things that the function `find-punchandjudyvideo' does
when executed is this:

  (setq ee-audiovideo-last 'find-punchandjudyvideo)

It sets the \"default audio/video file\" - more precisely, it
sets the global variable `ee-audiovideo-last' that indicate that
the way to play again the \"default audio/video file\" is by
running the function `find-punchandjudyvideo'.

This is similar to what the `find-xxxtext' functions do - they
store some information about the last PDF opened with a
`find-xxxtext' function into global variables. See:

  (find-pdf-like-intro \"11. How `M-h M-p' guesses everything\")
  (find-pdf-like-intro \"11. How `M-h M-p' guesses everything\" \"find-xxxtext\")

and, for more technical details:

  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-eev-quick-intro \"9.1. `code-c-d'\" \"find-code-c-d\")
  (find-code-video \"punchandjudyvideo\"
                   \"~/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4\")

In section 2 we mentioned that the key `M-p' in `eev-avadj-mode'
does this:

  M-p    play the default audio/video file at a time offset

Let's see in practice what this means. If we run these three
sexps here,

  (code-video \"punchandjudyvideo\" \"~/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4\")
  (find-punchandjudyvideo \"1:17\" \"he will sing the baby to sleep\")
  (eev-avadj-mode 1)

we will a) define `find-punchandjudyvideo', b) set the global
variable `ee-audiovideo-last' to `find-punchandjudyvideo', c)
turn `eev-avadj-mode' on. Now `M-p' should work! If you type
`M-p' on any of the lines with timestamps below it will open the
default audio/video file at that timestamp.

  0:00
  0:10 calls the baby
  0:40 where's the baby
  1:04 right position
  1:17 he will sing the baby to sleep
  1:33 1-2-3
  1:48 baby downstairs
  3:12 slaps
  3:50 1-2-3
  4:34 you keep an eye on mr Punch
  4:46 hat
  5:03 hat
  5:25 did you see him?
  5:55 clown
  6:14 slaps
  6:52 sausages
  7:24 crocodile
  8:07 crocodile + sausages
  8:32 another scene
  8:39 fight
  9:03 clown
  9:45 mr punch






5. Passing options to mpv
=========================
By default mpv is called with just a few command-line options,
besides the ones that tell it at what position to start playing -
typically just these for videos,

  -fs -osdlevel 2

to make it run in full-screen mode with an on-screen display
showing the current position, and no options for audio.

If you want to change this you should set the variable
`ee-mpv-video-options'. See:

  (find-efunction 'find-mpv-video)
  (find-evariable   'ee-mpv-video-options)

Here is an example of changing `'ee-mpv-video-options' temporarily:

  (defun find-mpv-rot90-video (fname &optional pos &rest rest)
    \"Like `find-mpv-video', but with the extra option '--video-rotate=90'.\"
    (interactive \"sFile name: \")
    (let ((ee-mpv-video-options
          (cons \"--video-rotate=90\" ee-mpv-video-options)))
      (find-mpv-video fname pos)))





6. Youtube-dl
=============
Videos at Youtube are identified by unique 11-char codes that are
assigned to them when they are uploaded. We will call those 11-char
codes \"hashes\", even though the term is not totally adequade in this
case, and we will explain the main ideas considering the case of an
imaginary video whose title is just TITLE, and whose hash is
\"abcdefghijk\". The URL to access that video at Youtube would be this:

  http://www.youtube.com/watch?v=abcdefghijk
                                 \\---------/
                                  its hash

If we execute this on a shell,

  cd /tmp/
  youtube-dl -t 'http://www.youtube.com/watch?v=abcdefghijk'

then youtube-dl would download a local copy of the video; due to the
option \"-t\" (\"--title\"), the name of the local copy would have both
the title of the video and its hash, and, if the video is in MP4
format, that would be

  /tmp/TITLE-abcdefghijk.mp4.part

during the download, and would be renamed to

  /tmp/TITLE-abcdefghijk.mp4

as soon as the download is finished.




6.1. Downloading a local copy
-----------------------------
Place the point at hash in the URL below,

  http://www.youtube.com/watch?v=abcdefghijk

and run `M-x find-youtubedl-links'; `find-youtubedl-links' will use
the hash at point as a default for one of its arguments, will run
something equivalent to this sexp,

  (find-youtubedl-links nil nil \"abcdefghijk\")

and will create a buffer like this:

   ___________________________________________________________________________
  |# (find-youtubedl-links \"/tmp/\" \"{title}\" \"abcdefghijk\" \"{ext-}\" \"{stem}\") |
  |# (find-youtubedl-links \"/tmp/\" nil \"abcdefghijk\" nil \"{stem}\")            |
  |                                                                           |
  |# (find-youtubedl-links \"~/videos/\" nil \"abcdefghijk\" nil \"{stem}\")        |
  |# (find-youtubedl-links \"~/videos/tech/\" nil \"abcdefghijk\" nil \"{stem}\")   |
  |# (find-youtubedl-links \"/tmp/videos/\" nil \"abcdefghijk\" nil \"{stem}\")     |
  |# (find-youtubedl-links \"/tmp/\" nil \"abcdefghijk\" nil \"{stem}\")            |
  |# (find-efunction 'find-youtubedl-links)                                   |
  |                                                                           |
  | (eepitch-shell2)                                                         |
  | (eepitch-kill)                                                           |
  | (eepitch-shell2)                                                         |
  |# http://www.youtube.com/watch?v=abcdefghijk                               |
  |# http://www.youtube.com/watch?v=abcdefghijk#t=0m00s                       |
  |# http://www.youtube.com/watch?v=abcdefghijk#t=0h00m00s                    |
  |cd /tmp/                                                                   |
  |youtube-dl -t 'http://www.youtube.com/watch?v=abcdefghijk'                 |
  |                                                                           |
  |# youtube-dl -t -F    'http://www.youtube.com/watch?v=abcdefghijk'         |
  |# youtube-dl -t -f 18 'http://www.youtube.com/watch?v=abcdefghijk'         |
  |                                                                           |
  |# (find-es \"video\" \"youtube-dl\")                                           |
  |# (find-fline \"/tmp/\" \"abcdefghijk\")                                       |
  |# (find-fline \"/tmp/\" \"{title}-abcdefghijk\")                               |
  |# (find-fline \"/tmp/\" \"{title}-abcdefghijk{ext-}\")                         |
  |# (find-video \"/tmp/{title}-abcdefghijk{ext-}\")                            |
  |# (find-video \"/tmp/{title}-abcdefghijk{ext-}.part\")                       |
  |# (code-video \"{stem}video\" \"/tmp/{title}-abcdefghijk{ext-}\")              |
  |# (code-video \"{stem}video\" \"/tmp/{title}-abcdefghijk{ext-}.part\")         |
  |# (find-{stem}video)                                                       |
  |# (find-{stem}video \"0:00\")                                                |
  |                                                                           |
  |# Error messages (for the player):                                         |
  |# (find-ebuffer \"*Messages*\")                                              |
  |                                                                           |
  |                                                                           |
  |--:**-  *Elisp hyperlinks*   All L1     (Fundamental)----------------------|
  |___________________________________________________________________________|

which has LOTS of things... the part

   (eepitch-shell2)
   (eepitch-kill)
   (eepitch-shell2)
  cd /tmp/
  youtube-dl -t 'http://www.youtube.com/watch?v=abcdefghijk'

is obvious: it is an eepitch script that downloads a local copy
of the video from Youtube.




6.2. Guessing the title and extension
-------------------------------------
Let's simulate what would happen after the eepitch script above -
Execute this:

  (find-sh0 \"rm -v  /tmp/TITLE-abcdefghijk*\")
  (find-sh0 \"echo > /tmp/TITLE-abcdefghijk.mp4.part\")

Now use `M-2 M-e' to compare the buffers generated by two calls
to `find-youtubedl-links' below:

  (find-youtubedl-links nil nil \"abcdefghijk\")
  (find-youtubedl-links \"/tmp/\" nil \"abcdefghijk\")

In the second one we get a buffer where all occurrences
of \"{title}\" have been substituted by \"TITLE\", and all
occurrences of \"{ext-}\" by \".mp4\". What happened was that

  (ee-youtubedl-guess* \"/tmp/\" \"abcdefghijk\")
     --> (\"/tmp/TITLE-abcdefghijk.mp4.part\")

did find files what that hash string in their names in the
directory \"/tmp/\", and the function `ee-youtubedl-split' has
picked up the first of these file names and has split it into
components:

  (ee-youtubedl-split \"/tmp/TITLE-abcdefghijk.mp4.part\")
     --> (\"/tmp/\" \"TITLE\" \"abcdefghijk\" \".mp4\" \".mp4.part\")

The last of these components is what we will call the \"ext\" -
the \"full extension\" - and the previous one is the \"ext-\" -
the \"extension minus its optional `.part'\". The first three
components are the \"dir\", the \"title\", and the \"hash\".




6.3. The first lines regenerate the buffer
------------------------------------------
The arguments to `find-youtubedl-links' are:

  (find-youtubedl-links DIR TITLE HASH EXT- STEM)

and we just saw how `ee-youtubedl-guess*' and
`ee-youtubedl-split' can be used to guess TITLE, EXT and EXT-
from DIR and HASH.

All the arguments to `find-youtubedl-links' have defaults,
that are used when the received arguments are nil:

  * when HASH is nil, use the youtube hash around point,
    or \"{hash}\" if none;
  * when DIR is nil, use the value of `ee-youtubedl-dir',
    or \"{dir}\" if none;
  * when TITLE or EXT- are nil use the guessing method described
    above, and when they fail use \"{title}\" or \"{ext-}\";
  * when STEM is nil, use \"{stem}\".

The first two lines in a `find-youtubedl-links' regenerate the
buffer, and are usually equivalent to one another. In the buffer
generated by:

  (find-youtubedl-links \"/tmp/\" nil \"abcdefghijk\")

they are:

  (find-youtubedl-links \"/tmp/\" \"TITLE\" \"abcdefghijk\" \".mp4\" \"{stem}\")
  (find-youtubedl-links \"/tmp/\" nil \"abcdefghijk\" nil \"{stem}\")

The first one has only non-nil arguments - all the rules for
guesses and defaults have been applied - where in the second one
TITLE and EXT- are made nil.



6.4. Selecting a directory
--------------------------
The second block of lines in the `find-youtubedl-links' buffer
are used to let we switch the directory quickly. If we just
execute `M-x find-youtubedl-links' with the point on our example
hash, or, equivalently, if we do this,

  (find-youtubedl-links nil nil \"abcdefghijk\")

you will see that the first two lines will be:

  (find-youtubedl-links \"~/videos/\" \"{title}\" \"abcdefghijk\" \"{ext-}\" \"{stem}\")
  (find-youtubedl-links \"~/videos/\" nil \"abcdefghijk\" nil \"{stem}\")

which means that the guessing process didn't find a downloaded
copy, as TITLE is \"{title}\" and EXT- is \"{ext-}\". That's because
we are using \"~/videos/\" as the DIR, and our file

  /tmp/TITLE-abcdefghijk.mp4.part

is elsewhere, and the guessing functions only search in one
directory...

The second block contains these sexps,

  (find-youtubedl-links \"~/videos/\" nil \"abcdefghijk\" nil \"{stem}\")
  (find-youtubedl-links \"~/videos/tech/\" nil \"abcdefghijk\" nil \"{stem}\")
  (find-youtubedl-links \"/tmp/videos/\" nil \"abcdefghijk\" nil \"{stem}\")
  (find-youtubedl-links \"/tmp/\" nil \"abcdefghijk\" nil \"{stem}\")

and if we execute the last one we set DIR to \"/tmp/\".

To change the dir strings \"~/videos/\", \"~/videos/tech/\", \"/tmp/videos/\",
\"/tmp/\", that appear in the second block of `find-youtubedl-links'
buffers, change the variables `ee-youtubedl-dir', `ee-youtubedl-dir2',
`ee-youtubedl-dir3', `ee-youtubedl-dir4.'





7. `code-psnevideo'
===================
If we execute these two sexps

  (code-psnevideo
   \"punchandjudy\"
   \"http://angg.twu.net/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4\"
   \"K6LmZ0A1s9U\")

  (find-punchandjudyvideo \"1:27\")

the `find-punchandjudyvideo' link will work in a way that is
quite different from the one in the demo in section 4.3. It will
open a temporary buffer in which the first line is a sexp - that
calls `find-psnevideo-links' - that regenerates that buffer, and
the second line is a low-level sexp like this, but in a single
line,

  (find-video
   \"$S/http/angg.twu.net/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4\"
   \"1:27\")

that will play the local copy of the video starting from 1:27;
this means to to use this sexp to play the video

  (find-punchandjudyvideo \"1:27\")

you have to first execute it with `M-e', then type the <down> key
to go the second line, then type `M-e' again.

The last part of that buffer will either be just a message saying

  # Local file found. No need to download it again.

or this message here,

  #  Local file not found! 
  #  You need to run this: 

followed by an eepitch block that you can you use to download the
MP4 file, like the one here:

  (find-psne-intro \"1. Local copies of files from the internet\")

The middle of that buffer will have other things, like a link
like this

  http://www.youtube.com/watch?v=K6LmZ0A1s9U#t=1m27s

to the video on youtube, and a call to `code-video' that will
redefine `find-punchandjudyvideo' to make it play the video
directly instead of creating a temporary buffer containing a link
to play it.



7.1. `code-eevvideo'
--------------------
`code-eevvideo' is a variant of `code-psnevideo' that lets us use
shorter sexps. If we call this,

  (code-eevvideo \"eevnav\" \"M-x-list-packages-eev-nav\")

it will add \"http://angg.twu.net/eev-videos/\" and \".mp4\" to
the string \"M-x-list-packages-eev-nav\" and then call
`code-psnevideo'. As the third argument was omitted it will be
set to \"{youtubeid}\". I am using `code-eevvideo' as an
experiment: when I need to send a short screencast to someone who
uses eev I record the video, upload it to
http://angg.twu.net/eev-videos/ - not to youtube - and send to
the person a pair of sexps like these:

  (code-eevvideo \"eevnav\" \"M-x-list-packages-eev-nav\" \"kxBjiUo88_U\")
  (find-eevnavvideo \"0:00\")




7.2. `find-eevvideo-links'
--------------------------
It may be simpler to explain `code-eevvideo' in another order,
starting from the function `find-eevvideo-links' - that, as its
name suggests, is a hyperlink to a temporary buffer containing
elisp hyperlinks (plus some parts generated by templates). A sexp
like

  (find-eevvideo-links \"eev2020\" \"emacsconf2020\" \"hOAqBc42Gg8\")

generates a temporary buffer whose first line follows the
convention that \"the first line regenerates the buffer\", and
its second line is a link like

  (find-video \"$S/http/angg.twu.net/eev-videos/emacsconf2020.mp4\")

that plays the local copy of the video (if it exists). That
temporary buffer also contains several \"help sexps\" that point
to parts of this intro, and also a part like

  # URL, local file, and a link to the directory of the local file:
  #               http://angg.twu.net/eev-videos/emacsconf2020.mp4
  #              $S/http/angg.twu.net/eev-videos/emacsconf2020.mp4
  # (find-fline \"$S/http/angg.twu.net/eev-videos/\")

  # Youtube:
  # (kill-new \"http://www.youtube.com/watch?v=hOAqBc42Gg8\")
  #            http://www.youtube.com/watch?v=hOAqBc42Gg8

that tries (!) to explain clearly how the URL and the file name
of the local copy were generated from the argument
\"emacsconf2020\" to `find-eevvideo-links', and how the youtube
URL was generated by the argument \"hOAqBc42Gg8\"; and the
temporary buffer also contains a last part with a script to
download the .mp4 file, and a help sexp that explains that.

That temporary buffer also contains a pair of sexps like

  (code-video \"eev2020video\" \"$S/http/angg.twu.net/eev-videos/emacsconf2020.mp4\")
  (find-eev2020video)

that are easy to understand - the first one defines
`find-eev2020video' as a short link to play the local copy of the
.mp4 file.

If you compare the temporary buffers generated by these two
sexps,

  (find-eevvideo-links \"eev2020\" \"emacsconf2020\" \"hOAqBc42Gg8\")
  (find-eevvideo-links \"eev2020\" \"emacsconf2020\" \"hOAqBc42Gg8\" \"17:20\")

you will see that the second sexp adds a time offset \"17:20\"s
at several places, and adds a \"#t=17m20s\"s at the end of each
youtube URL. These sexps and URLs can be used for _communication_
- for example, if I am chatting with someone on an IRC channel I
can say \"watch this:\", and then send these two lines:

  (find-eev2020video \"17:20\")
  http://www.youtube.com/watch?v=hOAqBc42Gg8#t=17m20s

If I take the

  (find-eevvideo-links \"eev2020\" \"emacsconf2020\" \"hOAqBc42Gg8\")

and change it to

        (code-eevvideo \"eev2020\" \"emacsconf2020\" \"hOAqBc42Gg8\")

this `code-eevvideo' sexps defines, or redefines,
`find-eev2020video', to a \"version for communication\", such
that

  (find-eev2020video \"17:20\")

runs

  (find-eevvideo-links \"eev2020\" \"emacsconf2020\" \"hOAqBc42Gg8\" \"17:20\")

that generates a temporary buffer with all the stuff described
above, instead of playing the video file right away - to play the
video file you have to execute the sexp

  (find-video \"$S/http/angg.twu.net/eev-videos/emacsconf2020.mp4\" \"17:20\")

in the second line of the temporary buffer.

There are some examples of `find-eevvideo-links' sexps here:

  (find-videos-intro \"2. Some `find-eevvideo-links'\")

At this moment I don't have variants of `find-eevvideo-links' and
`code-eevvideo' that point to other sides - see the comments
here:

  (find-eev \"eev-tlinks.el\" \"hardcoded-paths\")





" pos-spec-list)))

;; (find-audiovideo-intro)





;;;                  _ _   _          _           _
;;;  _ __ ___  _   _| | |_(_)_      _(_)_ __   __| | _____      __
;;; | '_ ` _ \| | | | | __| \ \ /\ / / | '_ \ / _` |/ _ \ \ /\ / /
;;; | | | | | | |_| | | |_| |\ V  V /| | | | | (_| | (_) \ V  V /
;;; |_| |_| |_|\__,_|_|\__|_| \_/\_/ |_|_| |_|\__,_|\___/ \_/\_/
;;;
;; «find-multiwindow-intro» (to ".find-multiwindow-intro")
;; Skel: (find-intro-links "multiwindow")

(defun find-multiwindow-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-multiwindow-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-multiwindow-intro)
Source code:  (find-efunction 'find-multiwindow-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.




1. Introduction
===============
In many situations - for example, when we want to script a
debugger, or to test programs that have to talk to one another,
or to control several external machines simultaneously - the
default window setup for eepitch, which is this,

   ____________________
  |          |         |
  |          |         |
  |  script  |  shell  |
  |          |         |
  |          |         |
  |__________|_________|

is not enough; other setups, like these,

   ______________________
  |          |           |        _________________________
  |          |  shell A  |       |            |            |
  |          |___________|       |   script   |     GDB    |
  |  script  |           |       |            |            |
  |          |  shell B  |       |____________|____________|
  |          |___________|       |            |            |
  |          |           |       |   program  |   program  |
  |          |  shell C  |       |     I/O    |    source  |
  |__________|___________|       |____________|____________|

may be necessary. Eev comes with a few _low-level_ tools for
creating these setups; they are not very smart, but they should
be easy to understand and to tweak - and I have the impression
that ideas for good high-level tools will only come from
practical experimentation.



2. `find-wset'
==============
Suppose that we are in a buffer A, and we want to create a window
configuration with A at the left, and with the buffers B and C
stacked on one another at the right. That is:

   ___________           ___________
  |           |         |     |     |
  |           |         |     |  B  |
  |     A     |   -->   |  A  |_____|
  |           |         |     |     |
  |           |         |     |  C  |
  |___________|         |_____|_____|

To do that from the keyboard we could type this:

  C-x 3   C-x o   C-x b B RET   C-x 2   C-x o   C-x b C RET

You can try that here (the initial `C-x 1' is an extra, for
convenience):

  (eek \"C-x 1         ;; delete-other-windows
        C-x 3         ;; split-window-horizontally (left/right)
        C-x o         ;; other-window              (-> right)
        C-x b B RET   ;; switch to the buffer `B'
        C-x 2         ;; split-window-vertically   (upper/lower)
        C-x o         ;; other-window              (-> lower right)
        C-x b C RET   ;; switch to the buffer `C'
        \")

We can write something equivalent to that as a `progn', in a way
that makes it easy to replace later the `C-x b B RET' and the
`C-x b C RET' by arbitrary sexp hyperlinks. We get:

  (progn (eek \"C-x 1 C-x 3 C-x o\")
         (find-ebuffer \"B\")
         (eek \"C-x 2 C-x o\")
         (find-ebuffer \"C\")
         (eek \"C-x o\")
         )

When I started to rewrite my window configurations into that form
I realized that the `eek's were being used in a very limited way
- they only invoked a very small repertoire of window commands,
all of them starting with `C-x'. So maybe I should have an
interpreter for a simple language of window commands and sexp
hyperlinks, in the which window setup above could be expressed
like this:

  '(\"13o\"
    (find-ebuffer \"B\")
    \"2o\"
    (find-ebuffer \"C\")
    \"o\"
    )

`find-wset' supports something like that, but with all the window
command strings collapsed into a single one, with \"_\"s meaning
\"execute the next sexp from the sexp list\". The corresponding
call to `find-wset' is:

  (find-wset \"13o_2o_o\" '(find-ebuffer \"B\") '(find-ebuffer \"C\"))

For the full list of supported window command characters - and
how to extend it - see the source:

  (find-eev \"eev-multiwindow.el\")




3. High-level words
===================
Very often we want to create window setups like

   _______________            _______________
  |       |       |          |       |       |
  |       |       |          |       |   B   |
  |   A   |   B   |    or    |   A   |_______| ;
  |       |       |          |       |       |
  |       |       |          |       |   C   |
  |_______|_______|          |_______|_______|

there are shorthands for that. If you run

  (find-2a sexpA sexpB)

that will create a window setting like the one at the left above,
initially with two copies of the current buffer, then will run
sexpA at the window \"A\" and sexpB at the window \"B\", and
finally will select the window \"A\", i.e., leave the cursor at
the window at the left; this

  (find-2b sexpA sexpB)

will do exactly the same as the `(find-2a ...)' above, but will
select the window \"B\" - the one at the right - at the end of
the process. For three-window settings we have these:

  (find-3a sexpA sexpB sexpC)
  (find-3b sexpA sexpB sexpC)
  (find-3c sexpA sexpB sexpC)

all three create the three-window setting at the right above,
initially with all three windows displaying the current buffer,
then run sexpA at the window \"A\", sexpB at the window \"B\",
and sexpC at the window \"C\"; the difference is that find-3a
selects the window \"A\", find-3b the window \"B\", find-3c the
window \"C\".




4. Several eepitch targets
==========================
If we try to build a window setup like this one, with two eepitch
targets, with just `find-wset', we will run into problems -

   ________________________
  |          |             |
  |          |   *shell*   |
  |  script  |_____________|
  |          |             |
  |          |  *shell 2*  |
  |__________|_____________|

because `(eepitch-shell)' and `(eepitch-shell2)' try to create a
shell buffer and put it in an _another_ window, not the one we
are in... one solution is to call the `(eepitch-*)' sexps inside
an `ee-here', like this:

  (ee-here '(eepitch-shell))
  (ee-here '(eepitch-shell2))

where `ee-here' is a hack that runs a sexp in a way that
preserves the current window configuration, then switches the
buffer in the current selected window to the current eepitch
target. We can use this to create the window setting above,

  (find-wset \"13o2_o_o\"
             ' (ee-here '(eepitch-shell))
             ' (ee-here '(eepitch-shell2))
             )

This is too long - and would make a very bad one-liner - but
there are two shorthands. First, \"e\" is a variant of \"_\" that
runs its sexp inside an `(ee-here ...) - so this is equivalent
the thing above,

  (find-wset \"13o2eoeo\"
             '(eepitch-shell)
             '(eepitch-shell2)
             )

Second, these things are useful enough to deserve a high-level
word, so this is equivalent to:

  (find-3ee '(eepitch-shell) '(eepitch-shell2))




5. Restarting eepitch targets
=============================
Sometimes we want to do the same as above, but restarting both
eepitch targets, i.e., something like this:

  (find-3ee '(progn (eepitch-shell)  (eepitch-kill) (eepitch-shell))
            '(progn (eepitch-shell2) (eepitch-kill) (eepitch-shell2))
            )

There's a variant of `ee-here' that does that: `ee-here-reset'.
For example,

  (ee-here-reset '(eepitch-shell2))

is equivalent to:

  (ee-here '(progn (eepitch-shell2) (eepitch-kill) (eepitch-shell2)))

and the letter \"E\" is a variant of \"e\" that uses
`ee-here-reset' instead of `ee-here'; also, `find-3EE' is a
variant of `find-3ee' that restarts both targets. Let's adapt
this example,

  (find-eepitch-intro \"3. Other targets\")

to make it show the two eepitch targets at once in a three-window
settings. It becomes:

 (find-3EE '(eepitch-shell) '(eepitch-python))
 (eepitch-shell)
echo Hello... > /tmp/o
 (eepitch-python)
print(open(\"/tmp/o\").read())
 (eepitch-shell)
echo ...and bye >> /tmp/o
 (eepitch-python)
print(open(\"/tmp/o\").read())

 Now compare:
 (eek \"C-x 1\")
 (find-3ee '(eepitch-shell) '(eepitch-python))
 (find-3EE '(eepitch-shell) '(eepitch-python))




6. Non-trivial examples
=======================
See:

  (find-eev-quick-intro \"6.2. Other targets\")
  (find-eev-quick-intro \"6.2. Other targets\" \"(find-3EE\")
  (find-rcirc-intro \"1. The example that I use in workshops\")
  (find-prepared-intro \"3. An `ee' for Python\")
  (find-prepared-intro \"4. `eepy'\")

The examples with `find-3EE' were created using `M-#', as
explained in the next section.




7. Eepitch blocks for two targets
=================================
An eepitch script with two targets uses several different kinds
of red star lines - `(eepitch-target1)', `(eepitch-target2)',
`(find-3EE ...)', `(find-3ee ...)', etc. We don't want to have to
type all those by hand, so there is a hack similar to `M-T' that
generates all those kinds from just \"target1\" and \"target2\"
to let us just copy around the sexps we need.

This key binding for this hack is `meta-shift-3' - that Emacs
sees as `M-#' - but it is disabled by default. To enable it, do
this:

  ;; See: (find-eevfile \"eev-mode.el\" \"eewrap-two-eepitches\")
  (define-key eev-mode-map \"\\M-#\" 'eewrap-two-eepitches)

Now compare the result of typing `M-T' here,

python

with the result of typing `M-#' on this line,

shell python

which yield this:

 (find-3EE '(eepitch-shell) '(eepitch-python))
 (find-3ee '(eepitch-shell) '(eepitch-python))
 (eepitch-shell)
 (eepitch-python)

Remember that The line with `find-3EE' restart the two targets,
and the line with `find-3ee' just recreates the window setting
with the two targets but without restarting them; so the line
with `find-3EE' sort of works as two `eepitch-kill's.




8. Adding support for new characters in `find-wset'
===================================================
The standard characters supported by `find-wset' are these:

  char  action                       key
  ---- ---------------------------- ---------
  `1'  `delete-other-windows'       (C-x C-1)
  `2'  `split-window-vertically'    (C-x C-2)
  `3'  `split-window-horizontally'  (C-x C-3)
  `s'  `split-window-sensibly'
  `o'  `other-window'               (C-x o)
  `+'  `balance-windows'            (C-x +)
  `_'  execute the next sexp

but the action of each one is defined in a different function,
and to add support for a new character, say, `=', we just need to
define a function with the right name - in this case,
`find-wset-='.

The source code is simple enough, so take a look:

  (find-eev \"eev-multiwindow.el\" \"find-wset-_\")

" pos-spec-list)))

;; (find-multiwindow-intro)



;;;           _
;;;  _ __ ___(_)_ __ ___
;;; | '__/ __| | '__/ __|
;;; | | | (__| | | | (__
;;; |_|  \___|_|_|  \___|
;;;
;; «find-rcirc-intro» (to ".find-rcirc-intro")
;; Skel: (find-intro-links "rcirc")

(defun find-rcirc-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-rcirc-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-rcirc-intro)
Source code:  (find-efunction 'find-rcirc-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Recent versions with Emacs come with two IRC clients built-in:
Rcirc and ERC. I never understood ERC well enough, and I found
rcirc quite easy to understand and to hack, so eev has some
support for rcirc (and no support for ERC).

  (find-node \"(rcirc)Top\")
  (find-node \"(erc)Top\")

The eev support for rcirc consists mainly of three high-level
functions that connect to Freenode (the IRC server where most
discussion of free software projects USED TO happen), and three
high-level functions that connect to LiberaChat (the IRC server
where most discussion of free software projects were moved to).
These functions are called:

  `find-freenode', `find-freenode-2a' and `find-freenode-3a',
  `find-libera', `find-libera-2a' and `find-libera-3a'.

For a good explanation of what IRC is, see:

  http://www.irchelp.org/faq/new2irc.html




1. The example that I use in workshops
======================================
Let's start with an example. In

  (setq rcirc-default-nick \"hakuryo\")
  (setq ee-libera-ichannels \"#eev\")
  (find-libera-3a \"#eev\")

the first sexp tells rcirc to use the nickname \"hakuryo\" when
connecting to an IRC server; the second sets the set of \"initial
channels\" on LiberaChat to just one channel, #eev - a channel
that is usually empty, but that doesn't require authentication;
the third sexp is a \"sexp hyperlink to the LiberaChat channel
#eev\". The third sexp:

  1) creates a window setting like this,

       _________________________
      |           |             |
      |           |  LiberaChat |
      |           |    server   |
      |           |   messages  |
      |  current  |_____________|
      |  buffer   |             |
      |           |    #eev     |
      |           |   channel   |
      |           |             |
      |___________|_____________|

  2) tells rcirc to connect to LiberaChat and to the channel #eev
     in it,

  3) makes the window at the left - window \"A\" in the
     terminology of eev-multiwindow.el - the active window. See:

       (find-multiwindow-intro \"3. High-level words\")
       (find-multiwindow-intro \"3. High-level words\" \"find-3a\")

The connection process takes time - about 20 seconds at my
machine - but you will be able to see in window \"B\" the server
messages as they appear, and in window \"C\" the messages of the
#eev channel. You can then use the window \"C\" to interact with
the other users in #eev, and to experiment with commands. See:

  (find-rcircnode \"Internet Relay Chat\" \"Once you have joined a channel\")
  (find-rcircnode \"Getting started with rcirc\" \"To talk in a channel\")
  (find-rcircnode \"rcirc commands\" \"/join #emacs\")




2. The two-window setting
=========================
Try this:

  (find-libera-2a \"#eev\")

It creates a window setting like

   _________ ________
  |         |        |
  |         |        |
  | current |  irc   |
  | buffer  | buffer |
  |         |        |
  |_________|________|

which is nice for when you don't want to follow the irc server
messages.




3. Tracking activity
====================
TODO: explain this:

  (find-rcircnode \"Channels\" \"M-x rcirc-track-minor-mode\")

and how to use it as a one-window setting. Also:

  (find-efunctiondescr 'rcirc-track-minor-mode)
  (find-efunction      'rcirc-track-minor-mode)
  (find-evariable      'rcirc-track-minor-mode-map)
  (find-ekeymapdescr    rcirc-track-minor-mode-map)

  (find-efunctiondescr 'rcirc-next-active-buffer)
  (find-efunction      'rcirc-next-active-buffer)

  (global-set-key [f2] 'rcirc-next-active-buffer)

  (find-eev \"eev-elinks.el\" \"find-esetkey-links\")
  (find-eev \"eev-elinks.el\" \"find-esetkey-links\" \"video\")
  (find-esetkey-links (kbd \"<f2>\") 'rcirc-next-active-buffer)




4. Commands with very short names
=================================
We can apply this idea

  (find-eev-quick-intro \"7.4. Commands with very short names\")
  (find-eev-quick-intro \"7.4. Commands with very short names\" \"(defun c ()\")

to rcirc. If you connect occasionally to the channels #eev,
#emacs, #git and #ruby, you can run this, or put these lines in
your .emacs:

  (setq rcirc-default-nick \"hakuryo\")
  (defun e2 () (interactive) (find-libera-2a \"#eev\"))
  (defun e3 () (interactive) (find-libera-3a \"#eev\"))
  (defun m2 () (interactive) (find-libera-2a \"#emacs\"))
  (defun m3 () (interactive) (find-libera-3a \"#emacs\"))
  (defun g2 () (interactive) (find-libera-2a \"#git\"))
  (defun g3 () (interactive) (find-libera-3a \"#git\"))
  (defun r2 () (interactive) (find-libera-2a \"#ruby\"))
  (defun r3 () (interactive) (find-libera-3a \"#ruby\"))




5. `find-libera-links'
======================
You can generate lines like the ones above by running
`find-libera-links'. For example:

  (find-libera-links \"e\" \"#eev\")
  (find-libera-links \"r\" \"#ruby\")



6. Other servers
================
TODO: explain how to use find-rcirc-buffer and how to adapt
find-libera-* to other servers. Example:

  (find-rcirc-buffer-2a \"irc.debian.org\" \"#debian-live\" nil \"#debian-live\")
  (find-rcirc-buffer-3a \"irc.debian.org\" \"#debian-live\" nil \"#debian-live\")

See:

  (find-eev \"eev-rcirc.el\" \"find-libera\")

" pos-spec-list)))

;; (find-rcirc-intro)





;;;  _                       _       _
;;; | |_ ___ _ __ ___  _ __ | | __ _| |_ ___  ___
;;; | __/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \/ __|
;;; | ||  __/ | | | | | |_) | | (_| | ||  __/\__ \
;;;  \__\___|_| |_| |_| .__/|_|\__,_|\__\___||___/
;;;                   |_|
;;
;; «find-templates-intro» (to ".find-templates-intro")
;; Skel: (find-intro-links "templates")

(defun find-templates-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-templates-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-templates-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-templates-intro\")
More intros:  (find-eev-quick-intro)
              (find-escripts-intro)
              (find-links-conv-intro)
              (find-eev-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



This intro is being rewritten!
The prerequisites for understand this are:
  (find-elisp-intro)
  (find-links-conv-intro \"3. Classification\")




1. Introduction
===============
In dec/2019 I sent this e-mail to the eev mailing list:

  https://lists.gnu.org/archive/html/eev/2019-12/msg00001.html

It was a kind of a call for help. It contained a very brief
explanation of how the \"templated\" functions of eev, like
`find-ekey-links' and `find-latex-links', are implemented, and
showed how people can write their own templated functions as
quick hacks.

If you want to learn how to _use_ templated functions, start by:

  (find-eev-quick-intro \"4.2. `find-ekey-links' and friends\")
  (find-eev-quick-intro \"7.5. `find-latex-links'\")

If you want to look at the source code of the existing templated
functions, take a look at:

  (find-eev \"eev-elinks.el\")
  (find-eev \"eev-tlinks.el\")

  (find-links-intro \"3. Elisp hyperlinks buffers conventions\")

This tutorial is for people who want to learn how to _write_
their own templated functions.

To learn how to write your own templated functions you need to:

  1) learn how to use `ee-template0' by reading its source code
     and playing with examples in the source and here,

  2) learn how to use `find-elinks' - same thing,

  3) learn how to use `find-find-links-links-new'.





2. `ee-template0'
=================
See:

  (find-efunction 'ee-template0)
  (find-eev \"eev-template0.el\")

Try:

  (ee-template00 \"a{(+ 2 3)}b\")

  (let ((hi \"Here: \")
        (a 22)
        (b 33))
    (ee-template00 \"{hi}{a}+{b}={(+ a b)}\"))

  (defun foo (a b) (ee-template00 \"{a}+{b}={(+ a b)}\"))
  (foo 22 33)

  (ee-template0 \"{<} a{(+ 2 3)} {>}\")




3. `find-elinks'
================
See:

  (find-efunction 'find-elinks)
  (find-eev \"eev-elinks.el\" \"find-elinks\")

Now try these examples. They are multi-line versions with
comments of the examples in the source file.

   (find-elinks
     '((a sexp)
       \"a string\")
     )

Now try these examples. They are longer, multi-line versions of
the examples in the source file.

   (find-elinks
     '((a sexp)
       \"a string\")
     )

   (find-elinks
     '((a sexp)
       \"a string\")
     \"st\")

   (find-elinks
     '((a sexp)
       \"a string\")
     \"st\" \"i\")

   (find-elinks
     '((a sexp)
       (another sexp)
       (sexps get comment signs)
       (strings in sexps: \"foo  bar\")
       (newlines in strings in sexps get backslashed: \"\\n\")
       (ticks in sexps: 'a '(b c))
       (nils in sexps: nil () (nil nil))
       \"a string\"
       \"another string\"
       \"strings don't get comment signs\"
       \"empty strings become empty lines\"
       \"\"
       \"newlines in strings\\nbecome real newlines\"
       \"nils are dropped:\"
       nil
       \"see?\"
       \"\"
       (another sexp)
       )
     )

Normally the first argument to `find-elinks' is backquoted. See:

  (find-elnode \"Backquote\")

Try:

  `(foo ,(+ 2 3) bar)
  `(foo ,'(+ 2 3) bar)
  `(foo ,(list 2 3) bar)
  `(foo ,@(list 2 3) bar)

See:

  (find-eev \"eev-elinks.el\" \"find-efunction-links\")

The first argument to `find-elinks' is called LIST. Elements of
LIST that are sexps are converted to strings using `ee-HS'. See:

  (find-eev \"eev-wrap.el\" \"ee-S\")



4. Skels
========
Many functions in eev have comments that start with \";; Skel:\",
like this:

  ;; Skel: (find-find-links-links-new \"fossil\" \"url subdir c\" \"\")

A comment like that before a function means that I wrote that
function by first running that sexp and then modifying the code
that that sexp generated, that was a \"skeleton\".

Try:

  (find-find-links-links-new \"fossil\" \"url subdir c\" \"\")
  (find-eev \"eev-tlinks.el\" \"find-fossil-links\")
  (find-eevgrep \"grep --color -nH --null -e Skel: *.el\")





5. `find-find-links-links'
==========================
(Note: `find-find-links-links' is obsolete, and is superseded by
`find-find-links-links-new')

ALL my `find-*-links' started as quick hacks.
SOME of them were useful enough to deserve being cleaned up.
A FEW of them ended up in:

  http://angg.twu.net/eev-current/eev-elinks.el.html
  http://angg.twu.net/eev-current/eev-tlinks.el.html
  (find-eev \"eev-elinks.el\")
  (find-eev \"eev-tlinks.el\")

...but there are lots of other `find-*-links' functions in:

  http://angg.twu.net/.emacs.templates.html

They are trivial to write. I start with a skeleton that I obtain by
running `M-x find-find-links-links', and then I modify the first line
in that buffer, regenerate, modify, regenerate, and so on until happy.
Run each of the sexps below with `M-2 M-e' to compare the buffers that
they generate:

  (find-find-links-links \"{k}\" \"{stem}\" \"{args}\")
  (find-find-links-links \"\\\\M-u\" \"{stem}\" \"{args}\")
  (find-find-links-links \"\\\\M-u\" \"macports\" \"{args}\")
  (find-find-links-links \"\\\\M-u\" \"macports\" \"pkgname\")
  (find-find-links-links \"\\\\M-u\" \"macports\" \"pkgname anotherarg\")




So: start by running something like

  (find-find-links-links \"\\\\M-u\" \"macports\" \"pkgname\")
  (find-find-links-links \"\\\\M-u\" \"homebrew\" \"pkgname\")

then copy the

\(define-key eev-mode-map \"\\M-h\\M-u\" 'find-macports-links)

\(defun find-macports-links (&optional pkgname &rest pos-spec-list)
\"Visit a temporary buffer containing hyperlinks for foo.\"
  (interactive)
  (setq pkgname (or pkgname \"{pkgname}\"))
  (apply 'find-elinks
   `((find-macports-links ,pkgname ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-macports-links)
     \"\"
     ,(ee-template0 \"\\
\")
     )
   pos-spec-list))

;; Test: (find-macports-links ___)

to your notes, replace the `(interactive)' by

  (interactive (list (ee-debpkgname-ask)))

and start adding things to the string in (ee-template0 \"...\").

I will try to update this intro in the next days:

  (find-templates-intro)
  http://angg.twu.net/eev-intros/find-templates-intro.html





Etc:

  (find-links-conv-intro)
  (find-links-conv-intro \"3. Classification\")
  (find-eev \"eev-tlinks.el\" \"find-find-links-links\")
  (find-eev \"eev-tlinks.el\" \"find-intro-links\")
  (find-eev \"eev-wrap.el\" \"find-eewrap-links\")

" rest)))

;; (find-templates-intro)




;;;                                          _
;;;  _ __  _ __ ___ _ __   __ _ _ __ ___  __| |
;;; | '_ \| '__/ _ \ '_ \ / _` | '__/ _ \/ _` |
;;; | |_) | | |  __/ |_) | (_| | | |  __/ (_| |
;;; | .__/|_|  \___| .__/ \__,_|_|  \___|\__,_|
;;; |_|            |_|
;;
;; «find-prepared-intro»  (to ".find-prepared-intro")
;; (find-eev "eev-bounded.el")

(defun find-prepared-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-prepared-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-prepared-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-prepared-intro\")
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



1. Prepared shells
==================
Long before eepitch had been created, eev had another way -
technically much simpler, but clumsier from the user's point of
view - to send commands to external shells (and other shell-like
programs; but to simplify we will say just \"shells\"). Here is
an overview of how it worked: if the user marked the three lines
below,

  rm -Rfv /tmp/foo/
  mkdir   /tmp/foo/
  cd      /tmp/foo/

and typed `M-x eev' (which stood for \"Emacs-execute-verbosely\")
then Emacs would save those three lines into a temporary script
file, usually \"~/.eev/ee.sh\"; that would be just half of
\"sending commands to an external shell\", and for the other half
the user would have to go to an external prepared shell - that
would usually be running in an xterm, and totally independent
from Emacs - and type \"ee\" there. The shell had to be
\"prepared\" in the sense that it would understand the \"ee\"
command correctly, as meaning: \"execute the commands in the
temporary script as if the user were typing them at the prompt\".
Technically, that would mean that instead of calling
\"~/.eev/ee.sh\" as a shell script its contents would be
\"sourced\" - i.e., executed in the current shell context - and
in verbose mode.

Usually we would prepare bash by patching the file ~/.bashrc and
putting the definition for \"ee\" there. We will discuss how to
do that later; now let's test a simple environment in which `M-x
eev' and \"ee\" work. First execute these two sexps:

  (make-directory \"~/.eev/\" 'force)
  (eev \"rm -Rv /tmp/foo\\nmkdir  /tmp/foo/\\ncd     /tmp/foo/\\n\")

Now run this script:

 (eepitch-bash)
 (eepitch-kill)
 (eepitch-bash)
export PS1='$PWD# '
function ee () { set -v; . ~/.eev/ee.sh; set +v; }
ee



2. `ee'
=======
\[TODO: Write this section! Explain how several interpreters can
be programmed to accept an `ee' command to execute temporary
scripts...]

  See (obs: this is very old!):
  http://angg.twu.net/eev-article.html#making-progs-receive-cmds

  (find-eev \"eev-langs.el\")
  (find-eev \"eev-bounded.el\")
  (find-eev \"eev-rctool\")




3. An `ee' for Python
=====================
Here is a simple way to make Python execute commands saved in a
temporary script when the user types `ee()' (note that it is not
just `ee' - the `()' is needed). We will show first an example in
which the temporary script is prepared by running \"cat\" from a
shell - then we will explain a more user-friendly way to save a
region from the current buffer as the temporary script.

Note that the demo below uses `find-wset', which is an
advanced (i.e., hackish) feature explained here:

  (find-multiwindow-intro \"Several eepitch targets\")

 (find-3EE '(eepitch-shell) '(eepitch-python))
 (find-3ee '(eepitch-shell) '(eepitch-python))
 (eepitch-python)
import os
def ee():
  exec(open(os.getenv(\"HOME\")+\"/.eev/ee.py\").read(), globals())

 (eepitch-shell)
cat > ~/.eev/ee.py <<'%%%'
print(1+2)
%%%

 (eepitch-python)
ee()

 (eepitch-shell)
cat > ~/.eev/ee.py <<'%%%'
def foo (x):
    return x*x

print(foo(5))
%%%

 (eepitch-python)
ee()
print(foo(6))



4. `eepy'
=========
The function `eev' receives three parameters, called `s', `e', and
`altfile'; `e' and `altfile' are optional, and `s' should be either a
string or a number. When `s' is a string, then the commands to be
saved into the temporary script are taken from `s'; the numeric case
will be discussed later.

A call to

  (eev \"print(1+2)\" nil \"~/.eev/ee.py\")

writes \"print(1+2)\" (with an added trailing newline, but that's
a technical detail) into the \"alternative file\"
\"~/.eev/ee.py\" - the default would be \"~/.eev/ee.sh\". We can
use that to simplify our demo a bit:

 (eek \"C-x 1\")
 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
import os
def ee():
  exec(open(os.getenv(\"HOME\")+\"/.eev/ee.py\").read(), globals())

 (eev \"print(1+2)\" nil \"~/.eev/ee.py\")
ee()
 (eev \"def foo (x):\\n    return x*x\\n\\nprint(foo(5))\" nil \"~/.eev/ee.py\")
ee()
print(foo(6))


  In the example below the first line defines a `eepy' in a
  simplistic way:

 (defun eepy (s &optional e) (eev s e \"~/.eev/ee.py\"))
 (eek \"C-x 1\")
 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
import os
def ee():
  exec(open(os.getenv(\"HOME\")+\"/.eev/ee.py\").read(), globals())

 (eepy \"print(1+2)\")
ee()
 (eepy \"def foo (x):\\n    return x*x\\n\\nprint(foo(5))\")
ee()
print(foo(6))




5. `M-x eepy' and `M-x eev'
===========================
Now let's define a more realistic `eepy' - one that can also be
called interactively. We want `M-x eepy' to save the current
_region_ into the temporary script; `eepy' has to be a _command_,
and we will use the argument \"r\" to its `interactive' clause,
to make the function `eepy' receive two numbers - the start and
the end of the region - and it will pass these two numbers to
`eev'.

  (defun eepy (s &optional e)
  \"Save the region between S and E (or the string S) into ~/.eev/ee.py .\"
    (interactive \"r\")
    (eev s e \"~/.eev/ee.py\"))

When the first argument, `s', to `eev', is a number, not a
string, then `eev' expects the second argument, `e', to also be a
number - and then `s' and `e' are considered as the extremities
of a region of text in the current buffer. This idea - that the
first argument can be either a string or a number - comes from:

  (find-efunctiondescr 'write-region \"If START is a string\")

But try these:

  (ee-se-to-string         \"foo\" nil)
  (ee-se-to-string-with-nl \"foo\" nil)
  (ee-se-to-string         \"foo\\n\" nil)
  (ee-se-to-string-with-nl \"foo\\n\" nil)
  (ee-se-to-string         (- (point) 5) (point))
  (ee-se-to-string-with-nl (- (point) 5) (point))
  (ee-se-to-string         (point) (- (point) 5))
  (ee-se-to-string-with-nl (point) (- (point) 5))




\[Garbage:\]

  (find-elnode \"Defining Commands\")
  (find-defun-intro \"\\ninteractive\\n\")
  (find-efunction 'eev)

" rest)))

;; (find-prepared-intro)

;; (find-bashnode "Bourne Shell Builtins" "current shell context")




;;;  _                           _          _
;;; | |__   ___  _   _ _ __   __| | ___  __| |
;;; | '_ \ / _ \| | | | '_ \ / _` |/ _ \/ _` |
;;; | |_) | (_) | |_| | | | | (_| |  __/ (_| |
;;; |_.__/ \___/ \__,_|_| |_|\__,_|\___|\__,_|
;;;
;; «find-bounded-intro» (to ".find-bounded-intro")
;; Skel: (find-intro-links "bounded")
(defun find-bounded-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-bounded-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-bounded-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-bounded-intro\")
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Note that you need to understand the concept of \"prepared
shells\" quite well to be able to use this... see:

  (find-prepared-intro)



Bad news: I've been using this feature very little, and I have
not yet adapted the old, crappy docs to the new \"intro\"
format... =\\ So this is just a bunch of notes!

Source code:           \(find-eev \"eev-bounded.el\")
Obsolete related code: \(find-eev \"eev-langs.el\")
Old mentions to this:  \(find-TH \"eev-article\" \"delimited-regions\")
                 http://angg.twu.net/eev-article.html#delimited-regions




Delimited (\"bounded\") regions
=============================
Try:

#
# (eev-bounded)
cd
echo At: $PWD
cd /tmp/
echo At: $PWD

#
%
% (eelatex-bounded)

Hello

%


Defining new bounded functions
==============================
Try:

  (find-code-bounded 'eev-bounded 'eev \"\\n#\\n\")
  (find-code-bounded 'eev-bounded 'eev 'ee-delimiter-hash)

as usual, when we remove the \"find-\"s the generated code is
executed instead of displayed.




The default bounded function
============================
...is stored in the variable `ee-bounded-function', and can be
re-run with `M-x ee-bounded-function' (i.e., there's a function
with the same name as the variable). I used to bind `f3' to that,
but in modern Emacsen this is bound to a macro key:

  (find-enode \"Basic Keyboard Macro\" \"<F3>\")

so you should do something like this, but for your favourite key:

  (define-key eev-mode-map [f3] 'ee-bounded-function)
" pos-spec-list)))

;; (find-bounded-intro)




;;;       _                            _
;;;   ___| |__   __ _ _ __  _ __   ___| |___
;;;  / __| '_ \ / _` | '_ \| '_ \ / _ \ / __|
;;; | (__| | | | (_| | | | | | | |  __/ \__ \
;;;  \___|_| |_|\__,_|_| |_|_| |_|\___|_|___/
;;;
;; «find-channels-intro» (to ".find-channels-intro")
;; Skel: (find-intro-links "channels")

(defun find-channels-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-channels-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-channels-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-channels-intro\")
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



1. Introduction
===============
Before eepitch had been invented eev had two other ways to send
commands to external shell-like programs. The first way,
described here,

  (find-prepared-intro)
  (find-prepared-intro \"2. `ee'\")

was technically very simple: running `M-x eev' would save a
series of commands - usually the contents of the region - into a
temporary script file, and then the user would type something
like `ee' at the prompt of a (\"prepared\") shell; that would
make it read the saved commands and execute them.

Here we will describe the second of the Old Ways - one in which
the target program, which is usually a shell running in an xterm,
is sent a signal saying \"execute the command NOW\" as soon as a
command is saved into a temporary file by Emacs. The difficulty
is that this requires not only a directory for temporary files,
but also an Expect script, which acts as an intermediary that
listens to signals and handles them pretending that the saved
commands came from the keyboard... and, as some people have said,
this is \"a nightmare to set up\".

Here we explain the protocol - that can be adapted to other
cases too, like, for example, to make Emacs talk to SmallTalk
environments and to computer algebra systems with GUIs - and we
will present several tests that should help with troubleshooting.





2. Low-level tests
==================
Before we start the theory please try these low-level tests.


2.1. Preparation
----------------
Have have to make sure that:

  1) \"eev-channel.el\" has been loaded,
  2) the \"$EEVDIR/eegchannel\" script exists and is executable,
  3) we have expect installed - eegchannel depends on it,
  4) the temporary directory \"$EEVTMPDIR/\" exists.

Here is an e-script for that:

  (add-to-list 'load-path (ee-expand \"$EEVDIR\"))
  (require 'eev-channel)

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
sudo apt-get install expect
echo     $EEVDIR
cd       $EEVDIR
pwd
wget -nc http://angg.twu.net/eev-current/eegchannel
chmod 755 eegchannel
ls -lAF  $EEVDIR/eegchannel
expect -v
$EEVDIR/eegchannel A echo ok
echo     $EEVTMPDIR
mkdir -p $EEVTMPDIR/

# (find-eev \"eegchannel\")




2.2. Test eegchannel
--------------------
In this test we create a window setting like this,

   ______________________
  |          |           |
  |          |   shell   |
  |  this    |___________|
  |  intro   |           |
  |          |  shell 2  |
  |__________|___________|

and then:

  1) we run \"eegchannel A python\" at the lower shell. This runs
     a python interpreter \"listening on channel A\";

  2) we use the top shell to send the lines \"print(2+3)\" and
     \"exit()\" \"through the channel A\". In low-level terms
     this means that for each line

       a) we save it into the file \"$EEVTMPDIR/eeg.A.str\",

       b) we send a signal SIGUSR1 to the process -
          \"eegchannel\" - whose PID is stored in the file
          \"$EEVTMPDIR/eeg.A.pid\".

       c) When the eegchannel listening on the channel A receives
          a SIGUSR1 it sends the line in \"$EEVTMPDIR/eeg.A.str\"
          to the process that it controls (that is \"python\") \"as
          if the user had typed it\".

Here is the demo. Run it with <F8>s:

 (find-3EE '(eepitch-shell) '(eepitch-shell2))
 (find-3ee '(eepitch-shell) '(eepitch-shell2))
 (eepitch-shell)
# Optional clean-up:
ls -lAF $EEVTMPDIR/eeg.A.*
rm -fv  $EEVTMPDIR/eeg.A.*

 (eepitch-shell2)
# Start a python interpreter \"listening on channel A\":
$EEVDIR/eegchannel A python3

 (eepitch-shell)
# Check that eeg.A.pid exists and is very recent:
date
ls -lAF $EEVTMPDIR/eeg.A.*

# Send lines to eegchannel:
echo 'print(2+3)' > $EEVTMPDIR/eeg.A.str
kill -USR1    $(cat $EEVTMPDIR/eeg.A.pid)
echo 'exit()'     > $EEVTMPDIR/eeg.A.str
kill -USR1    $(cat $EEVTMPDIR/eeg.A.pid)



2.3. Test `eechannel'
---------------------
TODO: write this.
See:

  (find-eev \"eev-channels.el\" \"eechannel\")


2.4. Test `eexterm'
-------------------
TODO: write this.
See:

  (find-eev \"eev-channels.el\" \"eexterm\")





3. The innards
==============
Let's start with a detailed low-level view of of what we have
just summarized as to \"save a command into a temporary file,
then send a signal to the external program etc etc\".

Warning: this document is still VERY INCOMPLETE, and some parts
of it are just notes, drafts, and links that may not work for
you... =(

  (find-eev \"eegchannel\")
  (find-eev \"anim/\")
  (find-eev \"anim/channels.anim\")
  http://angg.twu.net/eev-current/eegchannel.html





4. The protocol, in diagrams
============================
Here's a diagram that shows roughly what we have when X is
running both an Emacs and an xterm, each in a separate window.
Many details have been omitted - for examples, the real
communication happens through fifos and ptys, that are not shown
- but it's easy to build a complete diagram from this. The arrows
indicate flow of information.

             keyboard  mouse      display
                 |       |           ^
                 v       v           |
            +----------------------------+
            |                            |
            |             X              |
            |                            |
            +----------------------------+
   key/mouse | ^ display    key/mouse | ^ display
      events v | commands      events v | commands
         +---------+              +---------+
         |         |              |         |
         |  emacs  |              |  xterm  |
         |         |              |         |
         +---------+              +---------+
                            chars and | ^ chars and
                              signals v | signals
                                  +---------+
                                  |         |
                                  |   sh    |
                                  |         |
                                  +---------+

To make the external shell at the right able to receive commands
from Emacs we will insert a program between the \"xterm\" and the
\"sh\" boxes - an Expect script called eechannel, that will
usually be totally transparent, in the sense that it will let all
the chars and signals that it receives pass through it without
changes.

The trick is that \"eechannel\" will also be \"listening to
commands from the outside\", according to the following protocol:

  1) each instance of eechannel \"listens\" to a fixed, named
     \"channel\"; for simplicity, let's suppose that this name is
     \"A\".

  2) When an eechannel receives a signal SIGUSR1 it reads a
     string from the file \"eech.A.str\" and sends that to the
     shell, as if the user had typed that.

  3) To make it simpler to send the signal, when eechannel starts
     up it saves its pid into the file \"eech.A.pid\".

That protocol can be depicted as the four horizontal arrows
below; the temporal order is from top to bottom.

                                    +-------------+
				    |             |
				    |    xterm    |
				    |             |
				    +-------------+
   +-----------+                         |  ^
   |           | --> eeg.A.str           v  |
   |   emacs   | <-- eeg.A.pid      +-------------+
   |           | -----------------> |             |
   +-----------+     eeg.A.str ---> | eechannel A |
                                    |             |
				    +-------------+
                                         |  ^
				         v  |
				    +-------------+
				    |             |
				    |     sh      |
				    |             |
				    +-------------+

When Emacs wants to send a line, say, \"cd /tmp/\", to eechannel,
it does this:

  1) \"--> eeg.A.str     \"   writes \"cd /tmp/\" into eech.A.str,
  2) \"<-- eeg.A.pid     \"   reads eechannel's pid from eech.B.str,
  3) \"----------------->\"   sends a signal SIGUSR1 to that pid,
  4) \"    eeg.A.str --->\"   ...and when eechannel receives a SIGUSR1
                                 it reads the contents of eech.A.str and
                                 sends that to its spawned shell.

Actually there's something else - how these programs are started.
If we run just \"xterm\", it behaves in its default way, and runs
a shell. But if we run

  \"xterm -e eechannel A /bin/sh\"

then xterm runs \"eechannel A /bin/sh\" instead of just
\"/bin/sh\"; and \"eechannel A /bin/sh\" runs \"/bin/sh\".

Also, evaluating this

  (find-bgprocess \"xterm\")

from Emacs runs an xterm, which runs a shell; evaluating either
of these sexps,

  (find-bgprocess \"xterm -e eechannel A /bin/sh\")
  (find-bgprocess \"xterm -e eechannel A $SHELL\")

runs an xterm, which runs the \"eechannel\" script, which runs a
shell. The sexp

  (eexterm \"A\")

is a shorthand for the one using \"$SHELL\". Also, note that
\"eechannel A ...\" saves its pid into \"eech.A.pid\".

The diagram that shows what happens when we run `(eexterm \"A\")'
is this one, below - note that the four arrows of the sending
protocol are not shown.

                +------------+
                |            |
                |     X      |
                |            |
                +------------+
               /  ^       \\  ^
              v  /         v  \\
   +-----------+            +-------------+
   |           | initiates  |             |
   |   emacs   |:::::::::::>|    xterm    |
   |           |            |             |
   +-----------+            +-------------+
                              ::   |  ^
                              \\/   v  |
                            +-------------+
                            |             |
              eeg.A.pid <-- | eechannel A |
                            |             |
                            +-------------+
                              ::   |  ^
                              \\/   v  |
                            +-------------+
                            |             |
                            |     sh      |
                            |             |
                            +-------------+



5. Downloading and testing eechannel
====================================
Here I'll suppose that the directory \"~/bin/\" exists and is in
your PATH. Run this to download the \"eechannel\" script and make
it executable:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
cd ~/bin/
# See:  http://angg.twu.net/bin/eechannel.html
wget -n http://angg.twu.net/bin/eechannel
chmod 755 eechannel

Now let's test - from Emacs - if a local copy of \"eechannel\"
exists, is in your PATH, and can be executed (remember that it
needs Expect). Calling \"eechannel\" with not enough arguments
should yield a nice error message.

  (find-fline \"~/bin/eechannel\")
  (find-sh    \"~/bin/eechannel\")
  (find-sh          \"eechannel\")

It uses the environment variable EEVTMPDIR to decide where the
temporary files - ee.CHANNELAME.pid, ee.CHANNELNAME.str - will
be, so let's check that this directory exists:

       (getenv \"EEVTMPDIR\")
  (find-fline \"$EEVTMPDIR/\")

Running eechannel with a first argument \"-v\" should show a lot
of info, and then save the pid at eech.CHANNELAME.pid and run the
given command. Let's try with a command that exits immediately.

  (find-sh \"eechannel -v A /bin/true\")
  (find-fline \"$EEVTMPDIR/\" \"eech.A.pid\")

Now let's verify - using just eepitch, without xterm at this
moment - whether eechannel is being able to receive signals. In
the eepitch script below, two `(find-sh0 \"kill ...\")'s should
each make the controlled program receive a command - at the first
\"kill\" an \"echo 2\", at the second an \"echo 3\".

 (eepitch-comint \"A\" \"eechannel -v A /bin/sh\")
 (eepitch-kill)
 (eepitch-comint \"A\" \"eechannel -v A /bin/sh\")
echo 1

 (find-sh0 \"             cat $EEVTMPDIR/eech.A.pid\")
 (find-sh0 \"echo 'echo 2'  > $EEVTMPDIR/eech.A.str\")
 (find-sh0 \"             cat $EEVTMPDIR/eech.A.str\")
 (find-sh0 \"kill -USR1 $(cat $EEVTMPDIR/eech.A.pid)\")

 (find-sh0 \"echo 'echo 3'  > $EEVTMPDIR/eech.A.str\")
 (find-sh0 \"kill -USR1 $(cat $EEVTMPDIR/eech.A.pid)\")
echo 4

If the eepitched shell did run \"echo 1\", \"echo 2\", \"echo
3\", \"echo 4\", then things are working - \"echo 1\" and \"echo
4\" were sent through eepitch, but \"echo 3\" and \"echo 4\" were
sent though channels.

Now let's check if we can run an xterm, and if it can run an
eechannel-ed shell instead of a plain shell. Check if the second
sexp above starts an xterm, displays the info from \"eechannel
-v\", and then runs a shell:

  (find-bgprocess \"xterm\")
  (find-bgprocess \"xterm -e eechannel -v A $SHELL\")
  (find-bgprocess \"xterm -e eechannel    A $SHELL\")

if that worked, run the four sexps below with <f8>,

 (find-sh0 \"echo 'echo 1'  > $EEVTMPDIR/eech.A.str\")
 (find-sh0 \"kill -USR1 $(cat $EEVTMPDIR/eech.A.pid)\")
 (find-sh0 \"echo 'echo 2'  > $EEVTMPDIR/eech.A.str\")
 (find-sh0 \"kill -USR1 $(cat $EEVTMPDIR/eech.A.pid)\")

and check if the eechannel-ed shell in the xterm has received the
commands \"echo 1\" and \"echo 2\". If it did, try the
higher-level version below - but use <f9>s on each line instead
of the <f8>s:

 (eexterm \"A\")
echo 3
echo 4

If that worked, we're done. =)



6. Several xterms
=================
http://angg.twu.net/eev-current/anim/channels.anim.html

 (eexterm \"A\")
 (eexterm \"B\")
# listen on port 1234
netcat -l -p 1234

 (eexterm \"A\")
# Send things to port 1234 (on localhost)
{
  echo hi
  sleep 1
  echo bye
  sleep 1
} | netcat -q O localhost 1234






\[NOT DONE YET. The rest is (recyclable) garbage.\]






\(Old stuff:)

  emacs runs: (find-bgprocess \"xterm -T 'channel A' -e eegchannel A /bin/sh\")
  xterm runs:                                          eegchannel A /bin/sh
  eegchannel saves its pid at ~/.eev/eeg.A.pid and runs:            /bin/sh

At this point the xterm is running a shell that is \"listening on
channel A\"; eegchannel pretends to be transparent and passes all
characters and signals in the vertical arrows transparently - but when
it receives a certain signal (a SIGUSR1) it reads the contents from
the file ~/.eev/eeg.A.str and passes it to the shell, as if those
characters were coming from the xterm - i.e., as if the used had typed
them.

Here are the details of this \"protocol\":
when we type F9 on a line containing \"echo foo\",

  emacs saves the string \"echo foo\\n\" into ~/.eev/A.str,
  emacs reads the pid of eegchannel from ~/.eev/eeg.A.pid (say, \"1022\"),
  emacs runs \"kill -USR1 1022\",
  eegchannel reads the string from ~/.eev/A.str, and sends it to the shell.

NOTE: one frequent question is: \"why are you using two normal files
and SIGUSR1s for the communication between emacs and eegchannel,
instead of making them talk through a fifo?\" - the answer is: try to
explain the details of fifos - including creation - to someone who
knows little about the inner workings of a *NIX kernel and who is
uncomfortable to accept another kind of black box... The way with two
files and SIGUSR1s is simpler, works well enough, and I've been able
to write all the code for it in a few hours - and I still don't know
enough about fifos to implement a fifo version of this.

and another one, that was mostly used to control external
programs running in dedicated xterms. You can see an animation
demonstrating it - and an explanation of what each line does -
here:

  http://angg.twu.net/eev-current/anim/channels.anim.html
  http://angg.twu.net/eev-current/doc/shot-f9.png



How to set it up
================
\[To be written\]

                 ssh edrx@other.machine
  eegchannel A   ssh edrx@other.machine
  (find-prepared-intro \"\\n`ee'\\n\")
  (find-eev \"anim/channels.anim\")
" pos-spec-list)))

;; (find-channels-intro)





;;;        _     _
;;; __   _(_) __| | ___  ___  ___
;;; \ \ / / |/ _` |/ _ \/ _ \/ __|
;;;  \ V /| | (_| |  __/ (_) \__ \
;;;   \_/ |_|\__,_|\___|\___/|___/
;;;
;; «find-videos-intro» (to ".find-videos-intro")
;; Skel: (find-intro-links "videos")

(defun find-videos-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-videos-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-videos-intro)
Source code:  (find-efunction 'find-videos-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.


This intro needs to be rewritten!!!
Most of the things here were reimplemented
in a much better way in nov/2019. See:
  (find-video-links-intro \"2. From the HTML\")
  (find-video-links-intro \"9. First-class videos\")
  (find-eev \"eev-videolinks.el\" \"ee-1stclassvideos-info\")

Prerequisites:
  (find-psne-intro)
  (find-audiovideo-intro)
  (find-audiovideo-intro \"4. Short hyperlinks to audio and video files\")
  (find-audiovideo-intro \"7. `code-psnevideo'\")
  (find-audiovideo-intro \"7.1. `code-eevvideo'\")





1. Some videos
==============
NOTE: this list of first-class videos is obsolete!
You can get the current list by running one of these sexps:

  (find-1stclassvideos)
  (find-eev \"eev-videolinks.el\" \"ee-1stclassvideos-info\")

At this moment I have these eight videos about eev (I am
deliberately ignoring the ones that I consider obsolete!):

  1. \"How to record executable notes with eev - and how to play them back\":
     http://angg.twu.net/emacsconf2019.html
     http://angg.twu.net/emacsconf2019.html#code-video
     http://angg.twu.net/eev-videos/emacsconf2019.mp4
     http://www.youtube.com/watch?v=86yiRG8YJD0
     (find-eev2019video)

  2. \"On why most of the best features in eev look like 5-minute hacks\":
     http://angg.twu.net/emacsconf2020.html
     http://angg.twu.net/emacsconf2020.html#code-video
     http://angg.twu.net/eev-videos/emacsconf2020.mp4
     http://www.youtube.com/watch?v=hOAqBc42Gg8
     (find-eev2020video)

  3. \"How to install eev with M-x list-packages and how to navigate its tutorials\":
     http://angg.twu.net/2020-list-packages-eev-nav.html
     http://angg.twu.net/2020-list-packages-eev-nav.html#code-video
     http://angg.twu.net/eev-videos/2020-list-packages-eev-nav.mp4
     http://www.youtube.com/watch?v=kxBjiUo88_U
     (find-eevnavvideo)

  4. \"Some template-based functions of eev that are not five-minute hacks\":
     http://angg.twu.net/2020-some-template-based.html
     http://angg.twu.net/2020-some-template-based.html#code-video
     http://angg.twu.net/eev-videos/2020-some-template-based.mp4
     http://www.youtube.com/watch?v=91-9YfRPsuk
     (find-eevtemplvideo)

  5. \"How to create hyperlinks to \"here\" with `find-here-links'\":
     http://angg.twu.net/2020-find-here-links.html
     http://angg.twu.net/2020-find-here-links.html#code-video
     http://angg.twu.net/eev-videos/2020-find-here-links.mp4
     http://www.youtube.com/watch?v=8jtiBlaDor4
     (find-eevfherelvideo)

  6. \"Using test blocks in eev\":
     http://angg.twu.net/2021-test-blocks.html
     http://angg.twu.net/eev-videos/2021-test-blocks.mp4
     http://www.youtube.com/watch?v=fpsF_M55W4o
     (find-eevtestblsvideo)

  7. \"Short videos about workflows - and how to upload them\":
     http://angg.twu.net/2021-ssr.html
     http://angg.twu.net/eev-videos/2021-ssr.mp4
     http://www.youtube.com/watch?v=_0_NLXTVhBk

  8. \"How to use the `[Video links:]' blocks in the `intro's of eev\"
     http://angg.twu.net/2021-video-links.html
     http://angg.twu.net/eev-videos/2021-video-links.mp4
     http://www.youtube.com/watch?v=xQqWufQgzVY
     (find-eevvlinksvideo \"0:00\")


The ones that I prepared for the two EmacsConfs are very
well-rehearsed, the other ones are not.

The links with #code-video, like

  http://angg.twu.net/emacsconf2019.html#code-video

point to indexes of the videos made with sexp hyperlinks.

The best way to watch them is to download local copies of their
.mp4s and then use the short hyperlinks described in

  (find-audiovideo-intro \"4. Short hyperlinks to audio and video files\")

to jump to positions in them.




2. Short links to eev video tutorials
=====================================
The \"short links to eev video tutorials\" are made to be trivial
to use from the _htmlized_ versions of the intros; they are not
so trivial from Emacs. If you open the htmlized version of this
section in a browser - its URL is:

  http://angg.twu.net/eev-intros/find-videos-intro.html#2

you will notice that links like

  (find-eev2020video \"6:25\" \"`find-video'\")
   \\---------------/  \\--/
    function name:    time:
    points to here    points to
    (this section)    YouTube

have two hyperlinks: the function name, \"find-eev2020video\",
points to this section of this intro, and the timestamp,
\"6:25\", points to YouTube; in this example, the \"6:25\" points
to my presentation about eev in the EmacsConf2020, and it plays
that video starting from 6:25.

At this moment only these `find-eev*video' function are htmlized
in this way:

  1. \"How to record executable notes with eev - and how to play them back\"
     http://angg.twu.net/emacsconf2019.html
     (find-eev2019video \"0:00\")

  2. \"On why most of the best features in eev look like 5-minute hacks\"
     http://angg.twu.net/emacsconf2020.html
     (find-eev2020video \"0:00\")

  3. \"How to install eev with M-x list-packages and how to navigate its tutorials\"
     http://angg.twu.net/2020-list-packages-eev-nav.html
     (find-eevnavvideo \"0:00\")

  4. \"Some template-based functions of eev that are not five-minute hacks\"
     http://angg.twu.net/2020-some-template-based.html
     (find-eevtemplvideo \"0:00\")

  5. \"How to create hyperlinks to \"here\" with `find-here-links'\"
     http://angg.twu.net/2020-find-here-links.html
     (find-eevfherelvideo \"0:00\")

  6. \"Using test blocks in eev\":
     http://angg.twu.net/2021-test-blocks.html
     (find-eevtestblocksvideo \"0:00\")

  7. \"How to use the `[Video links:]' blocks in the `intro's of eev\"
     http://angg.twu.net/2021-video-links.html
     (find-eevvlinksvideo \"0:00\")

If you follow these `find-eev*video' sexp hyperlinks in Emacs you
will _usually_ get a temporary buffer with links to that video...
see the next section.

...or for an explanation in video, see:

  http://angg.twu.net/2021-video-links.html
  (find-eevvlinksvideo \"0:00\")
  http://www.youtube.com/watch?v=xQqWufQgzVY





3. Some `find-eevvideo-links'
=============================
When you run a sexp like this

  (find-eev2020video \"0:00\")

in Emacs it by default runs this,

  (find-eevvideo-links \"eev2020\" \"emacsconf2020\" \"hOAqBc42Gg8\" \"0:00\")

that creates a temporary buffer containing commands for doing
several things - like downloading a local copy of that video,
playing the local copy, and overriding the definition of
`find-eev2020video' with another one, that plays the local copy
straight away without creating a temporary buffer.

That temporary buffer is a bit hard to understand, and I need to
make a video explaining how to use each part of it (TODO!
Urgent)...

The \"short links to eev video tutorials\" listed above call
these `find-eevvideo-links' sexps:

  1. (find-eevvideo-links \"eev2019\" \"emacsconf2019\" \"86yiRG8YJD0\")
  2. (find-eevvideo-links \"eev2020\" \"emacsconf2020\" \"hOAqBc42Gg8\")
  3. (find-eevvideo-links \"eevnav\"  \"2020-list-packages-eev-nav\" \"kxBjiUo88_U\")
  4. (find-eevvideo-links \"eevtempl\" \"2020-some-template-based\" \"91-9YfRPsuk\")
  5. (find-eevvideo-links \"eevfherel\" \"2020-find-here-links\" \"8jtiBlaDor4\")
  6. (find-eevvideo-links \"eevtestblocks\" \"2021-test-blocks\" \"fpsF_M55W4o\")
  7. (find-eevvideo-links \"2021ssr\" \"2021-ssr\" \"_0_NLXTVhBk\")

They are htmlized in a nice way - see:

  http://angg.twu.net/eev-intros/find-videos-intro.html#3

The function `find-eevvideo-links' is explained here:

  (find-audiovideo-intro \"7.2. `find-eevvideo-links'\")

" pos-spec-list)))

;; (find-videos-intro)





;;;        _     _                  _ _       _
;;; __   _(_) __| | ___  ___       | (_)_ __ | | _____
;;; \ \ / / |/ _` |/ _ \/ _ \ _____| | | '_ \| |/ / __|
;;;  \ V /| | (_| |  __/ (_) |_____| | | | | |   <\__ \
;;;   \_/ |_|\__,_|\___|\___/      |_|_|_| |_|_|\_\___/
;;;
;; «find-video-links-intro»  (to ".find-video-links-intro")
;; Skel: (find-intro-links "video-links")
;; Test: (find-video-links-intro)

(defun find-video-links-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-video-links-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-video-links-intro)
Source code:  (find-efunction 'find-video-links-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.




1. Introduction
===============
Many of the tutorials of eev have \"[Video links:]\" blocks.
For example:

  (find-eev-quick-intro \"4. Creating Elisp Hyperlinks\")
  (find-eev-quick-intro \"4. Creating Elisp Hyperlinks\" \"[Video links:]\")
  (find-eev-quick-intro \"6. Controlling shell-like programs\")
  (find-eev-quick-intro \"6. Controlling shell-like programs\" \"[Video links:]\")

They contain links like these,

  (find-eevnavvideo \"10:36\" \"if I type <f8> six times here\")
  (find-eev2019video \"15:11\" \"Demo: the eepitch block (in red star lines)\")
  (find-eevtestblsvideo \"2:33\" \"if I run f8 here I start a new Lua interpreter\")

that are \"short links to eev video tutorials\". These functions
with names like `find-eev*video' are _sort of_ expanded to
functions that specify explicitly which video on youtube to play,
and where the local copy of the video can be found if we prefer
to play the local copy. For the general idea of how this
expansion is implemented, see:

  (find-eev-quick-intro \"9. Shorter hyperlinks\")
  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-eev-quick-intro \"9.1. `code-c-d'\" \"{c}\")

The strings \"eevnav\", \"eev2019\", and \"eevtestbls\" in the
names of the functions correspond to the argument \"c\" of
`code-c-d'.




2. From the HTML
================
The \"intros\" of eev can be read both from Emacs and from a
browser. For example, the HTMLized version of this intro is at:

  http://angg.twu.net/eev-intros/find-video-links-intro.html

In the HTMLized version each \"short link to a video tutorial\"
has two hyperlinks, like this:

  (find-eev2020video \"6:25\" \"`find-video'\")
   \\---------------/  \\--/
    function name:    time:
    points to here    points to
    (this section)    YouTube

the function name, \"find-eev2020video\", points to a place -
this section! - with help about how these links work, and the
timestamp, \"6:25\", points to YouTube; in this example, the
\"6:25\" points to my presentation about eev in the
EmacsConf2020, and it plays that video starting from 6:25. More
precisely, the \"6:25\" points to:

  https://www.youtube.com/watch?v=hOAqBc42Gg8#t=6m25s
                                  \\---------/   \\---/
                                    \"hash\"      time

We call the string \"hOAqBc42Gg8\" the \"hash\" of the video;
calling it the \"youtube id\" of the video would be more precise,
but longer. The \"6:25\" is converted to a \"#t=6m25s\", that
makes youtube start playing the video from the right position.




3. `find-youtube-video'
=======================
In some situations - see the section 7 - running

  (find-eev2020video \"6:25\" \"`find-video'\")

makes Emacs run this:

  (find-youtube-video \"hOAqBc42Gg8\" \"6:25\")

Note that running this sexp

  (ee-find-youtube-video \"hOAqBc42Gg8\" \"6:25\")

returns one of these sexps,

  (find-googlechrome \"http://www.youtube.com/watch?v=hOAqBc42Gg8#t=6m25s\")
  (find-firefox      \"http://www.youtube.com/watch?v=hOAqBc42Gg8#t=6m25s\")

according to the variable `ee-find-youtube-video-program'; you
can configure it to use one of the most common browsers with:

  (setq ee-find-youtube-video-program 'find-googlechrome)
  (setq ee-find-youtube-video-program 'find-firefox)




4. Configuring the browser
==========================
The variables `ee-firefox-program' and `ee-googlechrome-program'
determine the programs that `find-firefox' and
`find-googlechrome' should try to execute. You can test if they
are correct by trying:

  (find-firefox      \"http://www.lua.org/start.html\")
  (find-googlechrome \"http://www.lua.org/start.html\")

Typically on GNU/Linux systems the right values are:

  (setq ee-firefox-program      \"firefox\")
  (setq ee-googlechrome-program \"google-chrome\")

and on M$ Windows typically what works is something like:

  (setenv \"FIREFOXDIR\"      \"c:/Program Files/Mozilla Firefox\")
  (setenv \"GOOGLECHROMEDIR\" \"c:/Program Files/Google/Chrome/Application\")
  (setq ee-firefox-program      \"$FIREFOXDIR/firefox.exe\")
  (setq ee-googlechrome-program \"$GOOGLECHROMEDIR/chrome.exe\")

but people usually have to adjust the paths by hand. One way to
test if the paths are right is to open the directories in dired
and see if the files \"firefox.exe\" and \"chrome.exe\" are there
- i.e., run these sexps and see if they find the right files in
the right directories:

  (find-fline \"$FIREFOXDIR/\"      \"firefox.exe\")
  (find-fline \"$GOOGLECHROMEDIR/\" \"chrome.exe\")

If you've never heard of dired, then read this:

  (find-enode \"Dired\")




5. Local copies
===============
In some situations - see section 7 - a sexp like

  (find-eev2020video \"6:25\" \"`find-video'\")

will try to play a local copy of the video file. In the case of
`find-eev2020video' this means a local copy of this file,

  http://angg.twu.net/eev-videos/emacsconf2020.mp4

downloaded by `psne'-ing, as explained here:

  (find-psne-intro)

The local copy will be played with Mpv, with:

  (find-mpv-video \"$S/http/angg.twu.net/eev-videos/emacsconf2020.mp4\" \"6:25\")




5.1. Subtitles
--------------
Some of the videos in

  http://angg.twu.net/eev-videos/

have subtitles in separate files. If a video called
NAMEOFTHEVIDEO(.mp4) has subtitles in two formats, .srt and .srt,
then the _low-level way_ to download it from the eev home page
would be with this eepitch block:

 (eepitch-shell2)
 (eepitch-kill)
 (eepitch-shell2)
  mkdir -p $S/http/angg.twu.net/eev-videos/
  cd       $S/http/angg.twu.net/eev-videos/
  wget -nc  http://angg.twu.net/eev-videos/NAMEOFTHEVIDEO.mp4
  wget -N   http://angg.twu.net/eev-videos/NAMEOFTHEVIDEO.srt
  wget -N   http://angg.twu.net/eev-videos/NAMEOFTHEVIDEO.vtt

The eepitch block above follows all the conventions that are
explained here,

  (find-psne-intro \"1. Local copies of files from the internet\")

except for the arguments to wget. The help message for wget
explains \"-nc\" and \"-N\" as:

  (find-sh \"wget --help\" \"-nc, --no-clobber\")
  (find-sh \"wget --help\" \"-N,  --timestamping\")

  -nc, --no-clobber        skip downloads that would download to
                             existing files (overwriting them)
  -N,  --timestamping      don't re-retrieve files unless newer than
                             local

The \".mp4\"s at angg.twu.net are never updated, so if we already
have a local copy of the .mp4 then wget shouldn't try to download
it again. In contrast, subtitle files are revised occasionally,
so if the subtitle files on angg.twu.net are newer than the local
copy then wget should download the newer version, and overwrite
the local subtitle files with their newer versions.

The _medium-level way_ to download videos from angg.twu.net, with
or without subtitles, is by running sexps like these ones:

  (find-psne-eevvideo-links \"NAMEOFTHEVIDEO\" \"\")
  (find-psne-eevvideo-links \"NAMEOFTHEVIDEO\" \".srt\")
  (find-psne-eevvideo-links \"NAMEOFTHEVIDEO\" \".srt .vtt\")

Try them - they create temporary buffers with scripts like the
one in the beginning of this section, but with different commands
for downloading subtitles: the sexp with \".srt .vtt\" creates a
script that downloads two subtitle files, the one with \".srt\"
creates a script that downloads just one, and the sexp with \"\"
creates a script that doesn't try to download any subtitle files.

The _high-level way_ to play a first-class video is with sexps
like this one - see section 9 for details:

  (find-eev2021video)

The sexp above calls this one,

  (find-1stclassvideo-video \"eev2021\")

that will play the local copy of the video if it exists, or
download a local copy if we don't have a local copy of it yet.
The entry about the video \"eev2021\" in the list of first-class
videos is here:

  (find-eev \"eev-videolinks.el\" \"eev2021\")

Try these sexps:

  (ee-1stclassvideos-mp4stem  \"eev2021\")
  (ee-1stclassvideos-localmp4 \"eev2021\")
  (ee-1stclassvideos-mp4found \"eev2021\")
  (ee-1stclassvideos-field    \"eev2021\"       :subs)
  (ee-1stclassvideos-field    \"2021workshop5\" :subs)

You will see that the video \"eev2021\" has subtitles, and the
video \"2021workshop5\" doesn't.

NOTE: the support for subtitles is recent, and is still a bit
rough. The temporary buffer generated by
`find-1stclassvideo-links' contains a few extra lines when a
video has subtitles - compare:

  (find-1stclassvideo-links \"eev2021\")
  (find-1stclassvideo-links \"2021workshop5\")

but the process of downloading or updating subtitles is not well
integrated in the rest of eev yet, and at this moment the
\"right\" way to download or update subtitles is with
`find-1stclassvideo-links' and with the link in its \"Download
subtitles\" section.





6. Configuring Mpv
==================
After installing Mpv you may have to configure its path. On
GNU/Linux this typically works,

  (setq ee-mpv-program \"mpv\")

and on M$ Windows you will need something like this, but you will
have to adjust the path:

  (setenv \"MPVDIR\" \"c:/Users/myusername/path/to/mpv\")
  (setq ee-mpv-program \"$MPVDIR/mpv.exe\")

You can test if the path is right with the two sexps below. Note
that the first is for M$ Windows only, and that the second one
will display the basic command-line options of mpv.

  (find-fline \"$MPVDIR/\" \"mpv.exe\")
  (find-callprocess `(,ee-mpv-program \"--help\"))




7. `find-eev-video'
===================
All the standard functions for short links to video tutorials are
implemented using a function called `find-eev-video'. For
example, `find-eev2020video' is defined as:

  ;; For the real definition, see:
  ;; (find-eev \"eev-audiovideo.el\" \"video-tutorials\")
  ;; (find-eev \"eev-audiovideo.el\" \"video-tutorials\" \"eev2020\")
  ;;
  (defun find-eev2020video (&optional time &rest rest)
    \"[Long docstring omitted]\"
    (interactive)
    (find-eev-video \"emacsconf2020\" \"hOAqBc42Gg8\" time))

Calling:

  (find-eev2020video \"6:25\" \"`find-video'\")

runs:

  (find-eev-video \"emacsconf2020\" \"hOAqBc42Gg8\" \"6:25\")

that runs one of these sexps, depending on the current settings:

  (find-youtube-video \"hOAqBc42Gg8\" \"6:25\")
  (find-eevlocal-video \"emacsconf2020\" \"hOAqBc42Gg8\" \"6:25\")
  (find-eevlinks-video \"emacsconf2020\" \"hOAqBc42Gg8\" \"6:25\")

The one with `find-eevlocal-video' plays the local copy of

  http://angg.twu.net/eev-videos/emacsconf2020.mp4

if it has already been downloaded, and if the local copy is not
found it displays a temporary buffer with links and an e-script
for downloading - i.e., psne-ing - the video from the URL above.
The sexp with `find-eevlinks-video' works very similarly to the
one with `find-eevlocal-video', but it always displays the
temporary buffer with links and an e-script.

You can select the behavior of `find-eev-video' - and thus the
behavior of all short links to video tutorials, as they all call
`find-eev-video' - by running one of the `setq's below:

  (setq ee-find-eev-video-function 'find-eevyoutube-video)
  (setq ee-find-eev-video-function 'find-eevlocal-video)
  (setq ee-find-eev-video-function 'find-eevlinks-video)

`find-eevyoutube-video' is like `find-eev-video', but it discards
its first argument.

The default is `find-eevlocal-video', but for Windows users
starting with `find-eevyoutube-video' makes more sense. The
user-friendly way to select one of these behaviors is with these
sexps:

  (ee-use-local-videos)
  (ee-use-youtube-videos)

You can also run them with `M-x ee-use-local-videos' and `M-x
ee-use-youtube-videos'.




8. Windows
==========
This is my n-th different implementation of the innards of the
short links to video tutorials. This one - from nov/2021 - was
inspired by feedback of the Windows users that participated in
this workshop:

  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00037.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00045.html
  http://angg.twu.net/2021-oficina.html (<- in Portuguese)

My original implementation - from may/2021 - was the one
described here:

  http://angg.twu.net/2021-video-links.html

I'm trying to making the short links to video tutorials work from
Emacs _in a way that is convenient for both long-time users and
total beginners_. This is quite a challenge - especially because
since oct/2021 my notion of \"total beginners\" includes \"people
who use Windows and who have never used terminals in their
lives\". If you are one of those \"total beginners\" my
recommendation is: start by this tutorial - it will force you to
learn how to configure paths and how to test if Emacs knows how
to call a given external program - and then follow this one:

  (find-windows-beginner-intro)




9. First-class videos
=====================
I store videos of several kinds in:

  http://angg.twu.net/eev-videos/

The presentations and tutorials on eev are the \"first-class
citizens\" of that directory - mainly in the sense that a lot of
information about them is available and easy to find. Let's look
at one example in details. Try:

  (find-efunctiondescr 'find-eev2019video)
  (find-efunction      'find-eev2019video)
  (find-efunctionpp    'find-eev2019video)
  (find-1stclassvideo-links \"eev2019\")

All these sexps \"work\" - they point to meaningful places.

The data about these first-class videos is kept in the variable
`ee-1stclassvideos-info', that is defined here:

  (find-eev \"eev-videolinks.el\" \"ee-1stclassvideos-info\")

The most user-friendly way to access this data for a particular
video is with a sexp with `find-1stclassvideo-links', like this
one:

  (find-1stclassvideo-links \"2022findeevangg\")

and the best and most user-friendly way to access the list of
videos is by running `M-x find-1stclassvideos', or:

  (find-1stclassvideos)

That function displays a temporary buffer with a list of all
first-class videos, with a `find-1stclassvideo-links' sexp for
each one. Here is a screenshot:

  http://angg.twu.net/IMAGES/2022find-1stclassvideos.png





10. Second-class videos
=======================
I will refer to the videos in

  http://angg.twu.net/eev-videos/

that are not \"first-class citizens\" as - ta-da! - \"second
class citizens\". When I want to show something in one of those
videos to a person who uses eev I send her sexps like these:

  (code-eevvideo \"eevhydras\" \"2021-05-20_hydra_ei\")
  (find-eevhydrasvideo \"0:00\")
  (find-eevhydrasvideo \"0:50\")

If you run the sexps above and then these ones,

  (find-efunctiondescr 'find-eevhydrasvideo)
  (find-efunction      'find-eevhydrasvideo)
  (find-efunctionpp    'find-eevhydrasvideo)
  (find-1stclassvideo-links \"eevhydras\")

you will see `find-eevhydrasvideo' is \"not documented\", that
Emacs \"Do(es)n't know where `find-eevhydrasvideo' is defined\",
that the pretty-printed version of `find-eevhydrasvideo' doesn't
have a docstring after its argument list, and that many of the
links in the temporary buffer created by
`find-1stclassvideo-links' don't work.

For more information on `code-eevvideo' see the comments in its
source code, here:

  (find-eev \"eev-videolinks.el\" \"second-class-videos\")
  (find-eev \"eev-videolinks.el\" \"code-eevvideo\")




11. Hardcoded paths
===================
Practically all the functions defined above have `eev' in their
names, and they all convert the \"{stem}\" of a video to a URL
like this:

  http://angg.twu.net/eev-videos/{stem}.mp4

The conversion from \"{stem}\" to
\"http://angg.twu.net/eev-videos/{stem}.mp4\" is hardcoded in
these functions, and AT THIS MOMENT there isn't an easy way to
implement other similar conversions - pointing to other
repositories of videos - without changing a lot of code by hand.
This is mainly because I don't know anyone else who is putting
their videos on places from which they are easy to wget. If you
know something like this, please get in touch!

" pos-spec-list)))

;; (find-video-links-intro)






;;;      _       __
;;;   __| | ___ / _|_   _ _ __
;;;  / _` |/ _ \ |_| | | | '_ \
;;; | (_| |  __/  _| |_| | | | |
;;;  \__,_|\___|_|  \__,_|_| |_|
;;;
;; «find-defun-intro»  (to ".find-defun-intro")
;; Skel: (find-intro-links "defun")

(defun find-defun-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-defun-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-defun-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-defun-intro\")
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.


Note: this intro needs to be integrated with:
  (find-elisp-intro)
I wrote it for a mini-tutorial on Lisp that I gave.
It complements this:
  (find-eev-quick-intro \"2. Evaluating Lisp\")
  (find-eval-intro \"3. What to execute, and in what order\")
  (find-eval-intro \"3. What to execute, and in what order\" \"defun\")




Simple examples
===============

  (* 5 5)
  (* 6 6)
  (defun foo (a) (* a a))
  (foo 5)
  (foo 6)

  (+ 5 5)
  (defun foo (a) (+ a a))
  (foo 5)

  (symbol-function 'foo)
  ((lambda (a) (* a a)) 5)
  ((lambda (a) (+ a a)) 5)

See:
  (find-elnode \"Function Cells\")
  (find-elnode \"Defining Functions\")



Several arguments
=================

  (defun foo (a b) (+ (* a a) (* b b)))
  (foo 10 2)
  (foo 5)
  (defun foo () (+ (* 2 3) (* 4 5)))
  (foo 10 2)
  (foo 5)
  (foo)



progn and prog1
===============
The body of a \"defun\" works like a \"progn\".
See: (find-elnode \"Index\" \"* progn:\")

  (defun foo (a b)         (* a b))
  (defun foo (a b) (+ a b) (* a b))
  (defun foo (a b) (* a b) (+ a b))
  (defun foo (a b)         (+ a b))
  (foo 5 6)

  (progn   (* 5 6) (+ 5 6))
  (progn \"ignored\" (+ 5 6) \"result\")
  (progn)

  (prog1   (* 5 6) (+ 5 6))
  (prog1 \"result\" (+ 5 6) \"ignored\")
  (prog1)



Docstrings
==========
Note that a string in a (progn ...) does nothing - unless it is
the last BODY-FORM.

But see: (find-elnode \"Documentation\")

Try:

  (defun foo (a b) \"IGNORED\" (+ a b))
  (defun foo (a b) \"This is the description of `foo'\" (+ a b))
  (defun foo (a b) \"This is the docstring of `foo'\" (+ a b))
  (defun foo (a b) \"This function returns (* A B). Note the italics!\" (+ a b))
  (find-efunctiondescr 'foo)



&optional and &rest
===================
See: (find-elnode \"Argument List\")
Try:

  (defun foo (a &optional b c) (list \"a,b,c:\" a b c))
  (foo 11 22 33)
  (foo 11 22)
  (foo 11)
  (foo)
  (foo 11 22 33 44)

  (defun foo (a &optional b c &rest r) (list \"a,b,c,r:\" a b c r))
  (foo 11 22 33 44 55 66)
  (foo 11 22 33 44 55)
  (foo 11 22 33 44)
  (foo 11 22 33)
  (foo 11 22)
  (foo 11)
  (foo)

  (defun foo (a &rest r) (list \"a,r:\" a r))
  (foo 11 22 33 44)
  (foo 11 22 33)
  (foo 11 22)
  (foo 11)
  (foo)



A tool: my-insert
=================
See: (find-elnode \"Formatting Strings\")
Try:

  (format \"<%s>\" \"hello\")
  (format \"<%s>\" \"123\")
  (format \"<%s>\" 123)
  (format \"<%s>\" 'hello)
  (format \"<%s>\" '(+ 2 3))

  (format \"<%S>\" \"hello\")
  (format \"<%S>\" \"123\")
  (format \"<%S>\" 123)
  (format \"<%S>\" 'hello)
  (format \"<%S>\" '(+ 2 3))

Now define:

  (defun my-insert (obj)
    \"Print (insert) OBJ in the current buffer.\"
    (insert (format \"\\n  ;; \\\\--> %S\" obj)))

Try:

  (my-insert 123)
  (my-insert \"123\")
  (my-insert \"hello\")
  (my-insert 'hello)
  (my-insert '(+ 2 3))
  (my-insert nil)
  (my-insert '())
  (my-insert ())

See also:
  (find-elnode \"Character Type\")
  (find-elnode \"Basic Char Syntax\")
  (find-elnode \"Basic Char Syntax\" \"?\\\\n\")
  (find-elnode \"General Escape Syntax\")



interactive
===========
Not all Emacs functions are callable with `M-x' - only those that
are \"commands\" are callable in this way. And only \"commands\"
can be bound to keys...

See:
  (find-elnode \"Defining Functions\" \"the first two of the BODY-FORMS\")
  (find-elnode \"Defining Commands\")
  (find-elnode \"Command Overview\")

When you execute an `(interactive ...)' it does nothing - it
simply ignores its arguments (which aren't even evaluated!) and
returns nil. But just as


  (defun my-insert (obj) (insert (format \"\\n  ;; \\\\--> %S\" obj)))

  (find-efunctiondescr 'interactive)

  (eek \"M-x foo\")
  (commandp 'foo)

  (defun foo (&rest rest) (interactive) (bar rest))
  (eek \"M-x foo\")

  (defun foo (&rest rest)                   (bar rest))
  (defun foo (&rest rest) (interactive \"P\") (bar rest))
  (eek \"            M-x foo\")
  (eek \"M-1         M-x foo\")
  (eek \"M-1 M-2 M-3 M-x foo\")
  (eek \"M-- M-2 M-3 M-x foo\")

  (defun foo (&rest rest) (interactive \"R\") (bar rest))
  (eek \"M-x foo\")


" rest)))

;; (find-defun-intro)
;; (find-defun-intro "&rest")
;; (find-defun-intro "\ninteractive\n")
;; (find-defun-intro "Defining Commands")




;;;                                      _       _
;;;   ___ _ __ ___   __ _  ___ ___      (_)_ __ | |_ _ __ ___
;;;  / _ \ '_ ` _ \ / _` |/ __/ __|_____| | '_ \| __| '__/ _ \
;;; |  __/ | | | | | (_| | (__\__ \_____| | | | | |_| | | (_) |
;;;  \___|_| |_| |_|\__,_|\___|___/     |_|_| |_|\__|_|  \___/
;;;
;; «find-emacs-intro» (to ".find-emacs-intro")
;; Skel: (find-intro-links "emacs")

(defun find-emacs-intro (&rest rest) (interactive)
  (let ((ee-buffer-name "*(find-emacs-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-emacs-intro)
Source code:  (find-eev \"eev-intro.el\" \"find-emacs-intro\")
More intros:  (find-eev-quick-intro)
              (find-emacs-keys-intro)
              (find-eev-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



THIS INTRO IS OBSOLETE, and has been superseded by:
  (find-emacs-keys-intro)



Basic keys (eev)
================
The most basic keys of eev are:
  M-e   - to follow a hyperlink, see: (find-eval-intro \"Elisp hyperlinks\")
  M-k   - to go back,            see: (find-eval-intro \"\\nGoing back\")
  M-j   - to jump to certain predefined places - in particular,
         `M-j' takes you to the list of jump targets.
        `M-2j' takes you to this help page.
        `M-5j' takes you to: (find-eev-intro)
         See: (find-eev-quick-intro \"7.1. `eejump'\")
              (find-eejump-intro \"Families\")
  M-h M-h - hyperlinks to here, plus help.
         See: (find-links-intro \"`find-here-links'\")


The mnemonics are:
  M-e   - evaluate/execute
  M-j   - jump
  M-k   - kill buffer

It is possible to start learning Emacs and eev by remembering
only that `M-j' jumps to an index; when you're lost, type `M-j',
and will see a reminder of the main keys and of the main jump
targets - after that you can learn how to use `M-k' to kill the
top buffer, and `M-e' to follow elisp hyperlinks. The other keys
are explained in \"intros\" like this one.




Files, Buffers, Windows, Frames, Display, etc
=============================================
Emacs can edit several files at the same time, each one
in a \"buffer\".

  (find-enode \"Files\")
  (find-enode \"Buffers\")
  (find-enode \"Windows\")
  (find-enode \"Frames\")

The display of Emacs looks like this (asciified):
                                        __ _ _
              ______________emacs_______\\/|-|X|
          /  |                                 |  \\
          |  | bla.                            |  | Emacs
Window    |  |                                 |  | calls this
managers  |  |                                 |  | a \"window\".
call      |  |                                 |  /
this a    |  |--:** foo.txt  (Fundamental) ----|  <-- Its \"modeline\".
\"window\". /  |                                 |  \\
Emacs     \\  | bla bla.                        |  | Another
calls     |  | bleh                            |  | window.
this a    |  |                                 |  |
\"frame\".  |  |                                 |  /
          |  |--:** bar.txt  (Fundamental) ----|  <-- Its modeline.
          \\  |Find file: ~/bletch.txt_ ________|  <-- The minibuffer.

The bottom line of a frame is sometimes the \"echo area\",
sometimes the \"minibuffer\". The minibuffer acts like a
window when it is active, and `C-x o' can be used to move
from it to the \"normal windows\" and back. You can also
use the mouse to move between windows.

  (find-enode \"Echo Area\")
  (find-enode \"Minibuffer\")
  (find-enode \"Other Window\")

By default there's also a \"menu bar\" (with textual entries) and
a \"tool bar\" (with icons) at the top of each frame, but
advanced users usually disable them.

  (find-enode \"Menu Bar\")
  (find-enode \"Tool Bars\")




Basic keys (Emacs)
==================
\(find-enode \"Keys\" \"key sequence\")
\(find-enode \"User Input\" \"`Control-a'\" \"usually written `C-a'\")
\(find-enode \"User Input\" \"<META> key\")
\(find-enode \"Completion\" \"<TAB>\")

<ESC> <ESC> <ESC>               (find-enode \"Quitting\")
C-g   keyboard-quit             (find-enode \"Quitting\" \"`C-g'\")

M-x   execute-extended-command  (find-enode \"M-x\" \"Running Commands by Name\")



Cutting & pasting
=================
The \"region\" where cut & copy operate is always what is between
the \"point\" and the \"mark\":

  (find-enode \"Point\")
  (find-enode \"Mark\")

You can do cut, copy and paste by using the icons in the toolbar
or by using the menu bar (the relevant options are under
\"Edit\"), but the keys are worth learning:

  C-SPC   -- set-mark-command           (find-enode \"Setting Mark\")
  C-x C-x -- exchange-point-and-mark    (find-enode \"Setting Mark\" \"C-x C-x\")
  C-w     -- kill-region     (cut)      (find-enode \"Other Kill Commands\")
  M-w     -- kill-ring-save  (copy)     (find-enode \"Kill Ring\")
  C-y     -- yank            (paste)    (find-enode \"Kill Ring\")



Undoing
=======
C-/    -- undo    (find-enode \"Basic Undo\")
C-_    -- undo    (find-enode \"Basic Undo\")
                  (find-enode \"Undo\")


Windows
=======
See: (find-enode \"Frames\")
     (find-enode \"Windows\")
C-x o   -- other-window                      (find-enode \"Other Window\")
C-x 0   -- delete-window                     (find-enode \"Change Window\")
C-x 1   -- delete-other-windows (\"1 window\") (find-enode \"Change Window\")
C-x 2   -- split-window-vertically (Abv/Blw) (find-enode \"Split Window\")
C-x 3   -- split-window-horizontally   (L|R) (find-enode \"Split Window\")



Other keys / reference
======================
M-x     -- execute-extended-command     (find-enode \"M-x\")
            more about the minibuffer:  (find-enode \"Minibuffer\")
TAB     -- for completion:              (find-enode \"Completion\")
           for indentation:             (find-enode \"Indentation\")
           in programming modes:        (find-enode \"Basic Indent\")

                                        (find-enode \"Dired\")
C-x C-f -- find-file                    (find-enode \"Visiting\")
C-x C-s -- save-buffer                  (find-enode \"Saving\")
C-x C-c -- save-buffers-kill-emacs      (find-enode \"Saving\")
C-x b   -- switch-to-buffer             (find-enode \"Select Buffer\")
C-x k   -- kill-buffer                  (find-enode \"Kill Buffer\")

C-a     -- beginning-of-line            (find-enode \"Moving Point\")
C-e     -- end-of-line                  (find-enode \"Moving Point\")
M-<     -- beginning-of-buffer          (find-enode \"Moving Point\")
M->     -- end-of-buffer                (find-enode \"Moving Point\")

M-q     -- fill-paragraph               (find-enode \"Fill Commands\")

C-s     -- isearch-forward              (find-enode \"Incremental Search\")
C-r     -- isearch-backward             (find-enode \"Incremental Search\")
M-C-s   -- isearch-forward-regexp       (find-enode \"Regexp Search\")
M-C-r   -- isearch-backward-regexp      (find-enode \"Regexp Search\")
M-%     -- query-replace                (find-enode \"Replace\")

C-x (   -- start-kbd-macro              (find-enode \"Keyboard Macros\")
C-x )   -- end-kbd-macro                (find-enode \"Keyboard Macros\")
C-x e   -- call-last-kbd-macro          (find-enode \"Keyboard Macros\")
" rest)))

;; (find-emacs-intro)
;; (find-TH "emacs" "short-emacs-tutorial")






;; «find-org-intro» (to ".find-org-intro")
;; Skel: (find-intro-links "org")

(defun find-org-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-org-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-org-intro)
Source code:  (find-efunction 'find-org-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



Eev does some things similar to Org, but using a different
approach and different design principles. This sandboxed tutorial
is a _first attempt_ to show to Org users how to use Org and eev
at the same time, in the same files (or buffers).

Note: I wrote this after giving a presentation about eev in the
EmacsConf 2019 and getting some help from Org users there (mainly
Amin Bandali). Link:

  http://angg.twu.net/emacsconf2019.html




1. Preparation
==============
Run these sexps:

  (code-c-d \"org\" (ee-locate-library \"org.el\") \"org\" :gz)
  (require 'org)
  (require 'ob-sh)
  ;; or: (require 'ob-shell)
  (require 'ob-python)



2. Toggling org-mode on and off
===============================
Use these sexps,

  (org-mode)
  (fundamental-mode)

or `M-x org-mode' and `M-x fundamental-mode'.



3. Comment blocks
=================
If you are using an org file that is meant for exporting you can
mark as comments the more eev-ish parts in it, like this...

# (find-orgnode \"Comment lines\")
# (find-orgnode \"Exporting\")

#+BEGIN_COMMENT
# Run the eepitch block below to download a copy of my messy
# notes on org. See:
# (find-eev-quick-intro \"6. Controlling shell-like programs\")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
cd /tmp/
rm -fv org.e
wget http://angg.twu.net/e/org.e

#+END_COMMENT




4. Running code from my org.e
=============================
The code in comments in the previous section downloads a local
copy of my executable notes (i.e., my \"e-scripts\") on Org. Run
it, and compare:

# http://angg.twu.net/e/org.e.html#git
# (find-anchor \"/tmp/org.e\" \"git\")

The URL above points to my notes on downloading Org from git and
compiling its docs. The sexp hyperlinks below it lets you execute
these notes.




5. Evaluating source blocks
===========================
You can execute a source block in Org and display its results
with `C-c C-c'. See:

# (find-orgnode \"Working with source code\")
# (find-orgnode \"Evaluating code blocks\")
# (find-orgnode \"Evaluating code blocks\" \":results output\")
# (find-orgnode \"Results of evaluation\" \":results output\")
# (find-orgnode \"results\" \"output\")

Try it here:

#+BEGIN_SRC sh :results output
seq 200 204
#+END_SRC

Compare that with:

# (find-sh \"seq 200 204\")

and compare

#+BEGIN_SRC python :results output
def square (x):
    return x*x

print(square(5))
#+END_SRC

with:

#+BEGIN_COMMENT
 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
def square (x):
    return x*x

print(square(5))

#+END_COMMENT




5. Sectioning
=============
Not yet!
How do I mark a section as \"don't export this\"?

  (find-orgnode \"Headlines\")
  (find-orgnode \"Global and local cycling\")
  (find-efunctiondescr 'org-mode \"TAB\" \"org-cycle\")
  (find-efunctiondescr 'org-shifttab)



" pos-spec-list)))

;; (find-org-intro)





;;;                           _       _
;;;   ___       ___  ___ _ __(_)_ __ | |_ ___
;;;  / _ \_____/ __|/ __| '__| | '_ \| __/ __|
;;; |  __/_____\__ \ (__| |  | | |_) | |_\__ \
;;;  \___|     |___/\___|_|  |_| .__/ \__|___/
;;;                            |_|

;; «find-escripts-intro» (to ".find-escripts-intro")
;; Skel: (find-intro-links "escripts")

(defun find-escripts-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-escripts-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-escripts-intro)
Source code:  (find-efunction 'find-escripts-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.
The quickest way to open or recreate this is with `M-6 M-j'.



This intro will be merged with
  (find-here-links-intro)
at some point...



Eev's central idea is that you can keep \"executable logs\" of
what you do, in a format that is reasonably readable and that is
easy to \"play back\" later, step by step and in any order. We
call these executable logs \"e-scripts\".

We will start this intro by explaining how eev and e-scripts
appeared. Then we will see how to read and play back a sample
e-script, and understand its conventions; then we will see some
tools to help writing e-scripts in several usual formats:

  1) a file with hyperlinks and eepitch blocks,
  2) a file with e-script blocks and an index,
  3) several files with e-script blocks and indexes,
  4) source files with eepitch blocks in multi-line comments,
  5) temporary buffers, like the ones generated by
     `find-latex-links' and `find-lua-links'.




1. Prehistory
=============
Eev appeared by accident. I started using Emacs in 1994 or 1995.
I knew a bit of Lisp from a course in the university, and after
just a few hours using Emacs I started putting sexps like this in
my notes,

  (find-file \"/usr/share/emacs/19.24/lisp/\")
  (find-file \"/usr/share/emacs/19.24/lisp/files.el\")

to go quickly to files or directories that I found interesting. A
few days after that I wrote a function - that I could invoke as
`M-x eev' - that saved the text of the region in the file
\"~/ee.sh\", and I had an alias `ee' in my shell that would
execute the contents of that file in verbose mode, i.e., showing
each line before executing it. \"Eev\" meant
\"Emacs-execute-verbosely\", but `M-x eev' only saved a block of
commands into \"~/ee.sh\"; to execute them I had to switch to a
terminal and type \"ee\". If the function `eev' was called with a
string argument instead of being called interactively it would
write that string to \"~/ee.sh\"; a sexp like

  (eev \"man tar\")

was a very primitive hyperlink to the manpage for \"tar\". I
wrote extensions to these ideas gradually, and for YEARS I was
absolutely sure that Emacs was meant to be used exactly in that
way, and that EVERYBODY used elisp code as hyperlinks. At some
point in 1999 I sent a message to a mailing list about Emacs, and
I casually apologized for using my own functions - with weird
names - for elisp hyperlinks and for saving code to be sent to a
shell, and asked where in the elisp source files I could find the
standard function that did that.

RMS himself answered.

He said that no one else was using Emacs in that way; that that
was very interesting, and that someone should clean up and
document my code so that it could be included in Emacs.

Eev is not yet an official part of Emacs (long story!) and
eepitch practically replaced `M-x eev' as a way to execute shell
commands. For more on `M-x eev', see: (find-prepared-intro)





2. E-scripts
============
The best short definition for eev that I've found involves some
cheating, as it is a circular definition: \"eev is a library that
adds support for e-scripts to Emacs\" - and e-scripts are files
that contain chunks meant to be processed by eev's functions.
Almost any file can contain parts \"meant for eev\": for example,
a HOWTO or README file about some program will usually contain
some shell commands, and we can use `M-x eev' or eepitch to send
these commands to a shell; most of my own files contain lots of
elisp hyperlinks, and some of them even contain eepitch blocks
inside multi-line comments - for example, this Lua library:

  http://angg.twu.net/LATEX/dednat6/eoo.lua.html#Vector

Some of my files are \"pure e-scripts\": they are mostly made of
\"e-script blocks\" like the ones described here:

  (find-eev-quick-intro \"8.4. Creating e-script blocks\")

Here are two examples structured like this:

  http://angg.twu.net/e/emacs.e.html
  http://angg.twu.net/e/lua5.e.html

Each of these \"e-script blocks\" is an \"executable log\" of
something that I was trying to understand, or trying to do.




3. Sharing
==========
One of my first public texts about eev was the \"Eev Manifesto\":

  http://angg.twu.net/eev-manifesto.html

Here are its main parts.

  Everybody is fluent in only a small fraction of all Unix
  commands. If you could \"listen\" to how the Unix gurus
  \"speak\" to their machines you would learn which \"words\" are
  related to solving a particular task, and learn how they fit in
  \"sentences\". By checking the \"dictionary entries\" for
  them (i.e., manpages, info pages, READMEs, source code, etc)
  you could learn the real meaning of them. But then you'd be
  learning Unix by immersion, from real use, instead of having to
  rely only on \"textbooks\", \"dictionaries\" and sometimes
  \"Rosetta stones\", \"graffitis on toilet walls\" and \"old
  newspapers\".

  The fact is that you can make a record of how you \"speak\"
  Unix, and more, you can become a lot more productive if you do
  so. Many tasks consist on short fixed sequences of commands:
  connecting to your ISP via modem, unpacking a source package
  and recompiling it, printing a text file in two-column mode,
  and so on. The trick is that with some functions defined in
  eev.el you can write these sequences of commands in a plain
  text file, then mark a block of this file with your
  editor (which must be Emacs for this to work), then tell a
  shell to execute only the commands in that block; in this way
  you can easily execute only portions of what would otherwise
  have to be a monolythic script; this is great for when you're
  not sure if everything works, or if you just want to do some
  steps. Also, it would be easy to change bits of the \"script\"
  before execution, as you'll be doing things from inside an
  editor.

  (...)

  I have placed essentially all my \"scripts\" written in this
  way (I call them \"e-scripts\") in a public place. They contain
  almost everything I know about Unix.

  If you like this idea, please get in touch, send comments, ask
  questions -- about e-scripts or questions whose answer could
  become an e-script chunk -- or send me your e-scripts when you
  have some, or even ask for help on setting up your own e-script
  collection or e-script public site... anything! By asking good
  questions you can help me make the documentation get better.

  I really want to make this e-scripts idea spread. Learning Unix
  -- or simply more Unix -- could be made easier for everybody...
  please help! E-scripts are more fun to use, and easier to
  write, than texts that tell everything in terms of \"press
  this, do that\". A lot of effort and money are being invested
  now on these kinds of text, and they're often very depressing.
  Let's try to save the world from them, at least a bit, and
  maybe this money will be directed to better things. And
  teaching people Unix tricks will be both easier and more fun.

The Manifesto said that

  1) *NIX can be compared to an oral language,
  2) we can \"write\" the commands that we \"speak\",
  3) we learn mostly by \"listening\", or \"reading\", others,
  4) we have good reasons to write:

     a) executable logs make us more productive,
     b) our notes/logs can, and should, be shared.

What it *did not* say explicitly was:

  5) that *not sharing* should be *immoral*,
  6) one of my main objectives with eev was REVENGE.

I spent (what felt like) hundreds of hours in the university
trying to learn things with the *NIX users and gurus there - to
practically no avail. They answered very few of my questions,
they only very rarely showed me their code or notes, and I can
remember only a handful of cases in which we sat side-by-side on
a terminal.

These people - who shared very little - were the most respected
hackers of that place.

They had to be stripped of their status.





4. How to read an e-script
==========================
The indented block below between the two \"snip, snip\" lines -
we will call it \"Example 1\" - exemplifies most of the basic
techniques available for e-scripts. These techniques will be
reviewed in the subsections below.


  --snip, snip--

  # Index:
  # «.lua5.1-debian»	(to \"lua5.1-debian\")
  # «.lua-tutorial»	(to \"lua-tutorial\")



  #####
  #
  # The main Debian packages for Lua 5.1
  # 2018jun02
  #
  #####

  # «lua5.1-debian» (to \".lua5.1-debian\")
  # (find-status   \"lua5.1\")
  # (find-vldifile \"lua5.1.list\")
  # (find-udfile   \"lua5.1/\")
  # (find-status   \"lua5.1-doc\")
  # (find-vldifile \"lua5.1-doc.list\")
  # (find-udfile   \"lua5.1-doc/\")
  # (find-udfile   \"lua5.1-doc/doc/\")
  # (find-udfile   \"lua5.1-doc/test/\")
  # http://www.lua.org/docs.html
  # http://www.lua.org/manual/5.1/manual.html
  # file:///usr/share/doc/lua5.1-doc/doc/manual.html

   (eepitch-shell)
   (eepitch-kill)
   (eepitch-shell)
  sudo apt-get install lua5.1 lua5.1-doc




  #####
  #
  # Downloading and opening an eev-based Lua tutorial
  # 2018jun02
  #
  #####

  # «lua-tutorial» (to \".lua-tutorial\")
  # http://angg.twu.net/e/lua-intro.e.html
  # http://angg.twu.net/e/lua-intro.e

   (eepitch-shell)
   (eepitch-kill)
   (eepitch-shell)
  cd /tmp/
  rm -v lua-intro.e
  wget http://angg.twu.net/e/lua-intro.e

  # (find-fline \"/tmp/lua-intro.e\")
  # (find-anchor \"/tmp/lua-intro.e\" \"intro:types\")
  # (defun eejump-71 () (find-fline \"/tmp/lua-intro.e\"))

  --snip, snip--




4.1. Anchors and `to'
---------------------
The two lines below

  # «.foo»	 (to \"foo\")
  # «foo» 	(to \".foo\")

\"point to one another\". This is explained here:

  (find-eev-quick-intro \"8. Anchors\")

We used this in Example 1 to create an index. Compare with:

  # Index:
  # «.one»	(to \"one\")
  # «.two»	(to \"two\")

  ###
  ## Stuff in block \"one\"
  ###

  # «one» (to \".one\")
  (...)

  ###
  ## Stuff in block \"two\"
  ###

  # «two» (to \".two\")



4.2. Debian hyperlinks
----------------------
The hyperlinks using `find-status', `find-vldifile', and
`find-udfile' are hyperlinks to information about a Debian
package. These hyperlinks

  (find-status   \"bash\")
  (find-vldifile \"bash.list\")
  (find-udfile   \"bash/\")

are equivalent to:

  (find-fline \"/var/lib/dpkg/status\" \"\\nPackage: bash\\n\")
  (find-fline \"/var/lib/dpkg/info/bash.list\")
  (find-fline \"/usr/share/doc/bash/\")

See:

  (find-eev \"eev-blinks.el\" \"find-Package\")
  (find-eev \"eev-blinks.el\" \"find-Package\" \"find-status\")
  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-eev-quick-intro \"9.1. `code-c-d'\" \"(code-c-d \\\"ud\\\"\")
  (find-eev \"eev-code.el\" \"code-c-d-s\")
  (find-eev \"eev-code.el\" \"code-c-d-s\" \"(code-c-d \\\"ud\\\"\")
  (find-eev \"eev-code.el\" \"code-c-d-s\" \"(code-c-d \\\"vldi\\\"\")



4.3. URL hyperlinks
-------------------
The lines

  # http://www.lua.org/docs.html
  # http://www.lua.org/manual/5.1/manual.html
  # file:///usr/share/doc/lua5.1-doc/doc/manual.html

are URL hyperlinks. Here's how to follow them:

  (find-eev-quick-intro \"3.1. Non-elisp hyperlinks\")

Note that the \"file:///\" URL above points to a local copy of
the Lua manual at \"http://www.lua.org/manual/5.1/manual.html\" -
but the local copy will only exist if the Debian package
\"lua5.1-doc\" is installed.



4.4. Eepitch blocks
-------------------
This

   (eepitch-shell)
   (eepitch-kill)
   (eepitch-shell)
  sudo apt-get install lua5.1 lua5.1-doc

is an \"eepitch block\", as explained here:

  (find-eev-quick-intro \"6. Controlling shell-like programs\")
  (find-eev-quick-intro \"6.1. The main key: <F8>\")

Note that it will only work if you delete the whitespace before
the \"\"s!



4.5. Htmlized e-scripts
-----------------------
The \"Eev Manifesto\" in section 3 above has this:

  I have placed essentially all my \"scripts\" written in this
  way (I call them \"e-scripts\") in a public place. They contain
  almost everything I know about Unix.

The \"public place\" is here:

  http://angg.twu.net/e/

The links

  # http://angg.twu.net/e/lua-intro.e.html
  # http://angg.twu.net/e/lua-intro.e

point to one of these e-scripts - one that I use to teach (or
introduce) Lua to people that already know other programming
languages. The

  # http://angg.twu.net/e/lua-intro.e.html

point to an \"htmlized version\" of it, in which many of the
hyperlinks are converted to something that works in a browser.
The header of the .html explains briefly how the htmlized version
is produced.




4.6. The `rm' in the eepitch block
----------------------------------
When we execute this eepitch block a first time,

   (eepitch-shell)
   (eepitch-kill)
   (eepitch-shell)
  cd /tmp/
  rm -v lua-intro.e
  wget http://angg.twu.net/e/lua-intro.e

the \"rm\" gives an error:

  rm: cannot remove 'lua-intro.e': No such file or directory

When we execute it a second, third, fourth time, the \"rm\"
deletes the file \"/tmp/lua-intro.e\" that the wget downloaded in
the previous run.

I usually write my eepitch blocks a few lines at a time, and I
test the new lines by running the whole block again from the
beginning. This means that for me most of the time a line like

  rm -v lua-intro.e

does not give an error - and I got used to ignoring the error
when it's run for the first time.

Most e-scripts in

  http://angg.twu.net/e/

follow these conventions:

  1) they can be re-run,
  2) they start with clean-up code,
  3) I prefer to write the clean-up code to be short, even when
     this means that I will have to ignore errors and warnings.




4.7. A convention about order
-----------------------------
(Explain why I feel natural to put the eepitch block that
installs the lua5.1 packages at the end of the first e-script
block, after the hyperlinks to files from that package)








5. Tools for writing e-scripts
==============================
One of my favorite ways of describing eev is as a \"tool to
create executable logs\", but this only make sense if we clarify
some ideas and terms.

The e-script in Example 1 has two _e-script blocks_ plus an
_index_. The fist block has notes about installing the packages
for Lua5.1 in Debian and inspecting them, and the second block is
about downloading and using an eev-based Lua tutorial. Let's
think of each of these blocks as a _task_.

The task \"install Lua5.1\" is performed in one way if we're
doing it for the first time, and in a different way if we're
doing it for the n-th time with some memory of what we did in the
previous times and of what we found important and what not.
Performing a task like this consists of several steps, that can
be roughly divided into \"visiting\" and \"commands\". I borrowed
the term \"visiting\" from Emacs:

  (find-enode \"Visiting\" \"Visiting Files\")

Look at the first block of Example 1 again. It has several elisp
hyperlinks to information about the packages \"lua5.1\" and
\"lua5.1-doc\". Following those hyperlinks let us \"visit\" the
descriptions of the two packages, their lists of files, and some
of their directories. Then the e-script block has three URL links
to the Lua documentation in general and to its reference manual,
and then an eepitch block that runs an \"apt-get install\".

We saw how to follow links and how to execute eepitch blocks, so
an e-script block is \"executable\". But in what sense it is a
\"log\"?

  1. In the old days log books were always made of paper, and
     there was nothing automatic in taking notes with them. We
     would have to decide what to write and how to write it, and
     we would have to alternate between the \"task\" and \"taking
     notes\". After many years of practice _some_ people would
     learn how to take notes without distracting themselves much
     from the task at hand, and they would learn how to make
     their notes at the same time concise and readable enough.

  2. Nowadays, with computers, there are _some_ ways to write
     logs automatically - for example, most shells record the
     commands given to them - but the output is of low quality.

  3. Eev takes an intermediate stance between \"notes by hand\"
     and \"automatic notes\". It is possible to do
     \"task\"+\"notes\" with just a few more keystrokes than for
     doing just \"task\", but that requires learning some tricks,
     and having some practice.

The next sections discuss those tricks.





5.1. Anchors-to pairs and e-script blocks
-----------------------------------------
Anchor-to pairs can be generated easily using `M-A':

  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\")
  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\" \"M-A\")

Typing `M-A' on this line:

  # <bletch>

yields this pair of anchor-to lines:

  # «.bletch»	(to \"bletch\")
  # «bletch»	(to \".bletch\")

let's call the first one a \"to-forward\" and the second one a
\"to-back\". In my e-scripts files I follow the convention that
the to-forwards are all together at the beginning of the file,
forming an index of sections of the file, and each to-back is at
the beginning of a section. Most of the source files of eev
follow this convention; see, for example:

  (find-eev \"eev-blinks.el\" \".eek\")
  (find-eev \"eev-blinks.el\" \"eek\")

Note that there, and in most source files of eev, above each
to-back line there a title in big letters in ASCII art in Lisp
comments.

After creating a to-forward-to-back pair with `M-A' we have to
move the to-forward line to the index by hand, using cut and
paste.

In the e-script files in

  http://angg.twu.net/e/

I follow another convention - \"e-script blocks\". The title
above each to-backward line is written like this:

  #####
  #
  # Long description for the section Bletch
  # 2016feb29
  #
  #####

Note that you can generate a title in \"#\"s like that, followed
by a to-forward-to-back pair, by typing `M-B' on a line like:

  bletch Long description for the section Bletch

See:

  (find-eev-quick-intro \"8.4. Creating e-script blocks\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\" \"`M-B'\")

The to-forward line still has to be moved to the index by cut and
paste by hand.




5.2. Debian hyperlinks
----------------------
The key `M-D' converts a line with the name of a Debian package,
like this,

lua5.1-doc

to these three hyperlinks,

# (find-status   \"lua5.1-doc\")
# (find-vldifile \"lua5.1-doc.list\")
# (find-udfile   \"lua5.1-doc/\")

and moves the point to the next line. Try it! Put the point on
the \"bash\" line below and type `M-D M-D M-D':

bash
lua5.1
lua5.1-doc




5.3. URL hyperlinks
-------------------
I usually \"write\" URLs in Emacs by copying them from a browser
to Emacs.



5.4. Eepitch blocks
-------------------
We saw in

  (find-eev-quick-intro \"6.3. Creating eepitch blocks: `M-T'\")

how to create eepitch blocks with `M-T'. Try that on the line
that says \"lua51\" below:

lua51



5.5. `rm/mkdir/cd' triples
--------------------------
Try typing `M-R' on the line with the \"/tmp/foo/\" below:

/tmp/foo/




5.6. Hyperlinks to files, directories, and info nodes
-----------------------------------------------------
One practical way to create a `find-fline' hyperlink is with
`M-F' (`eewrap-find-fline'). Try it here:

/usr/share/doc/lua5.1-doc/test/
/usr/share/doc/lua5.1-doc/test/README
/usr/share/doc/lua5.1-doc/test/fibfor.lua

Note that the three lines above were copied from:

  (find-vldifile \"lua5.1-doc.list\")
  (find-vldifile \"lua5.1-doc.list\" \"/usr/share/doc/lua5.1-doc/test\")

Another way is by visiting a file and typing `M-h M-h'. See:

  (find-eev-quick-intro \"4.1. `find-here-links'\")

This can also be used to generate links to info nodes.

(...)




6. Tutorials
============
All the tutorials in eev follow these four principles:

  1. All the examples in them are very easy to run,
  2. The links to documentation are easy to follow,
  3. All the \"sub-examples\" are easy to run,
  4. The links to related documentation - including links to
     primary sources - are easy to follow.

I used to believe that all these four principles would be
immediately obvious to everyone who had played a bit with the
tutorials of eev, and I thought that people would realize that
these are very good principles, that they should follow too...
and so these people would start to apply these principles to
their own e-scripts, and they would adapt them to other
environments that are not Emacs-based, and these ideas would
spread (sort of) naturally. Well, I was totally wrong - and I
only discovered that in 2021, when I chatted with some relatively
advanced eev users to whom those ideas were not obvious at all.
So let me try to be explain these principles clearly - especially
the principles 3 and 4 - and show how they can be applied to
other contexts besides tutorials.

The idea of \"making sub-examples very easy to run\" is
especially easy to see in the elisp tutorials. In this
introduction

  (find-elisp-intro \"1. Introduction\")

we have first this sexp,

     (+ (* 2 3) (* 4 5))

and then these ones:

           2
             3
        (* 2 3)
                   4
                     5
                (* 4 5)
     (+ (* 2 3) (* 4 5))
  (list (* 2 3) (* 4 5))

In the first sexp people _could_ execute the subsexps (* 2 3)
and (* 4 5) by typing `M-E' in the right places... remember:

  (find-eval-intro \"2. The end of line and `M-e'\")
  (find-eval-intro \"2. The end of line and `M-e'\" \"without moving\")

...but it is much easier to see the sub-sexps, and to execute
them, when they are in different lines. This example also serves
to stress to beginners than number, like 4, are sexps too, and
they can be evaluated - the result of evaluating 4 is 4.

In a sequence of sub-sexps, like the one above, I usually try to
arrange the sexps in a didactic order: to understand the result
of (+ (* 2 3) (* 4 5)) we need to understand first the results
of (* 2 3) and (* 4 5). Also, the best way to understand the new
idea that appears in

  (list (* 2 3) (* 4 5))

is to compare it with this other sexp, that appears just before
it and is conceptually simpler:

     (+ (* 2 3) (* 4 5))

I try to apply this principle - \"make sub-examples very easy to
run\" - to tutorials for other languages, too. At this moment the
only tutorial for another language that I have that is in a
_reasonably_ organized form is this one,

               http://angg.twu.net/e/lua-intro.e.html
  (find-wgeta \"http://angg.twu.net/e/lua-intro.e\")
  (find-wgeta \"http://angg.twu.net/e/lua-intro.e\" \"intro:for\")

but parts of it were written in 2004, when these principles were
not yet very clear to me. I am revising it, and I am also trying
to convince some students to work together with me on tutorials
for shell and Python, but they are not very enthusiastic (yet).

The \"links to related documentation\" can also be arranged in a
didactical order. For example, here,

  (find-eev-quick-intro \"6.4. Red stars\")
  (find-eev-quick-intro \"6.4. Red stars\" \"bullet\")
  (find-eepitch-bullet-links)

the first link points to a section of a tutorial that most people
should have stumbled on; the second link points to technical a
point in it that most people ignore on a first reading, and the
third one points to something much more technical,
containing (executable!) source code.

(TO DO: mention test blocks)




7. Sequences of links
=====================
A good part of my executable notes consists of sequences of links
in which in practically all cases the new link \"refines\" the
previous one in some sense; the notion of \"refining\" explained
here is just one of these senses:

  (find-refining-intro \"1. Pos-spec-lists\")

Let me compare two styles: 1. textual, 2. executable notes.

  1. The section 2 of the tutorial `find-eev-quick-intro'
     mentions that `M-e' accepts several different numerical
     prefixes, but it only explains one case: `M-0 M-e'. One way
     to discover what the other numerical prefixes do is to use
     `M-h M-k', that is explained in the section 4.2 of
     `find-eev-quick-intro', on `M-e', and then follow the link
     in the temporary buffer that starts with `find-efunction'.
     The key `M-e' is bound to `ee-eval-sexp-eol', and one of
     lines in the docstring of `ee-eval-sexp-eol' says this:

       3:  same as 2, but also switch to the new window

     If we follow the source code we see that `M-3 M-e' executes
     `ee-eval-last-sexp-3', that calls a function called
     `find-wset', that is defined in eev-multiwindow.el. Some
     functions in eev-multiwindow.el call functions like
     `split-window-horizontally', that are mentioned in section 6
     of `(find-emacs-keys-intro)'.

     The comments at the top of eev-multiwindow.el say that the
     functions in that file are explained in a tutorial called
     `find-multiwindow-intro'. It turns out that the first
     argument of `find-wset' is a string that is interpreted as
     series of commands in a mini-language in which each
     character means a certain operation on windows.

Now let's see the second style: executable notes. I wrote the
block below by following the idea described here:

  (find-here-links-intro \"1. Alternating between \\\"task\\\" and \\\"notes\\\"\")

Every time that I found something interesting I would save a link
to it in my notes, and these are my notes with only very minor
clean-ups.

  2.   (find-eev-quick-intro \"2. Evaluating Lisp\")
       (find-eev-quick-intro \"2. Evaluating Lisp\" \"M-0 M-e\")
       (find-eev-quick-intro \"4.2. `find-ekey-links' and friends\")
       (eek \"M-h M-k  M-e\")
       (eek \"M-h M-k  M-e  ;; ee-eval-sexp-eol\")
  (find-eek \"M-h M-k  M-e  ;; ee-eval-sexp-eol\")
  (find-eek \"M-h M-k  M-e  ;; ee-eval-sexp-eol\" \"(find-efunction ')\")
             (find-efunction 'ee-eval-sexp-eol)
             (find-efunction 'ee-eval-sexp-eol \"3:\")
             (eek \"2*<up> M-3 M-e\")
             (find-efunction 'ee-eval-last-sexp)
             (find-efunction 'ee-eval-last-sexp-3)
             (find-efunction 'ee-eval-last-sexp-3 \"find-wset\")
             (find-efunction 'find-wset)

  (find-emacs-keys-intro \"6. Windows\")
  (find-emacs-keys-intro \"6. Windows\" \"L|R\")

  (find-eev-intro)
  (find-eev-intro \"M-5 M-0 M-j\")
  (find-eev-intro \"(find-multiwindow-intro)\")
  (find-multiwindow-intro)

  (find-wset \"13o_2o2o23oo33ooo\"  '(find-ebuffer \"B\"))
  (find-wset \"13o_2o2o23oo33ooo+\" '(find-ebuffer \"B\"))
  (find-2a nil '(find-efunction 'ee-eval-sexp-eol))
  (find-2b nil '(find-efunction 'ee-eval-sexp-eol))

The notes in the style 1 are a translation to English to the
notes in the style 2. I wrote the notes in style 2 first.

Some people _PANIC_ when they see notes written in the second
style, and most of us are conditioned to believe that a) we have
to try to write notes that are readable by everyone - even though
that is impossible if we take the \"everyone\" literally, b)
notes in the second style can't be shared in public. However:

  a) It is good karma to make our notes publically available even
     if very few people will be able to read them,

  b) we are our main readers - and we need to try to make our
     notes easily to read by ourselves,

  c) _executability_ helps in _readability_,

  d) people are experimenting with ways of writing executable
     notes, and even the most popular styles of doing that -
     Jupyter Notebooks, I guess? - are not yet really hugely
     popular.

One of my intents with eev is to encourage people to experiment
with ways of writing executable notes. This involves learning the
functions and techniques that already exist, and inventing new
ones - and, hopefully, sharing them.




8. IRC
======
(TO DO: explain this:)

  (find-efunction 'ee-0x0-upload-region)
  (find-efunction 'ee-0x0-upload-region \"aliased to `u0'\")


9. Git
======
Example:

  (progn

    ;; Links to the git repository:
    ;; https://github.com/edrx/emacs-lua
    ;; https://github.com/edrx/emacs-lua/blob/main/tests.e
    ;; https://raw.githubusercontent.com/edrx/emacs-lua/main/tests.e

    (setq ee-emluagit-base
	  \"https://raw.githubusercontent.com/edrx/emacs-lua/main/\")
    (defun find-emluagitfile (fname &rest rest)
      (apply 'find-wget (format \"%s%s\" ee-emluagit-base fname) rest))
    (defun find-emluagit (fname &rest rest)
      (apply 'find-wgeta (format \"%s%s\" ee-emluagit-base fname) rest))

    ;; Tests:
    ;; (find-emluagit \"tests.e\")
    ;; (find-emluagit \"tests.e\" \"find-angg-and-find-es\")
    ;; (find-emluagit \"tests.e\" \"find-angg-and-find-es\" \"Tests:\")
    ;; (find-emluagitfile \"tests.e\" \"Warning:\")

    ;; Links to a local copy of the git repository:
    ;; (find-git-links \"https://github.com/edrx/emacs-lua\" \"emlua\")
    ;; (setq ee-git-dir \"~/usrc/\")
    ;;
    ;; (find-code-c-d \"emlua\" \"~/usrc/emacs-lua/\" :anchor)
    (code-c-d \"emlua\" \"~/usrc/emacs-lua/\" :anchor)
    ;;
    ;; Tests:
    ;; (find-emlua \"\")
    ;; (find-emlua \"tests.e\")
    ;; (find-emlua \"tests.e\" \"find-angg-and-find-es\")
    ;; (find-emlua \"tests.e\" \"find-angg-and-find-es\" \"Tests:\")
    ;; (find-emluafile \"tests.e\" \"Warning:\")

    ;; Compare:
    ;; (find-emluagit \"tests.e\" \"find-angg-and-find-es\")
    ;; (find-emlua    \"tests.e\" \"find-angg-and-find-es\")

    )


" pos-spec-list)))

;; (find-escripts-intro)



;;;   ____ _ _
;;;  / ___(_) |_
;;; | |  _| | __|
;;; | |_| | | |_
;;;  \____|_|\__|
;;;
;; «find-git-intro»  (to ".find-git-intro")
;; Skel: (find-intro-links "git")

(defun find-git-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-git-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-git-intro)
Source code:  (find-efunction 'find-git-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



At this moment this is a call for help -
not an intro.




0. Introduction
===============
Git is extremely popular, and I've heard that the two packages
that attract most new users to Emacs are Org and Magit:

  https://github.com/magit/magit
  https://melpa.org/#/magit
  (find-epackage 'magit)

At this moment eev offers only a trivial hack to help people
download git repositories. If you put the point on the github url
above and type `M-h g' or `M-x find-git-links' you will get the
same effect as running the sexp below:

  (find-git-links \"https://github.com/magit/magit\" \"magit\")

You will get a temporary buffer with an e-script for downloading
(\"cloning\") that git repository and inspecting it in a handful
of ways.

I found git VERY hard to learn. To test most concepts you need a
repository with tags and branches and a second repository that
\"pulls\" from it, and no books or tutorials that I know of come
with a sequence of commands that set that up in a way that makes
the tests easy to reproduce. To make things worse, when I was
trying to set up a git repository for eev on github for the first
time I did some things wrong on the github side of the repository
that I did not know how to undo... after spending some days
trying to fix that I gave up, deleted that repository, created a
new one, and decided that I would always do LOTS of local tests
before messing up my public repository again.

This intro is about doing these local tests - but it is in a VERY
early draft.




1. Preparation
==============
Download the second URL below with `M-x brep',

  https://github.com/pluralsight/git-internals-pdf/
  https://github.com/pluralsight/git-internals-pdf/releases/download/v2.0/peepcode-git.pdf

and do the same for the fourth URL below here:

  https://git-scm.com/book/en/v2
  https://github.com/progit/progit2
  https://github.com/progit/progit2/releases
  https://github.com/progit/progit2/releases/download/2.1.277/progit.pdf

Then run this eepitch block,

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  rm -Rfv /tmp/git-test/
  mkdir   /tmp/git-test/
  cd      /tmp/git-test/
  #    http://angg.twu.net/bin/git-defs.html
  wget http://angg.twu.net/bin/git-defs
  cp -v $S/https/github.com/pluralsight/git-internals-pdf/releases/download/v2.0/peepcode-git.pdf .
  cp -v $S/https/github.com/progit/progit2/releases/download/2.1.277/progit.pdf .

and this prog1:

  (prog1
    (code-pdf-page \"gitinternals\" \"/tmp/git-test/peepcode-git.pdf\")
    (code-pdf-text \"gitinternals\" \"/tmp/git-test/peepcode-git.pdf\")
    (code-pdf-page \"progit\" \"/tmp/git-test/progit.pdf\")
    (code-pdf-text \"progit\" \"/tmp/git-test/progit.pdf\")
    (code-c-d \"gitdoc\" \"/usr/share/doc/git-doc/\")
    )

If can test if everything works by running the six sexps below:

  (find-fline \"/tmp/git-test/git-defs\")
  (find-gitinternalspage)
  (find-gitinternalstext)
  (find-progitpage)
  (find-progittext)
  (find-gitdocfile \"\")




2. A first repository
=====================
The manpage of git-revisions

  (find-man \"7 git-revisions\")
  (find-man \"7 git-revisions\" \"Here is an illustration\")
  (find-gitdocfile \"revisions.txt\" \"illustration, by Jon Loeliger\")

has this example, in which the commits have this structure:

  G   H   I   J
   \\ /     \\ /
    D   E   F
     \\  |  / \\
      \\ | /   |
       \\|/    |
        B     C
         \\   /
          \\ /
           A

Here is an e-script that creates it:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  rm -Rfv /tmp/git-test/repo1/
  mkdir   /tmp/git-test/repo1/
  cd      /tmp/git-test/repo1/
  .       /tmp/git-test/git-defs
  # (find-fline \"/tmp/git-test/git-defs\")

  git init
  Modify file1; Modify file2; git add file1 file2
                Commit A; git branch brAC
  Modify file1; Commit B; git branch brBDG
  git checkout brAC
  Modify file1; Commit C
  git checkout brBDG
  Modify file1; Commit D
  git checkout HEAD^ -b brE
  Modify file1; Commit E
  git checkout HEAD^
  git merge -s ours brAC -m F
  git branch brFI
  git checkout brBDG
  Modify file1; Commit G
  git checkout HEAD^ -b brH
  Modify file1; Commit H
  git checkout brFI
  Modify file1; Commit I
  git checkout HEAD^ -b brJ
  Modify file1; Commit J
  Diagram

  # (find-gitk \"/tmp/git-test/repo1/\")

Actually it creates the structure below - where, for example, the
node \"G,brBDG\" is a commit with message \"G\" and a branch
called \"brBDG\" pointing to it; I call that branch \"brBDG\"
because when it was created it pointed to the commit with message
\"B\", then it moved to the commit \"D\", then to \"G\".

                 I,brFI  J,brJ
                    |   /
  G,brBDG   H,brH   |  /
         \\ /        | /
          D  E,brE  F
           \\   |   / \\
            \\  |  /   |
             \\ | /    |
               B    C,brAC
                \\    /
                 \\  /
                  A

Here's a video showing the script above in action:

  http://angg.twu.net/eev-videos/2020-doubt-about-merging.mp4
          (code-eevvideo \"merg\" \"2020-doubt-about-merging\")
                    (find-mergvideo \"0:00\")
                    (find-mergvideo \"0:20\")

NEXT STEPS: explain how to use git merge both from the command
line and from magit; explain HEAD, master, and tags. Point to man
pages in man and text format and to sections in Scott Chacon's
book. I NEED LOTS OF HELP HERE.

  (find-man \"git-merge\")
  (find-gitinternalspage)
  (find-gitinternalstext)





3. A second repository
======================
HELP PLEEEASEEEE


 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  rm -Rfv /tmp/git-test/repo2/
  mkdir   /tmp/git-test/repo2/
  cd      /tmp/git-test/repo2/
  .       /tmp/git-test/git-defs
  # (find-fline \"/tmp/git-test/git-defs\")

  git clone /tmp/git-test/repo1/ .
  ls -lAF

  # (find-gitk  \"/tmp/git-test/repo2/\")
  # (find-fline \"/tmp/git-test/repo2/\")
  # (find-fline \"/tmp/git-test/repo2/.git/\")
  # (find-fline \"/tmp/git-test/repo2/.git/config\")

  # (find-man \"1 git-remote\")
  # (find-progitpage (+ 24  36) \"Adding Remote Repositories\")
  # (find-progittext (+ 24  36) \"Adding Remote Repositories\")

" pos-spec-list)))

;; (find-git-intro)






;;; __        ___        _                _
;;; \ \      / / |      | |__   ___  __ _(_)_ __  _ __   ___ _ __
;;;  \ \ /\ / / __)_____| '_ \ / _ \/ _` | | '_ \| '_ \ / _ \ '__|
;;;   \ V  V /\__ \_____| |_) |  __/ (_| | | | | | | | |  __/ |
;;;    \_/\_/ (   /     |_.__/ \___|\__, |_|_| |_|_| |_|\___|_|
;;;            |_|                  |___/
;;
;; «find-windows-beginner-intro»  (to ".find-windows-beginner-intro")
;; Skel: (find-intro-links "windows-beginner")

(defun find-windows-beginner-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-windows-beginner-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-windows-beginner-intro)
Source code:  (find-efunction 'find-windows-beginner-intro)
More intros:  (find-eev-quick-intro)
              (find-eval-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.
The quickest way to open or recreate this is with `M-3 M-j'.




This is a tutorial on how to install Emacs and eev on M$ Windows,
and on the basic steps for learning Emacs and eev after
installing them. If you're a W$ user you should start by reading
this tutorial online in a browser, at

  http://angg.twu.net/eev-intros/find-windows-beginner-intro.html

while you run Emacs in another window (see section 1). After
getting eev installed on your machine (see section 3) you will be
able to access this tutorial from Emacs by typing `M-3 M-j'.

The main tutorial on eev is this one,

  (find-eev-quick-intro)
  http://angg.twu.net/eev-intros/find-eev-quick-intro.html

The sections from 5.6 onwards were written in nov/2021 and they
cover how to install some external programs to make almost all
features of eev work on M$ Windows. Feedback on them would be
greatly appreciated!






0. Introduction
===============
My favorite exposition of what eev is is this presentation,
called \"How to record executable notes with eev - and how to
play them back\":

  http://angg.twu.net/emacsconf2019.html
  http://angg.twu.net/LATEX/2019emacsconf.pdf (slides)
  http://www.youtube.com/watch?v=86yiRG8YJD0  (video)

The video ends with a demo that shows a non-trivial example of
\"executable notes\". The most interesting part of that demo
shows how to use eev to send commands to an external program - a
unix shell, being run in interactive mode. Here's a link to that
part of the video on youtube, plus two related links that are
harder to explain:

  http://www.youtube.com/watch?v=86yiRG8YJD0#t=15m11s
  (find-eev2019video \"15:11\" \"Demo: the eepitch block\")
  http://angg.twu.net/.emacs.videos.html#eev2019

I don't have easy access to Windows machines, so I have to rely
on friends to test some things for me. I also don't have easy
access to people who use Windows and who do have experience using
terminals, so from time to time I give workshops for \"total
beginners\" like the one described here,

  (find-video-links-intro \"8. Windows\")

and I see that the people on those workshops get stuck on ideas
that I thought that were obvious, and I change my approach, and I
rewrite lots of things.

This is my N-th attempt to rewrite this tutorial.

Version of these instructions: 2022may03.




1. Download and install Emacs
=============================
Read the README below and then install Emacs using either the
link to the .exe or the link to the .zip:

https://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-28/
https://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-28/README-windows-binaries
https://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-28/emacs-28.0.92-installer.exe

You may need to create a desktop icon or shortcut to
<emacsdir>/bin/runemacs.exe.



1.1. Using Emacs 27.2 on Windows
--------------------------------
Installing eev - or any other package from ELPA - on Emacs 27.2
on Windows may be tricky. For details, see:

  (find-eev-install-intro \"5. Ways to download eev\")
  (find-eev-install-intro \"5. Ways to download eev\" \"2.\")
  (find-eev-install-intro \"5. Ways to download eev\" \"because of this bug\")

So: please upgrade to Emacs28-pretest if you can!




2. Key sequences and how to quit
================================
Most people who use Emacs do many things by using key sequences - for
example `C-x C-s' to save the current file.

`C-x C-s' is the Emacs notation for \"control-x control-s\". This
notation is explained here:

  (find-enode \"User Input\" \"<Ctrl>\" \"a\" \"C-a\")
  (find-enode \"User Input\" \"<Meta>-a\" \"M-a\" \"<Alt>\")

The best way to learn key sequences when you are a beginner is by
using the menu bar:

  (find-enode \"Menu Bar\")

for example, in the \"File\" menu the last option is:

  Quit             C-x C-c

If you type just `C-x' and wait the `C-x' will be displayed in the
\"Echo area\" at the bottom of the screen.  This is explained here:

  (find-enode \"Echo Area\")

  The line at the very bottom of the frame is the \"echo area\".  It is
  used to display small amounts of text for various purposes.

     The echo area is so-named because one of the things it is used
  for is \"echoing\", which means displaying the characters of a
  multi-character command as you type.  Single-character commands are
  not echoed.  Multi-character commands (*note Keys) are echoed if you
  pause for more than a second in the middle of a command.  Emacs then
  echoes all the characters of the command so far, to prompt you for
  the rest.  Once echoing has started, the rest of the command echoes
  immediately as you type it.  This behavior is designed to give
  confident users fast response, while giving hesitant users maximum
  feedback.

     The echo area is also used to display an \"error message\" when a
  command cannot do its job.  Error messages may be accompanied by
  beeping or by flashing the screen.

There are several ways to abort a key sequence in the middle.  They
are explained here,

  (find-enode \"Quitting\")

but what I recommend to beginners is: if you are stuck in the middle
of a key sequence and don't know how to abort it, just go to the
\"File\" menu, use the option \"Quit\", and restart Emacs.




3. Using M-x and installing eev
===============================
We can run commands by name by using `M-x'.  `M-x' uses the last line
of the screen as a \"minibuffer\" - see:

  (find-enode \"Minibuffer\")
  (find-enode \"Basic Minibuffer\" \"it appears in the echo area\")
  (find-enode \"M-x\" \"Running Commands by Name\")

To install eev, do this:
  1. run `M-x package-initialize',
  2. run `M-x list-packages',
  3. select \"eev\" at the list of packages,
  4. click on \"install\".

To load eev and enter its tutorial, run
  `M-x eev-beginner'.

The tutorial looks like this:
  (find-eev-quick-intro)

There's a video about these basic steps here:

  \"How to install eev with M-x list-packages and how to navigate its tutorials\"
  http://angg.twu.net/eev-videos/2020-list-packages-eev-nav.mp4
             http://angg.twu.net/2020-list-packages-eev-nav.html
             http://angg.twu.net/.emacs.videos.html#eevnav
             http://www.youtube.com/watch?v=kxBjiUo88_U





4. Understanding buffers and the mode line
==========================================
It's good to be able to interpret the mode line - it gives a lot of
information about where we are.  See:

  (find-enode \"Mode Line\")

For example, after running `M-x eev-beginner' the mode line says this:

  -:**-  *(find-eev-quick-intro)*   Top L1     (Fundamental eev) ---

The best way to understand what each component means is by moving the
mouse pointer onto it and looking at the help that is displayed.  The
main components in this case are:

   \"**\" - this buffer is read-write and has been modified.  See:

      (find-enode \"Mode Line\" \"**\")

   \"*(find-eev-quick-intro)*\" - the name of this buffer.

      A curiosity: this buffer is not associated to a file!  If you
      try to save it with `C-x C-s' or with the \"Save\" option in the
      \"File\" menu you will get a prompt in the minibuffer that starts
      with:

        File to save in:

      For more information on buffers and files, see:

        (find-enode \"Mode Line\" \" BUF \" \"name of the buffer\")
        (find-enode \"Buffers\" \"Most buffers are made by visiting files\")
        (find-enode \"Basic Files\")

   \"Top L1\" - see:

      (find-enode \"Mode Line\" \"Top\")
      (find-enode \"Mode Line\" \"line number at point\")
      (find-enode \"Point\")

   \"(Fundamental eev)\" - see:

      (find-enode \"Mode Line\" \"(MAJOR MINOR)\")
      (find-enode \"Mode Line\" \"major mode\")
      (find-enode \"Mode Line\" \"minor modes\")




5. More on modes
================
The \"eev\" in the mode line means that the key bindings defined by eev
are \"active\".  The main key bindings of eev are listed here,

  (find-emacs-keys-intro \"1. Basic keys (eev)\")

and if you want more details about what is an \"active keymap\" you can
read these sections of the manual:

  (find-enode \"Key Bindings\")
  (find-enode \"Keymaps\")
  (find-enode \"Local Keymaps\")

The five main major modes that beginners need to learn about are the
Fundamental Mode, the Help Mode, the Info Mode, Dired Mode, and Shell
mode.  In the Fundamental Mode most \"basic\" keys are interpreted as
editing keys - see:

  (find-enode \"Major Modes\" \"Fundamental mode\")
  (find-enode \"Keymaps\" \"self-inserting character\")




5.1. Eev mode
-------------
The three main keys of Eev Mode are these ones:

  M-e   - to follow a hyperlink.  Mnemonic: \"(e)valuate\"/\"(e)xecute\".
          See: (find-eev-quick-intro \"2. Evaluating Lisp\")
               (find-eev-quick-intro \"3. Elisp hyperlinks\")
  M-j   - to jump to certain predefined places.  In particular,
              `M-j' takes you to a buffer with basic help and a
                    list of jump targets. See:
                    (find-eev-quick-intro \"7.2. The list of eejump targets\")
          `M-2 M-j' takes you to this help page.
          `M-5 M-j' takes you to: (find-eev-quick-intro)
  M-k   - to go back.  Mnemonic: \"(k)ill buffer\".
          See: (find-eev-quick-intro \"3. Elisp hyperlinks\" \"M-k\")

The text above was taken from:

  (find-emacs-keys-intro \"1. Basic keys (eev)\")

From this point onwards I will suppose that the reader knows how to
use at least `M-e' and `M-j'.

Note the Eev mode is a global minor mode.  The next subsections are
about the other four main major modes - besides Fundamental mode.




5.2. Help Mode
--------------
Most help commands in Emacs display buffers in Help Mode.  For
example, if you run `C-h f Info-mode' or execute one of the sexps
below

  (find-efunctiondescr 'Info-mode)
  (find-efunctiondescr 'help-mode)
  (find-efunctiondescr 'dired-mode)

you will get buffers in Help Mode - they are read-only, and if you
type `q' in them this be interpreted as `quit' rather than as \"insert
the character `q'\".



5.3. Info Mode
--------------
The key sequence `C-h r' opens the Emacs manual in Info Mode.  This is
a read-only mode like Help Mode, but more keys become navigation keys.
The main ones are:

  q     Quit Info: reselect previously selected buffer.
  RET   Follow a node reference near point.
  n     Move to the \"next\" node of this node.
  p     Move to the \"previous\" node of this node.
  u     Move \"up\" from this node.

Also, the tool bar changes completely, and it shows icons
corresponding to the main navigation keys.  See:

  (find-enode \"Tool Bars\")



5.4. Dired Mode
---------------
Dired Mode lets you navigate the directory structure.  You can enter
it by typing `C-x C-f RET'.  If you type `RET' on a line that shows a
directory this will be interpreted as \"enter that directory\", and
`RET' on a line that shows a file is interpreted as \"open that file\".
See:

  (find-enode \"Dired\")
  (find-enode \"Dired Enter\" \"C-x C-f\")
  (find-enode \"ls in Lisp\")



5.5. Shell Mode
---------------
Emacs can run terminals inside its buffers - and by default that
terminal runs the default shell.  In Windows the default shell is
\"cmd.exe\", that is reminiscent of MS-DOS, and is VERY clumsly.  See:

  https://en.wikipedia.org/wiki/Windows_Console
  https://en.wikipedia.org/wiki/Command-line_interface
  https://en.wikipedia.org/wiki/MS-DOS#Windows_command-line_interface
  https://en.wikipedia.org/wiki/Cmd.exe
  https://en.wikipedia.org/wiki/Batch_file
  https://en.wikipedia.org/wiki/Terminal_emulator

If you run `M-x shell' you will get a shell buffer.  See:

  (find-enode \"Interactive Shell\")

especially this part of the first paragraph:

  To give input to the subshell, go to the end of the buffer and type
  the input, terminated by <RET>.

As an exercise, try to give these commands to the Windows shell:

  dir
  cd
  cd ..
  dir



5.6. Eshell
-----------
The manual of Eshell - here,

  (find-node \"(eshell)Top\")

describes it as: \"Eshell is a shell-like command interpreter
implemented in Emacs Lisp\".

This feature of eev:

  (find-psne-intro \"1. Local copies of files from the internet\")

needs to run wget on a unix-like shell, and it turns out that
this can be achieved easily using Eshell. For a discussion of the
tecnhical details, see:

  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00051.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00058.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00126.html



5.6.1. Echo in Eshell
---------------------
Note that the \"echo\" of Eshell has a quirk: if you run these
lines

  echo http://foo.bar/    >> ~/.psne.log
  echo http://qux.bletch/ >> ~/.psne.log
  echo http://plic.ploc/  >> ~/.psne.log

on a \"normal\" unix shell these URLs will be appended to
~/.psne.log with newlines separating them, but if you run that in
Eshell they will be appended without newlines. One way to fix
that is to invoke the (built-in!) echo of Eshell with the \"-N\"
option, like this:

  echo -N http://foo.bar/    >> ~/.psne.log
  echo -N http://qux.bletch/ >> ~/.psne.log
  echo -N http://plic.ploc/  >> ~/.psne.log

The option \"-N\" (\"terminate with a newline\") is only
explained in the source of eshell:

  (find-efile \"eshell/em-basic.el\" \"defun eshell/echo\" \"?N\")

`find-psne-links' accepts two optional arguments after the URL.
The first of them is called `wget-options', and when it is not
given it defaults to \"-nc\" (\"no-clobber\"). The second
optional argument is called `echo-options', and _ideally_ it
should default to \"\" on \"normal\" unix shells, and to \"-N\"
on eshell. When it is not given eev runs the function
`ee-find-psne-echo-options' to decide what to do, and the default
definition of that function is:

  ;; See: (find-eev \"eev-tlinks.el\" \"ee-find-psne-echo-options\")
  ;;
  (defun ee-find-psne-echo-options ()
    (if (eq system-type 'windows-nt) \"-N\" \"\"))

which means: if a person is \"on Windows\", suppose that she is
using Eshell, and that she needs \"echo -N\"; is a person is on
\"non-Windows\", suppose that she is using a \"normal\" unix
shell, and that she needs just \"echo\", without the \"-N\". The
exact definition of the term \"on Windows\" can be found here:

  (find-evardescr 'system-type      \"windows-nt\")
  (find-elnode \"System Environment\" \"windows-nt\")

The suppositions above are often wrong. Here is the easiest way
to fix them: if you are \"on Windows\" but you use a \"normal\"
unix shell, you can put these two lines in your ~/.emacs to
override the definition above:

  ;; See: (find-windows-beginner-intro \"5.6.1. Echo in Eshell\")
  (defun ee-find-psne-echo-options () \"\")

and if you are \"on non-Windows\" but you use mostly Eshell, you
can put this in your ~/.emacs instead:

  ;; See: (find-windows-beginner-intro \"5.6.1. Echo in Eshell\")
  (defun ee-find-psne-echo-options () \"-N\")

If you use both Eshell and \"normal\" unix shells you can try to
redefine `ee-find-psne-echo-options' in other ways. If you find a
smarter way that works well, please get in touch!!!




6. Video links
==============
If you're on Windows you need to start by configuring your
browser and the path to it. The old instructions for that are
here...

  (find-video-links-intro)
  (find-video-links-intro \"3. `find-youtube-video'\")
  (find-video-links-intro \"4. Configuring the browser\")

...but in may/2022 I created a better way. It is explained in
the next section.





7. eev-wconfig.el
=================
NOTE: until may/2022 this section was about this module:

  (find-eev \"eev-on-windows.el\")

that became obsolete, and was superseded by:

  (find-eev \"eev-wconfig.el\")
  (find-eev \"eev-wconfig.el\" \"intro\")

To run eev-wconfig you need to run these sexps:

  (require 'eev-wconfig)
  (find-wconfig-links)

`(find-wconfig-links)' shows a temporary buffer with links to
several \"sub-wconfigs\". The main ones are:

  (find-wconfig-browser-links)
  (find-wconfig-wget-links)
  (find-wconfig-shell-links)
  (find-wconfig-lua-links)
  (find-wconfig-mpv-links)
  (find-wconfig-exercises-links)

Each one of these sub-wconfigs has detailed instructions for
configuring the thing/word after it: browser, wget, (E)shell,
Lua, Mpv. The best way to understand how to use these
sub-wconfigs is by watching the video about eev-wconfig, that is
here:

  Page:  http://angg.twu.net/eev-wconfig.html
  Index: (find-1stclassvideoindex  \"2022eevwconfig\")
  Info:  (find-1stclassvideo-links \"2022eevwconfig\")
  Play:  (find-2022eevwconfigvideo   \"0:00\")
         (find-2022eevwconfigvideo   \"17:29\" \"(find-wconfig-browser-links)\")
         (find-2022eevwconfigvideo   \"26:52\" \"...a long digression...\")
         (find-2022eevwconfigvideo   \"39:20\" \"(find-wconfig-wget-links)\")
         (find-2022eevwconfigvideo   \"53:41\" \"(find-wconfig-shell-links)\")
         (find-2022eevwconfigvideo   \"57:45\" \"(find-wconfig-lua-links)\")
         (find-2022eevwconfigvideo   \"59:17\" \"(find-wconfig-mpv-links)\")
         (find-2022eevwconfigvideo \"1:06:16\" \"(find-wconfig-exercises-links)\")

Note that I've included links to the parts of the video that
describe each one of the sub-wconfigs, and note that you will
only be able to use the `find-2022eevwconfigvideo' links _to play
local copies of the videos_ after configuring mpv... so it's
better to start by accessing this section in a browser, at:

  http://angg.twu.net/eev-intros/find-windows-beginner-intro.html#7




7.1. `ee-use-windows'
---------------------
(Obsolete, deleted)




7.2. Testing wget.exe
---------------------
(Obsolete, deleted)



7.3. Lua
--------
(Obsolete, deleted)




7.4. Downloading videos
-----------------------
The default behavior for a video link like this one

  (find-eevtestblsvideo \"2:33\" \"if I run f8 here\")

is to download a local copy of the video if it hasn't been
already downloaded, and then play the local copy with mpv. The
downloading is not automatic; the user has to \"psne\" it - see
the section 7.2 and this video:

  (find-eevvlinksvideo \"6:09\" \"if I execute this `find-eevtestblsvideo'\")
  (find-eevvlinksvideo \"6:15\" \"the last line says `Local file NOT found'\")

Now that you understand psne-ing, try the three standard
behaviors for video links. First make sure that we don't have a
local copy of the \"eevtestbls\" video, by running this:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
  rm -fv $S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4
  ls -l  $S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4

Then run these sexps:

  (ee-use-local-videos)
  (find-eevtestblsvideo \"2:33\" \"if I run f8 here\")

  (setq ee-find-eev-video-function 'find-eevlinks-video)
  (find-eevtestblsvideo \"2:33\" \"if I run f8 here\")

  (ee-use-youtube-videos)
  (find-eevtestblsvideo \"2:33\" \"if I run f8 here\")

The three behaviors are explained here:

  (find-video-links-intro \"7. `find-eev-video'\")
  (find-video-links-intro \"The default is `find-eevlocal-video', but\")



7.5. Downloading PDFs
---------------------
(Obsolete, deleted)




7.6. Saving your settings
-------------------------
(Obsolete, deleted)



7.7. Testing your settings
--------------------------
(Obsolete, deleted)



7.8. Saving your settings to your ~/.emacs
------------------------------------------
(Obsolete, deleted)



8. Summary
==========

[Unfinished!!!]




" pos-spec-list)))

;; (find-windows-beginner-intro)



;;;  _____                   _
;;; | ____|_  _____ _ __ ___(_)___  ___  ___
;;; |  _| \ \/ / _ \ '__/ __| / __|/ _ \/ __|
;;; | |___ >  <  __/ | | (__| \__ \  __/\__ \
;;; |_____/_/\_\___|_|  \___|_|___/\___||___/
;;;
;; «find-eev-exercises-intro»  (to ".find-eev-exercises-intro")
;; Skel: (find-intro-links "eev-exercises")
;; Test: (find-eev-exercises-intro)

(defun find-eev-exercises-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-eev-exercises-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-eev-exercises-intro)
Source code:  (find-efunction 'find-eev-exercises-intro)
More intros:  (find-eev-quick-intro)
              (find-here-links-intro)
              (find-refining-intro)
              (find-escripts-intro)
              (find-eev-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.
The quickest way to open or recreate this is with `M-x ex',
but you will need to run this:
  (defun ex () (interactive) (find-eev-exercises-intro))




This is part of the material the I prepared for this workshop:
  http://angg.twu.net/2021-workshop.html
The rest was moved to:
  (find-saving-links-intro)




0. Prerequisites
================
I will suppose:

  a. that you have Emacs 27 or 28 (pretest) installed,

  b. that you have watched the \"installation and navigation\"
     video and tried everything on it. Two points are especially
     important: first, you should start eev with

       M-x eev-beginner

     until you are no longer a beginner - `M-x eev-beginner'
     makes sure that all modules of eev were loaded. Second,
     `M-j' will be very important. The video explains it at:

     (find-eevnavvideo \"6:28\" \"M-j: you can forget practically everything...\")
     (find-eevnavvideo \"6:41\" \"if you type just M-j\")
     (find-eevnavvideo \"6:48\" \"has a header that is beginner-friendly\")

     The beginner-friendly header generated by `M-j' has changed
     since I recorded that video, and now it shows these three
     ways of opening the file where we will put our notes instead
     of showing just the first one:

           M-1 M-j  runs:  (find-fline \"~/TODO\")
       M-2 M-1 M-j  shows the file ~/TODO in the right window
       M-3 M-1 M-j  opens ~/TODO in the right window

     I will suppose that you have tried `M-21j' and `M-31j' and
     that you understood what they do.

  c. I will also suppose that you are running Emacs \"in a way in
     which the standard keys should work\". For example, Doom
     Emacs redefines many keys, including M-1, M-2, etc, that we
     will use a lot, so Doom Emacs is \"bad\". I have the
     impression that all the modes that make Emacs use vi-like
     keybindings also interfere with keys that we will use, so
     they're \"bad\" too. Besides that some window managers
     capture keys combinations like Alt-Shift-<letter>, so
     they're \"bad\"; if you use a window manager like that
     please install another one that captures few keys and learn
     how switch between your favorite (\"bad\") WM and the one
     that you will use in the workshop. Also, one person that
     attended the workshop that I gave in november was trying to
     use Emacs in a terminal on a Raspberry Pi 0... she said that
     many things didn't work but gave few details, so I'll
     considered that _for the purposes of this workshop_ terminal
     Emacs is \"bad\" and GUI Emacs is \"good\".

  d. In some parts of the workshop I will suppose that people can
     switch between an Emacs window and a browser window; in
     particular, I will suppose that they can follow links to
     videos, and for beginners this is much easier to do on a
     browser. For example, you can \"follow\" the link

       (find-eev2020video \"2:53\" \"pos-spec-lists\")

     using a browser by opening the HTMLized version of this
     intro using this URL,

       http://angg.twu.net/eev-intros/find-eev-exercises-intro.html

     locating the link to the video there, and clicking on the
     link in its timestamp.

  e. I will _sort of_ suppose that the people on the workshop
     have read these sections of two basic tutorials and
     have tried to do the exercises in them:

     (find-eev-quick-intro)
     (find-eev-quick-intro \"1. Installing eev\")
     (find-eev-quick-intro \"2. Evaluating Lisp\")
     (find-eev-quick-intro \"3. Elisp hyperlinks\")
     (find-eev-quick-intro \"4. Creating Elisp Hyperlinks\")
     (find-eev-quick-intro \"4.1. `find-here-links'\")
     (find-eev-quick-intro \"4.2. `find-ekey-links' and friends\")
     (find-eev-quick-intro \"5. Links to Emacs documentation\")
     (find-eev-quick-intro \"5.1. Navigating the Emacs manuals\")
     (find-eev-quick-intro \"5.2. Cutting and pasting\")
     (find-windows-beginner-intro)
     (find-windows-beginner-intro \"1. Download and install Emacs\")
     (find-windows-beginner-intro \"2. Key sequences and how to quit\")
     (find-windows-beginner-intro \"3. Using M-x and installing eev\")
     (find-windows-beginner-intro \"4. Understanding buffers and the mode line\")
     (find-refining-intro)
     (find-refining-intro \"1. Pos-spec-lists\")

The prerequisites are just the ones above. The previous workshop
that I gave required installing external programs, but this one
will not be like that. For more info on that previous workshop,
see:

  http://angg.twu.net/2021-workshop.html#november






" pos-spec-list)))

;; (find-eev-exercises-intro)




;;;  _    _       
;;; | | _| | __ _ 
;;; | |/ / |/ _` |
;;; |   <| | (_| |
;;; |_|\_\_|\__,_|
;;;               
;; «find-kla-intro»  (to ".find-kla-intro")
;; Skel: (find-intro-links "kla")
;; Test: (find-kla-intro)

(defun find-kla-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-kla-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-kla-intro)
Source code:  (find-efunction 'find-kla-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.


  See: http://angg.twu.net/emacsconf2022-kla.html
    (find-eev \"eev-kla.el\")
    `kla' means \"kill link to anchor\".
  The prerequisites for this tutorial are:
    (find-eev-quick-intro \"8. Anchors\")
    (find-eev-quick-intro \"9.1. `code-c-d'\")
    (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\")
    (find-eev-quick-intro \"9.2. Extra arguments to `code-c-d'\" \"to anchors\")
    (find-eev-quick-intro \"4. Creating Elisp Hyperlinks\")
    (find-eev-quick-intro \"4.1. `find-here-links'\")




1. Introduction
===============
In this section of the main tutorial

  (find-eev-quick-intro \"10.1. Generating short hyperlinks to files\")

we saw that after running these three `code-c-d's,

  (code-c-d \"foo\"  \"/tmp/FOO/\"          :anchor)
  (code-c-d \"bar\"  \"/tmp/FOO/BAR/\"      :anchor)
  (code-c-d \"plic\" \"/tmp/FOO/BAR/PLIC/\" :anchor)

all these links will point to the same file:

  (find-file  \"/tmp/FOO/BAR/PLIC/bletch\")
  (find-fline \"/tmp/FOO/BAR/PLIC/bletch\")
  (find-foofile        \"BAR/PLIC/bletch\")
  (find-barfile            \"PLIC/bletch\")
  (find-plicfile                \"bletch\")

If we visit the file \"/tmp/FOO/BAR/PLIC/bletch\" and we type
`M-h M-h' - i.e., `find-here-links' - there, we well get a
temporary buffer whose \"core\" will be those five hyperlinks
above... `find-here-links' doesn't know how to choose the
\"best\" hyperlink to \"/tmp/FOO/BAR/PLIC/bletch\", so it shows
all the options. Usually at that point we choose the best
hyperlink ourselves, we refine it with these keys,

  (find-emacs-keys-intro \"1. Basic keys (eev)\")
  (find-emacs-keys-intro \"1. Basic keys (eev)\" \"refining hyperlinks\")

and we copy it to our notes.

This takes many keystrokes. For example, suppose that we have
anchors in /tmp/FOO/BAR/PLIC/bletch, and we want create a
hyperlink to the anchor just before point and put that link in
the kill ring. One sequence of keystrokes that does that is:

  M-1 M-h M-w    ;; ee-copy-preceding-tag-to-kill-ring
      M-h M-h    ;; find-here-links
   8*<down>      ;; go to the line with the `(find-plicfile ...)'
      M-h M-2    ;; ee-duplicate-this-line
      M-h M-y    ;; ee-yank-pos-spec
      M-h M--    ;; ee-shrink-hyperlink-at-eol
      C-a        ;; move-beginning-of-line
      C-SPC      ;; set-mark-command
      <down>     ;; next-line
      M-w        ;; kill-ring-save
      M-k        ;; ee-kill-this-buffer

The file eev-kla.el implements a way to do that with fewer
keystrokes. If we are in /tmp/FOO/BAR/PLIC/bletch and we type
`M-x eekla' - or `M-x kla', if we have defined certain aliases -
then this will \"kill link to anchor\", which is a mnemonic for
\"create the best link to the anchor before point and put in the
kill ring\".

The algorithm that chooses the \"best link\" sometimes needs some
tinkering to work correctly, so here in this tutorial I will
explain in details how it works, and how to replace its pieces
when needed.

The old way of producing these hyperlinks - the one that uses the
sequence `M-1 M-h M-w ... M-k' - is explained here:

  (find-saving-links-intro)
  (find-saving-links-intro \"2.3. The base case 3\")




2. `ee-code-c-d-pairs'
======================
A call to

       (code-c-d \"foo\" \"/tmp/FOO/\" :anchor)

does a bit more than just running the code shown by this sexp:

  (find-code-c-d \"foo\" \"/tmp/FOO/\" :anchor)

It also runs this,

  (ee-code-c-d-add-pair \"foo\" \"/tmp/FOO/\")

that modifies the variable `ee-code-c-d-pairs' in two steps: it
first deletes all the elements of `ee-code-c-d-pairs' that are of
the form (\"foo\" ...), and then it adds the pair

  (\"foo\" \"/tmp/FOO/\")

to the front of the list. If you want to look at the code that
does that, it is here:

  (find-eev \"eev-code.el\" \"code-c-d\")
  (find-eev \"eev-code.el\" \"code-c-d-pairs\")

and you can inspect the variable `ee-code-c-d-pairs' with:

  (find-eppp ee-code-c-d-pairs)

We will refer to the elements of `ee-code-c-d-pairs' as `c-d's. A
`c-d' is a pair made of a `c' and a `d', where these `c' and `d'
were the arguments given to a `code-c-d'.



3. The components
=================
In order to convert a filename like

  \"/tmp/FOO/BAR/PLIC/bletch\"

to a sexp like

       (find-plicfile \"bletch\")
  or:  (find-plic     \"bletch\")

eev needs to:

  1. select all the `c-d's in `ee-code-c-d-pairs' whose `d's are
     initial substrings of \"/tmp/FOO/BAR/PLIC/bletch\",

  2. select the \"best one\" among these `c-d's; in our example
     it will be

       (\"plic\" \"/tmp/FOO/BAR/PLIC/\")

  3. remove the prefix \"/tmp/FOO/BAR/PLIC/\" from
     \"/tmp/FOO/BAR/PLIC/bletch\" to obtain \"bletch\"; we will
     refer to \"/tmp/FOO/BAR/PLIC/bletch\" as the `fname', and to
     the \"bletch\" as the \"rest\". We will abbreviate the
     \"rest\" as `r', and we will refer to the length of `r' as
     `l'. So in this case we have:

       /tmp/FOO/BAR/PLIC/bletch
       \\----------------/\\----/
               d           r
       \\----------------------/
                 fname

     and \"bletch\" has 6 characters, so `l' is 6.

  4. build the sexp. We will refer to its components as:

       (find-plicfile \"bletch\")
             \\--/      \\----/
              c        shortfname
        \\-----------/
         find-cfile

       (find-plic     \"bletch\")
             \\--/      \\----/
              c        shorterfname
        \\-------/
         find-c

     `shortfname' and `shortfname' will usually be equal to `r',
     but not always. The differences will be explained in section
     ___.



4. The best `l-r-c-d'
=====================
The algorithm that chooses the \"best\" `c-d' is here:

  (find-eev \"eev-kla.el\" \"best-lrcd\")

If `fname' is \"/tmp/FOO/BAR/PLIC/bletch\" and
`ee-code-c-d-pairs' is this list,

  ((\"plic\" \"/tmp/FOO/BAR/PLIC/\")
   (\"bar\"  \"/tmp/FOO/BAR/\")
   (\"foo\"  \"/tmp/FOO/\")
   (\"eev\"  \"/home/edrx/eev-current/\")
   (\"e\"    \"/usr/share/emacs/27.1/lisp/\"))

then the `c-d's in `ee-code-c-d-pairs' that \"match\" `fname', in
the sense their `d's are initial substrings of

           \"/tmp/FOO/BAR/PLIC/bletch\"

will be these ones:

  ((\"plic\" \"/tmp/FOO/BAR/PLIC/\")
   (\"bar\"  \"/tmp/FOO/BAR/\")
   (\"foo\"  \"/tmp/FOO/\"))

Try this:

  (find-eppp (ee-kl-lrcds \"/tmp/FOO/BAR/PLIC/bletch\"))

It will show something like this:

  ((6           \"bletch\" \"plic\" \"/tmp/FOO/BAR/PLIC/\")
   (11     \"PLIC/bletch\" \"bar\"  \"/tmp/FOO/BAR/\")
   (15 \"BAR/PLIC/bletch\" \"foo\"  \"/tmp/FOO/\"))

note that each `c-d' that matched `fname' was converted to an
`l-r-c-d'; the `r' is the \"rest\" that remains of `fname' after
the deleting the initial `d', and the `l' is the length of the
\"rest\".

This sexp

  (ee-kl-lrcds \"/tmp/FOO/BAR/PLIC/bletch\")

returns _all_ the `l-r-c-d's that match that filename; this sexp

  (ee-kl-lrcd  \"/tmp/FOO/BAR/PLIC/bletch\")

returns _the_ `l-r-c-d' that matches that filename - i.e., the
\"best\" `l-r-c-d' that matches that filename. The best one is
chosen by sorting the `l-r-c-d's by their `l's and then returning
the first `l-r-c-d' in the sorted list. In that example the best
`l-r-c-d' will be this one:

  (6 \"bletch\" \"plic\" \"/tmp/FOO/BAR/PLIC/\")
   
Note that its `r' is as short as possible. When there are no
`c-d's matching the filename the function `ee-kl-lrcd' returns
nil.




5. `cl-loop'
============
The functions that produce the best `l-r-c-d' are implemented
using `cl-loop'. I didn't explain `cl-loop' in

  (find-elisp-intro)

because it was too complex, but let's see it now. The features of
`cl-loop' that we will need are explained here:

  (find-clnode \"Loop Basics\")
  (find-clnode \"Accumulation Clauses\" \"collect FORM\")
  (find-clnode \"Accumulation Clauses\" \"append FORM\")
  (find-clnode \"For Clauses\" \"for VAR in LIST by FUNCTION\")
  (find-clnode \"For Clauses\" \"for VAR on LIST by FUNCTION\")
  (find-clnode \"For Clauses\" \"for VAR = EXPR1 then EXPR2\")
  (find-clnode \"For Clauses\" \"destructuring\")
  (find-clnode \"Other Clauses\" \"if CONDITION CLAUSE\")

Try to understand these examples:

  (cl-loop for x in '(1 2 3 4 5 6)
           collect (* 10 x))

  (cl-loop for sublist on '(a b c d e f)
           collect sublist)

  (cl-loop for sublist on '(a b c d e f) by 'cddr
           collect sublist)

  (cl-loop for (x y . rest) on '(a b c d e f) by 'cddr
           collect (list x y rest))

  (cl-loop for (x y) on '(a b c d e f) by 'cddr
           collect (list x y))

  (cl-loop for a in '(-3 -2 -1 0 1 2 3)
           for sq = (* a a)
           if (>= sq 4)
           collect (list a sq))

Note that this

  (cl-loop for a in '(1 2 3)
           for b in '(4 5 6)
           collect (list a b))

returns ((1 4) (2 5) (3 6)) - `cl-loop' runs the two `for's \"in
parallel\" instead of treating them as nested. This is explained
here:

  (find-clnode \"For Clauses\" \"several\" \"for\" \"clauses in a row\")

One way to make the `for's of the example above behave as nested
is by nesting `cl-loop's and using `append' in the outer one
instead of `collect', like this:

  (cl-loop for a in '(1 2 3)
           append (cl-loop for b in '(4 5 6)
                           collect (list a b)))




6. `cl-defun'
=============
Some functions in eev-kla.el are defined with `cl-defun' to make
them easy to test. If you execute this `cl-defun',

  (cl-defun foo
    (&key a
          (b 2)
          (c (* b 10)))
    (list a b c))

this defines a function that can be called with the arguments
`a', `b', and `c' given in any order. Try:

  (foo :a 4 :b 5 :c 6)
  (foo :c 70 :b 80 :a 90)

These \":keyword-value\" pairs can also be omitted. The

    (&key a
          (b 2)
          (c (* b 10)))

means:

  1. when there isn't a pair `:a value-for-a', then set a to nil,
  2. when there isn't a pair `:b value-for-b', then set b to 2,
  3. when there isn't a pair `:c value-for-c', then set 3 to the
     result of (* b 10).

Try:

  (foo               )
  (foo           :c 6)
  (foo      :b 5     )
  (foo      :b 5 :c 6)
  (foo :a 4          )
  (foo :a 4      :c 6)
  (foo :a 4 :b 5     )
  (foo :a 4 :b 5 :c 6)

The keyword arguments for `cl-defun' are explained here:

  (find-clnode \"Argument Lists\" \"cl-defun\")
  (find-clnode \"Argument Lists\" \"&key ((KEYWORD VAR) INITFORM SVAR)\")
  (find-clnode \"Argument Lists\" \"&key c d (e 17)\")

Some functions in eev-kla.el use a trick to make nil arguments be
ignored. For example, try:

  ;; «aaa»
  (ee-kl-sexp-klt)
  (ee-kl-sexp-klt :anchor nil)
  (ee-kl-sexp-klt :anchor \"bbb\")

The source code for `ee-kl-sexp-klt' is here:

  (find-eev \"eev-kla.el\" \"generate-sexps\")
  (find-eev \"eev-kla.el\" \"generate-sexps\" \"ee-kl-sexp-klt\")




7. The default `c', `d', and `r'
================================
The functions `ee-kl-c', `ee-kl-d', and `ee-kl-r' are defined here:

  (find-eev \"eev-kla.el\" \"ee-kl-r-c-d\")

If they receive a `fname' they convert it to an `l-r-c-d' using
the ideas in sections 3 and 4, and then they extract the `r', the
`c', and the `d' from the `l-r-c-d'. If they don't receive a
`fname' they use this as the default:

  (find-eev \"eev-kla.el\" \"default-args\")
  (find-eev \"eev-kla.el\" \"default-args\" \"(defun ee-kl-fname\")



8. The functions that generate sexps
====================================
Try these tests:

  (ee-kl-lrcd       :fname \"/tmp/FOO/BAR/PLIC/bletch\")
  (ee-kl-c          :fname \"/tmp/FOO/BAR/PLIC/bletch\")
  (ee-kl-r          :fname \"/tmp/FOO/BAR/PLIC/bletch\")
  (ee-kl-find-cfile :fname \"/tmp/FOO/BAR/PLIC/bletch\")
  (ee-kl-find-cfile :c \"plic\")
  (ee-kl-sexp-klf   :fname \"/tmp/FOO/BAR/PLIC/bletch\")




9. Symlinks
===========
See: (find-eev \"eev-kla.el\" \"ee-kl-expand\")



10. Aliases
===========

;; From: (find-kla-intro \"10. Aliases\")
(defalias 'kla  'eekla)
(defalias 'klas 'eeklas)
(defalias 'klf  'eeklf)
(defalias 'klfs 'eeklfs)
(defalias 'klt  'eeklt)
(defalias 'klts 'eeklts)
(defalias 'kli  'ee-kl-insert)
(defalias 'kla2 'eekla2)



  (find-eev \"eev-kla.el\" \"other-defaults\")

  (find-here-links-intro \"8. Debugging\")
  (find-eevfile \"eev-hlinks.el\" \"Debug mode\")


" pos-spec-list)))

;; (find-kla-intro)




;;;           _ _ _        _           _           
;;;   ___  __| (_) |_     (_)_ __   __| | _____  __
;;;  / _ \/ _` | | __|____| | '_ \ / _` |/ _ \ \/ /
;;; |  __/ (_| | | ||_____| | | | | (_| |  __/>  < 
;;;  \___|\__,_|_|\__|    |_|_| |_|\__,_|\___/_/\_\
;;;                                                
;; «find-edit-index-intro»  (to ".find-edit-index-intro")
;; Skel: (find-intro-links "edit-index")
;; Test: (find-edit-index-intro)

(defun find-edit-index-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-edit-index-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-edit-index-intro)
Source code:  (find-efunction 'find-edit-index-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



This is a tutorial for:
  (find-eev \"eev-hydras.el\")
that is a module of eev that is not loaded by default.
To test it you will need the package \"hydra\", from ELPA,
  https://github.com/abo-abo/hydra
and this `require':
  (require 'eev-hydras)
This will define `M-x ei' as an alias. See:
  (find-eev \"eev-hydras.el\" \"ei\")




0. Introduction
===============
These sections of the basic tutorial

  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\")

explain how to create \"index/section anchor pairs\", but they
say that the \"index\" line should be moved to the index at the
top of the file _by hand_. \"Moving the index line to the index\"
is a process that takes many steps, and even after years of
practice I would still commit mistakes very often when doing it.
Here we will see a way to perform it very quickly, with a
sequence of keys like this one:

  M-x ei RET kopli0q

The obvious way of implementing \"moving the index line to the
index\" would be by defining a function that would perform the
many steps involved in this process, and that would check for
many possible kinds of errors, and handle them correctly... but
that turned out to be too hard.

When we run `M-x ei', that is defined as an alias - see:

  (find-eev \"eev-hydras.el\" \"ei\")

it actives a hydra that interprets the letters in

  kopli0q

as these \"commands\":

     <k>ill line
     <o>ther window
     <p>revious anchor
  eva<l>
     <i>insert
     <0> `C-x 0'
     <q>uit

In my QWERTY keyboard I can type the letters in `kopli0' very
quickly with my right hand, as its keys are arranged like this:

          0
    i   o   p
      k   l

I can do that without taking my eyes off the screen - yes, I am a
VERY bad typist! But read this:

  (find-refining-intro \"5. Pointing to anchors\")
  (find-refining-intro \"5. Pointing to anchors\" \"but I don't touch-type\")

and if anything fails I can recognize it immediately, and I can
fix it with \"undo\"s in the right way. Also, after the <i> I
sometimes type some <,>s and <.>s to adjust the indentation; this
will be explained in the next section.




1. Testing the hydra
====================
In this tutorial \"the hydra\" will mean `ee-edit-index-hydra'.
Typing `M-x ei' will activate the hydra. When it is active you
will see a message in the echo area showing its main keys. `q'
will \"quit\" that hydra, and deactivate it. Some keys that the
hydra don't understand will also quit it.

The hydra understands the keys <up>, <down>, <,>, and <.>, among
others. It interprets <up> and <down> in the obvious way, and it
interprets <,> and <.> as commands that reindent a `(to ...)' by
adding or deleting tabs before the \"(\". Test this here,

  aaa\t(to \"bbb\")
  cccc\t\t(to \"dddd\")
  ee\t\t\t(to \"fffff\")

and use the hydra to align the three `(to ...)'s.




2. An index
===========
This is an index,

# «.foo»\t\t(to \"foo\")
# «.bar»\t\t(to \"bar\")

followed by two \"e-script blocks\" created with `M-B'. See:

  (find-eev-quick-intro \"8. Anchors\")
  (find-eev-quick-intro \"8. Anchors\" \"`M-B'\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\")

#####
#
# foo
# 2021may20
#
#####

# «foo»  (to \".foo\")


#####
#
# bar
# 2021may20
#
#####

# «bar»  (to \".bar\")



3. An exercise
==============
Here is an exercise.

  1. Use `M-B' to convert the line with \"plic\" below into an
     e-script block, as explained here:

       (find-eev-quick-intro \"8.4. Creating e-script blocks\")

     plic

  2. Put the point on the \"index line\" above - that is the one
     that looks like this,

       # <.plic>  (to \"plic\")

     but with green double angle brackets instead of \"<>\"s -
     and type this, and watch what happens in each step:

       C-l M-x ei RET kopli0q

     The actions associated to \"kopli0q\" are:

       <k>ill the current line
       switch to the sec<o>nd window
       move backwards to the line of the <p>revious anchor
       eva<l> the current line - that has a `to' pointing to the index
       <i>nsert, i.e., yank, the last kill after this line
       <0>: run `C-x 0' to delete the window that shows the index
       <q>uit the hydra.

   3. Do the same for the \"bletch\" below:

        bletch

      but now also use <,>s, <.>s, and <up>s and <down>s between
      the <i> and the <0> to align all the `(to ...)'s in the index.




4. `M-x ei' is fragile
======================
Look at the source code of the hydra that `M-x ei' calls:

  (find-eev \"eev-hydras.el\" \"ee-edit-index-hydra\")

Several of the actions are defined with `eek'. For example, the
action of \"o\" is:

  (\"o\" (eek \"C-x 1 C-x 3 C-x o\"))

This is considered fragile - because it won't work if any of
these key sequences has been redefined - and a bad programming
style. I could have used this instead:

  ;; (find-estring (format-kbd-macro (kbd \"C-x 1 C-x 3 C-x o\") 'verbose))
  (\"o\" (delete-other-windows)
         (split-window-right)
         (other-window 1))

but the \"fragile\" style above is usually good enough for quick
hacks - `M-x ei' is a quick hack -, and it gives the readers the
feeling that they can write their own hydras as quick hacks, too.

" pos-spec-list)))

;; (find-edit-index-intro)







;;;  _   _                                     _         _
;;; | |_| |__  _ __ ___  ___   _ __ ___   __ _(_)_ __   | | _____ _   _ ___
;;; | __| '_ \| '__/ _ \/ _ \ | '_ ` _ \ / _` | | '_ \  | |/ / _ \ | | / __|
;;; | |_| | | | | |  __/  __/ | | | | | | (_| | | | | | |   <  __/ |_| \__ \
;;;  \__|_| |_|_|  \___|\___| |_| |_| |_|\__,_|_|_| |_| |_|\_\___|\__, |___/
;;;                                                               |___/

;; «find-three-main-keys-intro»  (to ".find-three-main-keys-intro")
;; Skel: (find-intro-links "three-main-keys")
;; (find-three-main-keys-intro)

;; Used in this video:
;; http://angg.twu.net/index.html#eev-three-main-keys
;; A version in English in coming soon!

(defun find-three-main-keys-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-three-main-keys-intro)*"))
    (apply 'find-eintro "\



  The three basic keys of eev:
  A mini tutorial
    Eduardo Ochs
    http://angg.twu.net/#eev
    http://angg.twu.net/#eev-three-main-keys
    (Version: 2019aug09)

  Eev's central idea is that you can keep \"executable logs\" of
  what you do, in a format that is reasonably readable and that is
  easy to \"play back\" later, step by step and in any order.

  The is the second of the mini-tutorials.
  The first one was on a way to install eev \"for total beginners\"
  that creates a script called ~/eev that starts Emacs with eev-mode on
  and opens the main sandboxed tutorial of eev:

    (find-eev-quick-intro)

  The `(find-*-intro)'s are interactive tutorials.
  The mini-tutorials are videos.
  This mini-tutorial is about the basic ideas that you need to understand
  to run the interactive tutorials.
                                       ^
                                       |
                                  three keys!



  In Emacs terminology `C-e' is `control-e',
                   and `M-e' is `alt-e'.
          We pronounce `M-e' as `meta-e'.
  Some old keyboards had a \"meta key\".
  See: https://en.wikipedia.org/wiki/Meta_key

  Some keys in Emacs recognize numeric prefixes.
  For example, `M-4 M-2 a' inserts 42 `a's.

  Lisp is a programming language that uses:
    1. lots of lists
    2. lots of parentheses - they delimit the lists
    3. no \",\"s between the elements of these lists
    4. the name of a function at the beginning of the list,
       the arguments for the function after that.

  These lists without commas are called \"sexps\".
  See: https://en.wikipedia.org/wiki/Sexp

  These sexp are Lisp expressions (programs!):

       (* 2 3)
               (* 4 5)
    (+ (* 2 3) (* 4 5))

  they return these results:

       (* 2 3)            --> 6
               (* 4 5)    --> 20
    (+ (* 2 3) (* 4 5))   --> 26

  This is a sexp with \"side effects\":

    (insert \"HELLO\")


  SPOILER: The basic keys of eev are:

  M-e   - to follow an elisp hyperlink. (Elisp = Emacs Lisp)
          Mnemonic: \"(e)valuate\"/\"(e)xecute\".
  M-k   - to go back.  Mnemonic: \"(k)ill buffer\".
  M-j   - to (j)ump to certain predefined places. For example:

       M-2 M-j  runs:  (find-emacs-keys-intro)
       M-5 M-j  runs:  (find-eev-quick-intro)
           M-j  takes you to the list of jump targets.

  You just need to learn `M-e', `M-k' and `M-j' keys to navigate:
    1. the sandboxed tutorials for eev,
    2. the list of all most important keys,
    3. and the Emacs manuals!

  This mini-tutorial is BASED on these parts of the main tutorial:
    (find-eev-quick-intro)
    (find-eev-quick-intro \"2. Evaluating Lisp\")
    (find-eev-quick-intro \"3. Elisp hyperlinks\")
    (find-eev-quick-intro \"7.1. `eejump'\")
    (find-eev-quick-intro \"7.1. `eejump'\" \"numeric arguments\")





2. Evaluating Lisp
==================
When you type `M-e' emacs moves the point to the end of the
current line, then runs a variant of `C-x C-e'. Try this on each
line of the block below:

  (+ (* 2 3)
     (* 4 5)
     )

`M-e' accepts several different numeric prefixes that alter its
behavior. We are only interested in one of them now - `M-0 M-e'
highlights the sexp for a fraction of a second instead of executing it.
Try it above.

Also:

  (insert \"HELLO\")

\[In this video we will also use `M-2 M-e' and `M-3 M-e' - they
create two-window settings with the \"target\" of the sexp in the
right window... but the idea of the \"target\" of a sexp only
makes sense when that sexp behaves as a hyperlink...]



3. Elisp hyperlinks
===================
Each one of the sexps below makes Emacs \"go somewhere\" if you execute
it with `M-e'. Executing sexps like those - we will call them \"elisp
hyperlinks\" - is like following a hyperlink in a browser.

In a browser you can \"go back\" after following a hyperlink because the
previous page is kept in the memory somehow. In Emacs+eev the easiest
way to \"go back\" is with `M-k', which runs a function called
`ee-kill-this-buffer'. If you follow one of the links below with
`M-e', it creates a new buffer and displays it. If you then type `M-k'
this new buffer is killed, and Emacs displays the buffer that was just
below it, which is this tutorial... try it! Here are some nice elisp
hyperlinks:

  (find-file  \"~/eev-current/eev-flash.el\")
  (find-fline \"~/eev-current/eev-flash.el\")
  (find-fline \"~/eev-current/\")
  (find-fline \"/tmp/\")
  (find-efunctiondescr 'find-file)
  (find-man \"date\")
  (find-man \"sleep\")
  (find-sh  \"date; sleep 1; date\")
  (find-node \"(emacs)Lisp Eval\")
  (find-enode       \"Lisp Eval\")
  (find-efunction 'find-file)

Not all elisp hyperlinks \"go somewhere\"; some are like buttons that
perform an action, like the one below, that acts as if the user had
pressed a series of keys,

  (eek \"<down> C-a H E L L O ! <up> C-e\")

and some display their output in the echo area:

  (find-sh0 \"date\")


Note: `find-fline' is a contraction of `find-file-line'...
`find-fline' is better for hyperlinks than `find-file' because it
can be \"refined\" to point to a precise place in a file. See:

  (find-refining-intro \"2. Refining hyperlinks\")

See also:

  (find-enode \"Major Modes\")
  (find-enode \"Major Modes\" \"Dired\")
  (find-links-conv-intro \"4. Elisp hyperlinks: some conventions\")
  (find-links-conv-intro \"messy\")



7.1. `eejump'
-------------
Some key sequences in Emacs accept numeric arguments. For
example, try typing `M-9 a' (not `M-9 M-a'!) - this will insert 9
copies of the letter `a'. See:

  (find-enode \"Arguments\")

Eev binds the key `M-j' (`eejump') to a function that jumps to a
place that depends on the numeric argument. For example, `M-5
M-j' runs (find-eev-quick-intro), that reloads this intro and
goes to the top of it, and

  `M-2 M-j' runs: (find-emacs-keys-intro)
  `M-6 M-j' runs: (find-escripts-intro)
  `M-1 M-j' runs: (find-fline \"~/TODO\")



7.2. The list of eejump targets
-------------------------------
If you type `M-j' without a prefix argument then it runs
`(find-eejumps)', that displays a help text followed by all the
current eejump targets as defuns, one in each line. Try it:

  (eek \"M-j\")
  (find-eejumps)

You will see that two of those entries are:

  (defun eejump-1 () (find-fline \"~/TODO\"))
  (defun eejump-5 () (find-eev-quick-intro))

The help text starts with:

  ;; (find-eejumps)
  ;; See: (find-eev-quick-intro \"7.1. `eejump'\")
  ;;      (find-emacs-keys-intro \"1. Basic keys (eev)\")
  ;; For example,
  ;;     M-1 M-j  runs:  (find-fline \"~/TODO\")
  ;;     M-2 M-j  runs:  (find-emacs-keys-intro)
  ;;     M-5 M-j  runs:  (find-eev-quick-intro)
  ;; Current eejump targets:

So if your mind goes blank and you forget everything except for
`M-j' and `M-e' you can just type `M-j' and follow one of the
elisp hyperlinks in the help text.




What are the next steps?
========================
Next steps:

  1) Learn e-script blocks:

      (find-eev-quick-intro \"6.1. The main key: <F8>\")
      (find-eev-quick-intro \"6.2. Other targets\" \"display all\")

  2) Learn how to use index-anchor pairs:

      (find-eev-quick-intro \"8.1. Introduction: `to'\")

  3) Learn how to write your executable notes and put them here:

      `M-1 M-j' runs: (find-fline \"~/TODO\")

Etc, etc, etc...
Note that learning to read comes after learning to write!...
See:

  (find-here-links-intro \"1.1. Reading and writing\")



Btw, the videos that I produced using this \"script\" are here:
  http://angg.twu.net/#eev-three-main-keys
  Version in English:
    http://www.youtube.com/watch?v=s0_48wzWFbU
    http://angg.twu.net/eev-videos/three-keys-2.mp4
  Version in Portuguese:
    http://www.youtube.com/watch?v=GUuCpmLItTs
    http://angg.twu.net/eev-videos/three-keys-1-pt.mp4

" pos-spec-list)))

;; (find-three-main-keys-intro)




;;;   __ _           _            _ _                 _       _
;;;  / _(_)_ __   __| |       ___| (_)___ _ __       (_)_ __ | |_ _ __ ___
;;; | |_| | '_ \ / _` |_____ / _ \ | / __| '_ \ _____| | '_ \| __| '__/ _ \
;;; |  _| | | | | (_| |_____|  __/ | \__ \ |_) |_____| | | | | |_| | | (_) |
;;; |_| |_|_| |_|\__,_|      \___|_|_|___/ .__/      |_|_| |_|\__|_|  \___/
;;;                                      |_|
;;
;; «find-elisp-intro»  (to ".find-elisp-intro")
;; Skel: (find-intro-links "elisp")

(defun find-elisp-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-elisp-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-elisp-intro)
Source code:  (find-efunction 'find-elisp-intro)
More intros:  (find-eev-quick-intro)
              (find-emacs-keys-intro)
              (find-eev-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.
The quickest way to open or recreate this is with `M-7 M-j'.



This intro is a very quick introduction to Emacs Lisp. Its intent
is not to teach people how to _write_ Elisp code, only to teach
them how to _read_ Elisp code.

Different people prefer different kinds of tutorials.
Many people love the eintr, but I don't: (find-node \"(eintr)Top\")
This tutorial here is what I would have liked to have had access to
when I started learning Emacs Lisp.
The ideas behind the style of this tutorial are explained here:
  http://angg.twu.net/find-elisp-intro.html
  (find-1stclassvideo-links \"2022findelispintro\")

TODO: integrate this with these older intros:
  (find-eval-intro)
  (find-defun-intro)




1. Introduction
===============
The main tutorial for eev starts by explaining the syntax of Emacs
Lisp - \"Elisp\" from here onwards - using a simple example. To evaluate
this sexp

     (+ (* 2 3) (* 4 5))

Emacs evaluates first its subexpressions, (* 2 3) and (* 4 5), and
then it calls `+' with the results - i.e., it runs (+ 6 20). Try:

           2
             3
        (* 2 3)
                   4
                     5
                (* 4 5)
     (+ (* 2 3) (* 4 5))
  (list (* 2 3) (* 4 5))

In last sexp the function `list' receives two numbers, 6 and 20, but
returns a list, (6 20), not a number - and this is an introduction to
the idea that Elisp functions can receive all kinds of Elisp objects
and return all kinds of Elisp objects.

Elisp objects include:

  numbers, like 0, 4, -42, and 2.5,
  strings, like \"foo\" and \"bar\",
  symbols, like `*', `+' and `list', and
  lists, like (6 20), (* 2 3), and (+ (* 2 3) (* 4 5)).

The ones listed above are the only ones that we will treat in this
introduction. For more on the other types of list objects, see:

  (find-elnode \"Lisp Data Types\")
  (find-elnode \"Programming Types\")
  (find-elnode \"Editing Types\")





2. Lists and conses
===================
Lists are implemented using \"cons\"es. For details and lots of great
diagrams see these two articles in the Wikipedia:

  https://en.wikipedia.org/wiki/Cons
  https://en.wikipedia.org/wiki/Cons#Lists
  https://en.wikipedia.org/wiki/Cons#Trees
  https://en.wikipedia.org/wiki/S-expression

The last one explains in detail what are \"sexp\"s.

The function `cons' \"constructs\" a bigger list by prepending an
element to it:

           (list 22 33)
  (cons 11 (list 22 33))
        (list 11 22 33)

The functions `car' and `cdr' both receive a list and return the
components of its first cons. The `car' of a list is its first
element, and the `cdr' of the list is the \"rest\" of the list. Try:

             (list 11 22 33)
       (cons 11 (list 22 33))
  (car (cons 11 (list 22 33)))
  (cdr (cons 11 (list 22 33)))

        (car (list 11 22 33))
                              (cdr (list 11 22 33))
  (cons (car (list 11 22 33)) (cdr (list 11 22 33)))




3. `quote'
==========
Not all functions evaluate their arguments. For example, `quote'
expects a single argument and returns it unchanged, without evaluating
it. Try:

            (* 2 3)
         (+ (* 2 3) (* 4 5))
  (quote    (* 2 3))
  (quote (+ (* 2 3) (* 4 5)))

Elisp has an abbreviation for `quote' - a single \"'\". Try:

         (quote (* 2 3))
               '(* 2 3)

  (quote       '(* 2 3))
  (quote (quote (* 2 3)))

Elisp has several other functions besides `quote' that do not evaluate
their arguments. These functions are called \"special forms\" and we
will see more about them in section 5, 6, 8, and 9.

  (find-elnode \"Special Forms\")




4. Symbols
==========
Note that symbols and strings are different:

               (quote *)
                     '*
                     \"*\"
  (symbol-name (quote *))
  (symbol-name       '*)

Numbers and strings \"evaluate to themselves\". Try:

           22
          \"22\"
        (+ 22   33)
  (concat \"22\" \"33\")

If you need more evidence that 22 and \"22\" are different run the sexps
below - they give errors! Numbers can be added but not concatened, and
strings can be concatenated but not added.

      (+ \"22\" \"33\")
  (concat 22   33)




5. Variables
============
When we evaluate a list like (* 2 3) the symbol in the beginning of
the list, `*', is interpreted as the name of a function... but when we
evaluate a symbol by itself it is interpreted as the name of a
variable, and it returns the value of that variable. Try:

  (set 'a 2)
        a
  (* 10 a)

  (set 'a 3)
        a
  (* 10 a)

We will often write examples like the one above more compactly, like
this:

  (set 'a 2)
  (set 'a 3)
        a
  (* 10 a)

If you execute lines 1, 3, and 4 in it you get one behavior, and
if you execute lines 2, 3, and 4 you get the other one. There are
some exercises on \"choosing the right order\" here:

  (find-eval-intro \"3. What to execute, and in what order\")

Again: note that evaluating the symbol `a' returns the value of a \"as
a variable\".

The function `setq' works like `set' but it doesn't expect us to quote
explicitly its first argument. Try:

  (set 'a 2)
  (setq a 2)
  (set 'a (* 2 3))
  (setq a (* 2 3))
        a
  (* 10 a)

`setq' is a special form - it doesn't evaluate its first argument but
evaluates the second one. See:

  (find-elnode \"Setting Variables\" \"Special Form: setq\")




6. Defining functions
=====================
Each symbol has a value \"as a variable\" and a value \"as a function\",
and these two values are independent from one another. We can define
functions with `defun'. Try:

  (defun foo (a) (* 10  a))
  (defun foo (a) (* 100 a))
        (foo  2)
        (foo  3)

   (setq foo 2)
   (setq foo 3)
    (foo foo)

                    foo
     (symbol-value 'foo)
  (symbol-function 'foo)

Function definitions are stored as lists starting with the symbol
`lambda' (\"lambda expressions\"). If you are a beginner you should
treat the details of this as a curiosity - but try the example below:

  (defun foo         (a) (* 10  a))
  (defun foo         (a) (* 100 a))
  (fset 'foo (lambda (a) (* 10  a)))
  (fset 'foo (lambda (a) (* 100 a)))

  (symbol-function 'foo)
                   (foo foo)

For more information, see:

  (find-elnode \"Lambda Expressions\")
  (find-elnode \"Defining Functions\" \"defun name args\")
  (find-elnode \"Function Names\")
  (find-elnode \"Symbol Components\")
  (find-elnode \"Function Cells\")
  (find-elnode \"Function Cells\" \"symbol-function symbol\")
  (find-elnode \"Function Cells\" \"fset symbol definition\")

Not all functions are represented as lambda expressions. The functions
of Emacs that are implemented in C are represented as \"subr\"s, that
are one of the kinds of objects that I said that I wouldn't discuss in
this introduction. See:

  (symbol-function '+)
  (find-elnode \"Programming Types\" \"Primitive Function Type\")
  (find-elnode \"Primitive Function Type\" \"subrs\")

Also, functions can be byte-compiled (see section 11), and they
can expect and receive any number of arguments. See:

  (find-elnode \"Defining Functions\" \"defun bar (a &optional b &rest c)\")

Try:

  (defun bar (a b &optional c d &rest e) (list a b c d e))
        (bar 1 2 3 4 5 6)
        (bar 1 2 3 4 5)
        (bar 1 2 3 4)
        (bar 1 2 3)
        (bar 1 2)
        (bar 1)




7. `read' and `eval'
====================
We've been evaluating all sexps with `M-e'... but try:

                   '(* 2 3)
             (eval '(* 2 3))

           (setq a '(* 2 3))
           (setq a '(* 4 5))
                 a
           (eval a)

                   \"(* 2 3)\"
             (read \"(* 2 3)\")
       (eval (read \"(* 2 3)\"))

It turns out the `M-e' first extracts a sexp from the current buffer
as a string, then `read' converts that string to a Lisp object -
usually a list or symbol -, and `eval' evaluates that list or symbol.
There is a also a function `prin1-to-string' that does the opposite of
`read', as in the diagram below:

                     read
   \"(* 2 3)\" ----------------->  (* 2 3)
             <-----------------      |
               prin1-to-string       |
                                     |
                                     v
                                     6

Try:

                    (*  2  3)
                   '(*  2  3)
  (prin1-to-string '(*  2  3))

So: programs and functions can be represented as lists, and also as
strings that be converted into lists by `read'; they can be inspected,
edited and modified on-the-fly; and we can write Lisp code that
generates and evaluates more Lisp code. Eev does this in a handful of
situations - for example:

  (find-eev-quick-intro \"find-code-c-d\")
  (find-here-links-intro \"4. `find-here-links-3'\")
  (find-eev-quick-intro \"4.2. `find-ekey-links' and friends\")

The idea that programs are lists is far more powerful than it
looks at first sight. See, for example:

  https://sep.yimg.com/ty/cdn/paulgraham/bbnexcerpts.txt
  http://www.paulgraham.com/diff.html
  http://www.paulgraham.com/lisp.html
  (find-elnode \"Simple Macro\")

Some functions in eev expect code that is meant to be `eval'-ed
later. For example:

  (find-multiwindow-intro \"2. `find-wset'\")
  (find-multiwindow-intro \"2. `find-wset'\" \"(find-wset\" \"13o_2o_o\")
  (find-wset \"13o_2o_o\" '(find-ebuffer \"B\") '(find-ebuffer \"C\"))

The functions in the `code-c-d' family produce code that is first
`read' and then `eval'-ed. See:

  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-code-c-d \"CODE\" \"/DIR/\" :info \"INFO\")





8. `let' and `let*'
===================
See:

  (find-elnode \"Local Variables\" \"Special Form: let \")
  (find-elnode \"Local Variables\" \"Special Form: let* \")

Try:

                      (setq y 2)
                            y
  (let  ((y 1))             y)
  (let  ( y   )             y)
  (let  ((y 1) (z y)) (list y z))
  (let* ((y 1) (z y)) (list y z))





9. More on strings
==================
Strings in Elisp can span several lines and can contain many
kinds of backslashed \"escape sequences\", like \"\\n\". Elisp
also has a syntax for characters, that starts with a \"?\" and
supports the same escape sequences as strings. See:

  (find-elnode \"Basic Char Syntax\")
  (find-elnode \"Character Type\")

Remember - from

  (find-eev-quick-intro \"2. Evaluating Lisp\" \"`M-0 M-e'\")

that `M-0 M-e' and `M-0 M-E' highlight the sexp before point...
you can use that to check the extent of a string whose
backslashes are hard to interpret visually. For example:

  \"\\
  \\\\\\\"\\\\\\\\\"






10. Backquote
=============
The backquote, \"`\", works like the quote, \"'\", but when a
backquoted list is evaluated it is processed recursively and its
components preceded by \",\" or \",@\" are evaluated and replaced
by their values. The details are quite tricky - try these
examples:

  `(foo     ,(+ 2 3) bar)
  `(foo    ,'(+ 2 3) bar)
  `(foo  ,(list 2 3) bar)
  `(foo ,@(list 2 3) bar)

and see:

  (find-elnode \"Backquote\")

The functions in eev that use `find-elinks' typically use
backquotes a lot. See:

  (find-links-conv-intro \"3. Classification\")
  (find-eev \"eev-elinks.el\" \"find-elinks\")
  (find-eev \"eev-elinks.el\" \"find-efunction-links\")

They are the hardest ones to read in the eev source.




11. Byte-compiled functions
===========================
Most functions in Emacs are byte-compiled - which means that
their function cells contain a \"byte-code\" instead of a lambda
expression. These byte-codes are very hard for humans to read.
See:

  (find-elnode \"What Is a Function\" \"byte-code function\")
  (find-elnode \"Byte-Code Type\")
  (find-elnode \"Byte Compilation\")
  (find-elnode \"Disassembly\")

Here is an example:

  (find-efunctiondescr 'find-file)
  (find-efunction      'find-file)
  (symbol-function     'find-file)
  (find-efunctionpp    'find-file)
  (find-efunctiond     'find-file)

The `find-efunctionpp' link above takes the content of the
function cell of `find-file' and \"pretty-prints\" it, i.e.,
indents it in a nice way, but the result in this case is
unreadable... and the `find-efunctiond' link shows a decompiled
version of that byte-code, which is only slightly better. Both
the `find-efunctionpp' and the `find-efunctiond' links show
internal representations that are very different from the source
code. Compare that with a case in which the function is not
byte-compiled:

  (find-efunctiondescr 'find-fline)
  (find-efunction      'find-fline)
  (symbol-function     'find-fline)
  (find-efunctionpp    'find-fline)

The `(find-efunctionpp 'find-fline)' shows a lambda expression
that is very similar to the defun that defined `find-fline'.





11.1. Why eev avoids byte-compilation
-------------------------------------
All the source files of eev have a \"no-byte-compile: t\" in
them. See:

  (find-eevgrep \"grep --color -nH -e no-byte-compile: *.el\")
  (find-elnode \"Byte Compilation\" \"no-byte-compile: t\")
  (find-enode \"Specifying File Variables\")

This `no-byte-compile: t' is non-standard, but it is a deliberate
design choice. I tried to make eev as beginner-friendly as
possible, and beginners find byte-compiled functions confusing,
so I avoided them as much as possible. Remember that several
functions in eev define other functions - for example:

  (find-eev-quick-intro \"9.1. `code-c-d'\")
  (find-eev-quick-intro \"9.1. `code-c-d'\" \"mass-produced\")
  (find-eev-quick-intro \"9.1. `code-c-d'\" \"find-code-c-d\")

and if you try to understand what a hyperlink function like one
below does by typing `M-h M-f' on it,

  (find-efile \"subr.el\")

then the `find-efunction' link in the `M-h M-f' buffer will not
work - but the `find-efunctionpp' link will. Try:

  (find-efunction   'find-efile)
  (find-efunctionpp 'find-efile)





11.2. How `find-efunction' works
--------------------------------
Eev defines hyperlink functions called `find-efunction',
`find-evariable' and `find-eface' that are wrappers around the
standard Emacs functions `find-function', `find-variable' and
`find-face-definition'; the eev variants support pos-spec-lists.
Try:

  (find-efunction 'find-fline)
  (find-function  'find-fline)
  (find-evariable 'ee-hyperlink-prefix)
  (find-variable  'ee-hyperlink-prefix)
  (find-eface           'eepitch-star-face)
  (find-face-definition 'eepitch-star-face)

The Emacs functions are defined here:

  (find-efile \"emacs-lisp/find-func.el\")

and their inner workings are quite complex. To begin with, hey
use `symbol-file', that works on the variable `load-history'.
Here are some links to documentation and tests:

  (find-efunctiondescr 'symbol-file)
  (find-elnode \"Where Defined\")
  (symbol-file 'find-fline          'defun)
  (symbol-file 'find-efile          'defun)
  (symbol-file 'ee-hyperlink-prefix 'defvar)
  (symbol-file 'eepitch-star-face   'defface)
  (find-eloadhistory \"eepitch\")
  (find-eloadhistory \"eepitch\" \"eepitch-star-face\")

The functions in \"find-func.el\" use `symbol-file' to find the
file where a given symbol was defined, and then search for a
defun, defvar of defface in it that _looks like_ the definition
that we are looking for.

Emacs knows that `find-efile' was defined in \"eev-code.el\",
because of:

  (symbol-file 'find-efile          'defun)
  (find-eloadhistory \"eev-code\" \"find-efile\")

but if we run

  (find-function 'find-efile)

this will fail, because Emacs will look for something like
\"(defun find-efile \" in \"eev-code.el\", and it will not find
it; it doesn't know that `find-efile' was defined by this
`code-c-d':

  (find-eev \"eev-code.el\" \"code-c-d-s\")
  (find-eev \"eev-code.el\" \"code-c-d-s\" \"\\\"e\\\"\")

Let's be even more precise. \"find-func.el\" defines these
low-level functions,

  (find-efunctiondescr 'find-function-noselect)
  (find-efunctiondescr 'find-variable-noselect)
  (find-efunctiondescr 'find-definition-noselect)

that return structures of the form (BUFFER . POS), and eev
defines a function

  (find-efunctiondescr 'find-ebufferandpos)
  (find-efunction      'find-ebufferandpos)

that jumps to a (BUFFER . POS); `find-efunction' and friends are
implemented using `find-ebufferandpos'. Try:

  (find-ebufferandpos
   (find-function-noselect 'find-fline)
   )
  (find-ebufferandpos
   (find-variable-noselect 'ee-hyperlink-prefix)
   )
  (find-ebufferandpos
   (find-definition-noselect 'eepitch-star-face 'defface)
   )

These `find-*-noselect' functions work quite well but are not
100% reliable - for example, if an elisp file has several
definitions for the same function, variable, or face, the
`find-*-noselect's don't know which ones were executed, neither
which one was executed last, overriding the other ones... and it
may return the position of a defun, defvar, or defface that is
not the \"active\" one. In eev redefinitions like these ones

  (code-pdf-page \"foomanual\" \"/usr/src/foo-1.2.3/manual.pdf\")
  (code-pdf-page \"foomanual\" \"/usr/src/foo-1.2.4/manual.pdf\")

are quite common, and

  (find-efunctionpp 'find-foomanualpage)

will give you information about the current definition.




12. Some advanced topics
========================
See: (find-lexical-intro)



" pos-spec-list)))

;; (find-elisp-intro)





;;;  _           _           _
;;; | | _____  _(_) ___ __ _| |
;;; | |/ _ \ \/ / |/ __/ _` | |
;;; | |  __/>  <| | (_| (_| | |
;;; |_|\___/_/\_\_|\___\__,_|_|
;;;
;; «find-lexical-intro»  (to ".find-lexical-intro")
;; Skel: (find-intro-links "lexical")

(defun find-lexical-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-lexical-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-lexical-intro)
Source code:  (find-efunction 'find-lexical-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-elisp-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.


This will become a _tutorial_ on lexical binding in the future,
and it will be a complement to this elisp tutorial:
  (find-elisp-intro)
but at this moment I know far less about lexical binding that I
should, and this is just an embarassingly small collection of
links, tests, and examples...

See:
  (find-eevfile \"eev-template0.el\" \"INCOMPATIBLE WITH LEXICAL BINDING\")
  (find-eev     \"eev-template0.el\" \"lexical-binding\")




0. How to use this
==================
Usually people select between lexical binding and dynamic binding
by putting their functions that use lexical binding in files with
a \"-*- lexical-binding:t -*-\" in their first line, like this:

  (find-efile \"play/tetris.el\" \"lexical-binding:t\")

See:

  (find-elnode \"Using Lexical Binding\")
  (find-elnode \"Using Lexical Binding\" \"first line\")

Here we will do something much more low-level. `(eval CODE)'
and `(eval CODE nil)' both eval CODE using dynamic binding,
but `(eval CODE t)' and `(eval CODE 'lexical)' eval CODE using
lexical binding. In dynamic binding all `defun's generate
lambdas, but in lexical binding some `defun's generate closures.
You can test this by running the four sexps below with `M-e' -
the `(eval ... 'lexical)' stores a closure in the function cell
of `foo':

  (eval '(let ((x 42)) (defun foo () (* x x))) nil)
  (symbol-function 'foo)

  (eval '(let ((x 42)) (defun foo () (* x x))) 'lexical)
  (symbol-function 'foo)

We will also use another technique, based on `M-e', for making
code eval-able in both dynamic binding mode and lexical binding
mode.

Let me list some facts...

  1) dynamic binding is still the current default, but it is
     being phased out and may be declared obsolete in a few years
     - AND EVEN KILLED COMPLETELY, despite the complaints of the
     many people who love it, like me;

  2) `M-e' goes to the end of the line, reads the sexp before
     point, and `eval's it;

  3) `M-e' _sort of_ emulates `C-e C-x C-e', but `C-x C-e'
     doesn't run a plain `eval' - it runs a function based on
     `eval' that has lots of bells and whistles, and that is too
     complex for my tiny brain. Take a look at its code:

        (eek \"M-h M-k  C-x C-e  ;; eval-last-sexp\")
        (find-efunctiondescr 'eval-last-sexp)
        (find-efunction      'eval-last-sexp)

  4) `C-x C-e' uses the buffer-local variable `lexical-binding'
     to decide whether it should use dynamic or lexical binding,
     while `M-e' does not. Try to eval the sexps below with a
     series or `C-e C-x C-e's:

       (setq-local lexical-binding t)
       (let ((x 42)) (defun foo () (* x x)))
       (symbol-function 'foo)

       (setq-local lexical-binding nil)
       (let ((x 42)) (defun foo () (* x x)))
       (symbol-function 'foo)

     You will see that the first block generates a closure and
     the second does not. If you try to execute those six sexps
     with `M-e's you'll get lambdas in both blocks.

  5) I chose to use a simple `eval' without the second argument
     in the plain `M-e' because: a) it is simpler to understand,
     b) it is simpler to explain to beginners, and b) people can
     change it by redefining this function:

       (find-eev \"eev-eval.el\" \"arg-variants\" \"ee-eval-last-sexp-default\")

  6) `M-e' supports numeric prefixes that select alternate
     actions - for example, `M-0 M-e' highlights the sexp instead
     of executing it - and it's easy to add new alternate
     actions. See:

       (find-eev \"eev-eval.el\" \"ee-eval-last-sexp\")
       (find-eev \"eev-eval.el\" \"arg-variants\")

  7) `M-1 M-1 M-e' (or: `M-11e') uses `(eval ... 'lexical)'
     instead of the plain `eval'. See:

       (find-efunction 'ee-eval-last-sexp-default)
       (find-efunction 'ee-eval-last-sexp-11)
       (find-efunction 'ee-eval-lexical)

     Try to execute the 4-line sexp below with both `M-e' and
     `M-11e':

        (let ((x 42))
          (defun foo () (* x x))
          (symbol-function 'foo)
          )

In the rest of this tutorial I will suppose that the reader knows
how to use `M-e' to eval sexps in the \"old\" dynamic binding
mode and `M-11e' to eval sexps in new lexical binding mode, and
knows how to run code blocks in BOTH dynamic and lexical binding
modes to compare the results.



(The rest of this tutorial is just a first draft)




1. `lambda' and `function'
==========================
See: (find-elnode \"Anonymous Functions\" \"Macro: lambda\")
     (find-elnode \"Anonymous Functions\" \"Special Form: function\")
     (find-elnode \"Anonymous Functions\" \"Special Form: function\" \"converted\")

  (let ((x 42))
    (lambda () x)
    )

  (let ((x 42))
    (function
      (lambda () x)
      )
    )



2. How closures work
====================
See: (find-elnode \"Dynamic Binding\" \"defun getx\")
     (find-elnode \"Lexical Binding\" \"defun getx\")
     (find-elnode \"Void Variables\")

  (let ((x 20))
    (defun getx () x)
    )

  (makunbound 'x)
  (setq x 99)
  (getx)
  (let ((x 42)) (getx))




3. `get/set'
============
;; These fsets work as defuns.
;; See: (find-elnode \"Function Cells\")

(fset 'foo (lambda () 20))
(fset 'foo (lambda () 42))
(foo)

;; This is the only sexp in this block that needs
;; to be run in lexical binding mode.
;;
(defun get/set0 ()
  \"Return a list with a `getter' closure and a `setter' closure.
The getter and the setter share the same lexical environment -
which means that they operate on the same `x'.
Different calls to this function generate getters and setters
with independent lexical environments - which means that they
operate on independent `x's.
This defun needs to be executed in lexical binding mode.\"
  (let* ((x nil))
    (list (lambda () x)
          (lambda (newvalue) (setq x newvalue)))))

(defun get/set (getter setter)
  \"Define a SETTER and a GETTER that operate on the same variable.
SETTER and GETTER are symbols: names of functions. The variable
lives in a lexical environment that is shared by both the GETTER
and the SETTER. Each call to this function generates a different
lexical environment.\"
  (let ((gs (get/set0)))
    (fset getter (nth 0 gs))
    (fset setter (nth 1 gs))))

;; Check that geta/seta and getb/setb operate on different
;; variables:
;;
(get/set 'geta 'seta)
(get/set 'getb 'setb)
(symbol-function 'geta)
(symbol-function 'getb)
(symbol-function 'seta)
(symbol-function 'setb)
(seta 20)
(setb 42)
(geta)
(getb)


;; Check that geta/seta use the same lexical environment
;; and that getb/setb use a second lexical environment.
;; See: (find-elnode \"Closures\")

          (symbol-function 'seta)
     (cdr (symbol-function 'seta))
(car (cdr (symbol-function 'seta)))

(eq
  (cadr (symbol-function 'geta))
  (cadr (symbol-function 'seta))
  )

(eq
  (cadr (symbol-function 'getb))
  (cadr (symbol-function 'setb))
  )



4. Alpha-conversion
===================
;; In lexical binding mode alpha-conversion works:
;; https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion
;; Try:

(setq b 3)
(defun getb () b)

(list
  (let ((a 2)) (list a (getb)))
  (let ((b 2)) (list b (getb)))
  )

;; In lexical binding mode the 4-line `list' sexp returns a list
;; of two identical lists. These two `let' sexps are equivalent:
;;
;;   (let ((a 2)) (list a (getb)))
;;   (let ((b 2)) (list b (getb)))
;;
;; and the choice of `a' or `b' for the name of the variable
;; doesn't matter. In dynamic binding mode the `b' of the
;; `(let ((b ...)) ...)' is seen by the (getb), and it shadows
;; the global `b'.




5. A thread
===========
In aug/2021 I sent an e-mail to the help-gnu-emacs mailing list
asking for help to write this tutorial; its title was \"Lexical
vs. dynamic: small examples?\". Few people sent small examples,
but some of the messages in the thread were fantastically good.
Here is a link to the thread itself, to two of my posts in it,
and to the messages that I considered \"fantastically good\"...

  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/threads.html#00283
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00283.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00294.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00314.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00342.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00344.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00345.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00352.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00355.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00350.html
  https://lists.gnu.org/archive/html/help-gnu-emacs/2021-08/msg00341.html

And here are (elisp hyper)links to them with descriptions:

(defun ee-dynlex-url (nnnnn)
  (format \"https://lists.gnu.org/archive/html/help-gnu-emacs/%s/msg%s.html\"
    \"2021-08\" nnnnn))

(defun find-dynlexpost (nnnnn &rest ignored)
  (find-eww (ee-dynlex-url nnnnn)))

(find-dynlexpost \"00283\" \"Edrx\"   \"initial post\")
(find-dynlexpost \"00294\" \"Edrx\"   \"primary source\")
(find-dynlexpost \"00314\" \"Drew\"   \"for the dynamic extent of the function call\")
(find-dynlexpost \"00342\" \"Drew\"   \"binds special vars dynamically and\")
(find-dynlexpost \"00344\" \"Stefan\" \"same as in code analysis\")
(find-dynlexpost \"00345\" \"Drew\"   \"by pretty much all of the wizards of Lisp\")
(find-dynlexpost \"00352\" \"Drew\"   \"dynamically binds a lambda to a name\")
(find-dynlexpost \"00355\" \"Drew\"   \"dyna-show.el\")
(find-dynlexpost \"00355\" \"Stefan\" \"lexical-let and dlet\")
(find-dynlexpost \"00341\" \"Drew\"   \"scope and extent are different things\")

" pos-spec-list)))

;; (find-lexical-intro)





(provide 'eev-intro)



;; Local Variables:
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
