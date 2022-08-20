;; eepitch.el - record interactions with shells as readable notes, redo tasks.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012,2015,2018-2022 Free Software Foundation, Inc.
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
;; Version:    20220801
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eepitch.el>
;;       htmlized: <http://angg.twu.net/eev-current/eepitch.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-eepitch-intro.html>
;;                                                (find-eev-quick-intro)
;;                                                (find-eepitch-intro)


;; «.eepitch»			(to "eepitch")
;; «.eepitch-this-line»		(to "eepitch-this-line")
;; «.eepitch-shell»		(to "eepitch-shell")
;; «.eepitch-kill»		(to "eepitch-kill")
;; «.ee-expand»			(to "ee-expand")
;; «.ee-split-and-expand»	(to "ee-split-and-expand")
;; «.find-comintprocess»	(to "find-comintprocess")
;;
;; «.eewrap-eepitch»		(to "eewrap-eepitch")
;; «.ee-adjust-red-stars»	(to "ee-adjust-red-stars")
;; «.glyphs»			(to "glyphs")
;; «.set-glyphs»		(to "set-glyphs")
;; «.set-keys»			(to "set-keys")
;;
;; «.other-terms»		(to "other-terms")
;;   «.eepitch-ansiterm»	(to "eepitch-ansiterm")
;;   «.eepitch-vterm»		(to "eepitch-vterm")
;;   «.eepitch-sly»		(to "eepitch-sly")
;;
;; «.eepitch-langs»		(to "eepitch-langs")

;;; Commentary:

;; Eev's central idea is that you can keep "executable logs" of what
;; you do, in a format that is reasonably readable and that is easy to
;; "play back" later, step by step and in any order. These "steps" are
;; mainly of two kinds:
;;
;;   1) elisp hyperlinks, and
;;   2) lines sent to shell-like programs.
;;
;; Eepitch is the module that implements support for (2). For
;; tutorials, see:
;;
;;   (find-eev-quick-intro "6. Controlling shell-like programs")
;;   (find-eepitch-intro)
;;   http://angg.twu.net/eev-intros/find-eev-quick-intro.html#6
;;   http://angg.twu.net/eev-intros/find-eepitch-intro.html
;;
;; For videos, see:
;;
;;   https://www.youtube.com/watch?v=Lj_zKC5BR64&t=16s
;;   http://angg.twu.net/#eev
;;
;;
;;
;; NOTE: some Emacs modes have ways to send lines to an interpreter;
;; for example, `python-mode' has `python-shell-send-string':
;;
;;   (find-efile "progmodes/python.el" "(defvar python-mode-map")
;;   (find-efile "progmodes/python.el"       "'python-shell-send-string")
;;   (find-efile "progmodes/python.el" "(defun python-shell-send-string")
;;
;; Eepitch reimplements that in a minimalistic way that is quite
;; flexible. There is a package called isend-mode - here:
;;
;;   https://github.com/ffevotte/isend-mode.el
;;   (find-epackage 'isend-mode)
;;
;; that also generalizes this "send lines to an interpreter" thing.
;; TODO: integrate eepitch and isend-mode!
;;
;; See also the `run-<lang>' functions:
;;
;;   (find-eapropos "^run-*")
;;   (find-eapropos "^inferior-*")
;;   (find-enode "External Lisp" "run-lisp")
;;   (find-enode "Executing Lisp" "Inferior Lisp mode")
;;   (find-enode "Executing Lisp" "Inferior Scheme mode")
;;   (find-elnode "Example Major Modes" "inferior Lisp job")
;;
;;
;;
;; NOTE 2: eepitch is based on code that Rubikitch sent to the eev
;; mailing list in 2006, but I rewrote his code completely several
;; times since then. See:
;;
;;   http://lists.gnu.org/archive/html/eev/2006-01/msg00000.html
;;   http://lists.gnu.org/archive/html/eev/2006-02/msg00000.html
;;   http://lists.gnu.org/archive/html/eev/2006-02/msg00001.html



;; The comments below are VERY old and were partly superseded by this:
;;   (find-eev-quick-intro "6. Controlling shell-like programs")
;;
;; The innards
;; ===========
;; In order to understand precisely how eepitch works (consider this a
;; preliminary hacker's guide!), let's make some definitions and
;; follow a low-level example. I will suppose that you have read
;; enough of
;;   <http://angg.twu.net/eev-current/eepitch.readme.html>
;; to understand how to use eepitch in the most basic cases.
;;
;; Some sexps, like `(shell)', always switch to a buffer with a
;; certain name when executed, and they create a buffer with that name
;; when it does not exist. We call that name the "target buffer name"
;; of the sexp, and, by convention, the cases in which the sexp raises
;; an error do not count. So, for example,
;;
;;   sexp              target buffer name
;;   ----------        ------------------
;;   (shell)           "*shell*"
;;   (shell "foo")     "foo"
;;   (info "(emacs)")  "*info*"
;;   (+ 1 2)           none/undefined
;;   (error)           none/undefined
;;
;; A "shell-like sexp" is one that has a target buffer name. So
;; `(shell)' and `(info "(emacs)")' are shell-like sexps, but `(+ 1
;; 2)' is not.
;;
;; Now consider the two Emacs frames below: we start with
;;                                 ___
;;    ______________emacs_________|-|X|
;;   |                                 |  \
;;   |  (eepitch '(shell))_           |  | We will call this the
;;   | cd /tmp/                        |  | "e-script window".
;;   | ls                              |  | The point is at the "_".
;;   |                                 |  | We will type F8 three times.
;;   |                                 |  |
;;   |                                 |  |
;;   |                                 |  |
;;   |                                 |  /
;;   |--:** NOTES  (Fundamental) ------|  <-- Its modeline.
;;   |_________________________________|  <-- The minibuffer.
;;
;; then we type F8 three times, and we get:
;;                                 ___
;;    ______________emacs_________|-|X|
;;   |                                 |  \
;;   |  (eepitch '(shell))            |  | Let's call this the
;;   | cd /tmp/                        |  | "e-script window".
;;   | ls                              |  | The point is at the "_".
;;   | _                               |  | We just typed F8 three times.
;;   |                                 |  /
;;   |--:** NOTES  (Fundamental) ------|  <-- Its modeline.
;;   |                                 |  \
;;   | /home/edrx# cd /tmp/            |  | Let's call this the
;;   | /tmp# ls                        |  | "target window".
;;   | ./  ../  foo.txt                |  |
;;   | /tmp#                           |  |
;;   |                                 |  /
;;   |--:** *shell*  (Shell:run) ------|  <-- Its modeline.
;;   |_________________________________|  <-- The minibuffer.
;;
;; When we typed F8 on the line " (eepitch '(shell))" the system
;; became "prepared". More precisely:
;;   a) `eepitch-code' was set to `(shell)',
;;   b) `eepitch-buffer-name' was set to the string "*shell*",
;;   c) the buffer "*shell*" was displayed in another window.
;; The actions (b) and (c) were performed by the function
;; `eepitch-prepare'.
;;
;; When we typed F8 on the line "cd /tmp/" the string "cd /tmp/" was
;; "pitched" to the target window, by `(eepitch-line "cd /tmp/")'.
;; Same for the "ls" in the next line. But before pitching each line
;; `eepitch-prepare' was run to make sure that a target window exists.
;;
;; We need more definitions. We say that the system is "(at least)
;; half-prepared" when:
;;   1) `eepitch-buffer-name' holds the target buffer name of the sexp
;;      in `eepitch-code',
;;   2) a buffer with name `eepitch-buffer-name' exists,
;; and we say that the system is "prepared" when (1) and (2) hold,
;; and, besides that,
;;   3) the current buffer's name is not `eepitch-buffer-name', and
;;   4) there is a window - that we will call the "target window" -
;;      showing the buffer `eepitch-buffer-name'.
;;
;; In the code below,
;;   `eepitch-buffer-create' takes care of conditions 1 and 2.
;;   `eepitch-assert-not-target', of condition 3.
;;   `eepitch-window-show', of condition 4 (supposing 1, 2, 3 hold).
;;   `eepitch-prepare', of all the conditions 1-4.



;; This `require' is needed because I had to replace the original
;; `eepitch-window-show' by a hack that needs `find-2a'... =(
;;
(require 'eev-multiwindow)



;; Some basic tools to make this file (almost) self-contained.
;;
(defun ee-bol () (point-at-bol))
(defun ee-eol () (point-at-eol))

(defun ee-read (str)  (read (concat "(progn\n" str "\n)")))
(defun ee-eval (sexp) (let ((debug-on-error nil)) (eval sexp)))

(defun ee-eval-string (str)
  "Wrap STR in a progn then read it and eval it.
Examples: (ee-eval-string \"(+ 1 2) (* 3 4) ;; this returns 12=3*4\")
          (ee-eval-string \";; this returns nil\")"
  (ee-eval (ee-read str)))

(defun ee-eval-string-print (str)
  "Wrap STR in a progn then read it, eval it, and print it."
  (prin1 (ee-eval-string str)))

(defun ee-next-line (&optional arg try-vscroll)
"Line `next-line', but ignoring visual line mode.
This function is used by `eepitch-this-line'."
  (interactive "p")
  (let ((line-move-visual nil))
    (next-line arg try-vscroll)))




;;;                  _ _       _     
;;;   ___  ___ _ __ (_) |_ ___| |__  
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ 
;;; |  __/  __/ |_) | | || (__| | | |
;;;  \___|\___| .__/|_|\__\___|_| |_|
;;;           |_|                    
;;
;; «eepitch»  (to ".eepitch")
;; See: (find-eepitch-intro "2.3. `(eepitch)'")
;; See this for a way to use red bullets insted of red stars:
;;      (find-eepitch-bullet-links)

(defvar eepitch-regexp "^[•]\\(.*\\)"
"The regexp used by `eepitch-this-line' to determine what is a red-star line.
Red star lines are evaluated as lisp, normal lines are pitched to
the target buffer.")

(defvar eepitch-comment-regexp "^[•][•]\\(.*\\)"
"The regexp used by `eepitch-this-line' to test if a line is a comment.
Comment lines are neither evaluated nor sent to the target buffer.
The test that ignores comment lines is applied before the test that decides
between red-star lines (that are eval'ed) and normal lines (that are sent).")

(defvar eepitch-buffer-name ""
  "The name of the target buffer for eepitch.
Set this to \"\" to force running `eepitch-buffer-create' again.
Note that `eepitch-buffer-create' sets this variable!")

(defvar eepitch-code '(error "eepitch not set up")
  "The code to create and switch to the target buffer.")

(defvar eepitch-window-show    '(eepitch-window-show))  ; cheap indirection
(defvar eepitch-kill           '(eepitch-kill-buffer))  ; cheap indirection
(defvar eepitch-kill-windows   'nil)	                ; cheap indirection
(defvar eepitch-line           'nil)	                ; cheap indirection
(defun eepitch-buffer-exists () (get-buffer        eepitch-buffer-name))
(defun eepitch-window-exists () (get-buffer-window eepitch-buffer-name))
(defun eepitch-target-buffer () (get-buffer        eepitch-buffer-name))
(defun eepitch-target-window () (get-buffer-window eepitch-buffer-name))
(defun eepitch-target-here () (eq (current-buffer) (eepitch-target-buffer)))

(defun eepitch-buffer-create ()
  "Eval the sexp in `eepitch-code' and set `eepitch-buffer-name'.
This is done without disturbing the current window configuration.\n
Remember that we say that \"the system is (at least) half-prepared\" when:
  1) `eepitch-buffer-name' holds the target buffer name of the sexp
     in `eepitch-code',
  2) a buffer with name `eepitch-buffer-name' exists.\n
This function makes sure that the system is at least half-prepared.
See `eepitch' and `eepitch-prepare'."
  (save-window-excursion
    (eval eepitch-code)
    (setq eepitch-buffer-name
	  (buffer-name (current-buffer)))))

;; 2018may30: This is broken in some versions of Emacs,
;; 
;; (defun eepitch-window-show ()
;;   "Display the buffer `eepitch-buffer-name' in another window.
;; This is just the default way of making sure that the \"target
;; window\" is visible; note that `eepitch' sets the variable
;; `eepitch-window-show' to `(eepitch-window-show)', and that
;; `eepitch-prepare' evaluates the sexp in the variable
;; `eepitch-window-show'. Alternative eepitch settings - like the
;; ones for GUD or Slime, that use multiple windows - put calls to
;; other functions instead of this one in the variable
;; `eepitch-window-show'.\n
;; This function uses `display-buffer', which calls
;; `split-window-sensibly'."
;;   (let ((pop-up-windows t)
;; 	(same-window-buffer-names nil))
;;     (display-buffer eepitch-buffer-name)))
;;
;; so we use a hack:

(defun eepitch-window-show ()
  "Display the buffer `eepitch-buffer-name' in another window.
This is just the default way of making sure that the \"target
window\" is visible; note that `eepitch' sets the variable
`eepitch-window-show' to `(eepitch-window-show)', and that
`eepitch-prepare' evaluates the sexp in the variable
`eepitch-window-show'. Alternative eepitch settings - like the
ones that would support GUD or Slime using multiple windows -
would put calls to other functions instead of this one in the
variable `eepitch-window-show'."
  (find-2a nil '(find-ebuffer eepitch-buffer-name)))

(defun eepitch-prepare ()
"If the eepitch buffer does not exist, create it; if it is not shown, show it.
In eepitch's terminology we say that the system is \"prepared\" when:
  1) the variable `eepitch-buffer-name' holds the target buffer
     name of the sexp in `eepitch-code',
  2) a buffer with name `eepitch-buffer-name' exists,
  3) the current buffer's name is not `eepitch-buffer-name', and
  4) there is a window - that we will call the \"target window\" -
     showing the buffer `eepitch-buffer-name'.
This function makes sure that the system is prepared. Note that
this function is called from both `eepitch' and
`eepitch-this-line'."
  (if (not (eepitch-buffer-exists))
      (eepitch-buffer-create))
  (if (eq (current-buffer) (eepitch-target-buffer))
      (error "Can't pitch to the current buffer"))
  (if (not (eepitch-window-exists))
      (eval eepitch-window-show)))

(defun eepitch (code)
"Set up a target for eepitch and make sure it is displayed in another window.
The argument CODE must be a \"shell-like sexp\", i.e., one that
when evaluated always switches to a buffer with a fixed name, and
when that buffer does not exists it creates it.\n
This function sets `eepitch-code' to CODE and sets the variables
`eepitch-window-show' and `eepitch-kill' to defaults that are
good for two-window settings, and then calls `eepitch-prepare',
which does all the hard work."
  (setq eepitch-code code)
  (setq eepitch-buffer-name "")	; so that `eepitch-buffer-exists' will fail
  (setq eepitch-window-show	; set the way to set up windows to the
      '(eepitch-window-show))   ; default two-window setting
  (setq eepitch-kill		; set the behavior of `eepitch-kill'
      '(eepitch-kill-buffer))   ; to just kill the target buffer
  (setq eepitch-line nil)	; use the default `eepitch-line'
  (eepitch-prepare)
  (list 'Target: eepitch-buffer-name))	; feedback (for <f8> and `M-e')




;;;                  _ _       _           _   _     _           _ _            
;;;   ___  ___ _ __ (_) |_ ___| |__       | |_| |__ (_)___      | (_)_ __   ___ 
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ _____| __| '_ \| / __|_____| | | '_ \ / _ \
;;; |  __/  __/ |_) | | || (__| | | |_____| |_| | | | \__ \_____| | | | | |  __/
;;;  \___|\___| .__/|_|\__\___|_| |_|      \__|_| |_|_|___/     |_|_|_| |_|\___|
;;;           |_|                                                               
;;
;; «eepitch-this-line»  (to ".eepitch-this-line")

(defun eepitch-eval-at-target-window (code)
  "Run CODE at the eepitch-target-window."
  (eepitch-prepare)
  (save-selected-window
    (select-window (eepitch-target-window))
    (eval code)))

(defun eepitch-line (line)
  "Send LINE to the target window and run the key binding for RET there.
If the value in the variable `eepitch-line' is non-nil then call
the function in that variable instead of running the default code.
This is a low-level function used by `eepitch-this-line'."
  (if eepitch-line
      (funcall eepitch-line line)
    (eepitch-eval-at-target-window	                 ; Default behavior:
     '(progn (goto-char (point-max))	                 ; at the end of buffer
	     (insert line)                               ; "type" the line
	     (call-interactively (key-binding "\r")))))) ; then do a RET

(defun eepitch-this-line ()
"Pitch this line to the target buffer, or eval it as lisp if it starts with `'.
Also, if it starts with `', skip it.
See: (find-eepitch-intro)
and: `eepitch', `eepitch-regexp', `eepitch-comment-regexp'."
  (interactive)
  (let ((line (buffer-substring (ee-bol) (ee-eol))))  ; get line contents
    (setq line (eepitch-preprocess-line line))        ;   and preprocess it
    (cond ((string-match eepitch-comment-regexp line) ; comment lines
	   (message "Comment: %s" line))              ;  are message'd,
	  ((string-match eepitch-regexp line)         ; red star lines
	   (ee-eval-string-print		      ;  are eval'ed and the
	    (match-string 1 line)))	              ;  result is printed,
	  (t (eepitch-prepare)			      ; normal lines
	     (eepitch-line line))))		      ;  are sent
  (ee-next-line 1))

;; See:
;; (find-eepitch-intro "3.3. `eepitch-preprocess-line'")
(defun eepitch-preprocess-line (line) line)



;;;                  _ _       _           _    _ _ _ 
;;;   ___  ___ _ __ (_) |_ ___| |__       | | _(_) | |
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ _____| |/ / | | |
;;; |  __/  __/ |_) | | || (__| | | |_____|   <| | | |
;;;  \___|\___| .__/|_|\__\___|_| |_|     |_|\_\_|_|_|
;;;           |_|                                     
;;
;; «eepitch-kill»  (to ".eepitch-kill")
;; See: (find-eepitch-intro "2.2. `(eepitch-kill)'")

(defun ee-kill-buffer (buffer)
  "Kill BUFFER if it exists, asking for fewer confirmations than usual."
  (if (get-buffer buffer)
      (let ((kill-buffer-query-functions nil))
	(kill-buffer buffer))))

(defun eepitch-kill-buffer ()
  "Kill the eepitch target buffer if it exists, avoiding most warnings.
This function does not change the current window configuration,
and is the default behavior for `eepitch-kill' in two-window
settings. See `eepitch' and `eepitch-kill'."
  (if (eepitch-buffer-exists)
      (if (eepitch-target-here)
	  (error "Can't kill this")
	(ee-kill-buffer eepitch-buffer-name) ; kill with few warnings
	)))

(defun eepitch-kill ()
  "Kill the current eepitch target buffer in the default way.
The default is always the one stored in the variable
`eepitch-kill', and is usually `eepitch-kill-buffer'.

A common idiom - called an \"eepitch block\"; see `eewrap-eepitch'
for a quick way to create eepitch blocks - is to use three
red-star lines in sequence to \"recreate the target\", like this:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

When we run the first `(eepitch-shell)' the eepitch target buffer
becomes the buffer \"*shell*\"; then we run the `(eepitch-kill)'
and we are sure that it will kill the buffer \"*shell*\", not
something else; then we run the last `(eepitch-shell)', and as
the eepitch target buffer does not exist it is recreated from
scratch."
  (eval eepitch-kill))




;;;                  _ _       _               _          _ _ 
;;;   ___  ___ _ __ (_) |_ ___| |__        ___| |__   ___| | |
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ _____/ __| '_ \ / _ \ | |
;;; |  __/  __/ |_) | | || (__| | | |_____\__ \ | | |  __/ | |
;;;  \___|\___| .__/|_|\__\___|_| |_|     |___/_| |_|\___|_|_|
;;;           |_|                                             
;;
;; «eepitch-shell»  (to ".eepitch-shell")
;; See: (find-eepitch-intro "2.2. `(eepitch-shell)'")

(defun eepitch-shell ()
  "Same as (eepitch '(shell)). See `eepitch' and `eewrap-eepitch'."
  (interactive)
  (eepitch '(shell)))

(defun eepitch-shell2  () (interactive) (eepitch '(shell "*shell 2*")))
(defun eepitch-shell3  () (interactive) (eepitch '(shell "*shell 3*")))
(defun eepitch-eshell  () (interactive) (eepitch '(eshell)))
(defun eepitch-eshell2 () (interactive) (eepitch '(eshell 2)))
(defun eepitch-eshell3 () (interactive) (eepitch '(eshell 3)))



;;;                                                  _ 
;;;   ___  ___        _____  ___ __   __ _ _ __   __| |
;;;  / _ \/ _ \_____ / _ \ \/ / '_ \ / _` | '_ \ / _` |
;;; |  __/  __/_____|  __/>  <| |_) | (_| | | | | (_| |
;;;  \___|\___|      \___/_/\_\ .__/ \__,_|_| |_|\__,_|
;;;                           |_|                      
;;
;; «ee-expand»            (to ".ee-expand")
;; «ee-split-and-expand»  (to ".ee-split-and-expand")
;; Tests: (ee-split              "ls   ~/foo")
;;        (ee-split-and-expand   "ls   ~/foo")
;;        (ee-split            '("ls" "~/foo"))
;;        (ee-split-and-expand '("ls" "~/foo"))
;;        (ee-split-and-expand '("ls" "~/foo bar"))

(defun ee-expand (fname)
"Expand \"~\"s and \"$ENVVAR\"s in file names, but only at the beginning."
  (cond ((string-match "^\\$\\([A-Za-z_][0-9A-Za-z_]*\\)\\(.*\\)" fname)
	 (concat (getenv (match-string 1 fname))
		 (match-string 2 fname)))
	((string-match "^\\(~\\([a-z][0-9a-z_]*\\)?\\)\\(/.*\\)?$" fname)
	 (concat (expand-file-name (match-string 1 fname))
		 (match-string 3 fname)))
	(t fname)))

(defun ee-split (str)
  "If STR is a string, split it on whitespace and return the resulting list.
If STR if a list, return it unchanged."
  (if (stringp str)
      (split-string str "[ \t\n]+")
    str))

(defun ee-split-and-expand (str)
"Convert STR to a list (if it's a string) and apply `ee-expand' to each element.
This function is used by `find-comintprocess', `find-bgprocess'
and `find-callprocess'."
  (mapcar 'ee-expand (ee-split str)))




;;;                      _       _   
;;;   ___ ___  _ __ ___ (_)_ __ | |_ 
;;;  / __/ _ \| '_ ` _ \| | '_ \| __|
;;; | (_| (_) | | | | | | | | | | |_ 
;;;  \___\___/|_| |_| |_|_|_| |_|\__|
;;;                                  
;; «find-comintprocess»  (to ".find-comintprocess")
;; See: (find-eepitch-intro "2.5. `find-comintprocess'")
;;      (find-eev "eev-plinks.el" "find-comintprocess")
;;      (find-eev "eev-plinks.el" "find-bgprocess")
;;      (find-enode "Shell Mode" "Comint mode")
;;      (find-enode "Terminal emulator")
;;      (find-elnode "Process Buffers")
;; This is a variant of `find-bgprocess' that runs the process inside
;; an Emacs buffer.

(defun find-comintprocess-ne (name &optional program-and-args)
  "Switch to the buffer named *NAME* and run the command PROGRAM-AND-ARGS there.
This function does not run `ee-expand' on the elements of PROGRAM-AND-ARGS."
  (let ((argv (ee-split (or program-and-args name))))
    (apply 'make-comint name (car argv) nil (cdr argv))
    (switch-to-buffer (format "*%s*" name))))

(defun find-comintprocess (name &optional program-and-args)
  "Switch to the buffer named *NAME* and run the command PROGRAM-AND-ARGS there.
If PROGRAM-AND-ARGS is a string, split it at whitespace to make it a list.
Each element of PROGRAM-AND-ARGS is expanded with `ee-expand'.
See: (find-eepitch-intro)"
  (find-comintprocess-ne name (ee-split-and-expand (or program-and-args name))))

(defun eepitch-comint (name &optional program-and-args)
"Set `eepitch' to run PROGRAM-AND-ARGS in comint mode, in the buffer \"*NAME*\"."
  (eepitch `(find-comintprocess ,name ',program-and-args)))

(defalias 'ee-eepitch-comint 'eepitch-comint-at)




;;;                           
;;; __      ___ __ __ _ _ __  
;;; \ \ /\ / / '__/ _` | '_ \ 
;;;  \ V  V /| | | (_| | |_) |
;;;   \_/\_/ |_|  \__,_| .__/ 
;;;                    |_|    
;;
;; «eewrap-eepitch»       (to ".eewrap-eepitch")
;; «ee-adjust-red-stars»  (to ".ee-adjust-red-stars")
;; See: (find-eev-quick-intro "6.3. Creating eepitch blocks: `M-T'")
;;      (find-eev-quick-intro "wrap")
;;      (find-wrap-intro)
;;      (find-eev "eev-mode.el" "eev-mode-map-set" "M-T" "eewrap-eepitch")
;;      (find-eepitch-bullet-links)

(defun eewrap-eepitch () (interactive)
  "Convert the current into an eepitch block (three lines long)."
  (let* ((fmt   " (eepitch-%s)\n (eepitch-kill)\n (eepitch-%s)")
	 (li    (ee-this-line-extract))
	 (newli (format fmt li li)))
    (insert (ee-adjust-red-stars newli)))
  (ee-next-line 1))

(defun ee-adjust-red-stars (str)
  "Replace all the red stars in STR by another character if needed.
By default this function returns STR unchanged. For a way to make
it replace the red stars by bullets, see:
  (find-eepitch-bullet-links)"
  str)

(defun ee-this-line-extract ()
  "Delete the contents of the current line and return it as a string."
  (delete-and-extract-region (ee-bol) (ee-eol)))

(defun ee-no-properties (str)
  (setq str (copy-sequence str))
  (set-text-properties 0 (length str) nil str)
  str)






;;;        _             _         
;;;   __ _| |_   _ _ __ | |__  ___ 
;;;  / _` | | | | | '_ \| '_ \/ __|
;;; | (_| | | |_| | |_) | | | \__ \
;;;  \__, |_|\__, | .__/|_| |_|___/
;;;  |___/   |___/|_|              
;;;
;; «glyphs»  (to ".glyphs")
;; See: (find-eev-quick-intro "6.4. Red stars")
;;      (find-eev "eev-anchors.el" "glyphs")
;; More on glyphs:
;;   http://angg.twu.net/glyphs.html
;; The `(<= 128 pos)' below is explained at:
;;   http://angg.twu.net/glyphs.html#bug-report

(defface eepitch-star-face
  '((t (:foreground "red")))
  "Face used for the red star glyph (char 15).")

(defun eepitch-set-glyph0 (pos &optional char face)
  "See: (find-eepitch-intro \"glyph\")"
  (aset standard-display-table pos
	(if char (vector (make-glyph-code char face)))))

(defun eepitch-set-glyph (pos &optional char face)
  "See: (find-eepitch-intro \"glyph\")
and: (find-anchors-intro \"glyphs\")
This is the high-level version of `eepitch-set-glyph0', with a hack
to make it work similarly in unibyte and multibyte buffers."
  (eepitch-set-glyph0 pos char face)
  (if (<= 128 pos)
      (eepitch-set-glyph0 (make-char 'eight-bit pos) char face)))


;;;           _           _             _         
;;;  ___  ___| |_    __ _| |_   _ _ __ | |__  ___ 
;;; / __|/ _ \ __|  / _` | | | | | '_ \| '_ \/ __|
;;; \__ \  __/ |_  | (_| | | |_| | |_) | | | \__ \
;;; |___/\___|\__|  \__, |_|\__, | .__/|_| |_|___/
;;;                 |___/   |___/|_|              
;;
;; «set-glyphs»  (to ".set-glyphs")

(if (not standard-display-table)
    (setq standard-display-table (make-display-table)))
(eepitch-set-glyph ?\^O ?* 'eepitch-star-face)


;;;           _     _                  
;;;  ___  ___| |_  | | _____ _   _ ___ 
;;; / __|/ _ \ __| | |/ / _ \ | | / __|
;;; \__ \  __/ |_  |   <  __/ |_| \__ \
;;; |___/\___|\__| |_|\_\___|\__, |___/
;;;                          |___/     
;;
;; «set-keys»  (to ".set-keys")
;; Moved to: (find-eev "eev-mode.el" "eev-mode-map-set")
;; (global-set-key [f8]   'eepitch-this-line)
;; (global-set-key "\M-T" 'eewrap-eepitch)








;;;                  _ _       _               _       
;;;   ___  ___ _ __ (_) |_ ___| |__         __| | ___  
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ _____ / _` |/ _ \ 
;;; |  __/  __/ |_) | | || (__| | | |_____| (_| | (_) |
;;;  \___|\___| .__/|_|\__\___|_| |_|      \__,_|\___/ 
;;;           |_|                                      
;;
(defun eepitch-make-suffix (arg)
  (cond ((null arg) "")
	((stringp arg) (format " %s" arg))
	((numberp arg) (format " (%s)" arg))))

(defun eepitch-do (program-and-args &optional suffix)
  (eepitch-comint (format "%s%s" (ee-unsplit program-and-args)
			  (eepitch-make-suffix suffix))
		  program-and-args))


;;;                      _       _                  _           
;;;   ___ ___  _ __ ___ (_)_ __ | |_       ___  ___| |__   ___  
;;;  / __/ _ \| '_ ` _ \| | '_ \| __|____ / _ \/ __| '_ \ / _ \ 
;;; | (_| (_) | | | | | | | | | | ||_____|  __/ (__| | | | (_) |
;;;  \___\___/|_| |_| |_|_|_| |_|\__|     \___|\___|_| |_|\___/ 
;;;                                                             
;; Some programs, like zsh, echo their input when they are run in
;; eepitch buffers. Sometimes setting the variable
;; `comint-process-echoes' fixes this, but now always. The most
;; reliable (modern) way to run them is to use vterm, but this
;; requires compiling emacs with support for modules and installing
;; the vterm package.
;; See: (find-variable 'comint-process-echoes)
;; To do: send an e-mail to Olin Shivers about echoing and stty.

(defun eepitch-del-echo (flag)
"A hack to help determining whether a program echoes its commands or not.
An example of use:\n
 (eepitch-zsh)
 (eepitch-kill)
 (eepitch-zsh)
cd /tmp/
 (eepitch-del-echo t)
cd /tmp/
 (eepitch-del-echo nil)
cd /tmp/\n"
  (eepitch-eval-at-target-window `(setq comint-process-echoes ,flag))
  (message "At %s: %S" eepitch-buffer-name
	   `(setq comint-process-echoes ,flag)))

(defun eepitch-de (code)
  "Like `eepitch', but deletes the echoed commands.
Use this to control programs that echo the commands that they receive."
  (eepitch `(progn ,code (setq comint-process-echoes t))))

(defun eepitch-comint-de (name &optional program-and-args)
  "Like `eepitch-comint', but deletes the echoed commands.
Use this to control programs that echo the commands that they receive."
  (eepitch-de `(find-comintprocess ,name ',program-and-args)))



;;;   ___  _   _                 _              _     
;;;  / _ \| |_| |__   ___ _ __  | |_ ___   ___ | |___ 
;;; | | | | __| '_ \ / _ \ '__| | __/ _ \ / _ \| / __|
;;; | |_| | |_| | | |  __/ |    | || (_) | (_) | \__ \
;;;  \___/ \__|_| |_|\___|_|     \__\___/ \___/|_|___/
;;;                                                   
;; Useful for controlling certain external programs.

(defun ee-at0 (dir code)
  "Eval CODE at DIR.
If DIR does not end with a slash then weird things might happen.
Note the DIR is `ee-expand'-ed."
  (setq dir (ee-expand dir))
  (if (not (file-accessible-directory-p dir))
      (error "Can't chdir to %s" dir))
  (let ((default-directory dir))
    (eval code)))

(defun eepitch-comint-at (dir name &optional program-and-args)
  "Like `eepitch-comint', but executes `eepitch-buffer-create' at DIR."
  (ee-at0 dir `(eepitch-comint ,name ,program-and-args)))

(defun eepitch-to-buffer (name)
  (interactive "beepitch to buffer: ")
  (eepitch `(switch-to-buffer ,name)))

(defun ee-with-pager-cat (code)
  "Run CODE with the environment variable PAGER set to \"cat\".
This is useful for for running processes that use pagers like
\"more\" by default."
  (let ((process-environment (cons "PAGER=cat" process-environment)))
    (eval code)))

;; (defun at-nth-window (n code)
;;   "Run `other-window' N times, run CODE there, and go back."
;;   (save-selected-window
;;     (other-window n)
;;     (eval code)))




;;;   ___  _   _                 _                          
;;;  / _ \| |_| |__   ___ _ __  | |_ ___ _ __ _ __ ___  ___ 
;;; | | | | __| '_ \ / _ \ '__| | __/ _ \ '__| '_ ` _ \/ __|
;;; | |_| | |_| | | |  __/ |    | ||  __/ |  | | | | | \__ \
;;;  \___/ \__|_| |_|\___|_|     \__\___|_|  |_| |_| |_|___/
;;;                                                         
;; «other-terms»  (to ".other-terms")
;; All these functions use the same convention on their arguments:
;;
;;   1. the argument PROGRAM comes first, and when it is omitted it
;;      means that we should run the default shell;
;;
;;   2. the argument NAME0 comes second;
;;
;;   3. when the argument NAME0 is present the term buffer receives a
;;      name like "*vterm: NAME0*";
;;
;;   4. when PROGRAM is present but NAME is omitted the term buffer
;;      receives a name like "*vterm: PROGRAM*";
;;
;;   5. when both arguments are omitted the term buffer receives a
;;      name like "*vterm*" or "*ansi-term*".
;;
;; Note that these conventions are different from the ones that
;; `find-comintprocess' uses.

;; Tests:
;;   (find-ansitermprocess nil "DASH")
;;   (find-ansitermprocess "/bin/dash" "DASH")
;;   (find-ansitermprocess "/bin/dash")
;;   (find-ansitermprocess)
;;
(defun find-ansitermprocess (&optional program name0)
  "See: (find-eev \"eepitch.el\" \"other-terms\")"
  (let* ((name1 (or name0 program))
	 (name2 (if name1 (format "ansi-term: %s" name1) "ansi-term"))
	 (buffername (format "*%s*" name2)))
    (if (get-buffer   buffername)
	(find-ebuffer buffername)
      (ansi-term (or program shell-file-name) name2))))

;; Tests:
;;   (find-vtermprocess nil "DASH")
;;   (find-vtermprocess "/bin/dash" "DASH")
;;   (find-vtermprocess "/bin/dash")
;;   (find-vtermprocess)
;;
(defun find-vtermprocess (&optional program name0)
  "See: (find-eev \"eepitch.el\" \"other-terms\")"
  (require 'vterm)
  (let* ((name1 (or name0 program))
	 (name2 (if name1 (format "vterm: %s" name1) "vterm"))
	 (buffername (format "*%s*" name2))
	 (vterm-shell (or program vterm-shell)))
    (if (get-buffer   buffername)
	(find-ebuffer buffername)
      (vterm buffername))))


;; `eepitch-ansiterm' and `eepitch-vterm' use these variants of the
;; original `eepitch-line'.
;;
(defun eepitch-line-ansiterm (line)
  "Send LINE to the ansi-term buffer and run the key binding for RET there."
  (eepitch-eval-at-target-window
    '(progn (term-send-raw-string line)	   ; "type" the line
	    (term-send-raw-string "\r")))) ; then do a RET

(defun eepitch-line-vterm (line)
  "Send LINE to the vterm buffer and run the key binding for RET there."
  (eepitch-eval-at-target-window
    '(progn (vterm-send-string line)	; "type" the line
	    (vterm-send-return))))      ; then do a RET


;; «eepitch-ansiterm»  (to ".eepitch-ansiterm")
;; Tests:
;;   (eepitch-ansiterm nil "DASH")
;;   (eepitch-ansiterm "/bin/dash" "DASH")
;;   (eepitch-ansiterm "/bin/dash")
;;   (eepitch-ansiterm)
;;   (eepitch-line "echo $(tput setaf 1)Hello$(tput setaf 0)")
;;
(defun eepitch-ansiterm (&optional program name0)
  "This is like `eepitch-comint', but using ansi-term instead of comint.
It uses `eepitch-line-ansiterm' instead of `eepitch-line'.
The arguments are explained here:
  (find-eev \"eepitch.el\" \"other-terms\")"
  (interactive)
  (prog1 (eepitch `(find-ansitermprocess ,program ,name0))
    (setq eepitch-line 'eepitch-line-ansiterm)))

;; «eepitch-vterm»  (to ".eepitch-vterm")
;; Tests:
;;   (eepitch-vterm nil "DASH")
;;   (eepitch-vterm "/bin/dash" "DASH")
;;   (eepitch-vterm "/bin/dash")
;;   (eepitch-vterm)
;;   (eepitch-line "echo $(tput setaf 1)Hello$(tput setaf 0)")
;;
(defun eepitch-vterm (&optional program name0)
  "This is like `eepitch-comint', but using vterm instead of comint.
It uses `eepitch-line-vterm' instead of `eepitch-line'.
The arguments are explained here:
  (find-eev \"eepitch.el\" \"other-terms\")"
  (interactive)
  (prog1 (eepitch `(find-vtermprocess ,program ,name0))
    (setq eepitch-line 'eepitch-line-vterm)))

;; «eepitch-sly»  (to ".eepitch-sly")
;; This is a prototype. See:
;; https://github.com/joaotavora/sly/issues/527
;;
(defun find-slyprocess ()
  "Go to a Sly REPL buffer, This function is used by `eepitch-sly'."
  (require 'sly)
  (sly-setup)
  (if (and (sly-current-connection)
	   (sly-mrepl--find-buffer))
      (find-ebuffer (sly-mrepl--find-buffer) :end)
    (let ((sly-command-switch-to-existing-lisp 'never)
	  (sly-auto-select-connection 'never)
	  (sly-lisp-implementations '((sbcl ("sbcl"))))
	  (sly-default-lisp 'sbcl))
      (sly))))

(defun eepitch-sly () (interactive)
  (eepitch '(find-slyprocess)))



;;;  _                                                  
;;; | |    __ _ _ __   __ _ _   _  __ _  __ _  ___  ___ 
;;; | |   / _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \/ __|
;;; | |__| (_| | | | | (_| | |_| | (_| | (_| |  __/\__ \
;;; |_____\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___||___/
;;;                   |___/             |___/           
;;
;; «eepitch-langs»  (to ".eepitch-langs")
;; See: (find-eev-quick-intro "6.2. Other targets")
;;      (find-eepitch-intro "2.5. `find-comintprocess'")

;; Shells:
;; These three are commented out because they are defined above.
;; (defun eepitch-shell  () (interactive) (eepitch '(shell)))
;; (defun eepitch-shell2 () (interactive) (eepitch '(shell "*shell 2*")))
;; (defun eepitch-eshell () (interactive) (eepitch '(eshell)))
(defun eepitch-sh   () (interactive) (eepitch-comint "sh" "sh"))
(defun eepitch-bash () (interactive) (eepitch-comint "bash" "bash"))
(defun eepitch-dash () (interactive) (eepitch-comint "dash" "dash"))
(defun eepitch-ksh  () (interactive) (eepitch-comint "ksh" "ksh"))
(defun eepitch-tcsh () (interactive) (eepitch-comint "tcsh" "tcsh"))
(defun eepitch-zsh  () (interactive) (eepitch-comint-de "zsh" "zsh"))
(defun eepitch-scsh () (interactive) (eepitch-comint "scsh" "scsh"))
(defun eepitch-pwsh  () (interactive) (eepitch-ansiterm "pwsh" "pwsh"))
(defun eepitch-pwsh2 () (interactive) (eepitch-ansiterm "pwsh" "pwsh 2"))

;; Main interpreted languages:
(defun eepitch-lua51  () (interactive) (eepitch-comint "lua51"  "lua5.1"))
(defun eepitch-lua52  () (interactive) (eepitch-comint "lua52"  "lua5.2"))
(defun eepitch-lua53  () (interactive) (eepitch-comint "lua53"  "lua5.3"))
(defun eepitch-lua54  () (interactive) (eepitch-comint "lua54"  "lua5.4"))
(defun eepitch-luajit () (interactive) (eepitch-comint "luajit" "luajit"))
(defun eepitch-julia  () (interactive) (eepitch-comint "julia"  "julia"))
(defun eepitch-python2 () (interactive) (eepitch-comint "python2" "python2"))
(defun eepitch-python3 () (interactive) (eepitch-comint "python3" "python3"))
(defun eepitch-python  () (interactive) (eepitch-comint "python3" "python3"))
(defun eepitch-perl () (interactive) (eepitch-comint "perl" "perl -d -e 42"))
(defun eepitch-ruby    () (interactive) (eepitch-ansiterm "irb" "irb"))
(defun eepitch-irb     () (interactive) (eepitch-ansiterm "irb" "irb"))

;; Tcl:
(defun eepitch-tcl     () (interactive) (eepitch-comint "tclsh"   "tclsh"))
(defun eepitch-tclsh   () (interactive) (eepitch-comint "tclsh"   "tclsh"))
(defun eepitch-wish    () (interactive) (eepitch-comint "wish"    "wish"))
(defun eepitch-expect  () (interactive) (eepitch-comint "expect"  "expect"))

;; Lisps and Schemes:
(defun eepitch-ielm   () (interactive) (eepitch '(ielm)))
;; It would be better to run these ones in Slime, but how? (Help needed!)
(defun eepitch-sbcl   () (interactive) (eepitch-comint "sbcl" "sbcl"))
(defun eepitch-gcl    () (interactive) (eepitch-comint "gcl"  "gcl"))
(defun eepitch-guile  () (interactive) (eepitch-comint "guile" "guile"))
(defun eepitch-racket () (interactive) (eepitch-comint "racket" "racket"))
(defun eepitch-scheme () (interactive) (eepitch-comint "scheme" "scheme"))
(defun eepitch-mitscheme () (interactive)
  (eepitch-comint "mit-scheme" "mit-scheme"))
(defun eepitch-tinyscheme () (interactive)
  (eepitch-comint "tinyscheme" "tinyscheme"))
;;
(defun eepitch-fennel () (interactive) (eepitch-comint "fennel" "fennel"))
;; Or: (defun eepitch-fennel () (interactive) (eepitch '(fennel-repl nil)))


;; Haskell, ML, Erlang, Coq:
(defun eepitch-ghci   () (interactive) (eepitch-comint "ghci" "ghci"))
(defun eepitch-hugs   () (interactive) (eepitch-comint "hugs" "hugs"))
(defun eepitch-hugs98 () (interactive) (eepitch-comint "hugs" "hugs -98"))
(defun eepitch-ocaml  () (interactive) (eepitch-comint "ocaml" "ocaml"))
(defun eepitch-polyml () (interactive) (eepitch-comint "polyml" "poly"))
(defun eepitch-sml    () (interactive) (eepitch-comint "sml" "sml"))
(defun eepitch-labltk () (interactive) (eepitch-comint "labltk" "labltk"))
(defun eepitch-erl    () (interactive) (eepitch-comint "erl" "erl"))
(defun eepitch-coqtop () (interactive) (eepitch-comint "coqtop" "coqtop"))

;; Forth:
(defun eepitch-gforth () (interactive) (eepitch '(run-forth "gforth")))
(defun eepitch-gforth () (interactive) (eepitch-comint "gforth" "gforth"))
(defun eepitch-pforth () (interactive) (eepitch-comint "pforth" "pforth"))
(defun eepitch-yforth () (interactive) (eepitch-comint "yforth" "yforth"))

;; Mathematics:
(defun eepitch-maxima () (interactive) (eepitch-comint "maxima" "maxima"))
(defun eepitch-octave () (interactive) (eepitch-comint "octave" "octave"))
(defun eepitch-R () (interactive)
  (eepitch '(ee-with-pager-cat (find-comintprocess "R" "R"))))

;; Plotters:
;; We force GhostScript's resolution to make its window fit on the screen.
(defun eepitch-gs () (interactive) (eepitch-comint "gs" "gs -r45"))
(defun eepitch-gs () (interactive) (eepitch-comint "gs" "gs -r60"))
(defun eepitch-gnuplot () (interactive) (eepitch-comint "gnuplot" "gnuplot"))

;; Java-based languages:
(defun eepitch-bsh () (interactive)
  (eepitch-de '(find-comintprocess "bsh" "bsh")))
(defun eepitch-scala () (interactive)
  (eepitch '(find-comintprocess "scala" "scala")))
(defun eepitch-clojure () (interactive)
  (eepitch '(find-comintprocess "clojure" "clojure -r")))

;; SQL. To do: add postgres and sqlite
(defun eepitch-mysql () (interactive)
  (eepitch '(ee-with-pager-cat '(find-comintprocess "mysql" "mysql -u root"))))

;; SmallTalk
(defun eepitch-gst () (interactive)
  (eepitch '(find-comintprocess "gst" "gst")))

;; JavaScript
;; MozRepl is a Javascript REPL in a running Mozilla browser.
;; See: https://github.com/bard/mozrepl/wiki/tutorial
(defun eepitch-nodejs () (interactive) (eepitch-comint "nodejs" "nodejs"))
(defun eepitch-smjs () (interactive) (eepitch-comint "smjs" "smjs"))
(defun eepitch-mozrepl () (interactive)
  (eepitch-comint "mozrepl" "telnet localhost 4242"))

;; Programs from the TeX family.
;; They create logfiles in the current dir, so we run them in /tmp/.
(defun eepitch-luatex () (interactive)
  (eepitch-comint-at "/tmp/" "luatex" "luatex"))
(defun eepitch-lualatex () (interactive)
  (eepitch-comint-at "/tmp/" "lualatex" "lualatex"))
(defun eepitch-latex () (interactive)
  (eepitch-comint-at "/tmp/" "latex" "latex"))
(defun eepitch-tex   () (interactive)
  (eepitch-comint-at "/tmp/" "tex"   "tex"))
(defun eepitch-mf    () (interactive)
  (eepitch-comint-at "/tmp/" "mf"   "mf"))
(defun eepitch-mpost () (interactive)
  (eepitch-comint-at "/tmp/" "mpost" "mpost"))

;; Pulseaudio (this is to interact with its daemon)
(defun eepitch-pacmd () (interactive) (eepitch-comint "pacmd" "pacmd"))





(provide 'eepitch)



;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
