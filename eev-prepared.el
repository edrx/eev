;;; eev-prepared.el -- eev modules that use temporary dirs and prepared shells.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
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
;; Version:    20240307
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-prepared.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-prepared.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:

;; This used to be my main way of sending chunks of code to external
;; programs, but then `eepitch' appeared and I started to use eepitch
;; for almost everything. This is still useful in some niche cases but
;; I don't even know if there are other people besides me who still
;; use this code. I still use it to 1) send commands like `cd' to
;; terminals running outside emacs, 2) send chunks of shell commands
;; to external machines, 3) save blocks of LaTeX code that are loaded
;; by some .tex files for tests, 4) send chunks of code to Tcl/Tk or
;; SmallTalk.
;;
;; See: (find-prepared-intro)
;;      (find-eev "eev-env.el")



(require 'eev-env)
(require 'eev-code)

;; Compare with:
;;   (find-eev "eev-code.el" "code-c-d-s")
;;   (find-eev "eev-code.el" "code-c-d-s" "ee-eev-source-directory")
;; See: (find-eev-intro "that require extra setup:")
;;
(ee-setenv "EEVDIR"
           (let ((fname (locate-library "eev")))
             (if fname (directory-file-name (file-name-directory fname))
               "~/eev-current")))       ; eev.el, etc

(ee-setenv "EEVTMPDIR" "$HOME/.eev")	; ee.sh and other temp scripts
(ee-setenv "EEVRCDIR"  "$EEVDIR/rcfiles")
(ee-setenv "EE"        "$EEVTMPDIR/ee.sh")
(ee-setenv "EEG"       "$EEVTMPDIR/ee.eeg")
(ee-setenv "EEGDB"     "$EEVTMPDIR/ee.gdb")
(ee-setenv "EETEX"     "$EEVTMPDIR/ee.tex")
(ee-setenv "EEC"       "$EEVTMPDIR/ee.c")
(ee-setenv "EETMPC"    "$EEVTMPDIR/tmp.c")
(ee-setenv "EEAOUT"    "$EEVTMPDIR/ee.aout")

(defvar ee-eevtmpdir    (ee-expand "$EEVTMPDIR/")
  "The directory where the temporary script files are put.")

(defvar ee-eevrcdir     (ee-expand "$EEVRCDIR/")
  "The directory where some auxiliary rcfiles for eev are to be found.")

(defvar ee-file         (ee-expand "$EE")
  "The temporary script file used by `eev'.")
(defvar ee-file-tex     (ee-expand "$EETEX")
  "The temporary script file used by `eelatex'.")
(defvar ee-file-gdb     (ee-expand "$EEGDB")
  "The temporary script file used by `eegdb'.")
(defvar ee-file-generic (ee-expand "$EEG"))

(defvar eelatex-eevscript
  "cd $EEVTMPDIR/; latex tmp.tex && xdvi tmp.dvi &" "See `eelatex'.")



(code-c-d "eevtmp" "$EEVTMPDIR/"       :anchor)  ; (find-eevtmpfile "")
(code-c-d "eevrc"  "$EEVRCDIR/"        :anchor)  ; (find-eevrcfile "")
(code-c-d "eevex"  "$EEVDIR/examples/" :anchor)  ; (find-eevexfile "")

;; (defvar ee-eevdir       (ee-expand "$EEVDIR/")
;;   "The directory where the elisp files for eev live.")
;; (code-c-d "eev"    "$EEVDIR/"          :anchor)  ; (find-eev "")



;;;  __  __                             
;;; |  \/  |    __  __    ___  _____   __
;;; | |\/| | __ \ \/ /   / _ \/ _ \ \ / /
;;; | |  | ||__| >  <   |  __/  __/\ V / 
;;; |_|  |_|    /_/\_\   \___|\___| \_/  
;;;
;;; eev and friends (or: saving regions as temporary scripts)
;;;

(defun ee-se-to-string (s e)
  "Convert the pair (S E) to a string.
If S is a number then return the contents of the current buffer
between the positions S and E; if S is a string then return S and
ignore E. See `write-region' - it uses the same convention for
interpreting \"(S E)\"-pairs as this function."
  (cond ((numberp s) (buffer-substring-no-properties s e))
        ((stringp s) s)))

(defun ee-octal-to-num (str)
  "Convert STR - a sequence of octal digits - to a number."
  (let ((lastv (- (string-to-char (substring str -1)) ?0))
	(rest (substring str 0 -1)))
    (if (string= "" rest) lastv (+ lastv (* 8 (ee-octal-to-num rest))))))

(defun ee-write-string (str &optional altfile fmode)
  "Write STR to ALTFILE, or to ee-file if ALTFILE is nil.
FMODE should be either nil or a string containing a sequence of
octal digits; if it is not nil then do the equivalent of a
\"chmod FMODE file\"."
  (let ((fname (substitute-in-file-name (or altfile ee-file))))
    (write-region str nil fname)	; a standard kludge
    (if fmode (set-file-modes fname (ee-octal-to-num fmode)))))

(defun ee-write (s e pre post &optional altfile fmode)
  "Write PRE+(ee-se-to-string S E)+POST to ALTFILE, or to `ee-file'.
PRE and POST must be strings. See `ee-se-to-string' and
`ee-write-string'."
  (ee-write-string (concat pre (ee-se-to-string s e) post)
		   altfile fmode))

(defun ee-se-to-string-with-nl (s e)
  "Same as `ee-se-to-string', but force the result to end with a newline."
  (let ((str (ee-se-to-string s e)))
    (if (string-match "[^\n]\\'" str) (concat str "\n") str)))

(defun ee-write-with-nl (s e pre post &optional altfile fmode)
  "Same as `ee-write', but using `ee-se-to-string-with-nl'."
  (ee-write-string (concat pre (ee-se-to-string-with-nl s e) post)
		   altfile fmode))


;; See: (find-prepared-intro "1. Prepared shells")
;;
(defun eev (s &optional e altfile)
  "Save the region in `ee-file', or in ALTFILE if it is non-nil.
If S is a string write then write the string instead. See `ee-write'.
This function is mostly used to send blocks of commands to shells via
a temporary script file. The shells do not receive the commands
immediately - we need to tell them to execute the commands stored in
the temporary script.\n
For example, if we mark the block below and type `M-x eev',\n
  # A hyperlink: (find-efunction 'eev)
  echo $[1+2]
  # Temporary scripts can change the
  # directory and the environment.
  cd /tmp/\n
and then go to a prepared shell and run `ee', we see something like
this:\n
  /home/edrx$ ee
  # A hyperlink: (find-efunction 'eev)
  echo $[1+2]
  3
  # Temporary scripts can change the
  # directory and the environment.
  cd /tmp/
  /tmp$ \n
Note that this only works in \"prepared shells\", where `ee' has been
defined as a shell function in the correct way; the relevant code for
.bashrc or .zshrc is this:\n
  export EEVTMPDIR ;: ${EEVTMPDIR:=~/.eev}
  export EE        ;: ${EE:=$EEVTMPDIR/ee.sh}
  function ee () { set -v; . $EE$*; set +v; }\n
See: (find-eevfile \"INSTALL\")
and: (find-eevfile \"eev-rctool\")"
  (interactive "r")
  (ee-write-with-nl s e "" "" altfile)
  (format "eev: wrote %s" (or altfile ee-file)))

(defun eevs (s &optional e suffix)
  "Like `eev', but with a suffix; write the region to `ee-file'+SUFFIX.
For example, if $EE is \"~/.eev/ee.sh\" and SUFFIX is \"0\" then
write the region to the file \"~/.eev/ee.sh0\". The shell
function \"ee\" concatenates its first argument to the value of
$EE, so running \"ee 0\" on a prepared shell executes the
temporary script \"~/.eev/ee.sh0\" instead of \"~/.eev/ee.sh\".
If S is a string write then write the string instead. See `ee-write'."
  (interactive "r\nsSuffix: ")
  (eev s e (concat ee-file suffix)))

(defun eelatex (s &optional e)
  "Save the region to `ee-file-tex', then save `eelatex-eevscript' to `ee-file'.
An example: run `M-x eelatex' on the line below,

  Hello! $\\frac{42}{\\sqrt{5}}$

then go to a prepared shell and run \"ee\". A temporary LaTeX
file will be processed by \"latex\" and the resulting dvi file
will be shown on the screen.
If S is a string write then write the string instead. See `eev'."
  (interactive "r")
  (ee-write s e "" "" ee-file-tex)
  (eev eelatex-eevscript nil)
  (format "eelatex: wrote %s and %s" ee-file-tex ee-file))

(defun eegdb (s &optional e)
  "Save the region to the temporary GDB script file given by `ee-file-gdb'.
After that if your GDB init file was prepared adequately then
running \"ee\" on a GDB prompt will make GDB execute the commands
in the temporary GDB script.
If S is a string write then write the string instead. See `eev'."
  (interactive "r")
  (ee-write s e "" "" ee-file-gdb)
  (format "eegdb: wrote %s" ee-file-gdb))

;; Obsolete, or almost? Used by: (find-eevfile "eeg4")
(defun eeg (s &optional e)
  (interactive "r")
  (ee-write s e "" "" ee-file-generic)
  (format "eeg: wrote %s" ee-file-gdb))

(defun eeeval (s &optional e)
"Like `eev', but instead of saving the region execute it immediately as Lisp.
This function is very similar to `eval-region'."
  (interactive "r")
  (eval (read (concat "(progn " (ee-se-to-string s e) "\n)"))))



(defun ee-default-directory ()
  "Return `default-directory' usually, but behave specially in some modes.
If the current buffer is a w3m buffer that is visiting a local
file (i.e., if the url is like \"file://...\") then extract the
directory from the url instead of returning the value of
`default-directory'.\n
This function is used by `eecd'."
  (if (eq major-mode 'w3-mode)
      (let ((url (url-view-url 0)))
	(if (string-match "^file:\\(.*/\\)[^/]*$" url)
	    (match-string 1 url)
	  (error "Current url is %S, which is not a local file" url)))
    default-directory))

;; 2005jan10, incompatible change: added "dir"
(defun eecd (&optional dir command)
  "Save to $EE a \"cd\" command to `cd' to the current directory.
If DIR is not nil then use DIR; otherwise run `ee-default-directory'.
If COMMAND is not nil then save \"cd DIR; COMMAND\" instead of just
\"cd DIR\".\n
See `eev' for more about $EE and the temporary script file."
  (interactive)
  (eev (concat "cd " (file-name-directory
		      (or dir (ee-default-directory)))
	       "\n" (or command ""))))



(provide 'eev-prepared)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
