;;; eev-bounded.el -- functions like `eev-bounded', `eelatex-bounded', etc.

;; Copyright (C) 2012 Free Software Foundation, Inc.
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
;; Version:    20121226
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-bounded.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-bounded.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-bounded-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-bounded-intro)

;;; Commentary:
;;
;; This file adds support for "bounded functions" to eev. For example:
;; `M-x eev' saves the region between point and mark into the
;; temporary script file; `M-x eev-bounded' saves the region around
;; point, up to the first occurrences of a certain delimiters before
;; and after point, into the temporary script file.
;;
;; Big letters courtesy of Figlet.

;; See: (find-bounded-intro)

;; «.code-bounded»	(to "code-bounded")
;; «.obsolete»		(to "obsolete")




;;;      _      _ _           _ _                
;;;   __| | ___| (_)_ __ ___ (_) |_ ___ _ __ ___ 
;;;  / _` |/ _ \ | | '_ ` _ \| | __/ _ \ '__/ __|
;;; | (_| |  __/ | | | | | | | | ||  __/ |  \__ \
;;;  \__,_|\___|_|_|_| |_| |_|_|\__\___|_|  |___/
;;;                                              

(defvar ee-delimiter-hash      "\n#\n"  "See `eev-bounded'.")
(defvar ee-delimiter-percent   "\n%\n"  "See `eelatex-bounded'.")
(defvar ee-delimiter-semicolon "\n;;\n" "See `eeeval-boudned'.")

(put 'ee-delimiter-hash      'safe-local-variable 'stringp)
(put 'ee-delimiter-percent   'safe-local-variable 'stringp)
(put 'ee-delimiter-semicolon 'safe-local-variable 'stringp)

(defun ee-sdelim-to-s (sdelim)
  "Search backwards for STR and return the position after STR.
This function does not move point."
  (+ (save-excursion (search-backward sdelim))
     (length sdelim)))

(defun ee-edelim-to-e (edelim)
  "Search forward for STR and return the position before STR.
This function does not move point."
  (+ (save-excursion (search-forward edelim))
     (- (length edelim))))



;;;                _            _                           _          _ 
;;;   ___ ___   __| | ___      | |__   ___  _   _ _ __   __| | ___  __| |
;;;  / __/ _ \ / _` |/ _ \_____| '_ \ / _ \| | | | '_ \ / _` |/ _ \/ _` |
;;; | (_| (_) | (_| |  __/_____| |_) | (_) | |_| | | | | (_| |  __/ (_| |
;;;  \___\___/ \__,_|\___|     |_.__/ \___/ \__,_|_| |_|\__,_|\___|\__,_|
;;;                                                                      
;; «code-bounded»  (to ".code-bounded")
;; See: (find-bounded-intro "Defining new bounded functions")

(defun       code-bounded (newf f delim &optional adjust face dur)
  (eval (ee-read 
	 (ee-code-bounded  newf f delim           adjust face dur))))
(defun  find-code-bounded (newf f delim &optional adjust face dur)
  (find-estring-elisp
         (ee-code-bounded  newf f delim           adjust face dur)))
(defun    ee-code-bounded (newf f delim &optional adjust face dur)
  (setq adjust (or adjust 1))
  (setq face   (or face 'highlight))
  (setq dur    (or dur 0.75))
  (ee-template0 "
\(defun {newf} ()
  \"Run the function `{f}' on a delimited region around point.
See: (find-bounded-intro)\"
  (interactive)
  (setq ee-bounded-function '{newf})
  (let* ((s (ee-sdelim-to-s {(ee-S delim)}))
	 (e (ee-edelim-to-e {(ee-S delim)})))
    (ee-flash s (+ e {adjust})
	      '{(ee-S face)} {dur})
    ({f} (ee-se-to-string s e))))
   "))

(code-bounded 'eev-bounded     'eev     'ee-delimiter-hash)
(code-bounded 'eeg-bounded     'eeg     'ee-delimiter-hash)
(code-bounded 'eegdb-bounded   'eegdb   'ee-delimiter-hash)
(code-bounded 'eelatex-bounded 'eelatex 'ee-delimiter-percent)
(code-bounded 'eeeval-bounded  'eeeval  'ee-delimiter-semicolon)
(code-bounded 'eeb-eval        'eeeval  'ee-delimiter-semicolon)

;; Tests:
;; (find-code-bounded 'eev-bounded 'eev "\n#\n")
;; (find-code-bounded 'eev-bounded 'eev 'ee-delimiter-hash)




;;;      _       __             _ _   
;;;   __| | ___ / _| __ _ _   _| | |_ 
;;;  / _` |/ _ \ |_ / _` | | | | | __|
;;; | (_| |  __/  _| (_| | |_| | | |_ 
;;;  \__,_|\___|_|  \__,_|\__,_|_|\__|
;;;                                   
;; See: (find-bounded-intro "The default bounded function")

(defvar ee-bounded-function
  '(lambda () (error "ee-bounded-function not set"))
  "See: (find-bounded-intro)")

(defun ee-bounded-function ()
  "See: (find-bounded-intro)"
  (interactive)
  (funcall ee-bounded-function))

;; (define-key eev-mode-map [f3] 'ee-bounded-function)

(provide 'eev-bounded)










;;;        _               _      _       
;;;   ___ | |__  ___  ___ | | ___| |_ ___ 
;;;  / _ \| '_ \/ __|/ _ \| |/ _ \ __/ _ \
;;; | (_) | |_) \__ \ (_) | |  __/ ||  __/
;;;  \___/|_.__/|___/\___/|_|\___|\__\___|
;;;                                       
;; «obsolete»  (to ".obsolete")
;; Obsolete code that I don't want to delete yet
;; (mainly because the docstrings have some good ideas in them)

' (progn

(defun ee-add-quote (obj)
  "Return OBJ is OBJ is constant; else return 'OBJ."
  (if (or (numberp obj) (stringp obj)
	  (eq obj nil) (eq obj t) (keywordp obj))
      obj
    (list 'quote obj)))

(defun ee-pp0q (obj)
  "Like (ee-pp0 OBJ), but add a \"'\" in front if needed."
  (ee-pp0 (ee-add-quote obj)))

(defun ee-eeb-define-docstring
  (eeb-fun fun sdelim edelim flash-spec adjust extra-docs)
  "Used internally by `ee-eeb-define' to generate the docstring."
  (let ((args `(,eeb-fun ,fun ,sdelim ,edelim ,flash-spec ,adjust
		,@(if extra-docs (list extra-docs)))))
    (format "Run `%S' on a delimited region around point.
This is a wrapper function created by a sexp equivalent to first
one below (see `eeb-define'). To inspect the code that it
generates run the second sexp; and for an explanation of the
parameters, and a for a way of experimenting with them, see 
`eeb-define-try'.\n
  (eeb-define      %s)
  (find-eeb-define %s)%s"
  fun
  (mapconcat 'ee-pp0q args " ")
  (mapconcat 'ee-pp0q args " ")
  (if extra-docs (concat "\n\n" extra-docs) ""))))

(defun ee-eeb-define
  (eeb-fun fun sdelim &optional edelim flash-spec adjust extra-docs)
  "See `eeb-define' and `eeb-define-try'.
This function generates the code for defining EEB-FUN, as a string,
and returns it without `read'ing or `eval'ing it. An example:\n
  (find-estring (ee-eeb-define 'eev-bounded 'eev 'ee-delimiter-hash nil t t))"
  (format
   "(defun %S ()
  %S
  (interactive)
  (setq eeb-defaults '%s)
  (eeb-default-new))"
   eeb-fun 
   (ee-eeb-define-docstring
    eeb-fun fun sdelim edelim flash-spec adjust extra-docs)
   (ee-pp0 (list fun sdelim edelim flash-spec adjust))))

;; Tests:
;; (find-eeb-define 'eev-bounded 'eev "\n#\n" nil t t)
;; (find-eeb-define 'eev-bounded 'eev "\n#\n" nil t t "Example\nHere")
;; (eeb-define      'eev-bounded 'eev "\n#\n" nil t t)
;; (eeb-define      'eev-bounded 'eev "\n#\n" nil t t "Example\nHere")
;; (eeb-define      'eev-bounded 'eev 'ee-delimiter-hash nil t t "Example\nHere")
;; (find-efunctiondescr 'eev-bounded)

;; Note: the sexps in the docstring might come out wrong if they
;; contain nasty unibyte characters (this is a known possible bug).

(defun eeb-define
  (eeb-fun fun sdelim &optional edelim flash-spec adjust extra-docs)
  "Define EEB-FUN as a wrapper around FUN.
Use the delimiters SDELIM and EDELIM to find the region around
point where where FUN will operate; highlight the region using
FLASH-SPEC and ADJUST. If you want to add an example or extra
explanations to the docstring of EEB-FUN use EXTRA-DOCS.

See `eeb-define-try' for a detailed explanation of the parameters
and for a way of experimenting with them; see `find-eeb-define'
for a way to inspect to wrapper code."
  (eval (read (ee-eeb-define 
	       eeb-fun
	       fun sdelim edelim
	       flash-spec adjust extra-docs))))

(defun find-eeb-define (&rest rest)
  (find-estring (apply 'ee-eeb-define rest))
  (emacs-lisp-mode))


(defun eeb-define-try
  (eeb-fun fun sdelim &optional edelim flash-spec adjust extra-docs)
"This is similar to `eeb-define', but instead of defining EEB-FUN run it now.
The \"default action over bounded regions\" is determined by the
five entries in the list stored in the variable `eeb-defaults'
\(described below). All the \"bounded functions\", like
`eev-bounded', work by setting the variable `eeb-defaults' and
then calling the function `eeb-default-new', that interprets the
entries in `eeb-defaults' in a certain way and acts accordingly.


eeb-define
==========
Bounded functions like `eev-bounded' are defined by calling the
function `eeb-define' with the name of the function to define and
the five entries for the associated value for `eeb-defaults',
like this:

  (eeb-define 'eev-bounded 'eev 'ee-delimiter-hash nil t t)

`eeb-define-try' provides a nice way to test how functions
defined by `eeb-define' would behave after they are defined.
`eeb-define-try' expects the same arguments as `eeb-define', but
it ignores the first one - EEB-FUN -, and instead of defining a
function EEB-FUN that would set `eeb-defaults' and run
`eeb-default', it sets `eeb-defaults' immediately (temporarily,
using `let') and runs `eeb-default' on that.


eeb-defaults and eeb-default
============================
The variable `eeb-defaults' always holds a list of this form:

  (FUN SDELIM EDELIM FLASH-SPEC ADJUST)

where:
  FUN        is a function taking arguments \"s\" and \"e\", like `eev',
  SDELIM     is the starting delimiter (see `ee-edelim-adjust'),
  EDELIM     is the ending delimiter (default: same as sdelim),
  FLASH-SPEC tells how to highlight the region (see `eeflash-new'),
  ADJUST     should usually be t; see `ee-edelim-adjust'.

The \"default action on a delimited region\" is always something
composed of two \"standard actions\": first, highlight the region
temporarily, as described below; second, and most important, run
\"(FUN s e)\" on the region. FLASH-SPEC and ADJUST are only used
for the highlighting part; FUN is only used for the \"run (FUN s
e)\" part.

A nil at EDELIM means to use EDELIM := SDELIM; after
replacing the possible nil at EDELIM both SDELIM and
EDELIM are \"expanded\" with `ee-symbol-value' if their
values are symbols, and the results must be strings. Those
resulting strings are used as region delimiters by
`ee-sdelim-to-s' and `ee-edelim-to-e' to produce the \"s\" and
\"e\" arguments for the \"(FUN s e)\" call; see the documentation
for `ee-edelim-adjust' for an example that also shows how
ADJUST affects the highlighting.

A t at FLASH-SPEC means to use `eeflash-default' as FLASH-SPEC;
after treating the `t' case the value of FLASH-SPEC is
\"expanded\" with `ee-symbol-value' if it's a symbol, and the
result - that should be either nil or a list of the form \"(face
duration)\" - becomes temporarily the value of `ee-flash-spec',
and we invoke `eeflash-new' to highlight the region.


Examples
========
Here are some demos:\n
#.
# (eeb-define-try nil 'list            \"\\n#.\\n\" nil t t)
# (eeb-define-try nil 'ee-se-to-string \"\\n#.\\n\" nil t t)
# (eeb-define-try nil 'eeflash-new     \"\\n#.\\n\" nil t t)
# (eeb-define-try nil 'eev             \"\\n#.\\n\" nil t t)
echo $[1+2]
#.\n"
  (let ((eeb-defaults (list fun sdelim edelim flash-spec adjust)))
    (eeb-default-new)))

)





;; Local Variables:
;; coding:          utf-8-unix
;; no-byte-compile: t
;; End:
