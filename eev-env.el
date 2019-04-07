;;; eev-env.el -- set some environment variables.

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
;; Version:    2019mar02
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-env.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-env.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-psne-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-prepared-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-psne-intro)
;;                                                (find-prepared-intro)

;;; Commentary:

;; Used by: (find-eev "eev-prepared.el")
;;
;; Related but obsolete files:
;;   <http://angg.twu.net/eev-current/README.html>
;;   <http://angg.twu.net/eev-current/eev-langs.el.html>
;;   <http://angg.twu.net/eev-current/eev-rctool.html>





;;;   ___ _ ____   __  __   ____ _ _ __ ___ 
;;;  / _ \ '_ \ \ / /  \ \ / / _` | '__/ __|
;;; |  __/ | | \ V /    \ V / (_| | |  \__ \
;;;  \___|_| |_|\_/      \_/ \__,_|_|  |___/
;;;
;;; Set some environment variables (for ee-expand, getenv,
;;; shell buffers, xterms started from Emacs, etc).

;; (find-eevrcfile ".bashrc")
;; (find-eevrcfile ".zshrc")

;; Note: `ee-setenv' and its calls for "S" and "EEVDIR" were moved to:
;;   (find-eev "eev.el" "ee-setenv")
;;
(defun ee-setenv (envvar value)
  "In case the environment variable ENVVAR was not set set it to VALUE."
  (if (null (getenv envvar))
      (setenv envvar (ee-expand value))))

(ee-setenv "S" "~/snarf")       ; for `find-psne-links'

(ee-setenv "EEVDIR"
           (let ((fname (locate-library "eev")))
             (if fname (directory-file-name (file-name-directory fname))
               "~/eev-current")))       ; eev.el, etc




;;;                          _ _   _                           
;;;   ___  ___     __      _(_) |_| |__         ___ _ ____   __
;;;  / _ \/ _ \____\ \ /\ / / | __| '_ \ _____ / _ \ '_ \ \ / /
;;; |  __/  __/_____\ V  V /| | |_| | | |_____|  __/ | | \ V / 
;;;  \___|\___|      \_/\_/ |_|\__|_| |_|      \___|_| |_|\_/  
;;;                                                            
;; `ee-with-env' run a sexp in a modified environment.
;; Tests:
;;
;; (ee-with-env '(("FOO" "~/foo") ("FOOL" "oo")) '(find-sh0 "set | grep -a FOO"))
;; (ee-with-env '(("FOO" "~/foo")  "FOOL=oo")    '(find-sh0 "set | grep -a FOO"))
;; (ee-with-env    "FOO=~/foo       FOOL=oo"     '(find-sh0 "set | grep -a FOO"))
;; (ee-with-env    ""                            '(find-sh0 "set | grep -a FOO"))
;;
;; (ee-with-env0 '("A" "B=" "C=~/foo" ("D") ("E" "") ("F" "$SHELL")))
;;   --> ("A" "B=" "C=/home/edrx/foo" "D" "E=" "F=/usr/bin/zsh")
;; (ee-with-env0 "A B= C=~/foo")
;;   --> ("A" "B=" "C=/home/edrx/foo")
;;
;; See:
;; (find-evardescr 'process-environment "without \"=VALUE\"")
;; (find-evardescr 'process-environment "first one")

(defun ee-with-env (changes code)
  "Run the sexp CODE with the changes CHANGES in the environment.
CHANGES can be a list of (\"VAR\" \"VALUE\") pairs, a list of
\"VAR=VALUE\" strings, or a string that is split into
\"VAR=VALUE\" substrings at whitespace. Each \"VALUE\" is
expanded with `ee-expand'.\n
See the source for details, examples, and tests."
  (eval `(let ((process-environment
		(append (ee-with-env0 ',changes) process-environment)))
	   ,code)))

(defun ee-with-env0 (changes)
  "An internal function used by `ee-with-env'. See the source."
  (mapcar 'ee-with-env00 (if (stringp changes) (ee-split changes) changes)))

(defun ee-with-env00 (change)
  "An internal function used by `ee-with-env'. See the source."
  (if (stringp change)			              
      ;; cases "A", "B=", "C=...":
      (let ((pos (string-match "=" change)))
	(if (not pos)				      
	    change                                   ; case "A"
	  (let ((var (substring change 0 pos))        
		(val (substring change (1+ pos))))
	    (format "%s=%s" var (ee-expand val)))))  ; cases "B=" and "C=..."
    ;; cases ("D"), ("E" ""), ("F" "$FOO"):
    (if (not (cdr change))
	(car change)			              
      (format "%s=%s" (car change) (ee-expand (cadr change))))))




(provide 'eev-env)






;; Local Variables:
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
