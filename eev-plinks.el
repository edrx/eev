;;; eev-plinks.el -- elisp hyperlinks to invoke external processes.

;; Copyright (C) 2012 Free Software Foundation, Inc.
;;
;; This file is (not yet?) part of GNU eev.
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
;; Version:    2013aug28
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-plinks.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-plinks.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-links-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-links-intro)

;;; Commentary:

;; See:
;; (find-eev "eepitch.el" "find-comintprocess-ne")
;; (find-eev "eev-blinks.el" "find-sh")
;; (find-node "(libc)Executing a File" "execv")

;; Obvious applications:
;; (find-eev "eev-pdflike.el")
;; (find-eev "eev-audiovideo.el")
;; (find-eev "eev-brxxx.el")



;;;                                              
;;;  _ __  _ __ ___   ___ ___  ___ ___  ___  ___ 
;;; | '_ \| '__/ _ \ / __/ _ \/ __/ __|/ _ \/ __|
;;; | |_) | | | (_) | (_|  __/\__ \__ \  __/\__ \
;;; | .__/|_|  \___/ \___\___||___/___/\___||___/
;;; |_|                                          
;;

;; 2007sep29: Copied these functions from eev-mini.el to here...
;; In a near future all calls to external processes in eev will happen
;; through these functions... mainly because (1) they accept their
;; "program-and-args" argument as either a string (to be split at
;; whitespace) or as a list of strings, (2) they can either expand
;; each "word" or "program-and-args" with ee-expand or keep all words
;; unchanged, (3) they're short and clean.

;; Sorry, no docstrings yet... some tests:
;; (find-callprocess0  '("lua51" "-e" "print(1+2)"))
;; (find-callprocess00 '("lua51" "-e" "print(1+2)"))

;; Suffixes:
;; "-ne" means "(do) not ee-expand"
;; "0"  means "don't display in a temp buffer, just return the string"
;; "00" means "like `0', but more low-level: don't strip the trailing newline".

(defun ee-split   (str) (if (stringp str) (split-string str "[ \t\n]+") str))
(defun ee-unsplit (list) (if (listp list) (mapconcat 'identity list " ") list))
(defun ee-split-and-expand (str) (mapcar 'ee-expand (ee-split str)))
(defun ee-no-trailing-nl   (str) (replace-regexp-in-string "\n$" "" str))

(defun find-bgprocess-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (apply 'start-process (car argv) "*Messages*" argv)))

(defun find-callprocess00-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (with-output-to-string
      (with-current-buffer standard-output
	(apply 'call-process (car argv) nil t nil (cdr argv))))))

(defun find-callprocess0-ne (program-and-args)
  (ee-no-trailing-nl (find-callprocess00 program-and-args)))

(defun find-comintprocess-ne (name program-and-args)
  (let ((argv (ee-split program-and-args)))
    (apply 'make-comint name (car argv) nil (cdr argv))
    (switch-to-buffer (format "*%s*" name))))

(defun find-bgprocess     (program-and-args)
  (find-bgprocess-ne      (ee-split-and-expand program-and-args)))
(defun find-callprocess00 (program-and-args)
  (find-callprocess00-ne  (ee-split-and-expand program-and-args)))
(defun find-callprocess0  (program-and-args)
  (find-callprocess0-ne   (ee-split-and-expand program-and-args)))
(defun find-callprocessregion (program-and-args input)
  (find-callprocessregion (ee-split-and-expand program-and-args)))
(defun find-comintprocess (name program-and-args)
  (find-comintprocess-ne   name (ee-split-and-expand program-and-args)))

;; These two are like `find-sh', but more low-level.
(defun find-callprocess-ne (program-and-args &rest pos-spec-list)
  (apply 'find-eoutput-reuse (ee-unsplit program-and-args)
	 `(insert (find-callprocess00-ne ',program-and-args))
	 pos-spec-list))
(defun find-callprocess (program-and-args &rest pos-spec-list)
  (apply 'find-eoutput-reuse (ee-unsplit program-and-args)
	 `(insert (find-callprocess00 ',program-and-args))
	 pos-spec-list))




;; New, 2008jan02
;; Compare with `ee-find-grep'

(defun ee-find-comintprocess-ne (dir name program-and-args)
  (switch-to-buffer
   (with-temp-buffer
     (cd dir)
     (find-comintprocess-ne name program-and-args)
     (current-buffer))))

(defun ee-find-comintprocess (dir name program-and-args)
  (ee-find-comintprocess-ne
   (ee-expand dir) name (ee-split-and-expand program-and-args)))



;; find-wget
;;
(defun find-wget00 (url)
  (find-callprocess00 `("wget" "-q" "-O" "-" ,url)))

(defun find-wget (url &rest rest)
  (setq url (ee-expand url))
  (apply 'find-eoutput-reuse (format "*wget: %s*" url)
	 `(insert (find-wget00 ,url))
	 rest))



(provide 'eev-plinks)







;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
