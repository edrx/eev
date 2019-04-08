;;; eev-plinks.el -- elisp hyperlinks to invoke external processes.

;; Copyright (C) 2012,2018 Free Software Foundation, Inc.
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
;; Version:    2019jan25
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

;; Emacs can invoke external processes "synchronously",
;; "asynchronously", and as "command interpreters", as described here:
;;
;;   (find-elnode "Synchronous Processes" "waits for the process to" "terminate")
;;   (find-elnode "Asynchronous Processes" "runs in parallel")
;;   (find-enode "Interactive Shell" "M-x shell")
;;     (find-efile "comint.el" "shell-in-a-buffer")
;;
;; The most basic examples of each type in eev are:
;;
;;   synchronous:  `find-sh0',      `find-sh'
;;   asynchronous: `find-pdf-page', `find-firefox'
;;   comint:       `eepitch-shell'
;;
;; The low-level functions to create such processes are very different
;; in each of the three cases - and this file defines functions that
;; let us treat the three cases somewhat similarly. The main idea is
;; that the functions
;;
;;   `find-bgprocess-ne'
;;   `find-bgprocess'
;;   `find-callprocess00-ne'
;;   `find-callprocess0-ne'
;;   `find-callprocess-ne'
;;   `find-callprocess00'
;;   `find-callprocess0'
;;   `find-callprocess'
;;   `find-comintprocess-ne'
;;   `find-comintprocess'
;;
;; all receive an argument `program-and-args' that is either a string
;; or list of strings, and the functions `ee-split' and
;; `ee-split-and-expand' convert a `program-and-args' into a list of
;; strings. For example:
;;
;;   (ee-split              "xpdf   ~/LATEX/foo.pdf")
;;   (ee-split            '("xpdf" "~/LATEX/foo.pdf"))
;;
;; both return the list ("xpdf" "~/LATEX/foo.pdf"), and
;;
;;   (ee-split-and-expand   "xpdf   ~/LATEX/foo.pdf")
;;   (ee-split-and-expand '("xpdf" "~/LATEX/foo.pdf"))
;;
;; both return the list ("xpdf" "/home/edrx/LATEX/foo.pdf"). 
;;
;; The suffix `-ne' in the `find-xxx-ne' functions means "do not
;; expand", i.e., do not run `ee-expand' on each member of the list,
;; i.e., run `ee-split' instead of `ee-split-and-expand' on
;; `program-and-args'; and the suffixes `00' and `0' to the
;; `find-callprocess' functions work similarly to the suffixes `00'
;; and `0' in `find-sh'. Compare:
;;
;;   (find-sh00 "seq 9 12")
;;   (find-sh0  "seq 9 12")
;;   (find-sh   "seq 9 12")
;;
;; The `find-sh00' shows a newline-terminated string in the each area;
;; the `find-sh0' shows that string with the newline deleted; and the
;; `find-sh' shows it in a temporary buffer instead of in the acho
;; area.

;; «.find-bgprocess»		(to "find-bgprocess")
;; «.find-callprocess»		(to "find-callprocess")
;; «.find-callprocessregion»	(to "find-callprocessregion")
;; «.find-comintprocess»	(to "find-comintprocess")



;; See:
;; (find-eev "eepitch.el" "find-comintprocess-ne")
;; (find-eev "eev-blinks.el" "find-sh")
;; (find-node "(libc)Executing a File" "execv")

;; Obvious applications:
;; (find-eev "eev-pdflike.el")
;; (find-eev "eev-audiovideo.el")
;; (find-eev "eev-brxxx.el")


;; «.find-wget»		(to "find-wget")
;; «.find-gitk»		(to "find-gitk")
;; «.find-tkdiff»	(to "find-tkdiff")




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

;; Note that the functions `ee-split' and `ee-split-and-expand' are
;; defined in eepitch.el, because we want that library to be as
;; self-contained as possible; `find-comintprocess-ne' and
;; `find-comintprocess' are also defined there. See:
;;
;;   (find-eevfile "eepitch.el" "defun ee-split")
;;   (find-eevfile "eepitch.el" "defun find-comintprocess-ne")

(defun ee-unsplit (list) (if (listp list) (mapconcat 'identity list " ") list))
(defun ee-no-trailing-nl (str) (replace-regexp-in-string "\n$" "" str))



;;;  _                                             
;;; | |__   __ _ _ __  _ __ ___   ___ ___  ___ ___ 
;;; | '_ \ / _` | '_ \| '__/ _ \ / __/ _ \/ __/ __|
;;; | |_) | (_| | |_) | | | (_) | (_|  __/\__ \__ \
;;; |_.__/ \__, | .__/|_|  \___/ \___\___||___/___/
;;;        |___/|_|                                
;;
;; «find-bgprocess» (to ".find-bgprocess")
;;
(defun find-bgprocess-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (apply 'start-process (car argv) "*Messages*" argv)))

(defun find-bgprocess (program-and-args)
  (find-bgprocess-ne (ee-split-and-expand program-and-args)))



;;;            _ _                                   
;;;   ___ __ _| | |_ __  _ __ ___   ___ ___  ___ ___ 
;;;  / __/ _` | | | '_ \| '__/ _ \ / __/ _ \/ __/ __|
;;; | (_| (_| | | | |_) | | | (_) | (_|  __/\__ \__ \
;;;  \___\__,_|_|_| .__/|_|  \___/ \___\___||___/___/
;;;               |_|                                
;;
;; «find-callprocess» (to ".find-callprocess")
;;
(defun find-callprocess00-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (with-output-to-string
      (with-current-buffer standard-output
	(apply 'call-process (car argv) nil t nil (cdr argv))))))

(defun find-callprocess00 (program-and-args)
  (find-callprocess00-ne (ee-split-and-expand program-and-args)))

(defun find-callprocess0-ne (program-and-args)
  (ee-no-trailing-nl (find-callprocess00 program-and-args)))

(defun find-callprocess0 (program-and-args)
  (find-callprocess0-ne (ee-split-and-expand program-and-args)))

;; Like `find-sh', but more low-level.
;; See: (find-eev "eev-blinks.el" "find-eoutput")
;;      (find-eev "eev-blinks.el" "find-sh")
;;
(defun find-callprocess-ne (program-and-args &rest pos-spec-list)
  (apply 'find-eoutput-reuse (ee-unsplit program-and-args)
	 `(insert (find-callprocess00-ne ',program-and-args))
	 pos-spec-list))

(defun find-callprocess (program-and-args &rest pos-spec-list)
  (apply 'find-eoutput-reuse (ee-unsplit program-and-args)
	 `(insert (find-callprocess00 ',program-and-args))
	 pos-spec-list))


;;;                                                   _             
;;;  _ __  _ __ ___   ___ ___  ___ ___ _ __ ___  __ _(_) ___  _ __  
;;; | '_ \| '__/ _ \ / __/ _ \/ __/ __| '__/ _ \/ _` | |/ _ \| '_ \ 
;;; | |_) | | | (_) | (_|  __/\__ \__ \ | |  __/ (_| | | (_) | | | |
;;; | .__/|_|  \___/ \___\___||___/___/_|  \___|\__, |_|\___/|_| |_|
;;; |_|                                         |___/               
;;
;; «find-callprocessregion» (to ".find-callprocessregion")
;; Use this when program-and-args expects input from stdin.
;; Note: Code salvaged from a very old version of eev.
;; To do: write examples and test cases for this.
;; See: (find-efunctiondescr 'call-process-region)
;;      (find-elnode "Synchronous Processes" "Function: call-process-region")
;;
(defun find-callprocessregion-ne (program-and-args input)
  (let ((argv (ee-split program-and-args)))
    (with-temp-buffer
      (insert input)
      (apply 'call-process-region
	     (point-min) (point-max)
	     (car argv) 'delete t nil (cdr argv))
      (buffer-substring (point-min) (point-max)))))

(defun find-callprocessregion (program-and-args input)
  (find-callprocessregion-ne (ee-split-and-expand program-and-args)))



;;;                      _       _   
;;;   ___ ___  _ __ ___ (_)_ __ | |_ 
;;;  / __/ _ \| '_ ` _ \| | '_ \| __|
;;; | (_| (_) | | | | | | | | | | |_ 
;;;  \___\___/|_| |_| |_|_|_| |_|\__|
;;;                                  
;; «find-comintprocess» (to ".find-comintprocess")
;; `find-comintprocess-ne' and `find-comintprocess' are defined in
;; eepitch.el. See:
;;   (find-eevfile "eepitch.el" "defun find-comintprocess-ne ")
;;   (find-eevfile "eepitch.el" "defun find-comintprocess ")




;; «find-wget» (to ".find-wget")
;;
(defun find-wget00 (url)
  (find-callprocess00 `("wget" "-q" "-O" "-" ,url)))

(defun find-wget (url &rest rest)
  "Download URL with \"wget -q -O - URL\" and display the output."
  (setq url (ee-expand url))
  (apply 'find-eoutput-reuse (format "*wget: %s*" url)
	 `(insert (find-wget00 ,url))
	 rest))



;; «find-gitk» (to ".find-gitk")
;; Example: (find-eev-install-intro "find-gitk")
;;
(defun find-gitk (dir)
  "Run gitk in the directory DIR."
  (ee-at0 dir '(find-bgprocess "gitk --all --date-order")))


;; «find-tkdiff» (to ".find-tkdiff")
(defun find-tkdiff (f1 f2)
  (find-bgprocess `("tkdiff" ,f1 ,f2)))





(provide 'eev-plinks)







;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
