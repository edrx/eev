;;; eev-plinks.el -- elisp hyperlinks to invoke external processes.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012-2025 Free Software Foundation, Inc.
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
;; Version:    20250321
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-plinks.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-plinks.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                 <http://anggtwu.net/eev-intros/find-links-intro.html>
;;                                               (find-eev-intro)
;;                                               (find-links-intro)

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
;; `find-sh00' shows a newline-terminated string in the eecho area;
;; `find-sh0' shows that string with the last newline deleted; and
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


;; «.find-urlretrieve»		(to "find-urlretrieve")
;; «.find-wget»			(to "find-wget")
;; «.find-anggwget»		(to "find-anggwget")
;; «.find-anggwgeta»		(to "find-anggwgeta")
;; «.find-angg-public-copies»	(to "find-angg-public-copies")
;; «.find-gitk»			(to "find-gitk")
;; «.find-tkdiff»		(to "find-tkdiff")
;; «.find-osm»			(to "find-osm")
;; «.find-telegachat»		(to "find-telegachat")
;; «.find-telegachat-msgc»	(to "find-telegachat-msgc")
;; «.find-firefox»		(to "find-firefox")
;; «.find-googlechrome»		(to "find-googlechrome")
;; «.find-lynx»			(to "find-lynx")
;; «.find-lgrep»		(to "find-lgrep")
;; «.find-efunctionlgrep»	(to "find-efunctionlgrep")
;; «.find-clhsdoci»		(to "find-clhsdoci")




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
(defvar ee-find-callprocess00-exit-status nil)

(defun find-callprocess00-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (with-output-to-string
      (with-current-buffer standard-output
	(setq ee-find-callprocess00-exit-status
	      (apply 'call-process (car argv) nil t nil (cdr argv)))))))

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
;; Test: (find-callprocessregion "grep a" "a \n b \n aa \n c \n aaa")
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
  (find-callprocessregion-ne (ee-split-and-expand program-and-args) input))

(defun ee-callprocessregion (cmd)
  (interactive "sShell command: ")
  "Run CMD on the region and display the output in a temporary buffer.
This is similar to `shell-command-on-region'."
  (find-estring (find-callprocessregion cmd (ee-region))))




;;;                      _       _   
;;;   ___ ___  _ __ ___ (_)_ __ | |_ 
;;;  / __/ _ \| '_ ` _ \| | '_ \| __|
;;; | (_| (_) | | | | | | | | | | |_ 
;;;  \___\___/|_| |_| |_|_|_| |_|\__|
;;;                                  
;; «find-comintprocess» (to ".find-comintprocess")
;; `find-comintprocess-ne' and `find-comintprocess' are defined in
;; eepitch.el. See:
;;   (find-eev "eepitch.el" "find-comintprocess")



;;;             _                _        _                
;;;  _   _ _ __| |      _ __ ___| |_ _ __(_) _____   _____ 
;;; | | | | '__| |_____| '__/ _ \ __| '__| |/ _ \ \ / / _ \
;;; | |_| | |  | |_____| | |  __/ |_| |  | |  __/\ V /  __/
;;;  \__,_|_|  |_|     |_|  \___|\__|_|  |_|\___| \_/ \___|
;;;                                                        
;; «find-urlretrieve»  (to ".find-urlretrieve")
;; See: http://anggtwu.net/elisp/url-retrieve-test.el
;;             (find-angg "elisp/url-retrieve-test.el")
;; Tests:
;;   (find-urlretrieve00 "http://foo/bar")
;;   (find-urlretrieve00 "http://anggtwu.net/")
;;   (find-urlretrieve00 "http://anggtwu.net/doesnotexist")
;;   (find-estring (ee-urlretrieve0 "http://anggtwu.net/"))
;;   (find-estring (ee-urlretrieve0 "http://anggtwu.net/doesnotexist"))
;;
(defvar ee-urlretrieve-headers ""
  "The HTTP headers returned by the last call to `find-urlretrieve'.")

(defun ee-urlretrieve-header1 ()
  "Return the first line of `ee-urlretrieve-headers'."
  (replace-regexp-in-string "\n[^z-a]*" "" ee-urlretrieve-headers))

(defun ee-urlretrieve-ok ()
  "Check if the first line of `ee-urlretrieve-headers' is \"HTTP/1.1 200 OK\"."
  (equal "HTTP/1.1 200 OK" (ee-urlretrieve-header1)))

(defun ee-urlretrieve-assert-ok ()
  "Check if the first line of `ee-urlretrieve-headers' is \"HTTP/1.1 200 OK\".
If it is something else, throw an error."
  (if (not (ee-urlretrieve-ok))
      (error "Error: %s" (ee-urlretrieve-header1))))

(defun find-urlretrieve00 (url)
  "An internal function used by `find-urlretrieve'."
  (find-ebuffer
   (url-retrieve-synchronously url 'silent 'inhibit-cookies)
   "\n\n"))

;; 2021oct08: The functions below are broken - they corrupt non-ascii files.
;; See: https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00174.html
;;      (find-eev "eev-on-windows.el" "ee-download-with-eww")
;; TODO: fix them.

(defun ee-urlretrieve0 (url)
  "Use `url-retrieve-synchronously' to download URL.
When `url-retrieve-synchronously' is used for http or https it
returns a buffer containing the response headers, then a blank
line, then the contents (the \"message body\"). This function
saves the response headers in the variable
`ee-urlretrieve-headers', returns the message body, and deletes
the buffer.\n
This function doesn't perform any error checking and is as
simplistic as possible. Use it only to experiment with
`url-retrieve-synchronously'."
  (find-urlretrieve00 url)
  (setq ee-urlretrieve-headers
	(buffer-substring (point-min) (- (point) 2)))
  (prog1 (buffer-substring (point) (point-max))
    (kill-buffer (current-buffer))))

(defun ee-very-primitive-wget0 (url fname)
  "Try to download the contents of URL into FNAME.
If that works return the number of bytes written.
If that fails return nil or throw an error.
This is a quick hack."
  (let* ((contents (ee-urlretrieve0 url))
	 (ok (string-match "^HTTP/1.1 200 OK" ee-urlretrieve-headers)))
    (when ok
      (write-region contents nil (ee-expand fname))
      (string-bytes contents))))

(defun ee-very-primitive-wget1 (url)
  "This is like `ee-very-primitive-wget0', but always returns a string.
If URL is, say, http://foo.bar/plic/bletch.html then save its
contents in a file bletch.html in the current directory.
Return \"Downloaded nnn bytes\" in case of success and the HTTP
headers in case of error. This is a quick hack."
  (let* ((fname (replace-regexp-in-string ".*/" "" url))
         (rslt (ee-very-primitive-wget0 url fname)))
    (if rslt (format "Downloaded %d bytes" rslt)
      (format "Error:\n%s\n" ee-urlretrieve-headers))))

;; We can use `ee-very-primitive-wget1' to implement simplistic
;; versions of wget in eshell - the definition below is an example.
;; To make it work, run something like:
;;   (defalias    'eshell/wget 'ee-eshell/fakewget)
;; To delete it, do:
;;   (fmakunbound 'eshell/wget)
;; See:
;;   (find-eshellnode "Built-ins")
;;   (find-eshellgrep "grep --color -nH --null -e eshell/ *.el")
;;
(defun ee-eshell/fakewget (&rest args)
  (let* ((lastarg (nth (- (length args) 1) args)))
    (ee-very-primitive-wget1 lastarg)))




;;;   __ _           _                         _   
;;;  / _(_)_ __   __| |    __      ____ _  ___| |_ 
;;; | |_| | '_ \ / _` |____\ \ /\ / / _` |/ _ \ __|
;;; |  _| | | | | (_| |_____\ V  V / (_| |  __/ |_ 
;;; |_| |_|_| |_|\__,_|      \_/\_/ \__, |\___|\__|
;;;                                 |___/          
;;
;; «find-wget» (to ".find-wget")
;; `find-wget' is explained in this video:
;;   (find-2022findeevangghsubs "03:28")
;;   (find-2022findeevanggvideo "03:28")
;; See this thread about a bug on Windows:
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2024-01/msg00033.html
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2024-01/threads.html#00033

(defvar ee-wget-program "wget")

(defun ee-wget-downloading (url)
  (message "Wgetting: %s" url)
  (redisplay))

(defun find-wget00 (url)
  (find-callprocess00 `(,ee-wget-program "-q" "-O" "-" ,url)))

(defun find-wget0 (url &rest pos-spec-list)
  "Download URL with \"wget -q -O - URL\" and display the output.
If a buffer named \"*wget: URL*\" already exists then this
function visits it instead of running wget again.
If wget can't download URL then this function runs `error'."
  (interactive (browse-url-interactive-arg "URL: "))
  (let* ((eurl (ee-expand url))
	 (wgetprogandargs (list ee-wget-program "-q" "-O" "-" eurl))
	 (wgetbufname (format "*wget: %s*" eurl)))
    (if (get-buffer wgetbufname)
	(apply 'find-ebuffer wgetbufname pos-spec-list)
      ;;
      ;; If the buffer wgetbufname doesn't exist, then:
      (ee-wget-downloading url)
      (let* ((wgetoutput (find-callprocess00-ne wgetprogandargs))
	     (wgetstatus ee-find-callprocess00-exit-status))
	;;
	;; See: (find-node "(wget)Exit Status" "0" "No problems")
	(if (not (equal wgetstatus 0))
	    (error "wget can't download: %s" eurl))
	;;
	(find-ebuffer wgetbufname)	; create buffer
	(insert wgetoutput)
	(goto-char (point-min))
	(apply 'ee-goto-position pos-spec-list)))))

(defun find-wget (url &rest pos-spec-list)
  "Download URL with \"wget -q -O - URL\" and display the output.
If a buffer named \"*wget: URL*\" already exists then this
function visits it instead of running wget again.
If wget can't download URL then this function runs `error'.

The default definition for `find-wget' simply calls `find-wget0'.
If you need to adjust how `find-wget0' handles encodings,
redefine `find-wget' to make it call `find-wget0' inside a `let*'
block."
  (interactive (browse-url-interactive-arg "URL: "))
  (apply 'find-wget0 (ee-expand url) pos-spec-list))

(defun find-wget-mode (sexp url &rest pos-spec-list)
  "Like `find-wget', but runs SEXP in the buffer with the contents of URL.
SEXP is usually - but not necessarily - a sexp that sets major mode."
  (interactive (browse-url-interactive-arg "URL: "))
  (find-wget url)
  (eval sexp)
  (apply 'ee-goto-position pos-spec-list))

(defun find-wgeta (url &rest pos-spec-list)
  "Like `find-wget', but uses `ee-goto-anchor'."
  (interactive (browse-url-interactive-arg "URL: "))
  (find-wget url)
  (apply 'ee-goto-anchor pos-spec-list))

(defun find-wgeta-mode (sexp url &rest pos-spec-list)
  "Like `find-wget-mode', but uses `ee-goto-anchor'."
  (interactive (browse-url-interactive-arg "URL: "))
  (find-wget-mode sexp url)
  (apply 'ee-goto-anchor pos-spec-list))

(defun find-wget-elisp (url &rest pos-spec-list)
  "Like `find-wget', but puts the output buffer in emacs-lisp-mode."
  (interactive (browse-url-interactive-arg "URL: "))
  (apply 'find-wget-mode '(emacs-lisp-mode) url pos-spec-list))

(defun find-wgeta-elisp (url &rest pos-spec-list)
  "Like `find-wgeta', but puts the output buffer in emacs-lisp-mode."
  (interactive (browse-url-interactive-arg "URL: "))
  (apply 'find-wgeta-mode '(emacs-lisp-mode) url pos-spec-list))

(defun find-wget-org (url &rest pos-spec-list)
  "Like `find-wget-elisp', but puts the output buffer in org-mode."
  (interactive (browse-url-interactive-arg "URL: "))
  (apply 'find-wget-mode '(org-mode) url pos-spec-list))

;; Tests:
;; (find-wget  "http://anggtwu.net/eev-current/eev-plinks.el")
;; (find-wget  "http://anggtwu.net/eev-current/eev-plinks.el" "find-wget")
;; (find-wgeta "http://anggtwu.net/eev-current/eev-plinks.el" "find-wget")
;; (find-wget  "http://anggtwu.net/eev-current/DOESNOTEXIST")
;; (find-wget-elisp  "http://anggtwu.net/eev-current/eev-plinks.el" "find-wget")
;; (find-wgeta-elisp "http://anggtwu.net/eev-current/eev-plinks.el" "find-wget")
;; (find-wget-mode '(change-log-mode) "http://anggtwu.net/eev-current/ChangeLog")
;; (find-wget-org  "http://doc.norang.ca/org-mode.org")
;;
;; «find-anggwget»   (to ".find-anggwget")
;; «find-anggwgeta»  (to ".find-anggwgeta")
;; See: (find-2022findeevanggvideo "10:54" "Let me now show something else.")
;;      (find-2022findeevanggvideo "11:30"   "(find-angg-es-links)")
;;      (find-angg-es-links)

(defun find-anggwget (fname &rest pos-spec-list)
  "See `find-wget' and this: (find-angg-es-links)"
  (apply 'find-wget (concat "http://anggtwu.net/" fname) pos-spec-list))

(defun find-anggwgeta (fname &rest pos-spec-list)
  "See `find-wgeta' and this: (find-angg-es-links)"
  (apply 'find-wgeta (concat "http://anggtwu.net/" fname) pos-spec-list))

(defun find-anggwget-elisp (fname &rest pos-spec-list)
  "See `find-wget-elisp' and this: (find-angg-es-links)"
  (apply 'find-wget-elisp (concat "http://anggtwu.net/" fname) pos-spec-list))

(defun find-anggwgeta-elisp (fname &rest pos-spec-list)
  "See `find-wgeta-elisp' and this: (find-angg-es-links)"
  (apply 'find-wgeta-elisp (concat "http://anggtwu.net/" fname) pos-spec-list))

;; «find-angg-public-copies»  (to ".find-angg-public-copies")
;; The functions below are longer-named copies of these ones: 
;;   (find-angg-es-links 2 "public copies")
;;   (find-angg-es-links 2 "public copies" " find-angg ")
;;   (find-angg-es-links 2 "public copies" " find-anggfile ")
;;   (find-angg-es-links 2 "public copies" " find-es ")
;; The default for `find-angg' and `find-es' is still to yield errors
;; that say "`find-angg' and `find-es' not configured!". See:
;;   (find-eev "eev-tlinks.el" "find-angg-not-configured")
;; The functions below are only used in these tests:
;;   (find-eev "eev-htests.el" "tests" "find-angg-wget")
;;
(defun find-angg-wget (fname &rest rest)
  (apply 'find-wgeta (format "http://anggtwu.net/%s" fname) rest))
(defun find-anggfile-wget (fname &rest rest)
  (apply 'find-wget  (format "http://anggtwu.net/%s" fname) rest))
(defun find-es-wget (fname &rest rest)
  (apply 'find-wgeta (format "http://anggtwu.net/e/%s.e" fname) rest))





;; «find-gitk» (to ".find-gitk")
;; Tests: (find-git-intro "2. A first test")
;;        (find-eev-install-intro "find-gitk")
;;
(defun find-gitk (dir)
  "Run gitk in the directory DIR."
  (ee-at0 dir '(find-bgprocess "gitk --all --date-order")))

;; «find-tkdiff» (to ".find-tkdiff")
(defun find-tkdiff (f1 f2)
  (find-bgprocess `("tkdiff" ,f1 ,f2)))

;; «find-osm»  (to ".find-osm")
;; Tests: (find-osm  43.7731  11.2562 17 "Il Duomo")
;;        (find-osm -22.5014 -41.9259 15 "Near home")
;; Needs: (find-epackage-links 'osm)
;; See:   (find-eev "eev-tlinks.el" "find-osm-links")
;;
(defun find-osm (lat lon zoom &rest comments)
"Open a map. LAT, LON and ZOOM are the latitude, longitude, and zoom factor.
The COMMENTS are ignored. You need to have osm.el - OpenStreetMap
viewer - installed for this to work, and Emacs 28 or later."
  (require 'osm)
  (find-dbsw-call `(osm-goto ,lat ,lon ,zoom)))

;; Test: (find-osm-str "43.7731,11.2562,17" "Il Duomo")
;;
(defun find-osm-str (latlonzoomstr &rest comments)
  (apply 'find-osm (mapcar 'string-to-number
			   (split-string latlonzoomstr "[^-.0-9]+"))))

;; «find-telegachat»  (to ".find-telegachat")
;; Tests: (find-telegachat "@emacs_telega")
;;        (find-telegachat "@emacs_posts")
;;        (find-telegachat "@emacs_posts" "Emacs News and Posts")
;; See:   (find-eev "eev-tlinks.el" "find-telegachat-links")
;;
(defun find-telegachat (idn &rest rest)
  "Go to a Telega buffer or to a message in a Telega buffer.
When IDN is just the name of the numeric id of a Telegram
channel, like \"@emacs_telega\", just go to the buffer of that
channel, starting Telega and opening that channel if needed; when
IDN also has a message number, like in \"@emacs_telega#43590\",
also go to that message and highlight it."
  (let ((url (format "tg:telega:%s" idn)))
    (find-dbsw-call `(telega-tme-open-tg ,url))))

;; «find-telegachat-msgc»  (to ".find-telegachat-msgc")
;; Tests:             (find-telegachat      "@emacs_telega#43594")
;;                    (find-telegachat-msgc "@emacs_telega#43594")
;;        (find-eppp    (ee-telegachat-msg0 "@emacs_telega#43594"))
;;        (find-estring (ee-telegachat-msgc "@emacs_telega#43594"))
;;
(defun find-telegachat-msgc (idn &rest rest)
  "Like `find-telegachat', but shows the contents of the message."
  (apply 'find-estring (ee-telegachat-msgc idn) rest))

(defalias 'find-telegachatm 'find-telegachat-msgc)

(defun ee-telegachat-msg0 (idn)
  "An internal function used by `find-telegachat-msgc'."
  (save-window-excursion
    (find-telegachat idn)
    (telega-msg-at (point))))

(defun ee-telegachat-msgc (idn)
  "An internal function used by `find-telegachat-msgc'."
  (telega-msg-content-text (ee-telegachat-msg0 idn)))




;; «find-firefox»       (to ".find-firefox")
;; «find-googlechrome»  (to ".find-googlechrome")
;;
(defvar ee-firefox-program      "firefox")
(defvar ee-googlechrome-program "google-chrome")
(defun find-firefox      (url) (find-bgprocess `(,ee-firefox-program      ,url)))
(defun find-googlechrome (url) (find-bgprocess `(,ee-googlechrome-program ,url)))

;; «find-lynx»  (to ".find-lynx")
(defun find-lynx (url &rest pos-spec-list)
  (apply 'find-callprocess
	 `("lynx" "-dump" "-width=205" ,url) pos-spec-list))




;;;   __ _           _       _                      
;;;  / _(_)_ __   __| |     | | __ _ _ __ ___ _ __  
;;; | |_| | '_ \ / _` |_____| |/ _` | '__/ _ \ '_ \ 
;;; |  _| | | | | (_| |_____| | (_| | | |  __/ |_) |
;;; |_| |_|_| |_|\__,_|     |_|\__, |_|  \___| .__/ 
;;;                            |___/         |_|    
;;
;; «find-lgrep»  (to ".find-lgrep")
;; Tests: (find-lgrep 'brg "[ ']brg[ )]")
;;        (find-lgrep 'find-efile  "\"e\"")
;;     (ee-find-lgrep "/tmp/a.elc" "\"e\"")
;;
;; Used by: (find-eev "eev-blinks.el" "find-lgreps")
;;
(defun find-lgrep (f re)
  "Use lgrep to search for all occurrences of RE in a file.
The argument F is usually a symbol; it is converted to a filename
by:
  (symbol-file f-or-fname 'defun)
then if the filename ends in .elc, convert it to the
corresponding .el.

This function is used by `find-lgreps'."
  (grep-compute-defaults)
  (find-dbsw-call (ee-find-lgrep f re)))

(defun ee-find-lgrep (f re)
  "An internal function used by `find-lgrep'."
  (let* ((fname0  (if (symbolp f) (symbol-file f 'defun) f))
	 (fname   (ee-file-name-elc-to-el fname0))
         (dir     (file-name-directory    fname))
         (fname   (file-name-nondirectory fname)))
    `(lgrep ,re ,fname ,dir)))

(defun ee-file-name-elc-to-el (fname)
  "If FNAME ends with .elc, return the corresponding .el.
Return FNAME unchanged in the other cases.

Note: THIS IS CURRENTLY A PROTOTYPE! It doesn't work in the cases
in which the \".el\" and the corresponding \".elc\" are in
different directories... and so it doesn't work in packages
installed by straight.el."
  (replace-regexp-in-string ".elc$" ".el" fname))



;; «find-efunctionlgrep»  (to ".find-efunctionlgrep")
;; Tests: (ee-efunctionlgrep-re0 "foo")
;;        (ee-efunctionlgrep-re  "foo")
;;      (find-efunctionlgrep 'find-eetcfile)
;;      (find-efunctionlgrep 'find-eetcfile 'eetc)
;;      (find-efunctionlgrep 'find-efile 'e)
;;      (find-efunctionlgrep 'find-fline)
;;      (find-efunctionlgrep 'find-fline 'modify)
;;      (find-efunctionlgrep 'cl-struct-p--cmacro 'cl-struct-p)
;;
(defun find-efunctionlgrep (f &optional stem)
  "Use `find-lgrep' to find possible places in which F is defined.
See: (find-strange-functions-intro)"
  (interactive (find-function-read))
  (setq stem (or stem f))
  (let* ((fname0 (symbol-file f 'defun))
	 (fname  (ee-file-name-elc-to-el fname0))
	 (re     (ee-efunctionlgrep-re stem)))
    (find-lgrep fname re)))

(defun ee-efunctionlgrep-re  (stem)
  "An internal function used by `find-efunctionlgrep'."
  (eval (ee-efunctionlgrep-re0 stem)))

(defun ee-efunctionlgrep-re0 (stem)
  "An internal function used by `find-efunctionlgrep'."
  (setq stem (format "%s" stem))
  `(rx (or (and "\"" ,stem "\"")
	   (and (any "( '")
		,stem
		(group (or (any " )") eol))
		;;     (or (any " )") eol)  ; <- doesn't work
		))))



;; «find-clhsdoci»  (to ".find-clhsdoci")
;; Needs: (find-try-sly-intro "1. Install the Common Lisp Hyperspec")
;;        (find-try-sly-intro "2. Install some elisp packages")
;;   Try: (ee-clhs-lookup-index "car")
;;        (find-clhsdoci        "car")
;;        (find-ehashtable (clhs-symbols))
;;
(defun find-clhsdoci (clhsname &rest rest)
  "Open the page of the Common Lisp Hyperspec corresponding to CLHSNAME.
This function uses `ee-clhs-lookup-index' and `find-clhsdoc'."
  (interactive (list (ee-lisp-symbol-around-point-ask)))
  (let* ((str0 (ee-clhs-lookup-index clhsname))
	 (str (replace-regexp-in-string "\\.html?$" "" str0)))
    (find-clhsdoc str)))

(defun ee-clhs-lookup-index (clhsname)
  "Use the hash table (clhs-symbols) to convert CLHSNAME to (part of) a URL.
This is an internal function used by `find-clhsdoci'."
  (require 'clhs)
  (gethash (upcase clhsname) (clhs-symbols)))





(provide 'eev-plinks)







;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
