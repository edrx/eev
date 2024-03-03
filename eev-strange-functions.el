;;; eev-strange-functions.el -- Support for functions defined in strange ways.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
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
;; Version:    20240303
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-strange-functions.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-strange-functions.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-strange-functions-intro.html>
;;                                               (find-strange-functions-intro)

;;; Comment:

;; See: (find-strange-functions-intro)

;; (load (buffer-name))

;; «.find-sf-links»	(to "find-sf-links")
;; «.get-sexp»		(to "get-sexp")
;; «.hprog»		(to "hprog")
;; «.hprog-tools»	(to "hprog-tools")
;;   «.regexps»		(to "regexps")
;; «.hprog-cases»	(to "hprog-cases")
;;   «.1stclassvideo»	(to "1stclassvideo")
;;   «.codecd»		(to "codecd")
;;   «.pdf»		(to "pdf")


(defvar ee-sf-sexp)
(defvar ee-sf-stem)
(defvar ee-sf-suffix)
(defvar ee-sf-result)



;; TODO: move to: (find-eev "eev-wrap.el" "ee-S")
;; Tests:
;; (ee-Qp 42)
;; (ee-Qp 'a)
;; (ee-Qp :ka)
;; (ee-Qrest '(:ka (2 3)))
;; See: (find-efunction 'ee-S)


;;;   __ _           _            __       _ _       _        
;;;  / _(_)_ __   __| |      ___ / _|     | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____/ __| |_ _____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____\__ \  _|_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |___/_|       |_|_|_| |_|_|\_\___/
;;;                                                           
;; «find-sf-links»  (to ".find-sf-links")
;; Tests: (find-sf-links '(find-eev2021video "2:34"))
;;        (find-sf-links '(find-eev2021blabl "2:34"))
;;  (find-sf-debug-links '(find-eev2021video "2:34"))
;;
;; (find-eev2021video)
;; (find-eev2021video "2:34")
;; (find-eevfile "")

(defun find-sf-links (sexp)
  (interactive (ee-sf-get-sexp-flash))
  (setq ee-sf-sexp   sexp)
  (setq ee-sf-stem   nil)
  (setq ee-sf-suffix nil)
  (setq ee-sf-result nil)
  (ee-sf-run-hprog))

;; (find-eev "eev-hlinks.el" "ee-find-here-debug-links")

(defun find-sf-debug-links (sexp)
  (interactive (ee-sf-get-sexp-flash))
  (setq ee-sf-sexp   sexp)
  (setq ee-sf-stem   nil)
  (setq ee-sf-suffix nil)
  (setq ee-sf-result nil)
  (ee-sf-run-hprog-first-half)
  (find-elinks
   `("Hello"
     ,(ee-template0 "\
# (find-sf-debug-links {(ee-Q sexp)})

# The last call to
#     '(find-here-links ARG)
#  -> '(ee-detect-here)
#  -> '(ee-hlang-run ee-hprog-find-here-links)
# produced this:
#   ee-hlang-sexp1  =>  {(ee-S ee-hlang-sexp1)}
#   ee-hlang-sexp2  =>  {(ee-S ee-hlang-sexp2)}
# See:
#   ee-hlang-sexp1
#   ee-hlang-sexp2
#   (find-efunction '{(car ee-hlang-sexp1)})
#   (find-efunction '{(car ee-hlang-sexp2)})
#   (find-eev \"eev-hlinks.el\" \"find-here-links\")
#   (find-eev \"eev-hlinks.el\" \"find-here-links\" \"If ARG\")
#   (find-eev \"eev-hlinks.el\" \"ee-find-here-links\")
#   (find-eev \"eev-hlinks.el\" \"ee-find-here-debug-links\")
#   (find-eev \"eev-hlinks.el\" \"ee-hlang-run\")
#   (find-eev \"eev-hlinks.el\" \"ee-hlang-run\" \"ee-detect-here\")
#   (find-eev \"eev-hlinks.el\" \"ee-hprog-find-here-links\")

# ee-sf-sexp:     {(ee-S ee-sf-sexp)}
# ee-hlang-sexp1: {(ee-S ee-hlang-sexp1)}
# ee-hlang-sexp2: {(ee-S ee-hlang-sexp2)}

# ee-sf-stem:     {(ee-S ee-sf-stem)}
# ee-sf-suffix:   {(ee-S ee-sf-suffix)}
# ee-sf-result:   {(ee-S ee-sf-result)}
")
     )))

(defun find-strange-function-links (dbg)
  (interactive "P")
  (if dbg
      (find-sf-debug-links (ee-sf-get-sexp-flash))
    (find-sf-links (ee-sf-get-sexp-flash))))




;;;             _                            
;;;   __ _  ___| |_      ___  _____  ___ __  
;;;  / _` |/ _ \ __|____/ __|/ _ \ \/ / '_ \ 
;;; | (_| |  __/ ||_____\__ \  __/>  <| |_) |
;;;  \__, |\___|\__|    |___/\___/_/\_\ .__/ 
;;;  |___/                            |_|    
;;
;; «get-sexp»  (to ".get-sexp")
;; Based on: (find-efunction 'ee-eval-last-sexp-0)
;;           (find-efunction 'ee-eval-last-sexp-1)
;;     Test: (ee-sf-get-sexp-flash)
;;
(defun ee-sf-get-sexp-flash ()
  "Highlight (\"flash\") the sexp before eol and return it.
This is similar to `M-0 M-e': we go to the end of the line and
then we return the sexp before point."
  (ee-goto-eol)
  (save-excursion
    (eeflash+ (ee-backward-sexp)
	      (ee-forward-sexp)
	      ee-highlight-spec))
  (read (ee-last-sexp)))






;;;  _                           
;;; | |__  _ __  _ __ ___   __ _ 
;;; | '_ \| '_ \| '__/ _ \ / _` |
;;; | | | | |_) | | | (_) | (_| |
;;; |_| |_| .__/|_|  \___/ \__, |
;;;       |_|              |___/ 
;; See:
;; (find-eev "eev-kl-here.el" "hprog")
;; (find-efunction 'klapt)
;; (find-efunction 'ee-kl-sexp-at-eol-p)
;; «hprog»  (to ".hprog")

(setq ee-hprog-for-sf
 '(:or
   (:if (ee-sf-1stclassvideo-p)     (find-sf-elinks-elisp))
   (:if (ee-sf-codecd-p)            (find-sf-elinks-elisp))
   (:if (ee-sf-pdf-p)               (find-sf-elinks-elisp))
   ;;
   (:if t (error "Sexp does not start with a strange function! Try `M-1 M-h M-s'"))
   ))

;; See: (find-efunction 'ee-hlang-run)
;;      (find-efunction 'ee-detect-here)
;;      (find-eev "eev-kl-here.el" "kl")
(defun ee-sf-run-hprog-first-half ()
  (ee-hlang-run ee-hprog-for-sf))

(defun ee-sf-run-hprog ()
  (ee-sf-run-hprog-first-half)
  (eval ee-hlang-sexp2))



;;;  _                             _              _     
;;; | |__  _ __  _ __ ___   __ _  | |_ ___   ___ | |___ 
;;; | '_ \| '_ \| '__/ _ \ / _` | | __/ _ \ / _ \| / __|
;;; | | | | |_) | | | (_) | (_| | | || (_) | (_) | \__ \
;;; |_| |_| .__/|_|  \___/ \__, |  \__\___/ \___/|_|___/
;;;       |_|              |___/                        
;;
;; «hprog-tools»  (to ".hprog-tools")
;;
(defun find-sf-elinks-elisp ()
  (find-elinks-elisp
   `(,(ee-template0 "\
;; (find-sf-links '{(ee-S ee-sf-sexp)})
;; See: (find-strange-functions-intro \"2. Here\")
")
     ,@(eval ee-sf-result))))

;; «regexps»  (to ".regexps")
;; Tests: (ee-sf-make-find-rx  '("video" "hsubs" "lsubs"))
;;        (ee-sf-make-find-re  '("video" "hsubs" "lsubs"))
;;        (ee-sf-match-find-re 'find-foovideo '("video" "hsubs" "lsubs"))
;;
(defun ee-sf-make-find-rx (suffixes)
  `(rx bos "find-" (group (+ any)) (group (or ,@suffixes)) eos))

(defun ee-sf-make-find-re (suffixes)
  (eval (ee-sf-make-find-rx suffixes)))

(defun ee-sf-match-find-re (f suffixes)
  "Check if F is a symbol of the form `find-<stem><suffix>'.
For example, if SUFFIXES is '(\"foo\" \"bar\"), test if F is a
symbol of the form `find-<stem>foo' or `find-<stem>bar'; and if F
is `find-blahfoo' then set `ee-sf-stem' to \"blah\" and set
`ee-sf-suffix' to \"foo\", and return \"blah\"."
  (let ((re (ee-sf-make-find-re suffixes)))
    (and (symbolp f)
	 (string-match re (symbol-name f))
	 (setq ee-sf-suffix (match-string 2 (symbol-name f)))
	 (setq ee-sf-stem   (match-string 1 (symbol-name f))))))



;;;  _   _                                                 
;;; | | | |_ __  _ __ ___   __ _    ___ __ _ ___  ___  ___ 
;;; | |_| | '_ \| '__/ _ \ / _` |  / __/ _` / __|/ _ \/ __|
;;; |  _  | |_) | | | (_) | (_| | | (_| (_| \__ \  __/\__ \
;;; |_| |_| .__/|_|  \___/ \__, |  \___\__,_|___/\___||___/
;;;       |_|              |___/                           
;;
;; «hprog-cases»  (to ".hprog-cases")


;;;  _     _       _                     _     _            
;;; / |___| |_ ___| | __ _ ___ _____   _(_) __| | ___  ___  
;;; | / __| __/ __| |/ _` / __/ __\ \ / / |/ _` |/ _ \/ _ \ 
;;; | \__ \ || (__| | (_| \__ \__ \\ V /| | (_| |  __/ (_) |
;;; |_|___/\__\___|_|\__,_|___/___/ \_/ |_|\__,_|\___|\___/ 
;;;                                                         
;; «1stclassvideo»  (to ".1stclassvideo")
;; Tests: (ee-sf-1stclassvideo-stem 'find-FOOhsubs)
;;        (setq ee-sf-sexp '(find-FOOvideo "2:34" "aa" "bb"))
;;        (ee-sf-1stclassvideo-p)

(defvar ee-sf-1stclassvideo-functions
  '(find-1stclassvideo-def
    find-1stclassvideo-index
    find-1stclassvideo-links
    find-code-1stclassvideo
    code-1stclassvideo))

(defun ee-sf-1stclassvideo-stem (f)
  (ee-sf-match-find-re f '("video" "hsubs" "lsubs")))

(defun ee-sf-1stclassvideo-p ()
  "Check if ee-sf-sexp is a strange sexp related to 1stclassvideos."
  (pcase ee-sf-sexp
    ((and `(,f ,time . ,rest)
	  (let stem (ee-sf-1stclassvideo-stem f))
	  (guard (stringp stem)))
     (setq ee-sf-result `(ee-sf-1stclassvideo-links ,stem ,time . ,rest)))
    ((and `(,f)
	  (let stem (ee-sf-1stclassvideo-stem f))
	  (guard (stringp stem)))
     (setq ee-sf-result `(ee-sf-1stclassvideo-links ,stem)))
    ((and `(,f ,stem . ,rest)
	  (guard (member f ee-sf-1stclassvideo-functions)))
     (setq ee-sf-result `(ee-sf-1stclassvideo-links ,stem . ,rest)))))

;; Tests (use `M-x sf'):
;;   (find-eevnavvideo "0:31" "0.1. M-x package-initialize")
;;
(defun ee-sf-1stclassvideo-links (c &optional time &rest rest)
  (let* ((qrest       (ee-Qrest rest))
	 (basicinfo   (ee-1stclassvideo-basicinfo  c time))
	 (basicsexps  (ee-1stclassvideo-basicsexps c time))
	 (defuns      (ee-1stclassvideo-defuns c)))
    `(,(ee-template0 "\
;; Variants:
{basicsexps}\

;; Info about this video:
{basicinfo}\

;; Source and location in the load-history:
;; (find-efunctionlgrep 'find-{c}video \"{c}\")
;; (find-eloadhistory-for 'find-{c}video)
;; (find-eloadhistory-for 'find-{c}video 2 \" find-{c}video)\")

;; Defuns:
{defuns}\
"))))


;;;                _                          _ 
;;;   ___ ___   __| | ___        ___       __| |
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |
;;;  \___\___/ \__,_|\___|      \___|     \__,_|
;;;                                             
;; «codecd»  (to ".codecd")
;; Tests: (ee-sf-codecd-stem 'find-foofile)
;;        (ee-sf-codecd-d 'eev)
;;        (setq ee-sf-sexp '(find-eevfile "ChangeLog"))
;;        (setq ee-sf-sexp '(find-eev     "ChangeLog"))
;;        (setq ee-sf-sexp '(find-enode   "Keys"))
;;        (ee-sf-codecd-p)

(defvar ee-sf-codecd-functions
  '(find-code-c-d
    code-c-d))

(defun ee-sf-codecd-stem (f)
  (ee-sf-match-find-re f '("file" "sh" "sh0" "grep" "node")))

(defun ee-sf-codecd-stem0 (f)
  (ee-sf-match-find-re f '("")))

(defun ee-sf-codecd-d (c)
  (let ((symbol (ee-intern "ee-%sdir" c)))
    (and (boundp symbol)
	 (symbol-value symbol))))

(defun ee-sf-codecd-p ()
  (pcase ee-sf-sexp
    ((and `(,f . ,rest)
	  (let c (ee-sf-codecd-stem f))
	  (guard (stringp c))
	  (let d (ee-sf-codecd-d c))
	  (guard (stringp d)))
     (setq ee-sf-result `(ee-sf-codecd-links ,c ,d)))
    ((and `(,f . ,rest)
	  (let c (ee-sf-codecd-stem0 f))
	  (guard (stringp c))
	  (let d (ee-sf-codecd-d c))
	  (guard (stringp d)))
     (setq ee-sf-result `(ee-sf-codecd-links ,c ,d)))
    ((and `(,f ,c ,d . ,rest)
	  (guard (member f ee-sf-codecd-functions)))
     (setq ee-sf-result `(ee-sf-codecd-links ,c ,d)))))


;; (find-elinks-elisp (ee-sf-codecd-links "CC" "DD"))
;; (find-elinks-elisp (ee-sf-codecd-links "eev"))
;; (find-eevfile "")
;; (code-c-d "foo" "bar")

(defun ee-sf-codecd-links (c d)
  "Check if ee-sf-sexp is a strange sexp related to `code-c-d'."
  (let* ((defuns      (ee-code-c-d c d)))
    `(,(ee-template0 "\
;; Variants:
;; (find-{c}file \"\")
;; (find-{c}sh   \"pwd\")
;; (find-{c}sh0  \"pwd\")
;; (find-{c}node \"\")

;; Source and location in the load-history:
;; (find-efunctionlgrep 'find-{c}file '{c})
;; (find-eloadhistory-for 'find-diagxyfile)
;; (find-eloadhistory-for 'find-diagxyfile 2 \" ee-diagxyfile)\")

;; Defuns (incomplete and without the overrides):

{defuns}\
"))))



;;;            _  __ 
;;;  _ __   __| |/ _|
;;; | '_ \ / _` | |_ 
;;; | |_) | (_| |  _|
;;; | .__/ \__,_|_|  
;;; |_|              
;;
;; «pdf»  (to ".pdf")

;; Tests: (ee-sf-pdf-stem 'find-foofile)
;;        (ee-sf-pdf-d 'eev)
;;        (setq ee-sf-sexp '(find-eevfile "ChangeLog"))
;;        (setq ee-sf-sexp '(find-eev     "ChangeLog"))
;;        (setq ee-sf-sexp '(find-enode   "Keys"))
;;        (ee-sf-pdf-p)

(defvar ee-sf-pdf-functions
  '(find-code-pdf-page
    find-code-pdf-text
    find-code-pdf-text8
    code-pdf-page
    code-pdf-text
    code-pdf-text8))

(defun ee-sf-pdf-stem (f)
  (ee-sf-match-find-re f '("page" "text")))

(defun ee-sf-pdf-file (c)
  "If C is \"FOO\" and (find-FOOpage) opens BAR.pdf, return \"BAR.pdf\"."
  (let ((symbol (ee-intern "ee-%spdf" c)))
    (and (boundp symbol)
	 (symbol-value symbol))))

(defun ee-sf-pdf-p ()
  "Check if ee-sf-sexp is a strange sexp related to PDFs."
  (pcase ee-sf-sexp
    ((and `(,f . ,rest)
	  (let c (ee-sf-pdf-stem f))
	  (guard (stringp c))
	  (let fname (ee-sf-pdf-file c))
	  (guard (stringp fname)))
     (setq ee-sf-result `(ee-sf-pdf-links ,c ,fname)))
    ((and `(,f ,c ,fname . ,rest)
	  (guard (member f ee-sf-pdf-functions)))
     (setq ee-sf-result `(ee-sf-pdf-links ,c ,fname)))))


;; (find-elinks-elisp (ee-sf-pdf-links "CC" "DD"))
;; (find-elinks-elisp (ee-sf-pdf-links "eev"))
;; (find-eevfile "")
;; (code-c-d "foo" "bar")

(defun ee-sf-pdf-links (c fname)
  (let* ((defuns1  (ee-code-pdf-page c fname))
	 (defuns2  (ee-code-pdf-text c fname))
	 (page0    (cadr ee-sf-sexp))
	 (page     (if page0 (format " %s" (ee-S page0)) ""))
	 (pagerest (format "%s%s" page (ee-Qrest (cddr ee-sf-sexp))))
	 )
    `(,(ee-template0 "\
;; Variants:
;; (find-{c}page)
;; (find-{c}text)
;; (find-{c}page{page})
;; (find-{c}text{page})
;; (find-{c}page{pagerest})
;; (find-{c}text{pagerest})

;; Source and location in the load-history:
;; (find-efunctionlgrep 'find-{c}page \"{c}\")
;; (find-eloadhistory-for 'find-{c}page)
;; (find-eloadhistory-for 'find-{c}page 2 \" find-{c}page)\")

;; Some defuns (recreated - may be wrong):

{defuns1}
{defuns2}
"))))




(provide 'eev-strange-functions)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
