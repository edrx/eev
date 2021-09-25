;;; eev-hlinks.el --- `find-here-links' and variants.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.
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
;; Version:    20201231
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-hlinks.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-hlinks.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-here-links-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-here-links-intro)

;;; Commentary:

;; This file implements `find-here-links', its variants for beginners,
;; and the many low-level functions that are needed to make them work.
;;
;; `find-here-links' generates a temporary buffer with links to
;; "here". There are several kinds of "here"s - see:
;;
;;   (find-here-links-intro "3. `find-here-links'")
;;   (find-here-links-intro "3. `find-here-links'" "several kinds")
;;
;; The "here" buffer is sometimes called the "target" buffer. See:
;;
;;   (find-here-links-intro "4. `find-here-links-3'")
;;   (find-here-links-intro "4. `find-here-links-3'" "terminology")
;;
;; For each kind of "here" we have a "test function" that tests if the
;; current buffer is of that kind of here and a "links function" that
;; generates links for that kind of here. Here's the conventions on
;; their names. A sexp like
;;
;;   (find-man "1 date")
;;
;; opens a manpage buffer; the test function and the links function
;; for manpage buffers are:
;;
;;   (ee-man-bufferp)
;;   (ee-find-man-links)
;;
;; They all have the same stem - "man" - but different prefixes and
;; suffixes.

;; TODO: Some of the `ee-find-*-links' functions are defined in other
;; files. Which ones? Give examples!
;;
;;   (find-eapropos "ee-find-.*-links")

;; The main workhorse function in this file is `ee-find-here-links',
;; that _currently_ uses a big `cond' to run these test functions in a
;; certain order until one of them returns true, and then it returns
;; the result of the links function associated to that test (and to
;; that kind of "here"). But I am trying to rewrite it...



;; «.ee-types-of-here»		(to "ee-types-of-here")
;; «.tests-and-links»		(to "tests-and-links")
;; «.ee-which-here-tests»	(to "ee-which-here-tests")
;;
;; «.find-here-links»		(to "find-here-links")
;; «.find-here-links-beginner»	(to "find-here-links-beginner")
;; «.find-here-links-3»		(to "find-here-links-3")





;;;   __ _           _       _                         _ _       _        
;;;  / _(_)_ __   __| |     | |__   ___ _ __ ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _ \ '__/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____| | | |  __/ | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|\___|_|  \___|     |_|_|_| |_|_|\_\___/
;;;                                                                       
;; «find-here-links» (to ".find-here-links")
;; See: (find-eev-quick-intro "`M-h M-h'")

;; (find-efunction 'find-grep-links)
;; (find-efunction 'find-einfo-links)
;; (find-efunction 'find-file-links)
;; (find-find-links-links "\\M-h" "here" "")
;; (find-efunction 'find-ecolors)

;; Moved the key binding to:
;;   (find-eevfile "eev-mode.el" "\\M-h\\M-h")
;; (define-key eev-mode-map "\M-h\M-h" 'find-here-links)

;; TODO: support cases like these:
;;   (find-efunctiondescr 'condition-case)






;;;  _                                      __       _                   
;;; | |_ _   _ _ __   ___  ___        ___  / _|     | |__   ___ _ __ ___ 
;;; | __| | | | '_ \ / _ \/ __|_____ / _ \| |_ _____| '_ \ / _ \ '__/ _ \
;;; | |_| |_| | |_) |  __/\__ \_____| (_) |  _|_____| | | |  __/ | |  __/
;;;  \__|\__, | .__/ \___||___/      \___/|_|       |_| |_|\___|_|  \___|
;;;      |___/|_|                                                        
;;
;; «ee-types-of-here»  (to ".ee-types-of-here")
;; New code. Not working yet. Commented out.
;; Ugliness: `ee-add-type-of-here' runs `sort' too often.
;; See: (find-eev "eev-code.el" "alists")
;;      (find-elnode "Sequence Functions" "Function: sort ")

'
(progn

(setq ee-types-of-here nil)

(defun ee-sort-types-of-here ()
  (setq ee-types-of-here
	(sort ee-types-of-here
	      (lambda (a b) (string< (car a) (car b))))))

(defun ee-add-type-of-here (priority testcode linkscode)
  (let* ((label   (format "%s %s" priority (ee-S testcode))))
    (setq ee-types-of-here
	  (ee-aset ee-types-of-here label (list testcode linkscode)))))

(defun ee-add-toh-major (testcode linkscode)
  (ee-add-type-of-here "20 major" testcode linkscode))

(defun ee-add-toh-bname (testcode linkscode)
  (ee-add-type-of-here "40 bname" testcode linkscode))

(defun ee-add-toh-bhelp (testcode linkscode)
  (ee-add-type-of-here "60 help " testcode linkscode))

(defun ee-add-toh-other (testcode linkscode)
  (ee-add-type-of-here "80 other" testcode linkscode))

;; By major mode:
(ee-add-toh-major '(ee-info-bufferp)           '(ee-find-info-links))
(ee-add-toh-major '(ee-man-bufferp)            '(ee-find-man-links))
(ee-add-toh-major '(ee-grep-bufferp)           '(ee-find-grep-links))
(ee-add-toh-major '(ee-w3m-bufferp)            '(ee-find-w3m-links))
(ee-add-toh-major '(ee-dired-bufferp)          '(ee-find-file-links))
(ee-add-toh-major '(ee-wdired-bufferp)         '(ee-find-file-links))
(ee-add-toh-major '(ee-custom-bufferp)         '(ee-find-custom-links))
(ee-add-toh-major '(ee-epackages-bufferp)      '(ee-find-epackages-links))

;; By buffer name:
(ee-add-toh-bname '(ee-intro-bufferp)          '(ee-find-intro-links))
(ee-add-toh-bname '(ee-ecolors-bufferp)        '(ee-find-ecolors-links))
(ee-add-toh-bname '(ee-efaces-bufferp)         '(ee-find-efaces-links))
;; (ee-add-toh-bname '(ee-freenode-bufferp)    '(ee-find-freenode-links))
;; (ee-add-toh-bname '(ee-pdftext-bufferp)     '(ee-find-pdftext-links))

;; When the buffer name is "*Help*" we parse the first line: 
(ee-add-toh-bhelp '(ee-efunctiondescr-bufferp) '(ee-find-efunctiondescr-links))
(ee-add-toh-bhelp '(ee-efacedescr-bufferp)     '(ee-find-efacedescr-links))
(ee-add-toh-bhelp '(ee-evardescr-bufferp)      '(ee-find-evardescr-links))
(ee-add-toh-bhelp '(ee-epackage-bufferp)       '(ee-find-epackage-links))

;; Other cases:
(ee-add-toh-other '(ee-file-bufferp)           '(ee-find-file-links))

(ee-sort-types-of-here)

;; Inspect the data structures:
;; (find-epp ee-types-of-here)
;; (find-estring (mapconcat 'car ee-types-of-here "\n"))


)






;;;  _            _                         _   _ _       _        
;;; | |_ ___  ___| |_ ___    __ _ _ __   __| | | (_)_ __ | | _____ 
;;; | __/ _ \/ __| __/ __|  / _` | '_ \ / _` | | | | '_ \| |/ / __|
;;; | ||  __/\__ \ |_\__ \ | (_| | | | | (_| | | | | | | |   <\__ \
;;;  \__\___||___/\__|___/  \__,_|_| |_|\__,_| |_|_|_| |_|_|\_\___/
;;;                                                                
;; «tests-and-links»  (to ".tests-and-links")
;; For each kind of here we have:
;;   a) a test function that tests if the current buffer is of that kind,
;;   b) a function that return hyperlinks for that kind of here.
;;
;; Idea: rename them, and use names like:
;;   ee-here-info-p
;;   ee-here-info-links



;; Some tools for detecting which kind of buffer we're in.
(defun ee-buffer-re (re)
  (if (string-match re (buffer-name))
      (match-string 1 (buffer-name))))
(defun ee-buffer-eq (str) (string= str (buffer-name)))

(defun ee-buffer-help0    () (ee-buffer-eq "*Help*"))
(defun ee-buffer-help-re0 (re n)
  (if (ee-buffer-help0)
      (save-excursion
	(goto-char (point-min))
	(if (looking-at re) (match-string n)))))

(defun ee-buffer-help (re n) (intern (or (ee-buffer-help-re0 re n) "nil")))



;; By major mode
(defun ee-grep-bufferp      () (eq major-mode 'grep-mode))
(defun ee-man-bufferp       () (eq major-mode 'Man-mode))
(defun ee-rcirc-bufferp     () (eq major-mode 'rcirc-mode))
(defun ee-info-bufferp      () (eq major-mode 'Info-mode))
(defun ee-dired-bufferp     () (eq major-mode 'dired-mode))
(defun ee-wdired-bufferp    () (eq major-mode 'wdired-mode))
(defun ee-w3m-bufferp       () (eq major-mode 'w3m-mode))
(defun ee-custom-bufferp    () (eq major-mode 'Custom-mode))
(defun ee-epackages-bufferp () (eq major-mode 'package-menu-mode))

;; By buffer name
(defun ee-intro-bufferp    () (ee-buffer-re "^\\*(find-\\(.*\\)-intro)\\*$"))
(defun ee-freenode-bufferp () (ee-buffer-re "^\\(.*\\).freenode\\.net"))
(defun ee-ecolors-bufferp  () (ee-buffer-eq "*Colors*"))
(defun ee-efaces-bufferp   () (ee-buffer-eq "*Faces*"))
(defun ee-pdftext-bufferp  () (ee-buffer-re "^pdftotext"))

;; By buffer name (when it is "*Help*")
(defvar ee-efunctiondescr-re
  "^\\([^ \t\n]+\\) is a[^\t\n]*\\(function\\|Lisp macro\\|special form\\)")
(defun  ee-efunctiondescr-bufferp () (ee-buffer-help ee-efunctiondescr-re 1))
(defun  ee-find-efunctiondescr-links ()
  (let ((f (ee-efunctiondescr-bufferp)))
    `((find-efunction-links ',f)
      (find-efunctiondescr ',f))))

(defvar ee-evardescr-re "^\\([^ \t\n]+\\) is a variable")
(defun  ee-evardescr-bufferp () (ee-buffer-help ee-evardescr-re 1))
(defun  ee-find-evardescr-links ()
  (let ((v (ee-evardescr-bufferp)))
    `((find-evariable-links ',v)
      (find-evardescr ',v))))

(defvar ee-efacedescr-re "^Face: \\([^ \t\n]+\\)")
(defun  ee-efacedescr-bufferp () (ee-buffer-help ee-efacedescr-re 1))
(defun  ee-find-efacedescr-links ()
  (let ((f (ee-efacedescr-bufferp)))
    `((find-eface-links ',f)
      (find-efacedescr ',f))))

(defvar ee-epackage-re "^\\([^ \t\n]+\\) is a[ -~]+ package")
(defvar ee-epackage-re-27 "^Package \\([^ \t\n]+\\) is") ; for Emacs 27
(defun  ee-epackage-bufferp ()
  (or (ee-buffer-help ee-epackage-re 1)
      (ee-buffer-help ee-epackage-re-27 1)))
(defun  ee-find-epackage-links ()
  (let ((p (ee-epackage-bufferp)))
    (list (ee-find-epackage-links0 p "{c}" "{d}"))))

;; By buffer name (when the mode is man)
(defvar ee-man-re "^\\*Man \\(.*\\)\\*$")
(defun  ee-find-man-links () 
  (let ((mp (ee-buffer-re ee-man-re)))
    `((find-man ,mp))))

(defvar ee-custom-re "^\\*Customize Group: \\(.*\\)\\*$")
(defun  ee-find-custom-links () 
  (let* ((name   (ee-buffer-re ee-custom-re))
	 (symbol (intern (downcase (replace-regexp-in-string " " "-" name)))))
    `((find-customizegroup ',symbol))))

;; Other cases
(defun ee-file-bufferp     () buffer-file-name)

(defun ee-find-dired-links ()
  `((find-extra-file-links ,(ee-dired-to-fname 'no-error))
    ""
    ,@(ee-find-file-links)))

(defun ee-find-efaces-links    () `((find-efaces)))
(defun ee-find-ecolors-links   () `((find-ecolors)))
(defun ee-find-epackages-links () `((find-epackages)))
(defun ee-find-pdftext-links   () (ee-pdflike-page-links))



;; to to:
;; ee-find-w3m-links
;; ee-find-ecolor-links
;; 

(defun ee-find-here-links ()
  (cond ;; by major mode
	((ee-info-bufferp)      (cons "" (ee-find-info-links)))      ; M-h M-i
	((ee-man-bufferp)       (cons "" (ee-find-man-links)))       ; ?
	((ee-grep-bufferp)      (cons "" (ee-find-grep-links)))	     ; M-h M-g
	((ee-w3m-bufferp)       (cons "" (ee-find-w3m-links)))	     ; M-h M-w
	;; ((ee-dired-bufferp)  (cons "" (ee-find-dired-links)))     ; ?
	((ee-dired-bufferp)     (cons "" (ee-find-file-links)))	     ; M-h f
	((ee-wdired-bufferp)    (cons "" (ee-find-file-links)))	     ; M-h f
	((ee-custom-bufferp)    (cons "" (ee-find-custom-links)))    ; ?
	((ee-epackages-bufferp) (cons "" (ee-find-epackages-links))) ; ?
	;; by buffer name
	((ee-intro-bufferp)     (cons "" (ee-find-intro-links)))     ; M-h M-i
	((ee-freenode-bufferp)  (cons "" (ee-find-freenode-links)))  ; ?
	((ee-ecolors-bufferp)   (cons "" (ee-find-ecolors-links)))   ; ?
	((ee-efaces-bufferp)    (cons "" (ee-find-efaces-links)))    ; ?
	((ee-pdftext-bufferp)   (cons "" (ee-find-pdftext-links)))   ; ?
	;; by buffer name, when it is "*Help*"
	((ee-efunctiondescr-bufferp) (cons "" (ee-find-efunctiondescr-links)))
	((ee-efacedescr-bufferp)     (cons "" (ee-find-efacedescr-links)))
	((ee-evardescr-bufferp)      (cons "" (ee-find-evardescr-links)))
	((ee-epackage-bufferp)       (cons "" (ee-find-epackage-links)))
	;; other cases
	((ee-file-bufferp)      (cons "" (ee-find-file-links)))	   ; M-h f
	(t (list "" "Not implemented!" "See:"
		 '(find-efunction 'ee-find-here-links)))
	))

(defun find-here-links-test (sexp)
"See: (find-links-intro \"`find-here-links'\")"
  (find-wset "13o_2o_o" sexp '(find-here-links)))

;; (find-man "1 cat")
;; (progn (find-man "1 cat") (buffer-name))
;; (find-eevfile "eev-rcirc.el")

(defun ee-find-here-links0 ()
  `(,(ee-H "See: ")
    (find-eev-quick-intro "4.1. `find-here-links'")
    (find-emacs-keys-intro "1. Basic keys (eev)" "M-h M-h")
    (find-here-links-intro "4. `find-here-links-3'")
    ))



;;;   __ _           _       _                         _ _       _        
;;;  / _(_)_ __   __| |     | |__   ___ _ __ ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _ \ '__/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____| | | |  __/ | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|\___|_|  \___|     |_|_|_| |_|_|\_\___/
;;;                                                                       

;; (find-find-links-links "\\M-h" "here" "")
;;
(defun find-here-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks pointing to here."
  (interactive)
  (apply 'find-elinks
   `(;; The first line of a find-here-links buffer DOES NOT
     ;; regenerates the buffer - instead the first lines point to
     ;; help pages.
     ,@(ee-find-here-links0)
     ,@(ee-find-here-links)
     )
   pos-spec-list))

;; Test: (find-here-links)
;; (progn (find-enode "Screen") (find-here-links))



;;;  _                _                       
;;; | |__   ___  __ _(_)_ __  _ __   ___ _ __ 
;;; | '_ \ / _ \/ _` | | '_ \| '_ \ / _ \ '__|
;;; | |_) |  __/ (_| | | | | | | | |  __/ |   
;;; |_.__/ \___|\__, |_|_| |_|_| |_|\___|_|   
;;;             |___/                         
;;
;; «find-here-links-beginner»  (to ".find-here-links-beginner")
;; This is a hack for beginners that is explained in a tutorial. See:
;; (find-refining-intro "4. A tip for beginners")
;; (find-refining-intro "4. A tip for beginners" "find-here-links-beginner")
;;
(defun find-here-links-beginner (&optional arg)
  "A variant of `find-here-links' that may create a three-window setting."
  (interactive "P")
  (if arg (find-here-links-3) (find-here-links)))

;; «find-here-links-3»  (to ".find-here-links-3")
;; See: (find-here-links-intro "4. `find-here-links-3'")
;;      (find-here-links-intro "5. `find-here-links-1'")
;;
(defvar ee-window-configuration-before-M-h-M-3 nil)

(defun find-here-links-3 ()
  "A variant of `find-here-links' that creates a three-window setting.
Before creating the three windows this function tries to save the
current window configuration to the variable
`ee-window-configuration-before-M-h-M-3', but if that variable is
not nil we abort instead of overwriting it.
See: (find-here-links-intro \"4. `find-here-links-3'\")"
  (interactive)
  (if ee-window-configuration-before-M-h-M-3
      (let ((overwrite
	     (yes-or-no-p "Overwrite `ee-window-configuration-before-M-h-M-3'? ")))
	(if (not overwrite)
	    (error))))
  (setq ee-window-configuration-before-M-h-M-3
	(current-window-configuration))
  (find-wset "13_o2_o_coo" nil '(find-here-links) '(eejump-1)))

(defun find-here-links-1 ()
  "Restore the window configuration before `find-here-links-3'.
See: (find-here-links-intro \"5. `find-here-links-1'\")"
  (interactive)
  (set-window-configuration ee-window-configuration-before-M-h-M-3)
  (setq ee-window-configuration-before-M-h-M-3 nil))








(provide 'eev-hlinks)



;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
