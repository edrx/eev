;;; eev-hlinks.el --- `find-here-links' and variants.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.
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
;; Version:    20211219
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
;; Their names are of the form `ee-*-bufferp' and `ee-find-*-links'.
;;
;; TODO: Some of the `ee-find-*-links' functions are defined in other
;; files. Which ones? Give examples!
;;
;;   (find-eapropos "ee-find-.*-links")
;;
;; The main workhorse function in this file is `ee-find-here-links',
;; Its current version runs the program in `ee-fhl-main-program' using
;; the function `ee-fhl-run' to interpret it.



;; «.ee-fhl-main-program»	(to "ee-fhl-main-program")
;; «.ee-fhl-run»		(to "ee-fhl-run")
;; «.ee-find-here-links»	(to "ee-find-here-links")
;; «.ee-find-here-links-old»	(to "ee-find-here-links-old")
;; «.find-here-links»		(to "find-here-links")
;;
;; «.low-level-functions»	(to "low-level-functions")
;; «.tests-and-links»		(to "tests-and-links")
;;
;; «.find-here-links-beginner»	(to "find-here-links-beginner")
;; «.find-here-links-3»		(to "find-here-links-3")





;;;                  _                                                   
;;;  _ __ ___   __ _(_)_ __    _ __  _ __ ___   __ _ _ __ __ _ _ __ ___  
;;; | '_ ` _ \ / _` | | '_ \  | '_ \| '__/ _ \ / _` | '__/ _` | '_ ` _ \ 
;;; | | | | | | (_| | | | | | | |_) | | | (_) | (_| | | | (_| | | | | | |
;;; |_| |_| |_|\__,_|_|_| |_| | .__/|_|  \___/ \__, |_|  \__,_|_| |_| |_|
;;;                           |_|              |___/                     
;;
;; «ee-fhl-main-program»  (to ".ee-fhl-main-program")
;; In the past this "main program" was a big cond, that was easy to
;; understand but hard to debug... in oct/2021 I replaced the big cond
;; by a program in a domain-specific language that should be easy to
;; understand, easy to extend, and easy to debug - the one below.
;;
;; `find-here-links' runs this "program" with `ee-fhl-run'.
;;
;; Try: (find-eppp ee-fhl-main-program)
;;     (ee-fhl-run ee-fhl-main-program)

(defvar ee-fhl-main-program
 '(:or
   ;; By major mode:
   (:if (ee-info-bufferp)      (ee-find-info-links))
   (:if (ee-man-bufferp)       (ee-find-man-links))
   (:if (ee-grep-bufferp)      (ee-find-grep-links))
   (:if (ee-eww-bufferp)       (ee-find-eww-links))
   (:if (ee-w3m-bufferp)       (ee-find-w3m-links))
   (:if (ee-dired-bufferp)     (ee-find-file-links))
   (:if (ee-wdired-bufferp)    (ee-find-file-links))
   (:if (ee-custom-bufferp)    (ee-find-custom-links))
   (:if (ee-epackages-bufferp) (ee-find-epackages-links))
   ;;
   ;; By buffer name:
   (:if (ee-intro-bufferp)     (ee-find-intro-links))
   (:if (ee-freenode-bufferp)  (ee-find-freenode-links))
   (:if (ee-ecolors-bufferp)   (ee-find-ecolors-links))
   (:if (ee-efaces-bufferp)    (ee-find-efaces-links))
   (:if (ee-pdftext-bufferp)   (ee-find-pdftext-links))
   ;;
   ;; By buffer name, when it is "*Help*":
   (:if (ee-efunctiondescr-bufferp) (ee-find-efunctiondescr-links))
   (:if (ee-efacedescr-bufferp)     (ee-find-efacedescr-links))
   (:if (ee-evardescr-bufferp)      (ee-find-evardescr-links))
   (:if (ee-epackage-bufferp)       (ee-find-epackage-links))
   ;;
   ;; Other cases:
   (:if (ee-file-bufferp)        (ee-find-file-links))
   (:if t                        '("" "Not implemented!" "See:"
				   (find-efunction 'ee-find-here-links)))
   )
 "See `ee-find-here-links'.")


;;;  ____  ____  _     
;;; |  _ \/ ___|| |    
;;; | | | \___ \| |    
;;; | |_| |___) | |___ 
;;; |____/|____/|_____|
;;;                    
;; «ee-fhl-run»  (to ".ee-fhl-run")
;; Here we define the interpreter for the DSL that is used in
;; `ee-fhl-main-program'.
;; TODO: implement (:subprogram-in VARNAME).

(defvar ee-fhl-sexp1 nil
  "When `ee-fhl-run' finds an (ee-fhl-:if SEXP1 SEXP2)
   that succeeds it stores the SEXP1 in this variable.")

(defvar ee-fhl-sexp2 nil
  "When `ee-fhl-run' finds an (ee-fhl-:if SEXP1 SEXP2)
   that succeeds it stores the SEXP2 in this variable.")

(defun ee-fhl-run (fhl-program)
  "See `ee-fhl-main-program'."
  (setq ee-fhl-sexp1 nil)
  (setq ee-fhl-sexp2 nil)
  (ee-fhl-eval fhl-program)
  (list ee-fhl-sexp1 ee-fhl-sexp2))

;; Tests:
;; (ee-fhl-eval  '(:eval (+ 20 3)))
;; (ee-fhl-eval  '(:if (< 1 2) (list 3 4)))
;; (ee-fhl-:eval '(+ 1 2) '(+ 3 4))
;; (ee-fhl-:if   '(< 1 2) '(list 3 4))
;; (ee-fhl-:if   '(> 1 2) '(list 3 4))
;; (ee-fhl-:or   '(:eval nil) '(:eval nil) '(:eval 42) '(:eval 99))
;;
(defun ee-fhl-eval (fhl-sexp)
  (let* ((f (ee-intern "ee-fhl-%s" (car fhl-sexp)))
	 (rest (cdr fhl-sexp)))
    (apply f rest)))

(defun ee-fhl-:eval (&rest sexps)
  (eval (cons 'progn sexps)))

(defun ee-fhl-:if (sexp1 sexp2)
  (when (eval sexp1)
    (setq ee-fhl-sexp1 sexp1)
    (setq ee-fhl-sexp2 sexp2)
    sexp2))

(defun ee-fhl-:or (&rest fhl-sexps)
  (cl-loop for fhl-sexp in fhl-sexps
	   for fhl-result = (ee-fhl-eval fhl-sexp)
	   until fhl-result
           finally return fhl-result))

;; «ee-find-here-links»  (to ".ee-find-here-links")
;; Try:           (ee-find-here-links)
;;   (find-elinks (ee-find-here-links))
;;   ee-fhl-sexp1
;;   ee-fhl-sexp2

(defun ee-find-here-links ()
  "Run all the `(:if SEXP1 SEXP2)'s in `ee-fhl-main-program' until
one succeeds. When the successful `(:if ___ ___)' is found store
its SEXP1 in `ee-fhl-sexp1', its SEXP2 in `ee-fhl-sexp2', and
return the result of (eval ee-fhl-sexp2), slightly preprocessed."
  (ee-fhl-run ee-fhl-main-program)
  (cons "" (eval ee-fhl-sexp2)))


;; «ee-find-here-links-old»  (to ".ee-find-here-links-old")
;; Old version:
;; (defun ee-find-here-links ()
;;   (cond ;; by major mode
;;         ((ee-info-bufferp)      (cons "" (ee-find-info-links)))      ; M-h M-i
;;         ((ee-man-bufferp)       (cons "" (ee-find-man-links)))       ; ?
;;         ((ee-grep-bufferp)      (cons "" (ee-find-grep-links)))      ; M-h M-g
;;         ((ee-w3m-bufferp)       (cons "" (ee-find-w3m-links)))       ; M-h M-w
;;         ;; ((ee-dired-bufferp)  (cons "" (ee-find-dired-links)))     ; ?
;;         ((ee-dired-bufferp)     (cons "" (ee-find-file-links)))      ; M-h f
;;         ((ee-wdired-bufferp)    (cons "" (ee-find-file-links)))      ; M-h f
;;         ((ee-custom-bufferp)    (cons "" (ee-find-custom-links)))    ; ?
;;         ((ee-epackages-bufferp) (cons "" (ee-find-epackages-links))) ; ?
;;         ;; by buffer name
;;         ((ee-intro-bufferp)     (cons "" (ee-find-intro-links)))     ; M-h M-i
;;         ((ee-freenode-bufferp)  (cons "" (ee-find-freenode-links)))  ; ?
;;         ((ee-ecolors-bufferp)   (cons "" (ee-find-ecolors-links)))   ; ?
;;         ((ee-efaces-bufferp)    (cons "" (ee-find-efaces-links)))    ; ?
;;         ((ee-pdftext-bufferp)   (cons "" (ee-find-pdftext-links)))   ; ?
;;         ;; by buffer name, when it is "*Help*"
;;         ((ee-efunctiondescr-bufferp) (cons "" (ee-find-efunctiondescr-links)))
;;         ((ee-efacedescr-bufferp)     (cons "" (ee-find-efacedescr-links)))
;;         ((ee-evardescr-bufferp)      (cons "" (ee-find-evardescr-links)))
;;         ((ee-epackage-bufferp)       (cons "" (ee-find-epackage-links)))
;;         ;; other cases
;;         ((ee-file-bufferp)      (cons "" (ee-find-file-links)))    ; M-h f
;;         (t (list "" "Not implemented!" "See:"
;;                  '(find-efunction 'ee-find-here-links)))
;;         ))




;;;   __ _           _       _                         _ _       _        
;;;  / _(_)_ __   __| |     | |__   ___ _ __ ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _ \ '__/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____| | | |  __/ | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|\___|_|  \___|     |_|_|_| |_|_|\_\___/
;;;                                                                       
;; «find-here-links»  (to ".find-here-links")
;; Skel: (find-find-links-links-new "here" "" "")
;; Key binding:
;;     (find-eev "eev-mode.el" "eev-mode-map-set" "M-h" "M-h" "find-here-links")
;; See: (find-eev-quick-intro "`M-h M-h'")
;;      (find-here-links-intro)
;;      (find-links-conv-intro "3. Classification")
;;      (find-links-conv-intro "3. Classification" "that violate")
;; Tests: (find-here-links)
;;        (progn (find-enode "Screen") (find-here-links))
;;
(defun find-here-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks pointing to \"here\".
See: (find-here-links-intro)"
  (interactive)
  (let ((ee-buffer-name "*(find-here-links)*"))
    (apply
     'find-elinks
     `(;; The first line of a find-here-links buffer DOES NOT
       ;; regenerates the buffer - instead the first lines point to
       ;; help pages.
       ,@(ee-find-here-links0)
       ,@(ee-find-here-links)
       )
     pos-spec-list)))

(defun ee-find-here-links0 ()
  "The header used by `find-here-links'."
  `(,(ee-H "See: ")
    (find-eev-quick-intro "4.1. `find-here-links'")
    (find-emacs-keys-intro "1. Basic keys (eev)" "M-h M-h")
    (find-here-links-intro "4. `find-here-links-3'")
    ))







;;;  _                       _                _ 
;;; | |    _____      __    | | _____   _____| |
;;; | |   / _ \ \ /\ / /____| |/ _ \ \ / / _ \ |
;;; | |__| (_) \ V  V /_____| |  __/\ V /  __/ |
;;; |_____\___/ \_/\_/      |_|\___| \_/ \___|_|
;;;                                             
;; «low-level-functions»  (to ".low-level-functions")
;; Low lever functions used by the functions of the form
;; `ee-*-bufferp' and `ee-find-*-links' defined in the next section.

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


;;;  _____         _                         _   _ _       _        
;;; |_   _|__  ___| |_ ___    __ _ _ __   __| | | (_)_ __ | | _____ 
;;;   | |/ _ \/ __| __/ __|  / _` | '_ \ / _` | | | | '_ \| |/ / __|
;;;   | |  __/\__ \ |_\__ \ | (_| | | | | (_| | | | | | | |   <\__ \
;;;   |_|\___||___/\__|___/  \__,_|_| |_|\__,_| |_|_|_| |_|_|\_\___/
;;;                                                                 
;; «tests-and-links»  (to ".tests-and-links")
;; For each kind of "here" we have:
;;   a) a test function that tests if the current buffer is of that kind,
;;   b) a function that return hyperlinks for that kind of here.
;;
;; These functions have names like `ee-*-bufferp' and
;; `ee-find-*-links', and they are defined below. Note that they are
;; used in the `ee-fhl-main-program', defined at the top of this file.

;; By major mode
(defun ee-grep-bufferp      () (eq major-mode 'grep-mode))
(defun ee-man-bufferp       () (eq major-mode 'Man-mode))
(defun ee-rcirc-bufferp     () (eq major-mode 'rcirc-mode))
(defun ee-info-bufferp      () (eq major-mode 'Info-mode))
(defun ee-dired-bufferp     () (eq major-mode 'dired-mode))
(defun ee-wdired-bufferp    () (eq major-mode 'wdired-mode))
(defun ee-eww-bufferp       () (eq major-mode 'eww-mode))
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
      (find-efunctiondescr ',f)
      (find-efunction ',f))))

(defvar ee-evardescr-re "^\\([^ \t\n]+\\) is a variable")
(defun  ee-evardescr-bufferp () (ee-buffer-help ee-evardescr-re 1))
(defun  ee-find-evardescr-links ()
  (let ((v (ee-evardescr-bufferp)))
    `((find-evariable-links ',v)
      (find-evardescr ',v)
      (find-evariable ',v))))

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
    (list (ee-find-epackage-links0 p nil "{d}"))))

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
(defun ee-find-eww-links       () `((find-eww ,(plist-get eww-data :url))))
(defun ee-find-w3m-links       () `((find-w3m ,w3m-current-url)))


;; For debugging.
;; TODO: remember how to use this.
;;
;; (defun find-here-links-test (sexp)
;; "See: (find-links-intro \"`find-here-links'\")"
;;   (find-wset "13o_2o_o" sexp '(find-here-links)))





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
