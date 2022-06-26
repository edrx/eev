;;; eev-hlinks.el --- `find-here-links' and variants.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.
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
;; Version:    20220625
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-hlinks.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-hlinks.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
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
;;
;; Debug mode
;; ==========
;; The best way to understand how this works is to run
;; `find-here-links' with a prefix argument. When we run
;; `find-here-links' _without_ a prefix argument it displays a header
;; and then the links to "here"; _with_ a prefix argument it displays
;; the header and then a lot of internal information about "here". For
;; example, if you run
;;
;;   (eek "M-h M-h  ;; find-here-links")
;;
;; then "here" is a file, and `find-here-links' displays a header and
;; then the result of:
;;
;;   (ee-find-file-links)
;;
;; If you run, say,
;;
;;   (eek "M-0 M-h M-h  ;; M-0 find-here-links")
;;
;; then `find-here-links' will detect which kind of "here" is "here",
;; and will display the header and then the result of
;; `(ee-find-here-debug-links)'. Most of that will be static text, but
;; that output will also contain lines like these ones,
;;
;;   (find-efunction 'ee-file-bufferp)
;;   (find-efunction 'ee-find-file-links)
;;
;; that display information about the results that were generated and
;; saved in the last time that the function `ee-detect-here' was
;; called.


;; TODO
;; ====
;; 1. Some of the `ee-find-*-links' functions are defined in other
;;    files. Which ones? Give examples! This may help:
;;
;;      (find-eapropos "ee-find-.*-links")
;;
;; The main workhorse function in this file is `ee-find-here-links',
;; Its current version runs the program in `ee-fhl-main-program' using
;; the function `ee-fhl-run' to interpret it.



;; «.find-here-links»		(to "find-here-links")
;; «.ee-find-here-links»	(to "ee-find-here-links")
;; «.ee-find-here-debug-links»	(to "ee-find-here-debug-links")
;; «.ee-find-here-links-tests»	(to "ee-find-here-links-tests")
;; «.ee-fhl-main-program»	(to "ee-fhl-main-program")
;; «.ee-fhl-run»		(to "ee-fhl-run")
;;
;; «.ee-find-here-links-old»	(to "ee-find-here-links-old")
;;
;; «.low-level-functions»	(to "low-level-functions")
;; «.tests-and-links»		(to "tests-and-links")
;;
;; «.find-here-links-beginner»	(to "find-here-links-beginner")
;; «.find-here-links-3»		(to "find-here-links-3")






;;;   __ _           _       _                         _ _       _        
;;;  / _(_)_ __   __| |     | |__   ___ _ __ ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _ \ '__/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____| | | |  __/ | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|\___|_|  \___|     |_|_|_| |_|_|\_\___/
;;;                                                                       
;; «find-here-links»  (to ".find-here-links")
;; Skel: (find-find-links-links-new "here" "" "")
;; See: (find-eev "eev-mode.el" "eev-mode-map-set" "find-here-links")
;;      (find-eev-quick-intro "`M-h M-h'")
;;      (find-here-links-intro)
;;      (find-links-conv-intro "3. Classification")
;;      (find-links-conv-intro "3. Classification" "that violate")
;; Tests: (find-here-links)
;;        (progn (find-enode "Screen") (find-here-links))
;;
;; TODO: write one test for each kind of "here" that we support.
;;
(defun find-here-links (&optional arg &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks pointing to \"here\".
If ARG is not nil, show some info about how `find-here-links'
detected which kind of \"here\" the current buffer is.
See: (find-here-links-intro)"
  (interactive "P")
  (let ((ee-buffer-name "*(find-here-links)*"))
    (apply
     'find-elinks
     `(;; The first line of a find-here-links buffer DOES NOT
       ;; regenerates the buffer - instead the first lines point to
       ;; help pages.
       ,@(ee-find-here-links0)
       ,@(ee-find-here-links arg)
       )
     pos-spec-list)))

(defun ee-find-here-links0 ()
  "The header used by `find-here-links'."
  `(,(ee-H "See: ")
    (find-eev-quick-intro "4.1. `find-here-links'")
    (find-emacs-keys-intro "1. Basic keys (eev)" "M-h M-h")
    (find-here-links-intro "4. `find-here-links-3'")
    ))

;; «ee-find-here-links»  (to ".ee-find-here-links")
;;
(defun ee-find-here-links (&optional arg)
  "Generate the non-header part of the \"*(find-here-links)*\" buffer.
This function runs `(ee-fhl-run ee-fhl-main-program)', that runs
the program in `ee-fhl-main-program' with `ee-fhl-run' until an
expression like this in it

  (:if SEXP1 SEXP2)

succeeds. When the successful `(:if ___ ___)' is found its SEXP1
is stored in the global variable `ee-fhl-sexp1' and its SEXP2 is
stored in the global variable `ee-fhl-sexp2'. Their values will
be things like:

  SEXP1  =>  (ee-<kind>-bufferp)
  SEXP2  =>  (ee-find-<kind>-links)

For example, if we run `find-here-links' in an info buffer we
will have this,

  SEXP1  =>  (ee-info-bufferp)
  SEXP2  =>  (ee-find-info-links)

and `(eval (ee-find-info-links))' produces the non-header part of
the \"*(find-here-links)*\" buffer.

If ARG is non-nil, show some info about how `ee-fhl-run' decided
which kind \"here\" the current buffer is."
  (ee-detect-here)
  (if arg
      (cons "" (ee-find-here-debug-links))
    (cons "" (eval ee-fhl-sexp2))))

;; «ee-find-here-debug-links»  (to ".ee-find-here-debug-links")
;; See: (find-eevfile "eev-hlinks.el" "Debug mode")
;;      (find-eev     "eev-hlinks.el" "find-here-links" "If ARG")
;;
(defun ee-find-here-debug-links ()
  `("# The last call to"
    "#     '(find-here-links ARG)"
    "#  -> '(ee-detect-here)"
    "#  -> '(ee-fhl-run ee-fhl-main-program)"
    "# produced this:"
    ,(format "#   ee-fhl-sexp1  =>  %s" (ee-S ee-fhl-sexp1))
    ,(format "#   ee-fhl-sexp2  =>  %s" (ee-S ee-fhl-sexp2))
    "# See:"
    "#   ee-fhl-sexp1"
    "#   ee-fhl-sexp2"
    ,(format "#   (find-efunction '%s)" (car ee-fhl-sexp1))
    ,(format "#   (find-efunction '%s)" (car ee-fhl-sexp2))
    "#   (find-eev \"eev-hlinks.el\" \"find-here-links\")"
    "#   (find-eev \"eev-hlinks.el\" \"find-here-links\" \"If ARG\")"
    "#   (find-eev \"eev-hlinks.el\" \"ee-find-here-links\")"
    "#   (find-eev \"eev-hlinks.el\" \"ee-find-here-debug-links\")"
    "#   (find-eev \"eev-hlinks.el\" \"ee-fhl-run\")"
    "#   (find-eev \"eev-hlinks.el\" \"ee-fhl-run\" \"ee-detect-here\")"
    "#   (find-eev \"eev-hlinks.el\" \"ee-fhl-main-program\")"
    ))



;; «ee-find-here-links-tests»  (to ".ee-find-here-links-tests")
;; Low-level tests (old, written before `ee-find-here-debug-links'):
;;
;;   (find-2a nil                 '(find-here-links))
;;   (find-2a nil '(find-elinks (ee-find-here-links)))
;;           (ee-fhl-run ee-fhl-main-program)
;;                 (list ee-fhl-sexp1 ee-fhl-sexp2)
;;                                    ee-fhl-sexp2
;;   (find-2a nil '(find-elinks (eval ee-fhl-sexp2)))





;;;                  _                                                   
;;;  _ __ ___   __ _(_)_ __    _ __  _ __ ___   __ _ _ __ __ _ _ __ ___  
;;; | '_ ` _ \ / _` | | '_ \  | '_ \| '__/ _ \ / _` | '__/ _` | '_ ` _ \ 
;;; | | | | | | (_| | | | | | | |_) | | | (_) | (_| | | | (_| | | | | | |
;;; |_| |_| |_|\__,_|_|_| |_| | .__/|_|  \___/ \__, |_|  \__,_|_| |_| |_|
;;;                           |_|              |___/                     
;;
;; «ee-fhl-main-program»  (to ".ee-fhl-main-program")
;; `ee-find-here-links' runs this "program" with `ee-fhl-run'.
;;
;; Try: (find-eppp ee-fhl-main-program)
;;     (ee-fhl-run ee-fhl-main-program)
;;
;; Note the _AT THIS MOMENT_ the easiest way to add support for a new
;; kind of "here" in `ee-fhl-main-program' is to override this
;; variable by setq-ing it in your init file... this is just because
;; I've been lazy and I haven't implemented YET a way to make
;; `ee-fhl-main-program' call "subprograms". If you need to extend
;; this please get in touch with me and I'll implement the missing
;; parts!!!

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
   (:if (ee-epackages-bufferp) (ee-find-epackages-links))
   (:if (ee-osm-bufferp)       (ee-find-osm-links))
   (:if (ee-helpful-bufferp)   (ee-find-helpful-links))
   (:if (ee-nov-bufferp)       (ee-find-nov-links))
   ;;
   ;; By buffer name:
   (:if (ee-intro-bufferp)     (ee-find-intro-links))
   (:if (ee-custom-bufferp)    (ee-find-custom-links))
   (:if (ee-custom-f-bufferp)  (ee-find-custom-f-links))
   (:if (ee-custom-v-bufferp)  (ee-find-custom-v-links))
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

(defun ee-detect-here ()
  "To understand this, run `find-here-links' with a prefix argument.
This is the standard high-level way to call `ee-fhl-run'."
  (ee-fhl-run ee-fhl-main-program))




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




;;;  _                       _                _ 
;;; | |    _____      __    | | _____   _____| |
;;; | |   / _ \ \ /\ / /____| |/ _ \ \ / / _ \ |
;;; | |__| (_) \ V  V /_____| |  __/\ V /  __/ |
;;; |_____\___/ \_/\_/      |_|\___| \_/ \___|_|
;;;                                             
;; «low-level-functions»  (to ".low-level-functions")
;; Low level functions used by the functions of the form
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

;; Tests: (custom-unlispify-tag-name  'foo-bar)
;;        (ee-custom-lispify-tag-name "Foo Bar")
(defun ee-custom-lispify-tag-name (str)
  "Do the inverse of `custom-unlispify-tag-name'."
  (intern (downcase (replace-regexp-in-string " " "-" str))))



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
;; (defun ee-custom-bufferp () (eq major-mode 'Custom-mode))
(defun ee-epackages-bufferp () (eq major-mode 'package-menu-mode))
(defun ee-osm-bufferp       () (eq major-mode 'osm-mode))
(defun ee-helpful-bufferp   () (eq major-mode 'helpful-mode))
(defun ee-nov-bufferp       () (eq major-mode 'nov-mode))

;; By buffer name
(defun ee-intro-bufferp    () (ee-buffer-re "^\\*(find-\\(.*\\)-intro)\\*$"))
(defun ee-freenode-bufferp () (ee-buffer-re "^\\(.*\\).freenode\\.net"))
(defun ee-ecolors-bufferp  () (ee-buffer-eq "*Colors*"))
(defun ee-efaces-bufferp   () (ee-buffer-eq "*Faces*"))
(defun ee-pdftext-bufferp  () (ee-buffer-re "^pdftotext"))
(defun ee-custom-bufferp   () (ee-buffer-re ee-custom-re))
(defun ee-custom-f-bufferp () (ee-buffer-re ee-custom-f-re))
(defun ee-custom-v-bufferp () (ee-buffer-re ee-custom-v-re))

;; By buffer name (when it is "*Help*")
(defvar ee-efunctiondescr-re
  "^\\([^ \t\n]+\\) is a[^\t\n]*\\(function\\|Lisp macro\\|special form\\)")
(defun  ee-efunctiondescr-bufferp () (ee-buffer-help ee-efunctiondescr-re 1))
(defun  ee-find-efunctiondescr-links ()
  (let ((f (ee-efunctiondescr-bufferp)))
    `((find-efunction-links ',f)
      (find-efunctiondescr ',f)
      (find-efunction ',f)
      (describe-function ',f)
      )))

(defvar ee-evardescr-re "^\\([^ \t\n]+\\) is a variable")
(defun  ee-evardescr-bufferp () (ee-buffer-help ee-evardescr-re 1))
(defun  ee-find-evardescr-links ()
  (let ((v (ee-evardescr-bufferp)))
    `((find-evariable-links ',v)
      (find-evardescr ',v)
      (find-evariable ',v)
      (describe-variable ',v)
      )))

(defvar ee-efacedescr-re "^Face: \\([^ \t\n]+\\)")
(defun  ee-efacedescr-bufferp () (ee-buffer-help ee-efacedescr-re 1))
(defun  ee-find-efacedescr-links ()
  (let ((f (ee-efacedescr-bufferp)))
    `((find-eface-links ',f)
      (find-efacedescr ',f)
      (describe-face ',f)
      )))

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
    `((find-man ,mp)
      (man ,mp)
      )))

(defvar ee-custom-re "^\\*Customize Group: \\(.*\\)\\*$")
(defun  ee-find-custom-links () 
  (let* ((name   (ee-buffer-re ee-custom-re))
	 (symbol (ee-custom-lispify-tag-name name)))
    `((find-customizegroup ',symbol)
      (customize-group ',symbol)
      )))

(defvar ee-custom-f-re "^\\*Customize Face: \\(.*\\)\\*$")
(defun  ee-find-custom-f-links () 
  (let* ((name   (ee-buffer-re ee-custom-f-re))
	 (symbol (ee-custom-lispify-tag-name name)))
    `((find-eface-links ',symbol)
      (find-customizeface ',symbol)
      (customize-face ',symbol)
      )))

(defvar ee-custom-v-re "^\\*Customize Option: \\(.*\\)\\*$")
(defun  ee-find-custom-v-links () 
  (let* ((name   (ee-buffer-re ee-custom-v-re))
	 (symbol (ee-custom-lispify-tag-name name)))
    `((find-evariable-links ',symbol)
      (find-customizeoption ',symbol)
      (find-customizevariable ',symbol)
      (customize-option ',symbol)
      (customize-variable ',symbol)
      )))

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
