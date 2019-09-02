;;; eev-explain.el -- explain an eev sexp.
;; Explain some difficult sexps, like eejump-nnn and code-xxx.
;; This is an experimental feature - I am using it in tutorials and
;; videos.

;; Copyright (C) 2019 Free Software Foundation, Inc.
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
;; Version:    2019aug06
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-explain.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-explain.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:

;; (load "eev-explain.el")

;; This is very new. Everything is going to change.
;;
;; Terminology: a "bet" is a triple of the form (begin end text); the
;; global variable `ee-bets' holds a list of bets.
;;
;; The tests below show some of the things we do with ee-bets:
;;
;;   (defun eejump-100 () (set-frame-font "nil2"))
;;   (progn (eek "  <up> C-e")      (ee-bets-set))
;;   (progn (eek "2*<up> C-e") (cdr (ee-bets-set)))
;;   ee-bets
;;   (ee-bets-text 0)
;;   (ee-bets-text 1)
;;   (ee-bets-text 2)
;;   (ee-bets-flash 0)
;;   (ee-bets-flash 2)
;;   (ee-bets-flash 2 7 0)
;;   (ee-bets-flash 2 7 0 nil 'eepitch-star-face)
;;   (ee-bets-flash 3)
;;   (ee-bets-flash 4)
;;
;; (ee-explain-eejump)
;; (eev-explain)



;;;  _          _       
;;; | |__   ___| |_ ___ 
;;; | '_ \ / _ \ __/ __|
;;; | |_) |  __/ |_\__ \
;;; |_.__/ \___|\__|___/
;;;                     

(defun ee-forward-sexp1 ()
  "Like `ee-forward-sexp' but returns point on success and nil on error."
  (interactive)
  (condition-case nil
      (progn (ee-forward-sexp) (point))
    (error nil)))

(defun ee-forward-sexp3 ()
  "Like `ee-forward-sexp1' but returns (pos1 pos2 text), or nil on error."
  (interactive)
  (looking-at "[ \t\n]*\\(;[^\n]*\n[ \t\n]*\\)*")
  (goto-char (match-end 0))		; skip whitespace and comments
  (let ((pos1 (point)))
    (if (ee-forward-sexp1)
	(let* ((pos2 (point))
	       (text (buffer-substring-no-properties pos1 pos2)))
	    (list pos1 pos2 text)))))

(defun ee-forward-sexp3s ()
  "Like `ee-forward-sexp3' but returns a list of triples like (begin end text)."
  (interactive)
  (let ((bets ()))
    (catch 'no-more-sexps
      (while t
	(let ((bet (ee-forward-sexp3)))
	  (if bet
	      (setq bets (cons bet bets))
	    (throw 'no-more-sexps nil)))))
    (reverse bets)))

(defun ee-subsexps-before-point ()
  (save-excursion
    (ee-backward-sexp)
    (let ((whole-sexp (save-excursion (ee-forward-sexp3))))
      (when (eq (following-char) (aref "(" 0))
	(forward-char 1)
	(cons whole-sexp (ee-forward-sexp3s))))))

(defvar ee-bets ()
  "A list of triples of the form (begin end text) corresponding
to the subsexps of the sexp before point.")

(defun ee-bets-set ()
  (interactive)
  (setq ee-bets (ee-subsexps-before-point)))

(defun ee-bets-begin (n) (nth 0 (nth n ee-bets)))
(defun ee-bets-end   (n) (nth 1 (nth n ee-bets)))
(defun ee-bets-text  (n) (nth 2 (nth n ee-bets)))


;;;   __ _           _     
;;;  / _| | __ _ ___| |__  
;;; | |_| |/ _` / __| '_ \ 
;;; |  _| | (_| \__ \ | | |
;;; |_| |_|\__,_|___/_| |_|
;;;                        
;; (find-es "emacs" "set-string-face")
;; (find-angg ".emacs" "find-epalette")
;; (find-efaces)
;; (find-ecolors)

(defun ee-set-string-property (str property-name value)
  (put-text-property 0 (length str) property-name value str)
  str)

(defun ee-set-string-face (str &optional face)
  (ee-set-string-property str 'face face))

(defun ee-set-string-fg (str &optional fg)
  (ee-set-string-face str (cons 'foreground-color fg)))

(defun ee-bets-flash (n &optional b-adj e-adj spec face)
  (let* ((b (+ (or b-adj 0) (nth 0 (nth n ee-bets))))
         (e (+ (or e-adj 0) (nth 1 (nth n ee-bets))))
	 (text (buffer-substring-no-properties b e))
	 )
    (eeflash+ b e (or spec ee-highlight-spec))
    (if face (ee-set-string-face text face))
    text))

;; The `ee-explain' functions.
;; They work on the current value of the variable `ee-bets'.
;; Tests:
;;
;;   (defun eejump-100 () (set-frame-font "nil2"))
;;   (progn (eek "<up> C-e") (cdr (ee-bets-set)))
;;
;;   (find-2a nil '(find-estring (ee-explain-eejump)))
;;
;;   (code-c-d "ud" "/usr/share/doc/")
;;   (progn (eek "<up> C-e") (cdr (ee-bets-set)))
;;
;;   (find-2a nil '(find-estring (ee-explain-code-c-d)))
;; (buffer-substring 1 10)


(defun ee-explain-eejump ()
  (when (and (equal "defun" (ee-bets-text 1))
	   (string-match "^eejump-" (ee-bets-text 2)))
    (ee-bets-flash 2 7 0)
    (ee-bets-flash 4)
    (format "The sexp %s
Makes `M-%sj' execute this one-liner:  %s
See: (find-eev-quick-intro \"7.1. `eejump'\")"
	    (ee-bets-text 0)
	    (substring (ee-bets-text 2) 7)
	    (ee-bets-text 4))))

(defun ee-explain-code-c-d ()
  (when (equal "code-c-d" (ee-bets-text 1))
    (ee-bets-flash 2 1 -1)
    (ee-bets-flash 3 1 -1)
    (let* ((c (substring (ee-bets-text 2) 1 -1))
	   (d (substring (ee-bets-text 3) 1 -1))
	   (cpos (- (ee-bets-begin 2) (ee-bets-begin 0)))
	   (dpos (- (ee-bets-begin 3) (ee-bets-begin 0)))
	   )
      (format "The sexp %s
  makes:      (find-%sfile             \"FNAME\")
 act as:    (find-fline \"%sFNAME\")

See: (find-eev-quick-intro \"9.1. `code-c-d'\")"
	      (ee-bets-text 0)
	      c
	      d))))

(defun ee-explain-code-c-d ()
  (when (equal "code-c-d" (ee-bets-text 1))
    (let* ((c (ee-bets-flash 2 1 -1 nil 'eepitch-star-face))
	   (d (ee-bets-flash 3 1 -1))
	   )
      (format "The sexp %s
  makes:      (find-%sfile             \"FNAME\")
 act as:    (find-fline \"%sFNAME\")

See: (find-eev-quick-intro \"9.1. `code-c-d'\")"
	      (ee-bets-text 0)
	      c
	      d))))

;; (substring "abcdef" 1 -1)
;; (substring "abcdef" 1 nil)
;; (substring "abcdef" nil nil)
;; (substring "abcdef" 0 -1)


(defun eev-explain ()
  (interactive)
  (let ((ee-buffer-name "*eev-explain*")
	(str (or (ee-explain-eejump)
		 (ee-explain-code-c-d)
		 (ee-explain-code-pdf-page)
		 )
	     ))
    (if str
	(find-2a nil '(find-estring str)))))

;; (defun eejump-100 () (set-frame-font "nil2"))
;; (progn (eek "<up> C-e") (ee-bets-set))
;; ee-bets
;;
;; (ee-explain-eejump)
;; (eev-explain)





(provide 'eev-explain)






;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
