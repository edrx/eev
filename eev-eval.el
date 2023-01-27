;;; eev-eval.el -- variants of eval-last-sexp.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012-2023 Free Software Foundation, Inc.
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
;; Version:    20230127
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-eval.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-eval.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                 <http://anggtwu.net/eev-intros/find-eval-intro.html>
;;                                               (find-eev-intro)
;;                                               (find-eval-intro)

;;; Commentary:

;; This file implements:
;;   `M-E' (`ee-eval-last-sexp'), that is a better     `C-x C-e', and
;;   `M-e' (`ee-eval-sexp-eol'),  that is a better `C-e C-x C-e',
;; as described here:
;;   (find-eev-quick-intro "2. Evaluating Lisp")
;;   (find-eev-quick-intro "2. Evaluating Lisp" "When you type `M-e'")
;;   (find-eev-quick-intro "2. Evaluating Lisp" "numeric prefixes")
;;   (find-eev-quick-intro "2. Evaluating Lisp" "`M-0 M-e'")
;;   (find-eval-intro "`M-E' (meta-shift-e)")
;;   (find-lexical-intro "0. How to use this")
;;   (find-lexical-intro "0. How to use this" "`M-e' _sort of_ emulates")
;;
;; Note that `M-2 M-e' and `M-3 M-e' only make sense when the sexp is
;; a hyperlink.

;; Index:
;; «.tools»			(to "tools")
;; «.arg-variants»		(to "arg-variants")
;; «.ee-eval-last-sexp»		(to "ee-eval-last-sexp")
;; «.two-old-definitions»	(to "two-old-definitions")

(require 'eev-flash)		; (find-eev "eev-flash.el")
(require 'eev-multiwindow)	; (find-eev "eev-multiwindow.el")






;;;  _____           _     
;;; |_   _|__   ___ | |___ 
;;;   | |/ _ \ / _ \| / __|
;;;   | | (_) | (_) | \__ \
;;;   |_|\___/ \___/|_|___/
;;;                        
;; «tools»  (to ".tools")
;; Move backward and forward by sexps, get sexp, eval in special ways.

;; See (find-efunction 'eval-last-sexp-1)
(defun ee-backward-sexp ()
  "Like `(forward-sexp -1)' but always uses elisp syntax.
This is an internal function used by `ee-eval-last-sexp'."
  (interactive)
  (let ((forward-sexp-function nil))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (forward-sexp -1)
      (when (eq (preceding-char) ?\\)
	(forward-char -1)
	(when (eq (preceding-char) ??)
	  (forward-char -1)))))
  (point))

(defun ee-forward-sexp ()
  "Like `(forward-sexp 1)' but always uses elisp syntax.
This is an internal function used by `ee-eval-last-sexp'."
  (interactive)
  (let ((forward-sexp-function nil))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (forward-sexp 1)))
  (point))

(defun ee-last-sexp ()
  "An internal function used by `ee-eval-last-sexp'."
  (save-excursion
    (buffer-substring-no-properties
     (ee-backward-sexp) (ee-forward-sexp))))

(defmacro ee-no-debug (&rest body)
  `(let ((debug-on-error nil)) ,@body))

;; `ee-eval' is also defined in this other file:
;;   (find-eevfile "eepitch.el" "defun ee-eval")
;;   (defun ee-eval (sexp) (let ((debug-on-error nil)) (eval sexp)))
;;
(defun ee-eval (sexp)
  "Eval SEXP with `debug-on-error' turned off."
  (ee-no-debug (eval sexp)))

(defun ee-eval-lexical (sexp)
  "Like `ee-eval', but uses lexical binding."
  (ee-no-debug (eval sexp 'lexical)))




;;;                                    _             _       
;;;   __ _ _ __ __ _  __   ____ _ _ __(_) __ _ _ __ | |_ ___ 
;;;  / _` | '__/ _` | \ \ / / _` | '__| |/ _` | '_ \| __/ __|
;;; | (_| | | | (_| |  \ V / (_| | |  | | (_| | | | | |_\__ \
;;;  \__,_|_|  \__, |   \_/ \__,_|_|  |_|\__,_|_| |_|\__|___/
;;;            |___/                                         
;;
;; «arg-variants»  (to ".arg-variants")
;; The variants that are executed when we run `M-0 M-e', `M-1 M-e',
;; etc instead of just `M-e'. To implement a variant for, for example,
;; `M-4 M-2 M-e' you just need to define `ee-eval-last-sexp-42'.

(defun ee-eval-last-sexp-default (&optional arg)
  "Evaluate the sexp before point and show the result in the echo area."
  (prin1 (ee-eval (read (ee-last-sexp)))))

(defun ee-eval-last-sexp-0 ()
  "Highlight the sexp before point."
  (save-excursion
    (eeflash+ (ee-backward-sexp) (ee-forward-sexp)
	      ee-highlight-spec)))

(defun ee-eval-last-sexp-1 ()
  "Show the sexp before point as a string in the echo area."
  (prin1 (ee-last-sexp)))

(defun ee-eval-last-sexp-2 ()
  "Show the target of the sexp before point in another window."
  (find-wset "1so_o" '(ee-eval-last-sexp)))

(defun ee-eval-last-sexp-3 ()
"Show the target of the sexp before point in another window, and switch to it."
  (find-wset "1so_"  '(ee-eval-last-sexp)))

(defun ee-eval-last-sexp-4 ()
  "Evaluate the sexp before point in debug mode."
  (let ((sexp (read (ee-last-sexp))))
    (debug)
    (eval sexp)))

(defun ee-eval-last-sexp-5 ()
  "Evaluate the sexp before point with `debug-on-error' turned on."
  (let ((sexp (read (ee-last-sexp)))
	(debug-on-error t))
    (eval sexp)))

(defun ee-eval-last-sexp-7 ()
  "This is equivalent to M-e <down>."
  (ee-eval-last-sexp) (next-line))

(defun ee-eval-last-sexp-8 ()
  "Evaluate the sexp before point and pretty-print its result in other buffer."
  (find-epp (ee-eval (read (ee-last-sexp)))))

(defun ee-eval-last-sexp-11 ()
  "Like `ee-eval-last-sexp-default', but uses lexical binding."
  (prin1 (ee-eval-lexical (read (ee-last-sexp)))))

;; ;; The old definition for `ee-eval-last-sexp-default',
;; ;; that contained an undocumented hack:
;; ;;
;; (defun ee-eval-last-sexp-default (&optional arg)
;;   "Evaluate the sexp before point and show the result in the echo area."
;;   (prin1 (let ((ee-arg arg)) (ee-eval (read (ee-last-sexp))))))
;;
;; ;; This was hard to explain and to use...
;; ;;
;; (defun ee-eval-last-sexp-9 ()
;;   "A hack for testing `call-interactively'."
;;   (let ((interactive-clause (read (ee-last-sexp))))
;;     (let ((debug-on-error nil))
;;       (call-interactively
;;        `(lambda (&rest args) ,interactive-clause
;; 	  (message "%S" args))))))




;;;  __  __            
;;; |  \/  |       ___ 
;;; | |\/| |_____ / _ \
;;; | |  | |_____|  __/
;;; |_|  |_|      \___|
;;;                    
;; «ee-eval-last-sexp»  (to ".ee-eval-last-sexp")
;; See: (find-emacs-keys-intro "1. Basic keys (eev)")
;;      (find-emacs-keys-intro "1. Basic keys (eev)" "M-e")

(defun ee-eval-sexp-eol (&optional arg)
"Go to the end of line, then run `ee-eval-last-sexp'.
By default, evaluate sexp before eol, and print value in minibuffer.
This is eev's variant of `C-e C-x C-e', and it can behave in
several different ways depending on the prefix argument ARG.
See: (find-eev-quick-intro \"`M-0 M-e'\")

If ARG is:
  nil:  evaluate the sexp (with `debug-on-error' turned off)
    0:  highlight the sexp temporarily
    1:  show the sexp as a string
    2:  eval and show the target of the sexp in another window
    3:  same as 2, but also switch to the new window
    4:  evaluate the sexp in debug mode
    5:  run the sexp with `debug-on-error' turned on
    7:  evaluate then move down
    8:  eval then pretty-print the result in another buffer
   11:  like nil, but using lexical binding.

The listing above shows the default behaviors. To add a special
behavior for, say, ARG=42, define a function
`ee-eval-last-sexp-42'."
  (interactive "P")
  (ee-goto-eol)
  (ee-eval-last-sexp arg))


(defun ee-goto-eol ()
  "Go to the end of the line.
Replace this function by something more complex if you use modes
that put timestamps at the end of the line and you want to make
`M-e' ignore these timestamps."
  (interactive)
  (end-of-line))


;; See: (find-eval-intro "`M-E' (meta-shift-e)")
;;
(defun ee-eval-last-sexp (&optional arg)
  "By default, evaluate sexp before point, and show the result.
This is eev's variant of `eval-last-sexp', and it can behave in
several different ways depending on the prefix argument ARG.

See `ee-eval-sexp-eol'."
  (interactive "P")
  (if (null arg)
      (ee-eval-last-sexp-default)
    (if (fboundp (intern (format "ee-eval-last-sexp-%d" arg)))
	(funcall (intern (format "ee-eval-last-sexp-%d" arg)))
      (ee-eval-last-sexp-default arg))))
      


;; «two-old-definitions»  (to ".two-old-definitions")
;; Two old definitions for `ee-eval-last-sexp'.
;; I'm keeping them here in comments because they are nice to discuss
;; in workshops (when we have time to discuss basic elisp).
;;
;;
;; (defun ee-eval-last-sexp-OLD (&optional arg)
;;   "By default, evaluate sexp before point, and print value in minibuffer.
;; This is eev's variant of `eval-last-sexp', and it can behave in
;; several different ways depending on the prefix argument ARG.
;; See: (find-eev-quick-intro \"`M-0 M-e'\")
;; 
;; If ARG is:
;;   nil:  evaluate the sexp with `debug-on-error' turned off
;;     0:  highlight the sexp temporarily
;;     1:  show the sexp as a string
;;     2:  show the target of the sexp in another window
;;     3:  same, but also switch to the new window
;;     4:  evaluate the sexp in debug mode
;;     5:  run the sexp with `debug-on-error' turned on
;;     7:  this is equivalent to `M-e <down>'
;;     8:  eval then pretty-print the result in another buffer
;;     9:  a hack for testing `call-interactively'
;; other: set EE-ARG to ARG and eval (ee-last-sexp)."
;;   (interactive "P")
;;   (cond ((eq arg 0) (ee-eval-last-sexp-0))
;;         ((eq arg 1) (ee-eval-last-sexp-1))
;;         ((eq arg 2) (ee-eval-last-sexp-2))
;;         ((eq arg 3) (ee-eval-last-sexp-3))
;;         ((eq arg 4) (ee-eval-last-sexp-4))
;;         ((eq arg 5) (ee-eval-last-sexp-5))
;;         ((eq arg 7) (ee-eval-last-sexp-7))
;;         ((eq arg 8) (ee-eval-last-sexp-8))
;;         ((eq arg 9) (ee-eval-last-sexp-9))
;;         (t (prin1 (let ((ee-arg arg))
;;                     (ee-eval (read (ee-last-sexp))))))))
;; 
;; 
;; (defun ee-eval-last-sexp-VERY-OLD (&optional arg)
;;   "By default, evaluate sexp before point, and print value in minibuffer.
;; This is eev's variant of `eval-last-sexp', and it can behave in
;; several different ways depending on the prefix argument ARG.
;; If ARG is:
;;   nil:  evaluate the sexp with `debug-on-error' turned off
;;     0:  highlight the sexp temporarily
;;     1:  show the sexp as a string
;;     2:  show the target of the sexp in another window
;;     3:  same, but also switch to the new window
;;     4:  evaluate the sexp in debug mode
;;     5:  run the sexp with `debug-on-error' turned on
;;     8:  eval then pretty-print the result in another buffer
;;     9:  a hack for testing `call-interactively'"
;;   (interactive "P")
;;   (cond ((eq arg 0)
;;          (save-excursion
;;            (eeflash+ (ee-backward-sexp) (ee-forward-sexp)
;;                      ee-highlight-spec)))
;;         ((eq arg 1) (prin1 (ee-last-sexp)))
;;         ((eq arg 2) (find-wset "1so_o" ' (ee-eval-last-sexp)))
;;         ((eq arg 3) (find-wset "1so_"  ' (ee-eval-last-sexp)))
;;         ((eq arg 4) (let ((sexp (read (ee-last-sexp)))) (debug) (eval sexp)))
;;         ((eq arg 5) (let ((sexp (read (ee-last-sexp)))
;;                           (debug-on-error t))
;;                       (eval sexp)))
;;         ((eq arg 8) (find-epp (ee-eval (read (ee-last-sexp)))))
;;         ((eq arg 9) (let ((interactive-clause (read (ee-last-sexp))))
;;                       (let ((debug-on-error nil))
;;                         (call-interactively
;;                          `(lambda (&rest args) ,interactive-clause
;;                             (message "%S" args))))))
;;         (t (prin1 (let ((ee-arg arg))
;;                     (ee-eval (read (ee-last-sexp))))))))

(provide 'eev-eval)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
