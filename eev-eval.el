;;; eev-eval.el -- variants of eval-last-sexp.

;; Copyright (C) 2012,2019 Free Software Foundation, Inc.
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
;; Version:    2019mar02
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-eval.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-eval.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-eval-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-eval-intro)

;;; Commentary:

;; This file implements:
;;   `M-E' (`ee-eval-last-sexp'), that is a better     `C-x C-e', and
;;   `M-e' (`ee-eval-sexp-eol'),  that is a better `C-e C-x C-e',
;; as described here:
;;   (find-eev-quick-intro "2. Evaluating Lisp")
;;   (find-eev-quick-intro "2. Evaluating Lisp" "When you type `M-e'")
;;   (find-eev-quick-intro "2. Evaluating Lisp" "numeric prefixes")
;;   (find-eev-quick-intro "2. Evaluating Lisp" "`M-0 M-e'")
;; Note that `M-2 M-e' and `M-2 M-e' only make sense when the sexp is
;; a hyperlink.


(require 'eev-flash)		; (find-eev "eev-flash.el")
(require 'eev-multiwindow)	; (find-eev "eev-multiwindow.el")



;;;                  _                                            _ 
;;;   _____   ____ _| |      ___  _____  ___ __         ___  ___ | |
;;;  / _ \ \ / / _` | |_____/ __|/ _ \ \/ / '_ \ _____ / _ \/ _ \| |
;;; |  __/\ V / (_| | |_____\__ \  __/>  <| |_) |_____|  __/ (_) | |
;;;  \___| \_/ \__,_|_|     |___/\___/_/\_\ .__/       \___|\___/|_|
;;;                                       |_|                       
;;;
;;; Evaluating sexps (alternatives to eval-last-sexp)
;;;

;; See (find-efunction 'eval-last-sexp-1)
(defun ee-backward-sexp ()
  "An internal function used by `ee-eval-last-sexp'."
  (interactive)
  (with-syntax-table emacs-lisp-mode-syntax-table
    (forward-sexp -1)
    (when (eq (preceding-char) ?\\)
      (forward-char -1)
      (when (eq (preceding-char) ??)
	(forward-char -1))))
  (point))

(defun ee-forward-sexp ()
  "An internal function used by `ee-eval-last-sexp'."
  (interactive)
  (with-syntax-table emacs-lisp-mode-syntax-table
    (forward-sexp 1))
  (point))

(defun ee-last-sexp ()
  "An internal function used by `ee-eval-last-sexp'."
  (save-excursion
    (buffer-substring-no-properties
     (ee-backward-sexp) (ee-forward-sexp))))

(defmacro ee-no-debug (&rest body)
  `(let ((debug-on-error nil)) ,@body))

;; `ee-eval' is also defined elsewhere:
;;   (find-eevfile "eepitch.el" "defun ee-eval")
;;   (defun ee-eval (sexp) (let ((debug-on-error nil)) (eval sexp)))

(defun ee-eval (sexp) (ee-no-debug (eval sexp)))

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

(defun ee-eval-last-sexp-8 ()
  "Evaluate the sexp before point and pretty-print its result in other buffer."
  (find-epp (ee-eval (read (ee-last-sexp)))))

(defun ee-eval-last-sexp-9 ()
  "A hack for testing `call-interactively'"
  (let ((interactive-clause (read (ee-last-sexp))))
    (let ((debug-on-error nil))
      (call-interactively
       `(lambda (&rest args) ,interactive-clause
	  (message "%S" args))))))


(defun ee-eval-last-sexp (&optional arg)
  "By default, evaluate sexp before point, and print value in minibuffer.
This is eev's variant of `eval-last-sexp', and it can behave in
several different ways depending on the prefix argument ARG.
See: (find-eev-quick-intro \"`M-0 M-e'\")

If ARG is:
  nil:  evaluate the sexp with `debug-on-error' turned off
    0:  highlight the sexp temporarily
    1:  show the sexp as a string
    2:  show the target of the sexp in another window
    3:  same, but also switch to the new window
    4:  evaluate the sexp in debug mode
    5:  run the sexp with `debug-on-error' turned on
    8:  eval then pretty-print the result in another buffer
    9:  a hack for testing `call-interactively'
other: set EE-ARG to ARG and eval (ee-last-sexp)."
  (interactive "P")
  (cond ((eq arg 0) (ee-eval-last-sexp-0))
	((eq arg 1) (ee-eval-last-sexp-1))
	((eq arg 2) (ee-eval-last-sexp-2))
	((eq arg 3) (ee-eval-last-sexp-3))
	((eq arg 4) (ee-eval-last-sexp-4))
	((eq arg 5) (ee-eval-last-sexp-5))
	((eq arg 8) (ee-eval-last-sexp-8))
	((eq arg 9) (ee-eval-last-sexp-9))
	(t (prin1 (let ((ee-arg arg))
		    (ee-eval (read (ee-last-sexp))))))))

;; (defun ee-eval-last-sexp (&optional arg)
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

(defun ee-eval-sexp-eol (&optional arg)
"Go to the end of line, then run `ee-eval-last-sexp'.
See: (find-eval-intro)"
  (interactive "P")
  (end-of-line)
  (ee-eval-last-sexp arg))



(provide 'eev-eval)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
