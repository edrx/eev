;;; eev-flash.el -- functions to highlight a range of text temporarily.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012-2019,2023 Free Software Foundation, Inc.
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
;; Latest version: <http://anggtwu.net/eev-current/eev-flash.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-flash.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                 <http://anggtwu.net/eev-intros/find-eval-intro.html>
;;                                               (find-eev-intro)
;;                                               (find-eval-intro "M-0 M-e")

;; These functions are used by some variants of `ee-eval-last-sexp'
;; (especially `M-0 M-e') and by `eev-bounded' and friends.
;; (find-eev "eev-eval.el")
;; (find-eev "eev-bounded.el")

;; «.specs»	(to "specs")
;; «.eeflash»	(to "eeflash")





;; The old code follows.
;; To do: delete most of this, use just `ee-flash' instead.
;; Drop the idea of flash-specs as lists.

;; «specs»  (to ".specs")
;; (setq eeb-highlight-spec '(highlight 0.2))
(defvar ee-highlight-spec  '(highlight 0.75)) ; to do: rename highlight->flash
(defvar eeb-highlight-spec '(highlight 0.5))
(defvar eek-highlight-spec '(region 0.75))
(defvar eeflash-default    '(highlight 0.5))


;;;             __ _           _     
;;;   ___  ___ / _| | __ _ ___| |__  
;;;  / _ \/ _ \ |_| |/ _` / __| '_ \ 
;;; |  __/  __/  _| | (_| \__ \ | | |
;;;  \___|\___|_| |_|\__,_|___/_| |_|
;;;                                  
;;; temporary highlighting (flashing)
;; «eeflash»  (to ".eeflash")

(defun ee-flash (start end &optional face duration)
  "Highlight the region between START and END using FACE, for time DURATION."
  (let ((ovl (make-overlay start end)))
    (overlay-put ovl 'face (or face 'region))
    (run-at-time (or duration 1) nil 'delete-overlay ovl)))

(defun eeflash (start end &optional face duration)
  "Highlight the region between START and END using FACE, for time DURATION."
  (let ((ovl (make-overlay start end)))
    (overlay-put ovl 'face (or face 'region))
    (run-at-time (or duration 1) nil 'delete-overlay ovl)))

(defun eeflash+ (s &optional e spec add-to-e)
  "Highlight the region between S and E; face and duration are taken from SPEC.
This function only tries to do any work when S is a number and SPEC is non-nil.
When SPEC is non-nil it should be a pair of the form (FACE DURATION).
The argument ADD-TO-E is a hack for when we know that the region between S and
E+1 ends with a newline and it looks nicer to highlight the newline too; then
we set ADD-TO-E to 1."
  (if (and (numberp s) spec)
      (eeflash s (+ e (or add-to-e 0))
	       (car spec) (cadr spec)))
  (list s e spec add-to-e))


(provide 'eev-flash)



;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
