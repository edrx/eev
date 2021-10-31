;; eev-multiwindow.el - functions to create multi-window setups  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
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
;; Version:    20211031
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-multiwindow.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-multiwindow.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:
;;
;; For the main ideas, see: (find-multiwindow-intro)



;;;   __ _           _                        _   
;;;  / _(_)_ __   __| |    __      _____  ___| |_ 
;;; | |_| | '_ \ / _` |____\ \ /\ / / __|/ _ \ __|
;;; |  _| | | | | (_| |_____\ V  V /\__ \  __/ |_ 
;;; |_| |_|_| |_|\__,_|      \_/\_/ |___/\___|\__|
;;;                                               
;; These functions are explained here:
;; (find-multiwindow-intro "1. Introduction")
;; (find-multiwindow-intro "2. `find-wset'")
;; (find-multiwindow-intro "8. Adding support for new characters in `find-wset'")

(defun find-wset-1 () (delete-other-windows))
(defun find-wset-2 () (split-window-vertically))
(defun find-wset-3 () (split-window-horizontally))
(defun find-wset-s () (split-window-sensibly (selected-window)))
(defun find-wset-o () (other-window 1))
(defun find-wset-O () (other-window -1))
(defun find-wset-+ () (balance-windows))
(defun find-wset-c () (recenter))
(defun find-wset-_ () (eval (car sexps)) (setq sexps (cdr sexps)))
(defun find-wset-\  ())			; allow whitespace

(defun find-wset (chars &rest sexps)
  "Create a multi-window setting according to CHARS and SEXPS.
A detailed explanation is here: (find-multiwindow-intro)

Here is a list of the standard characters that can be used in CHARS:
  1:  `delete-other-windows'       (C-x C-1)
  2:  `split-window-vertically'    (C-x C-2)
  3:  `split-window-horizontally'  (C-x C-3)
  s:  `split-window-sensibly'
  o:  `other-window'               (C-x o)
  +:  `balance-windows'            (C-x +)
  c:  `recenter'                   (C-l)
  _:  execute the next sexp in SEXPS.

To add support for a new character, say `C', just define
a function `find-wset-C'."
  (if (not (equal chars ""))
      (let ((c     (substring chars 0 1))
	    (chars (substring chars 1)))
	(funcall (ee-intern "find-wset-%s" c))
	(apply 'find-wset chars sexps))))


;; High-level functions.
;; See: (find-multiwindow-intro "3. High-level words")
;;
(defun find-2a (a b)   (find-wset "13_o_o" a b))
(defun find-2b (a b)   (find-wset "13_o_"  a b))
(defun find-3a (a b c) (find-wset "13_o2_o_o"  a b c))
(defun find-3b (a b c) (find-wset "13_o2_o_oo" a b c))
(defun find-3c (a b c) (find-wset "13_o2_o_"   a b c))







;;;                  _ _       _       _                _        
;;;   ___  ___ _ __ (_) |_ ___| |__   | |__   __ _  ___| | _____ 
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \  | '_ \ / _` |/ __| |/ / __|
;;; |  __/  __/ |_) | | || (__| | | | | | | | (_| | (__|   <\__ \
;;;  \___|\___| .__/|_|\__\___|_| |_| |_| |_|\__,_|\___|_|\_\___/
;;;           |_|                                                
;; See:
;; (find-multiwindow-intro "4. Several eepitch targets")
;; (find-multiwindow-intro "5. Restarting eepitch targets")
;; (find-multiwindow-intro "7. Eepitch blocks for two targets")

(defun ee-here (code)
  "Example: (ee-here '(eepitch-xxx)) opens the target of (eepitch-xxx) here.
\"Here\" means \"in the current window, without disturbing the
current window configuration\". Normal calls to `eepitch-xxx'
functions split the screen and open the target buffer in another
window; by wrapping them in an `(ee-here ...)' we can bypass
that. This is mainly for `find-wset'."
  (let (result)
    (find-ebuffer
     (save-window-excursion
       (setq result (eval code))
       eepitch-buffer-name))
    result))

(defun ee-here-reset (code)
  "Like `ee-here', but also does an `eepitch-kill'."
  (let (result)
    (find-ebuffer
     (save-window-excursion
       (eval code)
       (eepitch-kill)
       (setq result (eval code))
       eepitch-buffer-name))
    result))


;; Mnemonic: "e" and "E" are both for preparing eepitch windows,
;; and "E" is more aggressive than "e" (it yells at you).
(defun find-wset-e () (ee-here       (car sexps)) (setq sexps (cdr sexps)))
(defun find-wset-E () (ee-here-reset (car sexps)) (setq sexps (cdr sexps)))
(defun find-3ee (b c) (find-wset "13o2eoeo" b c))
(defun find-3EE (b c) (find-wset "13o2EoEo" b c))

;; When I invented this I used "=" and "!" instead of "e" and "E" -
;; but I think that "e" and "E" are better.
(defun find-wset-= () (ee-here       (car sexps)) (setq sexps (cdr sexps)))
(defun find-wset-! () (ee-here-reset (car sexps)) (setq sexps (cdr sexps)))





(provide 'eev-multiwindow)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
