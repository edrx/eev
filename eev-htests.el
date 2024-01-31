;;; eev-htests.el -- Tests for eev-hlinks.el.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.
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
;; Version:    20240128
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-htests.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-htests.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-kla-intro.html>
;;                                               (find-kla-intro)

;;; Comment:

;; This file implements a way to test each case of `find-here-links'.
;; See:
;;   (find-eev "eev-hlinks.el" "hprog")

;; «.find-tlh»		(to "find-tlh")
;; «.tests»		(to "tests")



;;;   __ _           _       _   _ _     
;;;  / _(_)_ __   __| |     | |_| | |__  
;;; | |_| | '_ \ / _` |_____| __| | '_ \ 
;;; |  _| | | | | (_| |_____| |_| | | | |
;;; |_| |_|_| |_|\__,_|      \__|_|_| |_|
;;;                                      
;; «find-tlh»  (to ".find-tlh")
;; In this block we define two functions, `find-tlhs' and `find-tlhi',
;; that test functions that generate links to here. See:
;;
;;   (find-eaproposf "ee-find.*links")
;;   (find-eaproposf "ee-find.*linki")
;;   (find-eaproposf "ee-find.*link[si]")

(defun find-tlhs (sexp1 sexp2)
  "Test (a function that generates) links to here.
This function generates a 3-window setting like this,
   ___________
  |     |     |
  |     |  B  |
  |  A  |_____|
  |     |     |
  |     |  C  |
  |_____|_____|

in which A is the current buffer, B is the buffer obtained by
running SEXP1 in the buffer A, and C is the result of running
(find-elinks SEXP2) in the buffer B."
  (find-wset "13o_2o_o" sexp1 `(find-elinks ,sexp2)))

(defun find-tlhi (sexp1 sexp2)
  "Test (a function that generates a single) link to here.
This function generates a 3-window setting like this,
   ___________
  |     |     |
  |     |  B  |
  |  A  |_____|
  |     |     |
  |     |  C  |
  |_____|_____|

in which A is the current buffer, B is the buffer obtained by
running SEXP1 in the buffer A, and C is the result of running
(find-epp SEXP2) in the buffer B."
  (find-wset "13o_2o_o" sexp1 `(find-epp ,sexp2)))



;;;  _____         _       
;;; |_   _|__  ___| |_ ___ 
;;;   | |/ _ \/ __| __/ __|
;;;   | |  __/\__ \ |_\__ \
;;;   |_|\___||___/\__|___/
;;;                        
;; «tests»  (to ".tests")
;; See:
;;   (find-eev "eev-hlinks.el"  "hprog")
;;   (find-eev "eev-kl-here.el" "hprog")
;; Tests:
;;   (find-tlhs '(find-enode "Lisp Eval")   '(ee-find-info-links))
;;   (find-tlhi '(find-enode "Lisp Eval")   '(ee-find-info-linki))
;;   (find-tlhs '(find-node "(rcirc)Index") '(ee-find-info-links))
;;   (find-tlhi '(find-node "(rcirc)Index") '(ee-find-info-linki))
;;   (find-tlhs '(find-eev-quick-intro)     '(ee-find-intro-links))
;;   (find-tlhi '(find-eev-quick-intro)     '(ee-find-intro-linki))
;;   (find-tlhs '(find-man "1 cat")         '(ee-find-man-links))
;;   (find-tlhi '(find-man "1 cat")         '(ee-find-man-linki))
;;   (find-tlhs '(find-epackage 'magit)     '(ee-find-epackage-links))
;;   (find-tlhi '(find-epackage 'magit)     '(ee-find-epackage-linki))
;;   (find-tlhs '(find-epackages 'abs-mode) '(ee-find-epackages-links))
;;   (find-tlhi '(find-epackages 'abs-mode) '(ee-find-epackages-linki))
;;   (find-tlhs '(find-customizegroup 'editing) '(ee-find-custom-links))
;;   (find-tlhi '(find-customizegroup 'editing) '(ee-find-custom-linki))
;;   (find-tlhs '(find-customizeface 'bold)     '(ee-find-custom-f-links))
;;   (find-tlhi '(find-customizeface 'bold)     '(ee-find-custom-f-linki))
;;   (find-tlhs '(find-customizevariable 'goal-column) '(ee-find-custom-v-links))
;;   (find-tlhi '(find-customizevariable 'goal-column) '(ee-find-custom-v-linki))
;;   (find-tlhs '(find-eshortdoc 'keymaps)     '(ee-find-eshortdoc-links))
;;   (find-tlhi '(find-eshortdoc 'keymaps)     '(ee-find-eshortdoc-linki))
;;   (find-tlhs '(find-ecolors)                '(ee-find-ecolors-links))
;;   (find-tlhi '(find-ecolors)                '(ee-find-ecolors-linki))
;;   (find-tlhs '(find-efaces)                 '(ee-find-efaces-links))
;;   (find-tlhi '(find-efaces)                 '(ee-find-efaces-linki))
;;   (find-tlhs '(find-efunctiondescr 'car)    '(ee-find-efunctiondescr-links))
;;   (find-tlhi '(find-efunctiondescr 'car)    '(ee-find-efunctiondescr-linki))
;;   (find-tlhs '(find-efacedescr 'bold)       '(ee-find-efacedescr-links))
;;   (find-tlhi '(find-efacedescr 'bold)       '(ee-find-efacedescr-linki))
;;   (find-tlhs '(find-evardescr 'goal-column) '(ee-find-evardescr-links))
;;   (find-tlhi '(find-evardescr 'goal-column) '(ee-find-evardescr-linki))



(provide 'eev-htests)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
