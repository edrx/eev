;;; eev-kl-here.el -- Kill link to here.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.
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
;; Version:    20231223
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-kl-here.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-kl-here.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-kla-intro.html>
;;                                               (find-kla-intro)

;;; Commentary:

;;; This file implements some commands that look like the ones in
;;; `eev-kla.el' but that try to be smart in a way similar to
;;; `find-here-links'. Everything here is VERY experimental.

;; Index:
;; «.generate-sexps»	(to "generate-sexps")
;; «.kill-sexps»	(to "kill-sexps")
;; «.aliases»		(to "aliases")

(require 'eev-kla)		; (find-eev "eev-kla.el")
(require 'eev-hlinks)		; (find-eev "eev-hlinks.el")





;;;  ____                      
;;; / ___|  _____  ___ __  ___ 
;;; \___ \ / _ \ \/ / '_ \/ __|
;;;  ___) |  __/>  <| |_) \__ \
;;; |____/ \___/_/\_\ .__/|___/
;;;                 |_|        
;;
;; «generate-sexps»  (to ".generate-sexps")
;; See: (find-eev "eev-kla.el" "generate-sexps")
;;      (find-eev "eev-hlinks.el" "hprog")
;;      (find-efunction 'ee-find-info-links)
;;      (find-efunction 'ee-find-intro-links)
;;
(defun ee-find-einfo-link (str)
  "An internal function used by `ee-kl-sexp-klin'."
  (let ((pos-spec-list (if str (list str))))
    (if (ee-info-shortp)
	`(,(ee-info-shortf) ,(ee-info-node) ,@pos-spec-list)
      `(find-node ,(ee-info-fullnode) ,@pos-spec-list))))

(defun ee-find-eintro-link (str)
  "An internal function used by `ee-kl-sexp-klin'."
  (let* ((stem (ee-intro-stem))
	 (find-xxx-intro (ee-intern "find-%s-intro" stem))
	 (pos-spec-list (if str (list str))))
    `(,find-xxx-intro ,@pos-spec-list)))

(defun ee-kl-sexp-klin (&optional str)
  "<K>ill <l>ink to a <in>fo or <in>tro - make sexp."
  (cond ((ee-info-bufferp)  (ee-find-einfo-link  str))
	((ee-intro-bufferp) (ee-find-eintro-link str))
	(t (error "Not in info or in an intro!"))))


;; Todo: adapt:
;; (find-eev "eev-hlinks.el" "ee-hprog-find-here-links")
;; (find-eev "eev-hlinks.el" "ee-hlang-run")


;;;  _  ___ _ _     
;;; | |/ (_) | |___ 
;;; | ' /| | | / __|
;;; | . \| | | \__ \
;;; |_|\_\_|_|_|___/
;;;                 
;; «kill-sexps»  (to ".kill-sexps")
;; Commands that push sexps into the kill ring.
;; See: (find-eev "eev-kla.el" "kill-sexps")
;;
(defun eeklin ()
  "<K>ill <l>ink to <in>fo or <in>tro."
  (interactive)
  (ee-kl-kill (ee-kl-sexp-klin nil)))

(defun eeklins ()
  "<K>ill <l>ink to <in>fo or <in>tro and a <s>tring."
  (interactive)
  (ee-kl-kill (ee-kl-sexp-klin (ee-kl-region))))


;;;     _    _ _                     
;;;    / \  | (_) __ _ ___  ___  ___ 
;;;   / _ \ | | |/ _` / __|/ _ \/ __|
;;;  / ___ \| | | (_| \__ \  __/\__ \
;;; /_/   \_\_|_|\__,_|___/\___||___/
;;;                                  
;; «aliases»  (to ".aliases")
;; See: (find-eev "eev-kla.el" "aliases")
;;      (find-kla-intro "4. Aliases")
;; I use these aliases:
;; (defalias 'klin  'eeklin)
;; (defalias 'klins 'eeklins)


(provide 'eev-kl-here)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
