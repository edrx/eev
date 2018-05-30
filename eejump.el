;;; eejump.el -- jump quickly to predefined places.

;; Copyright (C) 2012,2016 Free Software Foundation, Inc.
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
;; Version:    2016sep23
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eejump.el>
;;       htmlized: <http://angg.twu.net/eev-current/eejump.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-eejump-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-eev-quick-intro)
;;                                                (find-eejump-intro)

;;; Commentary:

;; See: (find-eev-quick-intro "7.1. eejump")
;;      (find-eev-quick-intro "7.1. eejump" "meta-uppercase-j")
;;      (find-eejump-intro)
;;      (find-eev "eev-wrap.el" "eewrap-eejump")
;;      (find-eev "eev-blinks.el" "find-eejumps")




;; This module installs a key binding into eev-mode-keymap, so:
(require 'eev-mode)			; (find-eev "eev-mode.el")

(define-key eev-mode-map "\M-j" 'eejump)

(defun eejump (arg)
  "See: (find-eev-quick-intro \"7.1. eejump\")
and: (find-eejump-intro)"
  (interactive "P")
  (if (null arg)
      (find-eejumps)			; was: (eejump-*)
    (if (fboundp (intern (format "eejump-%d" arg)))
	(funcall (intern (format "eejump-%d" arg)))
      (eejump-str* (format "%d" arg)))))

(defun eejump-str* (str)
  "An internal, recursive function used by `eejump'.
See: (find-eejump-intro \"\\neejump\\n\")"
  (if (fboundp (intern (format "eejump-%s*" str)))
      (funcall (intern (format "eejump-%s*" str)))
    (eejump-str* (substring str 0 -1))))





;; This is an "eejump block", as described in:
;;   (find-eejump-intro "eejump blocks")
;; You should probably copy this to your .emacs - and
;; then start modifying it.
;;
;; Note that with eev-mode you can use:
;;   M-e to follow elisp hyperlinks,   see: (find-eval-intro "`M-e'")
;;   M-k to go back,                   see: (find-eval-intro "`M-k'")
;;   M-j to jump to predefined places, see: (find-eejump-intro "Families")
;;                  in particular:  M-j --> (find-efunction 'eejump-*)
;;                                 M-2j --> (find-emacs-intro)
;;                                 M-5j --> (find-eev-intro)
;;                                M-50j --> (find-eev "eev-readme.el")
;;
(defun eejump-*   () (find-efunction 'eejump-*))
(defun eejump-1   () (find-fline "~/TODO"))
(defun eejump-10  () (set-frame-font "5x7"  t))
(defun eejump-11  () (set-frame-font "6x13" t))
(defun eejump-2   () (find-emacs-keys-intro))
(defun eejump-5*  () (find-efunction 'eejump-5*))
(defun eejump-5   () (find-eev-quick-intro))
(defun eejump-50  () (find-eev "eev-readme.el"))
(defun eejump-59  () (find-eev-update-links))
(defun eejump-6   () (find-freenode    "#eev"))
(defun eejump-66  () (find-freenode-3a "#eev"))

(defun eejump-55  () (find-fline "~/.emacs"))
(defun eejump-552 () (find-eev "eev2-all.el"))
(defun eejump-555 () (find-eev ""))




(provide 'eejump)

;; To see all current targets, run:
;;   (find-eejumps)



;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
