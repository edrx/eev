;;; eejump.el -- jump quickly to predefined places.

;; Copyright (C) 2012-2019 Free Software Foundation, Inc.
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
;; Version:    2019mar05
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

;; This file defines `eejump', that is bound to `M-j'. Its usage is
;; explained here:
;;
;;   (find-eev-quick-intro "7.1. `eejump'")
;;   (find-eev-quick-intro "7.2. The list of eejump targets")
;;
;; The functions `find-eejumps' - that is invoked when we type `M-j'
;; without an argument - and `eewrap-eejump' - invoked by `M-J' - are
;; defined in other files:
;;
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "M-j" "eejump")
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "M-J" "eewrap-eejump")
;;   (find-eev "eev-blinks.el" "find-eejumps")
;;   (find-eev "eev-wrap.el" "eewrap-eejump")
;;
;; See:
;;
;;   (find-eev-quick-intro "7.3. Defining eejump targets")
;;   (find-eev-quick-intro "7.3. Defining eejump targets" "M-J")
;;
;; `find-eejumps' is a relatively new idea, and before implementing it
;; `M-j' did something MUCH harder to explain. Suppose that
;;
;;   * `eejump-12345' was not defined,
;;   * `eejump-1234' was not defined,
;;   * `eejump-123' was not defined,
;;   * `eejump-12*' was defined in my .emacs as:
;;     (defun eejump-12* () (find-efunction 'eejump-12*))
;;
;; then typing `M-12345j' would run (find-efunction 'eejump-12*), that
;; would jump to the definition of a `eejump-12*' in my .emacs; I
;; would put all my definitions of eejumps with prefixes starting with
;; "12" close to it. This (old) intro still describes that behavior:
;;
;;   (find-eejump-intro)
;;   (find-eejump-intro "5. eejump blocks")



;; (define-key eev-mode-map "\M-j" 'eejump)
;;
(defun eejump (arg)
  "Execute the one-liner associated to the numeric argument ARG.
When called without an argument execute `find-eejumps', that
shows all the current one-liners associated to numbers.
See: (find-eev-quick-intro \"7.1. `eejump'\")"
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





(defun eejump-*   () (find-efunction 'eejump-*))
(defun eejump-1   () (find-fline "~/TODO"))
(defun eejump-2   () (find-emacs-keys-intro))
(defun eejump-5   () (find-eev-quick-intro))
(defun eejump-6   () (find-escripts-intro))

(defun eejump-10  () (set-frame-font "5x7"  t))
(defun eejump-11  () (set-frame-font "6x13" t))
(defun eejump-12  () (set-frame-font "10x20" t))

(defun eejump-55  () (find-fline "~/.emacs"))
(defun eejump-555 () (find-eev ""))

;; Deleted:
;; (defun eejump-50  () (find-eev "eev-readme.el"))
;; (defun eejump-59  () (find-eev-update-links))
;; (defun eejump-5*  () (find-efunction 'eejump-5*))
;; (defun eejump-6   () (find-freenode    "#eev"))
;; (defun eejump-66  () (find-freenode-3a "#eev"))
;; (defun eejump-552 () (find-eev "eev-load.el"))


;; To see all current targets, run:
;;   (find-eejumps)

(provide 'eejump)



;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
