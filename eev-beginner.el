;;; eev-beginner.el -- load eev and turn eev-mode on.

;; Copyright (C) 2019 Free Software Foundation, Inc.
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
;; Version:    2019mar05
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-beginner.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                                                (find-eev-quick-intro)

;;; Commentary:

;; Most beginners start playing with Emacs+eev by following the
;; instructions here,
;;
;;   (find-eev-quick-intro "1. Installing eev")
;;
;; i.e., by copying and pasting a certain script to a terminal. That
;; script creates an executable file called "~/eev" that switches to
;; the directory where eev was unpacked and invokes Emacs like this:
;;
;;   emacs -l eev-beginner.el --eval="(find-eev-quick-intro)" $*
;;
;; which makes Emacs load "eev-beginner.el" (this file!) and run
;; `(find-eev-quick-intro)' to open the "eev quick intro" tutorial.
;; This file simply loads all the default modules of eev and turns
;; eev-mode on.
;;
;; Older versions of eev loaded "eev-readme.el" instead of
;; "eev-beginner.el". See:
;;
;;   (find-eev "eev-readme.el")

(add-to-list 'load-path default-directory)
(require 'eev-load)	       ; (find-eev "eev-load.el")
(eev-mode 1)

;; (find-eev-install-intro "1. Installing eev by hand")
;; (find-eev-install-intro "1. Installing eev by hand" "require 'eev-load")

(provide 'eev-beginner)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
