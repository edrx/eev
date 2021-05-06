;;; eev-beginner.el -- load eev, turn eev-mode on, open a tutorial.

;; Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
;; Version:    2021may06
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-beginner.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                                                (find-eev-quick-intro)

;;; Commentary:

;; This file loads all modules of eev, turns eev-mode on, and opens
;; the main tutorial - i.e., runs `(find-eev-quick-intro)'. It is used
;; by the two most common ways of installing eev:
;;
;; 1) If you install eev as an emacs package via `M-x list-packages'
;;    then no eev modules will be loaded UNTIL you run `M-x
;;    eev-beginner', which loads everything, starts eev-mode, and
;;    opens the main tutorial. Note that `eev-beginner' is the ONLY
;;    autoloaded function in eev! See:
;;
;;      (find-eev-quick-intro "1. Installing eev")
;;      (find-eev-intro "1. `eev-mode'")
;;      (find-eev-intro "1. `eev-mode'" "If you load eev")
;;
;; 2) Some people start playing with Emacs+eev by copying and pasting
;;    a certain script to a shell in a terminal and executing it,
;;    following the instructions here:
;;
;;      (find-eev-quick-intro "1. Installing eev")
;;      (find-eev-quick-intro "1. Installing eev" "tarball")
;;      (find-eev-install-intro "5.1. Using the tarball")
;;
;;    That script creates an executable file called "~/eev" whose contents
;;    are something like this:
;;
;;      #!/bin/sh
;;      cd ~/eev2/ && emacs -l eev-beginner.el --eval="(find-eev-quick-intro)" $*
;;
;;    Running "~/eev" on a shell starts Emacs and makes it load this
;;    file - that loads all the other modules of eev - and then makes
;;    Emacs evaluate the sexp `(find-eev-quick-intro)', that opens the
;;    main tutorial.
;;
;; The idea is that even after installing eev:
;;
;; 1) it should be trivial to try eev in "beginner mode",
;;
;; 2) it should be trivial to start an Emacs without anything from
;;    eev, or with at most with one "autoload" from eev - see:
;;
;;      (find-enode "Lisp Libraries" "autoloaded")
;;
;; Some people are very finicky about packages that make global
;; changes when loaded. Loading all modules of eev causes the (almost
;; insignificant?) global changes described here,
;;
;;   (find-eev-intro "1. " "the only" "things that happen")
;;
;; that aren't reverted by deactivating eev-mode with `M-x eev-mode',
;; and eev defines some functions with the prefix `find-', as
;; explained here:
;;
;;   (find-eev-intro "4. The prefix `find-'")
;;
;; I've tried to make eev friendly to several kinds of people,
;; including total beginners and very finicky old-timers, and this
;; file - "eev-beginner.el" - seems to provide a good solution.
;;
;; The instructions for making Emacs load eev at startup are here:
;;
;;   (find-eev-install-intro "2. The expert setup")


;; NOTE: older versions of eev loaded "eev-readme.el" instead of
;; "eev-beginner.el". Compare:
;;
;;   (find-eev "eev-readme.el")
;;   (find-eev "eev-load.el")



;; 2019mar13: Commented this out.
;; 2019mar29: Uncommented - it seems that emacs25 needs this.
(add-to-list 'load-path default-directory)

;; Load all the main modules of eev.
;; Do not load some advanced modules that require extra setup.
;; See the comments here: (find-eev "eev-load.el")
(require 'eev-load)

;; Turn eev-mode on.
;; See: (find-eev-intro "1. `eev-mode'")
(eev-mode 1)

;; Open the main tutorial.
(find-eev-quick-intro)

;; `eev-beginner' is the only autoload of eev at this moment.
;; See: (find-elnode "Autoload" "magic autoload comment")
;;      (find-eev "eev-load.el" "autoloads")

;;;###autoload
(defun eev-beginner ()
  "Load all basic modules of eev, turn eev-mode on, and open a tutorial."
  (interactive)
  (require 'eev-load)
  (eev-mode 1)
  (find-eev-quick-intro))


(provide 'eev-beginner)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
