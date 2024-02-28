;;; eev-beginner.el -- load eev, turn eev-mode on, open a tutorial.

;; Copyright (C) 2019-2021,2023 Free Software Foundation, Inc.
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
;; Latest version: <http://anggtwu.net/eev-current/eev-beginner.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-load.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-quick-intro.html>
;;                                               (find-eev-quick-intro)

;; «.load-path-hack»	(to "load-path-hack")
;; «.eev-beginner»	(to "eev-beginner")


;;; Commentary:

;; 1. Eev for beginners
;; ====================
;; This file loads all modules of eev, turns eev-mode on, and opens
;; the main tutorial - this one:
;;
;;   (find-eev-quick-intro)
;;
;; This file is used by the two most common ways of installing eev
;; "for beginners", described below. The ways of loading eev without
;; opening the tutorial are described in the next section.
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
;;    If you use one of the non-standard package managers for Emacs,
;;    like use-package or straight.el, then read this:
;;
;;      (find-eev-install-intro "5.5. `use-package'")
;;
;; 2) Some people start playing with Emacs+eev by copying and pasting
;;    a certain script to a shell in a terminal and executing it,
;;    following the instructions in the third sexp below:
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
;;
;; 2. Eev for non-beginners
;; ========================
;; This file:
;;
;;   a. loads all modules of eev,
;;   b. turns eev-mode on, and
;;   c. opens the main tutorial.
;;
;; If you want to do just (a), or not even (a) but you want to be
;; able to run `M-x eev-beginner' and `M-x eev-mode', then read:
;;
;;   (find-eev "eev-load.el")
;;   (find-eev-quick-intro "1. Installing eev")
;;
;;
;; 3. Rationale
;; ============
;; The idea is that even after installing eev:
;;
;; 1) it should be trivial to try eev in "beginner mode",
;;
;; 2) it should be trivial to start an Emacs without anything from
;;    eev, or with at most with one "autoload" from eev - see:
;;
;;      (find-enode "Lisp Libraries" "autoloaded")
;;
;; My reason for caring about (2) is that some people are very finicky
;; about packages that make global changes when loaded. Loading all
;; modules of eev causes the (almost insignificant?) global changes
;; described here,
;;
;;   (find-eev-intro "1. " "the only" "things that happen")
;;
;; that aren't reverted by deactivating eev-mode with `M-x eev-mode',
;; _AND_ eev defines some functions with the prefix `find-', as
;; explained here:
;;
;;   (find-eev-intro "4. The prefix `find-'")
;;   (find-eev-intro "4. The prefix `find-'" "list all")
;;
;; Some people consider that these `find-' functions from eev sort of
;; "invade the global namespace", and they want to be able to run
;; Emacs without them, and load eev only when they want or need to.
;; I've tried to make eev friendly to several kinds of people,
;; including total beginners and these very finicky old-timers, and
;; this file - "eev-beginner.el" - seems to provide a good solution.
;; The finicky old-timers just need to use one of the "expert setups":
;;
;;   (find-eev-install-intro "2. The expert setup")
;;   (find-eev "eev-load.el")




;; «load-path-hack»  (to ".load-path-hack")
;;
;; This is a hack to make eev easier to use by beginners that don't
;; understand the load-path yet. If they run something that runs:
;;
;;   (load "/path/to/eev-beginner.el")
;;
;; this hack puts the "/path/to/" in the load-path, and if they try
;; to load this file with
;;
;;   (eval-buffer)
;;
;; this hack puts the default-directory in the load-path.
;;
;; See: (find-elnode "How Programs Do Loading")
;;      (find-enode "Variable Index" "* load-path:")
;;      (find-elnode         "Index" "* load-path:")
;;      (find-eppp                      load-path)
;;      (find-efunctiondescr 'load)
;;      (find-efunctiondescr 'load "load-in-progress")
;;      (find-efunctiondescr 'load "load-file-name")
;;      (find-evardescr            'load-file-name)
;;
(add-to-list 'load-path
	     (if load-in-progress
		 (file-name-directory load-file-name)
	       default-directory))


;; This require loads all the main modules of eev.
;; It doesn't load some advanced modules that require extra setup.
;; See the comments here: (find-eev "eev-load.el")
(require 'eev-load)

;; Turn eev-mode on.
;; See: (find-eev-intro "1. `eev-mode'")
(eev-mode 1)

;; Open the main tutorial.
(find-eev-quick-intro)

;; «eev-beginner»  (to ".eev-beginner")
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
