;;; eev-beginner.el -- load eev, turn eev-mode on, open a tutorial.

;; Copyright (C) 2019 Free Software Foundation, Inc.
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
;; Version:    2019apr06
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-beginner.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                                                (find-eev-quick-intro)

;;; Commentary:

;; This file serves two purposes.
;;
;; 1) Most beginners start playing with Emacs+eev by following the
;;    instructions here,
;;
;;      (find-eev-quick-intro "1. Installing eev")
;;
;;    i.e., by copying and pasting a certain script to a terminal.
;;    That script creates an executable file called "~/eev" whose
;;    contents are something like this:
;;
;;      #!/bin/sh
;;      cd ~/eev2/ && emacs -l eev-beginner.el --eval="(find-eev-quick-intro)" $*
;;
;;    Running "~/eev" on a shell starts Emacs and makes it load this
;;    file and load the main tutorial - i.e., (find-eev-quick-intro).
;;
;; 2) If you install eev as an emacs package via `M-x list-packages'
;;    then no eev modules will be loaded UNTIL you run `M-x
;;    eev-beginner', which loads everything, starts eev-mode, and
;;    opens the main tutorial.
;;
;; When, or if, you are no longer a beginner, you may want to load eev
;; by just adding a "(require 'eev-load)" to your ".emacs". This loads
;; all the main modules but not turn on eev-mode on by default, and
;; does not open the tutorial. See:
;;
;;    (find-eev-intro "1. `eev-mode'")
;;    (find-eev "eev-load.el")


;; NOTE: older versions of eev loaded "eev-readme.el" instead of
;; "eev-beginner.el". See:
;;
;;   (find-eev "eev-readme.el")



;; 2019mar13: Commented this out.
;; 2019mar29: Uncommented - it seems that emacs25 needs this.
(add-to-list 'load-path default-directory)



;; Load all the main modules of eev.
;; Do not load some advanced modules that require extra setup.
;; See the comments here: (find-eev "eev-load.el")
(require 'eev-load)
(eev-mode 1)
(find-eev-quick-intro)


;; This is the only autoload of eev at this moment.
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
