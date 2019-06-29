;; eev-on-windows.el - some support for M$ Windows.

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
;; Version:    2019jun27
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-on-windows.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-on-windows.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:
;;
;; Experimental, undocumented, and messy. This is being used in a
;; course on LaTeX and Emacs in which the students are trying to run
;; everything on Windows and I am trying to help them even though I
;; know next to nothing about Windows.

;; «.eev-tar»		(to "eev-tar")
;; «.add-to-PATH»	(to "add-to-PATH")

;; (find-eev "eev-plinks.el" "find-urlretrieve")
;; (find-es "emacs" "package-untar")
;; (find-es "emacs" "load-path")
;; (find-angg ".emacs.local.w32")
;; (find-angg ".emacs.local.w32" "PATH")





;; «eev-tar»  (to ".eev-tar")
;; Use this - with `M-e' in each line - to download a very recent
;; version of eev using only Lisp.
;;
;;   (setq  eev-tar-dir   "~/eev-tar/")
;;   (setq  eev-tar-fname "~/eev-tar/eev2.tar")
;;   (setq  eev-tar-url   "http://angg.twu.net/eev-current/eev2.tar")
;;   (mkdir eev-tar-dir   t)
;;   (setq  eev-tar-contents nil)
;;   (setq  eev-tar-contents (find-urlretrieve0 eev-tar-url))
;;   (length (setq eev-tar-contents (find-urlretrieve0 eev-tar-url)))
;;   (write-region eev-tar-contents nil eev-tar-fname)
;;   
;;   (find-2a nil '(find-fline eev-tar-fname 1 '(tar-untar-buffer)))
;;   (eek "C-x o C-x 4 0")
;;   (find-2a nil '(find-fline eev-tar-dir nil '(eek "g")))
;;

;; Add something like this to your .emacs:
;;
;;   (add-to-list 'load-path "~/eev-tar/")
;;
;; Use these sexps to check if everything is alright:
;;
;;   (find-epp load-path)
;;   (find-estring (mapconcat 'identity load-path "\n"))
;;   (locate-library "eejump")
;;   (find-estring (list-load-path-shadows t))




;; «add-to-PATH»  (to ".add-to-PATH")

;; (setq mylist '(22 33 44))
;; (add-to-list 'mylist 44)
;;
;; (ee-dospath-add "A;B;C" "B")
;; (ee-dospath-add "A;B;C" "c:/B")
;;
;; (let* ((a 2) (a (* 10 a)) (a (+ 3 a))) a)
;;
;; (find-elnode "Index" "* delete:")

(defun ee-dospath-to-unix (str)
  (replace-regexp-in-string "\\\\" "/" str))
(defun ee-dospath-to-dos (str)
  (replace-regexp-in-string "/" "\\\\" str))
(defun ee-dospath-split (str)
  (split-string str ";"))
(defun ee-dospath-unsplit (list)
  (mapconcat 'identity list ";"))

(defun ee-dospath-add (path dir)
  (setq dir  (ee-dospath-to-dos dir))
  (setq path (ee-dospath-to-dos path))
  (let* ((list (ee-dospath-split path))
	 (newlist (cons dir (delete dir list))))
    (ee-dospath-unsplit newlist)))

(defun add-to-PATH (dir)
  (setenv "PATH" (ee-dospath-add (getenv "PATH") dir)))





(provide 'eev-on-windows)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
