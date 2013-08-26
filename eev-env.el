;;; eev-env.el -- set some environment variables.

;; Copyright (C) 2012 Free Software Foundation, Inc.
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
;; Version:    2012nov02
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-env.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-env.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-psne-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-prepared-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-psne-intro)
;;                                                (find-prepared-intro)

;;; Commentary:

;; Used by: (find-eev "eev-prepared.el")
;;
;; Related but obsolete files:
;;   <http://angg.twu.net/eev-current/README.html>
;;   <http://angg.twu.net/eev-current/eev-langs.el.html>
;;   <http://angg.twu.net/eev-current/eev-rctool.html>





;;;   ___ _ ____   __  __   ____ _ _ __ ___ 
;;;  / _ \ '_ \ \ / /  \ \ / / _` | '__/ __|
;;; |  __/ | | \ V /    \ V / (_| | |  \__ \
;;;  \___|_| |_|\_/      \_/ \__,_|_|  |___/
;;;
;;; Set some environment variables (for ee-expand, getenv,
;;; shell buffers, xterms started from Emacs, etc).

;; (find-eevrcfile ".bashrc")
;; (find-eevrcfile ".zshrc")

(defun ee-setenv (envvar value)
  "In case the environment variable ENVVAR was not set set it to VALUE."
  (if (null (getenv envvar))
      (setenv envvar (ee-expand value))))

(ee-setenv "S" "~/snarf")	; for `find-psne-links'

;; Obsolete? See:
;; (find-eev "eev-bounded.el")
;; (find-eev "eev.el" "ee-setenv")
(ee-setenv "EEVDIR"
	   (let ((fname (locate-library "eev")))
	     (if fname (directory-file-name (file-name-directory fname))
	       "~/eev-current")))	; eev.el, etc

(provide 'eev-env)






;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
