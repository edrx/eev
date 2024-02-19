;;; eev-aliases.el -- aliases that don't start with the standard prefixes. -*- lexical-binding: nil; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
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
;; Version:    20240219
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-aliases.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-aliases.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-kla-intro.html>
;;                                               (find-kla-intro)

;;; Comment:

;;

;; «.1stclassvideos»	(to "1stclassvideos")



;; «1stclassvideos»  (to ".1stclassvideos")
;; From: (find-eev "eev-tlinks.el" "1c")
(defalias '1c  'find-1stclassvideos)
(defalias '1cl 'find-1stclassvideo-links)

;; From: (find-eev "eev-kl-here.el" "aliases")

;; From: (find-eev "eev-kla.el" "aliases")

;; From: (find-templates-intro "4. Adding meat")





(provide 'eev-aliases)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
