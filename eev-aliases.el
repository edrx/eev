;;; eev-aliases.el -- aliases that don't start with the standard prefixes. -*- lexical-binding: nil; -*-

;; Copyright (C) 2024,2025 Free Software Foundation, Inc.
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
;; Version:    20250127
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-aliases.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-aliases.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-kla-intro.html>
;;                                               (find-kla-intro)

;;; Comment:
;;
;; The main tutorial of eev explains how to create "commands with
;; very short names" and "commands with very short numbers":
;;
;;   (find-eev-quick-intro "7.4. Commands with very short names")
;;   (find-eev-quick-intro "7.4. Commands with very short names" "short numbers")
;;
;; You can see the ones that are currently defined with:
;;
;;   (find-eejumps)
;;   (find-eeshortdefs)
;;   (find-eeshortaliases)
;;
;; The "commands with very short names" defined with `defun' are
;; displayed with `(find-eeshortdefs)', and the ones that are defined
;; with `defalias' are displayed by `(find-eeshortdefs)'.
;;
;; If you just "load" eev, in this sense,
;;
;;   (find-eev-levels-intro "2. Loading")
;;
;; then it will only define functions and variables that start with
;; the prefixes "find-", "ee", "code-" or "br", with one exception:
;; "to". This is explained here,
;;
;;   (find-eev-levels-intro "4. Aliases")
;;   (find-eev-intro "1. `eev-mode'")
;;   (find-eev-intro "1. `eev-mode'" "prefixes")
;;
;; but most people will load this file, that defines several short
;; aliases that don't start with those prefixes. See:
;;
;;   (find-dot-emacs-links "eev")

;; Index:

;; «.1stclassvideos»		(to "1stclassvideos")
;; «.kl-here»			(to "kl-here")
;; «.kla»			(to "kla")
;; «.query-replace-list»	(to "query-replace-list")
;; «.edit-index»		(to "edit-index")
;; «.u0»			(to "u0")




;; «1stclassvideos»  (to ".1stclassvideos")
;; From: (find-eev "eev-tlinks.el" "aliases")
;;  See: (find-video-links-intro "9. First-class videos")
(defalias '1c  'find-1stclassvideos)
(defalias '1cl 'find-1stclassvideo-links)

;; «kl-here»  (to ".kl-here")
;; From: (find-eev "eev-kl-here.el" "aliases")
;;  See: (find-kl-here-intro)
(defalias 'kl    'eekl)
(defalias 'kll	 'eekll)
(defalias 'kls	 'eekls)

;; «kla»  (to ".kla")
;; From: (find-eev "eev-kla.el" "aliases")
;;  See: (find-kla-intro)
(defalias 'kla   'eekla)
(defalias 'kla0  'eekla0)
(defalias 'klas  'eeklas)
(defalias 'klf   'eeklf)
(defalias 'klfs  'eeklfs)
(defalias 'klt   'eeklt)
(defalias 'klts  'eeklts)
(defalias 'kli   'ee-kl-insert)
(defalias 'kla2  'eekla2)

;; «query-replace-list»  (to ".query-replace-list")
;; From: (find-eev "eev-qrl.el" "aliases")
;;  See: (find-templates-intro "4. Adding meat")
(defalias 'qrl0 'ee-qrl0)
(defalias 'qrl  'ee-qrl)

;; «edit-index»  (to ".edit-index")
;; From: (find-eev "eev-hydras.el" "aliases")
;;  See: (find-edit-index-intro)
(defalias 'ei 'ee-edit-index)

;; «u0»  (to ".u0")
;; From: (find-eev "eev-tlinks.el" "ee-0x0-upload-region")
(defalias 'u0 'ee-0x0-upload-region)


;; TODO: fix this!
;; (find-eev "eev-tlinks.el" "show2")
;; (find-eev "eev-tlinks.el" "show2-use")
;; (find-eloadhistory-for 'show2 2 " show2)")






(provide 'eev-aliases)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
