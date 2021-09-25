;; eev-readme.el -- load all modules of eev and activate eev-mode.  -*- lexical-binding: nil; -*-

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
;; Version:    20190302
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-readme.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-update-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-eev-update-links)

;;; Commentary:

;; See this for the current standard ways to install eev:
;;   
;;   (find-eev-quick-intro "1. Installing eev")
;;   (find-eev-install-intro)
;;
;; The rest of this file is very old.
;; This file has been superseded by:
;;
;;   (find-eev "eev-beginner.el")

;; Quick instructions:
;;
;; 1) Download http://angg.twu.net/eev-current/eev2.tgz
;; 2) Unpack it somewhere - for example, in "/tmp/eev/".
;; 3) One of the files in eev2.tgz is called "eev-readme.el" (a.k.a.
;;    "this file"). Open it with Emacs. Suggestion: invoke Emacs with
;;    "emacs -fg bisque -bg black eev-readme.el", to get good colors.
;;    So, 1-3 can be:
;;
;;      mkdir /tmp/eev/
;;      cd    /tmp/eev/
;;      wget http://angg.twu.net/eev-current/eev2.tgz
;;      tar -xvzf eev2.tgz
;;      emacs -fg bisque -bg black eev-readme.el
;;
;; 4) Execute the multi-line "(progn ...)" block below. To do that,
;;    put the cursor after the ')' that is on a line by itself and
;;    type `C-x C-e' (`eval-last-sexp').
;; 5) Or, instead of executing the "(progn ...)" block below by hand
;;    with `C-x C-e', you can use a command-line argument to make
;;    Emacs execute ("load") this whole file:
;;
;;      emacs -fg bisque -bg black -l eev-readme.el
;;
;; 6) Now you should have eev-mode activated - and the mode line
;;    for this buffer should show an "eev", like this: 
;;       _______________________________________________________
;;      |                                                       |
;;      | (...)                                                 |
;;      |                                                       |
;;      |-:---  eev-readme.el   16% L23    (Emacs-Lisp eev)-----|
;;      |_______________________________________________________|
;;
;;    this means that the eev-mode keybindings are available.
;;
;;    The most important key is `M-e', which operates on the "sexp at
;;    eol", the sexp whose last `)' is at the end of the current line.
;;    A plain `M-e' executes the sexp at eol, and is roughly
;;    equivalent to `C-e C-x C-e' (where `C-e' moves to eol).
;;    If you type `M-0 M-e' (which we will abbreviate as `M-0e'),
;;    this moves to eol and highlights the sexp at eol instead of
;;    evaluating it.
;;
;; 7) You can now type:
;;     `M-0e' to highlight the sexp at eol - try: (eek "M-0 M-e")
;;      `M-e' to execute the sexp at eol      (-> follow a hyperlink),
;;      `M-k' to kill the current buffer      (-> go back),
;;      `M-j' to go to the list of predefined targets for `M-j',
;;     `M-5j' to go to the list of "intros",      (find-eev-intro)
;;    `M-50j' to come back to this readme, i.e.,  (find-eev "eev-readme.el")
;;    `M-59j' to visit this:                      (find-eev-update-links)
;;            that contains scripts for installing and updating eev2.
;;    If you are an Emacs newbie, then this may be interesting too:
;;     `M-2j' visits the list of basic keys in:   (find-emacs-intro)
;;
(progn
  (add-to-list 'load-path default-directory)
  (require 'eev2-all)
  (eev-mode 1)
  )

;; Local Variables:
;; no-byte-compile: t
;; End:
