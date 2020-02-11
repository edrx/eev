;;; eev-codings.el -- tricks to support both the UTF8 coding system and unibyte

;; Copyright (C) 2018-2019 Free Software Foundation, Inc.
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
;; Version:    2019mar04
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-coding.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-coding.el.html>
;;       See also: <http://angg.twu.net/eev-intros/find-eev-intro.html>

;;; Commentary:

;; Until mid-2017 a user who wanted to use anchors (like "«tag»") in
;; both unibyte and multibyte buffers had to put something like this
;;
;;   ee-anchor-format:  "«%s»"
;;
;; in the local variables section at the end of (at least some) of his
;; files; the functions defined here make the local variables section
;; trick unnecessary - `ee-format-as-anchor' now uses `ee-tolatin1' to
;; produce a search string that works both unibyte, on UTF-8, on
;; latin-1 files and some (most of?) other encodings.
;;
;; NOTE: `ee-tolatin1' a hack! Conversion to latin-1 seems to work in
;; most cases, but I don't understand very well the reasons why... I
;; have some notes about all this in these e-script blocks in my notes
;; about Emacs:
;;
;;   (find-es "emacs" "unibyte-2019")
;;   (find-es "emacs" "unibyte-2019-search")
;;   (find-es "emacs" "creating-utf8-files")
;;   (find-es "emacs" "ee-re-to")
;;   http://angg.twu.net/e/emacs.e.html#unibyte-2019
;;   http://angg.twu.net/e/emacs.e.html#unibyte-2019-search
;;   http://angg.twu.net/e/emacs.e.html#creating-utf8-files
;;   http://angg.twu.net/e/emacs.e.html#ee-re-to
;;
;;
;; NOTE 2: Sorry for taking so long!! Here's what happened. This page
;;
;;   http://angg.twu.net/glyphs.html
;;
;; tells a bit about the hacked 256-char fonts that I created many
;; years before UTF-8 became standard, and that I used for ages in
;; some of my notes and .tex files... I wanted to maintain
;; compatibility with the files that used those fonts, and this turned
;; out to be very hard - these hacked fonts only worked in files and
;; buffers in which the encoding was "raw-text",
;;
;;   (find-elnode "Non-ASCII Characters")
;;   (find-elnode "Disabling Multibyte" "unibyte")
;;   (find-elnode "Disabling Multibyte" "raw-text")
;;
;; and before 2019 I had a *very* poor understanding of how Emacs
;; converts between unibyte and multibyte and between raw-text,
;; latin-1 and utf-8...



;; «.ee-tolatin1»	(to "ee-tolatin1")
;; «.ee-tolatin1-re»	(to "ee-tolatin1-re")




;;;                  _        _       _   _       _ 
;;;   ___  ___      | |_ ___ | | __ _| |_(_)_ __ / |
;;;  / _ \/ _ \_____| __/ _ \| |/ _` | __| | '_ \| |
;;; |  __/  __/_____| || (_) | | (_| | |_| | | | | |
;;;  \___|\___|      \__\___/|_|\__,_|\__|_|_| |_|_|
;;;                                                 
;; «ee-tolatin1» (to ".ee-tolatin1")
;; Original comment:
;;
;; 2017jul29: this is a low-level hack to allow anchors like "«tag»"
;; to work on both unibyte and multibyte buffers and files without
;; requiring the user to set the variable `ee-anchor-format' in the
;; local variables section.

(defun ee-to-coding (coding str)
  (ee-no-properties (decode-coding-string str coding)))

(defun ee-tolatin1 (str)
  "Make STR compatible with both unibyte and multibyte buffers.
Convert STR to a multibyte format that works in both
unibyte (raw-text) and multibyte (e.g., utf-8) buffers. This may
fail if STR contains chars that are not in the latin-1 range.
This function is used by `ee-format-as-anchor'."
  (ee-to-coding 'latin-1 str))




;;;                  _        _       _   _       _                
;;;   ___  ___      | |_ ___ | | __ _| |_(_)_ __ / |      _ __ ___ 
;;;  / _ \/ _ \_____| __/ _ \| |/ _` | __| | '_ \| |_____| '__/ _ \
;;; |  __/  __/_____| || (_) | | (_| | |_| | | | | |_____| | |  __/
;;;  \___|\___|      \__\___/|_|\__,_|\__|_|_| |_|_|     |_|  \___|
;;;                                                                
;; «ee-tolatin1-re» (to ".ee-tolatin1-re")
;; 2019feb24: this is a hack!
;; Test code: (find-es "emacs" "unibyte-2019-search")

(defun ee-tolatin1-re (re)
  "Make the regexp RE compatible with the current buffer.
This is similar to `ee-tolatin1', but for regexps that contain
the \"«»\"s used to delimit anchors. For example,

  (ee-tolatin1-re \"\\253\\([!-~]\\)\\273\")

should return a regexp for anchors that works in the current
buffer. This is a hack and a work in progress!!! See the code for
comments."
    (let ((bfcs buffer-file-coding-system))
      (cond ((eq bfcs 'iso-latin-1-unix) (ee-tolatin1 re))
            ((eq bfcs 'raw-text-unix)    re)
            ((eq bfcs 'utf-8-unix)       (ee-tolatin1 re))
            (t                           re))))



(provide 'eev-codings)



;; Local Variables:
;; coding: utf-8-unix
;; no-byte-compile: t
;; End:
