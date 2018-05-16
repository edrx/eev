;;; eev-codings.el -- tricks to support both the UTF8 coding system and unibyte

;; Copyright (C) 2018 Free Software Foundation, Inc.
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
;; Version:    2018mai16
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
;; trick unneccessary - `ee-format-as-anchor' now uses `ee-tolatin1'
;; to produce a search string that works both unibyte, on UTF-8, on
;; latin-1 files and some (most?) other encodings.



;;;                  _        _       _   _       _ 
;;;   ___  ___      | |_ ___ | | __ _| |_(_)_ __ / |
;;;  / _ \/ _ \_____| __/ _ \| |/ _` | __| | '_ \| |
;;; |  __/  __/_____| || (_) | | (_| | |_| | | | | |
;;;  \___|\___|      \__\___/|_|\__,_|\__|_|_| |_|_|
;;;                                                 
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



(provide 'eev-codings)



;; Local Variables:
;; coding: utf-8-unix
;; End:
