;;; eev-anchors.el -- hyperlinks to anchors.  -*- lexical-binding: nil; -*-

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
;; Latest version: <http://angg.twu.net/eev-current/eev-anchors.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-anchors.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-anchors-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-anchors-intro)

;;; Commentary:

;; See the tutorial on anchors here:
;;   (find-eev-quick-intro "8. Anchors")

;; «.glyphs»		(to "glyphs")
;; «.ee-goto-anchor»	(to "ee-goto-anchor")
;; «.find-anchor»	(to "find-anchor")
;; «.to»		(to "to")




(require 'eepitch)             ; (find-eev "eepitch.el")
(require 'eev-codings)         ; (find-eev "eev-codings.el")

		 


;;;        _             _         
;;;   __ _| |_   _ _ __ | |__  ___ 
;;;  / _` | | | | | '_ \| '_ \/ __|
;;; | (_| | | |_| | |_) | | | \__ \
;;;  \__, |_|\__, | .__/|_| |_|___/
;;;  |___/   |___/|_|              
;;
;; «glyphs»  (to ".glyphs")
;; See: (find-eev "eepitch.el" "glyphs")

(defface eev-glyph-face-green
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "forest green"))
    (t (:bold t)))
  "Face used for the green glyphs (`<<' and `>>', chars 171 and 187).")

;; (eepitch-set-glyph ?« ?« 'eev-glyph-face-green)
;; (eepitch-set-glyph ?» ?» 'eev-glyph-face-green)

(eepitch-set-glyph 171 171 'eev-glyph-face-green)
(eepitch-set-glyph 187 187 'eev-glyph-face-green)




;;;                   _                      __                            _   
;;;   __ _ _ __   ___| |__   ___  _ __      / _| ___  _ __ _ __ ___   __ _| |_ 
;;;  / _` | '_ \ / __| '_ \ / _ \| '__|____| |_ / _ \| '__| '_ ` _ \ / _` | __|
;;; | (_| | | | | (__| | | | (_) | | |_____|  _| (_) | |  | | | | | | (_| | |_ 
;;;  \__,_|_| |_|\___|_| |_|\___/|_|       |_|  \___/|_|  |_| |_| |_|\__,_|\__|
;;;                                                                            

(defvar ee-anchor-format "«%s»" "See `ee-goto-anchor'.")
;;;###autoload
(put   'ee-anchor-format 'safe-local-variable #'stringp)

;; A paranoid setting would be:
;; (defvar ee-anchor-format nil "See `ee-goto-anchor'.")

(defun ee-format-as-anchor (tag)
  "Convert TAG into an anchor using `ee-anchor-format'."
  (if ee-anchor-format
      (format (ee-tolatin1 ee-anchor-format) tag)
    (error "`ee-anchor-format' is nil - can't convert string to anchor")))




;;;   __ _           _                        _                
;;;  / _(_)_ __   __| |       __ _ _ __   ___| |__   ___  _ __ 
;;; | |_| | '_ \ / _` |_____ / _` | '_ \ / __| '_ \ / _ \| '__|
;;; |  _| | | | | (_| |_____| (_| | | | | (__| | | | (_) | |   
;;; |_| |_|_| |_|\__,_|      \__,_|_| |_|\___|_| |_|\___/|_|   
;;;                                                            
;; «ee-goto-anchor»  (to ".ee-goto-anchor")
;; «find-anchor»     (to ".find-anchor")
;; See: (find-eev-quick-intro "8. Anchors")
;;      (find-eval-intro "Anchors and pages")

(defun ee-goto-anchor (&optional tag &rest rest)
  "Like `ee-goto-position', but TAG is converted to an anchor.
If the anchor obtained from TAG is not found then issue an error
but do not move point.
For example, if `ee-anchor-format' is \"<<%s>>\" then

  (ee-goto-anchor \"foo\" \"bar\")

searches for the first occurrence of \"<<foo>>\" in the current
buffer, then for the first occurrence of \"bar\" after that. If
\"<<foo>>\" is not found then do not move point.

It is good style to set `ee-anchor-format' globally to nil and
only use anchors in files where `ee-anchor-format' is declared in
the local variables section of the file; see:

  (find-node \"(emacs)File Variables\")
  (find-node \"(emacs)Specifying File Variables\")

a hint: one way of forcing reloading the local variables by hand
is by running `\\[normal-mode]'. 

The glyphs defined in (find-eev \"eev-anchors.el\" \"glyphs\")
can be used to make anchors using characters that stand out."
  (if tag (goto-char
	   (save-excursion
	     (goto-char (point-min))
	     (search-forward (ee-format-as-anchor tag))
	     (point))))
  (ee-goto-rest rest))

(defun find-anchor (fname &optional tag &rest pos-spec-list)
  "Like `find-fline', but TAG is converted to an anchor if not nil.
See `ee-goto-anchor'."
  (find-fline fname)
  (apply 'ee-goto-anchor tag pos-spec-list))



;;;  _        
;;; | |_ ___  
;;; | __/ _ \ 
;;; | || (_) |
;;;  \__\___/ 
;;;           
;; «to»  (to ".to")
;; See: (find-eev-quick-intro "8.1. Introduction: `to'")
;;      (find-eev-intro "1. `eev-mode'" "prefixes" "to")
;;
(defun to (tag &rest pos-spec-list)
  "Like `find-anchor', but does not switch to another buffer or file."
  (interactive "sAnchor: ")
  (apply 'ee-goto-anchor tag pos-spec-list))




(provide 'eev-anchors)







;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
