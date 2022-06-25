;;; eev-hydras.el --- eev functions that use hydra.el.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.
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
;; Version:    20220330
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-hydras.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-hydras.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-here-links-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-here-links-intro)

;;; Commentary:

;; This is a new, experimental feature that is not loaded by
;; default. It is mentioned briefly here:
;;
;;   (find-refining-intro "5. Pointing to anchors")
;;   (find-refining-intro "5. Pointing to anchors" "but I don't touch-type")
;;
;; It depends on the package "hydra" (from ELPA). You can install it
;; with `M-x list-packages', or with:
;;
;;   (find-epackage-links 'hydra "hydra" t)
;;
;; To learn how this experimental feature works, load this file and
;; run its tutorial - i.e., execute these two sexps:
;;
;;   (require 'eev-hydras)
;;   (find-eev-index-edit-intro)
;;
;; This code will probably change a LOT in the next months.
;; Update: I recorded a video, it's here:
;;
;;   http://angg.twu.net/eev-videos/2021-05-20_hydra_ei.mp4
;;  (find-eevvideo-links "hydraei" "2021-05-20_hydra_ei")



;; «.hydra-eev-index-edit»		(to "hydra-eev-index-edit")
;; «.find-eev-index-edit-intro»		(to "find-eev-index-edit-intro")



;; See: https://github.com/abo-abo/hydra
;;      (find-epackage-links 'hydra "hydra" t)
;;      (find-epackage       'hydra)
;;
(require 'hydra)



(defalias 'ei 'eev-index-edit)

(defun eev-index-edit ()
  "Call `eev-index-edit/body' to edit the index."
  (interactive)
  (hydra-eev-index-edit/body))

(defun eev-index-replace (from-string to-string)
  "Replace FROM-STRING to TO-STRING in the current line."
  (search-forward from-string (ee-eol))
  (replace-match to-string)
  (eek "C-a"))



;; «hydra-eev-index-edit»  (to ".hydra-eev-index-edit")
;; Try: (find-eapropos    "hydra-eev-index-edit")
;;      (find-ekeymapdescr hydra-eev-index-edit/keymap)
;;
(defhydra hydra-eev-index-edit (:color green :hint nil)
  "
_q_:quit    ^^^^                                    _0_: delthiswindow
 werty:  _u_:prev    _i_:insert  _o_:second window  _p_:prev
asdfgh:  _j_:next    _k_:kill    _l_:eval
         ^ ^         _,_:adj<-   _._:adj->
Standard usage: _k__o__p__l__i__0_q
Use `_,_'s and `_._'s between the `_i_' and the `_0_' to adjust the `(to ...)'.\n"
  ;;
  ;; Left column: movement by anchors
  ("u" (re-search-backward (ee-tag-re)))
  ("j" (re-search-forward  (ee-tag-re)))
  ;; Kill/Otherwindow/Prev/evaL/Insert/adj<-/adj->/delthiswindow:
  ("k" (eek "C-a C-SPC <down> C-w"))
  ("o" (eek "C-x 1 C-x 3 C-x o"))
  ("p" (re-search-backward (ee-tag-re)))
  ("l" (eek "M-e"))
  ("i" (eek "C-a <down> C-y <up>"))
  ("," (eev-index-replace "\t(to "   "(to "))
  ("." (eev-index-replace   "(to " "\t(to "))
  ("0" (eek "C-x 0"))
  ;; Other keys:
  ("<down>" (eek "<down>"))
  ("<up>"   (eek "<up>"))
  ("1"      (eek "C-x 1"))
  ("2"      (eek "M-2 M-e"))
  ("<"      (eek "M-<"))
  ("M-<"    (eek "M-<"))
  ("q"      nil))





;; «find-eev-index-edit-intro»  (to ".find-eev-index-edit-intro")
;; Skel: (find-intro-links "eev-index-edit")

(defun find-eev-index-edit-intro (&rest pos-spec-list) (interactive)
  (let ((ee-buffer-name "*(find-eev-index-edit-intro)*"))
    (apply 'find-eintro "\
\(Re)generate: (find-eev-index-edit-intro)
Source code:  (find-efunction 'find-eev-index-edit-intro)
More intros:  (find-eev-quick-intro)
              (find-eev-intro)
              (find-eepitch-intro)
This buffer is _temporary_ and _editable_.
It is meant as both a tutorial and a sandbox.



This \"intro\" is a sandboxed tutorial for:

  (find-eev \"eev-hydras.el\")
  (find-refining-intro \"5. Pointing to anchors\")
  (find-refining-intro \"5. Pointing to anchors\" \"but I don't touch-type\")

In these sections about anchors in the main tutorial - see:

  (find-eev-quick-intro \"8.3. Creating index/section anchor pairs\")
  (find-eev-quick-intro \"8.4. Creating e-script blocks\")

I said that I organize the indexes of my e-script files - like:

  (find-wget \"http://angg.twu.net/e/youtube.e\")
              http://angg.twu.net/e/youtube.e.html

by hand. This was true until april 2021, when I wrote a hydra for
that and started to play with it.

Remember that many functions in eev create temporary buffers that
have many lines, or blocks of lines, that can be used to perform
different actions; they act as visual interfaces in which the
actions are spread vertically, with at most one action per line.
Hydra.el lets us create interfaces in which each action is bound
to a key, and the minibuffer/echo area is used to display a
reminder of what each key does.

This is an index, followed by two e-script blocks, and by a
\"plic\" and a \"bletch\" that you will use to create new
e-script blocks.



# «.foo»	(to \"foo\")
# «.bar»	(to \"bar\")



#####
#
# foo
# 2021may20
#
#####

# «foo»  (to \".foo\")



#####
#
# bar
# 2021may20
#
#####

# «bar»  (to \".bar\")


plic
bletch




Here is the exercise.
1) Use `M-B' to convert the line with \"plic\" above into an
   e-script block, as explained here:

     (find-eev-quick-intro \"8.4. Creating e-script blocks\")

2) Put the point on the line that should be moved to the index -
   the one whose anchor is <.plic> with double angle brackets -
   and type:

     C-l M-x ei RET kopli0q

   The actions associated to \"kopli0q\" are:

     (k)ill the current line
     switch to the sec(o)nd window
     move backwards to the line of the (p)revious anchor
     eva(l) the current line - that has a `to' pointing to the index
     (i)nsert, i.e., yank, the last kill after this line
     (0): run `C-x 0' to delete the window that shows the index
     (q)uit the `hydra-eev-index-edit' mode

3) Do the same for the \"bletch\".

4) Take a look at the source code and figure out how to use the
   keys `1', ',', '.', <up>, and <down> in `hydra-eev-index-edit'
   mode.

" pos-spec-list)))

;; (find-eev-index-edit-intro)




(provide 'eev-hydras)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
