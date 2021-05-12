;;; eev-hydras.el --- eev functions that use hydra.el.

;; Copyright (C) 2021 Free Software Foundation, Inc.
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
;; Version:    20210511
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-hydras.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-hydras.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
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
;; It depends on the package "hydra".
;; I need to record a short video about this.
;; This code will probably change a LOT in the next months.


;; See: https://github.com/abo-abo/hydra
;;      (find-epackage-links 'hydra "hydra" t)
;;      (find-epackage       'hydra)
;;
(require 'hydra)



(defun eev-index-replace (from-string to-string)
  "Replace FROM-STRING to TO-STRING in the current line."
  (search-forward from-string (ee-eol))
  (replace-match to-string)
  (eek "C-a"))

(defun eev-index-edit ()
  "Call `eev-index-edit/body' to edit the index."
  (interactive)
  ;;
  ;; Definition of the hydra:
  ;;
  (defhydra hydra-eev-index-edit (:color green :hint nil)
  "
_q_:quit    ^^^^                                   _0_: delthiswindow
 werty:  _u_:prev    _i_:insert  _o_:other window  _p_:prev
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
  ;;
  ;; End of the definition of the hydra.
  ;;
  ;; Call the hydra:
  (hydra-eev-index-edit/body)
  )

(defalias 'ei 'eev-index-edit)


(provide 'eev-hydras)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
