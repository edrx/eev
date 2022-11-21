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
;;   (find-edit-index-intro)
;;
;; This code will probably change a LOT in the next months.
;; Update: I recorded a video, it's here:
;;
;;   http://angg.twu.net/eev-videos/2021-05-20_hydra_ei.mp4
;;  (find-eevvideo-links "hydraei" "2021-05-20_hydra_ei")



;; «.ei»				(to "ei")
;; «.ee-edit-index-hydra»		(to "ee-edit-index-hydra")

;; See: https://github.com/abo-abo/hydra
;;      (find-epackage-links 'hydra "hydra" t)
;;      (find-epackage       'hydra)
;;
(require 'hydra)

(defun ee-edit-index-replace (from-string to-string)
  "Replace FROM-STRING to TO-STRING in the current line."
  (eek "C-a")
  (search-forward from-string (ee-eol))
  (replace-match to-string)
  (eek "C-a"))



;;;  __  __                  _ 
;;; |  \/  |    __  __   ___(_)
;;; | |\/| |____\ \/ /  / _ \ |
;;; | |  | |_____>  <  |  __/ |
;;; |_|  |_|    /_/\_\  \___|_|
;;;                            
;; «ei»  (to ".ei")
;; This file is not loaded by default, but if you run
;;   (require 'eev-hydras)
;; this will define `M-x ei' as an alias for `M-x ee-edit-index'.
(defalias 'ei 'ee-edit-index)

(defun ee-edit-index ()
  "Call `ee-edit-index-hydra/body' to edit the index."
  (interactive)
  (ee-edit-index-hydra/body))



;;;  _   _           _           
;;; | | | |_   _  __| |_ __ __ _ 
;;; | |_| | | | |/ _` | '__/ _` |
;;; |  _  | |_| | (_| | | | (_| |
;;; |_| |_|\__, |\__,_|_|  \__,_|
;;;        |___/                 
;;
;; «ee-edit-index-hydra»  (to ".ee-edit-index-hydra")
;; Running a `defhydra' defines many functions.
;; You can inspect them with:
;;   (find-eapropos       "ee-edit-index-hydra")
;;   (find-ekeymapdescr    ee-edit-index-hydra/keymap)
;;   (find-efunctiondescr 'ee-edit-index-hydra/body)
;;   (find-efunctionpp    'ee-edit-index-hydra/body)
;;
(defhydra ee-edit-index-hydra (:color green :hint nil)
  "
_q_:quit    ^^^^                                    _0_: delthiswindow
 werty:  _u_:prev    _i_:insert  _o_:second window  _p_:prev
asdfgh:  _j_:next    _k_:kill    _l_:eval
         ^ ^         _,_:adj<-   _._:adj->
Standard usage: _k__o__p__l__i__0_q
Use `_,_'s and `_._'s between the `_i_' and the `_0_' to adjust the `(to ...)'.\n"
  ;;
  ;; <k>ill, <o>ther window, <p>rev anchor, eva<l>, <i>nsert, C-x <0>:
  ("k" (eek "C-a C-SPC <down> C-w"))
  ("o" (eek "C-x 1 C-x 3 C-x o"))
  ("p" (re-search-backward (ee-tag-re)))
  ("l" (eek "M-e"))
  ("i" (eek "C-a <down> C-y <up>"))
  ("0" (eek "C-x 0"))
  ("q" nil)
  ;;
  ;; Reindent:
  ("<down>" (eek "<down>"))
  ("<up>"   (eek "<up>"))
  (","   (ee-edit-index-replace "\t(to "   "(to "))
  ("."   (ee-edit-index-replace   "(to " "\t(to "))
  ;;
  ;; Next anchor and previous anchor:
  ("n" (re-search-forward  (ee-tag-re)))
  ("u" (re-search-backward (ee-tag-re)))
  ("j" (re-search-forward  (ee-tag-re)))
  ;;
  ;; Other keys:
  ("<"      (eek "M-<"))
  ("M-<"    (eek "M-<"))
  ("1"      (eek "C-x 1"))
  ("2"      (eek "M-2 M-e"))
  )







(provide 'eev-hydras)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
