;;; eev2-all.el -- load all the modules of the rewrite of eev (-> 0.96).
;;; This can also be used as an index to the (new) source files.

;; Copyright (C) 2012,2019 Free Software Foundation, Inc.
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
;; Version:    2019mar02
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev2-all.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev2-all.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                                                (find-eev-quick-intro)

;;; Commentary:

;; At one point there was a file "eev-all.el" that loaded all the
;; modules of eev. Then (circa 2012) many modules were refactored, and
;; the old and the new modules coexisted for a while; the file
;; "eev2-all.el" (this one!) was used to load the common ones and the
;; new ones...
;;
;; Now (2019) this file is about to be deprecated, and to be replaced
;; by "eev-load.el". See:
;;   (find-eev "eev-load.el")



;; The main "killer feature" of eev: a way to control interactive programs.
;; See: (find-eepitch-intro)
;;      (find-wrap-intro)
(require 'eepitch)             ; (find-eev "eepitch.el")
(require 'eev-wrap)	       ; (find-eev "eev-wrap.el")

;; The other main killer feature: elisp hyperlinks.
;;
;; The main way to follow hyperlinks (`M-e') and to go back (`M-k')
;; is described in:
;;   (find-eval-intro)
(require 'eev-flash)	       ; (find-eev "eev-flash.el")
(require 'eev-eval)	       ; (find-eev "eev-eval.el")
(require 'eev-multiwindow)     ; (find-eev "eev-multiwindow.el")
(require 'eev-mode)	       ; (find-eev "eev-mode.el")
;;
;; The implementation of hyperlink functions
;; is described in:
;;   (find-links-intro)
(require 'eev-blinks)	       ; (find-eev "eev-blinks.el")
(require 'eev-plinks)	       ; (find-eev "eev-plinks.el")
(require 'eev-elinks)	       ; (find-eev "eev-elinks.el")
(require 'eev-tlinks)	       ; (find-eev "eev-tlinks.el")
;;
;; The ways to mass-produce hyperlink functions
;; are described in:
;;   (find-code-c-d-intro "\nShorter hyperlinks\n")
(require 'eev-code)	       ; (find-eev "eev-code.el")
(require 'eev-env)	       ; (find-eev "eev-env.el")
(require 'eev-brxxx)	       ; (find-eev "eev-brxxx.el")
(require 'eev-pdflike)	       ; (find-eev "eev-pdflike.el")
(require 'eev-audiovideo)      ; (find-eev "eev-audiovideo.el")
(require 'eev-codings)	       ; (find-eev "eev-codings.el")
(require 'eev-anchors)	       ; (find-eev "eev-anchors.el")

;; User stuff.
;;   (find-eejump-intro)
(require 'eev-intro)	       ; (find-eev "eev-intro.el")
(require 'eev-edit)            ; (find-eev "eev-edit.el")
(require 'eejump)              ; (find-eev "eejump.el")
(require 'eev-rcirc)           ; (find-eev "eev-rcirc.el")

;; Advanced (and hard to use) features that may require creating a
;; temporary directory, patching rcfiles, and installing Expect
;; scripts. They have not been completely ported to eev2 yet! See:
;;   (find-prepared-intro)
;;   (find-bounded-intro)
;;   (find-channels-intro)
(require 'eev-prepared)		; (find-eev "eev-prepared.el")
(require 'eev-bounded)		; (find-eev "eev-bounded.el")
(require 'eev-channels)		; (find-eev "eev-channels.el")

(provide 'eev2-all)



;; Garbage-ish:
;;
;; I am keeping these links here just for reference - these files and
;; functions contain useful things that have not been copied to eev2
;; yet, not even in broken form. DO NOT UNCOMMENT!
;;
;; (require 'eev)                 ; (find-eev "eev.el")
;; (require 'eev-glyphs)          ; (find-eev "eev-glyphs.el")
;; (require 'eev-compose)         ; (find-eev "eev-compose.el")
;;       ; (find-eev "eev-glyphs.el" "eev-set-default-glyphs")
;;       ; (find-eev "eev.el"        "ee-setenv")
;; (require 'eev-steps)           ; (find-eev "eev-steps.el")
;; (require 'eev-langs)           ; (find-eev "eev-langs.el")
;; (require 'eev-mini-steps)      ; (find-eev "eev-mini-steps.el")
;; (require 'eechannel)           ; (find-eev "eechannel.el")





;; Local Variables:
;; coding:            raw-text-unix
;; no-byte-compile:   t
;; End:
