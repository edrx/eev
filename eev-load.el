;;; eev-load.el -- load all the main modules of eev.  -*- lexical-binding: nil; -*-
;;; This can also be used as an index to the main source files.

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.
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
;; Version:    20220827
;; Keywords:   e-scripts
;;
;; Supersedes: (find-eev "eev-all.el")
;;             (find-eev "eev2-all.el")
;;             (find-eev "eev-readme.el")
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-load.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-load.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                                                (find-eev-quick-intro)

;;; Commentary:

;; This file loads all the main modules of eev, and people who want to
;; read the source can use this as an index to them. The modules are
;; loaded more or less in the order that they are mentioned here:
;;
;;   (find-eev-quick-intro)
;;
;; Some of them are `require'd twice for clarity.



;; «autoloads»
;;
;; A NOTE ABOUT AUTOLOADS
;; ======================
;; Almost all functions in eev can be used as elisp hyperlinks, even
;; with `eev-mode' turned off; if eev-mode is off you just have to
;; type `C-e C-x C-e' to execute them instead of `M-e'. For example, 
;; most function definitions in
;;
;;   (find-eev "eev-blinks.el")
;;
;; are preceded by a test in comments, and the right way to set up
;; autoloads in eev would be one in which each one of these tests
;; could be executed from a state in which only the autoloads file had
;; been loaded...
;;
;; The standard way to do that would be to precede *each one* of these
;; definitions with an autoload magic comment - which would mean more
;; than a hundred autoloads. I find that ugly, so I decided to do
;; something different:
;;
;;   1) to load all the main modules run this:
;;
;;       (require 'eev-load)
;;
;;   2) to load all the modules and turn eev-mode on, run:
;;
;;       (require 'eev-load)
;;       (eev-mode 1)
;;
;; Note that the instructions above are for people who know how to set
;; up their load-path correctly; if you are not yet one of these
;; people, then you should start by:
;;
;;   (find-eev "eev-beginner.el")



;; Load all the main modules of eev:
;;
;; The "eev quick intro" itself, and many other intros:
(require 'eev-intro)	       ; (find-eev "eev-intro.el")
;;
;; Following elisp hyperlinks (`M-e') and going back (`M-k'):
;;   (find-eev-quick-intro "2. Evaluating Lisp" "When you type `M-e'")
;;   (find-eev-quick-intro "3. Elisp hyperlinks" "go back" "`M-k'")
(require 'eev-flash)	       ; (find-eev "eev-flash.el")
(require 'eev-multiwindow)     ; (find-eev "eev-multiwindow.el")
(require 'eev-eval)	       ; (find-eev "eev-eval.el")
(require 'eev-mode)	       ; (find-eev "eev-mode.el")
(require 'eev-anchors)	       ; (find-eev "eev-anchors.el")
(require 'eev-template0)       ; (find-eev "eev-template0.el")
;;
;; Support for many kinds of hyperlink functions. See:
;;   (find-eev-quick-intro "3. Elisp hyperlinks")
;;   (find-links-conv-intro "3. Classification")
(require 'eev-blinks)	       ; (find-eev "eev-blinks.el")
(require 'eev-plinks)	       ; (find-eev "eev-plinks.el")
(require 'eev-elinks)	       ; (find-eev "eev-elinks.el")
(require 'eev-tlinks)	       ; (find-eev "eev-tlinks.el")
(require 'eev-hlinks)	       ; (find-eev "eev-hlinks.el")
;;
;; The `brxxx' functions. See:
;;   (find-eev-quick-intro "3.1. Non-elisp hyperlinks")
(require 'eev-brxxx)	       ; (find-eev "eev-brxxx.el")
;;
;; The main "killer feature" of eev: a way to control interactive programs.
;;   (find-eev-quick-intro "6. Controlling shell-like programs")
;;   (find-eev-quick-intro "6.1. The main key: <F8>")
;;   (find-eev-quick-intro "6.3. Creating eepitch blocks: `M-T'")
;;   (find-eepitch-intro)
;;   (find-wrap-intro)
(require 'eepitch)             ; (find-eev "eepitch.el")
(require 'eev-wrap)	       ; (find-eev "eev-wrap.el")
;;
;; Eejump (`M-j'):
;;   (find-eev-quick-intro "7. Quick access to one-liners")
;;   (find-eev-quick-intro "7.1. `eejump'")
(require 'eejump)              ; (find-eev "eejump.el")
;;
;; Anchors.
;;   (find-eev-quick-intro "8. Anchors")
;;   (find-eev-quick-intro "8.1. Introduction: `to'")
(require 'eev-anchors)	       ; (find-eev "eev-anchors.el")
;;
;; Shorter hyperlinks and how to mass-produce them:
;;   (find-eev-quick-intro "9. Shorter hyperlinks")
;;   (find-code-c-d-intro  "2. Shorter hyperlinks")
;;   (find-eev-quick-intro "9.3. Hyperlinks to PDF files")
(require 'eev-code)	       ; (find-eev "eev-code.el")
(require 'eev-pdflike)	       ; (find-eev "eev-pdflike.el")
;;
;; Some technical things.
(require 'eev-codings)	       ; (find-eev "eev-codings.el")
(require 'eev-env)	       ; (find-eev "eev-env.el")
(require 'eev-edit)            ; (find-eev "eev-edit.el")
(require 'eev-testblocks)      ; (find-eev "eev-testblocks.el")
(require 'eev-kla)             ; (find-eev "eev-kla.el")
(require 'eev-helpful)         ; (find-eev "eev-helpful.el")
(require 'eev-rstdoc)          ; (find-eev "eev-rstdoc.el")
;;
;; Configuration on M$ Windows.
(require 'eev-wconfig)         ; (find-eev "eev-wconfig.el")
;;
;; Hyperlinks to audio, video, and IRC:
;;   (find-video-links-intro)
;;   (find-audiovideo-intro)
;;   (find-rcirc-intro)
(require 'eev-audiovideo)      ; (find-eev "eev-audiovideo.el")
(require 'eev-videolinks)      ; (find-eev "eev-videolinks.el")
(require 'eev-rcirc)           ; (find-eev "eev-rcirc.el")
;;
;; Advanced (and hard to use) features that may require creating a
;; temporary directory, patching rcfiles, and installing Expect
;; scripts... see:
;;   (find-prepared-intro)
;;   (find-bounded-intro)
;;   (find-channels-intro)
' (require 'eev-prepared)	; (find-eev "eev-prepared.el")
' (require 'eev-bounded)	; (find-eev "eev-bounded.el")
' (require 'eev-channels)	; (find-eev "eev-channels.el")
;; IMPORTANT: Since 2019mar05 these files are no longer loaded by
;; default! Note the "'" at the beginning of each line!


;; Make `M-x eev-beginner' work in the "expert setups" too.
;; See: (find-efunctiondescr 'autoload "If FUNCTION is already defined")
(autoload 'eev-beginner "eev-beginner" 
  "Load all basic modules of eev, turn eev-mode on, and open a tutorial."
  'interactive)                 ; (find-eev "eev-beginner.el")



(provide 'eev-load)



;; Old comments taken from eev-readme.el.
;; Most of them point to old files that are no longer in eev - but I
;; have them in my machine, and they may have some comments that I may
;; want to cannibalize.
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
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
