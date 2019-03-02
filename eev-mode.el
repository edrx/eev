;;; eev-mode.el -- a minor mode with keybindings for using eev conveniently.

;; Copyright (C) 2012,2013,2019 Free Software Foundation, Inc.
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
;; Latest version: <http://angg.twu.net/eev-current/eev-mode.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-mode.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                                                (find-eev-quick-intro)

;;; Commentary:

;; Turning on eev-mode simply activates the eev-mode-map keymap, and
;; adds an "eev" to the mode line to remind you this. Turning off
;; eev-mode deactivates the keymap and the reminder. If you want an
;; eev-mode-map with fewer or other bindings, follow the instructions
;; here:
;;   (find-eev "eev-mode.el" "when-not-eev-mode-map")

;; «.eev-mode-map-set»		(to "eev-mode-map-set")
;; «.when-not-eev-mode-map»	(to "when-not-eev-mode-map")
;; «.eev-mode»			(to "eev-mode")





(defun ee-kill-this-buffer ()
  "Kill the current buffer with fewer warnings than `kill-this-buffer'.
See: (find-eev-quick-intro \"3. Elisp hyperlinks\" \"go back\" \"`M-k'\")
and: (find-eval-intro \"`M-k'\")"
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-this-buffer)))




;;;  _                                    
;;; | | _____ _   _ _ __ ___   __ _ _ __  
;;; | |/ / _ \ | | | '_ ` _ \ / _` | '_ \ 
;;; |   <  __/ |_| | | | | | | (_| | |_) |
;;; |_|\_\___|\__, |_| |_| |_|\__,_| .__/ 
;;;           |___/                |_|    
;;;
;; «eev-mode-map-set» (to ".eev-mode-map-set")

(defvar eev-mode-map nil)

(defun eev-mode-map-set ()
  "Add the standard keybindings for eev to `eev-mode-keymap'."
  ;;
  ;; Keys for following hyperlinks and for going back.
  ;; See: (find-eev-quick-intro "2. Evaluating Lisp" "When you type `M-e'")
  ;;      (find-eev-quick-intro "3. Elisp hyperlinks" "go back" "`M-k'")
  ;; Source: (find-eev "eev-eval.el")
  (define-key eev-mode-map "\M-e" 'ee-eval-sexp-eol)  ; extends C-e C-x C-e
  (define-key eev-mode-map "\M-E" 'ee-eval-last-sexp) ; extends     C-x C-e
  (define-key eev-mode-map "\M-k" 'ee-kill-this-buffer)
  (define-key eev-mode-map "\M-K" 'bury-buffer)
  ;;
  ;; Jump to numbered places (or actions).
  ;; See:    (find-eev-quick-intro "7.1. `eejump'")
  ;; Source: (find-eev "eejump.el")
  (define-key eev-mode-map "\M-j" 'eejump)
  ;;
  ;; Eepitch: a simple way to script interactive programs.
  ;; See: (find-eev-quick-intro "6. Controlling shell-like programs")
  ;;      (find-eev-quick-intro "6.1. The main key: <F8>")
  ;;      (find-eev-quick-intro "6.3. Creating eepitch blocks: `M-T'")
  ;; Source: (find-eev "eepitch.el")
  (define-key eev-mode-map [f8]   'eepitch-this-line)
  (define-key eev-mode-map "\M-T" 'eewrap-eepitch)
  ;;
  ;; Functions that transform ("wrap") the current line.
  ;; See: (find-eev-quick-intro "`eewrap-(something)'")
  ;;      (find-wrap-intro)
  ;;      (find-wrap-intro "all wrapping functions")
  ;;      Source: (find-eev "eev-wrap.el")
  (define-key eev-mode-map "\M-A" 'eewrap-anchor)
  (define-key eev-mode-map "\M-B" 'eewrap-escript-block)
  (define-key eev-mode-map "\M-C" 'eewrap-code-c-d)
  (define-key eev-mode-map "\M-D" 'eewrap-debian)
  (define-key eev-mode-map "\M-F" 'eewrap-find-fline)
  (define-key eev-mode-map "\M-J" 'eewrap-eejump)
  (define-key eev-mode-map "\M-M" 'eewrap-man)
  (define-key eev-mode-map "\M-P" 'eewrap-pdflike)
  (define-key eev-mode-map "\M-R" 'eewrap-rm/mkdir/cd)
  (define-key eev-mode-map "\M-S" 'eewrap-sh)
  (define-key eev-mode-map "\M-V" 'eewrap-audiovideo)
  ;; (define-key eev-mode-map "\M-Z" 'eewrap-zsh)
  ;; (define-key eev-mode-map "\M-#" 'eewrap-two-eepitches)
  ;;
  ;; Keys for creating temporary buffers with elisp hyperlinks:
  ;; The highest-level command is `M-h M-h', described here:
  ;;   (find-eev-quick-intro "4. Creating Elisp Hyperlinks")
  ;;   (find-eev-quick-intro "4. Creating Elisp Hyperlinks" "`M-h M-h'")
  ;; Source: (find-eev "eev-elinks.el")
  (define-key eev-mode-map "\M-h\M-d" 'find-debpkg-links)
  (define-key eev-mode-map "\M-h\M-f" 'find-efunction-links)
  (define-key eev-mode-map "\M-h\M-g" 'find-grep-links)
  (define-key eev-mode-map "\M-h\M-h" 'find-here-links)
  (define-key eev-mode-map "\M-h\M-i" 'find-eintro-or-einfo-links)
  (define-key eev-mode-map "\M-h\M-k" 'find-ekey-links)
  (define-key eev-mode-map "\M-h\M-n" 'find-eunicodeucs)
  (define-key eev-mode-map "\M-h\M-p" 'find-pdflike-page-links)
  (define-key eev-mode-map "\M-h\M-v" 'find-evariable-links)
  (define-key eev-mode-map "\M-hf"    'find-file-links)
  (define-key eev-mode-map "\M-hM"    'find-ekbmacro-links)
  ;; (define-key eev-mode-map "\M-h\M-m" 'find-manpage-links)
  ;; (define-key eev-mode-map "\M-hm"    'find-last-manpage-links)
  ;;
  ;; Information about text properties, faces, and chars:
  ;; (define-key eev-mode-map "\M-h\M-s" 'find-efacedescr)
  (define-key eev-mode-map "\M-h\M-c" 'find-echardescr)
  (define-key eev-mode-map "\M-h\M-s" 'find-eface-links)
  (define-key eev-mode-map "\M-h\M-t" 'find-etpat)
  (define-key eev-mode-map "\M-ht"    'find-etpat0)
  (define-key eev-mode-map "\M-hc"    'find-ecolor-links)
  ;;
  ;; Keys for refining hyperlinks.
  ;; See: (find-eval-intro "Producing and refining hyperlinks")
  ;;      (find-eval-intro "`M-h M-2'")
  ;; Source: (find-eev "eev-edit.el")
  (define-key eev-mode-map "\M-h\M-2" 'ee-duplicate-this-line)
  (define-key eev-mode-map "\M-h\M-y" 'ee-yank-pos-spec)
  (define-key eev-mode-map "\M-h\M--" 'ee-shrink-hyperlink-at-eol)
  ;;
  ;; These ones are also from: (find-eev "eev-edit.el")
  (define-key eev-mode-map "\M-s" 'ee-flip-psne-ness)
  (define-key eev-mode-map "\M-I" 'eewrap-vldi-list-line)
  ;;
  ;; Obsolete:
  ;; (define-key eev-mode-map "\M-hg"    'find-git-links-1)
  ;;
  )


;; «when-not-eev-mode-map» (to ".when-not-eev-mode-map")
;; Now we run the function `eev-mode-map-set' above, but we only do
;; that if the variable `eev-mode-map' holds nil:
;;
(when (not eev-mode-map)
  (setq eev-mode-map (make-sparse-keymap))
  (eev-mode-map-set))
;;
;; The `(when ...)' above means that if you want to define your own
;; `eev-mode-map' with different keybindings you can do that by
;; putting something like this
;;
;;   (setq eev-mode-map (make-sparse-keymap))
;;   (define-key eev-mode-map "\M-e" 'ee-eval-sexp-eol)
;;   (define-key eev-mode-map "\M-E" 'ee-eval-last-sexp)
;;   (define-key eev-mode-map "\M-k" 'ee-kill-this-buffer)
;;   (define-key eev-mode-map "\M-K" 'bury-buffer)
;;   (define-key eev-mode-map "\M-j" 'eejump)
;;   (define-key eev-mode-map [f8]   'eepitch-this-line)
;;   (define-key eev-mode-map "\M-T" 'eewrap-eepitch)
;;
;; in your .emacs _before the point where you load eev_.






;;;                                           _      
;;;   ___  _____   __     _ __ ___   ___   __| | ___ 
;;;  / _ \/ _ \ \ / /____| '_ ` _ \ / _ \ / _` |/ _ \
;;; |  __/  __/\ V /_____| | | | | | (_) | (_| |  __/
;;;  \___|\___| \_/      |_| |_| |_|\___/ \__,_|\___|
;;;                                                  
;; «eev-mode» (to ".eev-mode")
;; This defines `eev-mode'. Turning on eev-mode simply activates the
;; eev-mode-map keymap, and adds an "eev" to the mode line to remind
;; you this.
;;
;; At one point, when the `find-*-intro' functions either did not
;; exist or were almost irrelevant, I decided that I had to write a
;; huge description of eev-mode... I regret that.
;;
;; See:
;;   (find-efunctiondescr 'eev-mode)
;;   (find-eminorkeymapdescr 'eev-mode)
;;   (find-ekeymapdescr eev-mode-map)
;;   (find-elnode "Keys in Documentation" "\\<MAPVAR>")

(defvar eev-mode-lighter " eev")
(defvar eev-mode-help "Toggle eev mode, i.e, activate or deactivate the `eev-mode-map' keymap.
With a prefix argument ARG, turn eev-mode on if positive, else off.
See: (find-eev-intro)
\\<eev-mode-map>
Commands to follow hyperlinks:
  \\[ee-eval-sexp-eol] -- go to the end of line, then do \\[ee-eval-last-sexp]
  \\[ee-eval-last-sexp] -- eval the sexp at the left of point
    See: (find-eval-intro)
Commands to return from hyperlinks:
  \\[ee-kill-this-buffer] -- kill this buffer
  \\[bury-buffer] -- put this buffer at the end of the list of all buffers
    See: (find-eval-intro \"\\nGoing back\\n\")
Other very very important commands:
      \\[eejump] -- jump to the list of eejump targets
  M-5 \\[eejump] -- jump to the tutorial at (find-eev-quick-intro)
    See: (find-eejump-intro)
  \\[eepitch-this-line]     -- pitch this line to another Emacs buffer,
           or execute it as lisp if it starts with `'
    See: (find-eepitch-intro)
Commands to convert the current line into hyperlinks:
  \\[eewrap-find-fline]  -- wrap its contents in a `find-fline'
  \\[eewrap-man]  -- wrap its contents in a `find-man'
  \\[eewrap-sh]  -- wrap its contents in a `find-sh'
  \\[eewrap-eepitch]  -- generate an \" (eepitch-{xxx,kill,xxx})\" block
  \\[eewrap-anchor]  -- convert to two anchors pointing to one another
  \\[eewrap-code-c-d]  -- wrap its contents in a `code-c-d' and a `find-_file'
  \\[eewrap-debian]  -- wrap its contents in three Debian hyperlinks
  \\[eewrap-eejump]  -- make a `(defun eejump-N ...)' from N and a hyperlink
  \\[eewrap-rm/mkdir/cd]  -- make a rm/mkdir/cd triple
  \\[eewrap-pdflike]  -- generate links to pdf-like documents
  \\[eewrap-audiovideo]  -- generate audio/video hyperlinks
    See: (find-wrap-intro)
Commands to generate pages with lists of hyperlinks:
  \\[find-here-links]  -- \"hyperlinks to here\", which supersedes all these:
    \\[find-file-links]   -- hyperlinks to the current file
    \\[find-find-eintro-or-einfo-links] -- hyperlinks to the current intro, or Info node
    \\[find-grep-links] -- hyperlinks to `find-xxxgrep' sexps
    \\[find-manpage-links] -- hyperlinks to a manpage (ask for name)
    \\[find-last-manpage-links]   -- hyperlinks to a manpage (being viewed)
    See: (find-links-intro \"`find-here-links'\")
  \\[find-efunction-links] -- hyperlinks to an Emacs function
  \\[find-ekey-links] -- hyperlinks to a key sequence and to the function
             associated to it
  \\[find-evariable-links] -- hyperlinks to an Emacs variable
  \\[find-eface-links] -- hyperlinks to a face (default: face at point)
  \\[find-debpkg-links] -- hyperlinks about a Debian package
  \\[find-ecolor-links]   -- hyperlinks to a color
Commands to edit hyperlinks:
  \\[ee-duplicate-this-line] -- duplicate this line
  \\[ee-yank-pos-spec] -- yank into pos-spec-list
  \\[ee-shrink-hyperlink-at-eol] -- shrink `find-xxxfile' to `find-xxx'
  \\[eewrap-vldi-list-line]     -- transform filename into hyperlink
  See: (find-eval-intro \"Producing and refining hyperlinks\")
Other commands:
  \\[find-eev-mode-links] -- show this help about eev-mode, or some links
  \\[describe-char] -- lots of info about the character at point
  \\[find-etpat] -- text properties at point
  \\[find-etpat0]   -- text properties at point (output in the echo area)")


(defun eev-mode-define ()
  "Use this to redefine `eev-mode' with another lighter and another docstring."
  (eval `
   ;;
   (define-minor-mode eev-mode
     ,eev-mode-help
     :init-value nil
     :global t
     :lighter ,eev-mode-lighter)
   ;; 
   ))

(eev-mode-define)

;; (progn (eev-mode 0) (eev-mode 1))
;; (find-efunctiondescr 'eev-mode)


;; Deleted code:
;;
;; Run the default bounded action (usually `eev-bounded'):
;; (define-key eev-mode-map [f3]   'eeb-default)
;; Steppers:
;; (define-key eev-mode-map [f9]   'eechannel-do-this-line)
;; (define-key eev-mode-map [f12]  'eesteps-do-step)
;; (define-key eev-mode-map "\M-P" 'ee-yank-one-line)
;; For "compose pairs":
;; (define-key eev-mode-map [?\C-,] 'eev-compose-two-keys) ; only works on X
;; (define-key eev-mode-map [?\M-,] 'eev-compose-two-keys) ; works anywhere

;; \\[eechannel-do-this-line]  -- send this line through the default channel,
;;          or execute this line as lisp if it starts with `'
;; \\[eeb-default]  -- execute the default action on bounded regions
;; \\[ee-yank-one-line]   -- \"send\" the first line of the last kill, as if the
;;          user had typed it
;; \\[eesteps-do-step] -- execute the next step from an `eesteps' list
;; \\[eev-help-page]     -- switch to a help page, or hide it and return



(provide 'eev-mode)






;; Local Variables:
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
