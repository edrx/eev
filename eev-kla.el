;;; eev-kla.el -- kill link to anchor and friends.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.
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
;; Version:    20220307
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-kla.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-kla.el.html>
;;       See also: <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;; «.intro»		(to "intro")
;; «.test»		(to "test")
;; «.ee-preferred-c»	(to "ee-preferred-c")
;; «.utils»		(to "utils")
;; «.eekla»		(to "eekla")
;; «.eeklf»		(to "eeklf")
;; «.aliases»		(to "aliases")




;;; Commentary:

;; «intro»  (to ".intro")
;;
;; 0. Prerequisites
;; ================
;; This tool will only make sense to people who understand `code-c-d',
;; anchors, and `find-here-links' very well. See:
;;
;;   (find-eev-quick-intro "9.1. `code-c-d'")
;;   (find-eev-quick-intro "9.2. Extra arguments to `code-c-d'")
;;   (find-eev-quick-intro "9.2. Extra arguments to `code-c-d'" "to anchors")
;;   (find-eev-quick-intro "8. Anchors")
;;   (find-eev-quick-intro "4. Creating Elisp Hyperlinks")
;;   (find-eev-quick-intro "4.1. `find-here-links'")
;;
;;
;;
;; 1. Choosing a link to here
;; ==========================
;; This section of the main tutorial
;;
;;   (find-eev-quick-intro "10.1. Generating short hyperlinks to files")
;;
;; explains that we can have several `code-c-d's "pointing to the same
;; directory", and if we run
;;
;;   (eek "M-h M-h  ;; find-here-links")
;;
;; here in this file it will try to generate links to "here" - and it
;; will show several options, including one for each `code-c-d' that
;; points to this directory or to any of its parents. One of the
;; options will be this one:
;;
;;   (find-eevfile "eev-kla.el")
;;
;; It is associated to this `code-c-d':
;;
;;   (code-c-d "eev" ee-eev-source-directory :anchor)
;;
;; that is one of the predefined `code-c-d's that are executed when
;; eev is loaded. It appears here:
;;
;;   (find-eev "eev-code.el" "code-c-d-s")
;;
;; Apparently there isn't a way to choose automatically which of the
;; links in the `find-here-links' buffer is the best one - we always
;; have to choose the "right" link by hand.
;;
;;
;;
;; 2. Our preferred link to here
;; =============================
;; This file implement a VERY HACKY way to choose the "right" link
;; automatically and to save it to the kill ring. When the variable
;;
;;   ee-preferred-c
;;
;; is non-nil it says which `code-c-d' is the "best" one for this
;; file; if you eval it above you will see that its value is "eev",
;; that is the "c", i.e., the first argument, of this `code-c-d':
;;
;;   (code-c-d "eev" ee-eev-source-directory :anchor)
;;
;; The "d" of that `code-c-d' is its second argument - the directory.
;; The "d" for that "c" is stored in this variable:
;;
;;   ee-eevdir
;;
;; so knowing the "c" is enough.
;;
;; My favorite way to set the value of `ee-preferred-c' in a directory
;; and in all its subdirectories is by using directory variables. See:
;;
;;   (find-enode "Directory Variables")
;;   (find-eev ".dir-locals.el")
;;
;;
;;
;; 3. Commands
;; ===========
;; At the moment this file implements these commands:
;;
;;   1) `M-x eekla', that <K>ills a <L>ink to an <A>nchor.
;;      See: (find-anchors-intro "3. The preceding tag" "M-1 M-h M-w")
;;           (find-refining-intro "5. Pointing to anchors")
;;
;;         «test»  (to ".test")
;;
;;      To test this, run `M-x eekla' here. It will highlight the
;;      anchor above ("test") for a fraction of a second and will show
;;      this message in the echo area:
;;
;;        Copied to the kill ring: (find-eev "eev-kla.el" "test")
;;
;;   2) `M-x eeklf', that <K>ills a <L>ink to a <F>ile.
;;       To test this, just run `M-x eeklf'. You will see this message
;;       in the echo area:
;;
;;         Copied to the kill ring: (find-eevfile "eev-kla.el")
;;
;;   3) `M-x eeklas'
;;
;;   4) `M-x eeklfs'
;;
;;   3) `M-x ee-preferred-c-show', that shows the current directory
;;      and the value of `ee-preferred-c' at the window at the right.
;;      This is useful to check if the ".dir-locals.el" file was
;;      correctly written. Try:
;;
;;        (ee-preferred-c-show)
;;
;; TODO: `M-x eekls', that <K>ills a <L>ink to a <S>tring.
;;
;;
;;
;; 4. The gory details
;; ===================
;; The function `find-here-links' calls the function
;; `ee-find-here-links' to generate the non-header part of the
;; "*(find-here-links)*" buffer. When `ee-find-here-links' decides
;; that "here" is a file it calls the function `ee-find-file-links' to
;; generate a series of links to the current file.
;;
;; If you really, really, REALLY want to know the innards of how this
;; works and how `ee-find-file-links' consults the data stored by
;; `code-c-d' to generate these links, follow the links below:
;;
;;   (find-eev "eev-hlinks.el" "ee-find-here-links")
;;   (find-eev "eev-elinks.el" "find-file-links")
;;   (find-eev "eev-elinks.el" "ee-find-xxxfile-sexps")
;;   (find-eev "eev-elinks.el" "ee-code-c-d-filter")
;;   (find-eev "eev-elinks.el" "ee-code-c-d-filter" "Try:")





;; «ee-preferred-c»  (to ".ee-preferred-c")
;;
(defvar ee-preferred-c nil
  "See: (find-eev \"eev-kla.el\")")

;;;###autoload
(put   'ee-preferred-c 'safe-local-variable #'stringp)

(defun ee-preferred-c-show ()
  "Show the current value of `ee-preferred-c' at the window at the right."
  (interactive)
  (find-2a nil '(find-epp (list default-directory ee-preferred-c))))



;; «utils»  (to ".utils")
;; Tests: (ee-kl-c)
;;        (ee-kl-dir)
;;        (ee-kl-fname)
;;                   ee-edir
;;        (ee-kl-dir   "e")
;;                         (ee-efile "textmodes/fill.el")
;;        (ee-kl-fname "e" (ee-efile "textmodes/fill.el"))
;;
(defun ee-kl-c ()
  (or ee-preferred-c (error "`ee-preferred-c' is nil here!")))

(defun ee-kl-dir (&optional c)
  (ee-expand (eval (read (format "ee-%sdir" (or c (ee-kl-c)))))))

(defun ee-kl-fname (&optional c fname)
  (ee-remove-prefix (ee-kl-dir c) (ee-expand (or fname buffer-file-name))))

;; Tests: (ee-kl-anchor)
;;        (ee-kl-region)
;;        (ee-kl-kill "(FOO \"BAR\")")
;;
(defun ee-kl-anchor ()
  (ee-copy-preceding-tag-to-kill-ring))

(defun ee-kl-region ()
  (if (not (use-region-p)) (error "The region is not active!"))
  (buffer-substring-no-properties (point) (mark)))

(defun ee-kl-kill (sexp)
  (kill-new (concat sexp "\n"))
  (message "Copied to the kill ring: %s" sexp))



;; Tests:
;;   (defun find-2ae (sexp) (find-2a nil `(find-estring ,sexp :end)))
;;   (find-2ae ' (concat "FOO\n" "BAR") )
;;   (find-2ae ' (ee-kla-sexp  "eev" "foo.el" "ANCHOR") )
;;   (find-2ae ' (ee-klas-sexp "eev" "foo.el" "ANCHOR" "FOO\nBAR") )
;;   (find-2ae ' (ee-klf-sexp  "eev" "foo.el") )
;;   (find-2ae ' (ee-klfs-sexp "eev" "foo.el" "FOO\nBAR") )
;;        
(defun ee-kla-sexp (c fname anchor)
  (format "(find-%s \"%s\" \"%s\")" c fname anchor))

(defun ee-klas-sexp (c fname anchor str)
  (format "(find-%s \"%s\" \"%s\" %s)" c fname anchor (ee-S str)))

(defun ee-klf-sexp (c fname)
  (format "(find-%sfile \"%s\")" c fname))

(defun ee-klfs-sexp (c fname str)
  (format "(find-%sfile \"%s\" %s)" c fname (ee-S str)))







;;;            _    _       
;;;   ___  ___| | _| | __ _ 
;;;  / _ \/ _ \ |/ / |/ _` |
;;; |  __/  __/   <| | (_| |
;;;  \___|\___|_|\_\_|\__,_|
;;;                         
;; «eekla»  (to ".eekla")
;; <K>ill <L>ink to <A>nchor,
;; <K>ill <L>ink to <A>nchor and <S>tring,
;; <K>ill <L>ink to <File>.
;; <K>ill <L>ink to <File> and <S>tring.
;; More precisely: produce a link to the preceding anchor and put it
;; in the kill-ring.

;; (eek "C-e 4*<left> C-SPC <<klas>>")


(defun eekla ()
  "<K>ill <L>ink to <A>nchor.
Put in the kill ring a link to the preceding anchor."
  (interactive)
  (ee-kl-kill (ee-kla-sexp (ee-kl-c) (ee-kl-fname) (ee-kl-anchor))))

(defun eeklas ()
  "<K>ill <L>ink to <A>nchor and <S>tring.
Put in the kill ring a link to the preceding anchor."
  (interactive)
  (ee-kl-kill (ee-klas-sexp (ee-kl-c) (ee-kl-fname) (ee-kl-anchor) (ee-kl-region))))

(defun eeklf ()
  "<K>ill <L>ink to <F>ile."
  (interactive)
  (ee-kl-kill (ee-klf-sexp (ee-kl-c) (ee-kl-fname))))

(defun eeklfs ()
  "<K>ill <L>ink to <F>ile and <S>tring."
  (interactive)
  (ee-kl-kill (ee-klfs-sexp (ee-kl-c) (ee-kl-fname) (ee-kl-region))))



;; «aliases»  (to ".aliases")
;; I use these aliases:
;; (defalias 'kla  'eekla)
;; (defalias 'klas 'eeklas)
;; (defalias 'klf  'eeklf)
;; (defalias 'klfs 'eeklfs)



(provide 'eev-kla)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
