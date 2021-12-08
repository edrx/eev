;;; eev-testblocks.el - create "test blocks" using multiline comments.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
;; Version:    20211208
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-testblocks.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-testblocks.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-links-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-links-intro)

;;; Commentary:

;; A "test block" in a script is a multiline comment that contains
;; interactive tests. See:
;;
;;   (find-eepitch-intro "3. Test blocks")
;;   (find-eepitch-intro "3.1. `find-eeit-links'")
;;   http://angg.twu.net/emacsconf2021.html
;;   http://angg.twu.net/LATEX/2021emacsconf.pdf

;; «.eeit»			(to "eeit")
;; «.ee-insert-test»		(to "ee-insert-test")
;; «.ee-insert-test-block»	(to "ee-insert-test-block")
;; «.examples»			(to "examples")





;;;                  _                     _        _            _   
;;;   ___  ___      (_)_ __  ___  ___ _ __| |_     | |_ ___  ___| |_ 
;;;  / _ \/ _ \_____| | '_ \/ __|/ _ \ '__| __|____| __/ _ \/ __| __|
;;; |  __/  __/_____| | | | \__ \  __/ |  | ||_____| ||  __/\__ \ |_ 
;;;  \___|\___|     |_|_| |_|___/\___|_|   \__|     \__\___||___/\__|
;;;                                                                  
;; «eeit»  (to ".eeit")
;; «ee-insert-test»  (to ".ee-insert-test")
;; «ee-insert-test-block»  (to ".ee-insert-test-block")
;; See: (find-eepitch-intro "3. Test blocks")
;; Insert a "test block" in a Lua/Python/Ruby/shell/Tcl/etc script.

(defalias 'eeit           'ee-insert-test-block)
(defalias 'ee-insert-test 'ee-insert-test-block)

(defun ee-insert-test-block ()
  "Insert a \"test block\" - an eepitch block in a multiline comment.
If the major mode is `foo-mode' then this function calls
`ee-insert-test-foo-mode' if it exists, and yields an error if
`ee-insert-test-foo-mode' is not defined."
  (interactive)
  (if (fboundp (intern (format "ee-insert-test-%s" major-mode)))
      (funcall (intern (format "ee-insert-test-%s" major-mode)))
    (error "ee-insert-test: Unsupported major mode: %s" major-mode)))


;;;                                 _           
;;;   _____  ____ _ _ __ ___  _ __ | | ___  ___ 
;;;  / _ \ \/ / _` | '_ ` _ \| '_ \| |/ _ \/ __|
;;; |  __/>  < (_| | | | | | | |_) | |  __/\__ \
;;;  \___/_/\_\__,_|_| |_| |_| .__/|_|\___||___/
;;;                          |_|                
;;
;; «examples»  (to ".examples")
;; See: (find-eepitch-intro "3.1. `find-eeit-links'")

(defun ee-insert-test-c-mode ()
  (interactive)
  (let* ((fnamec (buffer-name))
	 (fname  (replace-regexp-in-string ".c$" "" fnamec)))
    (insert (ee-adjust-red-stars (ee-template0 "\
/*
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
gcc -o {fname} {fnamec}
./{fname}

*/
")))))

(defun ee-insert-test-haskell-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
{-
 (eepitch-ghci)
 (eepitch-kill)
 (eepitch-ghci)
:load %s

-}
" (buffer-name)))))

(defun ee-insert-test-js-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
/*
 (eepitch-nodejs)
 (eepitch-kill)
 (eepitch-nodejs)
require(\"./%s\")

*/
" (buffer-name)))))

(defun ee-insert-test-julia-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
#=
 (eepitch-julia)
 (eepitch-kill)
 (eepitch-julia)
include(\"%s\")

=#
" (buffer-name)))))

(defun ee-insert-test-lisp-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
#|
 (eepitch-sbcl)
 (eepitch-kill)
 (eepitch-sbcl)
(load \"%s\")

|#
" (buffer-name)))))

(defun ee-insert-test-lua-mode ()
  (interactive)
  (let ((equals (make-string (or current-prefix-arg 0) ?=)))
    (insert (ee-adjust-red-stars (format "
--[%s[
 (eepitch-lua51)
 (eepitch-kill)
 (eepitch-lua51)
dofile \"%s\"

--]%s]
" equals (buffer-name) equals)))))

(defun ee-insert-test-maxima-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
/*
 (eepitch-maxima)
 (eepitch-kill)
 (eepitch-maxima)
load(\"%s\");

*/
" (buffer-name)))))

(defun ee-insert-test-org-mode ()
  (interactive)
  (insert (ee-adjust-red-stars "
#+begin_comment
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

#+end_comment

")))

(defun ee-insert-test-python-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
\"\"\"
 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
exec(open(\"%s\").read(), globals())

\"\"\"
" (buffer-name)))))

(defun ee-insert-test-racket-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
#|
 (eepitch-racket)
 (eepitch-kill)
 (eepitch-racket)
(load \"%s\")

|#
" (buffer-name)))))

(defun ee-insert-test-ruby-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
=begin
 (eepitch-ruby)
 (eepitch-kill)
 (eepitch-ruby)
load \"%s\"

=end
" (buffer-name)))))

;; For Chez Scheme.
(defun ee-insert-test-scheme-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
#|
 (eepitch-scheme)
 (eepitch-kill)
 (eepitch-scheme)
(load \"%s\")

|#
" (buffer-name)))))

;; ;; For Guile.
;; (defun ee-insert-test-scheme-mode ()
;;   (interactive)
;;   (insert (ee-adjust-red-stars (format "
;; #|
;;  (eepitch-guile)
;;  (eepitch-kill)
;;  (eepitch-guile)
;; (load \"%s\")
;; 
;; |#
;; " (buffer-name)))))

(defun ee-insert-test-sml-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
(*
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
sml
use \"%s\";

*)
" (buffer-name)))))

(defun ee-insert-test-sh-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
: <<'%%%%%%%%%%'
 (eepitch-sh)
 (eepitch-kill)
 (eepitch-sh)
. %s

%%%%%%%%%%
" (buffer-name)))))

(defun ee-insert-test-tcl-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
lindex {{} {This is a test block:
 (eepitch-tclsh)
 (eepitch-kill)
 (eepitch-tclsh)
  source %s

}} 0 ;# End of the test block
" (buffer-name)))))

(defun ee-insert-test-tuareg-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
(*
 (eepitch-ocaml)
 (eepitch-kill)
 (eepitch-ocaml)
#use \"%s\";;

*)
" (buffer-name)))))





(provide 'eev-testblocks)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
