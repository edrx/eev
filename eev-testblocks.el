;;; eev-testblocks.el - create "test blocks" using multiline comments.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2019-2025 Free Software Foundation, Inc.
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
;; Version:    20250913
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-testblocks.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-testblocks.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                 <http://anggtwu.net/eev-intros/find-links-intro.html>
;;                                               (find-eev-intro)
;;                                               (find-links-intro)

;;; Commentary:

;; A "test block" in a script is a multiline comment that contains
;; interactive tests. See:
;;
;;   (find-eepitch-intro "3. Test blocks")
;;   (find-eepitch-intro "3.1. `find-eeit-links'")
;;   http://anggtwu.net/emacsconf2021.html
;;   http://anggtwu.net/LATEX/2021emacsconf.pdf

;; «.eeit»			(to "eeit")
;; «.ee-insert-test»		(to "ee-insert-test")
;; «.ee-insert-test-block»	(to "ee-insert-test-block")
;; «.examples»			(to "examples")
;;   «.c-mode»			(to "c-mode")
;;   «.elixir-mode»		(to "elixir-mode")
;;   «.fennel-mode»		(to "fennel-mode")
;;   «.f90-mode»		(to "f90-mode")
;;   «.gnuplot-mode»		(to "gnuplot-mode")
;;   «.haskell-mode»		(to "haskell-mode")
;;   «.js-mode»			(to "js-mode")
;;   «.julia-mode»		(to "julia-mode")
;;   «.latex-mode»		(to "latex-mode")
;;   «.lisp»			(to "lisp")
;;   «.lisp-mode»		(to "lisp-mode")
;;     «.slime»			(to "slime")
;;   «.lua-mode»		(to "lua-mode")
;;   «.makefile-gmake»		(to "makefile-gmake")
;;   «.makefile-mode»		(to "makefile-mode")
;;   «.maxima-mode»		(to "maxima-mode")
;;   «.octave-mode»		(to "octave-mode")
;;   «.org-mode»		(to "org-mode")
;;   «.php-mode»		(to "php-mode")
;;   «.python-mode»		(to "python-mode")
;;   «.racket-mode»		(to "racket-mode")
;;   «.raku-mode»		(to "raku-mode")
;;   «.ruby-mode»		(to "ruby-mode")
;;   «.scheme-mode»		(to "scheme-mode")
;;   «.sml-mode»		(to "sml-mode")
;;   «.sh-mode»			(to "sh-mode")
;;   «.sql-mode»		(to "sql-mode")
;;   «.subed-vtt-mode»		(to "subed-vtt-mode")
;;   «.tcl-mode»		(to "tcl-mode")
;;   «.tuareg-mode»		(to "tuareg-mode")





;;;                  _                     _        _            _   
;;;   ___  ___      (_)_ __  ___  ___ _ __| |_     | |_ ___  ___| |_ 
;;;  / _ \/ _ \_____| | '_ \/ __|/ _ \ '__| __|____| __/ _ \/ __| __|
;;; |  __/  __/_____| | | | \__ \  __/ |  | ||_____| ||  __/\__ \ |_ 
;;;  \___|\___|     |_|_| |_|___/\___|_|   \__|     \__\___||___/\__|
;;;                                                                  
;; «eeit»  (to ".eeit")
;; «ee-insert-test»  (to ".ee-insert-test")
;; «ee-insert-test-block»  (to ".ee-insert-test-block")
;;
;; Long story short: `M-x eeit' inserts a "test block"
;; in a Lua/Python/Ruby/shell/Tcl/etc script.
;;
;; See: http://anggtwu.net/eepitch.html
;;      (find-eepitch-intro "3. Test blocks")
;;      (find-eev2021hsubs "00:14" "if we type f8 several times here")
;;      (find-eev2021video "00:14" "if we type f8 several times here")
;;      (find-eev2021hsubs "00:42" "as a multi-line comment")
;;      (find-eev2021video "00:42" "as a multi-line comment")
;;      
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


;; «c-mode»  (to ".c-mode")
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


;; «elixir-mode»  (to ".elixir-mode")
(defun ee-insert-test-elixir-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
~S\"\"\"
 (eepitch-iex)
 (eepitch-kill)
 (eepitch-iex)
c \"%s\"

\"\"\"
" (buffer-name)))))


;; «fennel-mode»  (to ".fennel-mode")
(defun ee-insert-test-fennel-mode ()
  (interactive)
  (let* ((fname (buffer-name))
	 (stem  (replace-regexp-in-string ".fnl$" "" fname)))
    (insert (ee-adjust-red-stars (format "
(comment \"Test block:
 (eepitch-fennel)
 (eepitch-kill)
 (eepitch-fennel)
,reload %s

\")
" stem)))))


;; «f90-mode»  (to ".f90-mode")
(defun ee-insert-test-f90-mode ()
  (interactive)
  (let* ((fullname  (buffer-name))
	(shortname (replace-regexp-in-string "\\.[fF].*$" "" fullname)))
    (insert (ee-adjust-red-stars (ee-template0 "
!T  (find-eepitch-intro \"3.3. `eepitch-preprocess-line'\")
!T  (setq eepitch-preprocess-regexp \"^!T ?\")
!T  (eepitch-shell)
!T  (eepitch-kill)
!T  (eepitch-shell)
!T gfortran {fullname} -o {shortname}
!T ./{shortname}

")))))


;; «gnuplot-mode»  (to ".gnuplot-mode")
(defun ee-insert-test-gnuplot-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
# (find-eepitch-intro \"3.3. `eepitch-preprocess-line'\")
# (setq eepitch-preprocess-regexp \"\")
# (setq eepitch-preprocess-regexp \"^#: \")
#
#:  (eepitch-shell)
#:  (eepitch-kill)
#:  (eepitch-shell)
#: gnuplot %s

" (buffer-name)))))


;; «haskell-mode»  (to ".haskell-mode")
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


;; «js-mode»  (to ".js-mode")
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


;; «julia-mode»  (to ".julia-mode")
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


;; «latex-mode»  (to ".latex-mode")
(defun ee-insert-test-latex-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
%% See: (find-eepitch-intro \"3.3. `eepitch-preprocess-line'\")
%% (setq eepitch-preprocess-regexp \"^\")
%% (setq eepitch-preprocess-regexp \"^%%T ?\")
%%
%%T  (eepitch-shell)
%%T  (eepitch-kill)
%%T  (eepitch-shell)
%%T 

" ; (buffer-name)
))))


;; «lisp-mode»  (to ".lisp-mode")
(defun ee-insert-test-lisp-mode ()
  (funcall (ee-intern "ee-insert-test-lisp-mode-%s" current-prefix-arg)))

(defun ee-insert-test-lisp-mode-nil ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
#|
 (eepitch-sbcl)
 (eepitch-kill)
 (eepitch-sbcl)
(load \"%s\")

|#
" (buffer-name)))))


;; «slime»  (to ".slime")
;; See: (find-eev "eepitch.el" "eepitch-slime")
(defun ee-insert-test-lisp-mode-1 ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
#|
 (eepitch-slime-kill)
 To restart Slime:
    (eepitch-set-source-and-M-x-b 2)
    (slime \"sbcl\")
 (eepitch-slime-select)
(load \"%s\")

|#
" (buffer-name)))))


;; «lua-mode»  (to ".lua-mode")
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


;; «makefile-gmake»  (to ".makefile-gmake")
(defun ee-insert-test-makefile-gmake-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
# See: (find-eepitch-intro \"3.3. `eepitch-preprocess-line'\")
# (setq eepitch-preprocess-regexp \"^\")
# (setq eepitch-preprocess-regexp \"^#T ?\")
#
#T  (eepitch-shell)
#T  (eepitch-kill)
#T  (eepitch-shell)
#T make -f %s TARGET

" (buffer-name)))))


;; «makefile-mode»  (to ".makefile-mode")
(defun ee-insert-test-makefile-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
# See: (find-eepitch-intro \"3.3. `eepitch-preprocess-line'\")
# (setq eepitch-preprocess-regexp \"^\")
# (setq eepitch-preprocess-regexp \"^#T ?\")
#
#T  (eepitch-shell)
#T  (eepitch-kill)
#T  (eepitch-shell)
#T make -f %s TARGET

" (buffer-name)))))


;; «maxima-mode»  (to ".maxima-mode")
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


;; «octave-mode»  (to ".octave-mode")
(defun ee-insert-test-octave-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
#{
 (eepitch-octave)
 (eepitch-kill)
 (eepitch-octave)
run %s

#}
" (buffer-name)))))


;; «org-mode»  (to ".org-mode")
(defun ee-insert-test-org-mode ()
  (interactive)
  (insert (ee-adjust-red-stars "
#+begin_comment
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

#+end_comment

")))


;; «php-mode»  (to ".php-mode")
(defun ee-insert-test-php-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
<?php
/*
 (eepitch-php)
 (eepitch-kill)
 (eepitch-php)
include '%s';

*/
?>
" (buffer-name)))))


;; «python-mode»  (to ".python-mode")
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


;; «racket-mode»  (to ".racket-mode")
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


;; «raku-mode»  (to ".raku-mode")
(defun ee-insert-test-raku-mode ()
  (interactive)
  (let ((libname
	 (replace-regexp-in-string
	  "\\.rakumod$" "" (buffer-name))))
    (insert (ee-adjust-red-stars (format "
#`(
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
raku %s

 (eepitch-raku)
 (eepitch-kill)
 (eepitch-raku)
use lib '.'
use %s

)
" (buffer-name) libname)))))


;; «ruby-mode»  (to ".ruby-mode")
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


;; «scheme-mode»  (to ".scheme-mode")
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


;; «sml-mode»  (to ".sml-mode")
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


;; «sh-mode»  (to ".sh-mode")
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


;; «sql-mode»  (to ".sql-mode")
(defun ee-insert-test-sql-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (format "
/*
 (eepitch-sqlite3)
 (eepitch-kill)
 (eepitch-sqlite3)
.read \"%s\"

*/
" (buffer-name)))))


;; «subed-vtt-mode»  (to ".subed-vtt-mode")
(defun ee-insert-test-subed-vtt-mode ()
  (interactive)
  (insert (ee-adjust-red-stars (ee-expand "
NOTE
 Set `subed-mpv-media-file'
 ==========================
       subed-mpv-media-file
                            (subed-guess-media-file)
 (setq subed-mpv-media-file (subed-guess-media-file))
       subed-mpv-media-file

 Start mpv, check the socket
 ===========================
 (find-sh0 \"rm -Rfv /tmp/subed/\")
  subed-mpv-socket-dir
 (subed-mpv--play subed-mpv-media-file)
 (subed-mpv--socket)
 (find-sh0 \"ls -lAF /tmp/subed/\")

 Send commands to the socket
 ===========================
 (find-subedsgrep \"grep --color=auto -nH --null -e subed-mpv--client-send *.el\")
 (subed-mpv--client-send `(set_property pause no))
 (subed-mpv--client-send `(set_property pause yes))
 (subed-mpv--client-send `(seek  10 relative+exact)))
 (subed-mpv--client-send `(seek -10 relative+exact)))

 Examine some local variables
 ============================
 (find-ebufferlocalvars \"\\n (subed\")
 (find-eaproposf \"subed-mpv\")
 (find-eaproposv \"subed-mpv\")
 subed-mpv--client-command-queue
 subed-mpv--client-proc
 subed-mpv--client-test-request
 subed-mpv--retry-delays
 subed-mpv--server-proc
 subed-mpv-arguments
 subed-mpv-executable
 subed-mpv-file-loaded-hook
 subed-mpv-frame-step-map
 subed-mpv-is-playing
 subed-mpv-media-file
 subed-mpv-playback-position
 subed-mpv-playback-position-hook
 subed-mpv-playback-speed
 subed-mpv-socket-dir
"))))


;; «tcl-mode»  (to ".tcl-mode")
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


;; «tuareg-mode»  (to ".tuareg-mode")
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
