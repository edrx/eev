;;; eev-testblocks.el - create "test blocks" using multiline comments.

;; Copyright (C) 2019,2020 Free Software Foundation, Inc.
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
;; Version:    2020aug05
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




;;;                  _                     _        _            _   
;;;   ___  ___      (_)_ __  ___  ___ _ __| |_     | |_ ___  ___| |_ 
;;;  / _ \/ _ \_____| | '_ \/ __|/ _ \ '__| __|____| __/ _ \/ __| __|
;;; |  __/  __/_____| | | | \__ \  __/ |  | ||_____| ||  __/\__ \ |_ 
;;;  \___|\___|     |_|_| |_|___/\___|_|   \__|     \__\___||___/\__|
;;;                                                                  
;; «ee-insert-test»  (to ".ee-insert-test")
;; See: (find-eepitch-intro "3. Test blocks")
;; Insert a "test block" in a Lua/Python/Ruby/shell/Tcl script.


(defalias 'eeit 'ee-insert-test)

(defun ee-insert-test ()
  "Insert a \"test block\" - an eepitch block in a multiline comment."
  (interactive)
  (if (fboundp (intern (format "ee-insert-test-%s" major-mode)))
      (funcall (intern (format "ee-insert-test-%s" major-mode)))
    (error "ee-insert-test: Unsupported major mode: %s" major-mode)))

;; The old definition was:
;;
;; (defun ee-insert-test ()
;;   "Insert a \"test block\" - an eepitch block in a multiline comment."
;;   (interactive)
;;   (cond ((eq major-mode 'lua-mode)     (ee-insert-test-lua))
;;         ((eq major-mode 'python-mode)  (ee-insert-test-python))
;;         ((eq major-mode 'ruby-mode)    (ee-insert-test-ruby))
;;         ((eq major-mode 'sh-mode)      (ee-insert-test-sh))
;;         ((eq major-mode 'tcl-mode)     (ee-insert-test-tcl))
;;         ((eq major-mode 'idris-mode)   (ee-insert-test-idris))
;;         ((eq major-mode 'haskell-mode) (ee-insert-test-haskell))
;;         (t (error "ee-insert-test: Unsupported major mode"))))


(defun ee-insert-test-julia-mode ()
  (interactive)
  (insert (format "
#=
 (eepitch-julia)
 (eepitch-kill)
 (eepitch-julia)
include(\"%s\")

=#
" (buffer-name))))

(defun ee-insert-test-lua-mode ()
  (interactive)
  (insert (format "
--[[
 (eepitch-lua51)
 (eepitch-kill)
 (eepitch-lua51)
dofile \"%s\"

--]]
" (buffer-name))))

(defun ee-insert-test-python-mode ()
  (interactive)
  (insert (format "
\"\"\"
 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
execfile(\"%s\", globals())

\"\"\"
" (buffer-name))))

(defun ee-insert-test-ruby-mode ()
  (interactive)
  (insert (format "
=begin
 (eepitch-ruby)
 (eepitch-kill)
 (eepitch-ruby)
load \"%s\"

=end
" (buffer-name))))

(defun ee-insert-test-sh-mode ()
  (interactive)
  (insert (format "
: <<'%%%%%%%%%%'
 (eepitch-sh)
 (eepitch-kill)
 (eepitch-sh)
. %s

%%%%%%%%%%
" (buffer-name))))

(defun ee-insert-test-tcl-mode ()
  (interactive)
  (insert (format "
set COMMENTED_OUT {
 (eepitch-tclsh)
 (eepitch-kill)
 (eepitch-tclsh)
source %s

}
" (buffer-name))))

(defun ee-insert-test-idris-mode ()
  (interactive)
  (insert (format "
{-
 (eepitch-to-buffer \"*idris-repl*\")

-}
" (buffer-name))))


(defun ee-insert-test-haskell-mode ()
  (interactive)
  (insert (format "
{-
 (eepitch-ghci)
 (eepitch-kill)
 (eepitch-ghci)
:load %s

-}
" (buffer-name))))





(provide 'eev-testblocks)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
