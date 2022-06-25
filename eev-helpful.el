;;; eev-helpful.el -- eev support for helpful.  -*- lexical-binding: nil; -*-

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
;; Version:    20220318
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-helpful.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-helpful.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)


;; «.find-helpful-links»	(to "find-helpful-links")
;; «.find-hlinks»		(to "find-hlinks")



;;; Commentary:

;; For more info on helpful, see:
;;
;;   https://github.com/Wilfred/helpful
;;   (find-epackage-links 'helpful)
;;
;; Status of this file: early alpha.
;; This code is very recent. Everything here may change.
;;
;; Note that `find-here-links' calls functions from this file
;; when the major mode is `helpful-mode' - see:
;;
;;   (find-eev "eev-hlinks.el" "ee-fhl-main-program")
;;   (find-eev "eev-hlinks.el" "ee-fhl-main-program" "helpful")
;;
;; Also, `find-efunction-links' and `find-evariable-links' now include
;; links with `find-hfunction' and `find-hvariable' in the temporary
;; buffers that they generate. If you execute them without having
;; helpful installed you will get errors that should be easy to
;; interpret as "hm, I need to install helpful.el"...





;;;  _                         _ _       _        
;;; | |__   ___ _ __ ___      | (_)_ __ | | _____ 
;;; | '_ \ / _ \ '__/ _ \_____| | | '_ \| |/ / __|
;;; | | | |  __/ | |  __/_____| | | | | |   <\__ \
;;; |_| |_|\___|_|  \___|     |_|_|_| |_|_|\_\___/
;;;                                               
;; «find-helpful-links»  (to ".find-helpful-links")
;; Skel: (find-find-links-links-new "helpful" "action symbol" "")
;; Test: (find-helpful-links "function" "find-file")
;;  See: (find-eevfile "eev-hlinks.el" "ee-fhl-main-program" "helpful")
;;
(defun find-helpful-links (&optional action symbol &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks to a helpful buffer."
  (interactive)
  (setq action (or action (ee-helpful-action) "{action}"))
  (setq symbol (or symbol (ee-helpful-symbol) "{symbol}"))
  (apply
   'find-elinks
   `((find-helpful-links ,action ,symbol ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-helpful-links)
     ""
     ,@(ee-find-helpful-links action symbol)
     )
   pos-spec-list))

(defun ee-find-helpful-links (&optional action symbol)
  (setq action (or action (ee-helpful-action) "{action}"))
  (setq symbol (or symbol (ee-helpful-symbol) "{symbol}"))
  (list (ee-template0 "\
# (find-h{action} '{symbol})
# (helpful-{action} '{symbol})
# (find-efunction 'helpful-{action})

# (find-efunction-links '{symbol})
# (find-evariable-links '{symbol})
")))


;; Tests: 
;; (ee-helpful-action "*helpful command: find-file*")
;; (ee-helpful-symbol "*helpful command: find-file*")
;;
(defvar ee-helpful-regexp "^\\*helpful \\([a-z]+\\): \\(.*\\)\\*$"
  "A regexp that extracts the \"action\" and the \"symbol\" from
the name of a buffer in helpful-mode.")

(defun ee-helpful-action (&optional buffername)
"An internal function used by `ee-find-helpful-links' and `find-helpful-links'."
  (setq buffername (or buffername (buffer-name)))
  (if (string-match ee-helpful-regexp buffername)
      (ee-no-properties (match-string 1 buffername))))

(defun ee-helpful-symbol (&optional buffername)
"An internal function used by `ee-find-helpful-links' and `find-helpful-links'."
  (setq buffername (or buffername (buffer-name)))
  (if (string-match ee-helpful-regexp buffername)
      (intern (ee-no-properties (match-string 2 buffername)))))



;; (load "eev-helpful.el")

;; See:


;;;  _     _       _        
;;; | |   (_)_ __ | | _____ 
;;; | |   | | '_ \| |/ / __|
;;; | |___| | | | |   <\__ \
;;; |_____|_|_| |_|_|\_\___/
;;;                         
;; «find-hlinks»  (to ".find-hlinks")
;; The functions below - with names like `find-h<action>' -
;; are hyperlinks to helpful buffers. They use this trick to
;; make helpful use the current window:
;;   (find-eev "eev-blinks.el" "find-dbsw")

;; Test: (find-hcallable 'find-file)
;;  See: (find-efunction 'helpful-callable)
(defun find-hcallable (symbol &rest pos-spec-list)
  "Run `(helpful-callable SYMBOL)' and search for POS-SPEC-LIST."
  (apply 'find-dbsw-call `(helpful-callable ',symbol) pos-spec-list))

;; Test: (find-hcommand  'find-file)
;;  See: (find-efunction 'helpful-command)
(defun find-hcommand (symbol &rest pos-spec-list)
  "Run `(helpful-command SYMBOL)' and search for POS-SPEC-LIST."
  (apply 'find-dbsw-call `(helpful-command ',symbol) pos-spec-list))

;; Test: (find-hfunction 'find-file)
;;  See: (find-efunction 'helpful-function)
(defun find-hfunction (symbol &rest pos-spec-list)
  "Run `(helpful-function SYMBOL)' and search for POS-SPEC-LIST."
  (apply 'find-dbsw-call `(helpful-function ',symbol) pos-spec-list))

;; Test: (find-hkey      (kbd "M-h M-h"))
;;  See: (find-efunction 'helpful-key)
(defun find-hkey (keyseq &rest pos-spec-list)
  "Run `(helpful-key KEYSEQ)' and search for POS-SPEC-LIST."
  (apply 'find-dbsw-call `(helpful-key ',keyseq) pos-spec-list))

;; Test: (find-hmacro    'cl-loop)
;;  See: (find-efunction 'helpful-macro)
(defun find-hmacro (symbol &rest pos-spec-list)
  "Run `(helpful-macro SYMBOL)' and search for POS-SPEC-LIST."
  (apply 'find-dbsw-call `(helpful-macro ',symbol) pos-spec-list))

;; Test: (find-hsymbol   'cl-loop)
;;  See: (find-efunction 'helpful-symbol)
(defun find-hsymbol (symbol &rest pos-spec-list)
  "Run `(helpful-symbol SYMBOL)' and search for POS-SPEC-LIST."
  (apply 'find-dbsw-call `(helpful-symbol ',symbol) pos-spec-list))

;; Test: (find-hvariable 'line-move-visual)
;;  See: (find-efunction 'helpful-variable)
(defun find-hvariable (symbol &rest pos-spec-list)
  "Run `(helpful-variable SYMBOL)' and search for POS-SPEC-LIST."
  (apply 'find-dbsw-call `(helpful-variable ',symbol) pos-spec-list))







(provide 'eev-helpful)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
