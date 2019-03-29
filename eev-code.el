;;; eev-code.el -- `code-c-d', that generates and evaluates Lisp defuns.

;; Copyright (C) 2012,2018,2019 Free Software Foundation, Inc.
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
;; Latest version: <http://angg.twu.net/eev-current/eev-code.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-code.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-code-c-d-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-code-c-d-intro)

;;; Commentary:

;; This file defines `code-c-d', that is used to mass-produce
;; short(er) hyperlinks, as explained here:
;;
;;   (find-eev-quick-intro "9. Shorter hyperlinks")
;;   (find-eev-quick-intro "9.1. `code-c-d'")
;;
;; and it also defines `find-code-c-d', that is a debugging function
;; that can be considered as a hyperlink to templated text. Try:
;;
;;   (find-code-c-d "CODE" "/DIR/" :info "INFO")



;; «.alists»		(to "alists")
;; «.code-c-d-pairs»	(to "code-c-d-pairs")
;; «.ee-tail-call2»	(to "ee-tail-call2")
;; «.code-c-d»		(to "code-c-d")
;; «.code-c-d-s»	(to "code-c-d-s")





;;;        _ _     _       
;;;   __ _| (_)___| |_ ___ 
;;;  / _` | | / __| __/ __|
;;; | (_| | | \__ \ |_\__ \
;;;  \__,_|_|_|___/\__|___/
;;;                        
;; «alists» (to ".alists")
;; A simple and flexible implementation of argument lists.
;; Inspired by: (find-node "(cl)Argument Lists")
;;              (find-node "(cl)Argument Lists" "&body")
;;    See also: (find-elnode "Symbol Type" ":" "keyword")
;;              (find-elnode "Constant Variables")

;; Test: (ee-aref '((1 . one) (2 . two) (3 . three)) 2)
;;                              -> two
(defun ee-aref (alist idx)
  "Like `aref', but for alists.
Example: (ee-aref '((1 . one) (2 . two) (3 . three)) 2)
                                -> two"
  (cdr (assoc idx alist)))

;; Test: (ee-adel '((1 . one) (2 . two) (3 . three)) 2)
;;              -> ((1 . one)           (3 . three))
;;
(defun ee-adel (alist idx)
  "Like `remq', but for alists. This is non-destructive, so wrap it in a setq.
Example: (ee-adel '((1 . one) (2 . two) (3 . three)) 2)
                -> ((1 . one)           (3 . three))"
  (remq (assoc idx alist) alist))

;; Test: (ee-aset '((1 . one) (2 . two) (3 . three)) 2 'foo)
;;    -> ((2 . foo) (1 . one)           (3 . three))
;;
(defun ee-aset (alist idx newelt)
  "Like `aset', but for alists. This is non-destructive, so wrap it in a setq.
Example: (ee-aset '((1 . one) (2 . two) (3 . three)) 2 'foo)
      -> ((2 . foo) (1 . one)           (3 . three))"
  (cons (cons idx newelt) (ee-adel alist idx)))

;; Tests: (ee-areplace '((1 . one) (2 . two) (3 . three)) 2 'foo)
;;                   -> ((1 . one) (2 . foo) (3 . three))
;;        (ee-areplace '((1 . one) (2 . two) (3 . three)) 0 'zero)
;;        -> ((0 . zero) (1 . one) (2 . two) (3 . three))
;;
(defun ee-areplace (alist idx newelt)
  "Like `ee-aset', but keeping the order.
Examples: (ee-areplace '((1 . one) (2 . two) (3 . three)) 2 'foo)
                     -> ((1 . one) (2 . foo) (3 . three))
          (ee-areplace '((1 . one) (2 . two) (3 . three)) 0 'zero)
          -> ((0 . zero) (1 . one) (2 . two) (3 . three))"
  (if (ee-aref alist idx)
      (progn (setcdr (assoc idx alist) newelt)
	     alist)
    (cons (cons idx newelt) alist)))



;;;                _                          _                   _          
;;;   ___ ___   __| | ___        ___       __| |      _ __   __ _(_)_ __ ___ 
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |_____| '_ \ / _` | | '__/ __|
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |_____| |_) | (_| | | |  \__ \
;;;  \___\___/ \__,_|\___|      \___|     \__,_|     | .__/ \__,_|_|_|  |___/
;;;                                                  |_|                     
;;
;; «code-c-d-pairs» (to ".code-c-d-pairs")
;; Used by: (find-eev "eev-elinks.el" "ee-code-c-d-filter")

(defvar ee-code-c-d-pairs nil
  "Each (code-c-d C D) call generates an entry (C (ee-expand D)) here.
A new entry with the same C as a previous one will replace the
previous one. This list is maintained by `ee-code-c-d-add-pair' and
is used by some functions in \"eev-insert.el\".")

(defun ee-code-c-d-add-pair (c d)
  (setq ee-code-c-d-pairs (ee-areplace ee-code-c-d-pairs c (list d))))




;;;                  _        _ _                 _ _ 
;;;   ___  ___      | |_ __ _(_) |       ___ __ _| | |
;;;  / _ \/ _ \_____| __/ _` | | |_____ / __/ _` | | |
;;; |  __/  __/_____| || (_| | | |_____| (_| (_| | | |
;;;  \___|\___|      \__\__,_|_|_|      \___\__,_|_|_|
;;;                                                   
;; «ee-tail-call2» (to ".ee-tail-call2")
;; The name "tail call" is misleading - this is recursive,
;; but not a tail call in the usual sense.

(defun ee-tail-call2 (fmt c d rest)
  "An internal function used to support keyword-argument pairs."
  (cond ((null rest) "")
	((keywordp (car rest))
	 (apply (intern (format fmt (car rest)))
		c d (cdr rest)))
	(t (error "Wrong rest: %S" rest))))





;;;                _                          _ 
;;;   ___ ___   __| | ___        ___       __| |
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |
;;;  \___\___/ \__,_|\___|      \___|     \__,_|
;;;                                             
;; «code-c-d» (to ".code-c-d")
;; See: (find-eev-quick-intro "9.1. `code-c-d'")
;; Try: (find-code-c-d "lua51" "~/usrc/lua-5.1.4/")
;;      (find-code-c-d "lua51" "~/usrc/lua-5.1.4/" :anchor)

;; code-c-d: top-level functions
;;
(defun      code-c-d (c d &rest rest)
  "See: (find-code-c-d-intro)
Try this: (find-code-c-d \"CODE\" \"/DIR/\" :info \"INFO\")"
  (ee-code-c-d-add-pair c d)
  (eval (ee-read (apply 'ee-code-c-d c d rest))))
(defun find-code-c-d (c d &rest rest)
  (find-estring-elisp (apply 'ee-code-c-d c d rest)))
(defun   ee-code-c-d (c d &rest rest)
  (if (stringp (car rest))
      (setq rest (cons :info rest)))
  (concat (ee-code-c-d-base c d)
	  (ee-code-c-d-rest c d rest)))

;; Support for extra arguments
;;
(defun   ee-code-c-d-rest (c d rest)
  (ee-tail-call2 "ee-code-c-d-%S" c d rest))
(defun find-code-c-d-rest (c d &rest rest)
  (find-estring-elisp (ee-code-c-d-rest c d rest)))

(defun ee-code-c-d-base (c d)
  (ee-template0 "\
   ;; {(ee-S `(find-code-c-d ,c ,d ,@rest))} 
   ;; {(ee-S `(ee-code-c-d-base ,c ,d))} 
   (setq ee-{c}dir \"{d}\")
   (setq ee-{c}tagsfile \"{d}TAGS\")
   (defun ee-{c}file (str)
     (concat (ee-expand ee-{c}dir) str))
   (defun ee-use-{c}-tags ()
     (setq tags-file-name ee-{c}tagsfile))
   (defun find-{c}file (str &rest pos-spec-list)
     (interactive (list \"\"))
     (ee-use-{c}-tags)
     (apply 'find-fline (ee-{c}file str) pos-spec-list))
   (defun find-{c}tag (str &rest pos-spec-list)
     (ee-use-{c}-tags)
     (apply 'ee-find-tag str pos-spec-list))
   (defun find-{c}sh (command &rest pos-spec-list)
     (apply 'find-sh-at-dir ee-{c}dir command pos-spec-list))
   (defun find-{c}sh0 (command)
     (funcall 'ee-find-xxxsh0 ee-{c}dir command))
   (defun find-{c}sh00 (command)
     (funcall 'ee-find-xxxsh00 ee-{c}dir command))
   (defun find-{c}w3m (furl &rest pos-spec-list)
     (apply 'find-w3m (ee-{c}file furl) pos-spec-list))
   (defun find-{c}grep (grep-command-args &rest pos-spec-list)
     (apply 'ee-find-grep ee-{c}dir grep-command-args pos-spec-list))
   "))

(defun ee-code-c-d-:info (c d info &rest rest)
  (concat (ee-template0 "
   ;; {(ee-S `(ee-code-c-d-:info ,c ,d ,info ,@rest))}
   (defun find-{c}node (page &rest pos-spec-list)
     (interactive (list \"\"))
     (setq ee-info-code \"{c}\")    ;; for M-h M-i
     (setq ee-info-file \"{info}\")    ;; for M-h M-i
     (apply 'find-node (format \"({info})%s\" page) pos-spec-list))
   ") (ee-code-c-d-rest c d rest)))

(defun ee-code-c-d-:linfo (c d manual &rest rest)
  (concat (ee-template0 "
   ;; {(ee-S `(ee-code-c-d-:linfo ,c ,d ,manual ,@rest))}
   (defun find-{c}node (section &rest pos-spec-list)
     (interactive (list \"\"))
     (apply 'ee-find-node ee-{c}dir \"{manual}\" section pos-spec-list))
   ") (ee-code-c-d-rest c d rest)))

(defun ee-code-c-d-:gz (c d &rest rest)
 (concat (ee-template0 "
   ;; {(ee-S `(ee-code-c-d-:gz ,c ,d ,@rest))}
   (defun find-{c}file (str &rest pos-spec-list)
     (interactive (list \"\"))
     (ee-use-{c}-tags)
     (apply 'find-fline-gz (ee-{c}file str) pos-spec-list))
   ") (ee-code-c-d-rest c d rest)))

(defun ee-code-c-d-:anchor (c d &rest rest)
 (concat (ee-template0 "
   ;; {(ee-S `(ee-code-c-d-:anchor ,c ,d ,@rest))}
   (defun find-{c} (str &rest pos-spec-list)
     (apply 'find-anchor (ee-{c}file str) pos-spec-list))
   ") (ee-code-c-d-rest c d rest)))

(defun ee-code-c-d-:wget (c d url &rest rest)
 (concat (ee-template0 "
   ;; {(ee-S `(ee-code-c-d-:wget ,c ,d ,url ,@rest))}
   (defun ee-{c}url (semiurl) (concat \"{url}\" semiurl))
   (defun find-{c}wget (semiurl &rest pos-spec-list)
     (interactive (list \"\"))
     (apply 'find-wget (ee-{c}url semiurl) pos-spec-list))
   ") (ee-code-c-d-rest c d rest)))

(defun ee-code-c-d-:grep (c d &rest rest) (ee-code-c-d-rest c d rest))  ; compat

;; Support functions.
;; Maybe I should rewrite some of them using `ee-at0'...
;;
(defun ee-find-node (dir manual page &rest pos-spec-list)
  (apply 'find-node (format "(%s%s)%s" dir manual page) pos-spec-list))

(defun ee-find-grep (dir grep-command-args &rest pos-spec-list)
  "Example: (ee-find-grep ee-eetcdir \"grep -niH -e tetris *\")
Note: the POS-SPEC-LIST arguments are currently not used."
  (let ((default-directory (ee-expand (or dir default-directory))))
    (grep grep-command-args)))

(defun ee-find-xxxsh (dir command &rest pos-spec-list)
  "Run COMMAND at DIR and display the result. See `code-c-d'."
  (apply 'find-sh (format "cd %s\n%s" dir command) pos-spec-list))

(defun ee-find-xxxsh0 (dir command)
  "Run COMMAND at DIR and return the result. See `code-c-d'."
  (find-sh0 (format "cd %s\n%s" dir command)))

(defun ee-find-xxxsh00 (dir command)
  "Run COMMAND at DIR and return the result. See `code-c-d'."
  (find-sh00 (format "cd %s\n%s" dir command)))

(defun ee-find-tag (tag &rest pos-spec-list)
  (let ((tags-add-tables nil))
    (find-tag tag))
  (ee-goto-rest pos-spec-list))

;; a test
;; (find-estring-elisp (ee-code-c-d-base "@@@" "!!!"))
;; (find-estring-elisp (ee-code-c-d "CCC" "DDD"))



;;;                _                          _     
;;;   ___ ___   __| | ___        ___       __| |___ 
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` / __|
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| \__ \
;;;  \___\___/ \__,_|\___|      \___|     \__,_|___/
;;;                                                 
;; «code-c-d-s» (to ".code-c-d-s")
;; Some default `code-c-d's (debian-centric).

(defun ee-locate-library (fname)
  (if (locate-library fname)
      (file-name-directory (locate-library fname))))
(defvar ee-eev-source-directory
  (ee-locate-library "eev-code.el"))
(defvar ee-emacs-lisp-directory
  (or (ee-locate-library "loadup.el")
      (format "/usr/share/emacs/%d.%d/lisp/"
	      emacs-major-version emacs-minor-version)))
(defvar ee-emacs-leim-directory
  (or (ee-locate-library "leim-list.el")
      (format "/usr/share/emacs/%d.%d/leim/"
	      emacs-major-version emacs-minor-version)))

(code-c-d "e"      ee-emacs-lisp-directory "emacs" :gz) ; (find-enode   "Top")
(code-c-d "el"     ee-emacs-lisp-directory "elisp" :gz) ; (find-elnode  "Top")
(code-c-d "eli"    ee-emacs-lisp-directory "eintr" :gz) ; (find-elinode "Top")
(code-c-d "eleim"  ee-emacs-leim-directory :gz)
(code-c-d "equail" (ee-eleimfile "quail/") :gz)
(code-c-d "eetc"   data-directory :gz)

(code-c-d "eev"    ee-eev-source-directory :anchor)     ; (find-eev "")

;; (find-efile "")
;; (find-equailfile "")
;; (find-equailfile "latin-ltx.el")

(code-c-d "ud"   "/usr/share/doc/" :gz)  ; (find-udfile "bash/")
(code-c-d "vldi" "/var/lib/dpkg/info/")	 ; (find-vldifile "bash.list")



(provide 'eev-code)




;; Local Variables:
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
