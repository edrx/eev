;;; eev-blinks.el -- support for basic hyperlinks in Emacs.  -*- lexical-binding: nil; -*-
;; The basic hyperlinks are the ones that do not depend on templates,
;; and that are not created by `code-c-d' and friends.

;; Copyright (C) 1999-2024 Free Software Foundation, Inc.
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
;; Version:    20241017
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-blinks.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-blinks.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:

;; For the general notion of elisp hyperlinks, see:
;;   (find-eev-quick-intro "3. Elisp hyperlinks")
;; For what are "basic elisp hyperlinks", see:
;;   (find-links-conv-intro "3. Classification")
;;   (find-links-conv-intro "3. Classification" "non-basic")




;; «.eek»			(to "eek")
;; «.ee-goto-position»		(to "ee-goto-position")
;; «.ee-goto-rest»		(to "ee-goto-rest")
;;   «.curved-single-quotes»	(to "curved-single-quotes")
;; «.find-fline»		(to "find-fline")
;; «.find-wottb»		(to "find-wottb")
;; «.find-dbsw»			(to "find-dbsw")
;; «.find-epackages»		(to "find-epackages")
;; «.find-efaces»		(to "find-efaces")
;; «.find-eregionpp»		(to "find-eregionpp")
;; «.find-eoverlayspp»		(to "find-eoverlayspp")
;; «.find-ebuffercontents»	(to "find-ebuffercontents")
;; «.find-ebufferandpos»	(to "find-ebufferandpos")
;; «.find-ebuffer»		(to "find-ebuffer")
;; «.find-eoutput»		(to "find-eoutput")
;; «.find-estring»		(to "find-estring")
;; «.find-epropertize»		(to "find-epropertize")
;; «.find-ehashtable»		(to "find-ehashtable")
;; «.find-estruct»		(to "find-estruct")
;; «.find-sh»			(to "find-sh")
;; «.find-man»			(to "find-man")
;; «.find-man-bug»		(to "find-man-bug")
;; «.find-w3m»			(to "find-w3m")
;; «.find-eww»			(to "find-eww")
;; «.find-Package»		(to "find-Package")
;; «.find-epp»			(to "find-epp")
;; «.find-efunctionpp»		(to "find-efunctionpp")
;; «.find-eloadhistory»		(to "find-eloadhistory")
;; «.find-eloadhistory-for»	(to "find-eloadhistory-for")
;; «.find-lgreps»		(to "find-lgreps")
;; «.find-einternals»		(to "find-einternals")
;; «.find-einsert»		(to "find-einsert")
;; «.find-eunicode»		(to "find-eunicode")
;;   «.ee-symbol-function»	(to "ee-symbol-function")
;; «.find-eejumps»		(to "find-eejumps")
;; «.find-eeshortdefs»		(to "find-eeshortdefs")
;; «.find-eaproposf»		(to "find-eaproposf")



(defvar ee-buffer-name nil)		; overridden by `let's

;; (find-efunction 'ee-find-tag)




;;;              _        _                 _     
;;;   __ _ _   _| |_ ___ | | ___   __ _  __| |___ 
;;;  / _` | | | | __/ _ \| |/ _ \ / _` |/ _` / __|
;;; | (_| | |_| | || (_) | | (_) | (_| | (_| \__ \
;;;  \__,_|\__,_|\__\___/|_|\___/ \__,_|\__,_|___/
;;;                                               
;;; autoloads for external functions
;; (find-elnode "Autoload")
;;
(autoload 'find-function-read "find-func")
(autoload 'pp-to-string "pp")
(autoload 'Man-translate-references "man")
(autoload 'Man-fontify-manpage      "man" nil t)
(autoload 'word-at-point "thingatpt")
(autoload 'list-iso-charset-chars     "mule-diag")
(autoload 'list-non-iso-charset-chars "mule-diag")
(autoload 'customize-read-group      "cus-edit")
(autoload 'custom-unlispify-tag-name "cus-edit")


;;;            _    
;;;   ___  ___| | __
;;;  / _ \/ _ \ |/ /
;;; |  __/  __/   < 
;;;  \___|\___|_|\_\
;;;                 
;; «eek» (to ".eek")
;; See: (find-eev-quick-intro "3. Elisp hyperlinks" "eek")
;;      (find-efunctiondescr 'edmacro-mode)
;;      (find-ekbmacro-links)

(defun eek (str)
  "Execute STR as a keyboard macro. See `edmacro-mode' for the exact format.\n
An example: (eek \"C-x 4 C-h\")"
  (interactive "sKeys: ")
  (execute-kbd-macro (read-kbd-macro str)))

(defun find-eek (str &rest pos-spec-list)
  (eek str)
  (apply 'ee-goto-position pos-spec-list))




;;;                                                _ _     _       
;;;  _ __   ___  ___       ___ _ __   ___  ___    | (_)___| |_ ___ 
;;; | '_ \ / _ \/ __| ___ / __| '_ \ / _ \/ __|___| | / __| __/ __|
;;; | |_) | (_) \__ \|___|\__ \ |_) |  __/ (__|___| | \__ \ |_\__ \
;;; | .__/ \___/|___/     |___/ .__/ \___|\___|   |_|_|___/\__|___/
;;; |_|                       |_|                                   
;;;
;; «ee-goto-position»  (to ".ee-goto-position")
;; Support for pos-spec-lists in hyperlinks.
;; See: (find-refining-intro "1. Pos-spec-lists")
;;      (find-refining-intro "2. Refining hyperlinks")

(defun ee-goto-position (&optional pos-spec &rest rest)
  "Process the \"absolute pos-spec-lists\" arguments in hyperlink functions.
POS-SPEC, the first element of a pos-spec-list, is treated
specially; if it is a string then jump to the first occurrence of
that string in the buffer, and if it a number jump to the line
with that number in the buffer; if it is nil, do nothing.

The rest of the pos-spec-list, REST, is treated by
`ee-goto-rest'.

Many kinds of hyperlinks - for example,

  (find-efunction 'ee-goto-position)

already jump to specific positions of a buffer; those hyperlink
functions support \"relative pos-spec-lists\", and they invoke
`ee-goto-rest' straight away to handle their pos-spec-lists -
they skip the first \"absolute\" pos-spec."
  (when pos-spec
    (cond ((numberp pos-spec)
	   (goto-char (point-min))
	   (forward-line (1- pos-spec)))
	  ((stringp pos-spec)
	   (goto-char (save-excursion	          ; This used to be just:
			(goto-char (point-min))	  ; (goto-char (point-min))
			(search-forward pos-spec) ; (search-forward pos-spec)
			(point))))		  ;
	  ((eq pos-spec :end)
	   (goto-char (point-max)))
	  (t (error "This is not a valid pos-spec: %S" pos-spec)))
    (if rest (ee-goto-rest rest))))

;; «ee-goto-rest»  (to ".ee-goto-rest")
;; See: (find-refining-intro "1. Pos-spec-lists")
;;
(defun ee-goto-rest (list)
  "Process \"relative pos-spec-lists\".
For each element in LIST, if it is:

  a string -> jump to the next occurrence of that string in the
              current buffer
  a number -> go down that many lines
  a list   -> evaluate the list (take care!)

anything else generates an error - but users are encouraged to
create their own extended versions of this function and override
the standard definition."
  (cond ((null list))
	((stringp (car list))
	 (search-forward (car list))
	 (ee-goto-rest (cdr list)))
	((numberp (car list))
	 (forward-line (car list))
	 (ee-goto-rest (cdr list)))
	((consp (car list))
	 (eval (car list))
	 (ee-goto-rest (cdr list)))
	(t (error "Not a valid pos-spec item: %S" (car list)))))


;; «curved-single-quotes»  (to ".curved-single-quotes")
;; In some situations Emacs converts "`foo'"s to "‘foo’"s, or
;; vice-versa, in info pages and docstrings; see:
;;
;;   (find-elnode "Keys in Documentation" "User Option: text-quoting-style")
;;   (find-elnode "Text Quoting Style"    "User Option: text-quoting-style")
;;   (find-elnode "Text Quoting Style" "curved single quotes")
;;   (find-elnode "Text Quoting Style" "grave accent and apostrophe")
;;   (find-efunction      'eejump "find-eejumps")
;;   (find-efunctiondescr 'eejump "find-eejumps")
;;
;; Eev doesn't have a way to convert strings in pos-spec-lists between
;; these styles yet; at this moment `ee-goto-position' and
;; `ee-goto-rest' do the simplest thing possible - they search for
;; their string arguments literally with `search-forward'.





;;;   __ _           _        __ _ _            
;;;  / _(_)_ __   __| |      / _| (_)_ __   ___ 
;;; | |_| | '_ \ / _` |_____| |_| | | '_ \ / _ \
;;; |  _| | | | | (_| |_____|  _| | | | | |  __/
;;; |_| |_|_| |_|\__,_|     |_| |_|_|_| |_|\___|
;;;   __ _           _                       _      
;;;  / _(_)_ __   __| |      _ __   ___   __| | ___ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _ \ / _` |/ _ \
;;; |  _| | | | | (_| |_____| | | | (_) | (_| |  __/
;;; |_| |_|_| |_|\__,_|     |_| |_|\___/ \__,_|\___|
;;;                                                 
;; «find-fline» (to ".find-fline")
;; Basic links: `find-fline' and `find-node'.
;; See: (find-eev-quick-intro "3. Elisp hyperlinks")
;;      (find-eev-quick-intro "9.2. Extra arguments to `code-c-d'" ":gz")

(defun find-fline (fname &rest pos-spec-list)
  "Hyperlink to a file (or a directory).
This function is similar to `find-file' but it supports a
\"pos-spec-list\" - see `ee-goto-position'.
Examples:\n
  (find-file  \"~/.emacs\")
  (find-fline \"~/.emacs\")
  (find-fline \"~/.emacs\" \"Beginning of the eev block\")"
  (find-file (ee-expand fname))
  (apply 'ee-goto-position pos-spec-list))

(defun find-fline-gz (fname &rest pos-spec-list)
"Like `find-fline', but also tries \"FNAME.gz\" if \"FNAME\" does not exist."
  (let* ((efname (ee-expand fname))
	 (efnamegz (concat efname ".gz")))
    (if (and (not (file-exists-p efname))
	     (file-exists-p      efnamegz))
	(apply 'find-fline efnamegz pos-spec-list)
      (apply   'find-fline efname   pos-spec-list))))

(defun find-node (nodestr &rest pos-spec-list)
  "Hyperlink to an info page.
This function is similar to `info' but it supports a
\"pos-spec-list\" - see `ee-goto-position'.
Examples:\n
  (info \"(emacs)Lisp Eval\")
  (find-node \"(emacs)Lisp Eval\" \"C-x C-e\")"
  (info nodestr)
  (ee-goto-rest pos-spec-list))





;;;   __ _           _                    _   _   _     
;;;  / _(_)_ __   __| |    __      _____ | |_| |_| |__  
;;; | |_| | '_ \ / _` |____\ \ /\ / / _ \| __| __| '_ \ 
;;; |  _| | | | | (_| |_____\ V  V / (_) | |_| |_| |_) |
;;; |_| |_|_| |_|\__,_|      \_/\_/ \___/ \__|\__|_.__/ 
;;;
;; «find-wottb» (to ".find-wottb")
;; Hyperlinks to the output of Emacs's help-like functions.

(defun find-wottb-call (sexp bufname &rest pos-spec-list)
  "Hyperlink to functions that call `with-output-to-temp-buffer'.
First evaluate SEXP with a trick to not let it split the current window,
then switch to the buffer that it created (it must be called BUFNAME),
then go to the position specified by POS-SPEC-LIST.\n
\(This is a horrible hack.)"
  (let ((same-window-buffer-names
	 (cons bufname same-window-buffer-names)))
    (eval sexp))
  (set-buffer bufname)			; why is this needed?
  (apply 'ee-goto-position pos-spec-list))


;; Tests: (find-eapropos "regexp")
;;        (find-eapropos "^find-.*-links" "find-intro-links")
;;
(defun find-eapropos (regexp &rest pos-spec-list)
  "Hyperlink to the result of running `apropos' on REGEXP."
  (interactive "sApropos symbol (regexp): ")
  (apply 'find-wottb-call '(apropos regexp) "*Apropos*" pos-spec-list))

;; Tests: (find-efunctiondescr 'find-file)
;;        (find-efunctiondescr 'next-line "line-move-visual")
;;
(defun find-efunctiondescr (symbol &rest pos-spec-list)
  "Hyperlink to the result of running `describe-function' on SYMBOL."
  (interactive (find-function-read))
  (apply 'find-wottb-call '(describe-function symbol) "*Help*" pos-spec-list))

;; Tests: (find-evardescr 'default-directory)
;;        (find-evariabledescr 'line-move-visual "visual lines")
;;
(defun find-evariabledescr (symbol &rest pos-spec-list)
  "Hyperlink to the result of running `describe-variable' on SYMBOL."
  (interactive (find-function-read 'variable))
  (apply 'find-wottb-call '(describe-variable symbol) "*Help*" pos-spec-list))

(defalias 'find-evardescr 'find-evariabledescr)

;; Tests: (find-ekeydescr "\M-h\M-h")
;;        (find-ekeydescr [down] "line-move-visual")
;;
(defun find-ekeydescr (key &rest pos-spec-list)
  "Hyperlink to the result of running `describe-key' on KEY."
  (interactive "kFind function on key: ")
  (apply 'find-wottb-call '(describe-key key) "*Help*" pos-spec-list))

;; Tests: (find-efunctiond 'find-file)
;;        (find-efunctiond 'next-line "next-line-add-newlines")
;;
(defun find-efunctiond (function &rest pos-spec-list)
  "Hyperlink to the result of running `disassemble' on FUNCTION."
  (interactive (find-function-read))
  (apply 'find-wottb-call '(disassemble function) "*Disassemble*"
	 pos-spec-list))

;; Tests: (find-customizegroup    'processes)
;;        (find-customizeoption   'fill-column)
;;        (find-customizevariable 'fill-column)
;;        (find-customizeapropos  "subed")
;;        (find-customizeface     'font-lock-comment-face)
;;
(defun find-customizegroup (group &rest pos-spec-list)
  "Hyperlink to the result of running `customize-group' on GROUP."
  (interactive (list (customize-read-group)))
  (when (stringp group)
    (if (string-equal "" group)
	(setq group 'emacs)
      (setq group (intern group))))
  (apply 'find-wottb-call '(customize-group group)
	 (format "*Customize Group: %s*" (custom-unlispify-tag-name group))
	 pos-spec-list))

(defun find-customizeoption (symbol &rest rest)
  "Hyperlink to the result of running `customize-option' on SYMBOL."
  (interactive (custom-variable-prompt))
  (apply 'find-dbsw-call `(customize-option ',symbol) rest))

(defun find-customizevariable (symbol &rest rest)
  "Hyperlink to the result of running `customize-variable' on SYMBOL."
  (interactive (custom-variable-prompt))
  (apply 'find-dbsw-call `(customize-variable ',symbol) rest))

(defun find-customizeapropos (pattern &optional type &rest rest)
  "Hyperlink to the result of running `customize-apropos' on PATTERN."
  (interactive (list (apropos-read-pattern "symbol") nil))
  (apply 'find-dbsw-call `(customize-apropos pattern type) rest))

(defun find-customizeface (face &rest rest)
  "Hyperlink to the result of running `customize-face' on PATTERN."
  (interactive (list (read-face-name
		      "Customize face"
                      (or (face-at-point t t) "all faces") t)))
  (apply 'find-dbsw-call `(customize-face face) rest))

;; Tests: (find-epackages nil "\n  0x0 ")
;;        (find-epackages t   "\n  0x0 ")
;;
(defun find-epackages0 (&optional no-fetch &rest pos-spec-list)
  "Hyperlink to the output of `list-packages'.
This is similar to `find-epackages', but it is much simpler and
far less convenient. The suffix `0' means \"low-level\"."
  (interactive "P")
  (apply 'find-wottb-call '(list-packages no-fetch)
	 "*Packages*" pos-spec-list))

;; Test: (find-epackage '0x0)
;; Note: `M-x find-epackage' currently doesn't work well.
;;  See: (find-elnode "Interactive Codes" "S" "An interned symbol")
;;       (find-elnode "Index" "* read-no-blanks-input:")
;;
(defun find-epackage (&optional pkg-desc &rest pos-spec-list)
  "Hyperlink to the output of `describe-package'."
  ;; (interactive "Spackage name: ")
  (interactive (list (intern (read-no-blanks-input "Package name: " ""))))
  (apply 'find-wottb-call '(describe-package pkg-desc)
	 "*Help*" pos-spec-list))

;; Test: (find-eshortdoc 'keymaps)
;;  See: (find-efunction 'shortdoc-display-group)
;; TODO: (autoload 'shortdoc-display-group "shortdoc")
;; TODO: make the autoload work in the interactive clause.
;;
(defun find-eshortdoc (group &rest rest)
  "Hyperlink to the output of `shortdoc-display-group'."
  (interactive (list (completing-read "Show summary for functions in: "
                                      (mapcar #'car shortdoc--groups))))
  (require 'shortdoc)
  (apply 'find-dbsw-call `(shortdoc-display-group ',group) rest))



;;;   __ _           _           _ _                  
;;;  / _(_)_ __   __| |       __| | |__  _____      __
;;; | |_| | '_ \ / _` |_____ / _` | '_ \/ __\ \ /\ / /
;;; |  _| | | | | (_| |_____| (_| | |_) \__ \\ V  V / 
;;; |_| |_|_| |_|\__,_|      \__,_|_.__/|___/ \_/\_/  
;;;                                                   
;; «find-dbsw»  (to ".find-dbsw")
;; See: https://lists.gnu.org/archive/html/help-gnu-emacs/2022-03/msg00354.html
;; Thanks to Emanuel Berg for help with this!
;;
(defun find-dbsw-call (sexp &rest pos-spec-list)
  "Run SEXP in \"display-buffer-same-window mode\" and go to POS-SPEC-LIST.
This is similar to `find-wottb-call' but uses another method to
force using the same window. This is an experimental hack and may
change soon."
  (let ((display-buffer-overriding-action '(display-buffer-same-window)))
    (eval sexp))
  (apply 'ee-goto-position pos-spec-list))



;;;   __ _           _                             _                         
;;;  / _(_)_ __   __| |       ___ _ __   __ _  ___| | ____ _  __ _  ___  ___ 
;;; | |_| | '_ \ / _` |_____ / _ \ '_ \ / _` |/ __| |/ / _` |/ _` |/ _ \/ __|
;;; |  _| | | | | (_| |_____|  __/ |_) | (_| | (__|   < (_| | (_| |  __/\__ \
;;; |_| |_|_| |_|\__,_|      \___| .__/ \__,_|\___|_|\_\__,_|\__, |\___||___/
;;;                              |_|                         |___/           
;;
;; «find-epackages»  (to ".find-epackages")
;; Tests: (find-epackages)
;;        (find-epackages 'eev)

(defun find-epackages (&rest pos-spec-list)
  "Hyperlink to the output of `list-packages'.
This is similar to `find-epackages0', but uses these three hacks:
  1. if a buffer called \"*Packages*\" exists, just switch to it,
  2. if it doesn't exist, create it with (list-packages 'no-fetch),
  3. use `ee-goto-position-package' instead of `ee-goto-position'."
  (interactive "P")
  (if (get-buffer "*Packages*")
      (find-ebuffer "*Packages*")
    (find-wottb-call
     '(list-packages 'no-fetch)
     "*Packages*"))
  (apply 'ee-goto-position-package pos-spec-list))

(defun ee-goto-position-package (&optional pkgsymbol &rest rest)
  "Like `ee-goto-position', but treats PKGSYMBOL as a package name.
This is an internal function used by `find-epackages'."
  (if (and pkgsymbol (symbolp pkgsymbol))
      (let ((nline (ee-packages-nline-for pkgsymbol)))
	(if (not nline) (error "Package not found"))
	(apply 'ee-goto-position nline rest))
    (apply 'ee-goto-position pkgsymbol rest)))

(defun ee-packages-nline-for (pkgsymbol &optional nns)
  "This is an internal function that only works in the *Packages* buffer."
  (car (reverse (ee-packages-nlines-for pkgsymbol nns))))

(defun ee-packages-nlines-for (pkgsymbol &optional nns)
  "This is an internal function that only works in the *Packages* buffer."
  (setq nns (or nns (ee-packages-nlines-and-names)))
  (cl-loop for (nline name) in nns
	   if (eq name pkgsymbol)
	   collect nline))

(defun ee-packages-nlines-and-names (&optional tles)
  "This is an internal function that only works in the *Packages* buffer."
  (cl-loop for nline from 1
	   for cols in (or tles tabulated-list-entries)
	   collect (list nline (package-desc-name (car cols)))))






;;;   __                     
;;;  / _| __ _  ___ ___  ___ 
;;; | |_ / _` |/ __/ _ \/ __|
;;; |  _| (_| | (_|  __/\__ \
;;; |_|  \__,_|\___\___||___/
;;;                          
;; «find-efaces» (to ".find-efaces")
;; Inspect faces, colors and characters.
;; Most of these use `find-wottb-call'.
;; Tests:
;;   (find-ecolors     " white")
;;   (find-efaces       'eepitch-star-face   "abcd")
;;   (find-efaces     "\neepitch-star-face " "abcd")
;;   (find-efacedescr   'default "Foreground:")
;;
;; Key bindings:
;;   (find-eev "eev-mode.el" "eev-mode-map-set")
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "M-h" "M-c" "find-echardescr")
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "M-h" "M-s" "find-eface-links")
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "M-h" "M-t" "find-etpat")
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "M-h"   "t" "find-etpat0")
;;   (find-eev "eev-mode.el" "eev-mode-map-set" "M-h"   "c" "find-ecolor-links")
;;     (find-enode "International Chars" "C-u C-x =" "describe-char")

(defun find-echardescr (&optional pos &rest pos-spec-list)
  "Hyperlink to the result of running `describe-char' at POS."
  (interactive)
  (setq pos (or pos (point)))
  (apply 'find-wottb-call '(describe-char pos) "*Help*" pos-spec-list))

(defun find-ecolors (&rest pos-spec-list)
  "Hyperlink to the result of running `list-colors-display'."
  (interactive)
  (apply 'find-wottb-call '(list-colors-display) "*Colors*" pos-spec-list))

;; See:
;;   (find-eev "eev-elinks.el" "find-eface-links")
;;   (find-eev "eev-elinks.el" "find-eface-links" "find-efacedescr")
;;
(defun find-efacedescr (&optional face &rest pos-spec-list)
  "Hyperlink to the result of running `describe-face' on FACE.
When called interactively use `ee-face-at-point' to select the
FACE. See the documentation for `ee-face-at-point' for the
details of how it works."
  (interactive (list (ee-face-at-point current-prefix-arg)))
  (apply 'find-wottb-call '(describe-face face) "*Help*" pos-spec-list))

(defun find-efaces (&rest pos-spec-list)
  "Hyperlink to the result of running `list-faces-display'."
  (interactive)
  (apply 'find-wottb-call '(list-faces-display)
	 "*Faces*" (ee-find-efaces-hack pos-spec-list)))

(defun ee-find-efaces-hack (pos-spec-list)
  "An internal function used by `find-efaces'."
  (let* ((face (car pos-spec-list))
	 (rest (cdr pos-spec-list)))
    (if (and face (symbolp face))	  ; if pos-spec-list starts with a symbol:
	(cons (format "\n%s " face) rest) ; convert it to a string in the right way
      pos-spec-list)))			  ; otherwise return pos-spec-list unchanged

(defun find-etpat (&optional pos &rest pos-spec-list)
  "Hyperlink to the result of running `describe-text-properties' at point.
See `find-etpat0' and `find-etpat00' for lower-level tools for
inspecting text proprties."
  (interactive)
  (setq pos (or pos (point)))
  (apply 'find-dbsw-call '(describe-text-properties pos)
	 pos-spec-list))

(defun find-etpat0 (&rest pos-spec-list)
"Hyperlink to a pretty version of the result of (text-properties-at (point))."
  (interactive)
  (let* ((ee-buffer-name
	  (or ee-buffer-name "*(text-properties-at (point))*")))
    (apply 'find-epp (text-properties-at (point)) pos-spec-list)))

(defun find-etpat00 ()
  "Show the result of (text-properties-at (point)) in the echo area."
  (interactive)
  (find-epp0 (text-properties-at (point))))

(defalias 'find-etp 'find-etpat)




;;;   __ _           _                           _                         
;;;  / _(_)_ __   __| |       ___ _ __ ___  __ _(_) ___  _ __  _ __  _ __  
;;; | |_| | '_ \ / _` |_____ / _ \ '__/ _ \/ _` | |/ _ \| '_ \| '_ \| '_ \ 
;;; |  _| | | | | (_| |_____|  __/ | |  __/ (_| | | (_) | | | | |_) | |_) |
;;; |_| |_|_| |_|\__,_|      \___|_|  \___|\__, |_|\___/|_| |_| .__/| .__/ 
;;;                                        |___/              |_|   |_|    
;;
;; «find-eregionpp»  (to ".find-eregionpp")
;; The functions `find-etpat', `find-etpat0' and `find-etpat00'
;; defined above can be used to the examine the text properties of
;; characters in a buffer; `find-eregionpp' can be used to examine all
;; text properties in a region.

;; Test:
;; (eek "2*<down> C-a C-SPC <down> C-x 1 C-x 3 C-x o <<find-eregionpp>>")
;;
(defun find-eregionpp (b e &optional arg)
  "Show the text properties of the text in the region.
This function pretty-prints the result of `(buffer-substring B
E)' and shows the result in a temporary buffer; B and E are the
extremities of the region. The argument ARG is passed to
`ee-eregionpp-preprocess', that uses it to decide which text
properties to omit."
  (interactive "r\nP")
  (let* ((ee-buffer-name (or ee-buffer-name "*(find-eregionpp)*"))
	 (str (buffer-substring b e))
	 (intervals0 (ee-string-intervals str))
	 (intervals (ee-eregionpp-preprocess intervals0 arg)))
    (find-epp intervals)))

;; See: https://lists.gnu.org/archive/html/help-gnu-emacs/2022-10/msg00729.html
;;      https://lists.gnu.org/archive/html/help-gnu-emacs/2022-10/msg00745.html
(defun ee-string-intervals (str)
  "This is similar to `object-intervals', but uses another output format."
  (cl-loop for (b e props) in (object-intervals str)
	   for s = (substring-no-properties str b e)
	   for pairs = (cl-loop for (x y) on props by 'cddr
				collect (list x y))
	   collect (cons s (ee-sort-pairs pairs))))

(defun ee-symbol< (symbol1 symbol2)
  (string< (symbol-name symbol1) (symbol-name symbol2)))

;; Tests: (find-eppp global-minor-modes)
;;        (find-eppp (ee-sort-symbols global-minor-modes))
(defun ee-sort-symbols (symbols)
  (sort symbols 'ee-symbol<))

(defun ee-sort-pairs (pairs)
  "Sort a list of PAIRS of the the form (symbol value)."
  (let ((pair< (lambda (pair1 pair2)
                 (string< (symbol-name (car pair1))
                          (symbol-name (car pair2))))))
    (sort pairs pair<)))

;; This is a stub!
(defun ee-eregionpp-preprocess (intervals0 arg)
  "Redefine this if you want ways to omit certain properties."
  intervals0)

;; Test:
;; (eek "2*<down> C-a C-SPC <down> C-x 1 C-x 3 C-x o <<find-eregionpp0>>")
;;
(defun find-eregionpp0 (b e &optional use-print-circle)
  "Show the text properties of the text in the region.
This function pretty-prints the result of `(buffer-substring B
E)' and shows the result in a temporary buffer; B and E are the
extremities of the region. If this function is called with a
numeric argument, as in `C-u M-x find-eregionpp0', then the
pretty-printer is run with the flag `print-circle' turned on, and
this makes the pretty-printer display copies of shared subobjects
using a more compact notation. See:
  (find-elnode \"Circular Objects\")"
  (interactive "r\nP")
  (let ((ee-buffer-name (or ee-buffer-name "*(find-eregionpp0)*"))
	(print-circle use-print-circle))
    (find-epp (buffer-substring b e))))

;; «find-eoverlayspp»  (to ".find-eoverlayspp")
(defun find-eoverlayspp ()
  "Show the overlays at point."
  (interactive)
  (find-epp (mapcar 'ee-overlay-pp0 (overlays-at (point) 'sorted))))

(defun ee-overlay-pp0 (ovl)
  (let* ((props (overlay-properties ovl)))
    (cons ovl (cl-loop for (a b) on props by 'cddr
		       collect (list a b)))))

;; «find-ebuffercontents»  (to ".find-ebuffercontents")
;; One way to inspect text properties in buffers with weird keymaps is
;; to copy the contents of these buffers to other buffers that are in
;; fundamental mode, then edit the copy to leave only the parts that
;; we want to inspect, and then use `find-eregionpp'.

;; Tests:
;; (find-wset "1_3o_o"   nil              '(find-ebuffercontents))
;; (find-wset "1_3o_o" '(find-eevfile "") '(find-ebuffercontents))
;;
(defun find-ebuffercontents (&optional b &rest pos-spec-list)
  "Show a copy of `(ee-buffer-contents B)' in a buffer in fundamental mode.
This function does not copy overlays.
BUG: at this moment invisible text is not copied. I need to fix that!!!"
  (interactive (list (current-buffer)))
  (let ((ee-buffer-name (or ee-buffer-name "*(find-ebuffercontents)*")))
    (apply 'find-estring (ee-buffer-contents b) pos-spec-list)))

(defun ee-buffer-contents (&optional b)
  "Like `ee-buffer-contents0', but uses the current buffer if B is nil.
If B neither nil nor an existing buffer this function returns the
empty string instead of throwing an error."
  (setq b (or b (current-buffer)))
  (if (get-buffer b) (ee-buffer-contents0 b) ""))

(defun ee-buffer-contents0 (b)
  "Return the contents of the buffer B. 
B must be a buffer or the name of an existing buffer. If the
buffer B is narrowed this function returns only its accessible
portion."
  (with-current-buffer b
    (buffer-substring (point-min) (point-max))))






;;;   __ _           _            __                  _   _             
;;;  / _(_)_ __   __| |      ___ / _|_   _ _ __   ___| |_(_) ___  _ __  
;;; | |_| | '_ \ / _` |____ / _ \ |_| | | | '_ \ / __| __| |/ _ \| '_ \ 
;;; |  _| | | | | (_| |____|  __/  _| |_| | | | | (__| |_| | (_) | | | |
;;; |_| |_|_| |_|\__,_|     \___|_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|
;;;                                                                      
;; «find-ebufferandpos» (to ".find-ebufferandpos")
;; Hyperlinks to the source code of Emacs functions and variables.
;; Tests:
;;   (find-efunction 'next-line)
;;   (find-evariable 'line-move-visual)

(defun find-ebufferandpos (buffer-and-pos &rest pos-spec-list)
  "Internal use; hyperlink to a \"buffer and pos\" structure.
Emacs has some standard (i.e., non-eev) functions that can be
used as hyperlinks, like `find-function' and `find-variable';
they call internal functions like `find-function-noselect' and
`find-variable-noselect', that return structures of the form
BUFFER-AND-POS, that are conses like (#<buffer foo> . 42). This
function jumps to the position described by a cons like that, and
then processes an optional relative POS-SPEC-LIST using
`ee-goto-rest'.

Functions like `find-efunction' and `find-evariable' (defined in
eev.el) are wrappers around `find-function' and `find-variable'
that add support for a relative pos-spec-list after the symbol."
  (if (not (bufferp (car buffer-and-pos)))
      (error "Bad (BUFFER . POS): %S" buffer-and-pos))
  (switch-to-buffer (car buffer-and-pos))
  (goto-char (cdr buffer-and-pos))
  (ee-goto-rest pos-spec-list))

(defun find-efunction (symbol &rest pos-spec-list)
  "Hyperlink to the result of running `find-function' on SYMBOL.
The `find-function' function of Emacs can be used as a hyperlink
- it finds the Elisp source code of SYMBOL -, but it doesn't
support a POS-SPEC-LIST like this function does."
  (interactive (find-function-read))
  (apply 'find-ebufferandpos (find-function-noselect symbol) pos-spec-list))

(defun find-ealias (symbol &rest pos-spec-list)
  "Like `find-efunction', but do not resolve aliases to functions.
See: https://lists.gnu.org/archive/html/help-gnu-emacs/2022-01/msg00323.html"
  (interactive (find-function-read))
  (apply 'find-ebufferandpos
         (find-function-search-for-symbol
          symbol nil (symbol-file symbol 'defun))
         pos-spec-list))

(defun find-evariable (symbol &rest pos-spec-list)
  "Hyperlink to the result of running `find-variable' on SYMBOL."
  (interactive (find-function-read 'variable))
  (apply 'find-ebufferandpos (find-variable-noselect symbol) pos-spec-list))

(defun find-eface (face &rest pos-spec-list)
  "Hyperlink to the result of running `find-face-definition' on FACE.
The `find-face-definition' function of Emacs can be used as a hyperlink
- it finds the Elisp source code of SYMBOL -, but it doesn't
support a POS-SPEC-LIST like this function does."
  (interactive (find-function-read 'defface))
  (apply 'find-ebufferandpos (find-definition-noselect face 'defface)
	 pos-spec-list))






;;;   __ _           _            _            __  __           
;;;  / _(_)_ __   __| |       ___| |__  _   _ / _|/ _| ___ _ __ 
;;; | |_| | '_ \ / _` |_____ / _ \ '_ \| | | | |_| |_ / _ \ '__|
;;; |  _| | | | | (_| |_____|  __/ |_) | |_| |  _|  _|  __/ |   
;;; |_| |_|_| |_|\__,_|      \___|_.__/ \__,_|_| |_|  \___|_|   
;;;                                                             
;; «find-ebuffer» (to ".find-ebuffer")
;; Hyperlinks to buffers
;; Tests:
;;   (find-ebuffer "*Messages*")

(defun find-ebuffer (buffer &rest pos-spec-list)
  "Hyperlink to an Emacs buffer (existing or not)."
  (interactive "bBuffer: ")
  (switch-to-buffer buffer)
  (apply 'ee-goto-position pos-spec-list))



;;;   __ _           _                        _               _   
;;;  / _(_)_ __   __| |       ___  ___  _   _| |_ _ __  _   _| |_ 
;;; | |_| | '_ \ / _` |_____ / _ \/ _ \| | | | __| '_ \| | | | __|
;;; |  _| | | | | (_| |_____|  __/ (_) | |_| | |_| |_) | |_| | |_ 
;;; |_| |_|_| |_|\__,_|      \___|\___/ \__,_|\__| .__/ \__,_|\__|
;;;                                              |_|              
;; «find-eoutput»  (to ".find-eoutput")
;; Tests:
;;   (find-estring "a\nb\nc\n")
;;   (find-estring-elisp "(dotimes (i 10) (insert \"\\na\"))")

(defun find-eoutput-rerun (buffer-name code &rest pos-spec-list)
  "Hyperlink to the effect of running CODE in Emacs.
If the buffer BUFFER-NAME does not exist then create it and run
CODE in it. If the buffer already exists, then \"run CODE
again\" (compare with `find-output-reuse'): delete the buffer,
recreate it, and run CODE in it.\n
For simplicity we are deleting the buffer and then recreating it,
but it could be better to just delete the buffer's contents. This
needs to be thought out."
  (if (get-buffer buffer-name)		  ; if the buffer exists
      (if (not (kill-buffer buffer-name)) ; try to kill it; confirm if needed
	  (error "Not killing the buffer %s" buffer-name)))
  (switch-to-buffer buffer-name)	  ; create the buffer
  (eval code)				  ; always run CODE on the empty buffer
  (goto-char (point-min))
  (apply 'ee-goto-position pos-spec-list))

(defun find-eoutput-reuse (buffer-name code &rest pos-spec-list)
  "Hyperlink to the effect of running CODE in Emacs.
If the buffer BUFFER-NAME does not exist then create it and run
CODE in it. If the buffer already exists, then \"reuse
it\" (compare with `find-output-rerun'): switch to it, ignore the
CODE argument, and process the POS-SPEC-LIST."
  (if (get-buffer buffer-name)		; if the buffer exists
      (switch-to-buffer buffer-name)	; then just switch to it
    (switch-to-buffer buffer-name)	; otherwise switch to it and
    (eval code)				; run CODE to produce its contents
    (goto-char (point-min)))
  (apply 'ee-goto-position pos-spec-list))

;; «find-estring»  (to ".find-estring")
;; Tests: (find-2a nil '(find-estring       ";; Foo\n(+ 1 2)\n"))
;;        (find-2a nil '(find-estring-elisp ";; Foo\n(+ 1 2)\n"))
;;        (find-estring-2a                  ";; Foo\n(+ 1 2)\n")
;;
(defun find-estring (string &rest pos-spec-list)
  "Visit a temporary buffer whose contents are given by STR.
The default name for the buffer is \"*string*\", but this can be
overridden by setting `ee-buffer-name' to another name with a
`let'. If the buffer already exists its contents are destroyed.
The buffer is not made read-only."
  (apply 'find-eoutput-rerun (or ee-buffer-name "*string*")
	 `(insert ,string) pos-spec-list))

(defun find-estring-elisp (string &rest pos-spec-list)
  "Visit a temporary buffer whose contents are given by STR.
This function is similar to `find-estring', but this one also
runs `emacs-lisp-mode' in the buffer."
  (apply 'find-eoutput-rerun (or ee-buffer-name "*string*")
	 `(progn (insert ,string) (emacs-lisp-mode)) pos-spec-list))

(defun find-estring-2a (str &rest pos-spec-list)
  "Show STR in the window at the right."
  (find-2a nil `(find-estring str ,@pos-spec-list)))




;;;   __ _           _                                            _   _         
;;;  / _(_)_ __   __| |       ___ _ __  _ __ ___  _ __   ___ _ __| |_(_)_______ 
;;; | |_| | '_ \ / _` |_____ / _ \ '_ \| '__/ _ \| '_ \ / _ \ '__| __| |_  / _ \
;;; |  _| | | | | (_| |_____|  __/ |_) | | | (_) | |_) |  __/ |  | |_| |/ /  __/
;;; |_| |_|_| |_|\__,_|      \___| .__/|_|  \___/| .__/ \___|_|   \__|_/___\___|
;;;                              |_|             |_|                            
;;
;; «find-epropertize»  (to ".find-epropertize")
;; See:
;;   (find-elnode "Special Properties" "face")
;;   (find-elnode "Special Properties" "mouse-face")
;;   (find-elnode "Special Properties" "keymap")
;;
;; Tests:
;;   (find-epropertize '(face (:foreground "red")))
;;   (find-epropertize '(face (:foreground "yellow" :background "red")))
;;   (find-epropertize '(face                      (:background "red")))
;;   (find-epropertize '(face font-lock-comment-face))
;;   (find-epropertize '(mouse-face highlight))
;;
;;    (setq       ee-fep-keymap (make-sparse-keymap))
;;    (define-key ee-fep-keymap (kbd "C-c C-c") 'next-line)
;;   (find-epropertize-2b `(keymap ,ee-fep-keymap))
;;   (find-epropertize-2b `(keymap ,ee-fep-keymap mouse-face highlight))
;;
(defun ee-epropertize (textproperties)
  "And internal function used by `find-epropertize'."
  (concat (apply 'propertize "Some text" textproperties) "\n\n"))

(defun find-epropertize (textproperties)
  "Show a string with TEXTPROPERTIES at the window at the right.
TEXTPROPERTIES is a list of the form (prop1 value1 prop2 value2 ...)."
    (find-2a nil `(find-estring ,(ee-epropertize textproperties) 3)))

(defun find-epropertize-2b (textproperties)
  "Show a string with TEXTPROPERTIES at the window at the right.
TEXTPROPERTIES is a list of the form (prop1 value1 prop2 value2 ...).
This is a variant of `find-epropertize' that switches to the window at
the right."
    (find-2b nil `(find-estring ,(ee-epropertize textproperties) 3)))





;; TODO: Delete this.
;; This is an ugly hack.
;; It was created for: (find-efunction 'find-anchors-intro)
;; It was made obsolete by: (find-eev "eev-codings.el")

(defun ee-raw-text-unix ()
  "Set the current buffer to unibyte (for certain glyphs).
See: (find-anchors-intro \"WARNING: some glyphs need raw-text-unix\")"
  (interactive)
  (set-buffer-file-coding-system 'raw-text-unix 'force)
  (set-buffer-multibyte nil))

(defun find-estring-lv (string &rest pos-spec-list)
  "Visit a temporary buffer whose contents are given by STR.
The default name for the buffer is \"*string*\", but this can be
overridden by setting `ee-buffer-name' to another name with a `let'.
If the buffer already exists its contents are destroyed.
The buffer is not made read-only.
The \"Local variables:\" section in the buffer is processed."
  (apply 'find-eoutput-rerun (or ee-buffer-name "*string*")
	 `(progn (ee-raw-text-unix)
		 (insert ,string)
		 (hack-local-variables))
	 pos-spec-list))



;;;  _               _       _        _     _           
;;; | |__   __ _ ___| |__   | |_ __ _| |__ | | ___  ___ 
;;; | '_ \ / _` / __| '_ \  | __/ _` | '_ \| |/ _ \/ __|
;;; | | | | (_| \__ \ | | | | || (_| | |_) | |  __/\__ \
;;; |_| |_|\__,_|___/_| |_|  \__\__,_|_.__/|_|\___||___/
;;;                                                     
;; Test:   (cl-defstruct mypoint x y)
;;       (setq myp (make-mypoint :x 3 :y 4))
;;                               (ee-struct-index-table myp)
;;   (ee-hashtable-to-string nil (ee-struct-index-table myp))
;;              (find-ehashtable (ee-struct-index-table myp))
;;
;; See also: (find-epackage-links 'ht)
;; «find-ehashtable»  (to ".find-ehashtable")

(defun ee-hashtable-to-string (f hashtable)
  "Apply F to each key-value pair of HASHTABLE, and return a big string.
The function F should be a function that expects three arguments
- the key, the value, and the hashtable - and returns either a
line terminated by a newline or an empty string. The lines
returned by F are collected in a list, then sorted, and then the
duplicates are removed; the result after that is concatenated
into a big string, and returned. The key-value pairs for which F
returned an empty string disappear in the concatenation.

If F is nil then use a default function.

I often refer to strings that may have, and usually do have,
newlines, as \"big strings\". This function returns a \"big string\"."
  (setq f (or f (lambda (k v h) (format "%S -> %S\n" k v))))
  (let ((lines (cl-loop for k being the hash-keys of hashtable
	 	        collect (funcall f k (gethash k hashtable) hashtable))))
    (apply 'concat (seq-uniq (sort lines 'string<)))))

(defun find-ehashtable (hashtable &rest pos-spec-list)
  (apply 'find-estring (ee-hashtable-to-string nil hashtable) pos-spec-list))




;;;   __ _           _                 _                   _   
;;;  / _(_)_ __   __| |       ___  ___| |_ _ __ _   _  ___| |_ 
;;; | |_| | '_ \ / _` |_____ / _ \/ __| __| '__| | | |/ __| __|
;;; |  _| | | | | (_| |_____|  __/\__ \ |_| |  | |_| | (__| |_ 
;;; |_| |_|_| |_|\__,_|      \___||___/\__|_|   \__,_|\___|\__|
;;;                                                            
;; «find-estruct»  (to ".find-estruct")
;; Hyperlinks that display human-readable versions of structures
;; created with cl-defstruct. To understand how this works, try this:
;;
;;         (cl-defstruct mytriple a b c)
;;                 (make-mytriple :a 22 :c "44")
;;   (find-estruct (make-mytriple :a 22 :c "44"))
;;
;; The second sexp returns this:
;;
;;   #s(mytriple 22 nil "44")
;;
;; that is difficult to read because it doesn't show the field names.
;; The `find-estruct' sexp above converts that to a multi-line
;; representation that shows the slot numbers and the field names.
;;
;; See: (find-clnode "Structures")
;;      (find-clnode "Structures" "cl-defstruct")
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00177.html
;;
;; Test:
;;
;;   (find-epp (macroexpand ' (cl-defstruct mypoint x y) ))
;;   (find-eppm             ' (cl-defstruct mypoint x y) )
;;  
;;   (cl-defstruct mypoint x y)
;;   (cl-defstruct (mypoint-colored (:include mypoint)) color)
;;  
;;   (setq myp (make-mypoint         :x 3 :y 4))
;;   (setq myp (make-mypoint-colored :x 3 :y 4))
;;   (setq myp (make-mypoint-colored :x 3 :y 4 :color "green"))
;;   
;;                         (ee-struct-class       myp)
;;                         (ee-struct-slot-names  myp)
;;                         (ee-struct-slot-names+ myp)
;;                         (ee-struct-index-table myp)
;;        (find-ehashtable (ee-struct-index-table myp))
;;         (gethash 'x     (ee-struct-index-table myp))
;;         (gethash 'y     (ee-struct-index-table myp))
;;         (gethash 'color (ee-struct-index-table myp))
;;                                                myp
;;                         (ee-struct-to-string   myp)
;;  
;;                                  (find-estruct myp)
;;                 (find-estruct (ee-struct-class myp))
;;
;; WARNING: this is a quick hack.
;; THANKS: to pjb from #emacs.

(defun ee-struct-class (stro)
  (get (type-of stro) 'cl--class))

(defun ee-struct-slot-names (stro)
  (cl-mapcar 'cl--slot-descriptor-name
	     (cl--struct-class-slots (ee-struct-class stro))))

(defun ee-struct-slot-names+ (stro)
  (cons '<type-name-field> (ee-struct-slot-names stro)))

(defun ee-struct-index-table (stro)
  (cl--struct-class-index-table (ee-struct-class stro)))

(defun ee-struct-to-string (stro)
  "Convert the structure object STRO to a multi-line string.
Here is an example. In

  (cl-defstruct mytriple a b c)
  (make-mytriple :a 22 :c \"44\")
    --> #s(mytriple 22 nil \"44\")

the representation `#s(mytriple 22 nil \"44\")' is difficult to
read because is does't have the field names. This function
converts that a series of lines of the form \"slotnumber
fieldname value\", like this:

  (ee-struct-to-string (make-mytriple :a 22 :c \"44\"))
    --> 0 <type-name-field> mytriple
        1 a 22
        2 b nil
        3 c \"44\"
"
  (let* ((ns (number-sequence 0 (length stro)))
	 (fieldnames (ee-struct-slot-names+ stro))
	 (lines (cl-mapcar
		 (lambda (n name o) (format "%d %S %S\n" n name o))
		 ns fieldnames stro)))
    (apply 'concat lines)))

(defun find-estruct (stro &rest pos-spec-list)
  (apply 'find-estring (ee-struct-to-string stro) pos-spec-list))


;; Added in 2022dec15.
;; TODO: refactor and write better docstrings!
;;
(defun ee-struct-to-triples (stro)
  "Like `ee-struct-to-string', but returns a list of triples."
  (let* ((ns (number-sequence 0 (length stro)))
	 (fieldnames (ee-struct-slot-names+ stro))
	 (triples (cl-mapcar
		   (lambda (n name o) (list n name o))
		   ns fieldnames stro)))
    triples))

(defun find-estructt (stro &rest pos-spec-list)
  "Like `find-estructt', but pretty-prints a list of triples."
  (apply 'find-epp (ee-struct-to-triples stro) pos-spec-list))







;;;   __ _           _           _     
;;;  / _(_)_ __   __| |      ___| |__  
;;; | |_| | '_ \ / _` |_____/ __| '_ \ 
;;; |  _| | | | | (_| |_____\__ \ | | |
;;; |_| |_|_| |_|\__,_|     |___/_| |_|
;;;                                    
;; «find-sh»  (to ".find-sh")
;; Hyperlinks to the output of shell commands.
;; See: (find-eev-quick-intro "3. Elisp hyperlinks" "find-sh0")
;; Tests:
;;   (find-sh   "seq 2095 2100")
;;   (find-sh0  "seq 2095 2100")
;;   (find-sh00 "seq 2095 2100")

;; Note: we could have written this using `find-eoutput-reuse'...
;;
(defun find-sh (command &rest pos-spec-list)
  "Hyperlink to the result of running the shell command COMMAND.
If a buffer named COMMAND does not exist then create it and put
there the output or running COMMAND; if a buffer named COMMAND
already exists then reuse it and do not run COMMAND again."
  (interactive "sShell command: ")
  (if (get-buffer command)		; if the buffer already exists
      (switch-to-buffer command)	; then just switch to it
    (switch-to-buffer command)		; otherwise create it
    (insert (shell-command-to-string command)) ; prepare its contents
    (goto-char (point-min)))		; and place point at its beginning
  (apply 'ee-goto-position pos-spec-list))

(defalias 'find-sh00 'shell-command-to-string)

(defun find-sh0 (command)
  "Hyperlink to the result of running the shell command COMMAND.
This function does not create a buffer like `find-sh' does;
instead, it just returns the output of COMMAND as string,
removing a trailing newline from the output if one is found.
Follow a `find-sh0' hyperlink just displays the output of the
COMMAND in the echo area."
  (replace-regexp-in-string "\n\\'" "" (shell-command-to-string command)))

(defun find-sh-at-dir (dir command &rest pos-spec-list)
  "Run COMMAND at DIR and display the result.
This is like `find-sh' but sets the buffer's default-directory to DIR."
  (let ((fullcmd (format "cd %s\n%s" dir command)))
    (prog1 (apply 'find-sh fullcmd pos-spec-list)
           (setq default-directory (ee-expand dir)))))

(defun find-sh-man-fontify (command &rest pos-spec-list)
  "Like `find-sh', but runs `Man-fontify-manpage' on the output."
  (apply 'find-eoutput-reuse
	 command
	 `(progn
	    (insert (shell-command-to-string ,command))
	    (goto-char (point-min))
	    (Man-fontify-manpage))
	 pos-spec-list))



;;;   __ _           _                             
;;;  / _(_)_ __   __| |      _ __ ___   __ _ _ __  
;;; | |_| | '_ \ / _` |_____| '_ ` _ \ / _` | '_ \ 
;;; |  _| | | | | (_| |_____| | | | | | (_| | | | |
;;; |_| |_|_| |_|\__,_|     |_| |_| |_|\__,_|_| |_|
;;;                                                
;; «find-man»  (to ".find-man")
;; Hyperlinks to manpages.
;; Tests:
;;   (find-man "1 cat")
;;   (find-man "bash(1)")
;;   (find-man "bash(1)" "multi-character")

(defvar ee-find-man-flag          nil "See `find-man'.")
(defvar ee-find-man-buffer        nil "See `find-man'.")
(defvar ee-find-man-pos-spec-list nil "See `find-man'.")

(defun find-man (manpage &rest pos-spec-list)
  "Hyperlink to a manpage."
  (interactive (list (ee-manpagename-ask)))
  (setq manpage (Man-translate-references manpage))
  ;;
  ;; Set the variables used by `ee-find-man-goto-pos-spec'.
  (setq ee-find-man-flag t)
  (setq ee-find-man-buffer (concat "*Man " manpage "*"))
  (setq ee-find-man-pos-spec-list pos-spec-list)
  ;;
  ;; See: (find-evardescr 'Man-notify-method "pushy" "current window")
  (let ((Man-notify-method 'pushy))
    ;;
    ;; This call to `man' will run `ee-find-man-goto-pos-spec' after
    ;; the manpage is rendered - because of the `advice-add' below.
    ;; This is a dirty trick!... see:
    ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2020-12/msg01100.html
    ;; https://lists.gnu.org/archive/html/help-gnu-emacs/2020-12/msg01102.html
    (man manpage)))

(defun ee-find-man-goto-pos-spec (&rest rest)
  "An internal function used by `find-man'."
  (when ee-find-man-flag
    (setq ee-find-man-flag nil)
    (with-current-buffer ee-find-man-buffer
      (apply 'ee-goto-position ee-find-man-pos-spec-list))))
    
(advice-add 'Man-bgproc-sentinel :after 'ee-find-man-goto-pos-spec)


;; 2020dec29: all this block was commented out.
;;
;; (defun find-man (manpage &rest pos-spec-list)
;;   "Hyperlink to a manpage."
;;   (interactive (list (ee-manpagename-ask)))
;;   (setq ee-find-man-flag t
;; 	ee-find-man-pos-spec-list pos-spec-list)
;;     (man manpage))
;;
;; ;; See: (find-elnode "Advising Functions")
;; ;;      (find-elnode "Porting old advice")
;; ;;      (find-efunctiondescr 'defadvice)
;; (defadvice Man-notify-when-ready (around find-man (man-buffer) activate)
;;   "After rendering a manpage jump to `ee-find-man-pos-spec-list'."
;;   (if (not ee-find-man-flag)
;;       ad-do-it
;;     (switch-to-buffer man-buffer)
;;     (apply 'ee-goto-position ee-find-man-pos-spec-list)
;;     (setq ee-find-man-flag nil)))
;;
;; «find-man-bug»  (to ".find-man-bug")
;; Note: find-man has an open bug that I did not have time to fix yet...
;; an example:
;;
;;   (find-man "1 git-commit")
;;   (find-man "1 git-commit" "-m <msg>, --message=<msg>")
;;
;; The second find-man link should run the
;;
;;   (ee-goto-position "-m <msg>, --message=<msg>")
;;
;; AFTER the man page gets completely rendered, but the mechanism that
;; delays its execution until the rendering it done it currently broken,
;; and the ee-goto-position runs too soon. A workaround for that is to
;; run the
;;
;;   (find-man "1 git-commit")
;; 
;; to render the manpage, then bury its buffer with M-K, then run this:
;; 
;;   (find-man "1 git-commit" "-m <msg>, --message=<msg>")

;; Missing: find-woman.
;; (find-node "(woman)Top")
;; (woman   "cat")
;; (woman "1 cat")




;;;   __ _           _               _____           
;;;  / _(_)_ __   __| |    __      _|___ / _ __ ___  
;;; | |_| | '_ \ / _` |____\ \ /\ / / |_ \| '_ ` _ \ 
;;; |  _| | | | | (_| |_____\ V  V / ___) | | | | | |
;;; |_| |_|_| |_|\__,_|      \_/\_/ |____/|_| |_| |_|
;;;
;; «find-w3m»  (to ".find-w3m")
;; Hyperlinks to webpages and files in HTML.
;; Needs this: (find-epackage 'w3m)
;; Tests: (find-w3m "http://www.emacswiki.org/")
;;        (find-w3m "http://www.emacswiki.org/" "EmacsNewbie")

(defun find-w3m (url &rest pos-spec-list)
  "Hyperlink to a page in HTML.
Use w3m to render the page as text in an Emacs buffer.
Apply `ee-expand' to URL; this changes URL when it starts with
\"~\" or \"$\". After that if URL starts with \"/\" prepend
\"file://\" to it.

These operations on URL keep \"real urls\" unchanged and convert
several kinds of filenames into urls that w3m can process - but
it doesn't convert relative filenames into urls. See
`expand-file-name'."
  (interactive "Murl: ")
  (require 'w3m)
  (let ((enable-local-variables nil)	; workaround for a w3m-el bug
	(w3m-async-exec nil)
	;; See: http://emacs-w3m.namazu.org/ml/msg10374.html
	(w3m-local-find-file-regexps '(nil . ""))
	(w3m-content-type-alist
	 (append w3m-content-type-alist '(("text/html" "" nil nil)))))
    (w3m (replace-regexp-in-string "^/" "file:///" (ee-expand url))))
  (ee-goto-rest pos-spec-list))



;; «find-eww»  (to ".find-eww")
;; Tests: (find-eww "http://anggtwu.net/")
;;        (find-eww "http://anggtwu.net/" "Welcome")
;;        (find-eww "/tmp/")
;;
(defvar ee-find-eww-search-yes nil)
(defvar ee-find-eww-search-for nil)

(defun find-eww (url &rest pos-spec-list)
  "Hyperlink to a page in HTML. Use eww as the browser.
URL can be either a real URL or a file name.

This function searches for POS-SPEC-LIST in the page, but only
after eww finishes rendering it. This is implemented by a hook:
this function saves the POS-SPEC-LIST in the variable
`ee-find-eww-search-for' and sets the variable
`ee-find-eww-search-yes' to t; `eww' runs the function
`ee-find-eww-search' after rendering the page, and
`ee-find-eww-search' processes these two variables."
  (setq ee-find-eww-search-for pos-spec-list)
  (setq ee-find-eww-search-yes t)
  (eww (ee-find-eww-preprocess-url url)))

(defun ee-find-eww-search ()
  "This function is run after eww finishes rendering a page.
If `ee-find-eww-search-yes' is true it searches for
`ee-find-eww-search-for' and sets `ee-find-eww-search-yes' to
false. These variables are set by `find-eww'."
  (when ee-find-eww-search-yes
    (setq ee-find-eww-search-yes nil)
    (apply 'ee-goto-position ee-find-eww-search-for)))

(add-hook 'eww-after-render-hook 'ee-find-eww-search)

;; Tests: (ee-find-eww-preprocess-url "/")
;;        (ee-find-eww-preprocess-url "/foo")
;;        (ee-find-eww-preprocess-url "C:/Users/")
;;        (ee-find-eww-preprocess-url "C:\\Users\\")
;; See:   (find-efunction 'ee-fname-to-url)
;;        (find-psne-intro "find-eww")
;;
(defun ee-find-eww-preprocess-url (url)
  "An internal function used by `find-eww'."
  (setq url (ee-expand url))
  (setq url (replace-regexp-in-string "\\\\" "/" url))
  (if (string-match "^/" url)
      (concat "file://" url)
    (if (string-match "^[A-Za-z]:/" url) ; experimental, for Windows
	(concat "file:///" url)
      url)))

;; (code-c-d "eww" (ee-elfile "net/") "eww" :gz)
;; (find-ewwfile "" "eww.el")
;; (find-ewwfile "eww.el")
;; (find-ewwnode "")





;;;      _      _     _             
;;;   __| | ___| |__ (_) __ _ _ __  
;;;  / _` |/ _ \ '_ \| |/ _` | '_ \ 
;;; | (_| |  __/ |_) | | (_| | | | |
;;;  \__,_|\___|_.__/|_|\__,_|_| |_|
;;;                                 
;; «find-Package»  (to ".find-Package")
;; Hyperlinks to information about Debian packages.
;; Tests:
;;   (find-status         "bash")
;;   (find-available      "bash")
;;   (find-grep-status    "bash")
;;   (find-grep-available "bash")
;;
;; Note that these links are all equivalent:
;;   (find-status                                    "bash")
;;   (find-Package "/var/lib/dpkg/status"            "bash")
;;   (find-fline   "/var/lib/dpkg/status" "\nPackage: bash\n")

(defun find-Package (fname &optional packagename &rest pos-spec-list)
  "Hyperlink to \"Package: \" anchors in Debian package control files.
See: `find-status', `find-available', (find-man \"grep-dctrl\")"
  (find-fline fname)
  (apply 'ee-goto-position
	 (if packagename (format "\nPackage: %s\n" packagename))
	 pos-spec-list))

(defun find-status (packagename &rest pos-spec-list)
  "Hyperlink to the info about the package PACKAGENAME in /var/lib/dpkg/status.
This is Debian-specific. See `find-Package'."
  (interactive (list (ee-debpkgname-ask)))
  (apply 'find-Package "/var/lib/dpkg/status" packagename pos-spec-list))

(defun find-available (packagename &rest pos-spec-list)
"Hyperlink to the info about the package PACKAGENAME in /var/lib/dpkg/available.
This is Debian-specific. See `find-Package'."
  (interactive (list (ee-debpkgname-ask)))
  (apply 'find-Package "/var/lib/dpkg/available" packagename pos-spec-list))

(defun find-grep-status (grepargs &rest pos-spec-list)
  (interactive "sgrep-status ")
  (apply 'find-sh (concat "grep-status " grepargs) pos-spec-list))

(defun find-grep-available (grepargs &rest pos-spec-list)
  (interactive "sgrep-available ")
  (apply 'find-sh (concat "grep-available " grepargs) pos-spec-list))




;;;   __ _           _                        
;;;  / _(_)_ __   __| |      ___ _ __  _ __  
;;; | |_| | '_ \ / _` |____ / _ \ '_ \| '_ \ 
;;; |  _| | | | | (_| |____|  __/ |_) | |_) |
;;; |_| |_|_| |_|\__,_|     \___| .__/| .__/ 
;;;                             |_|   |_|    
;; «find-epp»  (to ".find-epp")
;; Pretty-priting sexps.
;; "pp0" -> "pretty-print a Lisp object in a very compact way".
;; Tests:
;;   (find-epp '(mapcar (lambda (a) (* a a)) '(2 3 4 5)))
;;   (find-efunctionpp 'find-efunction)
;;   (find-epp  '(1 "2" (3 4)))
;;   (find-eppp '(1 "2" (3 4)))
;;   (find-eppm '(cl-loop for k from 1 do (if (foo) (cl-return (bar)))))

(defun find-epp0 (object)
  "Display a pretty-printed version of OBJECT in the echo area.
This function uses `message' and so it only makes sense to call
it from commands bound to keys, not by sexps that are evaluated
explicitly. Try this: (progn (message \"foo\") \"bar\")"
  (message (ee-pp0 object)))

(defun find-epp (object &rest pos-spec-list)
  "Visit a temporary buffer containing a pretty-printed version of OBJECT."
  (let ((ee-buffer-name (or ee-buffer-name "*pp*")))
    (apply 'find-estring-elisp (pp-to-string object) pos-spec-list)))

(defun find-eppp (object &rest pos-spec-list)
  "Visit a temporary buffer containing a pretty-printed version of OBJECT.
This is a variant of `find-epp' that is more suitable for objects
that `find-epp' would print in a single line."
  (let ((ee-buffer-name (or ee-buffer-name "*pp*")))
    (apply 'find-estring-elisp (ee-ppp0 object) pos-spec-list)))

(defun find-eppm (code &rest pos-spec-list)
  "This is essentially the same as (find-epp (macroexpand CODE))."
  (apply 'find-epp (macroexpand code) pos-spec-list))

(defun find-eppma (code &rest pos-spec-list)
  "This is essentially the same as (find-epp (macroexpand-all CODE))."
  (apply 'find-epp (macroexpand-all code) pos-spec-list))

;; «find-efunctionpp»  (to ".find-efunctionpp")
;; See: (find-elisp-intro "6. Defining functions")
;;      (find-elisp-intro "6. Defining functions" "lambda")
;;      (find-elisp-intro "11. Byte-compiled functions")
;;      (find-elisp-intro "11. Byte-compiled functions" "shows a lambda")
;; Test: (find-efunctionpp 'find-fline)
;;
(defun find-efunctionpp (symbol &rest pos-spec-list)
"Visit a temporary buffer containing the pretty-printed Lisp code for SYMBOL."
  (interactive (find-function-read))
  (let* ((fefpp (format "(find-efunctionpp '%S)" symbol))
	 (ee-buffer-name (or ee-buffer-name (format "*%s*" fefpp)))
	 (body (format ";; %s\n;; See: %S\n;;\n%s\n"
		       fefpp
		       '(find-eev "eev-blinks.el" "find-efunctionpp")
		       (pp-to-string (ee-symbol-function symbol)))))
    (apply 'find-estring-elisp body pos-spec-list)))

;; Test: (find-eppp-with-prefix ";; HELLO\n" '(1 "2" (3 4)))
;;
(defun find-eppp-with-prefix (prefix object &rest pos-spec-list)
  "Like `find-eppp', but puts PREFIX at the beginning of the buffer."
  (let ((ee-buffer-name (or ee-buffer-name "*pp*")))
    (apply 'find-estring-elisp
	   (concat prefix (ee-ppp0 object))
	   pos-spec-list)))




;;;  _                 _       _     _     _                   
;;; | | ___   __ _  __| |     | |__ (_)___| |_ ___  _ __ _   _ 
;;; | |/ _ \ / _` |/ _` |_____| '_ \| / __| __/ _ \| '__| | | |
;;; | | (_) | (_| | (_| |_____| | | | \__ \ || (_) | |  | |_| |
;;; |_|\___/ \__,_|\__,_|     |_| |_|_|___/\__\___/|_|   \__, |
;;;                                                      |___/ 
;; «find-eloadhistory» (to ".find-eloadhistory")
;; Display entries in the `load-history' alist.
;; See: (find-eval-intro "10.2. How `find-efunction' works")
;;      (find-elnode "Where Defined" "load-history")

;; Tests: (find-eloadhistory0 (locate-library "eepitch"))
;;        (find-eloadhistory0 (symbol-file 'eepitch-kill))
(defun find-eloadhistory0 (fname &rest pos-spec-list)
  "Hyperlink to the load-history entry for FNAME.
FNAME must be an absolute file name."
  (apply 'find-epp (assoc fname load-history)
	 pos-spec-list))

;; Tests: (find-eloadhistory "eepitch")
;;        (find-eloadhistory "eepitch" "eepitch-dash")
(defun find-eloadhistory (library &rest pos-spec-list)
  "Hyperlink to the load-history entry for LIBRARY.
LIBRARY is converted to an absolute file name using `locate-library'."
  (apply 'find-epp (assoc (locate-library library) load-history)
	 pos-spec-list))


;; «find-eloadhistory-for»  (to ".find-eloadhistory-for")
;; Tests: (find-eloadhistory-for 'eekla)
;;        (find-eloadhistory-for 'next-line)
;;        (find-eloadhistory-for (symbol-file 'eekla 'defun))
;;
(defun find-eloadhistory-for (f &rest rest)
  "Show the result of `(assoc F load-history)' in a temporary buffer.
If F is a symbol it is converted to a filename with (symbol-file F 'defun)."
  (let* ((fname      (if (symbolp f) (symbol-file f 'defun) f))
	 (fnameel    (ee-file-name-elc-to-el fname))
	 (ee-buffer-name (format "*%S*" `(find-eloadhistory-for ',f))))
    (apply
     'find-elinks-elisp
     `((find-eloadhistory-for ,(ee-add-quote f) ,@rest)
       (find-eloadhistory-links)
       ,(if (symbolp f) `(find-lgreps ',f))
       (find-fline ,fnameel)
       ""
       ,(ee-ppp0 (assoc fname load-history)))
     rest)))



;;;   __ _           _       _                          
;;;  / _(_)_ __   __| |     | | __ _ _ __ ___ _ __  ___ 
;;; | |_| | '_ \ / _` |_____| |/ _` | '__/ _ \ '_ \/ __|
;;; |  _| | | | | (_| |_____| | (_| | | |  __/ |_) \__ \
;;; |_| |_|_| |_|\__,_|     |_|\__, |_|  \___| .__/|___/
;;;                            |___/         |_|        
;;
;; `find-lgreps' is a trick for finding the source code of some
;; functions that were defined by certain macros or by `code-c-d' and
;; friends. For example, these sexps generate temporary buffers with
;; lots `find-efunction's that don't work:
;;
;;   (find-eaproposf "macro")
;;   (find-eaproposf "code-pdf")
;;   (find-eaproposf "^find-.*file")
;;   (find-eaproposf "^br")
;;
;; But compare:
;;
;;   (find-efunction 'brff)
;;   (find-lgreps    'brff)
;;   (find-efunction 'find-efile)
;;   (find-lpgreps   'find-efile "e")
;;   (find-efunction 'cl-struct-p--cmacro)
;;   (find-lgreps    'cl-struct-p--cmacro "cl-struct-p")
;;
;; «find-lgreps»  (to ".find-lgreps")

(defun find-lgreps (f &optional stem)
  "Go to a temporary buffer with links for finding the source code of F."
  (interactive (find-function-read))
  (setq stem (or stem (format "%s" f)))
  (let* ((fname0 (symbol-file f 'defun))
	 (fname  (ee-shorten-file-name (ee-file-name-elc-to-el fname0)))
	 (stem1  (format "\"%s\"" stem))
	 (stem2  (format "[( ']%s$" stem))
	 (stem3  (format "[( ']%s[ )]" stem)))
    (find-estring-elisp
     `(ee-template0 "\
;; (find-lgreps '{f} \"{stem}\")
;; (find-eloadhistory-for '{f})
;; (find-eloadhistory-for '{f} 2 \" {f})\")
;; (find-efunction 'find-lgreps)

(find-efunctionpp '{f})
{(ee-S `(find-lgrep ',f ,stem1))}
{(ee-S `(find-lgrep ',f ,stem2))}
{(ee-S `(find-lgrep ',f ,stem3))}
{(ee-S `(find-lgrep ,fname ,stem1))}
{(ee-S `(find-lgrep ,fname ,stem2))}
{(ee-S `(find-lgrep ,fname ,stem3))}
{(ee-S `(find-fline ,fname))}
"))))



;;;                                   _       _                        _     
;;;   ___ _ __ ___   __ _  ___ ___   (_)_ __ | |_ ___ _ __ _ __   __ _| |___ 
;;;  / _ \ '_ ` _ \ / _` |/ __/ __|  | | '_ \| __/ _ \ '__| '_ \ / _` | / __|
;;; |  __/ | | | | | (_| | (__\__ \  | | | | | ||  __/ |  | | | | (_| | \__ \
;;;  \___|_| |_| |_|\__,_|\___|___/  |_|_| |_|\__\___|_|  |_| |_|\__,_|_|___/
;;;                                                                          
;; «find-einternals»  (to ".find-einternals")
;; Hyperlinks to other things internal to Emacs
;; Tests:
;;   (find-etpat)
;;   (find-etpat0)

;; Test: (find-esubstitutecommandkeys eev-mode-help)
;;       (find-estring                eev-mode-help)
;;       (find-esubstitutecommandkeys (documentation 'eev-mode 'raw))
;;       (find-estring                (documentation 'eev-mode 'raw))
;;       (find-estring                (documentation 'eev-mode))
(defun find-esubstitutecommandkeys (string &rest pos-spec-list)
  "Hyperlink to the result of running `substitute-command-keys' on STRING."
  ;; To do: add buttons and links.
  (apply 'find-estring (substitute-command-keys string)
	 pos-spec-list))

;; Test: (find-ekeymapdescr isearch-mode-map "toggle-regexp")
(defun find-ekeymapdescr (keymap &rest pos-spec-list)
  "Hyperlink to the list of bindings in KEYMAP.
Example: (find-ekeymapdescr isearch-mode-map \"toggle-regexp\")"
  ;; To do: add the buttons/link thing
  (apply 'find-estring (substitute-command-keys "\\<keymap>\\{keymap}")
	 pos-spec-list))

;; Test: (find-eminorkeymapdescr 'eev-mode)
(defun find-eminorkeymapdescr (mode-symbol &rest pos-spec-list)
  "Hyperlink to the list of bindings in the minor mode MODE-SYMBOL.
Example: (find-eminorkeymapdescr 'eev-mode)"
  (apply 'find-ekeymapdescr (ee-minor-mode-keymap mode-symbol)
	 pos-spec-list))

(defalias 'find-eminormodekeymapdescr 'find-eminorkeymapdescr)

(defun ee-minor-mode-keymap (mode-symbol)
  "An auxiliary function used by `find-eminorkeymapdescr'.
Example: (find-ekeymapdescr (ee-minor-mode-keymap 'eev-mode))"
  (cdr (assq mode-symbol minor-mode-map-alist)))

;; Tests:
;;                                        (find-ebufferlocalvars)
;;                                        (find-ebufferlocalvars "mode-name")
;; (find-wset "1_3o_" '(find-eevfile "") '(find-ebufferlocalvars "mode-name"))
;; 
(defun find-ebufferlocalvars (&rest pos-spec-list)
  "Show the result of running `(buffer-local-variables)' in the current buffer."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(buffer-local-variables)*")))
    (apply
     'find-eppp-with-prefix
     ";; See:
;; (find-elnode \"Creating Buffer-Local\" \"buffer-local-variables\")
;; (find-efunction 'find-ebufferlocalvars)\n\n"
     (ee-buffer-local-variables)
     pos-spec-list)))

(defun ee-buffer-local-variables ()
  "Like `(buffer-local-variables)', but sorts the entries alphabetically."
  (ee-sort-pairs (buffer-local-variables)))

;; Broken? See: (find-efile "international/ccl.el")
(defun find-eccldump (ccl-code &rest pos-spec-list)
  "Hyperlink to the result of running `ccl-dump' on CCL-CODE.
Example: (find-eccldump ccl-decode-mule-utf-8)"
  (apply 'find-eoutput-reuse "*ccl-dump*"
	 `(ccl-dump ,ccl-code) pos-spec-list))

;; Broken? `list-iso-charset-chars' and friends are not defined in the
;; Unicode-2 branch of Emacs... Does this still work?
;;
(defun find-echarsetchars (charset &rest pos-spec-list)
  "See: (find-efunction 'list-charset-chars)
Examples: (find-echarsetchars 'mule-unicode-0100-24ff \"733x\")
          (find-echarsetchars 'mule-unicode-2500-33ff)"
  (interactive (list (read-charset "Character set: ")))
  (apply 'find-eoutput-reuse "*charset*"
	 '(cond ((charsetp charset)
		 (list-iso-charset-chars charset))
		((assq charset non-iso-charset-alist)
		 (list-non-iso-charset-chars charset))
		(t (error "Invalid character set %s" charset)))
	 pos-spec-list))




;;;   __ _           _            _                     _   
;;;  / _(_)_ __   __| |       ___(_)_ __  ___  ___ _ __| |_ 
;;; | |_| | '_ \ / _` |_____ / _ \ | '_ \/ __|/ _ \ '__| __|
;;; |  _| | | | | (_| |_____|  __/ | | | \__ \  __/ |  | |_ 
;;; |_| |_|_| |_|\__,_|      \___|_|_| |_|___/\___|_|   \__|
;;;                                                         
;; «find-einsert» (to ".find-einsert")

;; Test: (ee-insert "\n;; " '(?a ?z) 32 "Foo")
(defun ee-insert (&rest rest)
  "Insert characters, strings, or ranges of characters.
Example: (ee-insert '(?a ?z) 10 \"Foo!\")"
  (while rest
    (let ((o (car rest)))
      (cond ((stringp o) (insert o))
	    ((numberp o) (if (characterp o) (insert o)))
	    ((consp o) (mapc 'ee-insert (apply 'number-sequence o)))
	    (t (error "Not string/int/pair: %S" o))))
    (setq rest (cdr rest))))

;; Tests: (find-einsert '((32 1000) 10 (8000 12000)))
;;        (find-einsert '("Greek:\n" (913 969) 10 "Bold:\n" (120276 120327)))
;;        (find-einsert (list (char-from-name "MUSICAL SYMBOL C CLEF")))
(defun find-einsert (what &rest rest)
"Show characters, strings, and ranges of characters in a temporary buffer.
Example of use: (find-einsert '((32 1000) 10 (8000 12000)))"
  (apply 'find-eoutput-reuse "*einsert*"
	 `(apply 'ee-insert ',what) rest))



;;;   __ _           _                        _               _      
;;;  / _(_)_ __   __| |       ___ _   _ _ __ (_) ___ ___   __| | ___ 
;;; | |_| | '_ \ / _` |_____ / _ \ | | | '_ \| |/ __/ _ \ / _` |/ _ \
;;; |  _| | | | | (_| |_____|  __/ |_| | | | | | (_| (_) | (_| |  __/
;;; |_| |_|_| |_|\__,_|      \___|\__,_|_| |_|_|\___\___/ \__,_|\___|
;;;                                                                  
;; «find-eunicode» (to ".find-eunicode")

(defvar ee-unicode-data-file
  "/usr/share/unicode/UnicodeData.txt"
  "The table of unicode characters used by `find-eunicodeucs'.
Hint: install the Debian package \"unicode-data\".")

;; Test: (find-eunicode "203D;INTERROBANG")
(defun find-eunicode (&rest pos-spec-list)
  "Find POS-SPEC-LIST in the file UnicodeData.txt."
  (apply 'find-fline ee-unicode-data-file pos-spec-list))

;; Test: run the `eek' sexp below.
;;   (find-einsert '("Greek:\n" (913 969) 10 "Bold:\n" (120276 120327)) 4)
;;   (eek "<up> M-e  C-x 1  C-x 3  C-x o  M-h M-n")
(defun find-eunicodeucs (c &rest pos-spec-list)
  "Find the entry about the character at point in the file UnicodeData.txt."
  (interactive (list (char-after (point))))
  (apply 'find-eunicode (format "\n%04X" (encode-char c 'ucs)) pos-spec-list))



;;;       _                               
;;;   ___| | ___  ___ _   _ _ __ ___  ___ 
;;;  / __| |/ _ \/ __| | | | '__/ _ \/ __|
;;; | (__| | (_) \__ \ |_| | | |  __/\__ \
;;;  \___|_|\___/|___/\__,_|_|  \___||___/
;;;                                       
;; «ee-symbol-function»  (to ".ee-symbol-function")
;; This is an experimental hack for handling some versions of Emacs31. See:
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2024-07/msg00292.html
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2024-07/msg00311.html
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2024-07/msg00328.html
;; TODO: fix this:
;;   (find-elisp-intro "6. Defining functions")

(defun ee-closure-to-list (c)
  "Experimental!!! See the comments in the source!"
  (cl-loop for i from 1 to (length c)
	   collect (aref c (1- i))))

(defun ee-closure-to-lambda (c)
  "Experimental!!! See the comments in the source!"
  (let ((list (ee-closure-to-list c)))
    (seq-let [arglist body _ _ docstring interactivespec] list
      `(lambda ,arglist
	 ,@(if docstring (list docstring))
	 ,@(if interactivespec `((interactive ,interactivespec)))
	 ,@body))))

(defun ee-symbol-function (sym)
  "Experimental!!! See the comments in the source!"
  (let ((o (symbol-function sym)))
    (if (and (fboundp 'closurep)
	     (closurep o))
	(ee-closure-to-lambda o)
      o)))





;;;   __ _           _                  _                           
;;;  / _(_)_ __   __| |       ___  ___ (_)_   _ _ __ ___  _ __  ___ 
;;; | |_| | '_ \ / _` |_____ / _ \/ _ \| | | | | '_ ` _ \| '_ \/ __|
;;; |  _| | | | | (_| |_____|  __/  __/| | |_| | | | | | | |_) \__ \
;;; |_| |_|_| |_|\__,_|      \___|\___|/ |\__,_|_| |_| |_| .__/|___/
;;;                                  |__/                |_|        
;;
;; «find-eejumps» (to ".find-eejumps")
;; Display all the current eejump targets.
;; See: (find-eev-quick-intro "7.2. The list of eejump targets")
;; Try: (find-eejumps)
;; Related to: (find-eev "eejump.el")

(defun ee-defun-sexp-for (symbol) 
  `(defun ,symbol ,@(cdr (ee-symbol-function symbol))))

(defun ee-defun-str-for (symbol)
  (replace-regexp-in-string
   "^(defun \\([^ ]+\\) nil " "(defun \\1 () "
   (ee-S (ee-defun-sexp-for symbol))))

(defun ee-eejump-symbols ()
  (apropos-internal "^eejump-[0-9]*\\*?$" 'fboundp))

(defun ee-find-eejumps-header ()
  ";; Generated by: (find-eejumps)
;; See: (find-eev-quick-intro \"7.1. `eejump'\" \"`M-j'\")
;;      (find-eev-quick-intro \"7.2. The list of eejump targets\")
;;      (find-emacs-keys-intro \"1. Basic keys (eev)\")
;;      (find-emacs-keys-intro \"2. Key sequences\")
;; For example,
;;         M-5 M-j  runs:  (find-eev-quick-intro)
;;         M-2 M-j  runs:  (find-emacs-keys-intro)
;;         M-1 M-j  runs:  (find-fline \"~/TODO\")
;;     M-2 M-1 M-j  shows the file ~/TODO in the right window
;;     M-3 M-1 M-j  opens ~/TODO in the right window
;;         M-7 M-j  runs:  (find-elisp-intro)
;; Current eejump targets:\n\n")

(defun ee-find-eejumps-body ()
  (mapconcat 'ee-defun-str-for (ee-eejump-symbols) "\n"))

(defun find-eejumps (&rest pos-spec-list) (interactive)
  "See: (find-eev-quick-intro \"find-eejumps\")"
  (let ((ee-buffer-name "*(find-eejumps)*"))
    (apply 'find-estring-elisp
	   (concat (ee-find-eejumps-header)
		   (ee-find-eejumps-body))
	   pos-spec-list)))





;;;   __ _           _                      _                _      _       __     
;;;  / _(_)_ __   __| |       ___  ___  ___| |__   ___  _ __| |_ __| | ___ / _|___ 
;;; | |_| | '_ \ / _` |_____ / _ \/ _ \/ __| '_ \ / _ \| '__| __/ _` |/ _ \ |_/ __|
;;; |  _| | | | | (_| |_____|  __/  __/\__ \ | | | (_) | |  | || (_| |  __/  _\__ \
;;; |_| |_|_| |_|\__,_|      \___|\___||___/_| |_|\___/|_|   \__\__,_|\___|_| |___/
;;;                                                                                
;; «find-eeshortdefs»  (to ".find-eeshortdefs")
;; Test: (find-eeshortdefs)
;; See: (find-eev-quick-intro "7.4. Commands with very short names")

(defvar ee-shortdefp-maxlen-name 3)
(defvar ee-shortdefp-maxlen-def 80)

(defun ee-shortdefp (sym)
  (and (fboundp  sym)
       (commandp sym)
       (listp      (ee-symbol-function sym))
       (eq (car    (ee-symbol-function sym)) 'lambda)
       (<= (length (symbol-name        sym)) ee-shortdefp-maxlen-name)
       (<= (length (ee-defun-str-for   sym)) ee-shortdefp-maxlen-def)))

(defun ee-shortdef-symbols ()
  (apropos-internal "^.*$" 'ee-shortdefp))

(defun ee-find-eeshortdefs-body ()
  (mapconcat 'ee-defun-str-for (ee-shortdef-symbols) "\n"))

(defun ee-find-eeshortdefs-header ()
  ";; Generated by: (find-eeshortdefs)
;; See: (find-eev-quick-intro \"7.4. Commands with very short names\")
;; Current short defs:\n\n")

(defun find-eeshortdefs (&rest pos-spec-list)
  "This is similar to `find-eejumps', but lists commands with very short names."
  (interactive)
  (let ((ee-buffer-name "*(find-eeshortdefs)*"))
    (apply 'find-estring-elisp
	   (concat (ee-find-eeshortdefs-header)
		   (ee-find-eeshortdefs-body))
	   pos-spec-list)))



;;;   __ _           _                                                   __ 
;;;  / _(_)_ __   __| |       ___  __ _ _ __  _ __ ___  _ __   ___  ___ / _|
;;; | |_| | '_ \ / _` |_____ / _ \/ _` | '_ \| '__/ _ \| '_ \ / _ \/ __| |_ 
;;; |  _| | | | | (_| |_____|  __/ (_| | |_) | | | (_) | |_) | (_) \__ \  _|
;;; |_| |_|_| |_|\__,_|      \___|\__,_| .__/|_|  \___/| .__/ \___/|___/_|  
;;;                                    |_|             |_|                  
;;
;; «find-eaproposf»  (to ".find-eaproposf")
;; Tests: (find-eaproposf "^find-.*-links$")
;;        (find-eaproposv "process")
;;        (find-estring (ee-eaproposf0 "^find-.*-links$" 'fboundp ": %s\n"))

(defun find-eaproposf (regexp &rest rest)
  "Go to a temporary buffer listing all functions whose names match REGEXP."
  (interactive "sApropos function (regexp): ")
  (apply 'find-elinks-elisp
	 `(,(ee-template0 "\
;; (find-eaproposf {(ee-S regexp)})
;; (find-eaproposv {(ee-S regexp)})
;; (find-eapropos  {(ee-S regexp)})
;; (find-eapropos-links {(ee-S regexp)})
;; (find-efunction 'find-eaproposf)
")
	   ,(ee-eaproposf0 regexp 'fboundp "(find-efunction '%s)\n"))
	 rest))

(defun find-eaproposv (regexp &rest rest)
  "Go to a temporary buffer listing all variables whose names match REGEXP."
  (interactive "sApropos variable (regexp): ")
  (apply 'find-elinks-elisp
	 `(,(ee-template0 "\
;; (find-eaproposv {(ee-S regexp)})
;; (find-eaproposf {(ee-S regexp)})
;; (find-eapropos  {(ee-S regexp)})
;; (find-eapropos-links {(ee-S regexp)})
;; (find-efunction 'find-eaproposv)
")
	   ,(ee-eaproposf0 regexp 'boundp "(find-evardescr '%s)\n"))
	 rest))

(defun ee-eaproposf0 (regexp predicate fmt)
  "An internal function used by `find-eaproposf'."
  (mapconcat (lambda (sym) (format fmt sym))
	     (apropos-internal regexp predicate)
	     ""))





(provide 'eev-blinks)






;; Local Variables:
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
