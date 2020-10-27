;;; eev-blinks.el -- support for basic hyperlinks in Emacs.
;; The basic hyperlinks are the ones that do not depend on templates,
;; and that are not created by `code-c-d' and friends.

;; Copyright (C) 1999-2019 Free Software Foundation, Inc.
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
;; Version:    2020oct02
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-blinks.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-blinks.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
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
;; «.find-fline»		(to "find-fline")
;; «.find-wottb»		(to "find-wottb")
;; «.find-efaces»		(to "find-efaces")
;; «.find-ebufferandpos»	(to "find-ebufferandpos")
;; «.find-ebuffer»		(to "find-ebuffer")
;; «.find-eoutput»		(to "find-eoutput")
;; «.find-estring»		(to "find-estring")
;; «.find-sh»			(to "find-sh")
;; «.find-man»			(to "find-man")
;; «.find-man-bug»		(to "find-man-bug")
;; «.find-w3m»			(to "find-w3m")
;; «.find-Package»		(to "find-Package")
;; «.find-epp»			(to "find-epp")
;; «.find-eloadhistory»		(to "find-eloadhistory")
;; «.find-einternals»		(to "find-einternals")
;; «.find-einsert»		(to "find-einsert")
;; «.find-eunicode»		(to "find-eunicode")
;; «.find-eejumps»		(to "find-eejumps")



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
(autoload 'Man-fontify-manpage "man" nil t)
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

(defun eek (str) (interactive "sKeys: ")
  "Execute STR as a keyboard macro. See `edmacro-mode' for the exact format.\n
An example: (eek \"C-x 4 C-h\")"
  (execute-kbd-macro (read-kbd-macro str)))




;;;                                                _ _     _       
;;;  _ __   ___  ___       ___ _ __   ___  ___    | (_)___| |_ ___ 
;;; | '_ \ / _ \/ __| ___ / __| '_ \ / _ \/ __|___| | / __| __/ __|
;;; | |_) | (_) \__ \|___|\__ \ |_) |  __/ (__|___| | \__ \ |_\__ \
;;; | .__/ \___/|___/     |___/ .__/ \___|\___|   |_|_|___/\__|___/
;;; |_|                       |_|                                   
;;;
;; «ee-goto-position»  (to ".ee-goto-position")
;; Support for pos-spec-lists in hyperlinks.
;; See: (find-eval-intro "6. Refining hyperlinks")

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
;; See: (find-eval-intro "7. Pos-spec-lists")
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
  (apply 'ee-goto-position pos-spec-list))





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

;; Test: (find-customizegroup 'processes)
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

;; Tests: (find-epackages nil "\n  bdo ")
;;        (find-epackages t   "\n  bdo ")
;;
(defun find-epackages (&optional no-fetch &rest pos-spec-list)
  "Hyperlink to the output of `list-packages'."
  (interactive "P")
  (apply 'find-wottb-call '(list-packages no-fetch)
	 "*Packages*" pos-spec-list))

;; Test: (find-epackage 'bdo)
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
;;   (find-ecolors             " white")
;;   (find-efaces              "default")
;;   (find-efacedescr 'default "Foreground:")

(defun find-echardescr (&optional pos &rest pos-spec-list)
  "Hyperlink to the result of running `describe-char' at POS."
  (interactive)
  (setq pos (or pos (point)))
  (apply 'find-wottb-call '(describe-char pos) "*Help*" pos-spec-list))

(defun find-ecolors (&rest pos-spec-list)
  "Hyperlink to the result of running `list-colors-display'."
  (interactive)
  (apply 'find-wottb-call '(list-colors-display) "*Colors*" pos-spec-list))

(defun find-efacedescr (&optional face &rest pos-spec-list)
  "Hyperlink to the result of running `describe-face' on FACE."
  ;; (interactive (list (read-face-name "Describe face")))
  (interactive (list (face-at-point)))
  (apply 'find-wottb-call '(describe-face face) "*Help*" pos-spec-list))

(defun find-efaces (&rest pos-spec-list)
  "Hyperlink to the result of running `list-faces-display'."
  (interactive)
  (apply 'find-wottb-call '(list-faces-display) "*Faces*" pos-spec-list))

(defun find-etpat (&optional pos &rest pos-spec-list)
  "Hyperlink to the result of running `describe-text-properties' at point.
See `find-etpat0' and `find-etpat00' for lower-level tools for
inspecting text proprties."
  (interactive)
  (setq pos (or pos (point)))
  (apply 'find-wottb-call '(describe-text-properties pos)
	 "*Help*" pos-spec-list))

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
           (setq default-directory dir))))

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

(defvar ee-find-man-flag          nil "See `find-man'.")
(defvar ee-find-man-pos-spec-list nil "See `find-man'.")

;; See: (to "find-man-bug")
(defun find-man (manpage &rest pos-spec-list)
  "Hyperlink to a manpage."
  (interactive (list (ee-manpagename-ask)))
  (setq ee-find-man-flag t
	ee-find-man-pos-spec-list pos-spec-list)
    (man manpage))

;; See: (find-elnode "Advising Functions")
;;      (find-elnode "Porting old advice")
;;      (find-efunctiondescr 'defadvice)
(defadvice Man-notify-when-ready (around find-man (man-buffer) activate)
  "After rendering a manpage jump to `ee-find-man-pos-spec-list'."
  (if (not ee-find-man-flag)
      ad-do-it
    (switch-to-buffer man-buffer)
    (apply 'ee-goto-position ee-find-man-pos-spec-list)
    (setq ee-find-man-flag nil)))

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
;;   (find-functionpp 'find-efunction)

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

(defun find-efunctionpp (symbol &rest pos-spec-list)
"Visit a temporary buffer containing the pretty-printed Lisp code for SYMBOL."
  (interactive (find-function-read))
  (let ((ee-buffer-name
	 (or ee-buffer-name (format "*function %S*" symbol))))
    (apply 'find-epp
	   (symbol-function symbol)
	   ;; Note: if instead of the above we use
	   ;;  `(fset ',symbol ',(symbol-function symbol))
	   ;; the we get a buffer in which we can edit the code for SYMBOL.
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
	    ((numberp o) (if (char-valid-p o) (insert o)))
	    ((consp o) (mapc 'ee-insert (apply 'number-sequence o)))
	    (t (error "Not string/int/pair: %S" o))))
    (setq rest (cdr rest))))

;; Tests: (find-einsert '((32 1000) 10 (8000 12000)))
;;        (find-einsert '("Greek:\n" (913 969) 10 "Bold:\n" (120276 120327)))
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
  `(defun ,symbol ,@(cdr (symbol-function symbol))))

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
;;     M-1 M-j  runs:  (find-fline \"~/TODO\")
;;     M-2 M-j  runs:  (find-emacs-keys-intro)
;;     M-5 M-j  runs:  (find-eev-quick-intro)
;; Current eejump targets:\n\n")

(defun ee-find-eejumps-body ()
  (mapconcat 'ee-defun-str-for (ee-eejump-symbols) "\n"))

(defun find-eejumps (&rest pos-spec-list) (interactive)
  "See: (find-eev-quick-intro \"find-eejumps\")"
  (apply 'find-estring-elisp
	 (concat (ee-find-eejumps-header)
		 (ee-find-eejumps-body))
	 pos-spec-list))




(provide 'eev-blinks)






;; Local Variables:
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
