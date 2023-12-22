;;; eev-template0.el -- implement functions that eval `{}'s in a string.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2019-2023 Free Software Foundation, Inc.
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
;; Version:    20230127
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-template0.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-template0.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-quick-intro.html>
;;                                               (find-eev-quick-intro)

;; «.ee-template0»	(to "ee-template0")
;; «.lexical-binding»	(to "lexical-binding")
;; «.ee-template0-lex»	(to "ee-template0-lex")



;;; Commentary:
;;
;; This file implements `ee-template00', a function that receives a
;; string STR and replaces all substrings in it enclosed in `{}'s by
;; the result of evaluating them; in particular, it replaces each
;; `{VAR}' in STR by the contents of the variable VAR. Here are some
;; examples/tests:
;;
;;   (ee-template00 "a{(+ 2 3)}b")
;;     -->  "a5b"
;; 
;;   (let ((hi "Here: ") (a 22) (b 33)) (ee-template00 "{hi}{a}+{b}={(+ a b)}"))
;;       -->  "Here: 22+33=55"
;;
;;   (defun foo (a b) (ee-template00 "{a}+{b}={(+ a b)}"))
;;   (foo 22 33)
;;       -->  "22+33=55"
;;
;; This file also implements `ee-template0', that is a higher-level
;; version of `ee-template00' that adds a trick that makes each "{<}"
;; and each "{>}" in STR be replaced by "{" and "}" respectively. For
;; example:
;;
;;   (ee-template0 "{<} a{(+ 2 3)} {>}")
;;       -->  "{ a5 }"
;;
;; `ee-template0' is used by three families of functions in eev:
;;
;;   1) the `ee-wrap-*'s (and `eewrap-*'s),
;;   2) the `ee-code-*'s (and `code-*'s and `find-code-*'s),
;;   3) the `find-*-links' functions in "find-tlinks.el".
;;
;; To understand the kinds of templates that `ee-template0' is
;; typically used on, try the tests below; note that all of them
;; display their results in temporary buffers.
;;
;;   (find-estring (ee-wrap-escript-block "ANCHOR" "LONG TITLE"))
;;   (find-code-c-d "CODE" "/DIR/")
;;   (find-code-c-d "CODE" "/DIR/" :info "INFO" :gz)
;;   (find-code-pdf-page "livesofanimals" "/tmp/Coetzee99.pdf")
;;   (find-code-pdf-text "livesofanimals" "/tmp/Coetzee99.pdf")
;;   (find-code-brurl  'find-foo :remote 'brfoo :local 'brfool :dired 'brfood)
;;   (find-code-brfile 'find-bar                :local 'brbarl :dired 'brbard)
;;   (find-latex-links "FOO")
;;
;; Here are some links to places where the documentation of eev
;; mentions uses of `ee-template0':
;;
;;   (find-eevgrep "grep --color -nH -e ee-template0 *.el")
;;   (find-eev-quick-intro "7.5. `find-latex-links'")
;;   (find-eev-quick-intro "8.4. Creating e-script blocks" "`M-B'")
;;   (find-eev-quick-intro "9.2. Extra arguments to `code-c-d'")
;;   (find-eev-quick-intro "9.3. Hyperlinks to PDF files")
;;
;; Important: `ee-template0' is _INCOMPATIBLE WITH LEXICAL BINDING_.
;; See the comments at the end of this file.




;;;                  _                       _       _        ___  
;;;   ___  ___      | |_ ___ _ __ ___  _ __ | | __ _| |_ ___ / _ \ 
;;;  / _ \/ _ \_____| __/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \ | | |
;;; |  __/  __/_____| ||  __/ | | | | | |_) | | (_| | ||  __/ |_| |
;;;  \___|\___|      \__\___|_| |_| |_| .__/|_|\__,_|\__\___|\___/ 
;;;                                   |_|                          
;;
;; «ee-template0»  (to ".ee-template0")

(defvar ee-template00-re "{\\([^{}]+\\)}"
  "To make `ee-template0' use other delimiters instead of `{}'s
set this variable temporarily in a `let'.")

;; Test:
;; (ee-template0 "{<} a{(+ 2 3)} {>}")
;;
(defun ee-template0 (str)
  "Replace substrings enclosed by `{}'s in STR by the result of evaluating them.
Substrings of the form `{<}' and `{>}' in STR are replaced by `{'
and `}' respectively; apart from that, this is the same as
`ee-template00'.
Example:  (ee-template0 \"{<} a{(+ 2 3)} {>}\")
             -->  \"{ 5 }\""
  (let ((< "{") (> "}"))
    (ee-template00 str)))

;; Tests:
;; (ee-template00 "a{(+ 2 3)}b")
;; (let ((hi "Here: ") (a 22) (b 33)) (ee-template00 "{hi}{a}+{b}={(+ a b)}"))
;; 
(defun ee-template00 (str)
  "Replace substrings enclosed by `{}'s in STR by the result of evaluating them.
Examples:\n
  (ee-template00 \"a{(+ 2 3)}b\")
    -->  \"a5b\"\n
  (let ((hi \"Here:\") (a 22) (b 33))
    (ee-template00 \"{hi} {a} + {b} = {(+ a b)}\"))
    -->  \"Here: 22 + 33 = 55\""
  (save-match-data
    (replace-regexp-in-string
     ee-template00-re
     (lambda (_code_) (format "%s" (eval (read (substring _code_ 1 -1)))))
     str 'fixedcase 'literal)))




;;;  _           _           _   _     _           _ _             
;;; | | _____  _(_) ___ __ _| | | |__ (_)_ __   __| (_)_ __   __ _ 
;;; | |/ _ \ \/ / |/ __/ _` | | | '_ \| | '_ \ / _` | | '_ \ / _` |
;;; | |  __/>  <| | (_| (_| | | | |_) | | | | | (_| | | | | | (_| |
;;; |_|\___/_/\_\_|\___\__,_|_| |_.__/|_|_| |_|\__,_|_|_| |_|\__, |
;;;                                                          |___/ 
;; «lexical-binding»  (to ".lexical-binding")
;;
;; Here is a demo of how `ee-template0' can fail in lexical binding.
;; Note that the defun below is commented out with an initial "'".
'
(defun ee-dynlex-test (a b)
  (let* ((aa (concat a a))
         (bb (concat b b)))
    (ee-template0 "<{a} {aa} {b} {bb}>")))
;;
;; Tests:
;;
;;   1) Run the defun above with `M-e'.
;;      Then try this test:
;;        (ee-dynlex-test "Aa" "Bb")
;;      It will returns this:
;;        "<Aa AaAa Bb BbBb>"
;;
;;   2) Run the defun above in lexical binding mode with `M-1 M-1 M-e'.
;;      Then try this test:
;;        (ee-dynlex-test "Aa" "Bb")
;;      You will get this error:
;;        "format: Symbol's value as variable is void: a"
;;
;; See:
;;   (find-lexical-intro "0. How to use this")
;;   (find-lexical-intro "0. How to use this" "`M-1 M-1 M-e'")
;;   (find-lexical-intro "5. A thread")
;;
;; Here are some messages in help-gnu-emacs, emacs-devel and
;; bug-gnu-emacs about dynamic binding being deprecated:
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00054.html
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00085.html
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00095.html
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00096.html
;;   https://lists.gnu.org/archive/html/emacs-devel/2021-09/msg01854.html
;;   https://debbugs.gnu.org/cgi/bugreport.cgi?bug=30078#86 Drew (cites RMS)



;;;             _           
;;;            | | _____  __
;;;       _____| |/ _ \ \/ /
;;;  _ _ |_____| |  __/>  < 
;;; (_|_|_)    |_|\___/_/\_\
;;;                         
;; «ee-template0-lex»  (to ".ee-template0-lex")
;; `ee-template0-lex' is a variant of `ee-template0' that doesn't need
;; dynamic binding and that should work well in lexical binding. It is
;; implemented as a macro, so beginners would probably find it much
;; harder to understand than `ee-template0'.
;;
;; This is not used by eev. See:
;;   (find-eevgrep "grep --color=auto -nH -e ee-template0 *.el")
;;
;; The source code of eev needs to be clear to beginners, so I prefer
;; to use `ee-template0' everywhere in the source - but in theory it
;; should be possible to replace all occurrences of `ee-template0' by
;; `ee-template0-lex'.
;;
;; This is based on code sent by Stefan Monnier.

(defmacro ee-template0-lex (str)
  "A replacement for `ee-template0' that doesn't need dynamic binding.
This is a macro, and a call to
  (ee-template0-lex \"ab{(+ c d)}ef\")
is replaced by something equivalent (but to not exactly equal) to:
  (concat \"ab\" (format \"%s\" (+ c d)) \"ef\")
See the source code for examples and tests."
  (ee-template00-lex str))

(defun ee-template00-lex (str)
  "An internal function used by `ee-template0-lex'."
  `(let ((< "{") (> "}"))
     (ignore < >)   ; Silence byte-compiler in case `str' doesn't use those
     ,(ee-template000-lex str)))

(defun ee-template000-lex (str)
  "An internal function used by `ee-template00-lex'."
  (let ((exprs '())
        (i 0))
    (while (string-match "{\\([^{}]+\\)}" str i)
      (push (substring str i (match-beginning 0)) exprs)
      (setq i (match-end 0))
      (push `(format "%s" ,(read (match-string 1 str))) exprs))
    (push (substring str i) exprs)
    (cons 'concat (delete "" (nreverse exprs)))))

;; Tests:
;;               (ee-template0-lex "{<} a{(+ 2 3)} {>}")
;; (macroexpand '(ee-template0-lex "{<} a{(+ 2 3)} {>}"))
;;              (ee-template00-lex "{<} a{(+ 2 3)} {>}")
;;             (ee-template000-lex "{<} a{(+ 2 3)} {>}")
;;
' (let ((hi "Here:") (a 22) (b 33))
    (ee-template0-lex "{hi} {a} + {b} = {(+ a b)}"))





(provide 'eev-template0)






;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
