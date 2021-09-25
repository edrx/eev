;;; eev-template0.el -- implement functions that eval `{}'s in a string.

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
;; Version:    20210925
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-template0.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-template0.el.html>
;;       See also: <http://angg.twu.net/eev-intros/find-eev-quick-intro.html>
;;                                                (find-eev-quick-intro)

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
;; Note that `ee-template0' is intrinsically INCOMPATIBLE with lexical
;; binding - it is common to have `{VAR}'s in the string referring to
;; names of arguments of a `defun's, or to names of variables defined
;; in an enclosing `let' block, or to names of global variables.
;;
;; Here are some messages in help-gnu-emacs about dynamic binding
;; being deprecated:
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00054.html
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00085.html
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00095.html
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-06/msg00096.html
;;
;; Here is a tutorial on lexical-versus-dynamic binding:
;;
;;   (find-lexical-intro)
;;
;; And here are some links to places where the documentation of eev
;; mentions uses of `ee-template0':
;;
;;   (find-eevgrep "grep --color -nH -e ee-template0 *.el")
;;   (find-eev-quick-intro "7.5. `find-latex-links'")
;;   (find-eev-quick-intro "8.4. Creating e-script blocks" "`M-B'")
;;   (find-eev-quick-intro "9.2. Extra arguments to `code-c-d'")
;;   (find-eev-quick-intro "9.3. Hyperlinks to PDF files")



(defvar ee-template00-re "{\\([^{}]+\\)}"
  "To make `ee-template0' use other delimiters instead of `{}'s
set this variable temporarily in a `let'.")



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
    -->  \"22 + 33 = 55\""
  (save-match-data
    (replace-regexp-in-string
     ee-template00-re
     (lambda (_code_) (format "%s" (eval (read (substring _code_ 1 -1)))))
     str 'fixedcase 'literal)))

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



(provide 'eev-template0)






;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
