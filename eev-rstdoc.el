;;; eev-rstdoc.el -- links to documentation generated from RST files.  -*- lexical-binding: nil; -*-

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
;; Version:    20220818
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-rstdoc.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-rstdoc.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)


;; 0. Warning
;; ==========
;; This is an experimental feature that is not loaded by default.
;; It is being discussed in the eev mailing list:
;;
;;   https://lists.gnu.org/archive/html/eev/
;;   https://lists.gnu.org/archive/html/eev/2022-08/threads.html
;;   https://lists.nongnu.org/mailman/listinfo/eev
;;
;;
;; 1. Very short introduction
;; ==========================
;; We can load this file and make it define some new functions with:
;;
;;   (load (buffer-file-name))
;;   (ee-rstdoc-defun-all)
;;
;; After that these three sexps - try them! -
;;
;;   (find-pydoc  "tutorial/classes")
;;   (find-pydocw "tutorial/classes")
;;   (find-pydocr "tutorial/classes")
;;   
;; will work as sexp hyperlinks to these two URLs and this file:
;;
;;                https://docs.python.org/3/tutorial/classes.html
;;            /usr/share/doc/python3.9/html/tutorial/classes.html
;;   /usr/share/doc/python3.9/html/_sources/tutorial/classes.rst.txt
;;
;; If we look at the comments of `ee-rstdoc-defun-all', at
;;
;;   (find-eev "eev-rstdoc.el" "ee-rstdoc-defun-all")
;;
;; we will see that they are:
;;
;;   See: (find-code-rstdoc ee-rstdoc-python)
;;        (find-code-rstdoc ee-rstdoc-sympy)
;;        (find-code-rstdoc ee-rstdoc-matplotlib)
;;
;; The first `find-code-rstdoc' shows that the corresponding
;; `code-rstdoc' defines four functions: `find-pydoc', `find-pydocw',
;; `find-pydocr', and `pdk'. The first three are very easy to
;; understand; the fourth, `pdk', is much stranger, and it is it that
;; makes this module practical to use.
;;
;;
;; 2. Long introduction 
;; ====================
;; On my Debian box I have two local files that are related to this
;; page of the Python docs:
;;
;;                https://docs.python.org/3/tutorial/classes.html
;;
;; They are:
;;
;;            /usr/share/doc/python3.9/html/tutorial/classes.html
;;   /usr/share/doc/python3.9/html/_sources/tutorial/classes.rst.txt
;;
;; The first one is a local copy of the HTML page, and on my machine
;; opening that local copy with a browser is much faster than opening
;; the remote version. The second one is the source code in RST
;; (a.k.a. "reSructuredText") of the HTML page, and when I want to
;; copy and paste text from the examples I prefer to do that by
;; accessing the .rst.txt rather than by accessing the HTML.
;;
;; Similarly, this page of the SymPy docs
;;
;;                   https://docs.sympy.org/latest/gotchas.html
;;
;; is related to these two local files:
;;
;;            /usr/share/doc/python-sympy-doc/html/gotchas.html
;;   /usr/share/doc/python-sympy-doc/html/_sources/gotchas.rst.txt
;;
;; After loading this file in the right way (I will explain that
;; soon!) I can access those pages and files with the sexp hyperlinks
;; below:
;;
;;     (find-pydoc  "tutorial/classes")
;;     (find-pydocw "tutorial/classes")
;;     (find-pydocr "tutorial/classes")
;;     (find-sympydoc  "gotchas")
;;     (find-sympydocw "gotchas")
;;     (find-sympydocr "gotchas")
;;
;; The hyperlink functions above also support anchors. for example,
;; the first two sexp hyperlinks below
;;
;;     (find-pydoc  "tutorial/classes#class-objects")
;;     (find-pydocw "tutorial/classes#class-objects")
;;     (find-pydocr "tutorial/classes#class-objects")
;;
;; open these urls with a browser (with the function
;; `ee-rstdoc-browse-url'):
;;
;;                  https://docs.python.org/3/tutorial/classes.html#class-objects
;;   file:///usr/share/doc/python3.9-doc/html/tutorial/classes.html#class-objects
;;
;; In `find-pydocr' the anchor is ignored.
;;
;; In a nutshell, what is happening here is that the variable
;; `ee-rstdoc-python' contains instructions for shrinking strings like
;; these
;;
;;                  https://docs.python.org/3/tutorial/classes.html
;;          /usr/share/doc/python3.9-doc/html/tutorial/classes.html
;;   file:///usr/share/doc/python3.9-doc/html/tutorial/classes.html
;;     /usr/share/doc/python3.9/html/_sources/tutorial/classes.rst.txt
;;
;; into this,
;;
;;                                            tutorial/classes
;;
;; and also instructions for expanding these strings in several ways -
;; for example, to obtain the URLs of the local and the remote home
;; pages, and to obtain the file name of the .rst.txt... and the
;; variable `ee-rstdoc-sympy' contains other, different instructions for
;; shrinking and expanding strings, tailored to the SymPy docs.
;;
;;
;; 2. Try it
;; =========
;; Try this:
;;
;;   (load (buffer-file-name))
;;   (find-code-rstdoc ee-rstdoc-python)
;;
;; The function `find-code-rstdoc' is similar to `find-code-c-d',
;; described here -
;;
;;   (find-eev-quick-intro "9. Shorter hyperlinks")
;;
;; it shows the code that this sexp would execute:
;;
;;        (code-rstdoc ee-rstdoc-python)
;;
;; In the temporary buffer created by
;;
;;   (find-code-rstdoc ee-rstdoc-python)
;;
;; you can see the definition of four functions: `find-pydoc',
;; `find-pydocw', `find-pydocr', and `pdk'. The first three are sexp
;; hyperlinks, and have been explained above. The last one, `pdk', is
;; intended to be invoked as `M-x pdk', and it is a practical way to
;; shrink URLs and file names and to produce sexp hyperlinks like this
;; one,
;;
;;   (find-pydoc "tutorial/classes#class-objects")
;;
;; that can be easily transformed to
;;
;;   (find-pydocw "tutorial/classes#class-objects")
;;   (find-pydocr "tutorial/classes#class-objects")
;;
;; by adding the letters "w" and "r" by hand.
;;
;; The best way to understand how `M-x pdk' works is by trying it. Run
;; these two sexps:
;;
;;   (load (buffer-file-name))
;;   (code-rstdoc ee-rstdoc-python)
;;
;; The sexp with `load' above should return `t' and the sexp with
;; `code-rstdoc' should return `pdk'. The `pdk' is because the sexp
;; with `code-rstdoc' executes the four defuns in the string that this
;; sexp
;;
;;   (find-code-rstdoc ee-rstdoc-python)
;;
;; displays in a temporary buffer, and the last defun returns `pdk'.
;; Try `M-x pdk' with the point on each one of the strings below:
;;
;;                 https://docs.python.org/3/tutorial/classes.html#class-objects
;;         /usr/share/doc/python3.9-doc/html/tutorial/classes.html#class-objects
;;  file:///usr/share/doc/python3.9-doc/html/tutorial/classes.html#class-objects
;;    /usr/share/doc/python3.9/html/_sources/tutorial/classes.rst.txt#class-objects
;;                                           tutorial/classes#class-objects
;;
;; In all cases you will get this message in the echo area:
;;
;;   Copied to the kill ring: (find-pydoc "tutorial/classes#class-objects")
;;
;; Then try to insert that sexp with `C-y' and run it.
;;
;;
;; 3. Loading this
;; ===============
;; The right way to load this is by putting these lines in your
;; ~/.emacs - obviously after the place in which you load eev, and
;; without the initial ";;" in each line:
;;
;;   ;; (find-eev "eev-rstdoc.el")
;;   (require 'eev-rstdoc)
;;   ;; (find-code-rstdoc ee-rstdoc-python)
;;           (code-rstdoc ee-rstdoc-python)
;;   ;; (find-code-rstdoc ee-rstdoc-sympy)
;;           (code-rstdoc ee-rstdoc-sympy)
;;   ;; (find-code-rstdoc ee-rstdoc-matplotlib)
;;           (code-rstdoc ee-rstdoc-matplotlib)


;; «.ee-rstdoc-defun-all»	(to "ee-rstdoc-defun-all")

;; Should I use cl-defstruct instead?
;; See: (find-node "(cl)Structures" "cl-defstruct")
;;      (find-node "(eieio)Quick Start" "defclass")
;;      (find-node "(eieio)Slot Options" ":initarg")
;;      (find-node "(eieio)Accessing Slots" "slot-value")
;;
(require 'eieio)

(defclass ee-rstdoc ()
  ((name      :initarg :name)
   (res       :initarg :res)
   (base-html :initarg :base-html)
   (base-rst  :initarg :base-rst)
   (base-web  :initarg :base-web)
   (find-html :initarg :find-html)
   (find-rst  :initarg :find-rst)
   (find-web  :initarg :find-web)
   (kill      :initarg :kill)
   ))

(defalias 'ee-rstdoc-browse-url 'find-googlechrome)

(defun ee-rstdoc-stem (rd str)
  (dolist (re (slot-value rd 'res))
    (setq str (replace-regexp-in-string re "" str)))
  str)

(defun ee-rstdoc-hashanchor (str)
  (if (string-match "#" str)
      (format "#%s" (replace-regexp-in-string "^.*#" "" str))
    ""))

(defun ee-rstdoc-short (rd str)
  (format "%s%s"
	  (ee-rstdoc-stem rd str)
	  (ee-rstdoc-hashanchor str)))

(defun ee-rstdoc-html (rd str)
  (format "%s%s.html%s"
	  (slot-value rd 'base-html)
	  (ee-rstdoc-stem rd str)
	  (ee-rstdoc-hashanchor str)))

(defun ee-rstdoc-web (rd str)
  (format "%s%s.html%s"
	  (slot-value rd 'base-web)
	  (ee-rstdoc-stem rd str)
	  (ee-rstdoc-hashanchor str)))

(defun ee-rstdoc-rst (rd str)
  (format "%s%s.rst.txt"
	  (slot-value rd 'base-rst)
	  (ee-rstdoc-stem rd str)))

;; See: (find-eev "eev-elinks.el" "around-point")
;; Try: (rx (intersection (any "!-~") (not (any "\"<>"))))
;;  --> "[!#-;=?-~]"
(defun ee-rstdoc-around-point ()
  (ee-stuff-around-point "!#-;=?-~"))

(defun ee-rstdoc-short-around-point (rd)
  (ee-rstdoc-short rd (ee-rstdoc-around-point)))

(defun      code-rstdoc (rd)
  (eval (ee-read (ee-code-rstdoc rd))))
(defun find-code-rstdoc (rd &rest rest)
  (apply 'find-estring-elisp (ee-code-rstdoc rd) rest))
(defun   ee-code-rstdoc (rd)
  (let* ((name      (slot-value rd 'name))
         (find-html (slot-value rd 'find-html))
         (find-web  (slot-value rd 'find-web))
         (find-rst  (slot-value rd 'find-rst))
         (kill      (slot-value rd 'kill))
	 )
    (ee-template0 "\
;; (find-code-rstdoc {name})
;; See: (find-eev-quick-intro \"9.1. `code-c-d'\")
;;
(defun {find-html} (str &rest rest)
  \"Open the local html page associated to the rstdoc STR.
This function uses {name} to transform STR in the right way.\"
  (ee-rstdoc-browse-url (ee-rstdoc-html {name} str)))

(defun {find-web} (str &rest rest)
  \"Open the remote html page associated to the rstdoc STR.
This function uses {name} to transform STR.\"
  (ee-rstdoc-browse-url (ee-rstdoc-web {name} str)))

(defun {find-rst} (str &rest rest)
  \"Open the local .rst.txt file associated to the rstdoc STR
This function uses {name} to transform STR.\"
  (apply 'find-fline (ee-rstdoc-rst {name} str) rest))

(defun {kill} ()
  \"Put on the kill ring a sexp hyperlink to the rstdoc at point.
This function uses {name} to shrink the rstdoc at point
and to convert it into a sexp.\"
  (interactive)
  (ee-kl-kill
   (format \"({find-html} \\\"%s\\\")\"
           (ee-rstdoc-short-around-point {name}))))
")))

(setq ee-rstdoc-python
      (ee-rstdoc
       :name      'ee-rstdoc-python
       :res       '("#.*$" "\\?.*$" ".html$" ".rst.txt$" "^file://"
                    "^https://docs.python.org/3/"
		    "^/usr/share/doc/python[0-9.]*-doc/html/"
		    "^/usr/share/doc/python3-doc/html/"
		    "^/usr/share/doc/python3.9-doc/html/")
       :base-html "file:///usr/share/doc/python3.9-doc/html/"
       :base-rst  "/usr/share/doc/python3.9/html/_sources/"
       :base-web  "https://docs.python.org/3/"
       :find-html 'find-pydoc
       :find-rst  'find-pydocr
       :find-web  'find-pydocw
       :kill      'pdk
       ))

(setq ee-rstdoc-sympy
      (ee-rstdoc
       :name      'ee-rstdoc-sympy
       :res       '("#.*$" "\\?.*$" ".html$" ".rst.txt$" "^file://"
		    "^/usr/share/doc/python-sympy-doc/html/"
		    "^/docs.sympy.org/latest/")
       :base-html "file:///usr/share/doc/python-sympy-doc/html/"
       :base-rst  "/usr/share/doc/python-sympy-doc/html/_sources/"
       :base-web  "https://docs.sympy.org/latest/"
       :find-html 'find-sympydoc
       :find-rst  'find-sympydocr
       :find-web  'find-sympydocw
       :kill      'sdk
       ))

(setq ee-rstdoc-matplotlib
      (ee-rstdoc
       :name      'ee-rstdoc-matplotlib
       :res       '("#.*$" "\\?.*$" ".html$" ".rst.txt$" "^file://"
		    "^/usr/share/doc/python-matplotlib-doc/html/"
		    "^/docs.matplotlib.org/latest/")
       :base-html "file:///usr/share/doc/python-matplotlib-doc/html/"
       :base-rst  "/usr/share/doc/python-matplotlib-doc/html/_sources/"
       :base-web  "https://matplotlib.org/stable/"
       :find-html 'find-mpldoc
       :find-rst  'find-mpldocr
       :find-web  'find-mpldocw
       :kill      'mdk
       ))

;; «ee-rstdoc-defun-all»  (to ".ee-rstdoc-defun-all")
;; See: (find-code-rstdoc ee-rstdoc-python)
;;      (find-code-rstdoc ee-rstdoc-sympy)
;;      (find-code-rstdoc ee-rstdoc-matplotlib)
;;
(defun ee-rstdoc-defun-all ()
  (interactive)
  (code-rstdoc ee-rstdoc-python)
  (code-rstdoc ee-rstdoc-sympy)
  (code-rstdoc ee-rstdoc-matplotlib))



(provide 'eev-rstdoc)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
