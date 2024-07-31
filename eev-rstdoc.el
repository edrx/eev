;;; eev-rstdoc.el -- links to documentation generated from RST files.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
;; Version:    20240731
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-rstdoc.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-rstdoc.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-rstdoc-intro.html>
;;                                               (find-rstdoc-intro)

;; «.introduction»		(to "introduction")
;; «.default-defvars»		(to "default-defvars")
;;   «.ee-rstdoc-:py»		(to "ee-rstdoc-:py")
;;   «.ee-rstdoc-:sympy»	(to "ee-rstdoc-:sympy")
;;   «.ee-rstdoc-:mpl»		(to "ee-rstdoc-:mpl")
;; «.other-defvars»		(to "other-defvars")
;;   «.ee-rstdoc-:clhs»		(to "ee-rstdoc-:clhs")
;; «.basic-ops»			(to "basic-ops")
;; «.around-point»		(to "around-point")
;; «.code-rstdoc»		(to "code-rstdoc")
;; «.default-defuns»		(to "default-defuns")


;; «introduction»  (to ".introduction")

;; 0. Warning
;; ==========
;; This docs are being rewritten!
;; At this moment the best docs are in this intro,
;;
;;   (find-rstdoc-intro)
;;
;; and there's a video about eev-rstdoc.el:
;;
;;   Title: Short hyperlinks to Python docs (eev @ EmacsConf2022)
;;   MP4:   http://anggtwu.net/eev-videos/emacsconf2022-py.mp4
;;   YT:    http://www.youtube.com/watch?v=QeqCYQSlz-I
;;   Page:  http://anggtwu.net/emacsconf2022-py.html
;;   Comment: A video about eev-rstdoc.el.
;;   Date:    2022dec04
;;   Length:  14:03
;;
;;   Play: (find-eev2022pyvideo "0:00")
;;   Info: (find-1stclassvideo-links "eev2022py")
;;
;; Old docs:


;; This is an experimental feature.
;; I'm still cleaning it up, and I'll give a presentation
;; about it in the EmacsConf2022:
;;
;;   http://anggtwu.net/emacsconf2022-py.html
;;
;; See also these posts in the mailing list:
;;
;;   https://lists.gnu.org/archive/html/eev/
;;   https://lists.gnu.org/archive/html/eev/2022-08/threads.html
;;   https://lists.gnu.org/archive/html/eev/2022-08/msg00011.html
;;   https://lists.gnu.org/archive/html/eev/2022-08/msg00013.html
;;   https://lists.nongnu.org/mailman/listinfo/eev
;;
;;
;; 1. Very short introduction
;; ==========================
;; Try this:
;;
;;   (load (buffer-file-name))
;;   (ee-rstdoc-default-defuns)
;;   (find-pydocw    "tutorial/classes")
;;   (find-sympydocw "tutorials/intro-tutorial/gotchas#equals-signs")
;;   (find-mpldocw   "tutorials/introductory/pyplot")
;;
;; Each one of the `find-*docw' functions above expands its argument
;; in a different way, converts it to a URL, and opens it in a
;; browser. These "ways of expanding" are configurable, but what makes
;; this package really useful is that is also has configurable "ways
;; of shortening" that can be used to produce other elisp hyperlinks
;; like the ones above. Let me explain that with an example. The "we"
;; in the example below is obviously a user who knows how this works;
;; we want to become like "we".
;;
;; The documentation of Python in intended to be read in a browser.
;; The `find-pydocw' sexp above opens this URL in a browser:
;;
;;   https://docs.python.org/3/tutorial/classes.html
;;
;; Suppose that we navigate the Python tutorial a bit, and we find
;; this other section in it that we want to keep a link to:
;;
;;   https://docs.python.org/3/tutorial/controlflow.html#lambda-expressions
;;
;; We copy that URL to an Emacs buffer, and then we type `M-x pdk'
;; (mnemonic: "Python-doc-kill") with the point on that URL. The `M-x
;; pdk' will inspect the text around the point, shorten it in the
;; right way, and it will display this message in the echo area:
;;
;;   Copied to the kill ring:
;;   # (find-pydoc "tutorial/controlflow#lambda-expressions")
;;
;; Then we use `C-y' to insert that line, and `M-h M-2 M-h M-2' to
;; duplicate it twice. We get this (modulo the ";;s"):
;;
;;   # (find-pydoc "tutorial/controlflow#lambda-expressions")
;;   # (find-pydoc "tutorial/controlflow#lambda-expressions")
;;   # (find-pydoc "tutorial/controlflow#lambda-expressions")
;;
;; Then we add a "w" and a "r" in the right places, and the three
;; lines above become these ones:
;;
;;   # (find-pydoc  "tutorial/controlflow#lambda-expressions")
;;   # (find-pydocw "tutorial/controlflow#lambda-expressions")
;;   # (find-pydocr "tutorial/controlflow#lambda-expressions")
;;
;; The first two sexps above will open URLs like these ones:
;;
;;   file:///usr/share/doc/python3.11-doc/html/tutorial/controlflow.html#lambda-expressions
;;   https://docs.python.org/3/tutorial/controlflow.html#lambda-expressions
;;
;; i.e., the `find-pydoc' uses the local copy of the Python docs, that
;; the browser can open very quickly, and the `find-pydocw' uses the
;; copy "from the web", that takes longer. The third sexp, the one
;; with `find-pydocr', opens this file:
;;
;;   /usr/share/doc/python3.11/html/_sources/tutorial/controlflow.rst.txt
;;
;; that is the source of "control.html", in RST format - see:
;;
;;   https://en.wikipedia.org/wiki/ReStructuredText
;;
;; I find it much easier to copy examples from a .rst than from the
;; HTML pages.
;;
;;
;; 2. Tutorials
;; ============
;; "Normal" tutorials have a lot of explanatory text, and a few
;; examples. We can use elisp hyperlinks of the kinds above to create
;; tutorials in another style - made of lots of executable examples,
;; plus elisp hyperlinks in comments that point to the standard,
;; "normal" tutorials, and to reference manuals. There are some
;; examples of tutorials in that style here:
;;
;;   (find-es "python" "tut-strings")
;;   (find-es "sympy" "tutorial")
;;   http://anggtwu.net/e/python.e.html#tut-strings
;;   http://anggtwu.net/e/sympy.e.html#tutorial
;;   http://anggtwu.net/eepitch.html#tutorials
;;
;;
;; 3. How this works
;; =================
;; The main function defined in this file is `code-rst', that works
;; similarly to `code-c-d', that is explained in this section of the
;; main tutorial:
;;
;;   (find-eev-quick-intro "9.1. `code-c-d'")
;;   (find-eev-quick-intro "9.1. `code-c-d'" "find-code-c-d")
;;
;; A sexp like
;;
;;   (code-rstdoc :py)
;;
;; generates some code - incluing defuns - and runs it. A sexp like
;;
;;   (find-code-rstdoc :py)
;;
;; generates the same code, and then shows it in a temporary buffer
;; instead of running it. The comments in that code contains LOTS of
;; simple tests and links to docs, so the temporary buffer generated
;; by the `find-code-rstdoc' is a good starting point for
;; understanding how this works.
;;
;; Look at the last defun in each of the `find-code-rstdoc's below:
;;
;;   (find-code-rstdoc :py)
;;   (find-code-rstdoc :sympy)
;;   (find-code-rstdoc :mpl)
;;
;; They define "killing functions" - i.e., functions that put lines in
;; the kill ring - called `pdk', `sdk', and `mdk'. These functions
;; violate the principle that each package should only define
;; functions with certain prefixes - see:
;;
;;   (find-eev-intro "1. `eev-mode'")
;;   (find-eev-intro "1. `eev-mode'" "prefixes")
;;
;; and this means that these sexps
;;
;;   (code-rstdoc :py)
;;   (code-rstdoc :sympy)
;;   (code-rstdoc :mpl)
;;
;; can't be run by default when eev is loaded - the user has to run
;; them explicitly somehow. In the "Very short introduction" above I
;; suggested running them with:
;;
;;   ;; See: (find-eev "eev-rstdoc.el" "default-defuns")
;;   (ee-rstdoc-default-defuns)
;;
;;
;; 4. Configuration
;; ================
;; Sexps like these
;;
;;        (code-rstdoc :py)
;;   (find-code-rstdoc :py)
;;
;; use the data stored in the variable `ee-rstdoc-:py'. This file
;; defines `ee-rstdoc-:py', and its variants for SymPy and MatPlotLib,
;; `ee-rstdoc-:sympy' and `ee-rstdoc-:mpl', here,
;;
;;   (find-eev "eev-rstdoc.el" "ee-rstdoc-:py")
;;   (find-eev "eev-rstdoc.el" "ee-rstdoc-:sympy")
;;   (find-eev "eev-rstdoc.el" "ee-rstdoc-:mpl")
;;
;; and in a way that supposes that we are on Debian Stable, and that
;; we have these packages installed:
;;
;;   python3.11-doc
;;   python-sympy-doc
;;   python-matplotlib-doc
;;
;; People on other distributions will probably have to take the
;; `defvar's of `ee-rstdoc-:py', `ee-rstdoc-:sympy', and
;; `ee-rstdoc-:mpl', convert them to setqs, adjust some of their
;; fields, and put them in their init files. Note that each one of
;; these setqs will have to be followed by a `code-rstdoc', or,
;; preferrably, by a pair of lines like these:
;;
;;   ;; (find-code-rstdoc :py)
;;           (code-rstdoc :py)



;; Redefine if needed:
(defalias 'ee-rstdoc-browse-url 'find-googlechrome)



;;; __     __         _       _     _           
;;; \ \   / /_ _ _ __(_) __ _| |__ | | ___  ___ 
;;;  \ \ / / _` | '__| |/ _` | '_ \| |/ _ \/ __|
;;;   \ V / (_| | |  | | (_| | |_) | |  __/\__ \
;;;    \_/ \__,_|_|  |_|\__,_|_.__/|_|\___||___/
;;;                                             
;; «default-defvars»  (to ".default-defvars")
;; Each one of three variables below specify how a certain family of
;; rstdoc functions should work. For example, `ee-rstdoc-:py'
;; specifies both how the `find-pydoc*' functions should expand their
;; arguments and how `M-x pdk' should shorten the string at point.
;;
;; See: (find-eev "eev-rstdoc.el" "introduction" "4. Configuration")

;; «ee-rstdoc-:py»  (to ".ee-rstdoc-:py")
;; Try: (find-code-rstdoc :py)
;;      (find-code-rstdoc :sympy)
;;      (find-code-rstdoc :mpl)
;;
(defvar ee-rstdoc-:py
      '(:base      "index"
        :base-web  "https://docs.python.org/3/"
        :base-html "file:///usr/share/doc/python3.11-doc/html/"
        :base-rst  "/usr/share/doc/python3.11/html/_sources/"
        :rst       ".rst.txt"
        :res       ("#.*$" "\\?.*$" ".html$" ".txt$" ".rst$" "^file://"
                    "^https://docs.python.org/3/"
                    "^/usr/share/doc/python[0-9.]*-doc/html/"
                    "^/usr/share/doc/python[0-9.]*/html/_sources/")
        :kill      pdk
	)
      "See: (find-code-rstdoc :py)")

;; «ee-rstdoc-:sympy»  (to ".ee-rstdoc-:sympy")
(defvar ee-rstdoc-:sympy
      '(:base      "index"
        :base-web  "https://docs.sympy.org/latest/"
        :base-html "file:///usr/share/doc/python-sympy-doc/html/"
        :base-rst  "/usr/share/doc/python-sympy-doc/html/_sources/"
	:res       ("#.*$" "\\?.*$" ".html$" ".txt$" ".rst$" "^file://"
		    "^/usr/share/doc/python-sympy-doc/html/"
		    "^https://docs.sympy.org/[0-9.]+/"
		    "^https://docs.sympy.org/latest/")
        :kill      sdk
	)
      "See: (find-code-rstdoc :sympy)")

;; «ee-rstdoc-:mpl»  (to ".ee-rstdoc-:mpl")
(defvar ee-rstdoc-:mpl
      '(:base      "index"
        :base-web  "https://matplotlib.org/stable/"
        :base-html "/usr/share/doc/python-matplotlib-doc/html/"
        :base-rst  "/usr/share/doc/python-matplotlib-doc/html/_sources/"
        :res       ("#.*$" "\\?.*$" ".html$" ".txt$" ".rst$" "^file://"
		    "^/usr/share/doc/python-matplotlib-doc/html/"
	 	    "^https?://docs.matplotlib.org/latest/")
        :kill      mdk)
      "See: (find-code-rstdoc :mpl)")


;;;   ___  _   _                     _       __                     
;;;  / _ \| |_| |__   ___ _ __    __| | ___ / _|_   ____ _ _ __ ___ 
;;; | | | | __| '_ \ / _ \ '__|  / _` |/ _ \ |_\ \ / / _` | '__/ __|
;;; | |_| | |_| | | |  __/ |    | (_| |  __/  _|\ V / (_| | |  \__ \
;;;  \___/ \__|_| |_|\___|_|     \__,_|\___|_|   \_/ \__,_|_|  |___/
;;;                                                                 
;; «other-defvars»  (to ".other-defvars")

;; «ee-rstdoc-:clhs»  (to ".ee-rstdoc-:clhs")
;; The Common Lisp Hyperspec.
;; The Debian package "hyperspec" installs a local copy
;; of the CLHS in /usr/share/doc/hyperspec/. To use this,
;; put these two lines in your ~/.emacs:
;;
;; ;; (find-code-rstdoc :clhs)
;;         (code-rstdoc :clhs)
;;
(defvar ee-rstdoc-:clhs
      '(:base      "Front/Contents"
        :base-web  "http://clhs.lisp.se/"
	           ;; "http://www.lispworks.com/documentation/HyperSpec/"
        :base-html "file:///usr/share/doc/hyperspec/"
        :base-rst  "/BASE-RST/"
        :rst       ".rst"
        :htm       ".htm"
        :res       ("#.*$" "\\?.*$" ".html?$" ".txt$" ".rst$" "^file://"
		    "http://clhs.lisp.se/"
		    "http://www.lispworks.com/documentation/HyperSpec/"
		    "http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/html/hyperspec/HyperSpec/"
		    "http://www.ai.mit.edu/projects/iiip/doc/CommonLISP/HyperSpec/"
		    "/usr/share/doc/hyperspec/")
        :kill      clk
	)
      "See: (find-code-rstdoc :clhs)")



;;;  ____            _                        
;;; | __ )  __ _ ___(_) ___    ___  _ __  ___ 
;;; |  _ \ / _` / __| |/ __|  / _ \| '_ \/ __|
;;; | |_) | (_| \__ \ | (__  | (_) | |_) \__ \
;;; |____/ \__,_|___/_|\___|  \___/| .__/|___/
;;;                                |_|        
;;
;; «basic-ops»  (to ".basic-ops")
;; Basic operations. Most of them receive a "keyword" like :py,
;; :sympy, or :mpl - see:
;;
;;   (find-elnode "Constant Variables")
;;
;; and they convert that into a symbol like `ee-rstdoc-:py' and read
;; fields from the variable `ee-rstdoc-:py'.
;;
;; Tests:
;; (ee-rstdoc-c   :py)
;; (ee-rstdoc-c   'py)
;; (ee-rstdoc-c   "py")
;; (ee-rstdoc-kw  :py)
;; (ee-rstdoc-kw  "py")
;; (ee-rstdoc-var :py)
;; (ee-rstdoc-var "py")
;; (ee-rstdoc-getfield  :py  :base-web)
;; (ee-rstdoc-getfield  :py  :base-web0)
;; (ee-rstdoc-getfield  :py0 :base-web0)
;; (ee-rstdoc-getfield0 :py  :base-web)
;; (ee-rstdoc-getfield0 :py  :base-web0)
;; (ee-rstdoc-getfield0 :py0 :base-web0)
;; (ee-rstdoc-hashanchor "https://docs.python.org/3/index.html")
;; (ee-rstdoc-hashanchor "https://docs.python.org/3/index.html#foo")
;; (ee-rstdoc-stem   :py "https://docs.python.org/3/index.html#foo")
;; (ee-rstdoc-short  :py "https://docs.python.org/3/index.html#foo")
;; (ee-rstdoc-html   :py "https://docs.python.org/3/index.html#foo")
;; (ee-rstdoc-web    :py "https://docs.python.org/3/index.html#foo")
;; (ee-rstdoc-rst    :py "https://docs.python.org/3/index.html#foo")

(defun ee-rstdoc-c   (kw) (replace-regexp-in-string "^:" "" (format "%s" kw)))
(defun ee-rstdoc-kw  (kw) (format ":%s" (ee-rstdoc-c kw)))
(defun ee-rstdoc-var (kw) (ee-intern "ee-rstdoc-:%s" (ee-rstdoc-c kw)))
(defun ee-rstdoc-get (kw) (symbol-value (ee-rstdoc-var kw)))

(defun ee-rstdoc-getfield (kw field)
  (let ((result (plist-get (ee-rstdoc-get kw) field)))
    (if (not result) (error "Empty field %S in %S" field (ee-rstdoc-var kw)))
    result))

(defun ee-rstdoc-getfield0 (kw field)
  (let* ((var (ee-rstdoc-var kw))
	 (fields (and (boundp var) (symbol-value var))))
    (and fields (plist-get fields field))))

(defun ee-rstdoc-stem (kw str)
  (dolist (re (ee-rstdoc-getfield kw :res))
    (setq str (replace-regexp-in-string re "" str)))
  str)

(defun ee-rstdoc-hashanchor (str)
  (if (string-match "#" str)
      (format "#%s" (replace-regexp-in-string "^.*#" "" str))
    ""))

(defun ee-rstdoc-short (kw str)
  (format "%s%s"
	  (ee-rstdoc-stem kw str)
	  (ee-rstdoc-hashanchor str)))

(defun ee-rstdoc-htm (kw)
  (or (ee-rstdoc-getfield0 kw :htm) ".html"))

(defun ee-rstdoc-html (kw &optional str)
  (if (not str)
      (setq str (ee-rstdoc-getfield kw :base)))
  (format "%s%s%s%s"
	  (ee-rstdoc-getfield kw :base-html)
	  (ee-rstdoc-stem kw str)
	  (ee-rstdoc-htm kw)
	  (ee-rstdoc-hashanchor str)))

(defun ee-rstdoc-web (kw &optional str)
  (if (not str)
      (setq str (ee-rstdoc-getfield kw :base)))
  (format "%s%s%s%s"
	  (ee-rstdoc-getfield kw :base-web)
	  (ee-rstdoc-stem kw str)
	  (ee-rstdoc-htm kw)
	  (ee-rstdoc-hashanchor str)))

(defun ee-rstdoc-rst (kw &optional str)
  (if (not str)
      (ee-rstdoc-getfield kw :base-rst)
    (format "%s%s%s"
  	    (ee-rstdoc-getfield kw :base-rst)
	    (ee-rstdoc-stem kw str)
	    (ee-rstdoc-getfield kw :rst))))


;; «around-point»  (to ".around-point")
;; See: (find-eev "eev-elinks.el" "around-point")
;; Try: (rx (intersection (any "!-~") (not (any "\"<>"))))
;;  --> "[!#-;=?-~]"
(defun ee-rstdoc-around-point ()
  (ee-stuff-around-point "!#-;=?-~"))

(defun ee-rstdoc-short-around-point (kw)
  (ee-rstdoc-short kw (ee-rstdoc-around-point)))

;; See: (find-code-rstdoc :py "ee-rstdoc-kill")
;; Test: (ee-rstdoc-kill '(find-pydoc "index"))
;; Based on: (find-efunction 'ee-kl-kill)
(defun ee-rstdoc-kill (sexp)
  (if (listp sexp)
      (setq sexp (ee-S sexp)))
  (kill-new (concat "# " sexp "\n"))
  (message "Copied to the kill ring: # %s" sexp))



;;;                _                     _      _            
;;;   ___ ___   __| | ___       _ __ ___| |_ __| | ___   ___ 
;;;  / __/ _ \ / _` |/ _ \_____| '__/ __| __/ _` |/ _ \ / __|
;;; | (_| (_) | (_| |  __/_____| |  \__ \ || (_| | (_) | (__ 
;;;  \___\___/ \__,_|\___|     |_|  |___/\__\__,_|\___/ \___|
;;;                                                          
;; «code-rstdoc»  (to ".code-rstdoc")
;; Test: (find-code-rstdoc :py)
;;
(defun      code-rstdoc (kw)
  (eval (ee-read (ee-code-rstdoc kw))))
(defun find-code-rstdoc (kw &rest rest)
  (let ((ee-buffer-name
	 (or ee-buffer-name "*find-code-rstdoc*")))
    (apply 'find-estring-elisp (ee-code-rstdoc kw) rest)))
(defun   ee-code-rstdoc (kw0)
  (let* ((c        (ee-rstdoc-c  kw0))
         (kw       (ee-rstdoc-kw kw0))
         (var      (ee-rstdoc-var kw))
         (base     (ee-rstdoc-getfield kw :base))
         (base-rst (ee-rstdoc-getfield kw :base-rst))
         (kill     (ee-rstdoc-getfield kw :kill))
	 )
    (ee-template0 "\
;; (find-code-rstdoc {kw})
;;      (code-rstdoc {kw})
;; Source: (find-eev \"eev-rstdoc.el\" \"code-rstdoc\")
;;    See: (find-rstdoc-intro \"3. `code-rstdoc'\")
;;         (find-rstdoc-intro \"4. `ee-rstdoc-:py' and friends\")
;;         (find-evariable '{var})
;;         (find-eppp       {var})
;;
;; Tests: (code-rstdoc {kw})
;;        (find-{c}doc-expand  \"{base}\")
;;        (find-{c}docw-expand \"{base}\")
;;        (find-{c}docr-expand \"{base}\")
;;        (find-{c}doc         \"{base}\")
;;        (find-{c}docw        \"{base}\")
;;        (find-{c}docr        \"{base}\")
;;        (find-{c}docrfile \"\")
;;        (find-{c}docrsh \"find * | sort\")
;;        (find-{c}dochelp)

(defun find-{c}doc-expand  (str &rest rest) (ee-rstdoc-html {kw} str))
(defun find-{c}docw-expand (str &rest rest) (ee-rstdoc-web  {kw} str))
(defun find-{c}docr-expand (str &rest rest) (ee-rstdoc-rst  {kw} str))

(defun find-{c}doc (&optional str &rest rest)
  \"Open the local html page associated to the rstdoc STR.
This function uses the data in `{var}' to transform STR.\"
  (interactive)
  (ee-rstdoc-browse-url (ee-rstdoc-html {kw} str)))

(defun find-{c}docw (&optional str &rest rest)
  \"Open the remote html page associated to the rstdoc STR.
This function uses the data in `{var}' to transform STR.\"
  (interactive)
  (ee-rstdoc-browse-url (ee-rstdoc-web {kw} str)))

(defun find-{c}docr (&optional str &rest rest)
  \"Open the local .rst.txt file associated to the rstdoc STR
This function uses the data in `{var}' to transform STR.\"
  (interactive)
  (apply 'find-fline (ee-rstdoc-rst {kw} str) rest))

(code-c-d \"{c}docr\" \"{base-rst}\")

(defun find-{c}dochelp (&optional str &rest rest)
  \"This function runs this: (find-code-rstdoc {kw})\"
  (interactive)
  (apply 'find-rstdoc-links {kw} rest))

(defun {kill} ()
  \"Put on the kill ring a sexp hyperlink to the rstdoc at point.
This function uses the regexps in the :res field of `{var}'
to shorten the rstdoc at point.\"
  (interactive)
  (ee-rstdoc-kill
   (format \"(find-{c}doc \\\"%s\\\")\"
           (ee-rstdoc-short-around-point {kw}))))

(defun {kill}f (&optional fname)
  \"Put on the kill ring a sexp hyperlink to the rstdoc of FNAME.
This function uses the regexps in the :res field of `{var}'
to shorten the rstdoc of FNAME.\"
  (interactive)
  (ee-rstdoc-kill
   (format \"(find-{c}doc \\\"%s\\\")\"
	   (ee-rstdoc-short {kw} (or fname (buffer-file-name))))))
")))



;; «default-defuns»  (to ".default-defuns")
;; Try: (find-code-rstdoc :py)
;;      (find-code-rstdoc :sympy)
;;      (find-code-rstdoc :mpl)
;;
;; This is mostly for tests.
;; Most people will prefer to define their own variables -
;; as in: (find-eev "eev-rstdoc.el" "variables")
;; - and then run `code-rstdoc's on them.
;;
(defun ee-rstdoc-default-defuns ()
  (interactive)
  (code-rstdoc :py)
  (code-rstdoc :sympy)
  (code-rstdoc :mpl))



(provide 'eev-rstdoc)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
