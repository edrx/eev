;;; eev-lean4.el -- Definitions for a workshop on Lean4.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
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
;; Version:    20240728
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-lean4.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-lean4.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-lean4-intro.html>
;;                                               (find-lean4-intro)

;;; Comment:
;;
;; This file contains part of the setup for this workshop:
;;
;;   http://anggtwu.net/2024-lean4-oficina-0.html
;;   (find-lean4-intro)
;;
;; This is very experimental and very undocumented!
;; Reload with:
;;
;;   (load (buffer-file-name))

;;

;; Index:
;; «.PATH»			(to "PATH")
;; «.lean4-mode»		(to "lean4-mode")
;; «.code-c-ds»			(to "code-c-ds")
;; «.etc»			(to "etc")
;; «.ee-let*-macro-leandoc»	(to "ee-let*-macro-leandoc")
;; «.code-leandocpdf»		(to "code-leandocpdf")
;; «.find-leandocprep-links»	(to "find-leandocprep-links")
;; «.find-leanbook-links»	(to "find-leanbook-links")
;; «.find-leanrstdoc-links»	(to "find-leanrstdoc-links")
;;
;; «.books»			(to "books")
;; «.leandoc-specs»		(to "leandoc-specs")
;;   «.ee-leandoc-:fplean4»	(to "ee-leandoc-:fplean4")
;;   «.ee-leandoc-:lean4»	(to "ee-leandoc-:lean4")
;;   «.ee-leandoc-:leanmeta»	(to "ee-leandoc-:leanmeta")
;;   «.ee-leandoc-:tclean4»	(to "ee-leandoc-:tclean4")
;;   «.ee-leandoc-:tpil4»	(to "ee-leandoc-:tpil4")

;; (require 'subr-x)
(require 'eev-rstdoc)
(setq package-install-upgrade-built-in t) ; needed for Emacs28?




;;;  ____   _  _____ _   _ 
;;; |  _ \ / \|_   _| | | |
;;; | |_) / _ \ | | | |_| |
;;; |  __/ ___ \| | |  _  |
;;; |_| /_/   \_\_| |_| |_|
;;;                        
;; «PATH»  (to ".PATH")
;; Similar to: export PATH=$HOME/.elan/bin:$PATH
;;        See: (find-lean4-intro "4. Install Lean4")
(setenv "PATH" (format "%s/.elan/bin:%s" (getenv "HOME") (getenv "PATH")))
;; (find-sh "echo $PATH")
;; (find-sh "echo $PATH | tr : '\n'")


;; «lean4-mode»  (to ".lean4-mode")
;; See: (find-lean4-intro "6. Install lean4-mode")
(add-to-list 'load-path "~/.emacs.d/elpa/lean4-mode")
(ignore-errors (require 'lean4-mode))


;; «code-c-ds»  (to ".code-c-ds")
;; (find-lean4prefile "")
;; (find-lean4presh "find * | sort")
(code-c-d "lean4pre"  "~/.elan/toolchains/leanprover--lean4---stable/src/lean/")

;; If lean4-mode or lsp-mode are not installed then the `code-c-d's
;; below will point to "nil" instead of to a real directory, and you
;; will have to rerun them at some point.
;;
;; (find-code-c-d "lean4mode" (ee-locate-library "lean4-mode"))
;; (find-code-c-d "lspmode"   (ee-locate-library "lsp-mode"))
        (code-c-d "lean4mode" (ee-locate-library "lean4-mode"))
        (code-c-d "lspmode"   (ee-locate-library "lsp-mode"))
;; (find-lean4modefile "")
;; (find-lspmodefile "")


;; «etc»  (to ".etc")
;; (find-es "lean" "lean4-mode-vc")
;; (find-es "lsp" "lsp-mode-git")

(defun l () (interactive) (find-es "lean"))
(defun a () (interactive) (mkdir "/tmp/L/" t) (find-fline "/tmp/L/a.lean"))

;; See: (find-eev "eev-tlinks.el" "ee-copy-rest-3-intro")
(defun cr3 () (interactive)
  (insert "# (ee-copy-rest-3m nil \"-- end\" \"/tmp/L/a.lean\")\n\n-- end\n"))





;;;  _      _                                             
;;; | | ___| |___/\__      _ __ ___   __ _  ___ _ __ ___  
;;; | |/ _ \ __\    /_____| '_ ` _ \ / _` |/ __| '__/ _ \ 
;;; | |  __/ |_/_  _\_____| | | | | | (_| | (__| | | (_) |
;;; |_|\___|\__| \/       |_| |_| |_|\__,_|\___|_|  \___/ 
;;;                                                       
;; «ee-let*-macro-leandoc»  (to ".ee-let*-macro-leandoc")
;; See: (find-templates-intro "7. let* macros")
;; Skel: (find-let*-macro-links "leandoc" "plist" "g kw kill baseweb gitrepo gitname")
;; Test: (ee-let*-macro-leandoc '(:usrc "~/bigsrc/") usrc)
;;
(defmacro ee-let*-macro-leandoc (pl &rest code)
  "An internal function used by `find-leandoc-links'."
  `(cl-letf*
       ((pl        ,pl)
	(plist     (if (symbolp pl) (symbol-value pl) pl))
	((symbol-function 'g) (lambda (field) (plist-get plist field)))
	(kw        (or (g :kw)       "{kw}"))
	(kill      (or (g :kill)     "{kill}"))
	(k2        (or (g :k2)       "{k2}"))
	(base      (or (g :base)     "{base}"))
	(baseweb   (or (g :base-web) "{baseweb}"))
	(rst       (or (g :rst)      ".rst"))
	(usrc      (or (g :usrc)     "~/usrc/"))
	(gitrepo   (or (g :git-repo) "{gitrepo}"))
	(gitsubdir (or (g :git-subdir) ""))
	(username  user-login-name)
	(basewebl  (ee-url-to-fname0 baseweb))
	(baseweblu (format "file://%s" (ee-url-to-fname  baseweb)))
	(baseweb-  (replace-regexp-in-string "https://" "" baseweb))
	(gitname   (replace-regexp-in-string "^.*/\\([^/]*\\)/?$" "\\1" gitrepo))
        (gitdir    (or (g :git-dir) (format "%s%s/" usrc gitname)))
	(baserst   (cond ((g :base-rst) (g :base-rst))
		 	 ((g :git-repo) (format "%s%s" gitdir gitsubdir))
			 (t             "/BASE-RST/")))
	(baserst-  (replace-regexp-in-string "~/" "" baserst))
	)
     ,@code))



;;;  _                      _                      _  __ 
;;; | | ___  __ _ _ __   __| | ___   ___ _ __   __| |/ _|
;;; | |/ _ \/ _` | '_ \ / _` |/ _ \ / __| '_ \ / _` | |_ 
;;; | |  __/ (_| | | | | (_| | (_) | (__| |_) | (_| |  _|
;;; |_|\___|\__,_|_| |_|\__,_|\___/ \___| .__/ \__,_|_|  
;;;                                     |_|              
;;
;; «code-leandocpdf»  (to ".code-leandocpdf")
;; Skel: (find-code-xxx-links "leandocpdf" "pl" "")
;; Test: (find-code-leandocpdf 'ee-leandoc-:lean4)
;;
(defun      code-leandocpdf (pl)
  (eval (ee-read      (ee-code-leandocpdf pl))))
(defun find-code-leandocpdf (pl)
  (let ((ee-buffer-name
	 (or ee-buffer-name "*find-code-leandocpdf*")))
    (find-estring-elisp (ee-code-leandocpdf pl))))
(defun   ee-code-leandocpdf (pl)
  (ee-let*-macro-leandoc
   pl
   (ee-template0 "\
;; (find-code-leandocpdf '{(ee-S pl)})
;;      (code-leandocpdf '{(ee-S pl)})
;;
;; The fields of this leandoc spec are (probably) here:
;;   (find-eev \"eev-lean4.el\" \"ee-leandoc-:{kw}\")
;; The code that generates this temporary buffer is here:
;;   (find-eev \"eev-lean4.el\" \"code-leandocpdf\")

;; Try: (find-{kw}doc  \"{base}\")
;;      (find-{kw}docw \"{base}\")
;;      (find-{kw}docr \"{base}\")
;;      (find-{kw}docrfile \"\")
;;      (find-rstdoc-links :{kw})
;;      (find-code-rstdoc :{kw})
;;      (find-{kw}page)
;;      (find-{kw}text)

;; The `setq' below creates a rstdoc spec from this leandoc spec.
;; See: (find-rstdoc-intro)
;;
(setq ee-rstdoc-:{kw}
      '(:base      \"{base}\"
        :base-web  \"{baseweb}\"
        :base-html \"file:///home/{username}/snarf/https/{baseweb-}\"
        :base-rst  \"{baserst}\"
        :rst       \"{rst}\"
        :res       (\"#.*$\" \"\\\\?.*$\"
                    \"\\\\.html$\" \"\\\\.txt$\" \"\\\\.rst$\" \"\\\\.md$\" \"\\\\.lean$\"
                    \"^file://+\" \"^/\" \"^home/{username}/\" \"^~/\" \"^snarf/\" \"^https:?/+\"
                    \"^{baseweb-}\"
                    \"^{baserst-}\")
        :kill      {kill}
        ))

;; Try: (find-code-rstdoc :{kw})
             (code-rstdoc :{kw})

(code-c-d \"{kw}doc\" \"{basewebl}\")
;; (find-{kw}docfile \"\")
;; (find-{kw}docfile \"\" \"print\")

(code-c-d \"{kw}\" \"{gitdir}\")
;; (find-{kw}file \"\")

;; Try: (find-{kw}page)
;;      (find-{kw}text)
(code-pdf-page  \"{kw}\" \"{basewebl}print.pdf\")
(code-pdf-text8 \"{kw}\" \"{basewebl}print.pdf\")

;; Try: ({k2}l \"Copyright\")
(defun {k2}l (&rest rest) (interactive)
  (apply 'find-leanbook-links '{kw} rest))

;; See: (find-leandocprep-links '{(ee-S pl)})

")))


;;;  _                      _                                 
;;; | | ___  __ _ _ __   __| | ___   ___ _ __  _ __ ___ _ __  
;;; | |/ _ \/ _` | '_ \ / _` |/ _ \ / __| '_ \| '__/ _ \ '_ \ 
;;; | |  __/ (_| | | | | (_| | (_) | (__| |_) | | |  __/ |_) |
;;; |_|\___|\__,_|_| |_|\__,_|\___/ \___| .__/|_|  \___| .__/ 
;;;                                     |_|            |_|    
;;
;; «find-leandocprep-links»  (to ".find-leandocprep-links")
;; Skel: (find-find-links-links-new "leandocprep" "pl" "ee-buffer-name")
;; Test: (find-leandocprep-links 'ee-leandoc-:lean4)
;;
(defun find-leandocprep-links (&optional pl &rest pos-spec-list)
"Visit a temporary buffer containing a script for preparing the
local files for the leandoc PL."
  (interactive)
  (setq pl (or pl "{pl}"))
  (let* ((ee-buffer-name "*find-leandocprep-links*"))
    (ee-let*-macro-leandoc
     pl
     (apply
      'find-elinks-elisp
      `((find-leandocprep-links ',pl ,@pos-spec-list)
	;; Convention: the first sexp always regenerates the buffer.
	(find-efunction 'find-leandocprep-links)
	""
	,(ee-template0 "\
;; 1. Make sure that we have a local copy of the htmls:
;;    (find-fline \"{basewebl}\")
;;    (find-wgetrecursive-links \"{baseweb}\")


;; 2. Produce the files `print.pdf' and `print.txt'.
;;
;;  a) Do we already have the print.pdf and print.txt? Check:
;;     (find-fline \"{basewebl}\")
;;     (find-fline \"{basewebl}\" \"print\")
;;
;;  b) If not, then use `M-x brg' on the url below, and
;;     make the browser print it to /tmp/print.pdf:
;;     (kill-new \"/tmp/print.pdf\")
;;     {baseweb}print.html
;;
;;  c) Copy the `print.pdf' to the right place:
;;     (find-sh0 \"cp -v /tmp/print.pdf {basewebl}\")
;;
;;  d) Generate the `print.txt':
;;     (find-{kw}docfile \"\")
;;     (find-{kw}docfile \"\" \"print\")
;;     (find-{kw}docsh \"pdftotext -layout print.pdf print.txt\")
;;     (find-{kw}docfile \"print.txt\")


;; 3. I use this to adjust how blogme htmlizes `find-{kw}doc':
;;    (find-blogme3-rstdoc-links \"{kw}\")
;;    ^ This only works on my machine!
")
	)
      pos-spec-list))))



;;;  _                  _                 _    
;;; | | ___  __ _ _ __ | |__   ___   ___ | | __
;;; | |/ _ \/ _` | '_ \| '_ \ / _ \ / _ \| |/ /
;;; | |  __/ (_| | | | | |_) | (_) | (_) |   < 
;;; |_|\___|\__,_|_| |_|_.__/ \___/ \___/|_|\_\
;;;                                            
;; «find-leanbook-links»  (to ".find-leanbook-links")
;; Skel: (find-find-links-links-new "leanbook" "bk secname" "ee-buffer-name")
;; Test: (find-leanbook-links 'leanmeta)
;;
(defun find-leanbook-links (&optional bk secname &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for leanbook."
  (interactive)
  (setq bk (or bk "{bk}"))
  (setq secname (or secname "{secname}"))
  (let* ((ee-buffer-name (format "*(find-leanbook-links '%s ...)*" bk)))
    (apply
     'find-elinks
     `((find-leanbook-links ',bk ,secname ,@pos-spec-list)
       (fpl ,secname)
       (ldl ,secname)
       (lml ,secname)
       (tcl ,secname)
       (tpl ,secname)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-efunction 'find-leanbook-links)
       ""
       ,(ee-template0 "\
# (find-eev \"eev-lean4.el\" \"ee-leandoc-:{bk}\")
# (find-{bk}doc)
# (find-{bk}docw)
# (find-{bk}docr)
# (find-{bk}docrfile \"\")
# (find-{bk}page)
# (find-{bk}text)
# (find-{bk}text 1 \"{secname}\")
# (find-{bk}docfile \"\" \"print\")
# (find-{bk}docfile \"print.txt\")
# (find-{bk}docgrep \"grep --color=auto -nH --null -e '{secname}' print.txt\")

")
       )
     pos-spec-list)))



;; «find-leanrstdoc-links»  (to ".find-leanrstdoc-links")
;; Superseded by: (find-efunction 'find-code-leandocpdf)





;;;  _                       _                 _                      
;;; | |    ___  __ _ _ __   | |__   ___   ___ | | _____      __ _ ___ 
;;; | |   / _ \/ _` | '_ \  | '_ \ / _ \ / _ \| |/ / __|    / _` / __|
;;; | |__|  __/ (_| | | | | | |_) | (_) | (_) |   <\__ \_  | (_| \__ \
;;; |_____\___|\__,_|_| |_| |_.__/ \___/ \___/|_|\_\___( )  \__,_|___/
;;;  _                      _                          |/            
;;; | | ___  __ _ _ __   __| | ___   ___   ___ _ __   ___  ___ ___ 
;;; | |/ _ \/ _` | '_ \ / _` |/ _ \ / __| / __| '_ \ / _ \/ __/ __|
;;; | |  __/ (_| | | | | (_| | (_) | (__  \__ \ |_) |  __/ (__\__ \
;;; |_|\___|\__,_|_| |_|\__,_|\___/ \___| |___/ .__/ \___|\___|___/
;;;                                           |_|                  
;; «books»  (to ".books")
;; «leandoc-specs»  (to ".leandoc-specs")

;; «ee-leandoc-:fplean4»  (to ".ee-leandoc-:fplean4")
;;  "Functional Programming in Lean".
;; Try: (find-fplean4doc  "introduction")
;;      (find-fplean4docw "introduction")
;;      (find-fplean4docr "introduction")
;;      (find-fplean4docrfile "")
;;      (find-rstdoc-links :fplean4)
;;      (find-code-rstdoc :fplean4)
;;      (find-fplean4page)
;;      (find-fplean4text)
(setq ee-leandoc-:fplean4
 '(:kw         "fplean4"
   :kill       "fpk"
   :k2         "fp"
   :rst        ".md"
   :base-web   "https://lean-lang.org/functional_programming_in_lean/"
   :git-repo   "https://github.com/leanprover/fp-lean"
   :git-subdir "functional-programming-lean/src/"
   :base       "introduction"
   ))
;; (find-code-leandocpdf 'ee-leandoc-:fplean4)
        (code-leandocpdf 'ee-leandoc-:fplean4)

;; «ee-leandoc-:lean4»  (to ".ee-leandoc-:lean4")
;;  "Lean Manual".
;; Try: (find-lean4doc  "whatIsLean")
;;      (find-lean4docw "whatIsLean")
;;      (find-lean4docr "whatIsLean.md")
;;      (find-lean4docrfile "")
;;      (find-rstdoc-links :lean4)
;;      (find-code-rstdoc :lean4)
;;      (find-lean4page)
;;      (find-lean4text)
(setq ee-leandoc-:lean4
 '(:kw         "lean4"
   :kill       "ldk"
   :k2         "ld"
   :base-web   "https://lean-lang.org/lean4/doc/"
   :git-repo   "https://github.com/leanprover/lean4"
   :usrc       "~/bigsrc/"
   :base-rst   "~/bigsrc/lean4/doc/"
   :base       "whatIsLean"
   :rst        ""
   ))
;; (find-code-leandocpdf 'ee-leandoc-:lean4)
        (code-leandocpdf 'ee-leandoc-:lean4)

;; «ee-leandoc-:leanmeta»  (to ".ee-leandoc-:leanmeta")
;;  "Metaprogramming in Lean 4".
;; Try: (find-leanmetadoc  "main/01_intro")
;;      (find-leanmetadocw "main/01_intro")
;;      (find-leanmetadocr "main/01_intro")
;;      (find-leanmetadocrfile "")
;;      (find-rstdoc-links :leanmeta)
;;      (find-code-rstdoc :leanmeta)
;;      (find-leanmetapage)
;;      (find-leanmetatext)
(setq ee-leandoc-:leanmeta
 '(:kw         "leanmeta"
   :kill       "lmk"
   :k2         "lm"
   :rst        ".lean"
   :base-web   "https://leanprover-community.github.io/lean4-metaprogramming-book/"
   :git-repo   "https://github.com/leanprover-community/lean4-metaprogramming-book"
   :git-subdir "lean/"
   :base       "main/01_intro"
   ))
;; (find-code-leandocpdf 'ee-leandoc-:leanmeta)
        (code-leandocpdf 'ee-leandoc-:leanmeta)

;; «ee-leandoc-:tclean4»  (to ".ee-leandoc-:tclean4")
;;  "Type Checking in Lean 4".
;; Try: (find-tclean4doc  "title_page")
;;      (find-tclean4docw "title_page")
;;      (find-tclean4docr "title_page")
;;      (find-tclean4docrfile "")
;;      (find-rstdoc-links :tclean4)
;;      (find-code-rstdoc :tclean4)
;;      (find-tclean4page)
;;      (find-tclean4text)
(setq ee-leandoc-:tclean4
 '(:kw         "tclean4"
   :kill       "tck"
   :k2         "tc"
   :rst        ".md"
   :base-web   "https://ammkrn.github.io/type_checking_in_lean4/"
   :git-repo   "https://github.com/ammkrn/type_checking_in_lean4"
   :git-subdir "src/"
   :base       "title_page"
   ))
;; (find-code-leandocpdf 'ee-leandoc-:tclean4)
        (code-leandocpdf 'ee-leandoc-:tclean4)
;;
;; (find-2a nil '(find-leanrstdoc-links 'ee-leandoc-:tclean4))


;; «ee-leandoc-:tpil4»  (to ".ee-leandoc-:tpil4")
;;  "Theorem Proving in Lean 4".
;; Try: (find-tpil4doc  "introduction")
;;      (find-tpil4docw "introduction")
;;      (find-tpil4docr "introduction")
;;      (find-tpil4docrfile "")
;;      (find-rstdoc-links :tpil4)
;;      (find-code-rstdoc :tpil4)
;;      (find-tpil4page)
;;      (find-tpil4text)
(setq ee-leandoc-:tpil4
 '(:kw         "tpil4"
   :kill       "tpk"
   :k2         "tp"
   :rst        ".md"
   :base-web   "https://lean-lang.org/theorem_proving_in_lean4/"
   :git-repo   "https://github.com/leanprover/theorem_proving_in_lean4"
   :git-subdir ""
   :base       "introduction"
   ))
;; (find-code-leandocpdf 'ee-leandoc-:tpil4)
        (code-leandocpdf 'ee-leandoc-:tpil4)










(provide 'eev-lean4)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
