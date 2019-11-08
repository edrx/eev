;;; eev-brxxx.el -- define families of browse-url-like functions.

;; Copyright (C) 2012-2019 Free Software Foundation, Inc.
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
;; Version:    2019oct27
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-brxxx.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-brxxx.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-psne-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-brxxx-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-psne-intro)
;;                                                (find-brxxx-intro)

;;; Commentary:

;; See: (find-eev-quick-intro "3.1. Non-elisp hyperlinks")
;;      (find-psne-intro "Local copies")
;;      (find-brxxx-intro)
;;
;; The main functions here are `code-brurl' and `code-brfile', that
;; lets us mass-define "brxxx functions" based on a given function
;; that expects urls - in the case of `code-brurl' - or on a function
;; that expects file names - in the case of `code-brfile'.
;;
;; Try these tests to see the kind of code that is generated:
;;
;;   (find-code-brurl  'find-foo :remote 'brfoo :local 'brfool :dired 'brfood)
;;   (find-code-brfile 'find-bar                :local 'brbarl :dired 'brbard)
;;
;; The functions `brfoo', `brfool' and `brbarl' operate on the
;; url-at-point, and the functions `brfood' and `brbard' operate on
;; the file at the current line and only work in dired mode. The
;; functions `brfool' and `brbarl' call `find-foo' and `find-bar' on
;; the _local copy_ of the url that they receive, like this:
;;
;;   (brfool "http://some/url")
;;     --> (find-foo (ee-url-to-local-url "http://some/url"))
;;     --> (find-foo (concat "file://" (ee-expand "$S/http/some/url")))
;;
;;   (brbarl "http://some/url")
;;     --> (find-bar (ee-url-to-fname "http://some/url"))
;;     --> (find-bar (ee-expand "$S/http/some/url"))
;;
;; Try the sexps above with:
;;
;;   (defun find-foo (url)   (find-2a nil '(find-estring url)))
;;   (defun find-bar (fname) (find-2a nil '(find-estring fname)))
;;   (code-brurl  'find-foo :remote 'brfoo :local 'brfool :dired 'brfood)
;;   (code-brfile 'find-bar                :local 'brbarl :dired 'brbard)
;;
;; The idea of local copies is explained here:
;;
;;   (find-psne-intro "Local copies")
;;
;;
;;
;; This file, i.e.: (find-eev "eev-brxxx.el")
;; supersedes:      (find-eev "eev-browse-url.el")
;; but eev-browse-url.el still has some nice comments that I would
;; like to bring here.


;; «.code-brurl»		(to "code-brurl")
;; «.ee-code-brurl-rest»	(to "ee-code-brurl-rest")
;; «.code-brfile»		(to "code-brfile")
;; «.ee-code-brfile-rest»	(to "ee-code-brfile-rest")
;; «.code-brxxxs»		(to "code-brxxxs")



(require 'eev-code)			; (find-eev "eev-code.el")

(autoload 'browse-url-interactive-arg "browse-url")


;;;                                    _                 
;;;   ___ ___  _ ____   _____ _ __ ___(_) ___  _ __  ___ 
;;;  / __/ _ \| '_ \ \ / / _ \ '__/ __| |/ _ \| '_ \/ __|
;;; | (_| (_) | | | \ V /  __/ |  \__ \ | (_) | | | \__ \
;;;  \___\___/|_| |_|\_/ \___|_|  |___/_|\___/|_| |_|___/
;;;                                                      
;; Here we define some conversion functions that are used by
;; `code-brurl' and `code-brfile'.
;;
;; The `brxxx' functions call the conversion functions defined below,
;; that convert between four kinds of strings: "dired", "url",
;; "fname", and "local-url". The table below explains which function
;; does what; note that "<$S>" means the expansion of "$S", and that
;; if the point is on the file name "b" in a dired buffer visiting the
;; directory "/foo/bar" then `(ee-dired-to-fname)' returns
;; "/foo/bar/b", not something that starts with "$S".
;;
;;  dired     url           fname            local-url
;;   "b"                 "/foo/bar/b"      "file:///foo/bar/b"
;;        "http://a/b"  "$S/http/a/b"  "file:///<$S>/http/a/b"
;;  ========================================================== 
;;             *                                      (code-brurl _ :remote _)
;;             * |-----------------------------> *    (code-brurl _ :local  _)
;;    * |--------------------------------------> *    (code-brurl _ :dired  _)
;;
;;             * |----------> *                       (code-brfile _ :local _)
;;    * |-------------------> *                       (code-brfile _ :dired _)
;;                            
;;             * |----------> *                       ee-url-to-fname
;;                            * |--------------> *    ee-fname-to-url
;;             * |-----------------------------> *    ee-url-to-local-url
;;    * |-------------------> *                       ee-dired-to-fname
;;    * |--------------------------------------> *    ee-dired-to-url

(defun ee-url-to-fname0 (url)
  "Convert an url like http://foo/bar to a filename like $S/http/foo/bar."
  (replace-regexp-in-string "^\\(https?\\|ftp\\)://" "$S/\\1/" url))

(defun ee-url-to-fname (url)
  "Convert an url like http://foo/bar to a filename like <$S>/http/foo/bar.
\(The \"<$S>\" above means the expansion of \"$S\"; see `ee-expand')."
  (ee-expand (ee-url-to-fname0 url)))

(defun ee-fname-to-url (fname)
  "Convert a filename to a \"file://\" url (after running `ee-expand' on it)."
  (concat "file://" (expand-file-name (ee-expand fname))))

(defun ee-url-to-local-url (url)
  "Convert a url like http://foo/bar to a url like file://<$S>/http/foo/bar.
This should be made smarter - file:// urls should be returned unchanged."
  ;; Add comments about psne and the snarf directory
  (ee-fname-to-url (ee-url-to-fname url)))

(defun ee-dired-to-fname (&optional no-error-if-not-filep)
  "Convert the file name at point (in dired mode) to an absolute file name."
  (if (eq major-mode 'dired-mode)
      (file-name-sans-versions (dired-get-filename nil no-error-if-not-filep) t)
    (error "Not in dired mode")))

(defun ee-dired-to-url (&optional no-error-if-not-filep)
"Convert the file name at point (in dired mode) to a url like file://<$S>/___."
  (ee-fname-to-url (ee-dired-to-fname no-error-if-not-filep)))




;; An internal function used by `ee-code-brurl-rest' and
;; `ee-code-brfile-rest'. Similar to:
;; (find-eev "eev-code.el" "ee-tail-call2")

(defun ee-tail-call1 (fmt f rest)
  "An internal function used to support keyword-argument pairs."
  (cond ((null rest) "")
	((keywordp (car rest))
	 (apply (intern (format fmt (car rest)))
		f (cdr rest)))
	(t (error "Wrong rest: %S" rest))))



;;;                _            _                     _ 
;;;   ___ ___   __| | ___      | |__  _ __ _   _ _ __| |
;;;  / __/ _ \ / _` |/ _ \_____| '_ \| '__| | | | '__| |
;;; | (_| (_) | (_| |  __/_____| |_) | |  | |_| | |  | |
;;;  \___\___/ \__,_|\___|     |_.__/|_|   \__,_|_|  |_|
;;;                                                     
;; (find-tail-call-links "brurl" "f")

;; «code-brurl» (to ".code-brurl")
;; Test: (find-code-brurl  'find-foo :remote 'brfoo :local 'brfool :dired 'brfood)
;;
(defun      code-brurl (f &rest rest)
  "Define a family of brxxx functions from a function that operates on URLs"
  (eval (ee-read      (apply 'ee-code-brurl f rest))))
(defun find-code-brurl (f &rest rest)
  (find-estring-elisp (apply 'ee-code-brurl f rest)))
(defun   ee-code-brurl (f &rest rest)
"Generate code for a family of functions from a function that operates on URLs"
  (concat (ee-template0 "\
;; {(ee-S `(find-code-brurl ',f ,@(mapcar 'ee-add-quote rest)))}
")  (ee-code-brurl-rest f rest)))

;; «ee-code-brurl-rest» (to ".ee-code-brurl-rest")
;; Support for extra arguments

(defun ee-code-brurl-rest (f rest)
  (cond ((null rest) "")
	((keywordp (car rest))
	 (apply (intern (format "ee-code-brurl-%S" (car rest)))
		f (cdr rest)))
	(t (error "Wrong rest: %S" rest))))

(defun ee-code-brurl-:remote (f brxxx &rest rest)
  (concat (ee-template0 "
\(defun {brxxx} (url &rest ignore)
  \"Apply `{f}' on URL.\"
  (interactive (browse-url-interactive-arg \"URL: \"))
  (setq browse-url-browser-function '{brxxx})
  (message \"(%S %S) -> %S\" '{f} url
	                   ({f} url)))
")  (ee-code-brurl-rest f rest)))

(defun ee-code-brurl-:local (f brxxxl &rest rest)
  (concat (ee-template0 "
\(defun {brxxxl} (url &rest ignore)
  \"Apply `{f}' on the local url associated to URL.\"
  (interactive (browse-url-interactive-arg \"URL: \"))
  (setq browse-url-browser-function '{brxxxl})
  (setq url (ee-url-to-local-url url))
  (message \"(%S %S) -> %S\" '{f} url
	                   ({f} url)))
")  (ee-code-brurl-rest f rest)))

(defun ee-code-brurl-:dired (f brxxxd &rest rest)
  (concat (ee-template0 "
\(defun {brxxxd} (&rest ignore)
  \"Apply `{f}' on the url of the dired file at point.\"
  (interactive)
  (let ((url (ee-dired-to-url)))
    (message \"(%S %S) -> %S\" '{f} url
                             ({f} url))))
")  (ee-code-brurl-rest f rest)))

;; Test:
;; (find-code-brurl 'pluc :remote 'brpluc :local 'brplucl :dired 'brplucd)
;;      (code-brurl 'pluc :remote 'brpluc :local 'brplucl :dired 'brplucd)
;;  
;; (find-efunction 'find-brxxx-intro)
;; (find-brxxx-intro "M-x brpluc")





;;;                _            _           __ _ _      
;;;   ___ ___   __| | ___      | |__  _ __ / _(_) | ___ 
;;;  / __/ _ \ / _` |/ _ \_____| '_ \| '__| |_| | |/ _ \
;;; | (_| (_) | (_| |  __/_____| |_) | |  |  _| | |  __/
;;;  \___\___/ \__,_|\___|     |_.__/|_|  |_| |_|_|\___|
;;;                                                     
;; (find-tail-call-links "brfile" "f")

;; «code-brfile» (to ".code-brfile")
;; code-brfile: top-level functions
;;
;; Test: (find-code-brfile 'find-bar :local 'brbarl :dired 'brbard)
;;
(defun      code-brfile (f &rest rest)
  "Define a family of brxxx functions from a function that operates on files"
  (eval (ee-read      (apply 'ee-code-brfile f rest))))
(defun find-code-brfile (f &rest rest)
  (find-estring-elisp (apply 'ee-code-brfile f rest)))
(defun   ee-code-brfile (f &rest rest)
"Generate code for a family of functions from a function that operates on files"
  (concat (ee-template0 "\
;; {(ee-S `(find-code-brfile ',f ,@(mapcar 'ee-add-quote rest)))}
")  (ee-code-brfile-rest f rest)))

;; «ee-code-brfile-rest» (to ".ee-code-brfile-rest")
;; Support for extra arguments

(defun ee-code-brfile-rest (f rest)
  (cond ((null rest) "")
	((keywordp (car rest))
	 (apply (intern (format "ee-code-brfile-%S" (car rest)))
		f (cdr rest)))
	(t (error "Wrong rest: %S" rest))))

(defun ee-code-brfile-:local (f brxxxl &rest rest)
  (concat (ee-template0 "
\(defun {brxxxl} (url &rest ignore)
  \"Apply `{f}' on the local file name associated to URL.\"
  (interactive (browse-url-interactive-arg \"URL: \"))
  (setq browse-url-browser-function '{brxxxl})
  (let ((fname (ee-url-to-fname url)))
    (message \"(%S %S) -> %S\" '{f} fname
                             ({f} fname))))
")  (ee-code-brfile-rest f rest)))

(defun ee-code-brfile-:dired (f brxxxd &rest rest)
  (concat (ee-template0 "
\(defun {brxxxd} (&rest ignore)
  \"Apply `{f}' on the dired file at point.\"
  (interactive)
  (let ((fname (ee-dired-to-fname)))
    (message \"(%S %S) -> %S\" '{f} fname
                             ({f} fname))))
")  (ee-code-brfile-rest f rest)))




;;;                _            _                               
;;;   ___ ___   __| | ___      | |__  _ ____  ____  ____  _____ 
;;;  / __/ _ \ / _` |/ _ \_____| '_ \| '__\ \/ /\ \/ /\ \/ / __|
;;; | (_| (_) | (_| |  __/_____| |_) | |   >  <  >  <  >  <\__ \
;;;  \___\___/ \__,_|\___|     |_.__/|_|  /_/\_\/_/\_\/_/\_\___/
;;;                                                             
;; «code-brxxxs» (to ".code-brxxxs")
;; See: (find-eev-quick-intro "3.1. Non-elisp hyperlinks")
;;      (find-eev-quick-intro "3.1. Non-elisp hyperlinks" "brg")
;;      (find-eev "eev-pdflike.el" "code-brxxxs")
;;      (find-eev "eev-blinks.el" "find-w3m")
;;      (find-efile "net/browse-url.el")

(defun find-googlechrome (url) (find-bgprocess `("google-chrome" ,url)))
(defun find-firefox      (url) (find-bgprocess `("firefox"       ,url)))

(code-brurl 'find-psne-links       :remote 'brep)

;; (code-brurl 'browse-url-firefox :remote 'brm  :local 'brml  :dired 'brmd)
;; (code-brurl 'browse-url-firefox :remote 'brff :local 'brffl :dired 'brffd)
(code-brurl 'find-firefox          :remote 'brm  :local 'brml  :dired 'brmd)
(code-brurl 'find-firefox          :remote 'brff :local 'brffl :dired 'brffd)
(code-brurl 'find-googlechrome     :remote 'brg  :local 'brgl  :dired 'brgd)
(code-brurl 'find-w3m              :remote 'brw  :local 'brwl  :dired 'brwd)

(code-brurl 'find-wget             :remote 'brwget)

(code-brfile 'find-fline                         :local 'brfl)




;; These are defined elsewhere.
;;   (code-brfile 'find-xpdf-page   :local 'brxpdfl     :dired 'brxpdfd)
;;   (code-brfile 'find-evince-page :local 'brevincel   :dired 'brevinced)
;;   (code-brfile 'find-xdvi-page   :local 'brxdvil     :dired 'brxdvid)
;;   (code-brfile 'find-pdf-text    :local 'brpdftextl  :dired 'brpdftextd)
;;   (code-brfile 'find-djvu-text   :local 'brdjvutextl :dired 'brdjvutextd)
;; See: (find-eev "eev-pdflike.el")

;; These too...
;;   (code-brfile 'find-video :local 'brvideol :dired 'brvideod)
;;   (code-brfile 'find-audio :local 'braudiol :dired 'braudiod)
;; See: (find-eev "eev-audiovideo.el")

;; Some obsolete definitions (with the old syntax):
;;   (eeurl-define-from :fname->action: 'eecd       :local:  'brcdl)
;;   (eeurl-define-from :url->action:   'eepsne     :remote: 'brp)
;;   (eeurl-define-from :url->action:   'eetmpwget  :remote: 'brtmpwget)

;; (find-eevgrep "grep -nH -e brg *.el")




(provide 'eev-brxxx)




;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
