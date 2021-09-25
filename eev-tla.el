;;; eev-tla.el --- eev links based on TLAs, i.e., three-letter acronyms.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.
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
;; Version:    20210618
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-tla.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-tla.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-here-links-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-here-links-intro)

;;; Commentary:

;; This is a new, experimental feature that is not loaded by default.
;; It is a better version of a tool that I've used for some years to
;; index both the sources and the PDFs of my LaTeX files.
;;
;; In 2018 I extended the idea of "very short names" to "three letter
;; acronyms" (TLAs), and I started to use TLAs to point to my .tex
;; files; for example, the `M-x lod' would open the file
;; ~/LATEX/2019logicday.tex, `M-x lodp' would display its PDF, and
;; these two sexps
;;
;;   (lodp 6 "set-comprehensions")
;;   (lod    "set-comprehensions")
;;
;; would point to a) the page 6 of the PDF and b) to the anchor
;; "<<set-comprehensions>>" in the .tex file... after each section
;; anchor in the .tex I had a pair of sexps like those two above, that
;; I could copy to other files to use as hyperlinks to both the output
;; and the source of that section. With this my LaTeXing life became
;; much better - because with this I had a very good way to point to
;; my LaTeX tricks!...
;;
;; In 2021 I sent an e-mail to Erich Ruff after seeing this e-mail
;; that he sent to the Hyperbole mailing list:
;;
;;   https://lists.gnu.org/archive/html/hyperbole-users/2021-05/msg00042.html
;;
;; we exchanged ideas in a series of private e-mails, and I wrote this
;; prototype to see if it could be a convenient way to make his long
;; sexp hyperlinks shorter.
;;
;; This version supposes that we have a bijection between TLAs
;; (symbols) and file names (strings). In my other, older,
;; implementation I sometimes had two TLAs associated to the same file
;; name, like this:
;;
;;   c2m202fp -> "~/LATEX/2020-2-C2-fracs-parcs.pdf"
;;   c2fp     -> "~/LATEX/2020-2-C2-fracs-parcs.pdf"
;;
;; note that both `c2fp' and `c2m202fp' are symbols longer than three
;; characters, but I'd refer to both of them as "TLA"s anyway.
;;
;; Here are the design principles that I used:
;; 
;;   1. We may have hundreds of files that we want to create TLA-links
;;      for. A "TLA" - a "three-letter acronym" may be longer than
;;      three characters.
;; 
;;   2. It should be easy to create a TLA, and a `code-tla', for a new
;;      file.
;; 
;;   3. Some of the files that we may want to access with TLAs may be
;;      read-only in some sense - so it's not feasible to define the
;;      TLAs associated to them in their "Local Variables:" sections,
;;      because we do not want to change them to add local variables.
;;      See:
;; 
;;        (find-enode "File Variables" ";;; Local Variables:")
;; 
;;   4. Creating links to a file that has a TLA associated to it has
;;      to be very fast. If we are in
;; 
;;        /mnt/fichte/fuchs_erich-fichte_im_gespraech_1_1762-1798.txt
;; 
;;      and we are after the anchor "<<Fichte Charakter>>", with the
;;      region being the string "some text that is not an anchor",
;;      then there must be a short sequence of keystrokes that
;;      produces a temporary buffer containing these lines, among
;;      other stuff:
;; 
;;        (fim1a "Fichte Charakter")
;;        (fim1 "some text that is not an anchor")
;; 
;;      The conversion from the file name and the `fim1' is done using
;;      a hash table.
;; 
;;   5. If we create a TLA for a file whose name ends in .tex, like this,
;; 
;;        (code-tla "lod" "~/LATEX/2019logicday.tex")
;; 
;;      then the `code-tla' should also create functions `lodp' and
;;      `lodt' such that
;; 
;;         (lodp 6 "set-comprehensions")
;;         (loda   "set-comprehensions")
;;         (lodt "calculate")
;; 
;;      are equivalent to:
;; 
;;         (find-pdf-page "~/LATEX/2019logicday.tex" 6)
;;         (find-anchor   "~/LATEX/2019logicday.tex" "set-comprehensions")
;;         (find-pdf-text "~/LATEX/2019logicday.tex" "calculate")


;; To try this, do:
;;   (load "eev-tla.el")
;; and then use `M-3 M-j' to generate the temporary buffers,
;; and `M-3 M-3 M-j' to insert a {tla}p/{tla}a pair.



;; «.hash-table»		(to "hash-table")
;; «.code-tla»			(to "code-tla")
;; «.find-tla-def-links»	(to "find-tla-def-links")
;; «.find-tla-links»		(to "find-tla-links")
;;
;; «.find-pdf-txt»		(to "find-pdf-txt")
;; «.find-pdf-txt-links»	(to "find-pdf-txt-links")



;; «hash-table»  (to ".hash-table")

(setq ee-tla-table (make-hash-table :test 'equal))

;; Tests: (ee-tla-canonicalize nil)
;;        (ee-tla-canonicalize "/home/edrx/foo")
(defun ee-tla-canonicalize (o)
  (if (stringp o) (ee-shorten-file-name (ee-expand o)) o))

;; Here the argument tla has to be a symbol.
(defun ee-tla-set (tla fname)
  (setq fname (ee-tla-canonicalize fname))
  (puthash fname tla ee-tla-table)
  (puthash tla fname ee-tla-table))

(defun ee-tla-get    (o) (gethash o ee-tla-table))
(defun ee-tla-remove (o) (remhash o ee-tla-table))
(defun ee-tla-fname  () (ee-tla-canonicalize (buffer-file-name)))
(defun ee-tla-tla    () (ee-tla-get (ee-tla-fname)))

;; Based on: (find-eev "eev-edit.el" "ee-copy-this-line-to-kill-ring")
(defun ee-tla-tag ()
  (save-excursion
    (if (re-search-backward (ee-tag-re) nil 'no-error)
	(ee-no-properties (match-string 1)))))



;; Test: (find-estring (ee-tla-table-to-string))
;;
(defun ee-tla-table-to-string ()
  (let ((lines (cl-loop for k being the hash-keys of ee-tla-table
			collect (format "%S -> %S\n" k (ee-tla-get k)))))
    (apply 'concat (sort lines 'string<))))

;; Tests:
;; (code-tla "ats" "~/LATEX/2020ats.tex")
;; (code-tla "pyt" "~/LATEX/2020pythontex.tex")
;; (code-tla "dnv" "~/LATEX/2020dednat6-video.tex")
;; (code-tla "qui" "~/LATEX/2020quiver.tex")
;; (code-tla "grt" "~/LATEX/2020groth-tops.tex")
;; (code-tla "grd" "~/LATEX/2021groth-tops-defs.tex")
;; (code-tla "grc" "~/LATEX/2021groth-tops-children.tex")
;; (code-tla "grs" "~/LATEX/2021groth-tops-children-slides.tex")
;; (code-tla "has" "~/LATEX/2021haskell.tex")
;; (code-tla "exc" "~/LATEX/2021excuse.tex")
;; (grt)
;; (grta "grotop-J")
;; (grta "grotop-J" "first")
;; (grtp 11 "grotop-J" "first")


;;;                _            _   _       
;;;   ___ ___   __| | ___      | |_| | __ _ 
;;;  / __/ _ \ / _` |/ _ \_____| __| |/ _` |
;;; | (_| (_) | (_| |  __/_____| |_| | (_| |
;;;  \___\___/ \__,_|\___|      \__|_|\__,_|
;;;                                         
;; «code-tla»  (to ".code-tla")
;;  Skel: (find-code-xxx-links "tla" "tla fname" "")
;; Tests: (find-code-tla "qux" "~/LATEX/2019J-ops-algebra.tex")
;;        (find-code-tla 'qux  "~/LATEX/2019J-ops-algebra.tex")
;;        (find-code-tla "qux" "~/LATEX/2019J-ops-algebra.txt")
;;        (find-code-tla 'qux  "~/LATEX/2019J-ops-algebra.txt")
;; Note that here the first argument can be either a string or a
;; symbol - try the tests above!

(defun      code-tla (tla fname)
  (eval (ee-read      (ee-code-tla tla fname))))
(defun find-code-tla (tla fname)
  (find-estring-elisp (ee-code-tla tla fname)))
(defun   ee-code-tla (tla fname)
  (let* ((fnamepdf (ee-tla-tex-to-pdf fname))
	 (haspdf (stringp fnamepdf)))
    (concat
     (ee-template0 "\
;; (find-code-tla \"{tla}\" \"{fname}\")
;;      (code-tla \"{tla}\" \"{fname}\")

(ee-tla-set '{tla} \"{fname}\")

(defun {tla} (&rest pos-spec-list)
  (interactive)
  (apply 'find-fline \"{fname}\" pos-spec-list))
(defun {tla}a (&rest pos-spec-list)
  (apply 'find-anchor \"{fname}\" pos-spec-list))
")
     (if haspdf
	 (ee-template0 "
(defun {tla}p (&optional page &rest rest)
  (interactive)
  (find-pdf-page \"{fnamepdf}\" page))
(defun {tla}t (&optional page &rest rest)
  (interactive)
  (apply 'find-pdf-text \"{fnamepdf}\" page rest))
") "")
     )))

;; Tests: (ee-tla-tex-to-pdf "~/LATEX/2019J-ops-algebra.foo")
;;        (ee-tla-tex-to-pdf "~/LATEX/2019J-ops-algebra.tex")
;;
(defun ee-tla-tex-to-pdf (fname)
  (if (string-match ".tex$" fname)
      (replace-regexp-in-string ".tex$" ".pdf" fname)))




;; «find-tla-def-links»  (to ".find-tla-def-links")
;; Skel: (find-find-links-links-new "tla-def" "fname tla" "haspdf")
;;
(defun find-tla-def-links (&optional fname tla &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for tla-def."
  (interactive)
  (setq fname (or fname "{fname}"))
  (setq tla (or tla "{tla}"))
  (let* ((haspdf "{haspdf}"))
    (apply
     'find-elinks-elisp
     `((find-tla-def-links ,fname ,tla ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-efunction 'find-tla-def-links)
       ,(ee-template0 "\
;; This file doesn't have a TLA. Change the last argument above...

;; (find-estring (ee-tla-table-to-string))
;; (find-fline \"~/.emacs\"      \"(code-tla '{tla} \")
;; (find-fline \"~/.emacs.tlas\" \"(code-tla '{tla} \")
;; (ee-copy-rest 2 '(find-fline \"~/.emacs\"))
;; (ee-copy-rest 1 '(find-fline \"~/.emacs.tlas\"))
;; (find-code-tla '{tla} \"{fname}\")
(code-tla '{tla} \"{fname}\")\
")
       )
     pos-spec-list)))



;; «find-tla-links»  (to ".find-tla-links")
;; Skel: (find-find-links-links-new "tla" "tla fname tag" "haspdf")
;; Test: (find-tla-links)
;;
(defun find-tla-links (&optional tla fname tag &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for tla."
  (interactive)
  (setq tla (or tla "{tla}"))
  (setq fname (or fname "{fname}"))
  (setq tag (or tag "{tag}"))
  (let* ((haspdf "{haspdf}"))
    (apply
     'find-elinks-elisp
     `((find-tla-links ',tla ,fname ,tag ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-efunction 'find-tla-links)
       ""
       ,(ee-template0 "\
;; Links to here:
({tla})
({tla}a \"{tag}\")

({tla}p 1 \"{tag}\")
({tla}a   \"{tag}\")
({tla}t 1)

;; Inspect or remove the current association:
;; (find-estring (ee-tla-table-to-string) \"{tla} ->\")
;; (find-estring (ee-tla-table-to-string) \"-> {tla}\")
;; (ee-tla-remove '{tla})
;; (ee-tla-remove \"{fname}\")

;; Save the current association:
;; (find-fline \"~/.emacs\"      \"(code-tla '{tla} \")
;; (find-fline \"~/.emacs.tlas\" \"(code-tla '{tla} \")
;; (ee-copy-rest 1 '(find-fline \"~/.emacs\"))
;; (ee-copy-rest 0 '(find-fline \"~/.emacs.tlas\"))
(code-tla '{tla} \"{fname}\")\
")
       )
     pos-spec-list)))





;; (ee-tla-remove 'etl)
;; (ee-tla-remove (ee-tla-fname))
;; (find-estring (ee-tla-table-to-string))
;; (eejump-3)

(defun find-tla-here-links ()
  "Visit a temporary buffer containing hyperlinks for TLAs.
If the current file has a TLA associated to it, run `find-tla-links';
if it doesn't, run `find-tla-def-links'.\n
See: (find-eevfile \"eev-tla.el\" \";; Commentary:\")"
  (interactive)
  (let* ((fname (ee-tla-fname))
	 (tla (ee-tla-tla))
	 (tag (ee-tla-tag)))
    (if tla (find-tla-links tla fname tag)
      (find-tla-def-links fname))))
	 
(defun eejump-3 () (find-tla-here-links))


;; Tests: (find-estring (ee-tla-link 'foo 99 "TaG"))
;;        (find-estring (ee-tla-link 'foo 9 "TaG"))
;;        (find-estring (ee-tla-link))
;;
(defun ee-tla-link (&optional tla n tag)
  (setq tla (or tla "{tla}"))
  (setq n   (format "%s" (or n 1)))
  (setq tag (or tag "{tag}"))
  (let ((s (replace-regexp-in-string "." " " n)))
    (ee-template0 "\
% ({tla}p {n} \"{tag}\")
% ({tla}a {s} \"{tag}\")
")))

(defun eejump-33 ()
  (eek "C-a")
  (insert (ee-tla-link (ee-tla-tla) 99 (ee-tla-tag))))



;; Let's make `tla' point to this file,
(code-tla 'tla (ee-eevfile "eev-tla.el"))
;; so that people will start with a non-empty
;; `ee-tla-table'. Try:
;;   (find-estring (ee-tla-table-to-string) "tla ->")
;;   (find-estring (ee-tla-table-to-string) "-> tla")




;;;   __ _           _                 _  __       _        _   
;;;  / _(_)_ __   __| |      _ __   __| |/ _|     | |___  _| |_ 
;;; | |_| | '_ \ / _` |_____| '_ \ / _` | |_ _____| __\ \/ / __|
;;; |  _| | | | | (_| |_____| |_) | (_| |  _|_____| |_ >  <| |_ 
;;; |_| |_|_| |_|\__,_|     | .__/ \__,_|_|        \__/_/\_\\__|
;;;                         |_|                                 
;;
;; «find-pdf-txt»  (to ".find-pdf-txt")
;; This is a variant of `find-pdf-text' that implements a part of what
;; Erich Ruff suggested here:
;;   https://lists.gnu.org/archive/html/eev/2021-06/msg00013.html
;;
;; These two sexps are similar, but the second uses a saved ".txt":
;;   (find-pdf-text "~/Coetzee99.pdf" 3 "LECTURE I")
;;   (find-pdf-txt  "~/Coetzee99.pdf" 3 "LECTURE I")
;;
;; This is a VERY EARLY prototype.
;; I haven't yet tried to integrate this with `code-tla' or with:
;;
;;   (find-pdf-like-intro "7. Shorter hyperlinks to PDF files")
;;   (find-pdf-like-intro "8. `find-pdf'-pairs")
;;   (code-pdf-page "livesofanimals" "~/Coetzee99.pdf")
;;   (code-pdf-text "livesofanimals" "~/Coetzee99.pdf" -110)
;;
;; TODO: a `code-pdf-txt' that works like `code-pdf-text' but uses
;; `find-pdf-txt' instead of `find-pdf-text'.


;; Test: (find-pdf-text   "~/Coetzee99.pdf")
;;       (find-pdf-text   "~/Coetzee99.pdf" 1)
;;       (find-pdf-text   "~/Coetzee99.pdf" 3)
;;       (find-pdf-text   "~/Coetzee99.pdf" 3 "LECTURE I")
;;       (find-sh0 "rm -fv ~/Coetzee99.txt ~/Coetzee99.txt~")
;;       (find-pdf-txt    "~/Coetzee99.pdf")
;;       (find-pdf-txt    "~/Coetzee99.pdf" 3)
;;       (find-pdf-txt    "~/Coetzee99.pdf" 3 "LECTURE I")
;;  See: (find-efunctionpp          'find-pdf-text)
;;       (find-efunctionpp          'find-pdftotext-text)
;;       (find-eev "eev-pdflike.el" "find-pdftotext-text")
;;       (find-eev "eev-pdflike.el" "find-sh-page")
;;       (find-eev "eev-pdflike.el" "ee-goto-position-page")
;;
(defun find-pdf-txt (fnamepdf &rest pos-spec-list)
"Open the .txt associated to FNAMEPDF or run `find-pdf-txt-links' to create it."
  (let ((fnametxt (ee-fnamepdf-to-fnametxt fnamepdf)))
    (if (file-exists-p fnametxt)
	(progn (find-fline fnametxt)
	       (apply 'ee-goto-position-page pos-spec-list))
      (find-pdf-txt-links fnamepdf))))

;; «find-pdf-txt-links»  (to ".find-pdf-txt-links")
;; Test: (find-sh0    "rm -fv ~/Coetzee99.txt ~/Coetzee99.txt~")
;;       (find-pdf-txt-links "~/Coetzee99.pdf")
;; Skel: (find-find-links-links-new "pdf-txt" "fnamepdf" "fnametxt")
;;
(defun find-pdf-txt-links (&optional fnamepdf &rest pos-spec-list)
"`find-pdf-txt' runs this when the .txt file for FNAMEPDF does not exist."
  (interactive)
  (setq fnamepdf (or fnamepdf "{fnamepdf}"))
  (let* ((fnametxt (or (ee-fnamepdf-to-fnametxt fnamepdf) "{fnametxt}")))
    (apply
     'find-elinks
     `((find-pdf-txt-links ,fnamepdf ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-efunction 'find-pdf-txt-links)
       ""
       (find-pdf-text-insert 3 ,fnamepdf)
       (ee-copy-rest 1 '(find-fline ,fnametxt))
       )
     pos-spec-list)))

;; Tests: (ee-fnamepdf-to-fnametxt "~/Coetzee99.pdf")
;;        (ee-fnamepdf-to-fnametxt "~/Coetzee99.FOO")
;;
(defun ee-fnamepdf-to-fnametxt (fnamepdf)
  (if (string-match ".pdf$" fnamepdf)
      (replace-regexp-in-string ".pdf$" ".txt" fnamepdf)))

;; Tests: (ee-find-pdftotext-text "~/Coetzee99.pdf")
;;        (find-estring "(find-pdf-text-insert 2 \"~/Coetzee99.pdf\")")
;;   See: (find-efunctionpp          'find-pdf-text)
;;        (find-efunctionpp          'find-pdftotext-text)
;;        (find-eev "eev-pdflike.el" "find-pdftotext-text")
;;        (find-efunction         'ee-find-pdftotext-text)
;;        (find-eev "eev-plinks.el"  "find-callprocess")
;;
(defun find-pdf-text-insert (nlines fnamepdf)
  "Move down NLINES and insert FNAMEPDF converted to text."
  (save-excursion
    (let ((next-line-add-newlines t))
      (dotimes (i nlines) (next-line 1)))
    (insert (find-callprocess00 (ee-find-pdftotext-text fnamepdf)))))




(provide 'eev-tla)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
