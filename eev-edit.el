;;; eev-edit.el -- tools for editing (mainly refining) elisp hyperlinks.

;; Copyright (C) 2012,2019 Free Software Foundation, Inc.
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
;; Latest version: <http://angg.twu.net/eev-current/eev-edit.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-edit.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:

;; See:
;;   (find-eval-intro "9. Producing and refining hyperlinks")
;;   (find-escripts-intro "5.7. Refining hyperlinks")
;;   (find-escripts-intro "5.8. Pointing to anchors")
;;   (find-eevfile "eev-mode.el" "Keys for refining hyperlinks")
;;
;; `M-h M--', `M-s' and `M-I' are not yet explained in the intros!



;; «.ee-duplicate-this-line»		(to "ee-duplicate-this-line")
;; «.ee-yank-pos-spec»			(to "ee-yank-pos-spec")
;; «.ee-shrink-hyperlink-at-eol»	(to "ee-shrink-hyperlink-at-eol")
;; «.ee-flip-psne-ness»			(to "ee-flip-psne-ness")
;; «.eewrap-vldi-list-line»		(to "eewrap-vldi-list-line")




;;;      _             _ _           _       
;;;   __| |_   _ _ __ | (_) ___ __ _| |_ ___ 
;;;  / _` | | | | '_ \| | |/ __/ _` | __/ _ \
;;; | (_| | |_| | |_) | | | (_| (_| | ||  __/
;;;  \__,_|\__,_| .__/|_|_|\___\__,_|\__\___|
;;;             |_|                          
;;
;; «ee-duplicate-this-line» (to ".ee-duplicate-this-line")
;; (define-key eev-mode-map "\M-h\M-2" 'ee-duplicate-this-line)

(defun ee-duplicate-this-line ()
  "Duplicate the current line (without any changes to the kill ring)."
  (interactive)
  (let ((line (buffer-substring (ee-bol) (ee-eol))))
    (save-excursion (beginning-of-line) (insert-before-markers line "\n"))))


;;;                    _    
;;;  _   _  __ _ _ __ | | __
;;; | | | |/ _` | '_ \| |/ /
;;; | |_| | (_| | | | |   < 
;;;  \__, |\__,_|_| |_|_|\_\
;;;  |___/                  
;;
;; «ee-yank-pos-spec» (to ".ee-yank-pos-spec")
;; (define-key eev-mode-map "\M-h\M-y" 'ee-yank-pos-spec)

(defun ee-yank-pos-spec ()
  "Append the top of the kill ring to a hyperlink sexp, as a Lisp string.
This command is useful for \"refining elisp hyperlinks\" by adding a
pos-spec argument to them.  Here's an example; if you are using the
default `eev-mode-map' keybindings then

  `M-h M-i' runs `find-einfo-links',
  `M-h M-2' runs `ee-duplicate-this-line', and
  `M-h M-y' runs `ee-yank-pos-spec'.

Suppose that you are visiting the info node below,

  (find-enode \"Lisp Eval\")

and you find some interesting information in that page, close to
an occurrence of the string \"`defvar'\". You mark that string,
add it to the kill-ring with `M-w', then type `M-h M-i', go to
the line that contains

  # (find-enode \"Lisp Eval\")

and then you type `M-h M-2 M-h M-y'; it becomes these two lines:

  # (find-enode \"Lisp Eval\")
  # (find-enode \"Lisp Eval\" \"`defvar'\")

Now you check that the second line points to where you wanted,
and you copy that hyperlink to a more permanent place."
  (interactive)
  (goto-char (1- (point-at-eol)))	; put point before the ")"
  (insert " " (ee-pp0 (ee-last-kill)))) ; insert pos-spec



;;;      _          _       _    
;;;  ___| |__  _ __(_)_ __ | | __
;;; / __| '_ \| '__| | '_ \| |/ /
;;; \__ \ | | | |  | | | | |   < 
;;; |___/_| |_|_|  |_|_| |_|_|\_\
;;;                              
;; «ee-shrink-hyperlink-at-eol» (to ".ee-shrink-hyperlink-at-eol")
;; (define-key eev-mode-map "\M-h\M--" 'ee-shrink-hyperlink-at-eol)

(defun ee-shrink-sexp (sexp)
  "If the car of SEXP of the form `find-xxxfile', reduce it to `find-xxx'."
  (if (eq (car sexp) 'find-esfile)
      `(find-es ,(substring (nth 1 sexp) 0 -2) ,@(cddr sexp))
    (let* ((headname (symbol-name (car sexp)))
	   (last4chars (substring headname -4))
	   (-last4chars (substring headname 0 -4)))
      (if (equal last4chars "file")
	  `(,(intern -last4chars) ,@(cdr sexp))))))

(defun ee-shrink-hyperlink-at-eol ()
  (interactive)
  (end-of-line)
  (let* ((beg (save-excursion (ee-backward-sexp)))
	 (sexp (read (buffer-substring beg (point))))
	 (shrunksexp (ee-shrink-sexp sexp)))
    (when shrunksexp
      (delete-region beg (point))
      (insert (ee-pp0 shrunksexp)))))



;;;   __ _ _                                  
;;;  / _| (_)_ __        _ __  ___ _ __   ___ 
;;; | |_| | | '_ \ _____| '_ \/ __| '_ \ / _ \
;;; |  _| | | |_) |_____| |_) \__ \ | | |  __/
;;; |_| |_|_| .__/      | .__/|___/_| |_|\___|
;;;         |_|         |_|                   
;;
;; «ee-flip-psne-ness» (to ".ee-flip-psne-ness")
;; (define-key eev-mode-map "\M-s" 'ee-flip-psne-ness)

;; (find-eevfile "eev-insert.el" "defun ee-ill")

(defun ee-flip-psne-ness ()
  (interactive)
  (if (search-forward-regexp "\\$S/\\(https?\\|ftp\\)/\\|\\(https?\\|ftp\\)://")
      (cond ((match-string 1) (replace-match "\\1://"))
            ((match-string 2) (replace-match "$S/\\2/")))))






;;;        _     _ _       _ _     _        _ _            
;;; __   _| | __| (_)     | (_)___| |_     | (_)_ __   ___ 
;;; \ \ / / |/ _` | |_____| | / __| __|____| | | '_ \ / _ \
;;;  \ V /| | (_| | |_____| | \__ \ ||_____| | | | | |  __/
;;;   \_/ |_|\__,_|_|     |_|_|___/\__|    |_|_|_| |_|\___|
;;;                                                        
;; «eewrap-vldi-list-line» (to ".eewrap-vldi-list-line")
;; (define-key eev-mode-map "\M-I" 'eewrap-vldi-list-line)
;;
;; In a Debian system, for each installed package named xxxx there is
;; an associated file, at /var/lib/dpkg/info/xxxx.list - let's call
;; that the "vldi list" associated to the package xxxx - that lists
;; all the files installed by that package...
;;
;; To convert a vldi list to hyperlinks, copy it to a read-write
;; buffer and run M-I on each of its lines. More details later - this
;; is a hack!
;;
;; Old version: (find-eev "eev-insert.el" "ee-ill")

(defun eewrap-vldi-list-line () (interactive)
  "Convert a filename at the current line into a hyperlink, and go down.
Supports `find-man', `find-udfile', and `find-fline' hyperlinks.
This function recognizes lines containing directory names, and
handles them in the following way: if the current line contains a
directory name, say, /foo/bar, and the next line contains the
name of a file or a directory in /foo/bar, say, /foo/bar/plic,
then just delete the current line."
  (beginning-of-line)
  (if (looking-at "^\\(.*\\)\n\\1/")	; a directory
      (delete-region (point) (progn (ee-next-line 1) (point)))
    (ee-this-line-wrapn 1 'ee-wrap-vldi-list-line)))

(defun ee-wrap-vldi-list-line (line)
  "An internal function used by `eewrap-vldi-list-line'."
  (if (string-match "^.*/man./\\([^/]+\\)\\.\\([0-9A-Za-z]+\\)\\.gz$" line)
      (format "%s(find-man \"%s %s\")"
	      ee-H (match-string 2 line) (match-string 1 line))
    (if (string-match "^/usr/share/doc/\\(.*\\)$" line)
	(format "%s(find-udfile \"%s\")" ee-H (match-string 1 line))
      (format "%s(find-fline \"%s\")" ee-H line))))

;; (find-vldifile "bash.list")









(provide 'eev-edit)







;; Local Variables:
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
