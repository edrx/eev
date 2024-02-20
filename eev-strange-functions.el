;;; eev-strange-functions.el -- Support for functions defined in strange ways.  -*- lexical-binding: nil; -*-

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
;; Version:    20240219
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-strange-functions.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-strange-functions.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-strange-functions-intro.html>
;;                                               (find-strange-functions-intro)

;;; Comment:

;; MESSY! EXPERIMENTAL! INCOMPLETE! UNFINISHED!

;; «.aliases»	(to "aliases")


(defvar ee-sf-sexp)
(defvar ee-sf-result)



;; Tests:
;; (ee-Qp 42)
;; (ee-Qp 'a)
;; (ee-Qp :ka)
;; (ee-Qrest '(:ka (2 3)))
;; See: (find-efunction 'ee-S)

(defun ee-needs-quote (obj)
  "Return t when OBJ needs a \"'\"."
  (not (or (numberp obj) (stringp obj) (eq obj nil) (eq obj t) (keywordp obj))))

(defun ee-Q (obj)
  (if (ee-needs-quote obj) (format "'%s" (ee-S obj)) (ee-S obj)))

(defun ee-Qrest (rest)
  (mapconcat (lambda (obj) (format " %s" (ee-Q obj))) rest ""))



;;;  _     _       _                     _     _            
;;; / |___| |_ ___| | __ _ ___ _____   _(_) __| | ___  ___  
;;; | / __| __/ __| |/ _` / __/ __\ \ / / |/ _` |/ _ \/ _ \ 
;;; | \__ \ || (__| | (_| \__ \__ \\ V /| | (_| |  __/ (_) |
;;; |_|___/\__\___|_|\__,_|___/___/ \_/ |_|\__,_|\___|\___/ 
;;;                                                         

(defvar ee-sf-1stclassvideo-functions
  '(find-1stclassvideo-def
    find-1stclassvideo-index
    find-1stclassvideo-links))

(defvar ee-sf-1stclassvideo-re
  (rx bos "find-" (group (+ any)) (or "video" "hsubs" "lsubs") eos))

(defun ee-sf-1stclassvideo-stem (f)
  (and (symbolp f)
       (string-match ee-sf-1stclassvideo-re (symbol-name f))
       (match-string 1 (symbol-name f))))

(defun ee-sf-1stclassvideo-p ()
  (pcase ee-sf-sexp
    ((and `(,f ,time . ,rest)
	  (let stem (ee-sf-1stclassvideo-stem f))
	  (guard (stringp stem)))
     (setq ee-sf-result `(ee-sf-1stclassvideo-links ,stem ,time . ,rest)))
    ((and `(,f)
	  (let stem (ee-sf-1stclassvideo-stem f))
	  (guard (stringp stem)))
     (setq ee-sf-result `(ee-sf-1stclassvideo-links ,stem)))
    ((and `(,f ,stem . ,rest)
	  (guard (member f ee-sf-1stclassvideo-functions)))
     (setq ee-sf-result `(ee-sf-1stclassvideo-links ,stem . ,rest)))))

;; Tests:
;; (setq ee-sf-sexp '(find-eev2021video "2:34" "foo" "bar"))
;; (setq ee-sf-sexp '(find-eev2021video "2:34"))
;; (setq ee-sf-sexp '(find-eev2021video))
;; (setq ee-sf-sexp '(find-1stclassvideo-links "eev2021" "2:34" "foo" "bar"))
;; (setq ee-sf-sexp '(find-1stclassvideo-links "eev2021"))
;; (ee-sf-1stclassvideo-p)


(defun ee-sf-1stclassvideo-links (c &optional time &rest rest)
  "An internal function used by `find-1stclassvideo-links'."
  (ee-let*-macro-1stclassvideo-c
   c
   (let* ((qrest       (ee-Qrest rest))
	  (hsubsurl-t0 ";; HSubs: {hsubs}\n")
	  (hsubs-t0    ";; HSubs: (find-{c}hsubs \"{subsinit}\"{qrest})\n")
	  (lsubs-t0    ";; LSubs: (find-{c}lsubs \"{subsinit}\")\n")
	  (index-t0    ";; Index: (find-1stclassvideo-index \"{c}\")\n")
	  (hsubsurl    (if hsubs    (ee-template0 hsubsurl-t0) ""))
	  (hsubs       (if hsubs    (ee-template0 hsubs-t0)    ""))
	  (lsubs       (if hassubs  (ee-template0 lsubs-t0)    ""))
	  (index       (if hasindex (ee-template0 index-t0)    ""))
	  (dlsubs      (ee-1stclassvideo-dlsubs c))
	  )
     `("foo"
       "bar"
       ,(ee-template0 "\
;; Title: {title}
;; MP4:   {mp4}
;; YT:    {yt}
;; Page:  {page}
{hsubsurl}\
;; Comment: {comment}
;; Date:    {date}
;; Length:  {length}
;;
;; Play:  (find-{c}video \"00:00\")
{hsubs}\
{lsubs}\
{index}\
;; Info:  (find-1stclassvideo-links \"{c}\")
;;        (find-1stclassvideo-def   \"{c}\")
{dlsubs}\

;; See:
;; (find-video-links-intro \"9. First-class videos\")
;; (find-eev \"eev-videolinks.el\" \"first-class-videos\")
;; (find-eev \"eev-videolinks.el\" \"second-class-videos\")
")))))



;; See:
;; (find-eev "eev-kl-here.el" "hprog")
;; (find-efunction 'klapt)
;; (find-efunction 'ee-kl-sexp-at-eol-p)

(setq ee-hprog-for-sf
 '(:or
   (:if (ee-sf-1stclassvideo-p)     (find-elinks (eval ee-sf-result)))
   ;;
   (:if t (error "Buffer type not supported by ee-hprog-for-sf"))
   ))

(defun eesf ()
  (interactive)
  (setq ee-hlang-sexp2 nil)
  (setq ee-sf-result nil)
  (ee-goto-eol)
  (setq ee-sf-sexp (read (ee-last-sexp)))
  (ee-hlang-run ee-hprog-for-sf)
  (eval ee-hlang-sexp2)
  )


;; «aliases»  (to ".aliases")
(defalias 'sf 'eesf)


(provide 'eev-strange-functions)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
