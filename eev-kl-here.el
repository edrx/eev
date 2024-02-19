;;; eev-kl-here.el -- Kill link to here.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.
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
;; Latest version: <http://anggtwu.net/eev-current/eev-kl-here.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-kl-here.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-kl-here-intro.html>
;;                                               (find-kl-here-intro)

;;; Commentary:

;; This file implements an experimental variant of `find-here-links',
;; in which we only generate a single "link to here", and we push that
;; into the kill ring. It is a cross between this,
;;
;;   (find-here-links-intro "3. `find-here-links'")
;;   (find-here-links-intro "9. The hlang")
;;
;; and the "kill link" functions from:
;;
;;   (find-kla-intro)
;;
;; Note that:
;;
;;   1. everything here is VERY experimental,
;;   2. this file is not loaded by default,
;;   3. the recommended way to use it is to put a line like this in
;;      your ~/.emacs,
;;
;;        (require 'eev-kl-here)    ; (find-eev "eev-kl-here.el")
;;
;;   4. the current version of this file defines the functions `kl',
;;      `kll' and `kls', that don't start with the valid prefixes.

;; Index:
;; «.ee-find-linkis»		(to "ee-find-linkis")
;; «.hprog»			(to "hprog")
;; «.kl»			(to "kl")
;; «.find-kl-debug-links»	(to "find-kl-debug-links")
;; «.aliases»			(to "aliases")

(require 'eev-kla)		; (find-eev "eev-kla.el")
(require 'eev-hlinks)		; (find-eev "eev-hlinks.el")



;;;                   __ _           _       _ _       _    _     
;;;   ___  ___       / _(_)_ __   __| |     | (_)_ __ | | _(_)___ 
;;;  / _ \/ _ \_____| |_| | '_ \ / _` |_____| | | '_ \| |/ / / __|
;;; |  __/  __/_____|  _| | | | | (_| |_____| | | | | |   <| \__ \
;;;  \___|\___|     |_| |_|_| |_|\__,_|     |_|_|_| |_|_|\_\_|___/
;;;                                                                   
;; These functions are used by the hprogram in the next section. Each
;; `ee-find-{stem}-linki' is similar to the corresponding
;; `ee-find-{stem}-links', but the `...-links' function generates
;; several elisp hyperlinks and the `...-linki' function generates
;; just one. The `i' in the `linki' was originally a `1', but the `i'
;; is easier to type, to read, and to pronounce.
;;
;; See:
;;   (find-eaproposf "ee-find.*link[is]")
;;   (find-eev "eev-htests.el" "tests")
;;
;; «ee-find-linkis»  (to ".ee-find-linkis")

;; Skel: (find-linki-links "info")
(defun ee-find-info-linki ()
    (if (ee-info-shortp)
	`(,(ee-info-shortf) ,(ee-info-node))
      `(find-node ,(ee-info-fullnode))))

;; Skel: (find-linki-links "intro")
(defun ee-find-intro-linki ()
  (let* ((stem (ee-intro-stem))
	 (find-xxx-intro (ee-intern "find-%s-intro" stem)))
    (list find-xxx-intro)))

;; Skel: (find-linki-links "man")
(defun ee-find-man-linki ()
  `(find-man ,(ee-buffer-re ee-man-re)))

;; Skel: (find-linki-links "file")
(defun ee-find-file-linki ()
  (let* ((fname0 (or (buffer-file-name) default-directory))
	 (fname (ee-shorten-file-name fname0)))
    (if (ee-kl-c)
	`(,(ee-kl-find-cfile) ,(ee-kl-shorterfname))
      `(find-fline ,fname))))

;; Skel: (find-linki-links "epackage")
(defun ee-find-epackage-linki ()
  (let ((p (ee-epackage-bufferp)))
    `(find-epackage-links ',p)))

;; Skel: (find-linki-links "epackages")
(defun ee-find-epackages-linki ()
  (let ((pkgsymbol (ee-packages-package-here)))
    `(find-epackages ',pkgsymbol)))

;; Skel: (find-linki-links "custom")
(defun ee-find-custom-linki ()
  (let* ((name   (ee-buffer-re ee-custom-re))
	 (symbol (ee-custom-lispify-tag-name name)))
    `(find-customizegroup ',symbol)))

;; Skel: (find-linki-links "custom-f")
(defun ee-find-custom-f-linki ()
  (let* ((name   (ee-buffer-re ee-custom-f-re))
	 (symbol (ee-custom-lispify-tag-name name)))
    `(find-customizeface ',symbol)))

;; Skel: (find-linki-links "custom-v")
(defun  ee-find-custom-v-linki () 
  (let* ((name   (ee-buffer-re ee-custom-v-re))
	 (symbol (ee-custom-lispify-tag-name name)))
    `(find-customizevariable ',symbol)))

;; Skel: (find-linki-links "eshortdoc")
(defun  ee-find-eshortdoc-linki ()
  (let ((symbol (intern (ee-eshortdoc-bufferp))))
    `(find-eshortdoc ',symbol)))

;; Skel: (find-linki-links "ecolors")
(defun ee-find-ecolors-linki ()
  '(find-ecolors))

;; Skel: (find-linki-links "efaces")
(defun ee-find-efaces-linki ()
  '(find-efaces))

;; Skel: (find-linki-links "efunctiondescr")
(defun ee-find-efunctiondescr-linki ()
  (let ((f (ee-efunctiondescr-bufferp)))
    ;; `(find-efunctiondescr ',f)
    `(find-efunction-links ',f)
    ))

;; Skel: (find-linki-links "efacedescr")
(defun ee-find-efacedescr-linki ()
  (let ((f (ee-efacedescr-bufferp)))
    ;; `(find-efacedescr ',f)
    `(find-eface-links ',f)
    ))

;; Skel: (find-linki-links "evardescr")
(defun ee-find-evardescr-linki ()
  (let ((v (ee-evardescr-bufferp)))
    ;; `(find-evardescr ',v)
    `(find-evariable-links ',v)
    ))

;; Skel: (find-linki-links "epackage")
;; Needs a rename


;;;  _                           
;;; | |__  _ __  _ __ ___   __ _ 
;;; | '_ \| '_ \| '__/ _ \ / _` |
;;; | | | | |_) | | | (_) | (_| |
;;; |_| |_| .__/|_|  \___/ \__, |
;;;       |_|              |___/ 
;;
;; This is an hprogram similar to the one used by `find-here-links',
;; but in this one each :if returns a single sexp (for `kl').
;; See:
;;   (find-here-links-intro "9. The hlang")
;;   (find-eev "eev-hlinks.el" "hprog")
;; Tests:
;;   (find-eev "eev-htests.el" "tests")
;;
;; «hprog»  (to ".hprog")

(defvar ee-hprog-for-linki
 '(:or
   ;; By major mode:
   (:if (ee-info-bufferp)       (ee-find-info-linki))      ; done
   (:if (ee-man-bufferp)        (ee-find-man-linki))	   ; done
   (:if (ee-dired-bufferp)      (ee-find-file-linki))	   ; done
   (:if (ee-wdired-bufferp)     (ee-find-file-linki))	   ; done
   (:if (ee-epackages-bufferp)  (ee-find-epackages-linki)) ; done
   ;;
   ;; By buffer name:
   (:if (ee-intro-bufferp)     (ee-find-intro-linki))     ; done
   (:if (ee-custom-bufferp)    (ee-find-custom-linki))	  ; done
   (:if (ee-custom-f-bufferp)  (ee-find-custom-f-linki))  ; done
   (:if (ee-custom-v-bufferp)  (ee-find-custom-v-linki))  ; done
   (:if (ee-ecolors-bufferp)   (ee-find-ecolors-linki))   ; done
   (:if (ee-efaces-bufferp)    (ee-find-efaces-linki))    ; done
   (:if (ee-pdftext-bufferp)   (ee-find-pdftext-linki))   ; not yet
   (:if (ee-eshortdoc-bufferp) (ee-find-eshortdoc-linki)) ; done
   ;;
   ;; By buffer name, when it is "*Help*":
   (:if (ee-efunctiondescr-bufferp) (ee-find-efunctiondescr-linki)) ; done
   (:if (ee-efacedescr-bufferp)     (ee-find-efacedescr-linki))	    ; done
   (:if (ee-evardescr-bufferp)      (ee-find-evardescr-linki))	    ; done
   (:if (ee-epackage-bufferp)       (ee-find-epackage-linki))	    ; done
   ;;
   ;; Other cases:
   (:if (ee-libera-bufferp)      (ee-find-libera-linki))   ; not yet
   (:if (ee-freenode-bufferp)    (ee-find-freenode-linki)) ; not yet
   (:if (ee-file-bufferp)        (ee-find-file-linki))	   ; done
   ;;
   (:if t (error "Buffer type not supported by ee-hprog-linki"))
   ))

;; Similar to:
;;   (find-efunction 'ee-detect-here)
(defun ee-detect-linki ()
  (ee-hlang-run ee-hprog-for-linki))

(defun ee-get-linki ()
  (ee-detect-linki)
  (eval ee-hlang-sexp2))


;;;  _    _ 
;;; | | _| |
;;; | |/ / |
;;; |   <| |
;;; |_|\_\_|
;;;         
;; «kl»  (to ".kl")
;; Similar to:
;;   (find-eev "eev-kla.el" "kill-sexps")
;;   (find-eev "eev-kla.el" "aliases")

(defun eekl (&optional arg)
  "<K>ill <L>ink to here. Tries to be smart."
  (interactive "P")
  (ee-detect-linki)
  (if arg
      (find-kl-debug-links 'kl)
    (ee-kl-kill (ee-get-linki))))

(defun eekll (&optional arg)
  "<K>ill <L>ink to here; add a <L>ine. Tries to be smart."
  (interactive "P")
  (ee-detect-linki)
  (if arg
      (find-kl-debug-links 'kl)
    (ee-kl-kill (append (ee-get-linki) (list (ee-kl-line))))))

(defun eekls (&optional arg)
  "<K>ill <L>ink to here; add a <S>tring. Tries to be smart."
  (interactive "P")
  (ee-detect-linki)
  (if arg
      (find-kl-debug-links 'kl)
    (ee-kl-kill (append (ee-get-linki) (list (ee-kl-region))))))



;;;  ____       _                 
;;; |  _ \  ___| |__  _   _  __ _ 
;;; | | | |/ _ \ '_ \| | | |/ _` |
;;; | |_| |  __/ |_) | |_| | (_| |
;;; |____/ \___|_.__/ \__,_|\__, |
;;;                         |___/ 
;;
;; «find-kl-debug-links»  (to ".find-kl-debug-links")
;; Skel: (find-find-links-links-new "kl-debug" "symbol" "")
;; Test: (find-kl-debug-links 'KL)
;;
(defun find-kl-debug-links (&optional symbol &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for kl-debug."
  (interactive)
  (apply
   'find-elinks
   `((find-kl-debug-links ',symbol ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-kl-debug-links)
     ""
     ,(ee-template0 "\
# The last call to
#     '({symbol} ARG)
#  -> '(ee-detect-linki)
#  -> '(ee-hlang-run ee-hprog-for-linki)
# produced this:
#   ee-hlang-sexp1  =>  {(ee-S ee-hlang-sexp1)}
#   ee-hlang-sexp2  =>  {(ee-S ee-hlang-sexp2)}
# See:
#   ee-hlang-sexp1
#   ee-hlang-sexp2
#   (find-efunction '{(car ee-hlang-sexp1)})
#   (find-efunction '{(car ee-hlang-sexp2)})
# And:
#   (find-here-links-intro \"8. Debugging\")
#   (find-here-links-intro \"8. Debugging\" \"Each test tests\")
#   (find-eev \"eev-kl-here.el\" \"hprog\")
#   (find-eev \"eev-kl-here.el\" \"kl\")
")
     )
   pos-spec-list))



;;;        _ _                     
;;;   __ _| (_) __ _ ___  ___  ___ 
;;;  / _` | | |/ _` / __|/ _ \/ __|
;;; | (_| | | | (_| \__ \  __/\__ \
;;;  \__,_|_|_|\__,_|___/\___||___/
;;;                                
;; «aliases»  (to ".aliases")
;; See: (find-kla-intro "4. Aliases")
;; This is temporary!

(defalias 'kl    'eekl)
(defalias 'kll	 'eekll)
(defalias 'kls	 'eekls)

(defalias 'kla   'eekla)
(defalias 'kla0  'eekla0)
(defalias 'klas  'eeklas)
(defalias 'klf   'eeklf)
(defalias 'klfs  'eeklfs)
(defalias 'klt   'eeklt)
(defalias 'klts  'eeklts)
(defalias 'kli   'ee-kl-insert)
(defalias 'kla2  'eekla2)





(provide 'eev-kl-here)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
