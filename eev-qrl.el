;;; eev-qrl.el -- `query-replace-list', for editing templates.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022-2023 Free Software Foundation, Inc.
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
;; Version:    20240309
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-qrl.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-qrl.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-templates-intro.html>
;;                                               (find-templates-intro)

;;; Comment:

;; When we edit `find-*-links' functions we often have to copy
;; external strings into the argument of the (ee-template0 "...") -
;; and then we have to add backslashes before some characters and
;; convert "{"s to "{<}"s and "}"s to "{>}"s. The command `M-x qrl'
;; uses `query-replace-regexp' to do this interactively in a nice way.
;;
;; For more on this and on editing `find-*-links' functions, see:
;;
;;   (find-templates-intro "4. Adding meat")
;;
;; To understand how this works run one of these three sexps,
;;
;;   (require 'eev-qrl)
;;   (load (buffer-file-name))
;;   (eval-buffer)
;;
;; and then try the high-level tests below.
;;
;; High-level tests:
;;   (find-elinks-elisp '((qrl "from1" "to1" "from2" "to2") "from2from1"))
;;   (find-elinks-elisp '((qrl "a" "bb" "b" "aa")                "abcde"))
;;   (find-elinks-elisp '((qrl)                              "{\"foo\"}"))
;; Low-level tests:
;;   (ee-qrl-as)
;;   (ee-qrl-regexp)
;;   (ee-qrl-r0 "{")
;;   (ee-qrl-r1 "foo\\&bar")
;;   (ee-qrl-r2 "\\")
;;
;; The code below was based on this question:
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2023-12/msg00064.html

;; Index:
;; «aliases»  (to ".aliases")


(defvar ee-qrl-plist0
  '("\"" "\\\""
    "\\" "\\\\"
    "{"  "{<}"
    "}"  "{>}"))
(defvar ee-qrl-plist ee-qrl-plist0)

(defun ee-qrl-as     () (cl-loop for (a b) on ee-qrl-plist by 'cddr collect a))
(defun ee-qrl-regexp () (mapconcat 'regexp-quote (ee-qrl-as) "\\|"))
(defun ee-qrl-r0    (s) (plist-get ee-qrl-plist (ee-no-properties s) 'equal))
(defun ee-qrl-r1    (s) (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
(defun ee-qrl-r2    (s) (ee-qrl-r1 (ee-qrl-r0 s)))
(defun ee-qrl-r3  (a b) (ee-qrl-r2 (match-string 0)))

(defun ee-qrl0 (&rest plist)
  (interactive)
  (let ((ee-qrl-plist (or plist ee-qrl-plist)))
    (query-replace-regexp (ee-qrl-regexp) (list 'ee-qrl-r3 nil))
    "Done"))

(defun ee-qrl-narrow (&rest plist)
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (apply 'ee-qrl0 plist))))

(defun ee-qrl (&rest plist)
  "`query-replace-list'. See: (find-templates-intro \"4. Adding meat\")"
  (interactive)
  (if (region-active-p)			; if the region as active
      (apply 'ee-qrl-narrow plist)	; then narrow to the region
    (apply 'ee-qrl0 plist)))		; else replace to the end of the buffer

;; «.aliases»	(to "aliases")
;; Moved to: (find-eev "eev-aliases.el" "query-replace-list")

(provide 'eev-qrl)

;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
