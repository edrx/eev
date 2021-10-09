;; eev-on-windows.el - some support for M$ Windows.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2019,2021 Free Software Foundation, Inc.
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
;; Version:    20210811
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-on-windows.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-on-windows.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:
;;
;; Experimental, undocumented, and messy. Every time that I give a
;; workshop to Windows users this file changes a lot. The most recent
;; changes in this file correspond to a workshop that I will give in
;; mid-october 2021, and that I described in this thread:
;;
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00037.html
;;
;; I can't test the settings for Windows myself - a friend of mine
;; called Daniel Almeida is helping me to test this me before the
;; workshop.
;;
;; This file is not loaded by default. See:
;;   (find-eev "eev-load.el")
;;
;; At this moment what the Windows users need to do is:
;;   (require 'eev-on-windows)
;;   (mkdir "~/bin/" t)
;;   (delete-file "~/bin/wget.exe")
;;   (ee-download-with-eww "http://angg.twu.net/2021-oficina/wget.exe" "~/bin/")
;;   (ee-use-windows)
;; and they need to run the test in:
;;   (to "directories")


;; 2021:
;; «.ee-download-with-eww»	(to "ee-download-with-eww")
;; «.ee-use-eshell»		(to "ee-use-eshell")
;; «.ee-use-bullets»		(to "ee-use-bullets")
;; «.ee-use-gnu-linux»		(to "ee-use-gnu-linux")
;; «.ee-use-windows»		(to "ee-use-windows")
;; «.directories»		(to "directories")
;;
;; 2019:
;; «.eev-tar»			(to "eev-tar")
;; «.ee-add-to-PATH»		(to "ee-add-to-PATH")



;;;  ____   ___ ____  _ 
;;; |___ \ / _ \___ \/ |
;;;   __) | | | |__) | |
;;;  / __/| |_| / __/| |
;;; |_____|\___/_____|_|
;;;                     

(require 'eww)
(require 'eshell)
(require 'em-alias)


;; «ee-download-with-eww»  (to ".ee-download-with-eww")
;; Test:
;;   (mkdir "~/bin/" t)
;;   (delete-file "~/bin/wget.exe")
;;   (ee-download-with-eww "http://angg.twu.net/2021-oficina/wget.exe" "~/bin/")
;;
(defun ee-download-with-eww (url dir)
  (url-retrieve url #'eww-download-callback (list url dir)))


;; «ee-use-eshell»  (to ".ee-use-eshell")
;; TODO: define an `eepitch-eshell2'.
;;
(defun ee-use-eshell ()
  (interactive)
  (defun eepitch-shell  () (interactive) (eepitch-eshell))
  (defun eepitch-shell2 () (interactive) (eepitch-eshell))
  )

(defun ee-use-shell ()
  (interactive)
  (defun eepitch-shell  () (interactive) (eepitch '(shell "*shell*")))
  (defun eepitch-shell2 () (interactive) (eepitch '(shell "*shell 2*")))
  )


;; «ee-use-bullets»  (to ".ee-use-bullets")
;; From: (find-eepitch-bullet-links 2 "red bullets by default")
;;
(defun ee-use-bullets ()
  (interactive)
  (eepitch-set-glyph0 ?\u2022 ?\u2022 'eepitch-star-face)
  (defun eewrap-eepitch () (interactive)
    (let* ((fmt   "\u2022 (eepitch-%s)\n\u2022 (eepitch-kill)\n\u2022 (eepitch-%s)")
           (li    (ee-this-line-extract))
           (newli (format fmt li li)))
      (insert newli))
    (ee-next-line 1))
  )

(defun ee-use-red-stars ()
  (interactive)
  (defun eewrap-eepitch () (interactive)
    (let* ((fmt   " (eepitch-%s)\n (eepitch-kill)\n (eepitch-%s)")
           (li    (ee-this-line-extract))
           (newli (format fmt li li)))
      (insert newli))
    (ee-next-line 1))
  )


;; «ee-use-gnu-linux»  (to ".ee-use-gnu-linux")
;; «ee-use-windows»  (to ".ee-use-windows")
;;
(defun ee-use-gnu-linux ()
  (interactive)
  (ee-use-shell)
  (eshell/alias "wget" nil)
  (setq ee-wget-program         "wget")
  (setq ee-firefox-program      "firefox")
  (setq ee-googlechrome-program "google-chrome")
  (setq ee-mpv-program          "mpv")
  )

(defun ee-use-windows ()
  (interactive)
  (ee-use-shell)
  (eshell/alias "wget" "~/bin/wget.exe $*")
  (setq ee-wget-program         "~/bin/wget.exe")
  (setq ee-firefox-program      "$FIREFOXDIR/firefox.exe")
  (setq ee-googlechrome-program "$GOOGLECHROMEDIR/chrome.exe")
  (setq ee-mpv-program          "$MPVDIR/mpv.exe")
  (defalias 'find-pdf-page 'find-googlechrome-page)
  )


;; «directories»  (to ".directories")
;; These directories are for Daniel Almeida's machine.
;; Most people will have to configure this.
;;
(setenv "FIREFOXDIR"      "c:/Program Files/Mozilla Firefox")
(setenv "GOOGLECHROMEDIR" "c:/Program Files/Google/Chrome/Application")
(setenv "MPVDIR"          "c:/Users/danie/OneDrive/Documentos/mpv")

;; Basic tests:
;;   (find-fline         "~/bin/"            "wget.exe")
;;   (find-fline         "$GOOGLECHROMEDIR/" "chrome.exe")
;;   (find-fline         "$FIREFOXDIR/"      "firefox.exe")
;;   (find-fline         "$MPVDIR/"          "mpv.exe")
;;   (find-callprocess `("~/bin/wget.exe"              "--help"))
;;   (find-callprocess `("$GOOGLECHROMEDIR/chrome.exe" "--help"))
;;   (find-callprocess `("$FIREFOXDIR/firefox.exe"     "--help"))
;;   (find-callprocess `("$MPVDIR/mpv.exe"             "--help"))
;;   (find-callprocess `(,ee-wget-program              "--help"))
;;   (find-callprocess `(,ee-googlechrome-program      "--help"))
;;   (find-callprocess `(,ee-firefox-program           "--help"))
;;   (find-callprocess `(,ee-mpv-program               "--help"))
;;   (find-wget "http://angg.twu.net/eev-current/eev-on-windows.el")
;;
;; For the tests for using browsers as PDF viewers you will need to
;; understand these sections of the tutorials, and will need to run
;; some of the commands in them:
;;   (find-psne-intro "1. Local copies of files from the internet")
;;   (find-psne-intro "3. The new way: `M-x brep'")
;;   (find-pdf-like-intro "2. Preparation")
;;   (find-pdf-like-intro "2. Preparation" "Coetzee99")
;;
;; Then try:
;;   (find-googlechrome-page "~/Coetzee99.pdf" 3)
;;   (find-firefox-page      "~/Coetzee99.pdf" 3)
;;   (find-pdf-page          "~/Coetzee99.pdf" 3)
;;
;; The video links are explained here:
;;   (find-videos-intro "2. Short links to eev video tutorials")
;;   http://angg.twu.net/2021-video-links.html
;;
;; Test for the video links:
;;   (delete-file (ee-expand "$S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4"))
;;   (brep "http://angg.twu.net/eev-videos/2021-test-blocks.mp4")
;;   (find-eevvideo-links "testbls" "2021-test-blocks" "fpsF_M55W4o")
;;   (find-video "$S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4")
;;   (find-video "$S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4" "2:33")








;;;  ____   ___  _  ___  
;;; |___ \ / _ \/ |/ _ \ 
;;;   __) | | | | | (_) |
;;;  / __/| |_| | |\__, |
;;; |_____|\___/|_|  /_/ 
;;;                      
;;
;; «eev-tar»  (to ".eev-tar")
;; This is from 2019 and is now very obsolete -
;; partially because eev is in ELPA.
;;
;;   (setq  eev-tar-dir   "~/eev-tar/")
;;   (setq  eev-tar-fname "~/eev-tar/eev2.tar")
;;   (setq  eev-tar-url   "http://angg.twu.net/eev-current/eev2.tar")
;;   (mkdir eev-tar-dir   t)
;;   (setq  eev-tar-contents nil)
;;   (setq  eev-tar-contents (find-urlretrieve0 eev-tar-url))
;;   (length (setq eev-tar-contents (find-urlretrieve0 eev-tar-url)))
;;   (write-region eev-tar-contents nil eev-tar-fname)
;;   
;;   (find-2a nil '(find-fline eev-tar-fname 1 '(tar-untar-buffer)))
;;   (eek "C-x o C-x 4 0")
;;   (find-2a nil '(find-fline eev-tar-dir nil '(eek "g")))
;;
;; Add something like this to your .emacs:
;;
;;   (add-to-list 'load-path "~/eev-tar/")
;;
;; Use these sexps to check if everything is alright:
;;
;;   (find-epp load-path)
;;   (find-estring (mapconcat 'identity load-path "\n"))
;;   (locate-library "eejump")
;;   (find-estring (list-load-path-shadows t))
;;
;; See:
;;   (find-eev "eev-plinks.el" "find-urlretrieve")
;;   (find-es "emacs" "package-untar")
;;   (find-es "emacs" "load-path")
;;   (find-angg ".emacs.local.w32")
;;   (find-angg ".emacs.local.w32" "PATH")


;; «ee-add-to-PATH»  (to ".ee-add-to-PATH")
;; The last time that I used these commands to change the Windows PATH
;; was in 2019. In this message Eli Zaretskii recommended not changing
;; the PATH, and he was totally right:
;;   https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00052.html
;;
;; OLD TODO: Rewrite some of this using:
;;   (find-efunctiondescr 'parse-colon-path)
;;   (find-efunction      'parse-colon-path)
;;   (find-elnode "System Environment" "Variable: path-separator")
;;   (find-elnode "System Environment" "Function: parse-colon-path path")
;;
;; (setq mylist '(22 33 44))
;; (add-to-list 'mylist 44)
;;
;; (ee-dospath-add "A;B;C" "B")
;; (ee-dospath-add "A;B;C" "c:/B")
;;
;; (let* ((a 2) (a (* 10 a)) (a (+ 3 a))) a)
;;
;; (find-elnode "Index" "* delete:")

(defun ee-dospath-to-unix (str)
  (replace-regexp-in-string "\\\\" "/" str))
(defun ee-dospath-to-dos (str)
  (replace-regexp-in-string "/" "\\\\" str))
(defun ee-dospath-split (str)
  (split-string str ";"))
(defun ee-dospath-unsplit (list)
  (mapconcat 'identity list ";"))

(defun ee-dospath-add (path dir)
  (setq dir  (ee-dospath-to-dos dir))
  (setq path (ee-dospath-to-dos path))
  (let* ((list (ee-dospath-split path))
	 (newlist (cons dir (delete dir list))))
    (ee-dospath-unsplit newlist)))

(defun ee-add-to-PATH (dir)
  (setenv "PATH" (ee-dospath-add (getenv "PATH") dir)))







(provide 'eev-on-windows)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
