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
;; Version:    20211107
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-on-windows.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-on-windows.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;; «.how-to-test-this»			(to "how-to-test-this")
;; «.requires»				(to "requires")
;; «.ee-download-with-eww»		(to "ee-download-with-eww")
;; «.eshell»				(to "eshell")
;;   «.ee-use-eshell»			(to "ee-use-eshell")
;;   «.ee-use-shell»			(to "ee-use-shell")
;; «.wget»				(to "wget")
;;   «.ee-use-wget-exe»			(to "ee-use-wget-exe")
;;   «.ee-use-wget»			(to "ee-use-wget")
;; «.red-stars»				(to "red-stars")
;;   «.ee-use-bullets»			(to "ee-use-bullets")
;;   «.ee-use-red-stars»		(to "ee-use-red-stars")
;; «.find-angg-es»			(to "find-angg-es")
;;   «.ee-use-find-angg-es-remote»	(to "ee-use-find-angg-es-remote")
;;   «.ee-use-find-angg-es-local»	(to "ee-use-find-angg-es-local")
;; «.ee-use-windows»			(to "ee-use-windows")
;; «.ee-use-gnu-linux»			(to "ee-use-gnu-linux")
;; «.ee-use-gnu-linux-but-simulate»	(to "ee-use-gnu-linux-but-simulate")
;; «.directories»			(to "directories")
;; «.basic-tests»			(to "basic-tests")




;;; Commentary:
;;
;; Experimental, undocumented, and messy. Every time that I give a
;; workshop to Windows users this file changes a lot. Most of what was
;; here in the last revision was rewritten in a more human-readable
;; form and placed in:
;;
;;   (find-windows-beginner-intro)


;; «how-to-test-this»  (to ".how-to-test-this")
;;
;; THIS BLOCK IS BEING REWRITTEN AND IS A MESS
;;
;; 0.1. How to test this
;; =====================
;;
;;      (require 'eev-on-windows)
;;      (ee-use-windows)
;;      (to "directories")
;;
;; The sexp `(to ...)' will take you to the two last sections of this
;; file, where there are instructions for configuring some directories
;; and s series of tests.
;;
;;
;; 0.2. Setting your ~/.emacs
;; ==========================
;; If you are helping me to test this AND you know how to edit your
;; ~/.emacs (hey Daniel Bastos! Daniel Tavares and Julha, please
;; ignore this!) then this is what you should put there:
;;
;;   ;; See: (find-eevfile "eev-on-windows.el" ".emacs")
;;   (require 'eev-beginner)
;;   (require 'eev-on-windows)
;;   (ee-use-windows)
;;   (setenv "FIREFOXDIR"      "c:/Program Files/Mozilla Firefox")
;;   (setenv "GOOGLECHROMEDIR" "c:/Program Files/Google/Chrome/Application")
;;   (setenv "MPVDIR"          "c:/Users/danie/OneDrive/Documentos/mpv")
;;   ;; Tests: (find-eev "eev-on-windows.el" "directories")
;;
;; Note that "(require 'eev-on-windows)" runs some setenvs. The
;; setenvs above, that will override the ones in eev-on-windows.el
;; with the paths that are correct in your machine, MUST come AFTER
;; the "(require 'eev-on-windows)".



;; «requires»  (to ".requires")
;;
(require 'eww)
(require 'eshell)
(require 'em-alias)



;;;                          
;;;   _____      ____      __
;;;  / _ \ \ /\ / /\ \ /\ / /
;;; |  __/\ V  V /  \ V  V / 
;;;  \___| \_/\_/    \_/\_/  
;;;                          
;; «ee-download-with-eww»  (to ".ee-download-with-eww")
;; See: (find-windows-beginner-intro "7. eev-on-windows.el")
;;      (find-node "(eww)Top")
;; Test with:
;;   (mkdir "~/bin/" t)
;;   (delete-file "~/bin/wget.exe")
;;   (ee-download-with-eww "http://angg.twu.net/2021-oficina/wget.exe" "~/bin/")
;;
(defun ee-download-with-eww (url dir)
  (url-retrieve url #'eww-download-callback (list url dir)))



;;;  _____     _          _ _ 
;;; | ____|___| |__   ___| | |
;;; |  _| / __| '_ \ / _ \ | |
;;; | |___\__ \ | | |  __/ | |
;;; |_____|___/_| |_|\___|_|_|
;;;                           
;; «eshell»  (to ".eshell")

;; «ee-use-eshell»  (to ".ee-use-eshell")
;; Redefine `eepitch-shell' and `eepitch-shell2' to make them use
;; Eshell instead of the default shell.
;; See: (find-windows-beginner-intro "5.6. Eshell")
;;      (find-windows-beginner-intro "7.1. `ee-use-windows'")
;;
(defun ee-use-eshell ()
  (interactive)
  (defun eepitch-shell  () (interactive) (eepitch-eshell))
  (defun eepitch-shell2 () (interactive) (eepitch-eshell2))
  )

;; «ee-use-shell»  (to ".ee-use-shell")
;; Redefine `eepitch-shell' and `eepitch-shell2' with their default
;; definitions. Running this cancels the effect of `ee-use-eshell'.
;; See: (find-eev "eepitch.el" "eepitch-shell")
;;
(defun ee-use-shell ()
  (interactive)
  (defun eepitch-shell  () (interactive) (eepitch '(shell)))
  (defun eepitch-shell2 () (interactive) (eepitch '(shell "*shell 2*")))
  )


;;;                     _   
;;; __      ____ _  ___| |_ 
;;; \ \ /\ / / _` |/ _ \ __|
;;;  \ V  V / (_| |  __/ |_ 
;;;   \_/\_/ \__, |\___|\__|
;;;          |___/          
;;
;; «wget»  (to ".wget")
;; These functions configure what wget to use: on GNU/Linux we use the
;; wget that is on the $PATH, and on M$ Windows we use ~/bin/wget.exe.
;;
;;  BIG WARNING: 
;; These function DELETE your eshell-aliases-file and create a new one
;; with a single alias inside!!! See:
;;   (find-node "(eshell)Aliases" "eshell-aliases-file")

;; «ee-use-wget-exe»  (to ".ee-use-wget-exe")
(defun ee-use-wget-exe ()
  (interactive)
  (delete-file eshell-aliases-file)
  (eshell/alias "wget"  "~/bin/wget.exe $*")
  (setq ee-wget-program "~/bin/wget.exe"))

;; «ee-use-wget»  (to ".ee-use-wget")
(defun ee-use-wget ()
  (interactive)
  (delete-file eshell-aliases-file)
  (eshell/alias "wget"  "wget $*")
  (setq ee-wget-program "wget"))



;;;  ____          _       _                 
;;; |  _ \ ___  __| |  ___| |_ __ _ _ __ ___ 
;;; | |_) / _ \/ _` | / __| __/ _` | '__/ __|
;;; |  _ <  __/ (_| | \__ \ || (_| | |  \__ \
;;; |_| \_\___|\__,_| |___/\__\__,_|_|  |___/
;;;                                          
;; «red-stars»  (to ".red-stars")
;; In workshops for beginners I prefer to make them use red bullets
;; instead of red stars because the red bullets don't get corrupted
;; when pasted to Telegram of Gmail. See:
;;   (find-eev-quick-intro "6.4. Red stars")
;;   (find-red-star-links)

;; «ee-use-bullets»  (to ".ee-use-bullets")
;; From: (find-red-star-links 2 "red bullets by default")
;;
(defun ee-use-bullets ()
  (interactive)
  (eepitch-set-glyph0 ?• ?• 'eepitch-star-face)
  (defun ee-adjust-red-stars (str) (replace-regexp-in-string "" "•" str)))

;; «ee-use-red-stars»  (to ".ee-use-red-stars")
(defun ee-use-red-stars ()
  (interactive)
  (defun ee-adjust-red-stars (str) str))


;;;   __ _           _                                  __        
;;;  / _(_)_ __   __| |       __ _ _ __   __ _  __ _   / /__  ___ 
;;; | |_| | '_ \ / _` |_____ / _` | '_ \ / _` |/ _` | / / _ \/ __|
;;; |  _| | | | | (_| |_____| (_| | | | | (_| | (_| |/ /  __/\__ \
;;; |_| |_|_| |_|\__,_|      \__,_|_| |_|\__, |\__, /_/ \___||___/
;;;                                      |___/ |___/              
;;
;; «find-angg-es»  (to ".find-angg-es")
;; See: (find-angg-es-links)

;; «ee-use-find-angg-es-local»  (to ".ee-use-find-angg-es-local")
(defun ee-use-find-angg-es-local ()
  (interactive)
  (code-c-d "angg" "~/" :anchor :grep)
  (code-c-d "es"   "$ES/")
  (defun find-es (stem &rest rest)
    (apply 'find-anchor (ee-esfile (concat stem ".e")) rest))
  )

;; «ee-use-find-angg-es-remote»  (to ".ee-use-find-angg-es-remote")
(defun ee-use-find-angg-es-remote ()
  (interactive)
  (defun find-angg (fname &rest rest)
    (apply 'find-wgeta (format "http://angg.twu.net/%s" fname) rest))
  (defun find-es (fname &rest rest)
    (apply 'find-wgeta (format "http://angg.twu.net/e/%s.e" fname) rest))
  )




;; «ee-use-windows»  (to ".ee-use-windows")
;; «ee-use-gnu-linux»  (to ".ee-use-gnu-linux")
;; «ee-use-gnu-linux-but-simulate»  (to ".ee-use-gnu-linux-but-simulate")
;;
(defun ee-use-windows ()
  (interactive)
  (ee-use-bullets)
  (ee-use-eshell)
  (ee-use-wget-exe)
  (setq ee-pdftotext-program    "~/bin/pdftotext.exe")
  (setq ee-firefox-program      "$FIREFOXDIR/firefox.exe")
  (setq ee-googlechrome-program "$GOOGLECHROMEDIR/chrome.exe")
  (setq ee-mpv-program          "$MPVDIR/mpv.exe")
  (defalias 'find-pdf-page 'find-googlechrome-page)
  (ee-use-find-angg-es-remote)
  (ee-use-youtube-videos)
  )

(defun ee-use-gnu-linux ()
  (interactive)
  (ee-use-red-stars)
  (ee-use-shell)
  (ee-use-exe)
  (setq ee-pdftotext-program    "pdftotext")
  (setq ee-firefox-program      "firefox")
  (setq ee-googlechrome-program "google-chrome")
  (setq ee-mpv-program          "mpv")
  (ee-use-find-angg-es-local)
  (ee-use-local-videos)
  )

(defun ee-use-gnu-linux-but-simulate ()
  (interactive)
  (ee-use-bullets)
  (ee-use-eshell)
  (ee-use-wget)
  (setq ee-pdftotext-program    "pdftotext")
  (setq ee-firefox-program      "firefox")
  (setq ee-googlechrome-program "google-chrome")
  (setq ee-mpv-program          "mpv")
  (ee-use-find-angg-es-remote)
  (ee-use-youtube-videos)
  )




;; «directories»  (to ".directories")
;; From the internets:
;;
;;  "If you can start a Mozilla application by using a shortcut or
;;   launcher icon, then you can usually see where its installation
;;   directory is located by context-clicking (right-clicking) the
;;   icon and looking at the properties."
;;
;; Apparently you'll have to do this by hand for Firefox, Chrome, and
;; Mpv - I couldn't find a way to automate this... =/
;;
;; Most people will have to configure this.
;;
;; These directories are for Daniel Almeida's machine.
(setenv "FIREFOXDIR"      "c:/Program Files/Mozilla Firefox")
(setenv "GOOGLECHROMEDIR" "c:/Program Files/Google/Chrome/Application")
(setenv "MPVDIR"          "c:/Users/danie/OneDrive/Documentos/mpv")
;;
;; Julha needs to run this:
;; (setenv "FIREFOXDIR"      "c:/Program Files/Mozilla Firefox")
;; (setenv "MPVDIR"          "c:/Users/User/Desktop")
;; (defalias 'find-pdf-page 'find-firefox-page)



;; «basic-tests»  (to ".basic-tests")
;; 1. Basic tests
;; ==============
;;   (find-fline         "~/bin/"            "wget.exe")
;;   (find-fline         "~/bin/"            "pdftotext.exe")
;;   (find-fline         "$GOOGLECHROMEDIR/" "chrome.exe")
;;   (find-fline         "$FIREFOXDIR/"      "firefox.exe")
;;   (find-fline         "$MPVDIR/"          "mpv.exe")
;;   (find-callprocess `("~/bin/wget.exe"              "--help"))
;;   (find-callprocess `("~/bin/pdftotext.exe"         "--help"))
;;   (find-callprocess `("$GOOGLECHROMEDIR/chrome.exe" "--help"))
;;   (find-callprocess `("$FIREFOXDIR/firefox.exe"     "--help"))
;;   (find-callprocess `("$MPVDIR/mpv.exe"             "--help"))
;;   (find-callprocess `(,ee-wget-program              "--help"))
;;   (find-callprocess `(,ee-pdftotext-program         "--help"))
;;   (find-callprocess `(,ee-googlechrome-program      "--help"))
;;   (find-callprocess `(,ee-firefox-program           "--help"))
;;   (find-callprocess `(,ee-mpv-program               "--help"))
;;   (find-wget "http://angg.twu.net/eev-current/eev-on-windows.el")
;;                       (find-angg "eev-current/eev-on-windows.el")
;;                       (find-es "2021-oficina" "M-3-M-e")
;;
;;   KNOWN BUG: the "--help" option doesn't work on chrome in Windows.
;;
;; 1.1. Test `M-x brff' and `M-x brg'
;; ----------------------------------
;; Check if the tests in this section of the main tutorial work:
;;   (find-eev-quick-intro "3.1. Non-elisp hyperlinks")
;;
;;
;; 2. Tests for using the browser as a PDF viewer
;; ==============================================
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
;; You can select the browser to use for PDFs with these sexps:
;;   (defalias 'find-pdf-page 'find-googlechrome-page)
;;   (defalias 'find-pdf-page 'find-firefox-page)
;;
;;
;; 2.1. Test the short links to PDFs
;; ---------------------------------
;; The short links to PDFs are explained here:
;;   (find-pdf-like-intro "7. Shorter hyperlinks to PDF files")
;; Test:
;;   (code-pdf-page "livesofanimals" "~/Coetzee99.pdf")
;;   (find-livesofanimalspage (+ -110 127) "wrong thoughts")
;;
;; 3. Test the links to PDFs converted to text
;; ===========================================
;; The links to PDFs converted to text are explained here:
;;   (find-pdf-like-intro "3. Hyperlinks to PDF files")
;;   (find-pdf-like-intro "3. Hyperlinks to PDF files" "pdftotext")
;; Tests:
;;   (find-pdf-text                  "~/Coetzee99.pdf" 3)
;;   (code-pdf-text "livesofanimals" "~/Coetzee99.pdf")
;;   (find-livesofanimalstext (+ -110 127) "wrong thoughts")
;; Try also this:
;;   (find-extra-file-links "~/Coetzee99.pdf" "livesofanimals")
;;
;; 4. Test the links to videos
;; ===========================
;; The video links are explained here:
;;   (find-videos-intro "2. Short links to eev video tutorials")
;;   http://angg.twu.net/2021-video-links.html
;;
;; Basic tests for video links:
;;   (delete-file (ee-expand "$S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4"))
;;   (brep                    "http://angg.twu.net/eev-videos/2021-test-blocks.mp4")
;;   (delete-file (ee-expand "$S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4"))
;;   (find-video "$S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4")
;;   (find-video "$S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4" "2:33")
;;
;; Test the way to download a video that beginners usually meet first:
;;   (find-eevvideo-links "testbls" "2021-test-blocks" "fpsF_M55W4o")
;;
;; Test the short links to videos:
;;   (code-video "testblsvideo" "$S/http/angg.twu.net/eev-videos/2021-test-blocks.mp4")
;;   (find-testblsvideo)
;;   (find-testblsvideo "2:33")
;;   (find-angg ".emacs.videos" "testbls")





(provide 'eev-on-windows)





;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
