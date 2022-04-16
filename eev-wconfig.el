;;; eev-wconfig.el -- configure eev on M$ Windows.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.
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
;; Version:    20220416
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-wconfig.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-wconfig.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-psne-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-prepared-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-psne-intro)
;;                                                (find-prepared-intro)

;;; Comment:

;; This file is not loaded by default. To use it, run:
;;
;;   (require 'eev-wconfig)
;;   (find-wconfig-links)
;;
;; Or:
;;
;;   (load (buffer-file-name))
;;   (find-wconfig-links)
;;   (find-wconfig-wget-links)
;;   (find-wconfig-shell-links)
;;   (find-wconfig-pdf-links)
;;   (find-wconfig-lua-links)
;;   (find-wconfig-mpv-links)
;;
;; This is another attempt - based on ideas that I had in april/2022 -
;; to make eev easy to configure on M$ Windows. This attempt is mostly
;; targeted to "non-users", and it follows many of the principles that
;; I explained in these two videos,
;;
;;   Title: Org for Non-Users (2021)
;;   Page:  http://angg.twu.net/2021-org-for-non-users.html
;;   Play:  (find-2021orgfornonusersvideo "0:00")
;;   Info:  (find-1stclassvideo-links "2021orgfornonusers")
;;   Index: (find-1stclassvideoindex  "2021orgfornonusers")
;;
;;   Title: Why eev has a weird elisp tutorial and how to use it
;;   Page:  http://angg.twu.net/find-elisp-intro.html
;;   Play:  (find-2022findelispintrovideo "0:00")
;;   Info:  (find-1stclassvideo-links "2022findelispintro")
;;   Index: (find-1stclassvideoindex  "2022findelispintro")
;;
;; and in this page:
;;
;;   http://angg.twu.net/eev-customize.html
;;
;; More precisely, this is targeted to people who like to "open the
;; lid" to see how things look inside, and who don't mind if at first
;; they understand only a bit of what they see; and who are still
;; using mostly M$ Windows, but who are trying to use GNU/Linux more.
;; I believe that these people will find this approach much more
;; interesting and fun than my previous attempts.
;;
;; At this moment (2022apr16) I am testing this with a friend with
;; whom I interact mostly via Telegram and IRC. The docs are very
;; incomplete!


;; Index:
;; «.find-wconfig-links»	(to "find-wconfig-links")
;; «.find-wconfig-wget-links»	(to "find-wconfig-wget-links")
;; «.find-wconfig-shell-links»	(to "find-wconfig-shell-links")
;; «.find-wconfig-pdf-links»	(to "find-wconfig-pdf-links")
;; «.find-wconfig-lua-links»	(to "find-wconfig-lua-links")
;; «.find-wconfig-mpv-links»	(to "find-wconfig-mpv-links")




;; «find-wconfig-links»  (to ".find-wconfig-links")
;; Skel: (find-find-links-links-new "wconfig" "" "")
;; Test: (find-wconfig-links)
;;
(defun find-wconfig-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig."
  (interactive)
  (apply
   'find-elinks-elisp
   `((find-wconfig-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-wconfig-links)
     ""
     ,(ee-template0 "\
(find-wconfig-wget-links)
(find-wconfig-shell-links)
(find-wconfig-pdf-links)
(find-wconfig-lua-links)
(find-wconfig-mpv-links)
")
     )
   pos-spec-list))



;;; __        __         _   
;;; \ \      / /_ _  ___| |_ 
;;;  \ \ /\ / / _` |/ _ \ __|
;;;   \ V  V / (_| |  __/ |_ 
;;;    \_/\_/ \__, |\___|\__|
;;;           |___/          
;;
;; «find-wconfig-wget-links»  (to ".find-wconfig-wget-links")
;; Skel: (find-find-links-links-new "wconfig-wget" "" "")
;; Test: (find-wconfig-wget-links)
;;       (defun edt () (interactive) (eval-defun nil) (find-wconfig-wget-links))
;;
(defun find-wconfig-wget-links (&rest pos-spec-list)
"Visit a temporary buffer containing a script for configuring wget on Windows."
  (interactive)
  (apply
   'find-elinks-elisp
   `((find-wconfig-wget-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-wconfig-wget-links)
     ""
     ,(ee-template0 "\

;; 1. Download wget.exe
;; ====================
;; Run the sexps below to download a wget for Windows and put it
;; in ~/bin/wget.exe. For more info on that wget.exe, see:
;;   http://angg.twu.net/2021-oficina/
;;   http://angg.twu.net/2021-oficina/README
;;
(require 'eww)
(defun ee-download-with-eww (url dir)
  (url-retrieve url #'eww-download-callback (list url dir)))
;;
(mkdir       \"~/bin/\" t)
(delete-file \"~/bin/wget.exe\")
(ee-download-with-eww \"http://angg.twu.net/2021-oficina/wget.exe\" \"~/bin/\")
;;
;; Tests:
;;   (find-fline \"~/bin/\")
;;   (find-fline \"~/bin/\" \"wget.exe\")
;;   (find-callprocess \"~/bin/wget.exe --help\")
;; and:

 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)
rm --help
ls --help

ls ~/bin/
~/bin/wget.exe --version
~/bin/wget.exe --help
cd ~/bin/
rm -fv README
~/bin/wget.exe http://angg.twu.net/2021-oficina/README
ls -lF       README
ls -lF ~/bin/README



;; 2. Configure `find-wget'
;; ========================
;; Run this:
;;
(setq ee-wget-program \"~/bin/wget.exe\")
;;
;; To test it, run the tests in the sections \"Tests:\" here:
;;   (find-eev \"eev-plinks.el\" \"find-wget\")
;;   (find-eev \"eev-plinks.el\" \"find-wget\" \"Tests:\")
;; The concept of \"anchor\" is explained here:
;;   (find-eev-quick-intro \"8. Anchors\")



;; 3. Save the settings in your ~/.emacs
;; =====================================
;; If all the tests above worked, put the two lines below
;; in your ~/.emacs:

;; See: (find-wconfig-wget-links)
(setq ee-wget-program \"~/bin/wget.exe\")



;; Random notes
;; ============
;; The two sexps below undoes the configurations above:
;;   (setq ee-wget-program \"wget\")

")
     )
   pos-spec-list))





;;;  ____  _          _ _ 
;;; / ___|| |__   ___| | |
;;; \___ \| '_ \ / _ \ | |
;;;  ___) | | | |  __/ | |
;;; |____/|_| |_|\___|_|_|
;;;                       
;; «find-wconfig-shell-links»  (to ".find-wconfig-shell-links")
;; Skel: (find-find-links-links-new "wconfig-shell" "" "")
;; Test: (find-wconfig-shell-links)
;;       (defun edt () (interactive) (eval-defun nil) (find-wconfig-shell-links))
;;
(defun find-wconfig-shell-links (&rest pos-spec-list)
"Visit a temporary buffer containing a script for configuring eshell on Windows."
  (interactive)
  (apply
   'find-elinks-elisp
   `((find-wconfig-shell-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-wconfig-shell-links)
     ""
     ,(ee-template0 "\
;; Note: only run this after configuring wget!
;; See: (find-wconfig-wget-links)

;; (find-windows-beginner-intro \"5.5. Shell Mode\")
;; (find-eev-quick-intro \"6. Controlling shell-like programs\")
;; (find-eepitch-intro \"1.2. Two targets\")
;; (find-node \"(eshell)Top\")
;; (find-node \"(eshell)Command Index\")



;; 1. Make Eshell use wget.exe
;; ===========================
;; Run this to configure Eshell to use ~/bin/wget.exe:
;;
;;   (require 'eshell)
;;   (require 'em-alias)
;;   (eshell/alias \"wget\" \"~/bin/wget.exe $*\")
;;
;; The block above only need to be run once.
;; For the technical details, see:
;;
;;   (find-node \"(eshell)Aliases\" \"eshell-aliases-file\")
;;   (find-evardescr 'eshell-directory-name)
;;   (find-evariable 'eshell-directory-name)
;;   (find-evardescr 'eshell-aliases-file)
;;   (find-evariable 'eshell-aliases-file)
;;                    eshell-directory-name
;;                    eshell-aliases-file
;;        (find-fline eshell-directory-name)
;;        (find-fline eshell-aliases-file)
;;
;; Test with:

 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)
which wget
~/bin/wget.exe --version
wget --version



;; 2. Make `eepitch-shell' use Eshell
;; ==================================
;; See: (find-windows-beginner-intro \"5.5. Shell Mode\")
;;      (find-eev \"eepitch.el\" \"eepitch-shell\")
;;
;; Run these sexps:
;;
(defun eepitch-shell  () (interactive) (eepitch-eshell))
(defun eepitch-shell2 () (interactive) (eepitch-eshell2))
(defun eepitch-shell3 () (interactive) (eepitch-eshell3))



 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)
which wget

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
which wget


;; 3. Save this config in your ~/.emacs
;; ====================================
;; If the two eepitch blocks above behaved in the same way,
;; then put this block of four lines in your ~/.emacs:

;; See: (find-wconfig-shell-links)
(defun eepitch-shell  () (interactive) (eepitch-eshell))
(defun eepitch-shell2 () (interactive) (eepitch-eshell2))
(defun eepitch-shell3 () (interactive) (eepitch-eshell3))



;; 4. Note for *nix users testing this
;; ===================================
;; The configuration above can  be reverted with:
;;
(defun eepitch-shell  () (interactive) (eepitch '(shell)))
(defun eepitch-shell2 () (interactive) (eepitch '(shell \"*shell 2*\")))
(defun eepitch-shell2 () (interactive) (eepitch '(shell \"*shell 3*\")))

")
     )
   pos-spec-list))



;;;  ____     _  __ 
;;; |  _ \ __| |/ _|
;;; | |_) / _` | |_ 
;;; |  __/ (_| |  _|
;;; |_|   \__,_|_|  
;;;                 
;; «find-wconfig-pdf-links»  (to ".find-wconfig-pdf-links")
;; Skel: (find-find-links-links-new "wconfig-pdf" "" "")
;; Test: (find-wconfig-pdf-links)
;;       (defun edt () (interactive) (eval-defun nil) (find-wconfig-pdf-links))
;;
(defun find-wconfig-pdf-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig-pdf."
  (interactive)
  (apply
   'find-elinks-elisp
   `((find-wconfig-pdf-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-wconfig-pdf-links)
     ""
     ,(ee-template0 "\
;; Note: only run this after configuring
;; the pdf viewer, wget, and Eshell! See:
;;   (find-newbrowser-links nil nil nil \"3. Configure `find-pdf-page'\")
;;   (find-wconfig-wget-links)
;;   (find-wconfig-shell-links)



;; 1. Download pdftotext and a PDF file
;; ====================================
;; Run:

 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)
cd ~/bin/
rm -fv ~/bin/pdftotext.exe
wget http://angg.twu.net/2021-oficina/pdftotext.exe
rm -fv ~/Coetzee99.pdf
cd
wget https://tannerlectures.utah.edu/_resources/documents/a-to-z/c/Coetzee99.pdf

# Tests:
~/bin/pdftotext.exe --help
#
# (find-callprocess \"~/bin/pdftotext.exe --help\")



;; 2. Configure pdftotext
;; ======================
;; Run this:
(setq ee-pdftotext-program \"~/bin/pdftotext.exe\")
;;
;; And then run these tests:
(find-pdf-page \"~/Coetzee99.pdf\")
(find-pdf-page \"~/Coetzee99.pdf\" 3)
(find-pdf-text \"~/Coetzee99.pdf\")
(find-pdf-text \"~/Coetzee99.pdf\" 3)


")
     )
   pos-spec-list))





;;;  _                
;;; | |   _   _  __ _ 
;;; | |  | | | |/ _` |
;;; | |__| |_| | (_| |
;;; |_____\__,_|\__,_|
;;;                   
;; «find-wconfig-lua-links»  (to ".find-wconfig-lua-links")
;; Skel: (find-find-links-links-new "wconfig-lua" "" "")
;; Test: (find-wconfig-lua-links)
;;       (defun edt () (interactive) (eval-defun nil) (find-wconfig-lua-links))
;;
(defun find-wconfig-lua-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig-lua."
  (interactive)
  (apply
   'find-elinks-elisp
   `((find-wconfig-lua-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-wconfig-lua-links)
     ""
     ,(ee-template0 "\

 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)
rm -fv ~/bin/lua52.exe
wget http://angg.twu.net/2021-oficina/lua52.exe
rm -fv ~/bin/lua52.dll
wget http://angg.twu.net/2021-oficina/lua52.dll

 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)
~/bin/lua5.2 -v
~/bin/lua5.2 -i
  print(2+3)
  os.exit()


;; See: (find-eev-quick-intro \"6.2. Other targets\")

;; Redefine `eepitch-lua51' and `eepitch-lua52' to make
;; them (both) use ~/bin/lua52.exe.

(defun eepitch-lua51 () (interactive)
  (eepitch-comint \"lua52.exe\" \"~/bin/lua52.exe -i\"))
(defun eepitch-lua52 () (interactive)
  (eepitch-comint \"lua52.exe\" \"~/bin/lua52.exe -i\"))


")
     )
   pos-spec-list))




;; «find-wconfig-mpv-links»  (to ".find-wconfig-mpv-links")
;; Skel: (find-find-links-links-new "wconfig-mpv" "" "")
;; Test: (find-wconfig-mpv-links)
;;       (defun edt () (interactive) (eval-defun nil) (find-wconfig-mpv-links))
;;
(defun find-wconfig-mpv-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig-mpv."
  (interactive)
  (apply
   'find-elinks-elisp
   `((find-wconfig-mpv-links ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-wconfig-mpv-links)
     ""
     ,(ee-template0 "\

;; (find-video-links-intro \"6. Configuring Mpv\")

(setenv \"MPVDIR\" \"c:/Users/danie/OneDrive/Documentos/mpv\")
(setq ee-mpv-program \"$MPVDIR/mpv.exe\")

")
     )
   pos-spec-list))




(provide 'eev-wconfig)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
