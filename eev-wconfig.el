;;; eev-wconfig.el -- configure eev on M$ Windows.  -*- lexical-binding: nil; -*-

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
;; Version:    20230127
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-wconfig.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-wconfig.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                 <http://anggtwu.net/eev-intros/find-psne-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-psne-intro)

;;; Comment:

;; «intro»  (to ".intro")
;;
;; 1. Introduction
;; ===============
;; Some parts of eev call external programs:
;;
;;   (find-eev-quick-intro "3.1. Non-elisp hyperlinks")
;;   (find-eev-quick-intro "3.1. Non-elisp hyperlinks" "firefox")
;;   (find-eev-quick-intro "3.1. Non-elisp hyperlinks" "chrome")
;;   (find-eev-quick-intro "6.2. Other targets")
;;   (find-eev-quick-intro "6.2. Other targets" "python")
;;   (find-psne-intro "1. Local copies of files from the internet")
;;   (find-psne-intro "1. Local copies of files from the internet" "wget")
;;   (find-video-links-intro "1. Introduction")
;;   (find-video-links-intro "1. Introduction" "[Video links:]")
;;   (find-video-links-intro "5.1. Subtitles")
;;   (find-audiovideo-intro "4.3. A demo")
;;   (find-audiovideo-intro "4.3. A demo" "mpv")
;;   (find-pdf-like-intro "3. Hyperlinks to PDF files")
;;   (find-pdf-like-intro "3. Hyperlinks to PDF files" "xpdf")
;;   (find-pdf-like-intro "3. Hyperlinks to PDF files" "pdftotext")
;;
;; On *NIX systems the installation of these programs is easy, and no
;; configuration is needed - if "firefox", "google-chrome", "python3",
;; "wget", "xpdf", "pdftotext", and "mpv" are in the path then eev
;; simply calls them with the right arguments, and (usually)
;; everything works.
;;
;; On M$ Windows things are much harder: 1) eev needs to know the full
;; paths of the ".exe"s of these programs, 2) wget and pdftotext are
;; hard to install, 3) it is better to use a browser to open PDFs, 4)
;; I don't have access to a machine with Windows, 5) I have very
;; little access to people who use Windows and who can be convinced to
;; test this...
;;
;; This file is an attempt to solve this problem both "without magic"
;; and "with very little magic". Remember that:
;;
;;     "Any sufficiently advanced technology
;;      is indistinguishable from magic"
;;
;; Here I will use the term "magic" as a shorthand for a "sufficiently
;; advanced technology", i.e., something complex and non-obvious,
;; "that is indistinguishable from magic", in the sense of being
;; almost impossible to understand. I will also use "black box" as a
;; near-synonym for "magic"; "black box" has more letters than
;; "magic", but it invites us to use expressions like "opening the
;; black box".
;;
;; In dec/2021 I recorded a video, called "Org for Non-Users",
;;
;;   http://anggtwu.net/2021-org-for-non-users.html
;;   (find-1stclassvideo-links "2021orgfornonusers")
;;   (find-1stclassvideoindex  "2021orgfornonusers")
;;
;; in which I explained why I always found Org so hard to learn. Many
;; things in Org are implemented in ways that I don't understand, and
;; practically every time that I try to learn more features of Org I
;; start to ask questions like "hey, how is this implemented?", and I
;; get stuck trying to answer these "non-user" questions instead of
;; simply learning how to use the feature "as a user"...
;;
;; In the language of black boxes what happens when I try to learn Org
;; is this. I try to learn a new feature; I see lots of black boxes; I
;; try to open these black boxes, and fail miserably; I get frustrated
;; and exhausted; I postpone learning that feature to another day.
;;
;; I have the same relationship with `M-x customize'.
;;
;; The most "user-friendly" way to configure Emacs is with
;; `customize'. Try:
;;
;;   (eek "M-x customize")
;;   (customize-group    'processes)
;;   (customize-group    'shell)
;;   (customize-variable 'explicit-bash-args)
;;   (customize-face     'eev-glyph-face-red)
;;
;; Over the years I have experimented with several alternatives to
;; customize that "use less magic". This page,
;;
;;   http://anggtwu.net/eev-customize.html
;;
;; from april/2022, describe some of my experiments.
;;
;; I started to work on eev-wconfig.el a few days after making that
;; page. Eev-wconfig.el uses all the ideas there, plus a few new ones.
;;
;; Eev-wconfig can be used both "with no magic" and "with some magic".
;; Both ways create temporary buffers that have parts that perform
;; configuration steps and parts with elisp hyperlinks "that open the
;; (black) boxes" and explain what the configuration steps do.
;;
;;
;; «usage»  (to ".usage")
;;
;; 2. Usage
;; ========
;; To use this, run:
;;
;;   (find-wconfig-links)
;;
;; The `(find-wconfig-links)' will create a temporary buffer with
;; instructions. For more info, see the video:
;;
;;   http://anggtwu.net/eev-wconfig.html
;;   http://anggtwu.net/.emacs.videos.html#2022eevwconfig
;;   (find-1stclassvideo-links "2022eevwconfig")
;;   (find-1stclassvideoindex  "2022eevwconfig")



;; Index:
;; «.intro»				(to "intro")
;; «.usage»				(to "usage")
;; «.find-wconfig-links»		(to "find-wconfig-links")
;;   «.find-wconfig-browser-links»	(to "find-wconfig-browser-links")
;;   «.find-wconfig-wget-links»		(to "find-wconfig-wget-links")
;;   «.find-wconfig-shell-links»	(to "find-wconfig-shell-links")
;;   «.find-wconfig-lua-links»		(to "find-wconfig-lua-links")
;;   «.find-wconfig-mpv-links»		(to "find-wconfig-mpv-links")
;;
;;   «.find-wconfig-magic-links»	(to "find-wconfig-magic-links")
;;     «.ee-wconfig-run-magic»		(to "ee-wconfig-run-magic")
;;
;;   «.find-wconfig-exercises-links»	(to "find-wconfig-exercises-links")
;;
;;   «.find-wconfig-undo-links»		(to "find-wconfig-undo-links")





;;; __        __               __ _       
;;; \ \      / /__ ___  _ __  / _(_) __ _ 
;;;  \ \ /\ / / __/ _ \| '_ \| |_| |/ _` |
;;;   \ V  V / (_| (_) | | | |  _| | (_| |
;;;    \_/\_/ \___\___/|_| |_|_| |_|\__, |
;;;                                 |___/ 
;;
;; «find-wconfig-links»  (to ".find-wconfig-links")
;; Skel: (find-find-links-links-new "wconfig" "" "")
;; Test: (find-wconfig-links)
;;
(defun find-wconfig-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-efunction 'find-wconfig-links)
       ""
       ,(ee-template0 "\
;; See: (find-eev \"eev-wconfig.el\" \"intro\")


;; To configure eev on Windows \"without magic\", follow
;; the instructions in each of the five wconfigs belows:

(find-wconfig-browser-links)
(find-wconfig-wget-links)
(find-wconfig-shell-links)
(find-wconfig-lua-links)
(find-wconfig-mpv-links)


;; To configure eev on Windows \"with some magic\",
;; follow the instructions in:

(find-wconfig-magic-links)


;; Exercise: Learn Org!

(find-wconfig-exercises-links)

")
       )
     pos-spec-list)))




;;;  ____                                  
;;; | __ ) _ __ _____      _____  ___ _ __ 
;;; |  _ \| '__/ _ \ \ /\ / / __|/ _ \ '__|
;;; | |_) | | | (_) \ V  V /\__ \  __/ |   
;;; |____/|_|  \___/ \_/\_/ |___/\___|_|   
;;;                                        
;; «find-wconfig-browser-links»  (to ".find-wconfig-browser-links")
;; Skel: (find-find-links-links-new "wconfig-browser" "" "")
;; Test: (find-wconfig-browser-links)
;; See:  (find-newbrowser-links)
;;       (find-efunction 'find-newbrowser-links)
;;
(defun find-wconfig-browser-links (&rest pos-spec-list)
"Visit a temporary buffer containing a script for configuring the browser."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-browser-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-browser-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-wconfig-links)
       (find-efunction 'find-wconfig-browser-links)
       ""
       ,(ee-template0 "\

;; 0. The variable
;; ===============
;; See: (find-elisp-intro \"5. Variables\")
;; The current value of the variable `ee-googlechrome-program' is:
(setq ee-googlechrome-program
  \"{ee-googlechrome-program}\")


;; 1. Configure Chrome
;; ===================
;; Replace the string below by the full path of chrome.exe
;; and then execute the sexp below with `M-e'.
;; Note that you NEED to replace all the `\\'s in it by `/'s.
;;
(setq ee-googlechrome-program
 \"C:/Program Files (x86)/Google/Chrome/Application/chrome.exe\")


;; 2. Tests
;; ========
;; Try:
;;   (find-googlechrome \"https://www.lua.org/\")
;; Also, the section 3.1 of the main tutorial describes a
;; quick way to open the \"url at point\" with `M-x brg'.
;; Try it:
;;   (find-eev-quick-intro \"3.1. Non-elisp hyperlinks\")


;; 3. Update this page
;; ===================
;; See: (find-links-intro \"5. The first line regenerates the buffer\")
;; This page is generated by a template. Run the sexp in the
;; first line to regenerate this page and to make the first and
;; the last `setq's display the current value of the
;; variable `ee-googlechrome-program'.


;; 4. Save your configuration
;; ==========================
;; The \"init file\" is explained here: (find-enode \"Init File\")
;; Hint: `M-5 M-5 M-j' runs: (find-fline \"~/.emacs\")
;; See: (eek         \"M-j\")
;; Try: (eek \"M-5 M-5 M-j\")
;; If you need to review how to do copy-and-paste and saving, see:
;;   (find-eev-quick-intro \"5.2. Cutting and pasting\")
;;   (find-emacs-keys-intro \"3. Cutting & pasting\")
;;   (find-emacs-keys-intro \"7. Files and buffers\")
;;   (find-emacs-keys-intro \"7. Files and buffers\" \"C-x C-s\")
;;
;; Copy the updated version of the block below -
;; including the three commented lines at the top -
;; to your init file, and save it with `C-x C-s'.

;; See: (find-eev \"eev-load.el\" \"autoloads\")
;;      (find-eev \"eev-wconfig.el\" \"intro\")
;;      (find-wconfig-browser-links)
(require 'eev-load)
(require 'eev-wconfig)
(setq ee-googlechrome-program
  \"{ee-googlechrome-program}\")


;; 5. Save some things to your ~/TODO
;; ==================================
;; Remember that you are encouraged to use the file ~/TODO as a messy
;; log file, and that you can access it with `M-1 M-j'. See:
;;   (find-here-links-intro \"1. Alternating between \\\"task\\\" and \\\"notes\\\"\")
;;   (find-refining-intro \"3. Three buffers\")
;;   (find-refining-intro \"3. Three buffers\" \"~/TODO\")
;;   (find-refining-intro \"3. Three buffers\" \"M-1 M-j\")
;; Copy the two uncommented sexps below to your ~/TODO, and save it:

(find-wconfig-links)
(find-wconfig-browser-links)


;; 6. Test your saved configuration
;; ================================
;; Open a second Emacs without closing this one, and check that in the
;; second Emacs you can run `M-x eev-beginner' to load eev and open
;; `(find-eev-quick-intro)'; then run `M-1 M-j' in the second Emacs to
;; access your ~/TODO, and check that the lines
;;   (find-wconfig-links)
;;   (find-wconfig-browser-links)
;; that you saved in the section 5 are there. Run the tests in the
;; section 2 of `(find-wconfig-browser-links)' and check that they
;; work - i.e., that the second Emacs is using the right value for the
;; variable `ee-googlechrome-program'.
")
       )
     pos-spec-list)))




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
;;
(defun find-wconfig-wget-links (&rest pos-spec-list)
"Visit a temporary buffer containing a script for configuring wget on Windows."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-wget-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-wget-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-wconfig-links)
       (find-efunction 'find-wconfig-wget-links)
       ""
       ,(ee-template0 "\
;; Note: only run this after configuring the browser!
;; See: (find-wconfig-browser-links)


;; 1. Download wget.exe
;; ====================
;; Run each of the uncommented sexps below with `M-e'.
;; Note that some of the sexps are longer than one line.
;; See: (find-eev-quick-intro \"2. Evaluating Lisp\")
;;      (find-eev-quick-intro \"2. Evaluating Lisp\" \"M-0 M-e\")
;;
;; The sexps below will download a wget.exe from:
;;   http://anggtwu.net/eev-wconfig/
;; And put it in:
;;   (find-fline \"~/eev-wconfig/\")

(require 'eww)
(defun ee-download-with-eww (url dir)
  (url-retrieve url #'eww-download-callback (list url dir)))

(mkdir       \"~/eev-wconfig/\" t)
(delete-file \"~/eev-wconfig/wget.exe\")
(ee-download-with-eww \"http://anggtwu.net/eev-wconfig/wget.exe\"
             \"~/eev-wconfig/\")


;; 2. Use wget.exe to download some other files
;; ============================================
;; See: (find-eev-quick-intro \"6. Controlling shell-like programs\")
;;      (find-eev-quick-intro \"6.1. The main key: <F8>\")
;; Run the eepitch block below with `<f8>'s.

 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)
mkdir -p ~/eev-wconfig/
rm -fv   ~/eev-wconfig/README-wconfig.el
rm -fv   ~/eev-wconfig/pdftotext.exe
rm -fv   ~/eev-wconfig/lua52.exe
rm -fv   ~/eev-wconfig/lua52.dll
rm -fv   ~/eev-wconfig/lua53.exe
rm -fv   ~/eev-wconfig/lua53.dll
rm -fv   ~/eev-wconfig/lua54.exe
rm -fv   ~/eev-wconfig/lua54.dll
cd       ~/eev-wconfig/
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/README-wconfig.el
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/pdftotext.exe
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/lua52.exe
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/lua52.dll
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/lua53.exe
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/lua53.dll
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/lua54.exe
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/lua54.dll

# (find-callprocess0 '(\"~/eev-wconfig/wget.exe\"      \"--help\"))
# (find-callprocess0 '(\"~/eev-wconfig/pdftotext.exe\" \"--help\"))
# (find-callprocess0 '(\"~/eev-wconfig/lua52.exe\"     \"-v\"))
# (find-callprocess0 '(\"~/eev-wconfig/lua53.exe\"     \"-v\"))
# (find-callprocess0 '(\"~/eev-wconfig/lua54.exe\"     \"-v\"))

mkdir -p ~/eev-wconfig/
rm -fv   ~/eev-wconfig/Coetzee99.pdf
rm -fv   ~/eev-wconfig/2022dragABC.mp4
cd       ~/eev-wconfig/
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/Coetzee99.pdf
~/eev-wconfig/wget.exe http://anggtwu.net/eev-wconfig/2022dragABC.mp4




;; 3. Make `find-wget' use ~/eev-wconfig/wget.exe
;; ==============================================
;; Run this sexp with `M-e':

(setq ee-wget-program \"~/eev-wconfig/wget.exe\")

;; Then run the tests in the second sexp below:
;;   (find-eev \"eev-plinks.el\" \"find-wget\")
;;   (find-eev \"eev-plinks.el\" \"find-wget\" \";; Tests:\")
;; Some of the tests point to \"anchors\".
;; The concept of \"anchor\" is explained here:
;;   (find-eev-quick-intro \"8. Anchors\")



;; 4. Use the browser as a PDF viewer
;; ==================================
;; Run this sexp with `M-e':

(defalias 'find-pdf-page 'find-googlechrome-page)

;; Tests:
;;   (find-googlechrome \"~/eev-wconfig/Coetzee99.pdf\")
;;   (find-pdf-page     \"~/eev-wconfig/Coetzee99.pdf\")
;;   (find-pdf-page     \"~/eev-wconfig/Coetzee99.pdf\" 3)
;; For the technical details, see:
;;   (find-efunctiondescr 'find-pdf-page)
;;   (find-eev \"eev-pdflike.el\" \"find-googlechrome-page\")
;;   (find-eev \"eev-pdflike.el\" \"change-default-viewer\")


;; 5. Use ~/eev-wconfig/pdftotext.exe to view PDFs as text
;; =======================================================
;; Run this sexp with `M-e':

(setq ee-pdftotext-program \"~/eev-wconfig/pdftotext.exe\")

;; Tests:
;;   (find-pdf-text \"~/eev-wconfig/Coetzee99.pdf\")
;;   (find-pdf-text \"~/eev-wconfig/Coetzee99.pdf\" 3)
;; For the technical details, see:
;;   (find-eev \"eev-pdflike.el\" \"find-pdftotext-text\")



;; 6. Test if the browser can play videos
;; ======================================
;; Try this:
;;   (find-googlechrome \"~/eev-wconfig/2022dragABC.mp4\")
;; The browser doesn't support \"time offsets\" - see:
;;   (find-audiovideo-intro \"1. Time offsets\")
;; We will need to install mpv for that.




;; 7. Save these configs in your init file
;; =======================================
;; This is similar to: (find-wconfig-browser-links 2 \"4. Save\")
;; Save the block below - including the comment - to your ~/.emacs:

;; See: (find-wconfig-wget-links)
(setq ee-wget-program      \"~/eev-wconfig/wget.exe\")
(setq ee-pdftotext-program \"~/eev-wconfig/pdftotext.exe\")
(defalias 'find-pdf-page 'find-googlechrome-page)


")
       )
     pos-spec-list)))





;;;  ____  _          _ _ 
;;; / ___|| |__   ___| | |
;;; \___ \| '_ \ / _ \ | |
;;;  ___) | | | |  __/ | |
;;; |____/|_| |_|\___|_|_|
;;;                       
;; «find-wconfig-shell-links»  (to ".find-wconfig-shell-links")
;; Skel: (find-find-links-links-new "wconfig-shell" "" "")
;; Test: (find-wconfig-shell-links)
;;
(defun find-wconfig-shell-links (&rest pos-spec-list)
"Visit a temporary buffer containing a script for configuring eshell on Windows."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-shell-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-shell-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-wconfig-links)
       (find-efunction 'find-wconfig-shell-links)
       ""
       ,(ee-template0 "\
;; Note: only run this after configuring wget!
;; See: (find-wconfig-wget-links)


;; 1. Make Eshell use wget.exe
;; ===========================
;; Run the sexps below to configure Eshell to use ~/eev-wconfig/wget.exe:

(require 'eshell)
(require 'em-alias)
(eshell/alias \"wget\")
(eshell/alias \"wget\" \"~/eev-wconfig/wget.exe $*\")

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
~/eev-wconfig/wget.exe --version
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

;; Test: check that the two eepitch
;; blocks below both run Eshell.

 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)



;; 3. Configure the `echo' of eshell
;; =================================
;; See: (find-windows-beginner-intro \"5.6.1. Echo in Eshell\")
(defun ee-find-psne-echo-options () \"-N\")



;; 4. Check if `M-x brep' and psne-ing work
;; ========================================
;; See: (find-psne-intro)
;; and run the test in section 3:
;;      (find-psne-intro \"3. The new way: `M-x brep'\")
;; Also, check if in the temporary buffer generated by this sexp:
;;      (find-psne-links \"https://www.lua.org/index.html\")
;; you get an \"echo -N\" or just an \"echo\".
;; You should get \"echo -N\".




;; 5. Save some configs in your init file
;; ======================================
;; This is similar to: (find-wconfig-browser-links 2 \"4. Save\")
;; Save the block below - including the comments - to your ~/.emacs:

;; See: (find-wconfig-shell-links)
(defun eepitch-shell  () (interactive) (eepitch-eshell))
(defun eepitch-shell2 () (interactive) (eepitch-eshell2))
(defun eepitch-shell3 () (interactive) (eepitch-eshell3))
(defun ee-find-psne-echo-options () \"-N\")

")
       )
     pos-spec-list)))





;;;  _                
;;; | |   _   _  __ _ 
;;; | |  | | | |/ _` |
;;; | |__| |_| | (_| |
;;; |_____\__,_|\__,_|
;;;                   
;; «find-wconfig-lua-links»  (to ".find-wconfig-lua-links")
;; Skel: (find-find-links-links-new "wconfig-lua" "" "")
;; Test: (find-wconfig-lua-links)
;;
(defun find-wconfig-lua-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig-lua."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-lua-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-lua-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-wconfig-links)
       (find-efunction 'find-wconfig-lua-links)
       ""
       ,(ee-template0 "\
;; Note: only run this after downloading Lua!
;; See: (find-wconfig-wget-links 2 \"2. Use wget.exe to download\")


;; 1. Test if we can run Lua from Eshell
;; =====================================
;; The option `-i' means \"run in interactive mode\".

 (eepitch-eshell)
 (eepitch-kill)
 (eepitch-eshell)
~/eev-wconfig/lua52.exe -v
~/eev-wconfig/lua52.exe -i
  print(2+3)
  os.exit()



;; 2. Configure eepitch
;; ====================
;; See: (find-eev-quick-intro \"6.2. Other targets\")
;; Redefine `eepitch-lua51' and `eepitch-lua52' to make
;; them (both) use ~/eev-wconfig/lua52.exe.

(defun eepitch-lua51 () (interactive)
  (eepitch-comint \"lua52.exe\" \"~/eev-wconfig/lua52.exe -i\"))
(defun eepitch-lua52 () (interactive)
  (eepitch-comint \"lua52.exe\" \"~/eev-wconfig/lua52.exe -i\"))
(defun eepitch-lua53 () (interactive)
  (eepitch-comint \"lua53.exe\" \"~/eev-wconfig/lua53.exe -i\"))
(defun eepitch-lua54 () (interactive)
  (eepitch-comint \"lua54.exe\" \"~/eev-wconfig/lua54.exe -i\"))

;; Test:

 (eepitch-lua51)
 (eepitch-kill)
 (eepitch-lua51)
print(2+3)
for k,v in pairs(_G) do print(k) end



;; 3. Other languages: Python
;; ==========================
;; If you have Python installed, please test the block below...
;; According to these messages in help-gnu-emacs,
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2022-06/msg00019.html
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2022-06/msg00020.html
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2022-06/msg00022.html
;; on Windows Python needs \"-u\" and \"-i\" to work as a REPL in Eshell.

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
python3 -u -i
print(2+3)

;; If that worked - i.e., if after the print(2+3) you got a \"5\"
;; and another prompt - then try to redefine `eepitch-python' with:

(defun eepitch-python () (interactive)
  (eepitch-comint \"python3\" \"python3 -u -i\"))

;; ...and test if this works. Note that here we send four lines to
;; the Python REPL: a definition for the function \"square\", a
;; blank line, and a test that calls \"square\".

 (eepitch-python)
 (eepitch-kill)
 (eepitch-python)
def square (x):
    return x*x

print(square(5))




;; 4. Save some configs in your init file
;; ======================================
;; This is similar to: (find-wconfig-browser-links 2 \"4. Save\")
;; Save the block below - including the comments - to your ~/.emacs:

;; See: (find-wconfig-lua-links)
(defun eepitch-lua51 () (interactive)
  (eepitch-comint \"lua52.exe\" \"~/eev-wconfig/lua52.exe -i\"))
(defun eepitch-lua52 () (interactive)
  (eepitch-comint \"lua52.exe\" \"~/eev-wconfig/lua52.exe -i\"))
(defun eepitch-lua53 () (interactive)
  (eepitch-comint \"lua53.exe\" \"~/eev-wconfig/lua53.exe -i\"))
(defun eepitch-lua54 () (interactive)
  (eepitch-comint \"lua54.exe\" \"~/eev-wconfig/lua54.exe -i\"))
(defun eepitch-python () (interactive)
  (eepitch-comint \"python3\" \"python3 -u -i\"))

")
       )
     pos-spec-list)))



;;;  __  __            
;;; |  \/  |_ ____   __
;;; | |\/| | '_ \ \ / /
;;; | |  | | |_) \ V / 
;;; |_|  |_| .__/ \_/  
;;;        |_|         
;;
;; «find-wconfig-mpv-links»  (to ".find-wconfig-mpv-links")
;; Skel: (find-find-links-links-new "wconfig-mpv" "" "")
;; Test: (find-wconfig-mpv-links)
;;
(defun find-wconfig-mpv-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig-mpv."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-mpv-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-mpv-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-wconfig-links)
       (find-efunction 'find-wconfig-mpv-links)
       ""
       ,(ee-template0 "\
;; This configuration step is similar to:
;;   (find-wconfig-browser-links)

;; The current value of the variable `ee-mpv-program' is:
(setq ee-mpv-program
  \"{ee-mpv-program}\")


;; 1. Configure the path
;; =====================
;; Make sure that mpv is installed.
;; See: https://en.wikipedia.org/wiki/Mpv_(media_player)
;;      https://mpv.io/
;;      https://mpv.io/installation/
;;
;; Replace the string below by the full path of mpv.exe -
;; note that you NEED to replace all the \\s in it by /s:

(setq ee-mpv-program
  \"c:/Users/danie/OneDrive/Documentos/mpv/mpv.eve\")


;; 2. Test
;; =======
;; This test uses a short video that was downloaded by:
;;   (find-wconfig-wget-links \"2. Use wget.exe to download\")
;; Check that the file exists: 
;;   (find-fline \"~/eev-wconfig/\" \"2022dragABC.mp4\")
;; Try to play it with mpv:
;;   (find-mpv-video \"~/eev-wconfig/2022dragABC.mp4\")
;;   (find-video     \"~/eev-wconfig/2022dragABC.mp4\")


;; 3. Saving
;; =========
;; This is similar to: (find-wconfig-browser-links 2 \"4. Save\")
;; Save the block below - including the comment - to your ~/.emacs:

;; See: (find-wconfig-mpv-links)
(setq ee-mpv-program
  \"{ee-mpv-program}\")

")
       )
     pos-spec-list)))



;;;  __  __             _      
;;; |  \/  | __ _  __ _(_) ___ 
;;; | |\/| |/ _` |/ _` | |/ __|
;;; | |  | | (_| | (_| | | (__ 
;;; |_|  |_|\__,_|\__, |_|\___|
;;;               |___/        
;;
;; «find-wconfig-magic-links»  (to ".find-wconfig-magic-links")
;; Skel: (find-find-links-links-new "wconfig-magic" "" "")
;; Test: (find-wconfig-magic-links)
;;
(defun find-wconfig-magic-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig-magic."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-magic-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-magic-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-wconfig-links)
       (find-efunction 'find-wconfig-magic-links)
       ""
       ,(ee-template0 "\
;; You are not expected to understand this!
;; See: https://thenewstack.io/not-expected-understand-explainer/


;; 1. Configure chrome and mpv
;; ===========================
;; See: (find-wconfig-browser-links \"1. Configure Chrome\")
;;      (find-wconfig-browser-links \"3. Update this page\")
;;      (find-wconfig-mpv-links)
;; The current values of `ee-googlechrome-program'
;; and `ee-mpv-program' are:
;;
(setq ee-googlechrome-program
  \"{ee-googlechrome-program}\")
(setq ee-mpv-program
  \"{ee-mpv-program}\")

;; Set them to full paths, like:
(setq ee-googlechrome-program
 \"C:/Program Files (x86)/Google/Chrome/Application/chrome.exe\")
(setq ee-mpv-program
  \"c:/Users/danie/OneDrive/Documentos/mpv/mpv.eve\")



;; 2. Things that you need to run once
;; ===================================
;; See: (find-wconfig-wget-links)
;;      (find-wconfig-shell-links)



;; 3. Magic
;; ========
;; Running `(ee-wconfig-run-magic)'
;; runs the three blocks of code below.
;; See: (find-efunction     'ee-wconfig-run-magic)
;;      (find-estring-elisp 'ee-wconfig-magic-code)

{ee-wconfig-magic-code}



;; 4. Save your configuration
;; ==========================
;; See: (find-wconfig-browser-links \"4. Save your configuration\")

;; See: (find-wconfig-magic-links)
;;      (find-wconfig-magic-links 2 \"1. Configure chrome and mpv\")
;;      (find-wconfig-magic-links 2 \"2. Things that you need to run once\")
;;      (find-wconfig-magic-links 2 \"3. Magic\")
;;      (find-eev \"eev-load.el\" \"autoloads\")
;;      (find-eev \"eev-wconfig.el\" \"intro\")
;;
(require 'eev-load)
(require 'eev-wconfig)
(setq ee-googlechrome-program
  \"{ee-googlechrome-program}\")
(setq ee-mpv-program
  \"{ee-mpv-program}\")
(ee-wconfig-run-magic)


")
       )
     pos-spec-list)))




;; «ee-wconfig-run-magic»  (to ".ee-wconfig-run-magic")
;; Test: (find-estring-elisp ee-wconfig-magic-code)
(defun ee-wconfig-run-magic ()
  "This function simply runs `(eval ee-wconfig-magic-code)'.
See the variable `ee-wconfig-magic-code'."
  (interactive)
  (eval ee-wconfig-magic-code))

(defvar ee-wconfig-magic-code "\
;; See: (find-wconfig-wget-links)
(setq ee-wget-program      \"~/eev-wconfig/wget.exe\")
(setq ee-pdftotext-program \"~/eev-wconfig/pdftotext.exe\")
(defalias 'find-pdf-page 'find-googlechrome-page)

;; See: (find-wconfig-shell-links)
(defun eepitch-shell  () (interactive) (eepitch-eshell))
(defun eepitch-shell2 () (interactive) (eepitch-eshell2))
(defun eepitch-shell3 () (interactive) (eepitch-eshell3))
(defun ee-find-psne-echo-options () \"-N\")

;; See: (find-wconfig-lua-links)
(defun eepitch-lua51 () (interactive)
  (eepitch-comint \"lua52.exe\" \"~/bin/lua52.exe -i\"))
(defun eepitch-lua52 () (interactive)
  (eepitch-comint \"lua52.exe\" \"~/bin/lua52.exe -i\"))
(defun eepitch-lua53 () (interactive)
  (eepitch-comint \"lua53.exe\" \"~/bin/lua53.exe -i\"))
(defun eepitch-lua54 () (interactive)
  (eepitch-comint \"lua54.exe\" \"~/bin/lua54.exe -i\"))
")




;;;  _____                   _               
;;; | ____|_  _____ _ __ ___(_)___  ___  ___ 
;;; |  _| \ \/ / _ \ '__/ __| / __|/ _ \/ __|
;;; | |___ >  <  __/ | | (__| \__ \  __/\__ \
;;; |_____/_/\_\___|_|  \___|_|___/\___||___/
;;;                                          
;; «find-wconfig-exercises-links»  (to ".find-wconfig-exercises-links")
;; Skel: (find-find-links-links-new "wconfig-exercises" "" "")
;; Test: (find-wconfig-exercises-links)
;;
(defun find-wconfig-exercises-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig-exercises."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-exercises-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-exercises-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-wconfig-links)
       (find-efunction 'find-wconfig-exercises-links)
       ""
       ,(ee-template0 "\
;; Exercise: learn Org!
;; See: http://anggtwu.net/eev-wconfig.html
;;      http://anggtwu.net/eev-wconfig.html#learn-org


;; 1. Understand `code-c-d'
;; ========================
;; (find-eev-quick-intro \"9. Shorter hyperlinks\")
;; (find-eev-quick-intro \"9. Shorter hyperlinks\" \"[Video links:]\")
;; (find-eev-quick-intro \"9.1. `code-c-d'\")

;; 2. Download the .zip
;; ====================
;; From:
;;   http://anggtwu.net/eev-wconfig.html#learn-org
;; and unpack it.

;; 3. Create a link to the directory
;; =================================
;; Something like:
;;   (find-fline \"C:/I/unpacked/the/videos/here/\")
;; This may help:
;;   (find-enode \"Dired\")

;; 3. Understand `M-h M-e'
;; =======================
;; See: (find-audiovideo-intro \"4.1. `find-extra-file-links'\")
;; and use that to create links like these:

(code-c-d \"rainerkoenig\" \"C:/I/unpacked/the/videos/here/\")
;; (find-rainerkoenigfile \"\")
(code-video \"E02S01video\" \".../OrgMode_E02S01_-_Tags-GcUVvlClo9k.webm\")
;; (find-E02S01video \"0:00\")



")
       )
     pos-spec-list)))




;;;  _   _           _       
;;; | | | |_ __   __| | ___  
;;; | | | | '_ \ / _` |/ _ \ 
;;; | |_| | | | | (_| | (_) |
;;;  \___/|_| |_|\__,_|\___/ 
;;;                          
;; «find-wconfig-undo-links»  (to ".find-wconfig-undo-links")
;; Skel: (find-find-links-links-new "wconfig-undo" "" "")
;; Test: (find-wconfig-undo-links)
;;
(defun find-wconfig-undo-links (&rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for wconfig-undo."
  (interactive)
  (let ((ee-buffer-name (or ee-buffer-name "*(find-wconfig-undo-links)*")))
    (apply
     'find-elinks-elisp
     `((find-wconfig-undo-links ,@pos-spec-list)
       ;; Convention: the first sexp always regenerates the buffer.
       (find-efunction 'find-wconfig-undo-links)
       ""
       ,ee-wconfig-undo-links
       )
     pos-spec-list)))

(defvar ee-wconfig-undo-links "\
;; You are not expected to understand this!
;; See: https://thenewstack.io/not-expected-understand-explainer/
;; The progn below contains some direty tricks for testing this
;; on a machine running Debian GNU/Linux.

(progn

(setq ee-googlechrome-program \"google-chrome\")

;; (find-angg \"bin/eev-wconfig\")
(find-sh0 \"eev-wconfig d\")
(find-sh0 \"eev-wconfig w\")
;; (find-sh0 \"eev-wconfig c\")

(setq ee-wget-program \"wget\")
(defalias 'find-pdf-page 'find-xpdf-page)
(setq ee-pdftotext-program \"pdftotext\")

(require 'eshell)
(require 'em-alias)
(eshell/alias \"wget\")
(defun eepitch-shell  () (interactive) (eepitch '(shell)))
(defun eepitch-shell2 () (interactive) (eepitch '(shell \"*shell 2*\")))
(defun eepitch-shell2 () (interactive) (eepitch '(shell \"*shell 3*\")))
(defun ee-find-psne-echo-options () \"\")

(setq ee-mpv-program \"mpv\")

(defun eepitch-lua51  () (interactive) (eepitch-comint \"lua51\" \"lua5.1\"))
(defun eepitch-lua52  () (interactive) (eepitch-comint \"lua52\" \"lua5.2\"))
(defun eepitch-lua53  () (interactive) (eepitch-comint \"lua53\" \"lua5.3\"))
(defun eepitch-lua54  () (interactive) (eepitch-comint \"lua54\" \"lua5.4\"))
(defun eepitch-python () (interactive) (eepitch-comint \"python3\" \"python3\"))

)
")







(provide 'eev-wconfig)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
