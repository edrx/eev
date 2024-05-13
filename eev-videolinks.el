;;; eev-videolinks.el --- support for [Video links:] blocks.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.
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
;; Version:    20240513
;; Keywords:   e-scripts
;;
;; Latest version: <http://anggtwu.net/eev-current/eev-videolinks.el>
;;       htmlized: <http://anggtwu.net/eev-current/eev-videolinks.el.html>
;;       See also: <http://anggtwu.net/eev-current/eev-beginner.el.html>
;;                 <http://anggtwu.net/eev-intros/find-eev-intro.html>
;;                 <http://anggtwu.net/eev-intros/find-links-intro.html>
;;                                               (find-eev-intro)
;;                                               (find-links-intro)

;; (load "eev-videolinks.el")

;; «.find-eev-video»			(to "find-eev-video")
;; «.find-eevlocal-links»		(to "find-eevlocal-links")
;; «.select»				(to "select")
;;   «.ee-use-youtube-videos»		(to "ee-use-youtube-videos")
;;   «.ee-use-local-videos»		(to "ee-use-local-videos")
;; «.more-info»				(to "more-info")
;; «.ee-1stclassvideos-info»		(to "ee-1stclassvideos-info")
;;   «.eev2019»				(to "eev2019")
;;   «.eev2020»				(to "eev2020")
;;   «.eev2021»				(to "eev2021")
;;   «.eevnav»				(to "eevnav")
;;   «.eevtempl»			(to "eevtempl")
;;   «.eevfherel»			(to "eevfherel")
;;   «.eevtestbls»			(to "eevtestbls")
;;   «.eevvlinks»			(to "eevvlinks")
;;   «.oficina20210»			(to "oficina20210")
;;   «.oficina2021a»			(to "oficina2021a")
;;   «.oficina2021b»			(to "oficina2021b")
;;   «.2021ssr»				(to "2021ssr")
;;   «.2021workshop1»			(to "2021workshop1")
;;   «.2021workshop2»			(to "2021workshop2")
;;   «.2021workshop3»			(to "2021workshop3")
;;   «.2021workshop4»			(to "2021workshop4")
;;   «.2021workshop5»			(to "2021workshop5")
;;   «.2021workshop6»			(to "2021workshop6")
;;   «.2021orgfornonusers»		(to "2021orgfornonusers")
;;   «.2021ffll»			(to "2021ffll")
;;   «.2022eevmake0»			(to "2022eevmake0")
;;   «.2022findeevangg»			(to "2022findeevangg")
;;   «.2022findelispintro»		(to "2022findelispintro")
;;   «.2022pict2elua»			(to "2022pict2elua")
;;   «.2022eevwconfigpt1»		(to "2022eevwconfigpt1")
;;   «.2022eevwconfigpt2»		(to "2022eevwconfigpt2")
;;   «.2022eevwconfig»			(to "2022eevwconfig")
;;   «.2022yttranscript»		(to "2022yttranscript")
;;   «.2022tikz»			(to "2022tikz")
;;   «.eev2022kla»			(to "eev2022kla")
;;   «.eev2022py»			(to "eev2022py")
;;   «.eev2023repls»			(to "eev2023repls")
;;   «.eev2023replsb»			(to "eev2023replsb")
;;   «.2024git»				(to "2024git")
;;   «.2024luaso»			(to "2024luaso")
;; «.ee-1stclassvideos-field»		(to "ee-1stclassvideos-field")
;; «.second-class-videos»		(to "second-class-videos")
;;   «.code-eevvideo»			(to "code-eevvideo")
;;   «.code-youtubevideo»		(to "code-youtubevideo")
;; «.first-class-videos»		(to "first-class-videos")
;;  «.video-tutorials»			(to "video-tutorials")
;; «.strange-functions»			(to "strange-functions")

;;; Commentary:

;; In may/2021 I implemented the "[Video links:]" blocks in the
;; tutorials of eev, and I explained everything here:
;;
;;   http://anggtwu.net/2021-video-links.html
;;   (find-1stclassvideo-links "eevvlinks")
;;
;; ...but the innards of that first implementation were very ugly, and
;; I changed them several times.
;;
;; In dec/2022 I created this page,
;;
;;   http://anggtwu.net/eev-videos.html
;;
;; that explains in a much better way how these "[Video links:]" work,
;; with screenshots, and focusing on the idea of "First-class videos".
;; Each one of the "first-class videos" has a lot of information
;; associated to it, and the best way to access this information is by
;; running this:
;;
;;   (find-1stclassvideos)
;;
;; See:
;;
;;   (find-video-links-intro)
;;   (find-video-links-intro "7. `find-eev-video'")
;;   (find-video-links-intro "9. First-class videos")
;;   (find-audiovideo-intro "4. Short hyperlinks to audio and video files")




;;;   __ _           _                                 _     _            
;;;  / _(_)_ __   __| |       ___  _____   __   __   _(_) __| | ___  ___  
;;; | |_| | '_ \ / _` |_____ / _ \/ _ \ \ / /___\ \ / / |/ _` |/ _ \/ _ \ 
;;; |  _| | | | | (_| |_____|  __/  __/\ V /_____\ V /| | (_| |  __/ (_) |
;;; |_| |_|_| |_|\__,_|      \___|\___| \_/       \_/ |_|\__,_|\___|\___/ 
;;;                                                                       
;; «find-eev-video»  (to ".find-eev-video")
;; See: (find-video-links-intro "7. `find-eev-video'")
;;      (find-video-links-intro "7. `find-eev-video'" "setq")
;; Tests: (setq ee-find-eev-video-function 'find-eevyoutube-video)
;;        (setq ee-find-eev-video-function 'find-eevlocal-video)
;;        (setq ee-find-eev-video-function 'find-eevlinks-video)
;;        (find-eev-video "emacsconf2020" "hOAqBc42Gg8" "6:25")
;;
(defvar ee-find-eev-video-function 'find-eevlocal-video)

(defun find-eev-video (mp4stem hash &optional time &rest rest)
  (apply ee-find-eev-video-function mp4stem hash time rest))

(defun find-eevyoutube-video (mp4stem hash &optional time &rest rest)
  (find-youtube-video hash time))

(defun find-eevlocal-video (mp4stem hash &optional time &rest rest)
  (let* ((url (format "http://anggtwu.net/eev-videos/%s.mp4" mp4stem))
	 (fname (ee-shorten-file-name (ee-url-to-fname url))))
    (if (ee-psne-downloaded-p url)
	(find-mpv-video fname time)
      (find-eevlocal-links mp4stem hash time))))

(defun find-eevlinks-video (mp4stem hash &optional time &rest rest)
  (find-eevlocal-links mp4stem hash time))




;;;                  _                 _       _ _       _        
;;;   ___  _____   _| | ___   ___ __ _| |     | (_)_ __ | | _____ 
;;;  / _ \/ _ \ \ / / |/ _ \ / __/ _` | |_____| | | '_ \| |/ / __|
;;; |  __/  __/\ V /| | (_) | (_| (_| | |_____| | | | | |   <\__ \
;;;  \___|\___| \_/ |_|\___/ \___\__,_|_|     |_|_|_| |_|_|\_\___/
;;;                                                               
;; «find-eevlocal-links»  (to ".find-eevlocal-links")
;; Skel: (find-find-links-links-new "eevlocal" "stem hash time" "")
;; Tests: (find-eevlocal-links "emacsconf1234")
;;        (find-eevlocal-links "emacsconf2020")
;;        (find-eevlocal-links "emacsconf2020" nil "1:23")
;;
(defun find-eevlocal-links (&optional stem hash time &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for eevlocal."
  (interactive)
  (setq stem (or stem "{stem}"))
  (setq hash (or hash "{hash}"))
  (setq time (or time "{time}"))
  (apply
   'find-elinks
   `((find-eevlocal-links ,stem ,hash ,time ,@pos-spec-list)
     ;; Convention: the first sexp always regenerates the buffer.
     ,(ee-eevlocal-findmpvvideo stem time)
     ;; ""
     ,(ee-H "See:")
     (find-video-links-intro "7. `find-eev-video'")
     (find-video-links-intro "7. `find-eev-video'" "find-eevlocal-video")
     ""
     ,(ee-eevlocal-body stem hash time)
     )
   pos-spec-list))

;; Tests:
;; (find-estring (ee-eevlocal-body "emacsconf2020" "0123456789a" nil))
;; (find-estring (ee-eevlocal-body "emacsconf2020" "0123456789a" "1:23"))
;; (find-estring (ee-eevlocal-body "emacsconf1234" "0123456789a" "1:23"))
;; (find-estring (ee-eevlocal-youtube-comment "0123456789a" nil))
;; (find-estring (ee-eevlocal-youtube-comment "0123456789a" ""))
;; (find-estring (ee-eevlocal-youtube-comment "0123456789a" "1:23"))
;; (find-estring (ee-psne-url-comment "http://www.foo.org/bar.html"))
;; (find-estring (ee-eevlocal-psne "emacsconf1234" nil))
;; (find-estring (ee-eevlocal-psne "emacsconf2020" nil))
;; (find-estring (ee-eevlocal-psne "emacsconf2020" "1:23"))

(defun ee-eevlocal-body (stem hash time)
  "An internal function used by `find-eevlocal-links'."
  (let* ((url (format "http://anggtwu.net/eev-videos/%s.mp4" stem))
	 (youtube-comment (ee-eevlocal-youtube-comment hash time))
	 (psne-comment    (ee-psne-url-comment         url))
	 (psne            (ee-eevlocal-psne            stem time)))
    (ee-template0 "\
{youtube-comment}
{psne-comment}
{psne}\
")))

(defun ee-eevlocal-youtube-comment (hash time)
  "An internal function used by `find-eevlocal-links'."
  (let* ((youtubeurl (ee-find-youtube-url hash time))
	 (timearg (ee-time-to-arg time)))
    (ee-template0 "\
# Youtube:
# (kill-new \"{youtubeurl}\")
#            {youtubeurl}
# (find-youtube-video \"{hash}\"{timearg})
")))

;; Tests:
;; (find-estring (ee-eevlocal-psne "emacsconf2020" nil))
;; (find-estring (ee-eevlocal-psne "emacsconf2020" "1:23"))
;; (find-estring (ee-eevlocal-psne "emacsconf2345" nil))
;; (find-estring (ee-psne-if-needed "https://www.lua.org/index.html"))
;; (find-estring (ee-psne-if-needed "http://foo/bar"))
;; (find-estring (ee-eevlocal-findmpvvideo "emacsconf2020" nil))
;; (find-estring (ee-eevlocal-findmpvvideo "emacsconf2020" "{foo}"))
;; (find-estring (ee-eevlocal-findmpvvideo "emacsconf2020" "1:23"))
;;
(defun ee-eevlocal-psne (stem time)
  "An internal function used by `find-eevlocal-links'."
  (let* ((url (format "http://anggtwu.net/eev-videos/%s.mp4" stem)))
    (concat (ee-psne-if-needed url)
	    (if (not (ee-psne-downloaded-p url))
		(concat "\n" (ee-eevlocal-findmpvvideo stem time))))))

(defun ee-eevlocal-findmpvvideo (stem time)
  "An internal function used by `find-eevlocal-links'."
  (let* ((url     (format "http://anggtwu.net/eev-videos/%s.mp4" stem))
	 (fname   (ee-shorten-file-name (ee-url-to-fname url)))
	 (timearg (ee-time-to-arg time))) 
    (ee-template0 "\
# (find-mpv-video \"{fname}\"{timearg})
")))







;;;  ____       _           _   
;;; / ___|  ___| | ___  ___| |_ 
;;; \___ \ / _ \ |/ _ \/ __| __|
;;;  ___) |  __/ |  __/ (__| |_ 
;;; |____/ \___|_|\___|\___|\__|
;;;                             
;; «select»  (to ".select")
;; Tests: (ee-use-eevvideo-links)
;;        (ee-use-youtube-videos)
;;        (find-eevtestblsvideo "2:33")
;;
;; «ee-use-youtube-videos»  (to ".ee-use-youtube-videos")
(defun ee-use-youtube-videos ()
  "Make `find-eev-video' play videos on youtube using a browser.
This is a quick hack inspired by a workshop for Windows users. On
Windows it is hard to configure the mechanism that downloads
local copies of videos and plays the local copies with mpv, and
this makes the default behavior of the links in [Video links:]
blocks very inconvenient for beginners. This hack redefines the
function used by `find-eev-links', that is used by the links to
videos that are used in [Video links:] blocks, to make those
links use a browser to play the videos on youtube. To get back
the default behavior, run `ee-use-find-eevlocal-links'."
  (interactive)
  (setq ee-find-eev-video-function 'find-eevyoutube-video))

;; «ee-use-local-videos»  (to ".ee-use-local-videos")
(defun ee-use-local-videos ()
  "Use the default behavior for `find-eev-video'.
With the default definition the links in the [Video links:]
blocks of the tutorials of eev will try to download local copies
of the videos and play them with mpv. Compare with
`ee-use-find-eevyoutube-video'."
  (interactive)
  (setq ee-find-eev-video-function 'find-eevlocal-video))







;;;  __  __                  _        __       
;;; |  \/  | ___  _ __ ___  (_)_ __  / _| ___  
;;; | |\/| |/ _ \| '__/ _ \ | | '_ \| |_ / _ \ 
;;; | |  | | (_) | | |  __/ | | | | |  _| (_) |
;;; |_|  |_|\___/|_|  \___| |_|_| |_|_|  \___/ 
;;;                                            
;; «more-info»  (to ".more-info")
;; «ee-1stclassvideos-info»  (to ".ee-1stclassvideos-info")
;; Info on the first-class videos, in a format that is easy to access
;; from Lisp. The most user-friendly way to access this info is with:
;;
;;   (find-1stclassvideos)
;;
;; that shows a temporary buffer with many lines like these ones:
;;
;;   (find-1stclassvideo-links "eev2021")
;;
;; Each entry in `ee-1stclassvideos-info' is of the form (c plist).
;; The term `c' is a reference to this:
;;
;;   (find-eev-quick-intro "9.1. `code-c-d'" "{c}")
;;   (find-eev-quick-intro "9.1. `code-c-d'" "{c}" "code")
;;
;; For example, the entry ("eev2019" ...) contains info about the
;; video that can be played with `(find-eev2019video)'.
;;
(defvar ee-1stclassvideos-info
  '(;;
    ;; «eev2019»  (to ".eev2019")
    ;; Play: (find-eev2019video "0:00")
    ;; Index: (find-1stclassvideoindex "eev2019")
    ("eev2019"
     :title "How to record executable notes with eev - and how to play them back"
     :mp4   "http://anggtwu.net/eev-videos/emacsconf2019.mp4"
     :yt    "http://www.youtube.com/watch?v=86yiRG8YJD0"
     :page  "http://anggtwu.net/emacsconf2019.html"
     :hsubs "http://anggtwu.net/emacsconf2019-subtitles.html#00:00"
     :date    "2019nov02"
     :length  "19:21"
     :subs    ".vtt"
     :index   t
     :comment "A good non-technical introduction to eev.")
    ;;
    ;; «eev2020»  (to ".eev2020")
    ;; Play: (find-eev2020video "0:00")
    ;; Index: (find-1stclassvideoindex "eev2020")
    ("eev2020"
     :title "On why most of the best features in eev look like 5-minute hacks"
     :mp4   "http://anggtwu.net/eev-videos/emacsconf2020.mp4"
     :yt    "http://www.youtube.com/watch?v=hOAqBc42Gg8"
     :page  "http://anggtwu.net/emacsconf2020.html"
     :hsubs "http://anggtwu.net/emacsconf2020.html#00:00"
     :date    "2020nov28"
     :length  "47:08"
     :subs    ".vtt"
     :index   t
     :comment "A good technical introduction to eev.")
    ;;
    ;; «eev2021»  (to ".eev2021")
    ;; Play: (find-eev2021video "0:00")
    ;; Index: (find-1stclassvideoindex "eev2021")
    ("eev2021"
     :title "Test blocks"
     :mp4   "http://anggtwu.net/eev-videos/emacsconf2021.mp4"
     :yt    "http://www.youtube.com/watch?v=qM0Luz78qGw"
     :page  "http://anggtwu.net/emacsconf2021.html"
     :hsubs "http://anggtwu.net/emacsconf2021.html#00:00"
     :date    "2021nov21"
     :length  "6:04"
     :subs    ".vtt"
     :index   t
     :comment "START BY THIS VIDEO!!!")
    ;;
    ;; «eev2021b»  (to ".eev2021b")
    ;; Play: (find-eev2021bvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eev2021b")
    ("eev2021b"
     :title "Test blocks in Dednat6"
     :mp4   "http://anggtwu.net/eev-videos/emacsconf2021-dednat6.mp4"
     :yt    "http://www.youtube.com/watch?v=QUMo7vgkHJI"
     :page  "http://anggtwu.net/emacsconf2021.html#real-world-example"
     :hsubs "http://anggtwu.net/emacsconf2021-dednat6.html#00:00"
     :date    "2021nov28"
     :length  "8:11"
     :subs    ".vtt"
     :index   t
     :comment "How I use test blocks (see above) in real life.")
    ;;
    ;; «eevnav»  (to ".eevnav")
    ;; Play: (find-eevnavvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eevnav")
    ("eevnav"
     :title "How to install eev with M-x list-packages and how to navigate its tutorials"
     :mp4   "http://anggtwu.net/eev-videos/2020-list-packages-eev-nav.mp4"
     :yt    "http://www.youtube.com/watch?v=kxBjiUo88_U"
     :page  "http://anggtwu.net/2020-list-packages-eev-nav.html"
     :hsubs "http://anggtwu.net/2020-list-packages-eev-nav.html#00:01"
     :date    "2020oct04"
     :length  "12:41"
     :subs    ".vtt"
     :index   t
     :comment "If you are learning Emacs start by this video!")
    ;;
    ;; «eevtempl»  (to ".eevtempl")
    ;; Play: (find-eevtemplvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eevtempl")
    ("eevtempl"
     :title "Some template-based functions of eev that are not five-minute hacks"
     :mp4   "http://anggtwu.net/eev-videos/2020-some-template-based.mp4"
     :yt    "http://www.youtube.com/watch?v=91-9YfRPsuk"
     :page  "http://anggtwu.net/2020-some-template-based.html"
     :hsubs "http://anggtwu.net/2020-some-template-based.html#00:00"
     :date    "2020dec24"
     :length  "33:11"
     :subs    ".vtt"
     :index   t
     :comment "This is a follow-up to my presentation at the EmacsConf2020.")
    ;;
    ;; «eevfherel»  (to ".eevfherel")
    ;; Play: (find-eevfherelvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eevfherel")
    ("eevfherel"
     :title "How to create hyperlinks to \"here\" with `find-here-links'"
     :mp4   "http://anggtwu.net/eev-videos/2020-find-here-links.mp4"
     :yt    "http://www.youtube.com/watch?v=8jtiBlaDor4"
     :page  "http://anggtwu.net/2020-find-here-links.html"
     :hsubs "http://anggtwu.net/2020-find-here-links.html#0:00"
     :date    "2020dec26"
     :length  "24:08"
     :subs    ".vtt"
     :index   t
     :comment "A badly-rehearsed tutorial on an advanced feature.")
    ;;
    ;; «eevtestbls»  (to ".eevtestbls")
    ;; Play: (find-eevtestblsvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eevtestbls")
    ("eevtestbls"
     :title "Using test blocks in eev (jan/2021)"
     :mp4   "http://anggtwu.net/eev-videos/2021-test-blocks.mp4"
     :yt    "http://www.youtube.com/watch?v=fpsF_M55W4o"
     :page  "http://anggtwu.net/2021-test-blocks.html"
     :date    "2021jan24"
     :length  "4:45"
     :index   t
     :comment "My first video on test blocks. Watch the video `eev2021' instead.")
    ;;
    ;; «eevvlinks»  (to ".eevvlinks")
    ;; Play: (find-eevvlinksvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eevvlinks")
    ("eevvlinks"
     :title "How to use the `[Video links:]' blocks in the `intro's of eev"
     :mp4   "http://anggtwu.net/eev-videos/2021-video-links.mp4"
     :yt    "http://www.youtube.com/watch?v=xQqWufQgzVY"
     :page  "http://anggtwu.net/2021-video-links.html"
     :date    "2021may10"
     :length  "14:56"
     :index   t
     :comment "A very good tutorial on how the `(find-*video)' links work.")
    ;;
    ;; «oficina20210»  (to ".oficina20210")
    ;; Play: (find-oficina20210video "0:00")
    ;; Index: (find-1stclassvideoindex "oficina20210")
    ("oficina20210"
     :title "Introdução ao Software Livre: Python, Shell, Lua, Emacs, eev"
     :mp4   "http://anggtwu.net/eev-videos/2021projeto-de-ensino.mp4"
     :yt    "http://www.youtube.com/watch?v=OW6WRnSQwc0"
     :page  "http://anggtwu.net/2021-oficina.html"
     :lang  "portuguese"
     :date    "2021sep05"
     :length  "1:27:01"
     :index   t
     :comment "A (long) non-technical introduction to Free Software, Emacs, and eev.")
    ;;
    ;; «oficina2021a»  (to ".oficina2021a")
    ;; Play: (find-oficina2021avideo "0:00")
    ;; Index: (find-1stclassvideoindex "oficina2021a")
    ("oficina2021a"
     :title "Como instalar o eev no Emacs"
     :mp4   "http://anggtwu.net/eev-videos/2021-oficina-1.mp4"
     :yt    "http://www.youtube.com/watch?v=acFPMuZ5Jf4"
     :page  "http://anggtwu.net/2021-oficina.html"
     :hsubs "http://anggtwu.net/2021-oficina-1.html#00:00"
     :lang  "portuguese"
     :subs   ".vtt"
     :date    "2021nov05"
     :length  "14:58"
     :index   t
     :comment "A very good introduction in Portuguese for beginners.")
    ;;
    ;; «oficina2021b»  (to ".oficina2021b")
    ;; Play: (find-oficina2021bvideo "0:00")
    ;; Index: (find-1stclassvideoindex "oficina2021b")
    ("oficina2021b"
     :title "Exercícios de criar e guardar links (1)"
     :mp4   "http://anggtwu.net/eev-videos/2021-oficina-2.mp4"
     :yt    "https://www.youtube.com/watch?v=XbuDnkfizYs"
     :page  "http://anggtwu.net/2021-oficina.html"
     :lang  "portuguese"
     :date    "2021nov09"
     :length  "14:10"
     :comment "A few very basic exercises for beginners. In portuguese.")
    ;;
    ;; «2021ssr»  (to ".2021ssr")
    ;; Play: (find-2021ssrvideo "0:00")
    ;; Index: (find-1stclassvideoindex "2021ssr")
    ("2021ssr"
     :title "Short videos about workflows - and how to upload them"
     :mp4  "http://anggtwu.net/eev-videos/2021-ssr.mp4"
     :yt   "http://www.youtube.com/watch?v=_0_NLXTVhBk"
     :page "http://anggtwu.net/2021-ssr.html"
     :date    "2021jan04"
     :length  "4:52"
     :comment "A bad video about a question that I sent to the Org mailing list.")
    ;;
    ;; «2021workshop1»  (to ".2021workshop1")
    ;; Play: (find-2021workshop1video "0:00")
    ;; Index: (find-1stclassvideoindex "2021workshop1")
    ("2021workshop1"
     :title "The base cases 1 and 2 (workshop 2021-dec-04)"
     :mp4   "http://anggtwu.net/eev-videos/2021-workshop-1.mp4"
     :yt    "https://www.youtube.com/watch?v=HuqJFPD871E"
     :page  "http://anggtwu.net/2021-workshop.html"
     :date    "2021dec01"
     :length  "2:26"
     :subs    ".vtt"
     :index   t
     :comment "An advanced video about (find-saving-links-intro).")
    ;;
    ;; «2021workshop2»  (to ".2021workshop2")
    ;; Play: (find-2021workshop2video "0:00")
    ;; Index: (find-1stclassvideoindex "2021workshop2")
    ("2021workshop2"
     :title "Creating a link to a file with a 2-window setting (workshop 2021-dec-04)"
     :mp4   "http://anggtwu.net/eev-videos/2021-workshop-2.mp4"
     :yt    "http://www.youtube.com/watch?v=hqqIlZBXNhk"
     :page  "http://anggtwu.net/2021-workshop.html"
     :date    "2021dec01"
     :length  "3:39"
     :subs    ".vtt"
     :comment "Another advanced video about (find-saving-links-intro).")
    ;;
    ;; «2021workshop3»  (to ".2021workshop3")
    ;; Play: (find-2021workshop3video "0:00")
    ;; Index: (find-1stclassvideoindex "2021workshop3")
    ("2021workshop3"
     :title "Material on `M-3 M-e' (workshop 2021-dec-04)"
     :mp4   "http://anggtwu.net/eev-videos/2021-workshop-3.mp4"
     :yt    "http://www.youtube.com/watch?v=r83inf9s8zo"
     :page  "http://anggtwu.net/2021-workshop.html"
     :date    "2021dec03"
     :length  "18:22"
     :subs    ".vtt"
     :index   t
     :comment "A video on \"saving links to everything interesting\".")
    ;;
    ;; «2021workshop4»  (to ".2021workshop4")
    ;; Play: (find-2021workshop4video "0:00")
    ;; Index: (find-1stclassvideoindex "2021workshop4")
    ("2021workshop4"
     :title "Invisible text (workshop 2021-dec-04)"
     :mp4  "http://anggtwu.net/eev-videos/2021-workshop-4.mp4"
     :yt   "http://www.youtube.com/watch?v=lhpHHjBUxv8"
     :page "http://anggtwu.net/2021-workshop.html"
     :date    "2021dec04"
     :length  "5:42"
     :subs    ".vtt"
     :comment "How to create links to sections of intros.")
    ;;
    ;; «2021workshop5»  (to ".2021workshop5")
    ;; Play: (find-2021workshop5video "0:00")
    ;; Index: (find-1stclassvideoindex "2021workshop5")
    ("2021workshop5"
     :title "Copy from left to right (workshop 2021-dec-04)"
     :mp4   "http://anggtwu.net/eev-videos/2021-workshop-5.mp4"
     :yt    "http://www.youtube.com/watch?v=VzRsterVSXs"
     :page  "http://anggtwu.net/2021-workshop.html"
     :date    "2021dec04"
     :length  "8:50"
     :subs    ".vtt"
     :comment "This video explains how to read some diagrams.")
    ;;
    ;; «2021workshop6»  (to ".2021workshop6")
    ;; Play: (find-2021workshop6video "0:00")
    ;; Index: (find-1stclassvideoindex "2021workshop6")
    ("2021workshop6"
     :title "`find-extra-file-links' (workshop 2021-dec-04)"
     :mp4   "http://anggtwu.net/eev-videos/2021-workshop-6.mp4"
     :yt    "http://www.youtube.com/watch?v=-gi15-liGaU"
     :page  "http://anggtwu.net/2021-workshop.html"
     :date    "2021dec04"
     :length  "10:08"
     :subs    ".vtt"
     :comment "Some exercises on how to use `M-h M-e'.")
    ;;
    ;; «2021orgfornonusers»  (to ".2021orgfornonusers")
    ;; Play: (find-2021orgfornonusersvideo "0:00")
    ;; Index: (find-1stclassvideoindex "2021orgfornonusers")
    ("2021orgfornonusers"
     :title "Org for Non-Users (2021)"
     :mp4   "http://anggtwu.net/eev-videos/2021-org-for-non-users.mp4"
     :yt    "http://www.youtube.com/watch?v=Eh5Wz9Vh_XM"
     :page  "http://anggtwu.net/2021-org-for-non-users.html"
     :hsubs "http://anggtwu.net/2021-org-for-non-users.html#00:00"
     :date    "2021dec11"
     :length  "16:36"
     :subs    ".vtt"
     :comment "On why Org and eev follow opposite principles.")
    ;;
    ;; «2021ffll»  (to ".2021ffll")
    ;; Play: (find-2021ffllvideo "0:00")
    ;; Index: (find-1stclassvideoindex "2021ffll")
    ("2021ffll"
     :title "How I write 5-minute hacks in eev using `M-x find-find-links-links-new'"
     :mp4   "http://anggtwu.net/eev-videos/2021-ffll.mp4"
     :yt    "http://www.youtube.com/watch?v=h1CEL2fmkyc"
     :page  "http://anggtwu.net/2021-ffll.html"
     :date    "2021dec25"
     :length  "1:15:46"
     :index   t
     :comment "A tutorial on a very advanced feature.")
    ;;
    ;; «2022eevmake0»  (to ".2022eevmake0")
    ;; Play: (find-2022eevmake0video "0:00")
    ;; Index: (find-1stclassvideoindex "2022eevmake0")
    ("2022eevmake0"
     :title "Using eev to test make(files)"
     :mp4   "http://anggtwu.net/eev-videos/2022-eev-make-0.mp4"
     :yt    "http://www.youtube.com/watch?v=Iql5C-yQk5c"
     :page  "http://anggtwu.net/eev-make.html"
     :hsubs "http://anggtwu.net/2022-eev-make-0.html#00:00"
     :date    "2022jan04"
     :length  "6:43"
     :subs    ".vtt"
     :comment "How to use test blocks in weird places. Unrehearsed.")
    ;;
    ;; «2022findeevangg»  (to ".2022findeevangg")
    ;; Play: (find-2022findeevanggvideo "0:00")
    ;; Index: (find-1stclassvideoindex "2022findeevangg")
    ("2022findeevangg"
     :title "Running executable notes from http://angg.twu.net/ with find-angg and find-wget"
     :mp4   "http://anggtwu.net/eev-videos/2022-find-eev-angg.mp4"
     :yt    "http://www.youtube.com/watch?v=FoAzpGzFCSE"
     :page  "http://anggtwu.net/eev-find-angg.html"
     :hsubs "http://anggtwu.net/2022-find-eev-angg.html#00:00"
     :date    "2022jan16"
     :length  "17:59"
     :subs    ".vtt"
     :index   t
     :comment "A very good video on a very useful trick.")
    ;;
    ;; «2022findelispintro»  (to ".2022findelispintro")
    ;; Play: (find-2022findelispintrovideo "0:00")
    ;; Index: (find-1stclassvideoindex "2022findelispintro")
    ("2022findelispintro"
     :title "Why eev has a weird elisp tutorial and how to use it"
     :mp4   "http://anggtwu.net/eev-videos/2022-find-elisp-intro.mp4"
     :yt    "http://www.youtube.com/watch?v=WowDSciGs1A"
     :page  "http://anggtwu.net/find-elisp-intro.html"
     :hsubs "http://anggtwu.net/2022-find-elisp-intro.html#00:00"
     :date    "2022jan23"
     :length  "21:12"
     :subs    ".vtt"
     :index   t
     :comment "A very good video on the design decisions behind `(find-elisp-intro)'.")
    ;;
    ;; «2022pict2elua»  (to ".2022pict2elua")
    ;; Play: (find-2022pict2eluavideo "0:00")
    ;; Index: (find-1stclassvideoindex "2022pict2elua")
    ("2022pict2elua"
     :title "Pict2e-lua: a library for diagrams that is being developed with eev and test blocks"
     :mp4   "http://anggtwu.net/eev-videos/2022-pict2e-lua.mp4"
     :yt    "http://www.youtube.com/watch?v=hiHsUhGVLGM"
     :page  "http://anggtwu.net/pict2e-lua.html"
     :hsubs "http://anggtwu.net/2022-pict2e-lua.html#00:01"
     :date    "2022apr18"
     :length  "8:13"
     :subs    ".vtt"
     :index   t
     :comment "A very good demo of test blocks.")
    ;;
    ;; «2022eevwconfigpt1»  (to ".2022eevwconfigpt1")
    ;; Play: (find-2022eevwconfigpt1video "0:00")
    ;; Index: (find-1stclassvideoindex "2022eevwconfigpt1")
    ("2022eevwconfigpt1"
     :title "Configuração sem mágica: um experimento com o eev (versão em Português)"
     :mp4   "http://anggtwu.net/eev-videos/2022-eev-wconfig-pt-1.mp4"
     :yt    "http://www.youtube.com/watch?v=bdLbocmo3r8"
     :page  "http://anggtwu.net/eev-wconfig.html"
     :lang  "portuguese"
     :date    "2022may02"
     :length  "29:20"
     :comment "A video in Portuguese about eev-wconfig.el.")
    ;;
    ;; «2022eevwconfigpt2»  (to ".2022eevwconfigpt2")
    ;; Play: (find-2022eevwconfigpt2video "0:00")
    ;; Index: (find-1stclassvideoindex "2022eevwconfigpt2")
    ("2022eevwconfigpt2"
     :title "Configuração sem mágica: um experimento com o eev (versão em Português, parte 2)"
     :mp4   "http://anggtwu.net/eev-videos/2022-eev-wconfig-pt-2.mp4"
     :yt    "http://www.youtube.com/watch?v=ZAAxrJX-Am8"
     :page  "http://anggtwu.net/eev-wconfig.html"
     :lang  "portuguese"
     :date    "2022may02"
     :length  "46:00"
     :comment "A video in Portuguese about eev-wconfig.el - second part.")
    ;;
    ;; «2022eevwconfig»  (to ".2022eevwconfig")
    ;; Play: (find-2022eevwconfigvideo "0:00")
    ;; Index: (find-1stclassvideoindex "2022eevwconfig")
    ("2022eevwconfig"
     :title "Eev-wconfig.el: a way without \"magic\" to configure eev on M$ Windows"
     :mp4   "http://anggtwu.net/eev-videos/2022-eev-wconfig.mp4"
     :yt    "http://www.youtube.com/watch?v=Rm29XSdGCXw"
     :page  "http://anggtwu.net/eev-wconfig.html"
     :hsubs "http://anggtwu.net/2022-eev-wconfig.html#00:01"
     :date    "2022may15"
     :length  "1:15:48"
     :subs    ".vtt"
     :index   t
     :comment "This is mainly for beginners who use Windows.")
    ;;
    ;; «2022yttranscript»  (to ".2022yttranscript")
    ;; Play: (find-2022yttranscriptvideo "0:00")
    ;; Index: (find-1stclassvideoindex "2022yttranscript")
    ("2022yttranscript"
     :title "find-yttranscript-links: downloading transcripts of Youtube videos, in eev style"
     :mp4   "http://anggtwu.net/eev-videos/2022-yttranscript.mp4"
     :yt    "http://www.youtube.com/watch?v=SW3Tx-lHX3o"
     :page  "http://anggtwu.net/find-yttranscript-links.html"
     :hsubs "http://anggtwu.net/2022-yttranscript.html#00:01"
     :date    "2022oct20"
     :length  "28:28"
     :subs    ".vtt"
     :index   t
     :comment "A video about `find-yttranscript-links'.")
    ;;
    ;; «2022tikz»  (to ".2022tikz")
    ;; Play: (find-2022tikzvideo "0:00")
    ;; Index: (find-1stclassvideoindex "2022tikz")
    ("2022tikz"
     :title "Eev and TikZ, or: how to learn TikZ using a REPL"
     :mp4   "http://anggtwu.net/eev-videos/2022-eev-tikz.mp4"
     :yt    "http://www.youtube.com/watch?v=d7nIzpXcV6c"
     :page  "http://anggtwu.net/eev-tikz.html"
     :hsubs "http://anggtwu.net/2022-eev-tikz.html#00:01"
     :date    "2022nov02"
     :length  "1:36:27"
     :subs    ".vtt"
     :index   t
     :comment "A way to learn TikZ using examples from the manual.")
    ;;
    ;; «eev2022kla»  (to ".eev2022kla")
    ;; Play: (find-eev2022klavideo "0:00")
    ;; Index: (find-1stclassvideoindex "eev2022kla")
    ("eev2022kla"
     :title "Bidirectional links with eev (@ EmacsConf 2022)"
     :mp4   "http://anggtwu.net/eev-videos/emacsconf2022-kla.mp4"
     :yt    "http://www.youtube.com/watch?v=KRobfwXd7Cw"
     :page  "http://anggtwu.net/emacsconf2022-kla.html"
     :hsubs "http://anggtwu.net/emacsconf2022-kla.html#00:00"
     :date    "2022dec03"
     :subs    ".vtt"
     :length  "7:57"
     :comment "A video about eev-kla.el.")
    ;;
    ;; «eev2022py»  (to ".eev2022py")
    ;; Play: (find-eev2022pyvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eev2022py")
    ("eev2022py"
     :title "Short hyperlinks to Python docs (eev @ EmacsConf2022)"
     :mp4   "http://anggtwu.net/eev-videos/emacsconf2022-py.mp4"
     :yt    "http://www.youtube.com/watch?v=QeqCYQSlz-I"
     :page  "http://anggtwu.net/emacsconf2022-py.html"
     :hsubs "http://anggtwu.net/emacsconf2022-py.html#00:00"
     :date    "2022dec04"
     :subs    ".vtt"
     :length  "14:03"
     :comment "A video about eev-rstdoc.el.")
    ;;
    ;; «eev2023repls»  (to ".eev2023repls")
    ;; Play: (find-eev2023replsvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eev2023repls")
    ("eev2023repls"
     :title "REPLs in strange places: Lua, LaTeX, LPeg, LPegRex, TikZ (@ EmacsConf 2023)"
     :mp4   "http://anggtwu.net/eev-videos/emacsconf2023-repls.mp4"
     :yt    "http://www.youtube.com/watch?v=IDBQo_YYfA8"
     :page  "http://anggtwu.net/emacsconf2023.html"
     :hsubs "http://anggtwu.net/emacsconf2023-repls.html#00:00"
     :date    "2023dec02"
     :subs    ".vtt"
     :length  "59:11"
     :comment "My presentation at the EmacsConf2023. See: (find-show2-intro)")
    ;;
    ;; «eev2023replsb»  (to ".eev2023replsb")
    ;; Play: (find-eev2023replsbvideo "0:00")
    ;; Index: (find-1stclassvideoindex "eev2023replsb")
    ("eev2023replsb"
     :title "REPLs in strange places: updates, a demo, and how to try it"
     :mp4   "http://anggtwu.net/eev-videos/emacsconf2023-repls-b.mp4"
     :yt    "http://www.youtube.com/watch?v=s3enXsuXyNg"
     :page  "http://anggtwu.net/emacsconf2023.html"
     :hsubs "http://anggtwu.net/emacsconf2023-repls-b.html#00:00"
     :date    "2023dec25"
     :subs    ".vtt"
     :length  "20:52"
     :comment "How to run the demos here: (find-show2-intro)")
    ;;
    ;; «2024git»  (to ".2024git")
    ;; Play: (find-2024gitvideo "0:00")
    ;; Index: (find-1stclassvideoindex "2024git")
    ("2024git"
     :title "Learning git with \"try it!\"s (and eev)"
     :mp4   "http://anggtwu.net/eev-videos/2024-eev-git.mp4"
     :yt    "http://www.youtube.com/watch?v=lsVvokjqMY0"
     :page  "http://anggtwu.net/2023-eev-git.html"
     :hsubs "http://anggtwu.net/2024-eev-git.html#00:00"
     :date    "2024jan06"
     :length  "33:25"
     :subs    ".vtt"
     :comment "A video about (find-git-intro). Very accessible.")
    ;;
    ;; «2024luaso»  (to ".2024luaso")
    ;; Play: (find-2024luasovideo "0:00")
    ;; Index: (find-1stclassvideoindex "2024luaso")
    ("2024luaso"
     :title "Two ways of creating \".so\"s for Lua, one very fast, both using Emacs and eev"
     :mp4   "http://anggtwu.net/eev-videos/2024-find-luaso-links.mp4"
     :yt    "http://www.youtube.com/watch?v=zUW-6atPvUQ"
     :page  "http://anggtwu.net/find-luaso-links.html"
     :hsubs "http://anggtwu.net/2024-find-luaso-links.html#00:00"
     :date    "2024jan16"
     :length  "14:16"
     :subs    ".vtt"
     :comment "See: (find-lua-tutorial-intro)")
    ))


;; «ee-1stclassvideos-field»  (to ".ee-1stclassvideos-field")
;;
;;                            (find-eppp ee-1stclassvideos-info)
;;                      (assoc "eev2021" ee-1stclassvideos-info)
;;                 (cdr (assoc "eev2021" ee-1stclassvideos-info))
;;      (plist-get (cdr (assoc "eev2021" ee-1stclassvideos-info)) :page)
;;      (plist-get (cdr (assoc "foo"     ee-1stclassvideos-info)) :page)
;; (ee-1stclassvideos-field    "eev2021" :page)
;; (ee-1stclassvideos-field    "eev2021" :mp4)
;; (ee-1stclassvideos-field    "eev2021" :yt)
;; (ee-1stclassvideos-mp4stem  "eev2021")
;; (ee-1stclassvideos-pagestem "eev2022py")
;; (ee-1stclassvideos-localmp4 "eev2021")
;; (ee-1stclassvideos-mp4found "eev2021")
;; (ee-1stclassvideos-hash     "eev2021")
;; (ee-1stclassvideos-hsubsurl "eev2021")
;; (ee-1stclassvideos-hsubsurl "eev2021" "01:45")
;; (find-eev "eev-videolinks.el" "more-info")
;;
(defun ee-1stclassvideos-field (c &optional field)
  (plist-get (cdr (assoc c ee-1stclassvideos-info)) field))

(defun ee-1stclassvideos-mp4stem (c)
  (let ((mp4 (ee-1stclassvideos-field c :mp4)))
     (replace-regexp-in-string "^.*/\\([^/]*\\)\\.mp4$" "\\1" mp4)))

(defun ee-1stclassvideos-pagestem (c)
  (let ((url (ee-1stclassvideos-field c :page)))
     (replace-regexp-in-string "^.*/\\([^/]*\\)\\.html$" "\\1" url)))

(defun ee-1stclassvideos-hash (c)
  (let ((yt (ee-1stclassvideos-field c :yt)))
     (replace-regexp-in-string "^.*=\\([^=]*\\)$" "\\1" yt)))

(defun ee-1stclassvideos-localmp4 (c)
  (ee-shorten-file-name
   (ee-url-to-fname
    (ee-1stclassvideos-field c :mp4))))

(defun ee-1stclassvideos-mp4found (c)
  (ee-psne-downloaded-p
   (ee-1stclassvideos-field c :mp4)))

(defun ee-1stclassvideos-hsubsurl (c &optional pos)
  (let ((url0 (ee-1stclassvideos-field c :hsubs)))
    (if (not pos)
	url0
      (let ((baseurl (replace-regexp-in-string "#.*$" "" url0)))
	(format "%s#%s" baseurl pos)))))



;;;                _                                   _     _            
;;;   ___ ___   __| | ___        ___  _____   ____   _(_) __| | ___  ___  
;;;  / __/ _ \ / _` |/ _ \_____ / _ \/ _ \ \ / /\ \ / / |/ _` |/ _ \/ _ \ 
;;; | (_| (_) | (_| |  __/_____|  __/  __/\ V /  \ V /| | (_| |  __/ (_) |
;;;  \___\___/ \__,_|\___|      \___|\___| \_/    \_/ |_|\__,_|\___|\___/ 
;;;                                                                       
;; «second-class-videos»  (to ".second-class-videos")
;; «code-eevvideo»  (to ".code-eevvideo")
;; I store lots of videos in:
;;
;;   http://anggtwu.net/eev-videos/
;;
;; besides the official video tutorials for eev. The functions to
;; access the official video tutorials are defined in the next
;; section, as real defuns with real docstrings, but for the videos
;; that are not so important - my "second-class videos" - I prefer to
;; use sexps like these to define the sexp hyperlinks to them:
;;
;;        (code-eevvideo      "2021daniel4" "2021-daniel-4")
;;        (code-eevlinksvideo "2021daniel4" "2021-daniel-4")
;;                       (find-2021daniel4video "0:00")
;;
;; To understand how they work, try:
;;
;;       (find-code-eevvideo      "2021daniel4" "2021-daniel-4")
;;       (find-code-eevlinksvideo "2021daniel4" "2021-daniel-4")
;;   (find-code-eev{mod}video "M" "2021daniel4" "2021-daniel-4" "HASH")
;;
;; They are implemented using the three functions with "{mod}"s in
;; their names, defined below.

;; Test: (find-code-eev{mod}video "M" "2021daniel4" "2021-daniel-4" "HASH")
;;
(defun find-code-eev{mod}video (&optional mod c stem hash &rest rest)
  (find-estring-elisp (apply 'ee-code-eev{mod}video mod c stem hash rest)))
(defun      code-eev{mod}video (mod c stem &optional hash)
  (eval (ee-read (ee-code-eev{mod}video mod c stem hash))))
(defun   ee-code-eev{mod}video (&optional mod c stem hash)
  (setq mod  (or mod "{mod}"))
  (setq c    (or c "{c}"))
  (setq stem (or stem "{stem}"))
  (setq hash (or hash "{hash}"))
  (ee-template0 "\
;; (find-code-eev{<}mod{>}video \"{mod}\" \"{c}\" \"{stem}\" \"{hash}\")
;;      (code-eev{<}mod{>}video \"{mod}\" \"{c}\" \"{stem}\" \"{hash}\")
;;                (find-{c}video \"0:00\")
;;
;; See: (find-video-links-intro \"7. `find-eev-video'\" \"`find-eevlocal-video'\")
;;      (find-video-links-intro \"7. `find-eev-video'\" \"`find-eevlinks-video'\")
;;
(defun find-{c}video (&optional time &rest comments)
  (interactive)
  (find-eev{mod}-video \"{stem}\" \"{hash}\" time))
"))

;; Tests:
;; (find-code-eevvideo      "2021daniel4" "2021-daniel-4" "HASH")
;; (find-code-eevlocalvideo "2021daniel4" "2021-daniel-4" "HASH")
;; (find-code-eevlinksvideo "2021daniel4" "2021-daniel-4" "HASH")
;;      (code-eevvideo      "2021daniel4" "2021-daniel-4" "HASH")
;;      (code-eevlocalvideo "2021daniel4" "2021-daniel-4" "HASH")
;;      (code-eevlinksvideo "2021daniel4" "2021-daniel-4" "HASH")
;;                     (find-2021daniel4video "0:00")
;;
(defun find-code-eevvideo (&optional c stem hash &rest rest)
  (find-estring-elisp (apply 'ee-code-eev{mod}video "local" c stem hash rest)))
(defun      code-eevvideo (c stem &optional hash)
  (eval (ee-read (ee-code-eev{mod}video "local" c stem hash))))

(defun find-code-eevlocalvideo (&optional c stem hash &rest rest)
  (find-estring-elisp (apply 'ee-code-eev{mod}video "local" c stem hash rest)))
(defun      code-eevlocalvideo (c stem &optional hash)
  (eval (ee-read (ee-code-eev{mod}video "local" c stem hash))))

(defun find-code-eevlinksvideo (&optional c stem hash &rest rest)
  (find-estring-elisp (apply 'ee-code-eev{mod}video "links" c stem hash rest)))
(defun      code-eevlinksvideo (c stem &optional hash)
  (eval (ee-read (ee-code-eev{mod}video "links" c stem hash))))



;; «code-youtubevideo»  (to ".code-youtubevideo")
;; This block implements a way to create short hyperlinks to videos
;; that exist only on youtube. We can regard `code-youtubevideo' as a
;; variant of `code-eevvideo' that doesn't receive an mp4stem.
;; TODO: write docs!
;;
;; See:  (find-eev "eev-audiovideo.el" "find-youtube-video")
;; Test: (find-code-youtubevideo "punch" "K6LmZ0A1s9U" "Punch and Judy")
;;              
(defun      code-youtubevideo (c hash &rest rest)
  (eval (ee-read      (ee-code-youtubevideo c hash))))
(defun find-code-youtubevideo (c hash &rest rest)
  (find-estring-elisp (ee-code-youtubevideo c hash)))
(defun   ee-code-youtubevideo (c hash)
  (ee-template0 "\
;; (find-code-youtubevideo \"{c}\" \"{hash}\")
;;      (code-youtubevideo \"{c}\" \"{hash}\")
;;
;; Tests: (find-youtubedl-links nil \"TITLE\" \"{hash}\" nil \"{c}\")
;;        (find-youtube-video \"{hash}\" \"0:00\")
;;        (find-{c}video \"0:00\")

(defun find-{c}video (&optional time &rest rest)
  (interactive)
  (find-youtube-video \"{hash}\" time))
"))



;;;      _       __                 
;;;   __| | ___ / _|_   _ _ __  ___ 
;;;  / _` |/ _ \ |_| | | | '_ \/ __|
;;; | (_| |  __/  _| |_| | | | \__ \
;;;  \__,_|\___|_|  \__,_|_| |_|___/
;;;                                 
;; «first-class-videos»  (to ".first-class-videos")
;; «video-tutorials»     (to ".video-tutorials")
;;
;; This section contains one `defun' for each of one of the
;; first-class videos. See:
;;
;;   (find-video-links-intro "9. First-class videos")
;;   (find-1stclassvideos)
;;
;; If we defined them with `code-eevvideo' they wouldn't have
;; docstrings and `find-efunction' wouldn't be able to find their
;; definitions. Try:
;;
;;   (find-efunctiondescr 'find-eev2019video)
;;   (find-efunction      'find-eev2019video)






;;;  ____  _                                    
;;; / ___|| |_ _ __ __ _ _ __   __ _  ___       
;;; \___ \| __| '__/ _` | '_ \ / _` |/ _ \      
;;;  ___) | |_| | | (_| | | | | (_| |  __/_ _ _ 
;;; |____/ \__|_|  \__,_|_| |_|\__, |\___(_|_|_)
;;;                            |___/            
;;
;; «strange-functions»  (to ".strange-functions")
;; The `(code-1stclassvideos)' below defines lots of "strange
;; functions" with names like `find-{c}video', `find-{c}hsubs', and
;; `find-{c}lsubs'. The concept of "strange function" is explained
;; here:
;;   (find-strange-functions-intro)

;; See: (find-eev "eev-tlinks.el" "code-1stclassvideos")
;; (find-code-1stclassvideos)
        (code-1stclassvideos)






(provide 'eev-videolinks)



;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
