;;; eev-videolinks.el --- support for [Video links:] blocks.  -*- lexical-binding: nil; -*-

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
;; Version:    20211203
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-videolinks.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-videolinks.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-links-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-links-intro)

;; (load "eev-videolinks.el")

;; «.find-eev-video»		(to "find-eev-video")
;; «.find-eevlocal-links»	(to "find-eevlocal-links")
;; «.select»			(to "select")
;;   «.ee-use-youtube-videos»	(to "ee-use-youtube-videos")
;;   «.ee-use-local-videos»	(to "ee-use-local-videos")
;; «.first-class-videos»	(to "first-class-videos")
;;  «.video-tutorials»		(to "video-tutorials")
;;   «.find-eev2019video»	(to "find-eev2019video")
;;   «.find-eev2020video»	(to "find-eev2020video")
;;   «.find-eev2021video»	(to "find-eev2021video")
;;   «.find-eevnavvideo»	(to "find-eevnavvideo")
;;   «.find-eevtemplvideo»	(to "find-eevtemplvideo")
;;   «.find-eevfherelvideo»	(to "find-eevfherelvideo")
;;   «.find-eevtestblsvideo»	(to "find-eevtestblsvideo")
;;   «.find-eevvlinksvideo»	(to "find-eevvlinksvideo")
;;   «.find-2021workshop1video»	(to "find-2021workshop1video")
;;   «.find-2021workshop2video»	(to "find-2021workshop2video")
;;   «.find-2021workshop3video»	(to "find-2021workshop3video")
;;   «.find-2021workshop4video»	(to "find-2021workshop4video")
;;   «.find-2021workshop5video»	(to "find-2021workshop5video")
;;   «.find-2021workshop6video»	(to "find-2021workshop6video")
;; «.more-info»			(to "more-info")
;; «.ee-1stclassvideos-info»	(to "ee-1stclassvideos-info")
;;   «.eev2019»			(to "eev2019")
;;   «.eev2020»			(to "eev2020")
;;   «.eev2021»			(to "eev2021")
;;   «.eevnav»			(to "eevnav")
;;   «.eevtempl»		(to "eevtempl")
;;   «.eevfherel»		(to "eevfherel")
;;   «.eevtestbls»		(to "eevtestbls")
;;   «.eevvlinks»		(to "eevvlinks")
;;   «.oficina2021a»		(to "oficina2021a")
;;   «.oficina2021b»		(to "oficina2021b")
;;   «.2021workshop1»		(to "2021workshop1")
;;   «.2021workshop2»		(to "2021workshop2")
;;   «.2021workshop3»		(to "2021workshop3")
;;   «.2021workshop4»		(to "2021workshop4")
;;   «.2021workshop5»		(to "2021workshop5")
;;   «.2021workshop6»		(to "2021workshop6")
;; «.ee-1stclassvideos-field»	(to "ee-1stclassvideos-field")
;; «.second-class-videos»	(to "second-class-videos")
;;   «.code-eevvideo»		(to "code-eevvideo")

;;; Commentary:

;; In may/2021 I implemented the "[Video links:]" blocks in the
;; tutorials of eev, and I explained everything here:
;;
;;   http://angg.twu.net/2021-video-links.html
;;
;; but the innards of that implementation were ugly, and I've changed
;; them several times since then.
;;
;; This file - from nov/2021 - reimplements those innards in the way
;; described here:
;;
;;   (find-video-links-intro)
;;   (find-video-links-intro "7. `find-eev-video'")
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
  (let* ((url (format "http://angg.twu.net/eev-videos/%s.mp4" mp4stem))
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

(defun ee-eevlocal-body (stem hash time)
  "An internal function used by `find-eevlocal-links'."
  (let ((url (format "http://angg.twu.net/eev-videos/%s.mp4" stem)))
    (concat (ee-eevlocal-youtube-comment hash time)
	    "\n" (ee-psne-url-comment url)
	    "\n\n" (ee-eevlocal-psne stem time)
	    )))

;; Tests:
;; (find-estring (ee-eevlocal-youtube-comment "0123456789a" nil))
;; (find-estring (ee-eevlocal-youtube-comment "0123456789a" ""))
;; (find-estring (ee-eevlocal-youtube-comment "0123456789a" "1:23"))
;;
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
;; (find-estring (ee-eevlocal-psne "emacsconf1234" nil))
;; (find-estring (ee-eevlocal-psne "emacsconf2020" nil))
;; (find-estring (ee-eevlocal-psne "emacsconf2020" "1:23"))
;; (find-estring (ee-eevlocal-findmpvvideo "emacsconf2020" nil))
;; (find-estring (ee-eevlocal-findmpvvideo "emacsconf2020" "{foo}"))
;; (find-estring (ee-eevlocal-findmpvvideo "emacsconf2020" "1:23"))
;;
(defun ee-eevlocal-psne (stem time)
  "An internal function used by `find-eevlocal-links'."
  (let* ((url (format "http://angg.twu.net/eev-videos/%s.mp4" stem)))
    (concat (ee-psne-if-needed url)
	    (if (not (ee-psne-downloaded-p url))
		(concat "\n" (ee-eevlocal-findmpvvideo stem time))))))

(defun ee-eevlocal-findmpvvideo (stem time)
  "An internal function used by `find-eevlocal-links'."
  (let* ((url     (format "http://angg.twu.net/eev-videos/%s.mp4" stem))
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







;;;        _     _                  _         _             _       _     
;;; __   _(_) __| | ___  ___       | |_ _   _| |_ ___  _ __(_) __ _| |___ 
;;; \ \ / / |/ _` |/ _ \/ _ \ _____| __| | | | __/ _ \| '__| |/ _` | / __|
;;;  \ V /| | (_| |  __/ (_) |_____| |_| |_| | || (_) | |  | | (_| | \__ \
;;;   \_/ |_|\__,_|\___|\___/       \__|\__,_|\__\___/|_|  |_|\__,_|_|___/
;;;                                                                       
;; «first-class-videos»  (to ".first-class-videos")
;; «video-tutorials»  (to ".video-tutorials")
;; The functions defined in this section correspond
;; to the video tutorials listed in:
;;
;;   (find-videos-intro "1. Some videos")
;;
;; If we defined them with `code-eevvideo' they wouldn't have
;; docstrings and `find-efunction' wouldn't be able to find their
;; definitions.

;; (find-1stclassvideo-links "eev2019")
;; (find-1stclassvideo-links "eev2020")
;; (find-1stclassvideo-links "eevnav")
;; (find-1stclassvideo-links "eevtempl")
;; (find-1stclassvideo-links "eevfherel")
;; (find-1stclassvideo-links "eevtestbls")
;; (find-1stclassvideo-links "eevvlinks")

;; «find-eev2019video»  (to ".find-eev2019video")
;; Skel: (find-eevshortvideo-links "eev2019" "emacsconf2019" "86yiRG8YJD0")
;;  See: (find-videos-intro "1. Some videos" "emacsconf2019")
;; Index: http://angg.twu.net/.emacs.videos.html#eev2019
;;  Test: (find-eev2019video "0:00")
(defun find-eev2019video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"emacsconf2019\")
     http://angg.twu.net/emacsconf2019.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "emacsconf2019" "86yiRG8YJD0" time))

;; «find-eev2020video»  (to ".find-eev2020video")
;; Skel: (find-eevshortvideo-links "eev2020" "emacsconf2020" "hOAqBc42Gg8")
;;  See: (find-videos-intro "1. Some videos" "emacsconf2020")
;; Index: http://angg.twu.net/.emacs.videos.html#eev2020
;;  Test: (find-eev2020video "0:00")
(defun find-eev2020video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"emacsconf2020\")
     http://angg.twu.net/emacsconf2020.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "emacsconf2020" "hOAqBc42Gg8" time))

;; «find-eev2021video»  (to ".find-eev2021video")
;; Skel: (find-eevshortvideo-links "eev2021" "emacsconf2021" "{youtubeid}")
;;  See: (find-videos-intro "1. Some videos" "emacsconf2021")
;; Index: http://angg.twu.net/.emacs.videos.html#eev2021
;;  Test: (find-eev2021video "0:00")
(defun find-eev2021video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"emacsconf2021\")
     http://angg.twu.net/emacsconf2021.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "emacsconf2021" "{youtubeid}" time))

;; «find-eevnavvideo»  (to ".find-eevnavvideo")
;; Skel: (find-eevshortvideo-links "eevnav" "2020-list-packages-eev-nav" "kxBjiUo88_U")
;;  See: (find-videos-intro "1. Some videos" "2020-list-packages-eev-nav")
;; Index: http://angg.twu.net/.emacs.videos.html#eevnav
;;  Test: (find-eevnavvideo "0:00")
(defun find-eevnavvideo (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"2020-list-packages-eev-nav\")
     http://angg.twu.net/2020-list-packages-eev-nav.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2020-list-packages-eev-nav" "kxBjiUo88_U" time))

;; «find-eevtemplvideo»  (to ".find-eevtemplvideo")
;; Skel: (find-eevshortvideo-links "eevtempl" "2020-some-template-based" "91-9YfRPsuk")
;;  See: (find-videos-intro "1. Some videos" "2020-some-template-based")
;; Index: http://angg.twu.net/.emacs.videos.html#eevtempl
;;  Test: (find-eevtemplvideo "0:00")
(defun find-eevtemplvideo (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"2020-some-template-based\")
     http://angg.twu.net/2020-some-template-based.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2020-some-template-based" "91-9YfRPsuk" time))

;; «find-eevfherelvideo»  (to ".find-eevfherelvideo")
;; Skel: (find-eevshortvideo-links "eevfherel" "2020-find-here-links" "8jtiBlaDor4")
;;  See: (find-videos-intro "1. Some videos" "2020-find-here-links")
;; Index: http://angg.twu.net/.emacs.videos.html#eevfherel
;;  Test: (find-eevfherelvideo "0:00")
(defun find-eevfherelvideo (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"2020-find-here-links\")
     http://angg.twu.net/2020-find-here-links.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2020-find-here-links" "8jtiBlaDor4" time))

;; «find-eevtestblsvideo»  (to ".find-eevtestblsvideo")
;; Skel: (find-eevshortvideo-links "eevtestbls" "2021-test-blocks" "fpsF_M55W4o")
;;  See: (find-videos-intro "1. Some videos" "2021-test-blocks")
;; Index: http://angg.twu.net/.emacs.videos.html#eevtestbls
;;  Test: (find-eevtestblsvideo "0:00")
(defun find-eevtestblsvideo (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"2021-test-blocks\")
     http://angg.twu.net/2021-test-blocks.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2021-test-blocks" "fpsF_M55W4o" time))

;; «find-eevvlinksvideo»  (to ".find-eevvlinksvideo")
;; Skel: (find-eevshortvideo-links "eevvlinks" "2021-video-links" "xQqWufQgzVY")
;;  See: (find-videos-intro "1. Some videos" "2021-video-links")
;; Index: http://angg.twu.net/.emacs.videos.html#eevvlinks
;;  Test: (find-eevvlinksvideo "0:00")
(defun find-eevvlinksvideo (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"2021-video-links\")
     http://angg.twu.net/2021-video-links.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2021-video-links" "xQqWufQgzVY" time))

;; «find-2021workshop1video»  (to ".find-2021workshop1video")
;; Skel: (find-eevshortvideo-links "2021workshop1" "2021-workshop-1" "xQqWufQgzVY")
;;  See: (find-videos-intro "1. Some videos" "2021-workshop-1")
;; Index: http://angg.twu.net/.emacs.videos.html#2021workshop1
;;  Test: (find-2021workshop1video "0:00")
(defun find-2021workshop1video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"2021-workshop-1\")
     http://angg.twu.net/2021-workshop-1.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2021-workshop-1" "HuqJFPD871E" time))

;; «find-2021workshop2video»  (to ".find-2021workshop2video")
;; Skel: (find-eevshortvideo-links "2021workshop2" "2021-workshop-2" "xQqWufQgzVY")
;;  See: (find-videos-intro "1. Some videos" "2021-workshop-2")
;; Index: http://angg.twu.net/.emacs.videos.html#2021workshop2
;;  Test: (find-2021workshop2video "0:00")
(defun find-2021workshop2video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-videos-intro \"1. Some videos\" \"2021-workshop-2\")
     http://angg.twu.net/2021-workshop-2.html
     for more info on this particular video,
and: (find-video-links-intro \"7. `find-eev-video'\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2021-workshop-2" "hqqIlZBXNhk" time))

;; «find-2021workshop3video»  (to ".find-2021workshop3video")
;; Skel: (find-1stclassvideo-links "2021workshop3")
;; Tests: (find-2021workshop3video "0:00")
;;        (find-efunctiondescr 'find-2021workshop3video)
(defun find-2021workshop3video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-eev \"eev-videolinks.el\" \"2021workshop3\")
     http://angg.twu.net/eev-current/eev-videolinks.el.html#eev2020
     for more info on this particular video,
and: (find-video-links-intro \"7. \" \"find-eev-video\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2021-workshop-3" "r83inf9s8zo" time))

;; «find-2021workshop4video»  (to ".find-2021workshop4video")
;; Skel: (find-1stclassvideo-links "2021workshop4")
;; Tests: (find-2021workshop4video "0:00")
;;        (find-efunctiondescr 'find-2021workshop4video)
(defun find-2021workshop4video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-eev \"eev-videolinks.el\" \"2021workshop4\")
     http://angg.twu.net/eev-current/eev-videolinks.el.html#eev2020
     for more info on this particular video,
and: (find-video-links-intro \"7. \" \"find-eev-video\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2021-workshop-4" "lhpHHjBUxv8" time))

;; «find-2021workshop5video»  (to ".find-2021workshop5video")
;; Skel: (find-1stclassvideo-links "2021workshop5")
;; Tests: (find-2021workshop5video "0:00")
;;        (find-efunctiondescr 'find-2021workshop5video)
(defun find-2021workshop5video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-eev \"eev-videolinks.el\" \"2021workshop5\")
     http://angg.twu.net/eev-current/eev-videolinks.el.html#eev2020
     for more info on this particular video,
and: (find-video-links-intro \"7. \" \"find-eev-video\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2021-workshop-5" "VzRsterVSXs" time))

;; «find-2021workshop6video»  (to ".find-2021workshop6video")
;; Skel: (find-1stclassvideo-links "2021workshop6")
;; Tests: (find-2021workshop6video "0:00")
;;        (find-efunctiondescr 'find-2021workshop6video)
(defun find-2021workshop6video (&optional time &rest rest)
  "Play one of the video tutorials of eev starting at TIME.
See: (find-eev \"eev-videolinks.el\" \"2021workshop6\")
     http://angg.twu.net/eev-current/eev-videolinks.el.html#eev2020
     for more info on this particular video,
and: (find-video-links-intro \"7. \" \"find-eev-video\")
 or: http://angg.twu.net/eev-intros/find-video-links-intro.html#7
     for more info on these video tutorials."
  (interactive)
  (find-eev-video "2021-workshop-6" "-gi15-liGaU" time))





;;;  __  __                  _        __       
;;; |  \/  | ___  _ __ ___  (_)_ __  / _| ___  
;;; | |\/| |/ _ \| '__/ _ \ | | '_ \| |_ / _ \ 
;;; | |  | | (_) | | |  __/ | | | | |  _| (_) |
;;; |_|  |_|\___/|_|  \___| |_|_| |_|_|  \___/ 
;;;                                            
;; «more-info»  (to ".more-info")
;; «ee-1stclassvideos-info»  (to ".ee-1stclassvideos-info")
;; More info on the first-class videos, in a format that is easy to
;; access from Lisp. I am just starting to play with this, and the
;; functions that transform the data in this variable into other
;; formats don't exist yet. EVERYTHING HERE WILL PROBABLY CHANGE.

(defvar ee-1stclassvideos-info
  '(;;
    ;; «eev2019»  (to ".eev2019")
    ("eev2019"
     :title "How to record executable notes with eev - and how to play them back"
     :mp4  "http://angg.twu.net/eev-videos/emacsconf2019.mp4"
     :yt   "http://www.youtube.com/watch?v=86yiRG8YJD0"
     :page "http://angg.twu.net/emacsconf2019.html")
    ;;
    ;; «eev2020»  (to ".eev2020")
    ("eev2020"
     :title "On why most of the best features in eev look like 5-minute hacks"
     :mp4  "http://angg.twu.net/eev-videos/emacsconf2020.mp4"
     :yt   "http://www.youtube.com/watch?v=hOAqBc42Gg8"
     :page "http://angg.twu.net/emacsconf2020.html")
    ;;
    ;; «eev2021»  (to ".eev2021")
    ("eev2021"
     :title "Test blocks"
     :mp4  "http://angg.twu.net/eev-videos/emacsconf2021.mp4"
     :yt   "http://www.youtube.com/watch?v=qM0Luz78qGw"
     :page "http://angg.twu.net/emacsconf2021.html")
    ;;
    ;; «eev2021b»  (to ".eev2021b")
    ("eev2021b"
     :title "Test blocks in Dednat6"
     :mp4  "http://angg.twu.net/eev-videos/emacsconf2021-dednat6.mp4"
     :yt   "http://www.youtube.com/watch?v=QUMo7vgkHJI"
     :page "http://angg.twu.net/emacsconf2021.html")
    ;;
    ;; «eevnav»  (to ".eevnav")
    ("eevnav"
     :title "How to install eev with M-x list-packages and how to navigate its tutorials"
     :mp4   "http://angg.twu.net/eev-videos/2020-list-packages-eev-nav.mp4"
     :yt    "http://www.youtube.com/watch?v=kxBjiUo88_U"
     :page  "http://angg.twu.net/2020-list-packages-eev-nav.html")
    ;;
    ;; «eevtempl»  (to ".eevtempl")
    ("eevtempl"
     :title "Some template-based functions of eev that are not five-minute hacks"
     :mp4   "http://angg.twu.net/eev-videos/2020-some-template-based.mp4"
     :yt    "http://www.youtube.com/watch?v=91-9YfRPsuk"
     :page  "http://angg.twu.net/2020-some-template-based.html")
    ;;
    ;; «eevfherel»  (to ".eevfherel")
    ("eevfherel"
     :title "How to create hyperlinks to \"here\" with `find-here-links'"
     :mp4   "http://angg.twu.net/eev-videos/2020-find-here-links.mp4"
     :yt    "http://www.youtube.com/watch?v=8jtiBlaDor4"
     :page  "http://angg.twu.net/2020-find-here-links.html")
    ;;
    ;; «eevtestbls»  (to ".eevtestbls")
    ("eevtestbls"
     :title "Using test blocks in eev (jan/2021)"
     :mp4   "http://angg.twu.net/eev-videos/2021-test-blocks.mp4"
     :yt    "http://www.youtube.com/watch?v=fpsF_M55W4o"
     :page  "http://angg.twu.net/2021-test-blocks.html")
    ;;
    ;; :title "Short videos about workflows - and how to upload them"
    ;; :page  "http://angg.twu.net/2021-ssr.html"
    ;; ^ bad & obsolete
    ;;
    ;; «eevvlinks»  (to ".eevvlinks")
    ("eevvlinks"
     :title "How to use the `[Video links:]' blocks in the `intro's of eev"
     :mp4   "http://angg.twu.net/eev-videos/2021-video-links.mp4"
     :yt    "http://www.youtube.com/watch?v=xQqWufQgzVY"
     :page  "http://angg.twu.net/2021-video-links.html")
    ;;
    ;; «oficina2021a»  (to ".oficina2021a")
    ("oficina2021a"
     :title "Como instalar o eev no Emacs"
     :mp4   "http://angg.twu.net/eev-videos/2021-oficina-1.mp4"
     :yt    "http://www.youtube.com/watch?v=acFPMuZ5Jf4"
     :page  "http://angg.twu.net/2021-oficina.html"
     :lang  "portuguese")
    ;;
    ;; «oficina2021b»  (to ".oficina2021b")
    ("oficina2021b"
     :title "Exercícios de criar e guardar links (1)"
     :mp4   "http://angg.twu.net/eev-videos/2021-oficina-2.mp4"
     :yt    "https://www.youtube.com/watch?v=XbuDnkfizYs"
     :page  "http://angg.twu.net/2021-oficina.html"
     :lang  "portuguese")
    ;;
    ;; «2021workshop1»  (to ".2021workshop1")
    ("2021workshop1"
     :title "The base cases 1 and 2 (workshop 2021-dec-04)"
     :mp4   "http://angg.twu.net/eev-videos/2021-workshop-1.mp4"
     :yt    "https://www.youtube.com/watch?v=HuqJFPD871E"
     :page  "http://angg.twu.net/2021-workshop.html")
    ;;
    ;; «2021workshop2»  (to ".2021workshop2")
    ("2021workshop2"
     :title "Creating a link to a file with a 2-window setting (workshop 2021-dec-04)"
     :mp4  "http://angg.twu.net/eev-videos/2021-workshop-2.mp4"
     :yt   "http://www.youtube.com/watch?v=hqqIlZBXNhk"
     :page "http://angg.twu.net/emacsconf2021.html")
    ;;
    ;; «2021workshop3»  (to ".2021workshop3")
    ("2021workshop3"
     :title "Material on `M-3 M-e' (workshop 2021-dec-04)"
     :mp4  "http://angg.twu.net/eev-videos/2021-workshop-3.mp4"
     :yt   "http://www.youtube.com/watch?v=r83inf9s8zo"
     :page "http://angg.twu.net/emacsconf2021.html")
    ;;
    ;; «2021workshop4»  (to ".2021workshop4")
    ("2021workshop4"
     :title "Invisible text (workshop 2021-dec-04)"
     :mp4  "http://angg.twu.net/eev-videos/2021-workshop-4.mp4"
     :yt   "http://www.youtube.com/watch?v=lhpHHjBUxv8"
     :page "http://angg.twu.net/emacsconf2021.html")
    ;;
    ;; «2021workshop5»  (to ".2021workshop5")
    ("2021workshop5"
     :title "Copy from left to right (workshop 2021-dec-04)"
     :mp4  "http://angg.twu.net/eev-videos/2021-workshop-5.mp4"
     :yt   "http://www.youtube.com/watch?v=VzRsterVSXs"
     :page "http://angg.twu.net/emacsconf2021.html")
    ;;
    ;; «2021workshop6»  (to ".2021workshop6")
    ("2021workshop6"
     :title "`find-extra-file-links' (workshop 2021-dec-04)"
     :mp4  "http://angg.twu.net/eev-videos/2021-workshop-6.mp4"
     :yt   "http://www.youtube.com/watch?v=-gi15-liGaU"
     :page "http://angg.twu.net/emacsconf2021.html")
    ;;
    ))


;; «ee-1stclassvideos-field»  (to ".ee-1stclassvideos-field")
;;
;;                           (find-eppp ee-1stclassvideos-info)
;;                     (assoc "eev2021" ee-1stclassvideos-info)
;;                (cdr (assoc "eev2021" ee-1stclassvideos-info))
;;     (plist-get (cdr (assoc "eev2021" ee-1stclassvideos-info)) :page)
;;     (plist-get (cdr (assoc "foo"     ee-1stclassvideos-info)) :page)
;; (ee-1stclassvideos-field   "eev2021" :page)
;; (ee-1stclassvideos-field   "eev2021" :mp4)
;; (ee-1stclassvideos-field   "eev2021" :yt)
;; (ee-1stclassvideos-mp4stem "eev2021")
;; (ee-1stclassvideos-hash    "eev2021")
;; (find-eev "eev-videolinks.el" "more-info")
;;
(defun ee-1stclassvideos-field (c &optional field)
  (plist-get (cdr (assoc c ee-1stclassvideos-info)) field))

(defun ee-1stclassvideos-mp4stem (c)
  (let ((mp4 (ee-1stclassvideos-field c :mp4)))
     (replace-regexp-in-string "^.*/\\([^/]*\\)\\.mp4$" "\\1" mp4)))

(defun ee-1stclassvideos-hash (c)
  (let ((yt (ee-1stclassvideos-field c :yt)))
     (replace-regexp-in-string "^.*=\\([^=]*\\)$" "\\1" yt)))






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
;;   http://angg.twu.net/eev-videos/
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








(provide 'eev-videolinks)



;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
