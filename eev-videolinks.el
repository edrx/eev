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
;; Version:    20211027
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
;;   «.ee-use-eevyoutube-video»	(to "ee-use-eevyoutube-video")
;;   «.ee-use-eevlocal-video»	(to "ee-use-eevlocal-video")
;; «.code-eevvideo»		(to "code-eevvideo")
;; «.first-class-videos»	(to "first-class-videos")
;; «.video-tutorials»		(to "video-tutorials")
;;   «.find-eev2019video»	(to "find-eev2019video")
;;   «.find-eev2020video»	(to "find-eev2020video")
;;   «.find-eevnavvideo»	(to "find-eevnavvideo")
;;   «.find-eevtemplvideo»	(to "find-eevtemplvideo")
;;   «.find-eevfherelvideo»	(to "find-eevfherelvideo")
;;   «.find-eevtestblsvideo»	(to "find-eevtestblsvideo")
;;   «.find-eevvlinksvideo»	(to "find-eevvlinksvideo")

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
      (find-eevlocal-links stem hash time))))

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
#            {youtubeurl}\"
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
;;        (ee-use-eevyoutube-video)
;;        (find-eevtestblsvideo "2:33")
;;
;; «ee-use-eevyoutube-video»  (to ".ee-use-eevyoutube-video")
(defun ee-use-eevyoutube-video ()
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

;; «ee-use-eevlocal-video»  (to ".ee-use-eevlocal-video")
(defun ee-use-eevlocal-video ()
  "Use the default behavior for `find-eev-video'.
With the default definition the links in the [Video links:]
blocks of the tutorials of eev will try to download local copies
of the videos and play them with mpv. Compare with
`ee-use-find-eevyoutube-video'."
  (interactive)
  (setq ee-find-eev-video-function 'find-eevlocal-video))












;;;                _                                   _     _            
;;;   ___ ___   __| | ___        ___  _____   ____   _(_) __| | ___  ___  
;;;  / __/ _ \ / _` |/ _ \_____ / _ \/ _ \ \ / /\ \ / / |/ _` |/ _ \/ _ \ 
;;; | (_| (_) | (_| |  __/_____|  __/  __/\ V /  \ V /| | (_| |  __/ (_) |
;;;  \___\___/ \__,_|\___|      \___|\___| \_/    \_/ |_|\__,_|\___|\___/ 
;;;                                                                       
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
(defun find-{c}video (time &rest comments)
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





(provide 'eev-videolinks)



;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
