;;; eev-audiovideo.el -- elisp hyperlinks to audio and video files.

;; Copyright (C) 2013-2019 Free Software Foundation, Inc.
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
;; Version:    2020oct27
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-audiovideo.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-audiovideo.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-audiovideo-intro.html>
;;                                                (find-audiovideo-intro)

;;; Commentary:

;; This file implements links to audio or video files _at certain time
;; offsets_. Here are two examples, in a long syntax:
;;
;;   (find-video "~/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4")
;;   (find-video "~/eev-videos/Punch_and_Judy_Mark_Poulton-K6LmZ0A1s9U.mp4" "1:17")
;;
;; There are also the usual shorter hyperlinks, like this,
;;
;;   (find-punchandjudyvideo "1:04" "right position")
;;
;; and a very compact syntax, used by `eev-avadj-mode', in which `M-p'
;; plays the default audio or video file at the first time offset that
;; can the parsed in the current line (the "time from BOL"). See:
;;
;;   (find-audiovideo-intro)
;;   (find-audiovideo-intro "1. Time offsets")
;;   (find-audiovideo-intro "4. Short hyperlinks to audio and video files")
;;   (find-audiovideo-intro "4.3. A demo")
;;   (find-audiovideo-intro "4.4. The default audio/video file")
;;   (find-audiovideo-intro "4.4. The default audio/video file" "`M-p'")
;;
;; NOTE: I am not aware of other packages - for emacs or not - that
;; implement links that play audio or video files at given time
;; offsets... but in 2020jan03 a guy called stardiviner sent this
;; proposal to the Org mailing list:
;;
;;   https://lists.gnu.org/archive/html/emacs-orgmode/2020-01/msg00007.html



;; Historical note:
;;
;; I wrote a first version of this in 2011 or 2012, and some time
;; later a friend - Rafael Duarte Pinheiro - helped me to create a way
;; to play my indexed audios in a browser. Here is an example:
;;
;;   http://angg.twu.net/audios/2011dec13-ict.html
;;
;; I used these indexed audios to show what was happening in the
;; banana meetings of the Banana Institute of Science and Technology
;; of the banana campus in which I work, that is part of a banana
;; university in a banana republic.
;;
;; My tools for indexed audios later became one third of my "tools for
;; activists", that are documented here:
;;
;;   http://angg.twu.net/ferramentas-para-ativistas.html#audios-introducao
;;
;; but everything there is in Portuguese.
;;
;; In 2014 I had a burn-out and stopped working on these tools for
;; activists.




;; «.ee-time-from-bol»	(to "ee-time-from-bol")
;; «.eev-avadj-mode»	(to "eev-avadj-mode")
;; «.find-mplayer»	(to "find-mplayer")
;; «.find-termplayer»	(to "find-termplayer")
;; «.find-mpv-video»	(to "find-mpv-video")
;; «.find-mpv-audio»	(to "find-mpv-audio")
;; «.aliases»		(to "aliases")




(require 'eev-code)
(require 'eev-brxxx)



;; (find-efunction 'ee-stuff-around-point)
;; (find-elnode "Regexp Search")
;; (find-elnode "Regexp Backslash" "shy group")
;;
(defun ee-time-around-point ()
  (let ((time (ee-no-properties (ee-stuff-around-point "0-9:"))))
    (if (not (equal time ""))
	time)))


;;;  _   _                       __                           _           _ 
;;; | |_(_)_ __ ___   ___       / _|_ __ ___  _ __ ___       | |__   ___ | |
;;; | __| | '_ ` _ \ / _ \_____| |_| '__/ _ \| '_ ` _ \ _____| '_ \ / _ \| |
;;; | |_| | | | | | |  __/_____|  _| | | (_) | | | | | |_____| |_) | (_) | |
;;;  \__|_|_| |_| |_|\___|     |_| |_|  \___/|_| |_| |_|     |_.__/ \___/|_|
;;;                                                                         
;; «ee-time-from-bol»  (to ".ee-time-from-bol")

(defvar ee-time-regexp
	"\\(?:\\([0-9]?[0-9]\\):\\)?\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)")

(defun ee-re-search-from (pos regexp &optional limit repeat)
  (save-excursion
    (if pos (goto-char pos))
    (if (re-search-forward regexp limit t repeat)
	(match-string-no-properties 0))))

;; Test: 98:76:54 3:21 (ee-time-from-bol)
;;
(defun ee-time-from-bol ()
  "Try this: 98:76:54 3:21 (ee-time-from-bol)"
  (ee-re-search-from (ee-bol) ee-time-regexp (ee-eol)))

;; Test: 98:76:54 3:21 (ee-time-from-bol-flash)
;;
(defun ee-time-from-bol-flash () (interactive)
  "Try this: 98:76:54 3:21 (ee-time-from-bol-flash)"
  (if (ee-time-from-bol)
      (eeflash+ (match-beginning 0) (match-end 0) ee-highlight-spec))
  (ee-time-from-bol))





;;;  _   _                          _     _  __ _   
;;; | |_(_)_ __ ___   ___       ___| |__ (_)/ _| |_ 
;;; | __| | '_ ` _ \ / _ \_____/ __| '_ \| | |_| __|
;;; | |_| | | | | | |  __/_____\__ \ | | | |  _| |_ 
;;;  \__|_|_| |_| |_|\___|     |___/_| |_|_|_|  \__|
;;;                                                 
;; Tests:
;; (ee-time-to-seconds "1:00:00")
;; (ee-seconds-to-time   5)
;; (ee-seconds-to-time 300)
;; (ee-time+ -20 "0:05")

(defun ee-time-to-seconds (time)
  (save-match-data
    (if (string-match ee-time-regexp time)
	(+ (* 3600 (string-to-number (or (match-string 1 time) "0")))
	   (* 60 (string-to-number (match-string 2 time)))
	   (string-to-number (match-string 3 time))))))

(defun ee-seconds-to-time (seconds)
  (if (> 3600 seconds)
      (format-seconds "%m:%02s" seconds)
    (format-seconds "%h:%02m:%02s" seconds)))

(defun ee-time+ (seconds time)
  (save-match-data
    (ee-seconds-to-time
     (max 0 (+ seconds (ee-time-to-seconds time))))))

(defun ee-time-from-bol-shift (seconds)
  (interactive "P")
  (save-excursion
    (let ((time (ee-time-from-bol)))
      (if time
	  (replace-match (ee-time+ (or seconds 1) time) t t)
	(error "No time (mm:ss or hh:mm:ss) in the current line")))))

(defun ee-time-from-bol-shift- (seconds)
  (interactive "P")
  (ee-time-from-bol-shift (- (or seconds 1))))



;;;  _           _                     _ _         __      _     _            
;;; | | __ _ ___| |_    __ _ _   _  __| (_) ___   / /_   _(_) __| | ___  ___  
;;; | |/ _` / __| __|  / _` | | | |/ _` | |/ _ \ / /\ \ / / |/ _` |/ _ \/ _ \ 
;;; | | (_| \__ \ |_  | (_| | |_| | (_| | | (_) / /  \ V /| | (_| |  __/ (_) |
;;; |_|\__,_|___/\__|  \__,_|\__,_|\__,_|_|\___/_/    \_/ |_|\__,_|\___|\___/ 
;;;                                                                           
;; See: (find-audiovideo-intro "4.4. The default audio/video file")

(defvar ee-audiovideo-last nil
  "See: (find-audiovideo-intro \"The current audio or video\")")

(defun ee-audiovideo-sexp (time)
  (list ee-audiovideo-last time))

(defun ee-time-from-bol-rerun (&optional arg)
  "Play the current audio or video starting at '(ee-time-from-bol)'.
With a prefix of 0 just display what would be done. See:
  (find-audiovideo-intro \"time-from-bol\")
  (find-audiovideo-intro \"ee-audiovideo-last\")"
  (interactive "P")
  (cond ((eq arg 0)
	 (message "-> %S" (ee-audiovideo-sexp (ee-time-from-bol-flash))))
	(t (let   ((sexp  (ee-audiovideo-sexp (ee-time-from-bol))))
	     (eval sexp)
	     (message "%S" sexp)))))



;;;                       _  _                           _      
;;;   __ ___   ____ _  __| |(_)      _ __ ___   ___   __| | ___ 
;;;  / _` \ \ / / _` |/ _` || |_____| '_ ` _ \ / _ \ / _` |/ _ \
;;; | (_| |\ V / (_| | (_| || |_____| | | | | | (_) | (_| |  __/
;;;  \__,_| \_/ \__,_|\__,_|/ |     |_| |_| |_|\___/ \__,_|\___|
;;;                       |__/                                  
;;
;; «eev-avadj-mode»  (to ".eev-avadj-mode")
;; See: (find-audiovideo-intro "2. `eev-avadj-mode'")
;;
(setq eev-avadj-mode-map (make-sparse-keymap))
(define-key eev-avadj-mode-map "\M--" 'ee-time-from-bol-shift-)
(define-key eev-avadj-mode-map "\M-=" 'ee-time-from-bol-shift)
(define-key eev-avadj-mode-map "\M-+" 'ee-time-from-bol-shift)
(define-key eev-avadj-mode-map "\M-p" 'ee-time-from-bol-rerun)

(define-minor-mode eev-avadj-mode
  "eev audio/video adjust mode: a mode for adjusting audio/video link lines.
See: (find-audiovideo-intro \"`eev-avadj-mode'\")"
  :init-value nil
  :global nil
  :lighter " eev-avadj")

;; (eev-avadj-mode 0)
;; (eev-avadj-mode 1)
;; 1:15 foo




;;;                  _                       
;;;  _ __ ___  _ __ | | __ _ _   _  ___ _ __ 
;;; | '_ ` _ \| '_ \| |/ _` | | | |/ _ \ '__|
;;; | | | | | | |_) | | (_| | |_| |  __/ |   
;;; |_| |_| |_| .__/|_|\__,_|\__, |\___|_|   
;;;           |_|            |___/           
;;
;; «find-mplayer»  (to ".find-mplayer")
;; Play video files with mplayer.
;; Note that:
;; 1. mplayer is obsolete - see https://en.wikipedia.org/wiki/Mpv_(media_player)
;; 2. this should be called `find-mplayer-video' to follow the conventions on
;;    hyphens in: (find-eevfile "eev-pdflike.el")
;; 3. this is very old code.
;;
(defun    find-mplayer (fname &optional pos &rest rest)
  "Open FNAME with mplayer, with a GUI (in fullscreen mode, for video files)."
  (interactive "sFile name: ")
  (find-bgprocess (ee-find-mplayer fname pos)))
(defvar     ee-mplayer-options '("-fs" "-osdlevel" "2" "-zoom"))
(defun ee-mplayer-video-options () ee-mplayer-options)
(defun ee-find-mplayer (fname &optional pos &rest rest)
  `("mplayer"
    ,fname
    ,@(if pos `("-ss" ,(ee-secs-to-mm:ss pos)))
    ,@(ee-mplayer-video-options)
    ))

(defun      code-mplayer (c fname)
  (eval (ee-read      (ee-code-mplayer c fname))))
(defun find-code-mplayer (c fname)
  (find-estring-elisp (ee-code-mplayer c fname)))
(defun   ee-code-mplayer (c fname)
  (ee-template0 "\
    ;; {(ee-S `(find-code-mplayer ,c ,fname))} 
    ;;
    (defun find-{c} (&optional time &rest rest)
      (interactive (list (ee-time-around-point)))
      (setq ee-audiovideo-last 'find-{c})
      (if (eq time t)
        \"Just setting the default video\"
        (find-mplayer {(ee-S fname)} time)))
  "))


;;;  _                            _                       
;;; | |_ ___ _ __ _ __ ___  _ __ | | __ _ _   _  ___ _ __ 
;;; | __/ _ \ '__| '_ ` _ \| '_ \| |/ _` | | | |/ _ \ '__|
;;; | ||  __/ |  | | | | | | |_) | | (_| | |_| |  __/ |   
;;;  \__\___|_|  |_| |_| |_| .__/|_|\__,_|\__, |\___|_|   
;;;                        |_|            |___/           
;;
;; «find-termplayer»  (to ".find-termplayer")
;; Play audio files with mplayer (in an xterm).
;; Note that:
;; 1. mplayer is obsolete - see https://en.wikipedia.org/wiki/Mpv_(media_player)
;; 2. this should be called `find-mplayer-audio' to follow the conventions on
;;    hyphens in: (find-eevfile "eev-pdflike.el")
;; 3. this is very old code.
;;
(defvar     ee-termplayer-term-options '("xterm" "-geometry" "+200+100" "-e"))
(defvar     ee-termplayer-options ())
(defun ee-mplayer-audio-options () ee-termplayer-options)
(defun ee-find-termplayer (fname &optional pos &rest rest)
  `(,@ee-termplayer-term-options
    "mplayer"
    ,fname
    ,@(if pos `("-ss" ,(ee-secs-to-mm:ss pos)))
    ,@(ee-mplayer-audio-options)
    ))
(defun    find-termplayer (fname &optional pos &rest rest)
  "Open FNAME with mplayer, without a GUI (in a terminal - for audio files)."
  (interactive "sFile name: ")
  (find-bgprocess (ee-find-termplayer fname pos)))

(defun      code-termplayer (c fname)
  (eval (ee-read      (ee-code-termplayer c fname))))
(defun find-code-termplayer (c fname)
  (find-estring-elisp (ee-code-termplayer c fname)))
(defun   ee-code-termplayer (c fname)
  (ee-template0 "\
    ;; {(ee-S `(find-code-termplayer ,c ,fname))} 
    ;;
    (defun find-{c} (&optional time &rest rest)
      (interactive (list (ee-time-around-point)))
      (setq ee-audiovideo-last 'find-{c})
      (if (eq time t)
        \"Just setting the default audio\"
        (find-termplayer {(ee-S fname)} time)))
  "))






;;;                                 _     _            
;;;  _ __ ___  _ ____   __   __   _(_) __| | ___  ___  
;;; | '_ ` _ \| '_ \ \ / /___\ \ / / |/ _` |/ _ \/ _ \ 
;;; | | | | | | |_) \ V /_____\ V /| | (_| |  __/ (_) |
;;; |_| |_| |_| .__/ \_/       \_/ |_|\__,_|\___|\___/ 
;;;           |_|                                      
;;
;; «find-mpv-video»  (to ".find-mpv-video")
;; Experimental! To use it run:
;; (defun find-video (&rest rest) (apply 'find-mpv-video rest))
;;
(defun    find-mpv-video (fname &optional pos &rest rest)
  "Open FNAME with mpv, with a GUI (in fullscreen mode, for video files)."
  (interactive "sFile name: ")
  (find-bgprocess (ee-find-mpv-video fname pos)))
(defvar     ee-mpv-video-options '("--fs" "--osd-level=2"))
(defun ee-find-mpv-video (fname &optional pos &rest rest)
  `("mpv"
    ,fname
    ;; ,@(if pos `("--start" ,(ee-secs-to-mm:ss pos)))
    ,@(if pos (list (format "--start=%s" (ee-secs-to-mm:ss pos))))
    ,@ee-mpv-video-options
    ))

(defun      code-mpv-video (c fname)
  (eval (ee-read      (ee-code-mpv-video c fname))))
(defun find-code-mpv-video (c fname)
  (find-estring-elisp (ee-code-mpv-video c fname)))
(defun   ee-code-mpv-video (c fname)
  (ee-template0 "\
    ;; {(ee-S `(find-code-mpv-video ,c ,fname))} 
    ;;
    (defun find-{c} (&optional time &rest rest)
      (interactive (list (ee-time-around-point)))
      (setq ee-audiovideo-last 'find-{c})
      (if (eq time t)
        \"Just setting the default video\"
        (find-mpv-video {(ee-S fname)} time)))
  "))



;;;                                            _ _       
;;;  _ __ ___  _ ____   __      __ _ _   _  __| (_) ___  
;;; | '_ ` _ \| '_ \ \ / /____ / _` | | | |/ _` | |/ _ \ 
;;; | | | | | | |_) \ V /_____| (_| | |_| | (_| | | (_) |
;;; |_| |_| |_| .__/ \_/       \__,_|\__,_|\__,_|_|\___/ 
;;;           |_|                                        
;;
;; «find-mpv-audio»  (to ".find-mpv-audio")
;; Play audio with mpv (running in an xterm).
;; This is immature code. My notes are here:
;;   (find-es "mplayer" "mpv-audio")
;;
(defvar ee-mpv-term-options '("xterm" "-geometry" "+200+100" "-e"))
(defvar ee-mpv-audio-options '("--vid=no"))
(defun ee-find-mpv-audio (fname &optional pos &rest rest)
  `(,@ee-mpv-term-options
    "mpv"
    ,fname
    ,@(if pos `("--start" ,(ee-secs-to-mm:ss pos)))
    ,@ee-mpv-audio-options
    ))
(defun    find-mpv-audio (fname &optional pos &rest rest)
  "Open FNAME with mpv, without a GUI (in a terminal - for audio files)."
  (interactive "sFile name: ")
  (find-bgprocess (ee-find-mpv-audio fname pos)))

(defun      code-mpv-audio (c fname)
  (eval (ee-read      (ee-code-mpv-audio c fname))))
(defun find-code-mpv-audio (c fname)
  (find-estring-elisp (ee-code-mpv-audio c fname)))
(defun   ee-code-mpv-audio (c fname)
  (ee-template0 "\
    ;; {(ee-S `(find-code-mpv-audio ,c ,fname))} 
    ;;
    (defun find-{c} (&optional time &rest rest)
      (interactive (list (ee-time-around-point)))
      (setq ee-audioaudio-last 'find-{c})
      (if (eq time t)
        \"Just setting the default audio\"
        (find-mpv-audio {(ee-S fname)} time)))
  "))




;;;        _ _                     
;;;   __ _| (_) __ _ ___  ___  ___ 
;;;  / _` | | |/ _` / __|/ _ \/ __|
;;; | (_| | | | (_| \__ \  __/\__ \
;;;  \__,_|_|_|\__,_|___/\___||___/
;;;                                
;; «aliases»  (to ".aliases")

(defalias      'find-video      'find-mpv-video)
(defalias      'code-video      'code-mpv-video)
(defalias 'find-code-video 'find-code-mpv-video)

(defalias      'find-audio      'find-mpv-audio)
(defalias      'code-audio      'code-mpv-audio)
(defalias 'find-code-audio 'find-code-mpv-audio)

;; (find-code-brfile 'find-audio :local 'braudiol :dired 'braudiod)
        (code-brfile 'find-audio :local 'braudiol :dired 'braudiod)
;; (find-code-brfile 'find-video :local 'brvideol :dired 'brvideod)
        (code-brfile 'find-video :local 'brvideol :dired 'brvideod)





;;;                _                            _ _       
;;;   ___ ___   __| | ___        __ _ _   _  __| (_) ___  
;;;  / __/ _ \ / _` |/ _ \_____ / _` | | | |/ _` | |/ _ \ 
;;; | (_| (_) | (_| |  __/_____| (_| | |_| | (_| | | (_) |
;;;  \___\___/ \__,_|\___|      \__,_|\__,_|\__,_|_|\___/ 






(provide 'eev-audiovideo)




;; Garbage?


;; Tests: (ee-time-to-youtube-time "2")
;;        (ee-time-to-youtube-time "23")
;;        (ee-time-to-youtube-time "1:23")
;;        (ee-time-to-youtube-time "1:23:43")
;;
(defun ee-time-to-youtube-time (str)
  "Convert strings like \"1:23\" to strings like \"#t=1m23s\".
Supports the input formats \"ss\", \"mm:ss\", and \"hh:mm:ss\".
If the input does not match any of these formats, return nil."
  (save-match-data
    (cond ((string-match "^\\([0-9]+\\)$" str)
	   (format "#t=%ss" (match-string 1 str)))
          ((string-match "^\\([0-9]+\\):\\([0-9][0-9]\\)$" str)
	   (format "#t=%sm%ss" (match-string 1 str) (match-string 2 str)))
          ((string-match "^\\([0-9]+\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)$" str)
	   (format "#t=%sh%sm%ss" (match-string 1 str) (match-string 2 str)
		   (match-string 2 str))))))
	  



;;;                                
;;;  _ __ ___  _ __ ___  _ ___ ___ 
;;; | '_ ` _ \| '_ ` _ \(_) __/ __|
;;; | | | | | | | | | | |_\__ \__ \
;;; |_| |_| |_|_| |_| |_(_)___/___/
;;;                                

;; Convert between a number of seconds (a number)
;; and a "minutes:seconds" thing (a string).
;; OBSOLETE.
;; TODO: convert all calls to `ee-secs-to-mm:ss' to `ee-seconds-to-time' and
;;               all calls to `ee-mm:ss-to-secs' to `ee-time-to-seconds'.
;;
(defun ee-secs-to-mm:ss (n)
  "Force N - a number of seconds or an \"mm:ss\" string - to the mm:ss format"
  (if (stringp n) n
    (let* ((s (mod n 60))
	   (m (/ (- n s) 60)))
      (format "%d:%02d" m s))))
(defun ee-mm:ss-to-secs (mm:ss)
  "Force MM:SS - a string or a number of seconds - to a number of seconds"
  (if (numberp mm:ss) mm:ss
    (let* ((ms (mapcar 'string-to-number (split-string mm:ss ":"))))
      (+ (* 60 (car ms)) (cadr ms)))))


;; Old notes on time regexps:
;; (find-elnode "Time Parsing")
;; (seconds-to-time 4000)
;; (float-time '(0 4000 0))
;; (format-seconds "%h:%m:%s" 4000)
;; (format-seconds "%h:%02m:%02s" 4000)
;; (ee-seconds-to-time 260)
;; (ee-seconds-to-time 4000)
;; (ee-time-to-seconds "4:20")
;; (date-to-time "2:30")
;; (string-to-number "")
;; (ee-time+ 40 "4:20")
;; (ee-time+ -1000 "4:20")
;;
;; (defvar ee-time-regexp "[0-9]?[0-9]:[0-9][0-9]\\(:[0-9][0-9]\\)?")





;; Local Variables:
;; coding:            utf-8-unix
;; ee-anchor-format:  "«%s»"
;; no-byte-compile:   t
;; End:
