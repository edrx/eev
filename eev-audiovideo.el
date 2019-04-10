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
;; Version:    2019mar02
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-audiovideo.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-audiovideo.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-audiovideo-intro.html>
;;                                                (find-audiovideo-intro)

;;; Commentary:

;; There is an introduction to these features here,
;;
;;   (find-audiovideo-intro)
;;
;; but that needs to be rewritten...



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
(defvar ee-time-regexp
	"\\(?:\\([0-9]?[0-9]\\):\\)?\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)")

(defun ee-re-search-from (pos regexp &optional limit repeat)
  (save-excursion
    (if pos (goto-char pos))
    (if (re-search-forward regexp limit t repeat)
	(match-string-no-properties 0))))

(defun ee-time-from-bol ()
  "Try this: 98:76:54 3:21 (ee-time-from-bol)"
  (ee-re-search-from (ee-bol) ee-time-regexp (ee-eol)))

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
;;
(defvar ee-audiovideo-last nil
  "See: (find-audiovideo-intro \"The current audio or video\")")

(defun ee-audiovideo-sexp (time)
  (list ee-audiovideo-last time))


;;;                                  _  _                           _      
;;;   __ _     __   __      __ _  __| |(_)      _ __ ___   ___   __| | ___ 
;;;  / _` |____\ \ / /____ / _` |/ _` || |_____| '_ ` _ \ / _ \ / _` |/ _ \
;;; | (_| |_____\ V /_____| (_| | (_| || |_____| | | | | | (_) | (_| |  __/
;;;  \__,_|      \_/       \__,_|\__,_|/ |     |_| |_| |_|\___/ \__,_|\___|
;;;                                  |__/                                  
;;
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

;; (find-eev "eev-mode.el")
;; (find-code-video "thecompanyofwolves" "/sda5/torrents/The_Company_of_Wolves/The_Company_Of_Wolves.avi")
;;      (code-video "thecompanyofwolves" "/sda5/torrents/The_Company_of_Wolves/The_Company_Of_Wolves.avi")

;; 0:00 (ee-time-from-bol-shift -100)
;; 0:00 (ee-time-from-bol-shift -10)
;; 1:23 (if (ee-time-from-bol) (replace-match "abcd" t t)) 
;; 1:23 (if (ee-time-from-bol) (save-excursion (replace-match "abcd" t t)))

;; Ideally `M-1 M-x find-chomskyvideo' should use `ee-time-from-bol'...
;; (find-elnode "Index" "* replace-match:")





;;;                _                 _     _            
;;;   ___ ___   __| | ___     __   _(_) __| | ___  ___  
;;;  / __/ _ \ / _` |/ _ \____\ \ / / |/ _` |/ _ \/ _ \ 
;;; | (_| (_) | (_| |  __/_____\ V /| | (_| |  __/ (_) |
;;;  \___\___/ \__,_|\___|      \_/ |_|\__,_|\___|\___/ 
;;;                                                     

;; mplayer for video files
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

(defalias      'find-video      'find-mplayer)
(defalias      'code-video      'code-mplayer)
(defalias 'find-code-video 'find-code-mplayer)

;; (find-code-brfile 'find-video :local 'brvideol :dired 'brvideod)
        (code-brfile 'find-video :local 'brvideol :dired 'brvideod)





;;;                _                            _ _       
;;;   ___ ___   __| | ___        __ _ _   _  __| (_) ___  
;;;  / __/ _ \ / _` |/ _ \_____ / _` | | | |/ _` | |/ _ \ 
;;; | (_| (_) | (_| |  __/_____| (_| | |_| | (_| | | (_) |
;;;  \___\___/ \__,_|\___|      \__,_|\__,_|\__,_|_|\___/ 

;; mplayer in an xterm, for audio files
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

(defalias      'find-audio      'find-termplayer)
(defalias      'code-audio      'code-termplayer)
(defalias 'find-code-audio 'find-code-termplayer)

;; (find-code-brfile 'find-audio :local 'braudiol :dired 'braudiod)
        (code-brfile 'find-audio :local 'braudiol :dired 'braudiod)




(provide 'eev-audiovideo)




;; Garbage?

;;;                                
;;;  _ __ ___  _ __ ___  _ ___ ___ 
;;; | '_ ` _ \| '_ ` _ \(_) __/ __|
;;; | | | | | | | | | | |_\__ \__ \
;;; |_| |_| |_|_| |_| |_(_)___/___/
;;;                                

;; Convert between a number of seconds (a number)
;; and a "minutes:seconds" thing (a string)
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


;;;  _   _                                                     
;;; | |_(_)_ __ ___   ___       _ __ ___  __ _  _____  ___ __  
;;; | __| | '_ ` _ \ / _ \_____| '__/ _ \/ _` |/ _ \ \/ / '_ \ 
;;; | |_| | | | | | |  __/_____| | |  __/ (_| |  __/>  <| |_) |
;;;  \__|_|_| |_| |_|\___|     |_|  \___|\__, |\___/_/\_\ .__/ 
;;;                                      |___/          |_|    
;;
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
