;;; eev-channels.el -- control external processes usign signals,
;;; temporary files, and Expect scripts.

;; Copyright (C) 2012,2019 Free Software Foundation, Inc.
;;
;; This file is (not yet?) part of GNU eev.
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
;; Latest version: <http://angg.twu.net/eev-current/eev-channels.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-channels.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:

;; `eechannel' is very difficult to set up and has been mostly
;; superseded by `eepitch'. There is some documentation here,
;;
;;  (find-channels-intro)
;;
;; but it needs to be rewritten.


;; TODO: import code from:
;;   (find-eevgrep "grep -nH -e channel *.el")
;;   (find-eev "anim/channels.anim")
;;   (find-eev "eechannel.el")


(defun ee-read-file (fname)
  (with-temp-buffer
    (insert-file-contents fname)
    (buffer-string)))






;;;                 _                            _
;;;   ___  ___  ___| |__   __ _ _ __  _ __   ___| |
;;;  / _ \/ _ \/ __| '_ \ / _` | '_ \| '_ \ / _ \ |
;;; |  __/  __/ (__| | | | (_| | | | | | | |  __/ |
;;;  \___|\___|\___|_| |_|\__,_|_| |_|_| |_|\___|_|
;;;
;; (find-man "xterm" "-T string")
;; (find-man "xterm" "-e program [ arguments ... ]")
;; (find-eev "eegchannel")
;; (find-eev "eegchannel" "pidfile")
;; (find-eev "eegchannel" "strfile")
;;
;; There is a big diagram explaining how this works at:
;;
;;   (find-eev "anim/channels.anim")
;;
;; Note that this is a "communication diagram" - it shows which
;; programs start which other programs, and how they communicate.
;; Here is a call diagram for the lisp functions (and some
;; variables):
;;
;;   <F9> ---> eechannel-this-line
;;                    |          \ (on "" lines)
;;        (on non-"" |           v 
;;             lines) |            ee-eval-string 
;;                    v
;;             eechannel-send          
;;              |  |  |                
;;              |  |  v                  (sets)
;;              |  v eechannel-default  <------ eechannel
;;              v eechannel-strfile
;;       /---> eechannel-pid ----------> eechannel-pidfile
;;       |
;;      eechannel-kill

(defvar eechannel-default nil)

(defun eechannel-strfile (channel)
  (ee-expand (format "$EEVTMPDIR/eeg.%s.str" channel)))

(defun eechannel-pidfile (channel)
  (ee-expand (format "$EEVTMPDIR/eeg.%s.pid" channel)))

(defun eechannel-pid     (channel)
"Return the pid stored in the eeg.CHANNEL.pid file, as a string (or nil on error)."
  (let ((pidfile (eechannel-pidfile channel)))
    (if (file-exists-p pidfile)
	(ee-no-trailing-nl (ee-read-file pidfile)))))

(defun eechannel-kill (channel sig)
  "Send the signal SIG to the process listening on the channel CHANNEL."
  ;; We call "kill" to send the signal.
  (find-callprocess0 (format "kill %s %s" sig (eechannel-pid channel))))

(defun eechannel-send (channel str)
  "Send STR through channel CHANNEL (or through channel `eechannel-default')."
  (setq channel (or channel eechannel-default))
  (write-region str nil (eechannel-strfile channel))
  (find-callprocess0 (format "kill -USR1 %s" (eechannel-pid channel))))

(defun eechannel-this-line () (interactive)
  "Send the current line through the channel `eechannel-default', and go down.
If the line starts with a `' then evaluate it as lisp instead of sending it."
  (let ((line (buffer-substring (ee-bol) (ee-eol)))) ; contents of this line
    (if (string-match "^\\(.*\\)" line)             ; lines with a red star
	(ee-eval-string (match-string 1 line))       ; are eval'ed
      (eechannel-send nil (concat line "\n")))       ; other lines are sent
    (ee-next-line 1)))			             ; go down

(defun eechannel (channel)
  "Set the default channel to CHANNEL."
  (interactive "sDefault channel: ")
  (setq eechannel-default channel))




;;;                 _                                   _
;;;   ___  ___  ___| |__         __ _ ___ ___  ___ _ __| |_
;;;  / _ \/ _ \/ __| '_ \ _____ / _` / __/ __|/ _ \ '__| __|
;;; |  __/  __/ (__| | | |_____| (_| \__ \__ \  __/ |  | |_
;;;  \___|\___|\___|_| |_|      \__,_|___/___/\___|_|   \__|
;;;

(defun eechannel-pid-running-p (pid)
  "Return t if a process with pid PID is running. This is linux-specific."
  ;; I've heard the on BSDs "/proc" is optional and often disabled...
  ;; Calling "ps" every time sounds expensive, what should I do?
  (file-exists-p (format "/proc/%s" pid)))

;; The six functions below are for when we want to use eegchannel
;; directly, without calling it from an xterm (as in eexterm)...

(defun eechannel-args-ne (channel prog-and-args)
  `(,(ee-expand "$EEVDIR/eegchannel") ,channel
    ,@(ee-split prog-and-args)))

(defun eechannel-create-ne (channel prog-and-args)
  (find-bgprocess-ne (eechannel-args-ne channel prog-and-args)))

(defun eechannel-assert-ne (channel prog-and-args)
  (let ((pid (eechannel-pid channel)))
    (if (eechannel-pid-running-p (eechannel-pid channel))
	(message "Channel %s (pid %s) looks alive, reusing" channel pid)
      (eechannel-create-ne channel prog-and-args))))

(defun eechannel-args   (channel prog-and-args)
  (eechannel-args-ne   channel (ee-split-and-expand prog-and-args)))
(defun eechannel-create (channel prog-and-args)
  (eechannel-create-ne channel (ee-split-and-expand prog-and-args)))
(defun eechannel-assert (channel prog-and-args)
  (eechannel-assert-ne channel (ee-split-and-expand prog-and-args)))




;;;                 _
;;;   ___  _____  _| |_ ___ _ __ _ __ ___
;;;  / _ \/ _ \ \/ / __/ _ \ '__| '_ ` _ \
;;; |  __/  __/>  <| ||  __/ |  | | | | | |
;;;  \___|\___/_/\_\\__\___|_|  |_| |_| |_|
;;;
;; A call diagram:
;;
;;   eexterm ---------> eexterm-ne      
;;                        |    |       
;;                        |    v       
;;                        |  eechannel-pid-running-p
;;                        v             
;;   eexterm-create --> eexterm-create-ne    
;;                        |             
;;                        v             
;;   eexterm-args ----> eexterm-args-ne      
;;
;;   eexterm-kill -----> eechannel-kill

(defun eexterm-args-ne (channel prog-and-args xterm-args)
"Return a list of arguments for running a xterm listening on CHANNEL.
Try these examples:
  (eexterm-args-ne \"A\" nil nil)
  (eexterm-args-ne \"A\" '(\"ssh\" \"foo@bar\") \"-geometry 80x20\")"
  `("xterm"
    "-T" ,(format "channel %s" channel)
    ,@(ee-split xterm-args)
    "-e" ,(ee-expand "$EEVDIR/eegchannel") ,channel
    ,@(ee-split (or prog-and-args (ee-expand "$SHELL")))))

(defun eexterm-create-ne (channel prog-and-args xterm-args)
  "Start a xterm listening on CHANNEL. See `eexterm-args-ne'."
  (find-bgprocess-ne (eexterm-args-ne channel prog-and-args xterm-args)))

(defun eexterm-ne (channel prog-and-args xterm-args)
"Set the default channel to CHANNEL; create an xterm listening on CHANNEL if needed."
  (interactive "sDefault channel: ")
  (setq eechannel-default channel)
  (if (eechannel-pid-running-p (eechannel-pid channel))
      (message "Reusing xterm at channel %s" channel)
    (eexterm-create-ne channel prog-and-args xterm-args)))

(defun eexterm-args   (channel &optional prog-and-args xterm-args)
  (eexterm-args-ne   channel (ee-split-and-expand prog-and-args) xterm-args))

(defun eexterm-create (channel &optional prog-and-args xterm-args)
  "Create an xterm listening on CHANNEL."
  (eexterm-create-ne channel (ee-split-and-expand prog-and-args) xterm-args))

(defun eexterm        (channel &optional prog-and-args xterm-args)
"Set the default channel to CHANNEL; create an xterm listening on CHANNEL if needed."
  (interactive "sDefault channel: ")
  (eexterm-ne        channel (ee-split-and-expand prog-and-args) xterm-args))

(defalias 'eechannel-xterm 'eexterm)

(defun eexterm-kill (&optional channel sig)
  (interactive)
  (eechannel-kill (or channel eechannel-default) (or sig "")))



(provide 'eev-channels)



;; Local Variables:
;; coding:          raw-text-unix
;; no-byte-compile: t
;; End:
