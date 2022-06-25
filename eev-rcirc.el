;;; eev-rcirc.el -- rcirc-related elisp hyperlinks.  -*- lexical-binding: nil; -*-

;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
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
;; Version:    20211024
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-rcirc.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-rcirc.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-beginner.el.html>
;;                 <http://angg.twu.net/eev-intros/find-rcirc-intro.html>
;;                                                (find-rcirc-intro)

;; «.low-level»			(to "low-level")
;; «.find-rcirc-buffer»		(to "find-rcirc-buffer")
;; «.find-rcirc-buffer-2a»	(to "find-rcirc-buffer-2a")
;; «.find-rcirc-buffer-3a»	(to "find-rcirc-buffer-3a")
;; «.find-freenode»		(to "find-freenode")
;; «.find-libera»		(to "find-libera")
;; «.find-freenode-links»	(to "find-freenode-links")
;; «.find-libera-links»		(to "find-libera-links")

;;; Commentary:

;; This file - eev-rcirc.el - implements elisp hyperlinks for rcirc in
;; a way that lets several important actions be expressed as
;; one-liners.
;; 
;; 
;; 1. Convention on channels
;; =========================
;; The rcirc convention for naming buffers is that if we are
;; connected to the server irc.foobar.net then:
;; 
;;         *irc.foobar.net*     is the "server buffer" (with server messages),
;;   #emacs@irc.foobar.net      is the buffer for the channel #emacs,
;;    fsbot@irc.foobar.net      is the buffer for chatting with the user fsbot.
;; 
;; Note that in IRC we "/join" channels but we "/query" users. See:
;; 
;;   (find-node "(rcirc)rcirc commands" "/join #emacs")
;;   (find-node "(rcirc)rcirc commands" "/query fsbot")
;; 
;; The eev-rcirc convention treats the "server" and the "channel" as
;; separate parameters. The main conversion function is
;; `ee-rcirc-buffer':
;; 
;;   (ee-rcirc-buffer "irc.foobar.net" nil)
;;         -->       "*irc.foobar.net*"
;;   (ee-rcirc-buffer "irc.foobar.net" "#emacs")
;;         --> "#emacs@irc.foobar.net"
;;   (ee-rcirc-buffer "irc.foobar.net" "fsbot")
;;         -->  "fsbot@irc.foobar.net"
;; 
;; In IRC channel names can't have spaces, and neither can user names.
;; Some programming languages use the term "word" for a non-empty
;; string without spaces. The function `ee-split' accepts both strings
;; and lists:
;; 
;;   (ee-split   "#emacs   fsbot   #git   gitinfo")
;;       -->    ("#emacs" "fsbot" "#git" "gitinfo")
;;   (ee-split '("#emacs" "fsbot" "#git" "gitinfo"))
;;       -->    ("#emacs" "fsbot" "#git" "gitinfo")
;; 
;; If it receives a string it splits it into a list of words, and if
;; it receives a list it return it unchanged.
;; 
;; Eev-rcirc treats channels and users in a uniform way, as words, and
;; it accepts lists of channels and users to be given either as
;; strings or as lists. More precisely,
;; 
;;   1) all the functions defined here that have an argument called
;;      "channel" accept both a channel name and a user name,
;; 
;;   2) all the functions defined here that accept an argument called
;;      "channels" accept both a string and a list; they use
;;      `ee-split' internally to convert it to a list.
;;
;; and also:
;;
;;   3) some functions have arguments called "ichannels" and
;;      "achannels". "Ichannels" means "initial channels": the
;;      channels that we connect to when we connect to the server.
;;      "Achannels" means "always channels": the channels that we
;;      always reconnect. TODO: define precisely this "always".





;; Most of the _comments_ below are obsolete. The recommended way to
;; use this is now by running, for example,
;;
;;   (find-freenode-links "e" "#emacs")
;;
;; and then copying and pasting these lines to your .emacs:
;;
;;   (setq ee-freenode-ichannels "#eev #emacs")
;;   (setq ee-freenode-achannels "#eev #emacs")
;;   (defun e2 () (interactive) (find-freenode-2a "#emacs"))
;;   (defun e3 () (interactive) (find-freenode-3a "#emacs"))
;;
;; Then `M-x e3' creates the three-window setup described here,
;;
;;   (find-rcirc-intro "1. The server buffer and the channel buffers")
;;
;; that lets you follow all the steps of the connection. Once you're
;; sure that you're connected to the server you can use `M-x e2' to
;; create a two-window setup with just the current buffer and the
;; #emacs buffer.


;; Conventions on arguments:
;; CHANNELS can be a list ("#foo" "#bar") or a string like "#foo #bar";
;; If CHANNEL is a string starting with "#", it is a channel to /join;
;; if CHANNEL is a string not starting with "#", it is a nick to /query;
;; if CHANNEL is nil, that means to use the server buffer.




;;;                           _                    
;;;   ___  ___       _ __ ___(_)_ __ ___     __/\__
;;;  / _ \/ _ \_____| '__/ __| | '__/ __|____\    /
;;; |  __/  __/_____| | | (__| | | | (_|_____/_  _\
;;;  \___|\___|     |_|  \___|_|_|  \___|      \/  
;;;                                                
;; «low-level» (to ".low-level")
;; Low-level functions. These functions have very bad names and they
;; WILL BE TOTALLY REWRITTEN AT SOME POINT IN THE FUTURE.

(defun ee-rcirc-buffer (server &optional channel)
  "Return the name of an rcirc server buffer, or channel, or chat buffer."
  (if channel (format "%s@%s" channel server) (format "*%s*" server)))

(defun ee-rcirc-process (server)
  "If we are connected to SERVER then return its network connection process.
SERVER must be the name of an irc server, like \"irc.freenode.net\"."
  (get-buffer-process (ee-rcirc-buffer server)))

(defun ee-rcirc-connected (server)
  "Return non-nil if we are already connected to irc server SERVER.
SERVER must be the name of an irc server, like \"irc.freenode.net\"."
  (let ((buffer (get-buffer (ee-rcirc-buffer server))))
    (and buffer (rcirc-buffer-process buffer))))

(defun ee-rcirc-connect (server channels)
"If we are not already connected to the irc server SERVER then connect to it
and join the channels CHANNELS."
  (if (not (ee-rcirc-connected server))
      (rcirc-connect server nil nil nil nil (ee-split channels))))

(defun ee-rcirc-join-channels (server channels)
  "Join CHANNELS on server SERVER (which we must be already connected to)."
  (rcirc-join-channels (ee-rcirc-process server) (ee-split channels)))

(defun ee-rcirc-join-channel (server channel)
  "Join CHANNEL on server SERVER (which we must be already connected to).
CHANNEL can also be nil, meaning the server buffer, or a nick to /query."
  (switch-to-buffer (ee-rcirc-buffer server))
  (if channel
      (if (equal "#" (substring channel 0 1))
	  (rcirc-cmd-join channel)
	(rcirc-cmd-query channel))))

;; Written in 2021oct18. Experimental.
(defun ee-rcirc-connect-or-join (server &optional ichannels achannels channel)
  "Make sure that we are connected to SERVER, and join the specified channels.
When not connected to SERVER connect to it, taking the initial
list of channels from ICHANNELS. When we are already connected to
SERVER, join the channels in ACHANNELS. The name ACHANNELS
originally meant \"channels to always connect to\", but this was
changed years ago.\n
The argument CHANNEL is a hack: when CHANNEL is non nil it is
added to both ICHANNELS and ACHANNELS.\n
In some old versions of eev-rcirc.el SOME functions were able to
treat channel buffers and query buffers in a unified way...
\"joining\" the channel \"fsbot\" - note that \"fsbot\" doesn't
start with a \"#\" - would be the same as /query-ing fsbot. In
the current version this doesn't work."
  (if channel (setq ichannels (cons channel (ee-split ichannels))))
  (if channel (setq achannels (cons channel (ee-split ichannels))))
  (if (not (ee-rcirc-connected server))
      (rcirc-connect server nil nil nil nil (ee-split ichannels))
    (rcirc-join-channels (ee-rcirc-process server) (ee-split achannels))))





;;;   __ _           _                _                    
;;;  / _(_)_ __   __| |      _ __ ___(_)_ __ ___     __/\__
;;; | |_| | '_ \ / _` |_____| '__/ __| | '__/ __|____\    /
;;; |  _| | | | | (_| |_____| | | (__| | | | (_|_____/_  _\
;;; |_| |_|_| |_|\__,_|     |_|  \___|_|_|  \___|      \/  
;;;                                                        
;; «find-rcirc-buffer» (to ".find-rcirc-buffer")
;; Medium-level functions - two that only change the current buffer,
;; then two that create window setups.

(defun find-rcirc-buffer0
  (server &optional channel &rest pos-spec-list)
  "Switch to the buffer for CHANNEL on SERVER. Make no attempt to (re)connect."
  (apply 'find-ebuffer (ee-rcirc-buffer server channel) pos-spec-list))

;; Test: (find-rcirc-buffer ee-libera-server "#eev #emacs" nil "#eev")
(defun find-rcirc-buffer
  (server ichannels &optional achannels channel &rest pos-spec-list)
  "Switch to the buffer for CHANNEL on SERVER.
When not connected to SERVER connect to it, taking the initial
list of channels from ICHANNELS; always make sure that we are
connected to ACHANNELS and to CHANNEL.

If ACHANNELS is nil (not \"\") then use the list in ICHANNELS.
If CHANNEL is nil then switch to the server buffer."
  (ee-rcirc-connect       server (ee-split ichannels))
  (ee-rcirc-join-channels server (ee-split (or achannels ichannels)))
  (ee-rcirc-join-channel  server channel)
  (apply 'find-rcirc-buffer0 server channel pos-spec-list))



;; ;; «find-rcirc-buffer-2a» (to ".find-rcirc-buffer-2a")
;; 2021oct18: experimental.
;;
;; rcirc.el was heavily changed between the commit 0034067 (dated
;; 2021sep03) and the commit 608b2ec (dated 2021sep04) - see:
;;
;;   https://github.com/emacs-mirror/emacs/commits/master/lisp/net/rcirc.el
;;
;; and the changes made `find-rcirc-buffer-2a' and
;; `find-rcirc-buffer-3a' stop working. I am _trying_ to rwrite
;; `find-rcirc-buffer-2a' and `find-rcirc-buffer-3a' in a way that
;; make them work on both the old and the new rcircs, but some things
;; don't work on the rcircs yet.

(defun find-rcirc-buffer-2a
  (server ichannels &optional achannels channel &rest pos-spec-list)
  "This function is being rewritten. See the comments in the source code."
  (ee-rcirc-connect-or-join server ichannels achannels channel)
  (find-2a
   nil
   `(find-rcirc-buffer0 server channel ,@pos-spec-list)))

;; 2021oct18: experimental.
(defun find-rcirc-buffer-3a
  (server ichannels achannels channel &rest pos-spec-list)
  "This function is being rewritten. See the comments in the source code."
  (ee-rcirc-connect-or-join server ichannels achannels channel)
  (find-3a
   nil
   '(find-rcirc-buffer0 server nil)
   `(find-rcirc-buffer0 server channel ,@pos-spec-list)))



;; 2021oct18: commented out.
;;
;; ;; Test: (find-rcirc-buffer-2a "irc.freenode.net" "#eev" nil "#libreboot")
;; (defun find-rcirc-buffer-2a
;;   (server ichannels &optional achannels channel &rest pos-spec-list)
;;   "Connect to the irc server SERVER and create this window setup:
;;    _________ ________
;;   |         |        |
;;   | current |  irc   |
;;   | buffer  | buffer |
;;   |_________|________|
;; 
;; ICHANNELS is the list of initial channels (used when connecting
;; to the server for the first time). ACHANNELS is the list of
;; channels to always (re)connect to; if nil it defaults to
;; ICHANNELS. CHANNEL selects what to display in the window at
;; the right - nil means the server buffer, \"#foo\" means channel
;; \"#foo\", \"nick\" means query \"nick\"."
;;   (find-2a
;;    nil
;;    `(find-rcirc-buffer server ichannels achannels channel ,@pos-spec-list)))
;; 
;; ;; «find-rcirc-buffer-3a» (to ".find-rcirc-buffer-3a")
;; ;; Test: (find-rcirc-buffer-3a ee-libera-server "#eev" nil "#libreboot")
;; (defun find-rcirc-buffer-3a
;;   (server ichannels achannels channel &rest pos-spec-list)
;;   "Connect to the irc server SERVER and create this window setup:
;;    _________ _________
;;   |         |         |
;;   |         |   irc   |
;;   |         |  server |
;;   | current |_________|
;;   | buffer  |         |
;;   |         |   irc   |
;;   |         | channel |
;;   |_________|_________|
;; 
;; ICHANNELS is the list of initial channels (used when connecting
;; to the server for the first time). ACHANNELS is the list of
;; channels to always (re)connect to; if nil it defaults to
;; ICHANNELS. CHANNEL selects what to display in the lower right
;; window - \"#foo\" means channel \"#foo\", \"nick\" means query
;; \"nick\"."
;;   (find-3a
;;    nil
;;    '(find-rcirc-buffer server ichannels achannels)
;;    `(find-rcirc-buffer server ichannels achannels channel ,@pos-spec-list)))



;; (defun ee-irc-channel-around-point ()
;;   (ee-stuff-around-point "#A-Za-z0-9_"))
;; 
;; (defun ee-buffer-freenode-channel-name ()
;;   (replace-regexp-in-string
;;    "^\\(.*\\).irc\\.freenode\\.net" "\\1"
;;    (buffer-name)))





;;;  _____                              _      
;;; |  ___| __ ___  ___ _ __   ___   __| | ___ 
;;; | |_ | '__/ _ \/ _ \ '_ \ / _ \ / _` |/ _ \
;;; |  _|| | |  __/  __/ | | | (_) | (_| |  __/
;;; |_|  |_|  \___|\___|_| |_|\___/ \__,_|\___|
;;;                                            
;; «find-freenode» (to ".find-freenode")

(defvar ee-freenode-server "irc.freenode.net")

(defvar ee-freenode-ichannels "#eev #rcirc"
  "The list of initial channels to connect to at freenode.")

(defvar ee-freenode-achannels nil
  "The list of channels that `find-freenode' always reconnects to.
When this is nil act as if this was a copy of `ee-freenode-ichannels'.")

(defun find-freenode (&optional channel &rest pos-spec-list)
  "Connect to freenode and switch to the buffer for channel CHANNEL.
This is like `find-rcirc-buffer', but uses the variables
`ee-freenode-ichannels' and `ee-freenode-achannels'."
  (apply 'find-rcirc-buffer
	  ee-freenode-server
	  ee-freenode-ichannels
	  ee-freenode-achannels channel pos-spec-list))

(defun find-freenode-2a (channel)
  "Connect to freenode and create this window setup:
   _________ ________
  |         |        |
  | current |  irc   |
  | buffer  | buffer |
  |_________|________|

This is like `find-rcirc-buffer-2a' but uses
`ee-freenode-ichannels' and `ee-freenode-achannels'."
  (find-rcirc-buffer-2a ee-freenode-server
			ee-freenode-ichannels
			ee-freenode-achannels
			channel))

(defun find-freenode-3a (channel)
  "Connect to freenode and create this window setup:
   _________ _________
  |         |         |
  |         |   irc   |
  |         |  server |
  | current |_________|
  | buffer  |         |
  |         |   irc   |
  |         | channel |
  |_________|_________|

This is like `find-rcirc-buffer-3a' but uses
`ee-freenode-ichannels' and `ee-freenode-achannels'."
  (find-rcirc-buffer-3a ee-freenode-server
			ee-freenode-ichannels
			ee-freenode-achannels
			channel))

;; See: (find-efunction 'eepitch)
;;      (find-efunction 'eepitch-to-buffer)
(defun eepitch-freenode (channel)
  (interactive) (eepitch '(find-freenode channel)))




;;;  _     _ _                     ____ _           _   
;;; | |   (_) |__   ___ _ __ __ _ / ___| |__   __ _| |_ 
;;; | |   | | '_ \ / _ \ '__/ _` | |   | '_ \ / _` | __|
;;; | |___| | |_) |  __/ | | (_| | |___| | | | (_| | |_ 
;;; |_____|_|_.__/ \___|_|  \__,_|\____|_| |_|\__,_|\__|
;;;                                                     
;; «find-libera»  (to ".find-libera")
;; In may/2021 many important Free Software channels migrated from
;; Freenode to LiberaChat. These functions are similar to the ones
;; that start with `find-freenode', but they use LiberaChat instead.

(defvar ee-libera-server "irc.libera.chat")

(defvar ee-libera-ichannels "#eev #rcirc"
  "The list of initial channels to connect to at libera.")

(defvar ee-libera-achannels nil
  "The list of channels that `find-libera' always reconnects to.
When this is nil act as if this was a copy of `ee-libera-ichannels'.")

(defun find-libera (&optional channel &rest pos-spec-list)
  "Connect to libera and switch to the buffer for channel CHANNEL.
This is like `find-rcirc-buffer', but uses the variables
`ee-libera-ichannels' and `ee-libera-achannels'."
  (apply 'find-rcirc-buffer
	  ee-libera-server
	  ee-libera-ichannels
	  ee-libera-achannels channel pos-spec-list))

(defun find-libera-2a (channel)
  "Connect to libera and create this window setup:
   _________ ________
  |         |        |
  | current |  irc   |
  | buffer  | buffer |
  |_________|________|

This is like `find-rcirc-buffer-2a' but uses
`ee-libera-ichannels' and `ee-libera-achannels'."
  (find-rcirc-buffer-2a ee-libera-server
			ee-libera-ichannels
			ee-libera-achannels
			channel))

(defun find-libera-3a (channel)
  "Connect to libera and create this window setup:
   _________ _________
  |         |         |
  |         |   irc   |
  |         |  server |
  | current |_________|
  | buffer  |         |
  |         |   irc   |
  |         | channel |
  |_________|_________|

This is like `find-rcirc-buffer-3a' but uses
`ee-libera-ichannels' and `ee-libera-achannels'."
  (find-rcirc-buffer-3a ee-libera-server
			ee-libera-ichannels
			ee-libera-achannels
			channel))

;; See: (find-efunction 'eepitch)
;;      (find-efunction 'eepitch-to-buffer)
(defun eepitch-libera (channel)
  (interactive) (eepitch '(find-libera channel)))








;;;  _ _ _                          _ _       _        
;;; | (_) |__   ___ _ __ __ _      | (_)_ __ | | _____ 
;;; | | | '_ \ / _ \ '__/ _` |_____| | | '_ \| |/ / __|
;;; | | | |_) |  __/ | | (_| |_____| | | | | |   <\__ \
;;; |_|_|_.__/ \___|_|  \__,_|     |_|_|_| |_|_|\_\___/
;;;                                                    
;; «find-libera-links»  (to ".find-libera-links")

(defun find-libera-links (&optional c channels &rest pos-spec-list)
"Visit a temporary buffer containing code for connecting to a libera channel."
  (interactive)
  (setq c (or c "{c}"))
  (setq channels (or channels "{channels}"))
  (let ((channel (car (ee-split channels))))
    (apply 'find-elinks
     `((find-libera-links ,c ,channels)
       (find-libera-links "e" "#eev")
       (find-libera-links)
       ;; Convention: the first sexp always regenerates the buffer.
       ;; (find-efunction 'find-libera-links)
       ;; (find-efunction 'find-libera-2a)
       ""
       ,(ee-template0 "\
;; To copy this to your .emacs, use:
;; (ee-copy-rest 3 '(find-fline \"~/.emacs\"))



;; Use (setq rcirc-default-nick-name ...) to set your nickname.
;; The default is to use \"#eev\" as the list of \"initial channels\",
;; the same list as the list of channels to always reconnect to, and
;; `M-x e3' and `M-x e2' to create the window setups described here:
;;
;;   (find-rcirc-intro \"1. The example that I use in workshops\")
;;
(setq rcirc-default-nick \"hakuryo\")
(setq rcirc-default-nick \"{(user-login-name)}\")
(setq ee-libera-ichannels {(ee-pp0 ee-libera-ichannels)})
(setq ee-libera-achannels nil)
(defun {c}2 () (interactive) (find-libera-2a \"{channel}\"))
(defun {c}3 () (interactive) (find-libera-3a \"{channel}\"))
")
       )
     pos-spec-list)))

;; Tests: (find-libera-links)
;;        (find-libera-links "e" "#eev")
;;        (find-libera-links "e" "#eev #emacs")
;;   (find-rcirc-intro)

(provide 'eev-rcirc)




;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
