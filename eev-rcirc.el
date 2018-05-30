;;; eev-rcirc.el -- rcirc-related elisp hyperlinks.

;; Copyright (C) 2012,2013,2018 Free Software Foundation, Inc.
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
;; Version:    2018may28
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-rcirc.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-rcirc.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-rcirc-intro.html>
;;                                                (find-rcirc-intro)

;;; Commentary:

;; Here we implement a very thin eev-ish layer on top of rcirc. The
;; main goal is:
;;
;;   instant gratification and irc-ing for late-night zombie-mode
;;   half-brain-dead hackers
;;
;; which, in practical terms, means the following. Suppose that we
;; want to ask something on the channel #foobar of Freenode; we run
;; `M-x find-freenode-links', adjust the intended channel name to
;; #foobar, and set `M-99j' and `M-9j' to the right hyperlinks. Then,
;; if we are in a buffer called "stuff", `M-99j' would create this
;; window setting:
;;
;;     _____________________
;;    |          |          |
;;    |          | freenode |
;;    |          |  server  |
;;    |  stuff   |  buffer  |
;;    |          |__________|
;;    |          |          |
;;    |          | #foobar  |
;;    |__________|__________|
;;
;; which is great for following in real-time the connection being
;; established - I find this essential for when it is very late at
;; night and I am half-brain-dead -, and `M-9j' just switches to the
;; buffer of the channel #foobar.

;; Conventions on arguments:
;; CHANNELS can be a list ("#foo" "#bar") or a string like "#foo #bar";
;; If CHANNEL is a string starting with "#", it is a channel to /join;
;; if CHANNEL is a string not starting with "#", it is a nick to /query;
;; if CHANNEL is nil, that means to use the server buffer.


;; Building blocks:
;;
(defun ee-rcirc-buffer (server &optional channel)
  "Return the name of an rcirc server (or channel, or chat) buffer."
  (if channel (format "%s@%s" channel server) (format "*%s*" server)))

(defun ee-rcirc-process (server)
  (get-buffer-process (ee-rcirc-buffer server)))

(defun ee-rcirc-connected (server)
  "Return non-nil if we are already connected to SERVER."
  (and (get-buffer           (ee-rcirc-buffer server))
       (rcirc-buffer-process (ee-rcirc-buffer server))))

(defun ee-rcirc-connect (server channels)
"Connect to the irc server SERVER, if not already connected, and join CHANNELS."
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


;; Medium-level words without window setup.
;; Tests:
;; (find-rcirc-buffer "irc.freenode.net" "#eev #emacs")
;; (find-rcirc-buffer "irc.freenode.net" "#eev #emacs" nil           "#eev")
;; (find-rcirc-buffer "irc.freenode.net" "#eev #emacs" "#eev #emacs")
;; (find-rcirc-buffer "irc.freenode.net" "#eev #emacs" "#eev #emacs" "#eev")
;;
(defun find-rcirc-buffer0
  (server &optional channel &rest pos-spec-list)
  "Switch to the buffer for CHANNEL on SERVER. Make no attempt to (re)connect."
  (apply 'find-ebuffer (ee-rcirc-buffer server channel) pos-spec-list))

(defun find-rcirc-buffer
  (server ichannels &optional achannels channel &rest pos-spec-list)
  "Switch to the buffer for CHANNEL on SERVER.
When not connected connect to SERVER, taking the initial list of
channels from ICHANNELS; always make sure that we are connected
to ACHANNELS and to CHANNEL, and switch to the buffer for
CHANNEL.

If ACHANNELS is nil (not \"\") then use the list in ICHANNELS.
If CHANNEL is nil then switch to the server buffer."
  (ee-rcirc-connect       server (ee-split ichannels))
  (ee-rcirc-join-channels server (ee-split (or achannels ichannels)))
  (ee-rcirc-join-channel  server channel)
  (apply 'find-rcirc-buffer0 server channel pos-spec-list))




;; Medium-level words with window setup
;; Tests:
;; (find-rcirc-buffer-2a "irc.freenode.net" "#eev" nil "#libreboot")
;; (find-rcirc-buffer-3a "irc.freenode.net" "#eev" nil "#libreboot")
;;
(defun find-rcirc-buffer-2a
  (server ichannels &optional achannels channel &rest pos-spec-list)
  "Connect to the irc server SERVER and create this window setup:
   _________ ________
  |         |        |
  | current |  irc   |
  | buffer  | buffer |
  |_________|________|

ICHANNELS is the list of initial channels (used when connecting
to the server for the first time). ACHANNELS is the list of
channels to always (re)connect to; if nil it defaults to
ICHANNELS. CHANNEL selects what to display in the irc buffer at
the right - nil means the server buffer, \"#foo\" means channel
\"#foo\", \"nick\" means query \"nick\"."
  (find-2a
   nil
   `(find-rcirc-buffer server ichannels achannels channel ,@pos-spec-list)))

(defun find-rcirc-buffer-3a
  (server ichannels achannels channel &rest pos-spec-list)
  "Connect to the irc server SERVER and create this window setup:
   _________ _________
  |         |         |
  |         |   irc   |
  |         |  server |
  | current |_________|
  | buffer  |         |
  |         |   irc   |
  |         | channel |
  |_________|_________|

ICHANNELS is the list of initial channels (used when connecting
to the server for the first time). ACHANNELS is the list of
channels to always (re)connect to; if nil it defaults to
ICHANNELS. CHANNEL selects what to display in the irc channel at
the right - \"#foo\" means channel \"#foo\", \"nick\" means query
\"nick\"."
  (find-3a
   nil
   '(find-rcirc-buffer server ichannels achannels)
   `(find-rcirc-buffer server ichannels achannels channel ,@pos-spec-list)))








;; (find-find-links-links "{k}" "freenode" "channel")
;;
(defun ee-irc-channel-around-point ()
  (ee-stuff-around-point "#A-Za-z0-9_"))

;; High-level
;;
(defvar ee-freenode-server "irc.freenode.net")
(defvar ee-freenode-ichannels "#eev #rcirc")
(defvar ee-freenode-achannels "#eev #rcirc")

(defun find-freenode (&optional channel &rest pos-spec-list)
  (apply 'find-rcirc-buffer
	  ee-freenode-server
	  ee-freenode-ichannels 
	  ee-freenode-achannels channel pos-spec-list))

(defun find-freenode-3a (channel)
  (find-3a nil '(find-freenode) '(find-freenode channel)))

(defun ee-find-freenode-links (&optional channel)
  (setq channel (or channel (replace-regexp-in-string
			     "^\\(.*\\).irc\\.freenode\\.net" "\\1"
			     (buffer-name))))
  `((setq ee-freenode-ichannels ,ee-freenode-ichannels)
    (setq ee-freenode-achannels ,ee-freenode-achannels)
    ""
    (find-freenode ,channel)
    (find-freenode-3a ,channel)
    (defun eejump-9 () (find-freenode ,channel))
    (defun eejump-99 () (find-freenode-3a ,channel))
    ))

(defun find-freenode-links (&optional channel &rest pos-spec-list)
"Visit a temporary buffer containing hyperlinks for connecting to freenode."
  (interactive (list (ee-irc-channel-around-point)))
  (setq channel (or channel "{channel}"))
  (apply 'find-elinks
   `((find-freenode-links ,channel)
     ;; Convention: the first sexp always regenerates the buffer.
     (find-efunction 'find-freenode-links)
     ""
     ,@(ee-find-freenode-links channel)
     )
   pos-spec-list))

;; Tests: (find-freenode-links)
;;        (find-freenode-links "#eev")

(provide 'eev-rcirc)




;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
