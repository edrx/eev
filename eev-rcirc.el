;;; eev-rcirc.el -- rcirc-related elisp hyperlinks.

;; Copyright (C) 2012,2013 Free Software Foundation, Inc.
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
;; Version:    2013aug16
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-rcirc.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-rcirc.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                                                (find-eev-intro)

;;; Commentary:





;; When we run (rcirc-connect "irc.freenode.net" ...)
;; and the buffer "*irc.freenode.net*" already exists, `rcirc-connect'
;; does nasty things; so it's better to run
;;   (eepitch '(ee-rcirc-connect))
;; instead.
;;
;; Actually we want to do another trick too. Killing the buffer
;; "*irc.freenode.net*" is too expensive, as reconnection takes about
;; 10 seconds; so we set `eepitch-kill' to something different from
;; the default, which is `(eepitch-kill-buffer)'.

(defun ee-rcirc-serverbuf  (server)         (format "*%s*" server))
(defun ee-rcirc-channelbuf (server channel) (format "%s@%s" channel server))
(defun ee-rcirc-connected  (server)
  (and (get-buffer           (ee-rcirc-serverbuf server))
       (rcirc-buffer-process (ee-rcirc-serverbuf server))))

(defun ee-rcirc-connect    (server channels)
  "Connect to an irc server (if not already connected).
TODO: if we are already connected to SERVER, just connect to CHANNELS."
  (if (not (ee-rcirc-connected server))
      (rcirc-connect server nil nil nil nil channels))
  (switch-to-buffer (ee-rcirc-serverbuf server)))



;; Hyperlinks to rcirc buffers
;;
(defun find-rcirc-buffer (server channels &optional channel &rest pos-spec-list)
  "Connect to an irc server (if not already connected) and switch to CHANNEL.
If CHANNEL is a string starting with \"#\", it is a channel to /join;
if CHANNEL is a string not starting with \"#\", it is a nick to /query;
if CHANNEL is nil, that means to use the server buffer."
  (ee-rcirc-connect server channels)
  (if channel
      (if (equal "#" (substring channel 0 1))
	  (rcirc-cmd-join channel)
	(rcirc-cmd-query channel)))
  (apply 'ee-goto-position pos-spec-list))

(defvar ee-freenode-server "irc.freenode.net")
(defvar ee-freenode-channels '("#eev"))

(defun find-freenode (&optional channel &rest pos-spec-list)
  (apply 'find-rcirc-buffer
	  ee-freenode-server ee-freenode-channels channel pos-spec-list))

;; (find-freenode "#eev")
;; (find-freenode "#org-mode")
;; (find-freenode "edrx")


;; (find-freenode)





;; Support for eepitch'ing to rcirc buffers
;;
(defun ee-rcirc-sexp (server channel)
  `(find-ebuffer ,(ee-rcirc-channelbuf server channel)))

(defun eepitch-kill-rcirc  (server)
  (message "Not killing: %S" (ee-rcirc-serverbuf server)))

(defun eepitch-rcirc-server (server channels)
  "Connect to the irc server SERVER if not already connected, and to CHANNELS."
  (interactive)
  (eepitch `(ee-rcirc-connect ,server ',channels))
  (setq eepitch-kill `(eepitch-kill-rcirc ,server))
  (ee-rcirc-sexp server (car channels))) ; easter egg (use M-1 C-x C-e)

(defun eepitch-freenode (&optional channels) (interactive)
  (eepitch-rcirc-server "irc.freenode.net" (or channels '("#eev"))))

(defun eepitch-ircgnome (&optional channels) (interactive)
  (eepitch-rcirc-server "irc.gnome.org"    (or channels '("#docs"))))



(provide 'eev-rcirc)




;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
