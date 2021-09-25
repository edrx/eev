;;; eev-compose-hash.el --- `M-,' as a compose key (version with hash tables)  -*- lexical-binding: nil; -*-

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
;; Version:    20210607
;; Keywords:   e-scripts
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-compose-hash.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-compose-hash.el.html>
;;       See also: <http://angg.twu.net/eev-current/eev-readme.el.html>
;;                 <http://angg.twu.net/eev-intros/find-eev-intro.html>
;;                 <http://angg.twu.net/eev-intros/find-here-links-intro.html>
;;                                                (find-eev-intro)
;;                                                (find-here-links-intro)

;; «.ee-composes-do»	(to "ee-composes-do")
;; «.to-strings»	(to "to-strings")
;; «.compose»		(to "compose")
;; «.faces»		(to "faces")
;; «.bigstr-specs»	(to "bigstr-specs")

;;; Commentary:

;; This is a new, experimental feature that is not loaded by default.
;; I use LaTeX a lot, and Emacs has several ways to insert non-ascii
;; characters - like these ones:
;;
;;   Greek letters: (find-einsert '((900 1000)))
;;   Some mathematical characters: (find-einsert '((8592 9000)))
;;   Some mathematical alphabets:

' (find-einsert '("Bold: "                   (119808 119859) "\n"
                  "Italic: "                 (119860 119911) "\n"
                  "Bold italic: "            (119912 119963) "\n"
                  "Script: "                 (119964 120015) "\n"
                  "Bold script: "            (120016 120067) "\n"
                  "Fraktur: "                (120068 120119) "\n"
                  "Double-struck: "          (120120 120171) "\n"
                  "Bold Fraktur: "           (120172 120223) "\n"
                  "Sans-serif: "             (120224 120275) "\n"
                  "Sans-serif bold: "        (120276 120327) "\n"
                  "Sans-serif italic: "      (120328 120379) "\n"
                  "Sans-serif bold italic: " (120380 120431) "\n"
                  "Monospace: "              (120432 120483) "\n"
                  "\n" (120484 120831)))

;; See:
;;
;;   (find-equailfile "latin-ltx.el")
;;   (find-equailfile "sgml-input.el")
;;   (find-enode "Inserting Text" "C-x 8 <RET> left single")
;;   (find-enode "International Chars" "C-x 8")
;;   (find-enode "Input Methods")
;;
;; I needed something that was easier to understand and easier to
;; extend - in the sense of: adding new characters - than these
;; standard methods, and that would let me use one face for the
;; uppercase greek characters, another for lowercase greek, another
;; for the standard mathematical chars, and so on... so in the late
;; 90s I devised a very simple hack in which `M-,' followed by two
;; characters would "compose" these two characters according to a
;; conversion table, and I also had a very, very ugly hack that
;; allowed me to specify this conversion table, AND THE FACES, in a
;; somewhat human-friendly format, as lists of big strings and face
;; names... That implementation used alists for the conversion tables,
;; and was very hard to maintain.
;;
;; The implemention in this file is much simpler - because is uses
;; hash tables for the conversion tables, and uses a big regexp built
;; with `rx' to specify how to parse the human-readable conversion
;; tables given in big strings... and also, as a bonus, it lets me
;; specify how the special characters are to defined in LaTeX, both
;; with `\catcode' and with `\DeclareUnicodeCharacter'.
;;
;; I decided to include this in eev because it MAY be useful in my
;; next workshops on LaTeX (+ Emacs + eev): people can learn this,
;; including how to extend it, in a few minutes, and then learn the
;; standard input methods later - in more than a few minutes.
;;
;; This is very-new-ish code. Everything may change - ESPECIALLY the
;; terminology and the names of the variables and functions. I am
;; currently using these terms:
;;
;;   a "keys" means "a pair of easy-to-type characters" (that have an
;;     unicode character associated to them),
;;
;;   a "pos" means this unicode character,
;;
;;   "composes" means "all the data structures that show how each
;;      "keys" is converted to a "pos", how each "pos" is converted to
;;      latex code, which face is associated to each pos, etc, etc,
;;
;;   a "big string", or a "bigstr", is a string that may contain, and
;;     usually contains, newlines,
;;
;;   a "bigstr spec" is a big string that specifies how the "pos"s are
;;     associates to "keys", to latex code, and to faces.

;;
;; Usage:
;;   (load "eev-compose-hash.el")
;;   (ee-composes-do ee-composes-bigstr-accents)
;;   (ee-composes-do ee-composes-bigstr-otheriso)
;;   (ee-composes-do ee-composes-bigstr-math)
;;   (define-key eev-mode-map (kbd "M-,") 'ee-compose-pair)




;;;                                                          _       
;;;   ___ ___  _ __ ___  _ __   ___  ___  ___  ___        __| | ___  
;;;  / __/ _ \| '_ ` _ \| '_ \ / _ \/ __|/ _ \/ __|_____ / _` |/ _ \ 
;;; | (_| (_) | | | | | | |_) | (_) \__ \  __/\__ \_____| (_| | (_) |
;;;  \___\___/|_| |_| |_| .__/ \___/|___/\___||___/      \__,_|\___/ 
;;;                     |_|                                          
;;
;; «ee-composes-do»  (to ".ee-composes-do")
;;
(defun ee-composes-initialize ()
  (setq ee-composes-keys-to-pos  (make-hash-table :test 'equal))
  (setq ee-composes-pos-to-latex (make-hash-table :test 'equal))
  (setq ee-composes-current-face nil))

(ee-composes-initialize)

;; The regexp that is used to parse bigstrspecs.
;; The "4" stresses that it has 4 groups.
;; See: (find-es "emacs" "while-string-match")
;;      (find-es "emacs" "rx")
;;
(setq ee-composes-regexp4
      (rx-let ((nonblank (not (any " \t\n"))))
        (rx (or (and (group-n 1 nonblank) ; 1: pos
		     " "
		     (group-n 2 nonblank nonblank) ; 2: keys
		     (optional
		      " "
		      (group-n 3 nonblank (zero-or-more nonblank))) ; 3: latex
		     )
		(and "face: " (group-n 4 (one-or-more nonblank))) ; 4: face
		(and ";; " (zero-or-more (not "\n")))		  ; comment
		))))

(defun ee-composes-do (bigstrspec &optional action)
  "Parse BIGSTRSPEC and eval ACTION for each parseable part.
The default ACTION is (ee-composes-do-default)."
  (setq action (or action '(ee-composes-do-default)))
  (let ((bigstrspecpos 0))
    (while (string-match ee-composes-regexp4 bigstrspec bigstrspecpos)
      (let ((pos   (match-string 1 bigstrspec))
            (keys  (match-string 2 bigstrspec))
            (latex (match-string 3 bigstrspec))
            (face  (match-string 4 bigstrspec)))
	(eval action)
	(setq bigstrspecpos (match-end 0))))))

(defun ee-composes-do-default ()
  "This is the default action for `ee-composes-do'."
  (if pos   (puthash keys pos  ee-composes-keys-to-pos))
  (if pos   (eepitch-set-glyph0 (aref pos 0) (aref pos 0) ee-composes-current-face))
  (if latex (puthash pos latex ee-composes-pos-to-latex))
  (if face  (setq ee-composes-current-face (read face)))
  )

(defun ee-composes-do-remove-face ()
  "Experimental."
  (if pos   (puthash keys pos  ee-composes-keys-to-pos))
  (if pos   (eepitch-set-glyph0 (aref pos 0) (aref pos 0) 'default))
  (if latex (puthash pos latex ee-composes-pos-to-latex))
  (if face  (setq ee-composes-current-face (read face)))
  )

(defun ee-composes-do-test ()
  "An alternative action for `ee-composes-do', for tests."
  (insert (format "\n;; %S %S %S / %S" pos keys latex face)))

;; Low-level tests:
;; (load "eev-compose-hash.el")
;; (ee-composes-initialize)
;; (ee-composes-do "á 'a   é 'e   é !e EE")
;; (ee-composes-do "á 'a   é 'e   é !e EE" '(ee-composes-do-test))
;; (let ((pos "á") (keys "'a") (face nil) (latex "aacu")) (ee-composes-do-default))
;; (let ((pos "á") (keys "'a") (face nil) (latex "aacu")) (ee-composes-do-test))
;; (find-estring (ee-composes-to-string))



;;;  _              _        _                 
;;; | |_ ___    ___| |_ _ __(_)_ __   __ _ ___ 
;;; | __/ _ \  / __| __| '__| | '_ \ / _` / __|
;;; | || (_) | \__ \ |_| |  | | | | | (_| \__ \
;;;  \__\___/  |___/\__|_|  |_|_| |_|\__, |___/
;;;                                  |___/     
;;
;; «to-strings»  (to ".to-strings")
;; Functions to convert the hash tables to
;; several human-readable formats.
;;
;; Tests: (find-ehashtable ee-composes-pos-to-latex)
;;        (find-ehashtable ee-composes-keys-to-pos)
;;        (find-estring (ee-composes-to-string))
;;        (find-estring (ee-composes-to-catcodes))
;;        (find-estring (ee-composes-to-declareunicodes))
;;
(defun ee-composes-to-string ()
  (ee-hashtable-to-string
   (lambda (keys pos ht-keys-to-pos)
     (let ((latex (gethash pos ee-composes-pos-to-latex)))
       (format "%s %s %s\n" pos keys (or latex ""))))
   ee-composes-keys-to-pos))

(defun ee-composes-to-catcodes ()
  (ee-hashtable-to-string
   (lambda (pos latex ht-pos-to-latex)
     (format "\\catcode`%s=13 \\def%s{%s}\n" pos pos latex))
   ee-composes-pos-to-latex))

(defun ee-composes-to-declareunicodes ()
  (ee-hashtable-to-string
   (lambda (pos latex ht-pos-to-latex)
     (let* ((nnnn (format "%04X" (aref pos 0)))
	    (decl (format "\\DeclareUnicodeCharacter{%s}{%s}" nnnn latex))
	    (line (format "%-50s %% %s\n" decl pos)))
       line))
   ee-composes-pos-to-latex))



;;;                                           
;;;   ___ ___  _ __ ___  _ __   ___  ___  ___ 
;;;  / __/ _ \| '_ ` _ \| '_ \ / _ \/ __|/ _ \
;;; | (_| (_) | | | | | | |_) | (_) \__ \  __/
;;;  \___\___/|_| |_| |_| .__/ \___/|___/\___|
;;;                     |_|                   
;;
;; «compose»  (to ".compose")

(defun ee-compose-pair (&optional arg)
  "Read two keys and insert the result of their \"composition\".
Use `ee-composes-keys-to-pos' to determine the composite char.
When ARG is 0 show the current composes in a temporary buffer
instead of inserting."
  (interactive "P")
  (if (eq arg 0)
      (find-estring
       (concat "  See: (find-eev \"eev-compose-hash.el\")\n\n"
	       (ee-composes-to-string)))
    (let* ((keys (format "%c%c"
			 (read-event "Compose key 1: " t)
			 (read-event "Compose key 2: " t)))
	   (pos (gethash keys ee-composes-keys-to-pos)))
      (if (not pos)
	  (error "Pair %S not in `ee-composes-keys-to-pos'" keys))
      ;;
      ;; Should we insert N copies when ARG is N?
      (insert pos))))

;; Tests: (eek "RET M-, 8 8")
;;        (eek "M-0 M-,")



;;;   __                     
;;;  / _| __ _  ___ ___  ___ 
;;; | |_ / _` |/ __/ _ \/ __|
;;; |  _| (_| | (_|  __/\__ \
;;; |_|  \__,_|\___\___||___/
;;;                          
;; «faces»  (to ".faces")
;; Adapted from:
;; (find-anggfile "eev-current/eev-glyphs.el" "yellow-on-red")
;; (find-angg "eev-current/eev-math-glyphs.el" "faces")

;; Define several faces for glyphs in a few lines of code.
;; This is too rigid, but whatever.
;; (find-ecolors)
;; (find-efaces)
;; (find-efaces "ee-composes")
;;
(defun ee-composes-set-face (face fg bg)
  (make-face face)
  (set-face-foreground face fg)
  (set-face-background face bg))

(ee-composes-set-face 'ee-composes-face-Greek    "orange"        "gray20")
(ee-composes-set-face 'ee-composes-face-greek    "coral"         "gray20")
(ee-composes-set-face 'ee-composes-face-logical  "SteelBlue1"    "gray20")
(ee-composes-set-face 'ee-composes-face-math     "RoyalBlue2"    "gray20")
(ee-composes-set-face 'ee-composes-face-linear   "PaleVioletRed" "gray20")
(ee-composes-set-face 'ee-composes-face-script   "PaleVioletRed" "gray20")
(ee-composes-set-face 'ee-composes-face-graphic  "red"           "gray20")
(ee-composes-set-face 'ee-composes-face-category "red"           "gray20")
(ee-composes-set-face 'ee-composes-face-font     "gold"          "DarkOrange4")
(ee-composes-set-face 'ee-composes-face-yellow-on-red "yellow"  "red")



;;;  _     _           _                                  
;;; | |__ (_) __ _ ___| |_ _ __   ___ _ __   ___  ___ ___ 
;;; | '_ \| |/ _` / __| __| '__| / __| '_ \ / _ \/ __/ __|
;;; | |_) | | (_| \__ \ |_| |    \__ \ |_) |  __/ (__\__ \
;;; |_.__/|_|\__, |___/\__|_|    |___/ .__/ \___|\___|___/
;;;          |___/                   |_|                  
;;
;; «bigstr-specs»  (to ".bigstr-specs")
;; These are the default bigstr specs.
;; I uses setq instead of defvar because this is a hack!...

(setq ee-composes-bigstr-accents "
  face: nil
  À `A    È `E   Ì `I    Ò `O    Ù `U 
  à `a    è `e   ì `i    ò `o    ù `u 
  Á 'A    É 'E   Í 'I    Ó 'O    Ú 'U 
  á 'a    é 'e   í 'i    ó 'o    ú 'u 
  Â ^A    Ê ^E   Î ^I    Ô ^O    Û ^U 
  â ^a    ê ^e   î ^i    ô ^o    û ^u 
  Ã ~A                   Õ ~O     
  ã ~a                   õ ~o     
  Ä \"A   Ë \"E  Ï \"I   Ö \"O   Ü \"U 
  ä \"a   ë \"e  ï \"i   ö \"o   ü \"u 
  Ç 'C    Ç CC   Ñ ~N  	    
  ç 'c    ç cc   ñ ~n  	    
")

(setq ee-composes-bigstr-otheriso "
  face: nil
  ª _a
  º _o
  Æ AE
  æ ae
  ß ss

  ¼ 14
  ½ 12
  ¾ 34

  ¿ ??

  ¡ !! \\text{\\textexclamdown}
  § SS \\S

  ° 00 ^\\circ

  ± +- \\pm
  ÷ :- \\div
  · cd \\cdot
  × xx \\times
  ¬ nt \\neg

  face: eev-glyph-face-green
  « <<
  » >>

  face: ee-composes-face-math
  ² 22 \\superscriptTwo
  ³ 33 \\superscriptThree
  ¹ -1 \\superscriptOne
  ¹ 11 \\superscriptOne
")


(setq ee-composes-bigstr-math "
  face: ee-composes-face-Greek
  Γ GG \\Gamma
  Δ DD \\Delta
  Θ Th \\Theta
  Λ La \\Lambda
  Π Pi \\Pi
  Σ Si \\Sigma
  Φ Ph \\Phi
  Ψ Ps \\Psi
  Ω Om \\Omega

  face: ee-composes-face-greek
  α aa \\alpha
  β bb \\beta
  γ gg \\gamma
  δ dd \\delta
  ε ee \\epsilon
  ε ee \\varepsilon
  ζ ze \\zeta
  η et \\eta
  θ th \\theta
  ι io \\iota
  κ kk \\kappa
  λ ll \\lambda
  μ mu \\mu
  ν nu \\nu
  ξ xi \\xi
  π pi \\pi
  ρ ro \\rho
  σ si \\sigma
  τ ta \\tau
  φ ph \\varphi
  χ ch \\chi
  ψ ps \\psi
  ω om \\omega
  ϕ vp \\phi

  face: ee-composes-face-math
  • bu \\bullet
  … .. \\ldots
  ℓ el \\ell
  ⅋ && \\bindnasrepma
  ← <- \\ot
  ↑ up \\upto
  → -> \\to
  → to  
  ↓ dn \\dnto
  ↔ <> \\bij
  ↕ ud \\updownarrow
  ↖ NW \\nwarrow
  ↗ NE \\nearrow
  ↘ SE \\searrow
  ↙ SW \\swarrow
  ↣ >t \\twoheadrightarrow
  ↣ ep \\epito
  ↤ mo \\mapsfrom
  ↦ mt \\mapsto
  ⇀ hu \\rightharpoonup
  ⇐ <= \\Leftarrow
  ⇒ => \\funto
  ⇔ LR \\Leftrightarrow
  ⇝ s> \\squigglyarrow
  ∀ fa \\forall
  ∂ Pa \\partial
  ∃ ex \\exists
  ∅ em \\emptyset
  ∇ Na \\nabla
  ∈ in \\in
  ∖ sm \\backslash
  ∘ oo \\circ
  √ sq \\sqrt
  ∞ 88 \\infty
  ∧ la \\land
  ∨ lo \\lor
  ∩ ca \\cap
  ∪ cu \\cup
  ∫ In \\int
  ∼ 1~ \\sim
  ∼ ~1 \\sim
  ≃ -~ \\simeq
  ≅ =~ \\cong
  ≈ ~~ \\approx
  ≠ != \\neq
  ≡ == \\equiv
  ≤ le \\le
  ≥ ge \\ge
  ⊂ su \\subset
  ⊃ Su \\supset
  ⊆ se \\subseteq
  ⊇ Se \\supseteq
  ⊓ qa \\sqcap
  ⊔ qu \\sqcup
  ⊕ o+ \\oplus
  ⊖ o- \\ominus
  ⊗ ox \\otimes
  ⊘ o/ \\oslash
  ⊙ o. \\odot
  ⊢ |- \\vdash
  ⊣ -| \\dashv
  ⊤ TT \\top
  ⊥ BO \\bot
  ⊨ |= \\vDash
  ⊸ -o  
  ⋀ LA \\bigwedge
  ⋁ LO \\bigvee
  ⋂ CA \\bigcap
  ⋃ CU \\bigcup
  ⋄ po \\lozenge
  ⋅ Do \\Box
  〈 <1 \\langle
  〉 1> \\rangle
  ▁ __ \\_
  □ Bo \\Box
  ◻ Bo \\Box
  ◻ nc
  ♭ fl \\flat
  ♮ na \\natural
  ♯ sh \\sharp
  ✀ li
  ⟦ [[ \\llbracket
  ⟧ ]] \\rrbracket
  ⠆ :: {:}

  face: ee-composes-face-yellow-on-red
  𝐛 bf \\mathbf
  𝐢 it \\textsl
  𝐫 rm \\mathrm
  𝐬 sf \\mathsf
  𝐭 tx \\text

  face: ee-composes-face-script
  𝓐 sA \\mathcal{A}
  𝓑 sB \\mathcal{B}
  𝓒 sC \\mathcal{C}
  𝓓 sD \\mathcal{D}
  𝓔 sE \\mathcal{E}
  𝓕 sF \\mathcal{F}
  𝓖 sG \\mathcal{G}
  𝓗 sH \\mathcal{H}
  𝓘 sI \\mathcal{I}
  𝓙 sJ \\mathcal{J}
  𝓚 sK \\mathcal{K}
  𝓛 sL \\mathcal{L}
  𝓜 sM \\mathcal{M}
  𝓝 sN \\mathcal{N}
  𝓞 sO \\mathcal{O}
  𝓟 sP \\mathcal{P}
  𝓠 sQ \\mathcal{Q}
  𝓡 sR \\mathcal{R}
  𝓢 sS \\mathcal{S}
  𝓣 sT \\mathcal{T}
  𝓤 sU \\mathcal{U}
  𝓥 sV \\mathcal{V}
  𝓦 sW \\mathcal{W}
  𝓧 sX \\mathcal{X}
  𝓨 sY \\mathcal{Y}
  𝓩 sZ \\mathcal{Z}

  ;; Categories
  face: ee-composes-face-category
  𝐀 cA \\catA
  𝐁 cB \\catB
  𝐂 cC \\catC
  𝐃 cD \\catD
  𝐄 cE \\catE
  𝐅 cF \\catF
  𝐆 cG \\catG
  𝐇 cH \\catH
  𝐈 cI \\catI
  𝐉 cJ \\catJ
  𝐊 cK \\catK
  𝐋 cL \\catL
  𝐌 cM \\catM
  𝐍 cN \\catN
  𝐎 cO \\catO
  𝐏 cP \\catP
  𝐐 cQ \\catQ
  𝐑 cR \\catR
  𝐒 cS \\catS
  𝐓 cT \\catT
  𝐔 cU \\catU
  𝐕 cV \\catV
  𝐖 cW \\catW
  𝐗 cX \\catX
  𝐘 cY \\catY
  𝐙 cZ \\catZ


")


;; Test:
;; (load "eev-compose-hash.el")
;; (find-estring (ee-composes-to-string))
;;   (ee-composes-do ee-composes-bigstr-accents)
;;   (ee-composes-do ee-composes-bigstr-otheriso)
;;   (ee-composes-do ee-composes-bigstr-math)
;; (find-estring (ee-composes-to-string))
;; (find-estring (ee-composes-to-catcodes))
;; (find-estring (ee-composes-to-declareunicodes))
;; (define-key eev-mode-map (kbd "M-,") 'ee-compose-pair)
;;
;; 2021aug31:
;;   (ee-composes-do ee-composes-bigstr-math '(ee-composes-do-remove-face))
;;   (ee-composes-do ee-composes-bigstr-math)



(provide 'eev-compose-hash)


;; Local Variables:
;; coding:            utf-8-unix
;; no-byte-compile:   t
;; End:
