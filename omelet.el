;;; omelet.el --- NICOLA input method interface for Emacsen/Egg
;; version 2.99.2

;; Copyright (C) 1999, 2000 Free Software Foundation, Inc
;; Copyright (C) 1999, 2000 Itsushi Minoura <minoura@eva.hi-ho.ne.jp>
;;                          Hiromiz Hiruma <YRB03640@nifty.ne.jp>
;; Copyright (C) 2003, 2004, 2005 Tetsuo Tsukamoto

;; This file is not part of EGG.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT-
;; ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; $B$3$N%U%!%$%k$r%m!<%I$9$k$3$H$K$h$j!$(BTamago 4 $B4D6-$K$*$1$kF|K\8lF~NO(B
;; $B4D6-$,%m!<%^;z<0$+$i(B NICOLA ($B?F;X%7%U%HJ}<0(B) $B$K@Z$jBX$o$j$^$9!%(B
;; Tamago 3 $B$K$*$$$F(B omelet $B$,?F;X%7%U%H2=$r9T$C$F$$$?$N$HF1Ey$NF/$-$r(B
;; $B$9$k$o$1$G$9!%(B

;; Tamago $B$O%P!<%8%g%s(B 4.0.6 $B0J9_$r$*4+$a$7$^$9!%JQ49%5!<%P$K(B Anthy $B$d(B
;; Canna $B$r(B $BMxMQ$7$?$$>l9g$O(B CVS $B$N@hC<$+$i<hF@$5$l$k$3$H$r$*4+$a$7$^(B
;; $B$9!%(B

;; $ cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/tamago login
;; ($B%Q%9%o!<%I$rJ9$+$l$?$i6u$N%Q%9%o!<%I$N$^$^(B RETURN)
;; $ cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/tamago co tamago

;; $B;H$$J}$OC1=c$G!$$3$N%U%!%$%k$r(B load-path $B$NDL$C$F$$$k%G%#%l%/%H%j$K(B
;; $BCV$-!$(B ~/.emacs $B$K(B

;; (require 'omelet)

;; $B$H=q$-$^$9!%(BTamago $B$N@_Dj$K$D$$$F$O>\:Y$r>J$-$^$9$,!$:GDc8B(B
;; `default-input-method' $B$H$$$&JQ?t$r@_Dj$7$J$1$l$P$$$1$^$;$s!%$3$N(B
;; $BJQ?t$NCM$H$7$F$O(B "japanese-egg-anthy"$B!$(B"japanese-egg-wnn"$B!$(B
;; "japanese-egg-canna"$B!$(B "japanese-egg-sj3" $B$N$$$:$l$+$r@_Dj$7$^$9!%(B

;; ($BNc(B)
;; (setq default-input-method "japanese-egg-canna")

;; Emacs $B5/F08e!$(B\C-\ $B$G(B Tamago $B$r5/F0$7$^$9!%(B

;;; Logs:

;; Tamago 4 $BMQ$G$C$A$"$2HG(B ($B4@(B
;; by Tetsuo Tsukamoto

;; $B$$$m$$$m@_Dj$G$-$k$h$&$K$$$8$C$?(B by Itsushi Minoura
;;

;;; Code:

(require 'custom)
(require 'egg)
(require 'its)
(require 'its/hira)

(defconst omelet-version "2.99.2")

(defgroup omelet nil "Omelet (NICOLA on egg) related customization."
  :prefix "omelet-"
  :group 'egg)

(defcustom omelet-interval 0.1 "\
*$BF1;~BG80H=Dj;~4V$rICC10L$G;XDj!#(B
$BI8=`$G$O(B 0.1 $BIC!#(B"
  :type 'number
  :group 'omelet)

(defcustom omelet-rshift-keys
  (list (cond
	 ((eq system-type 'windows-nt)
	  [convert])
	 (t
	  [henkan]))) "\
*$B1&?F;X%-!<$H$7$F;H$&%-!<!#(B"
  :type '(repeat sexp)
  :group 'omelet)

(defcustom omelet-lshift-keys
  (list (cond
	 ((eq system-type 'windows-nt)
	  [noconvert])
	 (t
	  [muhenkan]))) "\
*$B:8?F;X%-!<$H$7$F;H$&%-!<!#(B"
  :type '(repeat sexp)
  :group 'omelet)

(defcustom omelet-use-US-layout nil  "\
*US $B%-!<%\!<%IG[Ns$+!)(B"
  :type 'boolean
  :group 'omelet)

(defvar omelet-jis-plain-rule-list
  '((?- . "$B!](B") (?^ . "$B!9(B") (?\\ . "$B!o(B")
    (?q . "$B!#(B") (?w . "$B$+(B") (?e . "$B$?(B") (?r . "$B$3(B") (?t . "$B$5(B")
    (?y . "$B$i(B") (?u . "$B$A(B") (?i . "$B$/(B") (?o . "$B$D(B") (?p . "$B!$(B")
    (?@ . "$B!"(B") (?\[ . "$B!+(B")
    (?a . "$B$&(B") (?s . "$B$7(B") (?d . "$B$F(B") (?f . "$B$1(B") (?g . "$B$;(B")
    (?h . "$B$O(B") (?j . "$B$H(B") (?k . "$B$-(B") (?l . "$B$$(B") (?\; . "$B$s(B")
    (?: . "$B!'(B") (?\] . "$B!D(B")
    (?z . "$B!%(B") (?x . "$B$R(B") (?c . "$B$9(B") (?v . "$B$U(B") (?b . "$B$X(B")
    (?n . "$B$a(B") (?m . "$B$=(B") (?, . "$B$M(B") (?. . "$B$[(B") (?/ . "$B!&(B")) "\
$BC1FHBG80;~$NF~NO%k!<%k!#(B")

(defvar omelet-jis-rshift-rule-list
  '((?1 . "$B!*(B") (?2 . "$B!H(B") (?3 . "$B!I(B") (?4 . "$B!t(B") (?5 . "$B!s(B")
    (?6 . "$B!N(B") (?7 . "$B!O(B") (?8 . "$B!J(B") (?9 . "$B!K(B") (?0 . "$B!X(B")
    (?- . "$B!Y(B") (?^ . "$B!F(B") (?\\ . "$B!G(B")
    (?q . "$B$p(B") (?w . "$B$,(B") (?e . "$B$@(B") (?r . "$B$4(B") (?t . "$B$6(B")
    (?y . "$B$h(B") (?u . "$B$K(B") (?i . "$B$k(B") (?o . "$B$^(B") (?p . "$B$'(B")
    (?@ . "$B!w(B") (?\[ . "$B!,(B")
    (?a . "$B%t(B") (?s . "$B$8(B") (?d . "$B$G(B") (?f . "$B$2(B") (?g . "$B$<(B")
    (?h . "$B$_(B") (?j . "$B$*(B") (?k . "$B$N(B") (?l . "$B$g(B") (?\; . "$B$C(B")
    (?: . "$B!((B") (?\] . "$B!=(B")
    (?z . "$B$q(B") (?x . "$B$S(B") (?c . "$B$:(B") (?v . "$B$V(B") (?b . "$B$Y(B")
    (?n . "$B$L(B") (?m . "$B$f(B") (?, . "$B$`(B") (?. . "$B$o(B") (?/ . "$B$)(B")) "\
$B1&?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(defvar omelet-jis-lshift-rule-list
  '((?1 . "$B!)(B") (?2 . "$B!?(B") (?3 . "$B!A(B") (?4 . "$B!V(B") (?5 . "$B!W(B")
    (?6 . "$B!N(B") (?7 . "$B!O(B") (?8 . "$B!Z(B") (?9 . "$B![(B") (?0 . "$B!P(B")
    (?- . "$B!Q(B") (?^ . "$B!a(B") (?\\ . "$B!2(B")
    (?q . "$B$!(B") (?w . "$B$((B") (?e . "$B$j(B") (?r . "$B$c(B") (?t . "$B$l(B")
    (?y . "$B$Q(B") (?u . "$B$B(B") (?i . "$B$0(B") (?o . "$B$E(B") (?p . "$B$T(B")
    (?@ . "$B!_(B") (?\[ . "$B!7(B")
    (?a . "$B$r(B") (?s . "$B$"(B") (?d . "$B$J(B") (?f . "$B$e(B") (?g . "$B$b(B")
    (?h . "$B$P(B") (?j . "$B$I(B") (?k . "$B$.(B") (?l . "$B$](B") (?\; . "$B%v(B")
    (?: . "$B!v(B") (?\] . "$B!\(B")
    (?z . "$B$%(B") (?x . "$B!<(B") (?c . "$B$m(B") (?v . "$B$d(B") (?b . "$B$#(B")
    (?n . "$B$W(B") (?m . "$B$>(B") (?, . "$B$Z(B") (?. . "$B$\(B") (?/ . "$B$n(B")) "\
$B:8?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(defvar omelet-us-plain-rule-list
  '((?- . "$B!](B") (?^ . "$B!9(B") (?\\ . "$B!o(B")
    (?q . "$B!#(B") (?w . "$B$+(B") (?e . "$B$?(B") (?r . "$B$3(B") (?t . "$B$5(B")
    (?y . "$B$i(B") (?u . "$B$A(B") (?i . "$B$/(B") (?o . "$B$D(B") (?p . "$B!$(B")
    (?\[ . "$B!"(B") (?\] . "$B!+(B")
    (?a . "$B$&(B") (?s . "$B$7(B") (?d . "$B$F(B") (?f . "$B$1(B") (?g . "$B$;(B")
    (?h . "$B$O(B") (?j . "$B$H(B") (?k . "$B$-(B") (?l . "$B$$(B") (?\; . "$B$s(B")
    (?z . "$B!%(B") (?x . "$B$R(B") (?c . "$B$9(B") (?v . "$B$U(B") (?b . "$B$X(B")
    (?n . "$B$a(B") (?m . "$B$=(B") (?, . "$B$M(B") (?. . "$B$[(B") (?/ . "$B!&(B")
    (?H . "$B$Q(B") (?X . "$B$T(B") (?V . "$B$W(B") (?B . "$B$Z(B") (?> . "$B$](B")) "\
$BC1FHBG80;~$NF~NO%k!<%k!#(B")

(defvar omelet-us-rshift-rule-list
  '((?1 . "$B!*(B") (?2 . "$B!H(B") (?3 . "$B!I(B") (?4 . "$B!t(B") (?5 . "$B!s(B")
    (?6 . "$B!N(B") (?7 . "$B!O(B") (?8 . "$B!J(B") (?9 . "$B!K(B") (?0 . "$B!X(B")
    (?- . "$B!Y(B") (?^ . "$B!F(B") (?\\ . "$B!G(B")
    (?q . "$B$p(B") (?w . "$B$,(B") (?e . "$B$@(B") (?r . "$B$4(B") (?t . "$B$6(B")
    (?y . "$B$h(B") (?u . "$B$K(B") (?i . "$B$k(B") (?o . "$B$^(B") (?p . "$B$'(B")
    (?@ . "$B!w(B") (?\[ . "$B!,(B")
    (?a . "$B%t(B") (?s . "$B$8(B") (?d . "$B$G(B") (?f . "$B$2(B") (?g . "$B$<(B")
    (?h . "$B$_(B") (?j . "$B$*(B") (?k . "$B$N(B") (?l . "$B$g(B") (?\; . "$B$C(B")
    (?z . "$B$q(B") (?x . "$B$S(B") (?c . "$B$:(B") (?v . "$B$V(B") (?b . "$B$Y(B")
    (?n . "$B$L(B") (?m . "$B$f(B") (?, . "$B$`(B") (?. . "$B$o(B") (?/ . "$B$)(B")) "\
$B1&?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(defvar omelet-us-lshift-rule-list
  '((?1 . "$B!)(B") (?2 . "$B!?(B") (?3 . "$B!A(B") (?4 . "$B!V(B") (?5 . "$B!W(B")
    (?6 . "$B!N(B") (?7 . "$B!O(B") (?8 . "$B!Z(B") (?9 . "$B![(B") (?0 . "$B!P(B")
    (?- . "$B!Q(B") (?^ . "$B!a(B") (?\\ . "$B!2(B")
    (?q . "$B$!(B") (?w . "$B$((B") (?e . "$B$j(B") (?r . "$B$c(B") (?t . "$B$l(B")
    (?y . "$B$Q(B") (?u . "$B$B(B") (?i . "$B$0(B") (?o . "$B$E(B") (?p . "$B$T(B")
    (?a . "$B$r(B") (?s . "$B$"(B") (?d . "$B$J(B") (?f . "$B$e(B") (?g . "$B$b(B")
    (?h . "$B$P(B") (?j . "$B$I(B") (?k . "$B$.(B") (?l . "$B$](B") (?\; . "$B%v(B")
    (?z . "$B$%(B") (?x . "$B!<(B") (?c . "$B$m(B") (?v . "$B$d(B") (?b . "$B$#(B")
    (?n . "$B$W(B") (?m . "$B$>(B") (?, . "$B$Z(B") (?. . "$B$\(B") (?/ . "$B$n(B")) "\
$B:8?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(defvar omelet-plain-rule-list nil)
(defvar omelet-rshift-rule-list nil)
(defvar omelet-lshift-rule-list nil)

(defun omelet-version ()
  "Display Version"
  (interactive)
  (message "omelet version: %s" omelet-version))

(defun omelet-event-to-key (event)
  "EVENT $B$rH/@8$9$k%-!<$r<hF@$9$k!#(B"
  (if (symbolp event)
      (vector event)
    event))

(defun omelet-its-state-machine (state next-state key emit)
  (let (expr-output-back kst/t output keyseq back)
    (cond
     ;; proceed to next status
     ((and next-state
	   (not (and its-disable-special-action
		     (eq (its-get-kst/t next-state) t))))
      (setq kst/t (its-get-kst/t next-state)
	    output (its-get-output next-state)
	    keyseq (its-get-keyseq next-state))
      (cond
       ;; Special actions.
       ((eq kst/t t)
	(if (stringp output)
	    (let ((its-current-language t))
	      (funcall emit (cons output keyseq) state 'its-cursor))
	  (funcall emit (cons "" keyseq) state 'its-cursor)
	  (apply (car output) (cdr output))))

       ;; Still, it's a intermediate state.
       ((its-kst-p kst/t)
	(funcall emit next-state state nil))

       ;; It's negative integer which specifies how many
       ;; characters we go backwards
       (kst/t
	(funcall emit next-state state 'its-cursor)
	(its-state-machine-keyseq (substring keyseq kst/t) emit (< key 0)))

       ;; Here we arrive to a terminal state.
       ;; Emit a DSYL, and go ahead.
       (t
	(funcall emit next-state state 'its-cursor))))

     ;; push back by otherwise status
     ((and (>= key 0)
	   (setq expr-output-back (its-get-otherwise state key)))
      (setq keyseq (concat (its-get-keyseq state) (vector key))
	    back (its-eob-back expr-output-back))
      (funcall emit
	       (cons (or (its-get-output expr-output-back)
			 (its-get-output
			  (its-goto-state (substring keyseq 0 back))))
		     (cons keyseq back))
	       state t)
      (its-state-machine-keyseq
       (substring keyseq back) emit))

     ((eq its-barf-on-invalid-keyseq 'its-keyseq-test)
      'its-keyseq-test-failed)

     ;; No next state for KEY.  It's invalid sequence.
     (its-barf-on-invalid-keyseq
      (its-input-error))

     (t
      ;; XXX Should make DSYL (instead of VSYL)?
      (setq keyseq (concat (its-get-keyseq state) (if (> key 0) (vector key))))
      (funcall emit (its-make-VSYL keyseq) state nil)))))

(defun omelet-rshift-single ()
  (let ((last-command-event 32))
    (call-interactively
     (cond ((its-in-fence-p)
	    #'its-kick-convert-region)
	   ((egg-conversion-fence-p)
	    #'egg-next-candidate)
	   (t
	    (or (lookup-key (current-global-map)
			    (char-to-string 32))
		#'undefined))))))

(defun omelet-lshift-single ()
  (let ((last-command-event 13))
    (call-interactively
     (cond ((its-in-fence-p)
	    #'its-exit-mode)
	   ((egg-conversion-fence-p)
	    #'egg-exit-conversion)
	   (t
	    (or (lookup-key (current-global-map)
			    (char-to-string 13))
		#'undefined))))))

(defun omelet-shift-followed-by-char (rule-list key)
  (when (egg-conversion-fence-p)
    (egg-exit-conversion))
  (let ((its-setup-fence-before-insert-SYL (not (its-in-fence-p)))
	(string (cdr (assq key rule-list))))
    (omelet-its-state-machine (its-get-start-state its-hira-map)
			      (list string "a") ; dummy
			      key
			      #'its-buffer-ins/del-SYL)))

(defun omelet-rshift-command (&optional arg)
  (interactive "P")
  (let ((right (eq this-command 'omelet-rshift-command))
	(single-stroke (sit-for omelet-interval)) event key keystr)
    (cond
     (single-stroke
      (if right
	  (omelet-rshift-single)
	(omelet-lshift-single)))
     (t
      (setq event (read-event))
      (setq key (omelet-event-to-key event))
      (when (integerp key)
	(setq keystr (char-to-string key)))
      (if (eq (key-binding (or keystr key)) this-command)
	  (dotimes (var 2)
	    (if right
		(omelet-rshift-single)
	      (omelet-lshift-single)))
	(omelet-shift-followed-by-char (if right
					   omelet-rshift-rule-list
					 omelet-lshift-rule-list)
				       key))))))

(defalias 'omelet-lshift-command 'omelet-rshift-command)

(defadvice its-state-machine (around omelet activate)
  (let ((this-key (this-command-keys))
	(single-stroke (sit-for omelet-interval))
	string states-list next-event next-key)
    (cond (single-stroke
	   (setq string (cdr (assq (ad-get-arg 1) ; KEY
				   omelet-plain-rule-list)))
	   (if (not string)
	       ad-do-it
	     (omelet-its-state-machine (ad-get-arg 0)    ; STATE
				       (list string "a") ; dummy
				       (ad-get-arg 1)    ; KEY
				       (ad-get-arg 2)))) ; EMIT
	  (t
	   (setq next-event (read-event))
	   (setq next-key (omelet-event-to-key next-event))
	   (when (integerp next-key)
	     (setq next-key (char-to-string next-key)))
	   (let ((this (key-description this-key))
		 (next (key-description next-key))
		 (rshifts (mapcar 'key-description omelet-rshift-keys))
		 (lshifts (mapcar 'key-description omelet-lshift-keys)))
	     (cond
	      ((member this rshifts)
	       (setq string (cdr (assq (string-to-char next-key)
				       omelet-rshift-rule-list))))
	      ((member this lshifts)
	       (setq string (cdr (assq (string-to-char next-key)
				       omelet-lshift-rule-list))))
	      ((member next rshifts)
	       (setq string (cdr (assq (string-to-char this-key)
				       omelet-rshift-rule-list))))
	      ((member next lshifts)
	       (setq string (cdr (assq (string-to-char this-key)
				       omelet-lshift-rule-list))))
	      (t
	       (setq string (cdr (assq (ad-get-arg 1) ; KEY
				       omelet-plain-rule-list)))
	       (setq unread-command-events
		     (nconc unread-command-events (list next-event)))))
	     (if (not string)
		 ad-do-it
	       (omelet-its-state-machine (ad-get-arg 0)	       ; STATE
					 (list string "a")     ; dummy
					 (ad-get-arg 1)	       ; KEY
					 (ad-get-arg 2)))))))) ; EMIT

(add-hook 'input-method-activate-hook
  #'(lambda ()
      (dolist (key omelet-rshift-keys)
	(define-key egg-modefull-map key 'omelet-rshift-command)
	(define-key egg-conversion-map key 'omelet-rshift-command)
	(define-key its-mode-map key 'omelet-rshift-command))

      (dolist (key omelet-lshift-keys)
	(define-key egg-modefull-map key 'omelet-lshift-command)
	(define-key egg-conversion-map key 'omelet-lshift-command)
	(define-key its-mode-map key 'omelet-lshift-command))

      ;; $B%-!<%\!<%I$r%;%C%H%"%C%W$9$k(B
      (cond
       (omelet-use-US-layout
	(setq omelet-plain-rule-list omelet-us-plain-rule-list)
	(setq omelet-rshift-rule-list omelet-us-rshift-rule-list)
	(setq omelet-lshift-rule-list omelet-us-lshift-rule-list))
       (t
	(setq omelet-plain-rule-list omelet-jis-plain-rule-list)
	(setq omelet-rshift-rule-list omelet-jis-rshift-rule-list)
	(setq omelet-lshift-rule-list omelet-jis-lshift-rule-list)))))

(provide 'omelet)

;; omelet.el ends here


