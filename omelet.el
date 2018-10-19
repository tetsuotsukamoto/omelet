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

;; このファイルをロードすることにより，Tamago 4 環境における日本語入力
;; 環境がローマ字式から NICOLA (親指シフト方式) に切り替わります．
;; Tamago 3 において omelet が親指シフト化を行っていたのと同等の働きを
;; するわけです．

;; Tamago はバージョン 4.0.6 以降をお勧めします．変換サーバに Anthy や
;; Canna を 利用したい場合は CVS の先端から取得されることをお勧めしま
;; す．

;; $ cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/tamago login
;; (パスワードを聞かれたら空のパスワードのまま RETURN)
;; $ cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/tamago co tamago

;; 使い方は単純で，このファイルを load-path の通っているディレクトリに
;; 置き， ~/.emacs に

;; (require 'omelet)

;; と書きます．Tamago の設定については詳細を省きますが，最低限
;; `default-input-method' という変数を設定しなければいけません．この
;; 変数の値としては "japanese-egg-anthy"，"japanese-egg-wnn"，
;; "japanese-egg-canna"， "japanese-egg-sj3" のいずれかを設定します．

;; (例)
;; (setq default-input-method "japanese-egg-canna")

;; Emacs 起動後，\C-\ で Tamago を起動します．

;;; Logs:

;; Tamago 4 用でっちあげ版 (汗
;; by Tetsuo Tsukamoto

;; いろいろ設定できるようにいじった by Itsushi Minoura
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
*同時打鍵判定時間を秒単位で指定。
標準では 0.1 秒。"
  :type 'number
  :group 'omelet)

(defcustom omelet-rshift-keys
  (list (cond
	 ((eq system-type 'windows-nt)
	  [convert])
	 (t
	  [henkan]))) "\
*右親指キーとして使うキー。"
  :type '(repeat sexp)
  :group 'omelet)

(defcustom omelet-lshift-keys
  (list (cond
	 ((eq system-type 'windows-nt)
	  [noconvert])
	 (t
	  [muhenkan]))) "\
*左親指キーとして使うキー。"
  :type '(repeat sexp)
  :group 'omelet)

(defcustom omelet-use-US-layout nil  "\
*US キーボード配列か？"
  :type 'boolean
  :group 'omelet)

(defvar omelet-jis-plain-rule-list
  '((?- . "−") (?^ . "々") (?\\ . "￥")
    (?q . "。") (?w . "か") (?e . "た") (?r . "こ") (?t . "さ")
    (?y . "ら") (?u . "ち") (?i . "く") (?o . "つ") (?p . "，")
    (?@ . "、") (?\[ . "゛")
    (?a . "う") (?s . "し") (?d . "て") (?f . "け") (?g . "せ")
    (?h . "は") (?j . "と") (?k . "き") (?l . "い") (?\; . "ん")
    (?: . "：") (?\] . "…")
    (?z . "．") (?x . "ひ") (?c . "す") (?v . "ふ") (?b . "へ")
    (?n . "め") (?m . "そ") (?, . "ね") (?. . "ほ") (?/ . "・")) "\
単独打鍵時の入力ルール。")

(defvar omelet-jis-rshift-rule-list
  '((?1 . "！") (?2 . "“") (?3 . "”") (?4 . "＃") (?5 . "％")
    (?6 . "［") (?7 . "］") (?8 . "（") (?9 . "）") (?0 . "『")
    (?- . "』") (?^ . "‘") (?\\ . "’")
    (?q . "ゐ") (?w . "が") (?e . "だ") (?r . "ご") (?t . "ざ")
    (?y . "よ") (?u . "に") (?i . "る") (?o . "ま") (?p . "ぇ")
    (?@ . "＠") (?\[ . "゜")
    (?a . "ヴ") (?s . "じ") (?d . "で") (?f . "げ") (?g . "ぜ")
    (?h . "み") (?j . "お") (?k . "の") (?l . "ょ") (?\; . "っ")
    (?: . "；") (?\] . "―")
    (?z . "ゑ") (?x . "び") (?c . "ず") (?v . "ぶ") (?b . "べ")
    (?n . "ぬ") (?m . "ゆ") (?, . "む") (?. . "わ") (?/ . "ぉ")) "\
右親指キーが押されたときの入力ルール。")

(defvar omelet-jis-lshift-rule-list
  '((?1 . "？") (?2 . "／") (?3 . "〜") (?4 . "「") (?5 . "」")
    (?6 . "［") (?7 . "］") (?8 . "【") (?9 . "】") (?0 . "｛")
    (?- . "｝") (?^ . "＝") (?\\ . "＿")
    (?q . "ぁ") (?w . "え") (?e . "り") (?r . "ゃ") (?t . "れ")
    (?y . "ぱ") (?u . "ぢ") (?i . "ぐ") (?o . "づ") (?p . "ぴ")
    (?@ . "×") (?\[ . "〃")
    (?a . "を") (?s . "あ") (?d . "な") (?f . "ゅ") (?g . "も")
    (?h . "ば") (?j . "ど") (?k . "ぎ") (?l . "ぽ") (?\; . "ヶ")
    (?: . "＊") (?\] . "＋")
    (?z . "ぅ") (?x . "ー") (?c . "ろ") (?v . "や") (?b . "ぃ")
    (?n . "ぷ") (?m . "ぞ") (?, . "ぺ") (?. . "ぼ") (?/ . "ゎ")) "\
左親指キーが押されたときの入力ルール。")

(defvar omelet-us-plain-rule-list
  '((?- . "−") (?^ . "々") (?\\ . "￥")
    (?q . "。") (?w . "か") (?e . "た") (?r . "こ") (?t . "さ")
    (?y . "ら") (?u . "ち") (?i . "く") (?o . "つ") (?p . "，")
    (?\[ . "、") (?\] . "゛")
    (?a . "う") (?s . "し") (?d . "て") (?f . "け") (?g . "せ")
    (?h . "は") (?j . "と") (?k . "き") (?l . "い") (?\; . "ん")
    (?z . "．") (?x . "ひ") (?c . "す") (?v . "ふ") (?b . "へ")
    (?n . "め") (?m . "そ") (?, . "ね") (?. . "ほ") (?/ . "・")
    (?H . "ぱ") (?X . "ぴ") (?V . "ぷ") (?B . "ぺ") (?> . "ぽ")) "\
単独打鍵時の入力ルール。")

(defvar omelet-us-rshift-rule-list
  '((?1 . "！") (?2 . "“") (?3 . "”") (?4 . "＃") (?5 . "％")
    (?6 . "［") (?7 . "］") (?8 . "（") (?9 . "）") (?0 . "『")
    (?- . "』") (?^ . "‘") (?\\ . "’")
    (?q . "ゐ") (?w . "が") (?e . "だ") (?r . "ご") (?t . "ざ")
    (?y . "よ") (?u . "に") (?i . "る") (?o . "ま") (?p . "ぇ")
    (?@ . "＠") (?\[ . "゜")
    (?a . "ヴ") (?s . "じ") (?d . "で") (?f . "げ") (?g . "ぜ")
    (?h . "み") (?j . "お") (?k . "の") (?l . "ょ") (?\; . "っ")
    (?z . "ゑ") (?x . "び") (?c . "ず") (?v . "ぶ") (?b . "べ")
    (?n . "ぬ") (?m . "ゆ") (?, . "む") (?. . "わ") (?/ . "ぉ")) "\
右親指キーが押されたときの入力ルール。")

(defvar omelet-us-lshift-rule-list
  '((?1 . "？") (?2 . "／") (?3 . "〜") (?4 . "「") (?5 . "」")
    (?6 . "［") (?7 . "］") (?8 . "【") (?9 . "】") (?0 . "｛")
    (?- . "｝") (?^ . "＝") (?\\ . "＿")
    (?q . "ぁ") (?w . "え") (?e . "り") (?r . "ゃ") (?t . "れ")
    (?y . "ぱ") (?u . "ぢ") (?i . "ぐ") (?o . "づ") (?p . "ぴ")
    (?a . "を") (?s . "あ") (?d . "な") (?f . "ゅ") (?g . "も")
    (?h . "ば") (?j . "ど") (?k . "ぎ") (?l . "ぽ") (?\; . "ヶ")
    (?z . "ぅ") (?x . "ー") (?c . "ろ") (?v . "や") (?b . "ぃ")
    (?n . "ぷ") (?m . "ぞ") (?, . "ぺ") (?. . "ぼ") (?/ . "ゎ")) "\
左親指キーが押されたときの入力ルール。")

(defvar omelet-plain-rule-list nil)
(defvar omelet-rshift-rule-list nil)
(defvar omelet-lshift-rule-list nil)

(defun omelet-version ()
  "Display Version"
  (interactive)
  (message "omelet version: %s" omelet-version))

(defun omelet-event-to-key (event)
  "EVENT を発生するキーを取得する。"
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

      ;; キーボードをセットアップする
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


