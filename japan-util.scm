;;; Hiragana-Katakana, Zenkaku-Hankaku conversion on selection or clipboard.
;;;
;;; Copyright (c) 2011 KIHARA Hideto https://github.com/deton/uim-japan-util
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

(require-extension (srfi 1 2 8))
(require-custom "japan-util-custom.scm")
(require "japanese.scm")
(require "key.scm")

;; reverse rule of ja-wide-rule. (excludes rule ("~" "〜"))
(define japan-util-wide-to-ascii-rule
  (filter-map
    (lambda (x)
      (let ((ascii (car x)))
        (and (not (string=? ascii "~"))
             (list (cadr x) ascii))))
    ja-wide-rule))

;; convert wide alphabet to ascii
;; (opposite of ja-wide in japanese.scm)
(define (japan-util-wide-to-ascii c)
  (cond ((assoc c japan-util-wide-to-ascii-rule) => cadr)
        (else c)))

(define japan-util-halfkana-rule
  '(("｡" "。")
    ("｢" "「")
    ("｣" "」")
    ("､" "、")
    ("･" "・")
    ("ｦ" "ヲ")
    ("ｧ" "ァ")
    ("ｨ" "ィ")
    ("ｩ" "ゥ")
    ("ｪ" "ェ")
    ("ｫ" "ォ")
    ("ｬ" "ャ")
    ("ｭ" "ュ")
    ("ｮ" "ョ")
    ("ｯ" "ッ")
    ("ｰ" "ー")
    ("ｱ" "ア")
    ("ｲ" "イ")
    ("ｳ" "ウ")
    ("ｴ" "エ")
    ("ｵ" "オ")
    ("ｶ" "カ")
    ("ｷ" "キ")
    ("ｸ" "ク")
    ("ｹ" "ケ")
    ("ｺ" "コ")
    ("ｻ" "サ")
    ("ｼ" "シ")
    ("ｽ" "ス")
    ("ｾ" "セ")
    ("ｿ" "ソ")
    ("ﾀ" "タ")
    ("ﾁ" "チ")
    ("ﾂ" "ツ")
    ("ﾃ" "テ")
    ("ﾄ" "ト")
    ("ﾅ" "ナ")
    ("ﾆ" "ニ")
    ("ﾇ" "ヌ")
    ("ﾈ" "ネ")
    ("ﾉ" "ノ")
    ("ﾊ" "ハ")
    ("ﾋ" "ヒ")
    ("ﾌ" "フ")
    ("ﾍ" "ヘ")
    ("ﾎ" "ホ")
    ("ﾏ" "マ")
    ("ﾐ" "ミ")
    ("ﾑ" "ム")
    ("ﾒ" "メ")
    ("ﾓ" "モ")
    ("ﾔ" "ヤ")
    ("ﾕ" "ユ")
    ("ﾖ" "ヨ")
    ("ﾗ" "ラ")
    ("ﾘ" "リ")
    ("ﾙ" "ル")
    ("ﾚ" "レ")
    ("ﾛ" "ロ")
    ("ﾜ" "ワ")
    ("ﾝ" "ン")
    ("ﾞ" "゛")
    ("ﾟ" "゜")
    ;; for conversion from fullwidth to halfwidth
    ("ｶﾞ" "ガ")
    ("ｷﾞ" "ギ")
    ("ｸﾞ" "グ")
    ("ｹﾞ" "ゲ")
    ("ｺﾞ" "ゴ")
    ("ｻﾞ" "ザ")
    ("ｼﾞ" "ジ")
    ("ｽﾞ" "ズ")
    ("ｾﾞ" "ゼ")
    ("ｿﾞ" "ゾ")
    ("ﾀﾞ" "ダ")
    ("ﾁﾞ" "ヂ")
    ("ﾂﾞ" "ヅ")
    ("ﾃﾞ" "デ")
    ("ﾄﾞ" "ド")
    ("ﾊﾞ" "バ")
    ("ﾊﾟ" "パ")
    ("ﾋﾞ" "ビ")
    ("ﾋﾟ" "ピ")
    ("ﾌﾞ" "ブ")
    ("ﾌﾟ" "プ")
    ("ﾍﾞ" "ベ")
    ("ﾍﾟ" "ペ")
    ("ﾎﾞ" "ボ")
    ("ﾎﾟ" "ポ")
    ("ｳﾞ" "ヴ")))

(define (japan-util-halfkana-to-fullkana c)
  (cond ((assoc c japan-util-halfkana-rule) => cadr)
        (else c)))

(define (japan-util-fullkana-to-halfkana c)
  (cond ((find (lambda (x) (string=? (cadr x) c)) japan-util-halfkana-rule)
          => car)
        ((string=? c "　") ; fullwidth space
          " ")
        (else c)))

(define japan-util-kana-rule
  '(("ぁ" "ァ")
    ("あ" "ア")
    ("ぃ" "ィ")
    ("い" "イ")
    ("ぅ" "ゥ")
    ("う" "ウ")
    ("ぇ" "ェ")
    ("え" "エ")
    ("ぉ" "ォ")
    ("お" "オ")
    ("か" "カ")
    ("が" "ガ")
    ("き" "キ")
    ("ぎ" "ギ")
    ("く" "ク")
    ("ぐ" "グ")
    ("け" "ケ")
    ("げ" "ゲ")
    ("こ" "コ")
    ("ご" "ゴ")
    ("さ" "サ")
    ("ざ" "ザ")
    ("し" "シ")
    ("じ" "ジ")
    ("す" "ス")
    ("ず" "ズ")
    ("せ" "セ")
    ("ぜ" "ゼ")
    ("そ" "ソ")
    ("ぞ" "ゾ")
    ("た" "タ")
    ("だ" "ダ")
    ("ち" "チ")
    ("ぢ" "ヂ")
    ("っ" "ッ")
    ("つ" "ツ")
    ("づ" "ヅ")
    ("て" "テ")
    ("で" "デ")
    ("と" "ト")
    ("ど" "ド")
    ("な" "ナ")
    ("に" "ニ")
    ("ぬ" "ヌ")
    ("ね" "ネ")
    ("の" "ノ")
    ("は" "ハ")
    ("ば" "バ")
    ("ぱ" "パ")
    ("ひ" "ヒ")
    ("び" "ビ")
    ("ぴ" "ピ")
    ("ふ" "フ")
    ("ぶ" "ブ")
    ("ぷ" "プ")
    ("へ" "ヘ")
    ("べ" "ベ")
    ("ぺ" "ペ")
    ("ほ" "ホ")
    ("ぼ" "ボ")
    ("ぽ" "ポ")
    ("ま" "マ")
    ("み" "ミ")
    ("む" "ム")
    ("め" "メ")
    ("も" "モ")
    ("ゃ" "ャ")
    ("や" "ヤ")
    ("ゅ" "ュ")
    ("ゆ" "ユ")
    ("ょ" "ョ")
    ("よ" "ヨ")
    ("ら" "ラ")
    ("り" "リ")
    ("る" "ル")
    ("れ" "レ")
    ("ろ" "ロ")
    ("ゎ" "ヮ")
    ("わ" "ワ")
    ("ゐ" "ヰ")
    ("ゑ" "ヱ")
    ("を" "ヲ")
    ("ん" "ン")
    ("う゛" "ヴ")))

(define (japan-util-hiragana-to-katakana c)
  (cond ((assoc c japan-util-kana-rule) => cadr)
        (else c)))

(define (japan-util-katakana-to-hiragana c)
  (cond ((find (lambda (x) (string=? (cadr x) c)) japan-util-kana-rule) => car)
        (else c)))

(define japan-util-dakuten-chars-alist
  '(("か" "が")
    ("き" "ぎ")
    ("く" "ぐ")
    ("け" "げ")
    ("こ" "ご")
    ("さ" "ざ")
    ("し" "じ")
    ("す" "ず")
    ("せ" "ぜ")
    ("そ" "ぞ")
    ("た" "だ")
    ("ち" "ぢ")
    ("つ" "づ")
    ("て" "で")
    ("と" "ど")
    ("は" "ば")
    ("ひ" "び")
    ("ふ" "ぶ")
    ("へ" "べ")
    ("ほ" "ぼ")
    ("カ" "ガ")
    ("キ" "ギ")
    ("ク" "グ")
    ("ケ" "ゲ")
    ("コ" "ゴ")
    ("サ" "ザ")
    ("シ" "ジ")
    ("ス" "ズ")
    ("セ" "ゼ")
    ("ソ" "ゾ")
    ("タ" "ダ")
    ("チ" "ヂ")
    ("ツ" "ヅ")
    ("テ" "デ")
    ("ト" "ド")
    ("ハ" "バ")
    ("ヒ" "ビ")
    ("フ" "ブ")
    ("ヘ" "ベ")
    ("ホ" "ボ")
    ("ウ" "ヴ")))

(define japan-util-handakuten-chars-alist
  '(("は" "ぱ")
    ("ひ" "ぴ")
    ("ふ" "ぷ")
    ("へ" "ぺ")
    ("ほ" "ぽ")
    ("ハ" "パ")
    ("ヒ" "ピ")
    ("フ" "プ")
    ("ヘ" "ペ")
    ("ホ" "ポ")))

(define japan-util-context-rec-spec
  (append
    context-rec-spec
    (list
      (list 'undo-len 0)
      (list 'undo-str #f))))
(define-record 'japan-util-context japan-util-context-rec-spec)
(define japan-util-context-new-internal japan-util-context-new)

(define japan-util-context-new
  (lambda args
    (let ((pc (apply japan-util-context-new-internal args)))
      pc)))

(define japan-util-init-handler
  (lambda (id im arg)
    (let ((pc (japan-util-context-new id im)))
      pc)))

(define (japan-util-release-handler pc)
  (im-deactivate-candidate-selector pc))

(define (japan-util-key-press-handler pc key key-state)
  (im-deactivate-candidate-selector pc)
  (if (ichar-control? key)
    (begin
      (japan-util-context-set-undo-str! pc #f)
      (im-commit-raw pc))
    (cond
      ((japan-util-show-help-key? key key-state)
        (japan-util-show-help pc))
      ((japan-util-hiragana-selection-key? key key-state)
        (japan-util-hiragana-selection pc))
      ((japan-util-hiragana-clipboard-key? key key-state)
        (japan-util-hiragana-clipboard pc))
      ((japan-util-katakana-selection-key? key key-state)
        (japan-util-katakana-selection pc))
      ((japan-util-katakana-clipboard-key? key key-state)
        (japan-util-katakana-clipboard pc))
      ((japan-util-ascii-fullkana-selection-key? key key-state)
        (japan-util-ascii-fullkana-selection pc))
      ((japan-util-ascii-fullkana-clipboard-key? key key-state)
        (japan-util-ascii-fullkana-clipboard pc))
      ((japan-util-ascii-selection-key? key key-state)
        (japan-util-ascii-selection pc))
      ((japan-util-ascii-clipboard-key? key key-state)
        (japan-util-ascii-clipboard pc))
      ((japan-util-wide-selection-key? key key-state)
        (japan-util-wide-selection pc))
      ((japan-util-wide-clipboard-key? key key-state)
        (japan-util-wide-clipboard pc))
      ((japan-util-fullwidth-katakana-selection-key? key key-state)
        (japan-util-fullwidth-katakana-selection pc))
      ((japan-util-fullwidth-katakana-clipboard-key? key key-state)
        (japan-util-fullwidth-katakana-clipboard pc))
      ((japan-util-halfwidth-katakana-selection-key? key key-state)
        (japan-util-halfwidth-katakana-selection pc))
      ((japan-util-halfwidth-katakana-clipboard-key? key key-state)
        (japan-util-halfwidth-katakana-clipboard pc))
      ((japan-util-undo-key? key key-state)
        (japan-util-undo pc)
        (japan-util-context-set-undo-str! pc #f))
      (else
        (japan-util-context-set-undo-str! pc #f)
        (im-commit-raw pc)))))

(define (japan-util-key-release-handler pc key state)
  (im-commit-raw pc))

(define (japan-util-get-candidate-handler pc idx accel-enum-hint)
  (define (get-label custom-key)
    (if (null? custom-key)
      ""
      ;; (problem if require "custom.scm" for key-str->gui-key-str)
      (let* ((key-str (car custom-key)) ; only check first key
             (parsed (parse-key-str key-str '() -1 0))
             (translated (apply apply-translators (cdr parsed)))
             (target-key (list-ref translated 1))
             (target-state (list-ref translated 2)))
        (charcode->string
          (if (shift-key-mask target-state)
            (ichar-upcase target-key)
            target-key)))))
  (case idx
    ((0) (list "->katakana" (get-label japan-util-katakana-selection-key) ""))
    ((1) (list "->hiragana" (get-label japan-util-hiragana-selection-key) ""))
    ((2) (list "->ascii+fullwidth katakana"
          (get-label japan-util-ascii-fullkana-selection-key) ""))
    ((3) (list "->ascii" (get-label japan-util-ascii-selection-key) ""))
    ((4) (list "->wide" (get-label japan-util-wide-selection-key) ""))
    ((5) (list "->fullwidth katakana"
          (get-label japan-util-fullwidth-katakana-selection-key) ""))
    ((6) (list "->halfwidth katakana"
          (get-label japan-util-halfwidth-katakana-selection-key) ""))
    ((7) (list "undo last commit"
          (get-label japan-util-undo-key) ""))))

(define (japan-util-set-candidate-index-handler pc idx)
  (case idx
    ((0) (japan-util-katakana-selection pc))
    ((1) (japan-util-hiragana-selection pc))
    ((2) (japan-util-ascii-fullkana-selection pc))
    ((3) (japan-util-ascii-selection pc))
    ((4) (japan-util-wide-selection pc))
    ((5) (japan-util-fullwidth-katakana-selection pc))
    ((6) (japan-util-halfwidth-katakana-selection pc))
    ((7) (japan-util-undo pc)))
  (im-deactivate-candidate-selector pc))

(register-im
 'japan-util
 "ja"
 "EUC-JP"
 japan-util-im-name-label
 japan-util-im-short-desc
 #f
 japan-util-init-handler
 japan-util-release-handler
 context-mode-handler
 japan-util-key-press-handler
 japan-util-key-release-handler
 #f
 japan-util-get-candidate-handler
 japan-util-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 #f
 #f
 #f
 )

(define (japan-util-show-help pc)
  (im-activate-candidate-selector pc 8 8)
  (im-select-candidate pc 0)) ; to select candidate by click

(define (japan-util-undo pc)
  (let ((str (japan-util-context-undo-str pc))
        (len (japan-util-context-undo-len pc)))
    (if str
      (begin
        (if (> len 0)
          (im-delete-text pc 'primary 'cursor len 0))
        (if (not (string=? str ""))
          (im-commit pc str))))))

(define (japan-util-acquire-text pc id)
  (and-let*
    ((ustr (im-acquire-text pc id 'beginning 0 'full))
     (latter (ustr-latter-seq ustr)))
    (and (pair? latter)
         (car latter))))

(define (japan-util-convert pc id convert)
  (let ((str (japan-util-acquire-text pc id)))
    (if (string? str)
      (let* ((convstr-list (convert (string-to-list str)))
             (convstr (string-list-concat convstr-list)))
        (if (or (eq? id 'clipboard)
                ;; for selection, avoid to unselect if there is no change.
                (not (string=? convstr str)))
          (begin
            (japan-util-context-set-undo-len! pc (length convstr-list))
            (japan-util-context-set-undo-str! pc
              (if (eq? id 'clipboard)
                ""
                str))
            (im-commit pc convstr)))))))

(define (japan-util-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (map japan-util-hiragana-to-katakana (ja-join-vu str-list)))))

(define (japan-util-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (map japan-util-hiragana-to-katakana (ja-join-vu str-list)))))

(define (japan-util-hiragana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (map japan-util-katakana-to-hiragana str-list))))

(define (japan-util-hiragana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (map japan-util-katakana-to-hiragana str-list))))

(define (japan-util-halfwidth-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (map japan-util-fullkana-to-halfkana str-list))))

(define (japan-util-halfwidth-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (map japan-util-fullkana-to-halfkana str-list))))

(define (japan-util-ascii-fullkana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (japan-util-ascii-convert
        (japan-util-halfkana-to-fullkana-convert str-list)))))

(define (japan-util-ascii-fullkana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (japan-util-ascii-convert
        (japan-util-halfkana-to-fullkana-convert str-list)))))

(define (japan-util-fullwidth-katakana-selection pc)
  (japan-util-convert pc 'selection japan-util-halfkana-to-fullkana-convert))

(define (japan-util-fullwidth-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard japan-util-halfkana-to-fullkana-convert))

(define (japan-util-ascii-selection pc)
  (japan-util-convert pc 'selection japan-util-ascii-convert))

(define (japan-util-ascii-clipboard pc)
  (japan-util-convert pc 'clipboard japan-util-ascii-convert))

;; convert wide alphabets in string list to ascii alphabets.
;; (cf. ja-string-list-to-wide-alphabet in japanese.scm)
(define (japan-util-ascii-convert str-list)
  (map japan-util-wide-to-ascii str-list))

(define (japan-util-wide-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (map ja-wide
        (japan-util-halfkana-to-fullkana-convert str-list)))))

(define (japan-util-wide-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (map ja-wide
        (japan-util-halfkana-to-fullkana-convert str-list)))))

(define (japan-util-halfkana-to-fullkana-convert str-list)
  ;; XXX: currently, join all dakuten not only for halfkana
  (japan-util-join-dakuten '()
    (map japan-util-halfkana-to-fullkana str-list)))

;; revise string list contains Dakuten "゛" or Han-Dakuten "゜"
;; (("゛") ("ウ")) -> ("ヴ")
(define (japan-util-join-dakuten res-lst lst)
  (receive
    (head tail)
    (break
      (lambda (x)
        (or (string=? x "゛") (string=? x "゜")))
      lst)
    (if (null? tail)
      (append res-lst head)
      (let*
        ((c (car tail))
         (next-c (and (pair? (cdr tail)) (cadr tail)))
         (joined-c
          (cond
            ((and (string=? c "゛")
                  next-c
                  (assoc next-c japan-util-dakuten-chars-alist))
              => cadr)
            ((and (string=? c "゜")
                  next-c
                  (assoc next-c japan-util-handakuten-chars-alist))
              => cadr)
            (else #f))))
        (if joined-c
          (japan-util-join-dakuten
            (append res-lst head (list joined-c))
            (cddr tail))
          (japan-util-join-dakuten
            (append res-lst head (list c))
            (cdr tail)))))))
