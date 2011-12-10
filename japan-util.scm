;;; Hiragana-Katakana, Zenkaku-Hankaku conversion on selection or clipboard.

(require-extension (srfi 1 2 8))
(require-custom "japan-util-custom.scm")
(require "japanese.scm")
(require "key.scm")

;; reverse rule of ja-wide-rule. (excludes "￥" to "yen" rule)
(define japan-util-wide-to-ascii-rule
  (filter-map
    (lambda (x)
      (let ((ascii (car x)))
        (and (not (string=? ascii "yen"))
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

(define japan-util-context-rec-spec context-rec-spec)
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
    (im-commit-raw pc)
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
      ((japan-util-wide-selection-key? key key-state)
        (japan-util-wide-selection pc))
      ((japan-util-wide-clipboard-key? key key-state)
        (japan-util-wide-clipboard pc))
      ((japan-util-ascii-selection-key? key key-state)
        (japan-util-ascii-selection pc))
      ((japan-util-ascii-clipboard-key? key key-state)
        (japan-util-ascii-clipboard pc))
      ((japan-util-fullwidth-katakana-selection-key? key key-state)
        (japan-util-fullwidth-katakana-selection pc))
      ((japan-util-fullwidth-katakana-clipboard-key? key key-state)
        (japan-util-fullwidth-katakana-clipboard pc))
      ((japan-util-halfwidth-katakana-selection-key? key key-state)
        (japan-util-halfwidth-katakana-selection pc))
      ((japan-util-halfwidth-katakana-clipboard-key? key key-state)
        (japan-util-halfwidth-katakana-clipboard pc))
      (else
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
    ((0) (list "->hiragana" (get-label japan-util-hiragana-selection-key) ""))
    ((1) (list "->katakana" (get-label japan-util-katakana-selection-key) ""))
    ((2) (list "->wide" (get-label japan-util-wide-selection-key) ""))
    ((3) (list "->ascii" (get-label japan-util-ascii-selection-key) ""))
    ((4) (list "->fullwidth-katakana"
          (get-label japan-util-fullwidth-katakana-selection-key) ""))
    ((5) (list "->halfwidth-katakana"
          (get-label japan-util-halfwidth-katakana-selection-key) ""))))

(define (japan-util-set-candidate-index-handler pc idx)
  (case idx
    ((0) (japan-util-hiragana-selection pc))
    ((1) (japan-util-katakana-selection pc))
    ((2) (japan-util-wide-selection pc))
    ((3) (japan-util-ascii-selection pc))
    ((4) (japan-util-fullwidth-katakana-selection pc))
    ((5) (japan-util-halfwidth-katakana-selection pc)))
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
  (im-activate-candidate-selector pc 6 6)
  (im-select-candidate pc 0)) ; to select candidate by click

(define (japan-util-acquire-text pc id)
  (and-let*
    ((ustr (im-acquire-text pc id 'beginning 0 'full))
     (latter (ustr-latter-seq ustr)))
    (and (pair? latter)
         (car latter))))

(define (japan-util-convert pc id convert)
  (let ((str (japan-util-acquire-text pc id)))
    (if (string? str)
      (let ((converted-str (string-list-concat (convert (string-to-list str)))))
        (if (not (string=? converted-str str))
          (im-commit pc converted-str))))))

(define (japan-util-hiragana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (map japan-util-katakana-to-hiragana str-list))))

(define (japan-util-hiragana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (map japan-util-katakana-to-hiragana str-list))))

(define (japan-util-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (map japan-util-hiragana-to-katakana (ja-join-vu str-list)))))

(define (japan-util-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (map japan-util-hiragana-to-katakana (ja-join-vu str-list)))))

(define (japan-util-halfwidth-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (map japan-util-fullkana-to-halfkana str-list))))

(define (japan-util-halfwidth-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (map japan-util-fullkana-to-halfkana str-list))))

(define (japan-util-fullwidth-katakana-selection pc)
  (japan-util-convert pc 'selection japan-util-halfkana-to-fullkana-convert))

(define (japan-util-fullwidth-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard japan-util-halfkana-to-fullkana-convert))

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
  (japan-util-join-dakuten
    (map japan-util-halfkana-to-fullkana str-list)))

;; revise string list contains Dakuten "゛" or Han-Dakuten "゜"
;; (("゛") ("ウ")) -> ("ヴ")
(define (japan-util-join-dakuten lst)
  (receive
    (head tail)
    (break
      (lambda (x)
        (or (string=? x "゛") (string=? x "゜")))
      lst)
    (if (null? tail)
      head
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
          (append head (list joined-c) (japan-util-join-dakuten (cddr tail)))
          (append head (list c) (japan-util-join-dakuten (cdr tail))))))))

(define (japan-util-ascii-selection pc)
  (japan-util-convert pc 'selection japan-util-ascii-convert))

(define (japan-util-ascii-clipboard pc)
  (japan-util-convert pc 'clipboard japan-util-ascii-convert))

;; convert wide alphabets in string list to ascii alphabets.
;; (cf. ja-string-list-to-wide-alphabet in japanese.scm)
(define (japan-util-ascii-convert str-list)
  (map japan-util-wide-to-ascii str-list))
