;;; Hiragana-Katakana, Zenkaku-Hankaku conversion on selection or clipboard.

(require-extension (srfi 1 2 8))
(require-custom "japan-util-custom.scm")
(require "japanese.scm")

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

(define japan-util-halfkana-to-fullkana-rule
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
    ("ﾟ" "゜")))

(define (japan-util-halfkana-to-fullkana c)
  (cond ((assoc c japan-util-halfkana-to-fullkana-rule) => cadr)
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
  (case idx
    ((0) (list "->hiragana" "h" ""))
    ((1) (list "->katakana" "k" ""))
    ((2) (list "->wide" "w" ""))
    ((3) (list "->ascii" "a" ""))
    ((4) (list "->fullwidth-katakana" "z" ""))
    ((5) (list "->halfwidth-katakana" "x" ""))))

(define (japan-util-set-candidate-index-handler pc idx)
  (case idx
    ((0) (japan-util-hiragana-selection pc))
    ((1) (japan-util-katakana-selection pc))
    ((2) (japan-util-wide-selection pc))
    ((3) (japan-util-ascii-selection pc))
    ((4) (japan-util-fullwidth-katakana-selection pc))
    ((5) (japan-util-halfwidth-katakana-selection pc))))

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
  (im-activate-candidate-selector pc 6 6))

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
      (japan-util-kana-convert str-list ja-type-hiragana))))

(define (japan-util-hiragana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (japan-util-kana-convert str-list ja-type-hiragana))))

(define (japan-util-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      (japan-util-kana-convert str-list ja-type-katakana))))

(define (japan-util-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      (japan-util-kana-convert str-list ja-type-katakana))))

(define (japan-util-halfwidth-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str-list)
      ;; XXX: exclude ascii symbols in ja-rk-rule-basic
      (japan-util-kana-convert str-list ja-type-halfkana))))

(define (japan-util-halfwidth-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str-list)
      ;; XXX: exclude ascii symbols in ja-rk-rule-basic
      (japan-util-kana-convert str-list ja-type-halfkana))))

(define (japan-util-kana-convert str-list idx)
  (map
    (lambda (e)
      (let ((ch (list-ref (ja-find-kana-list-from-rule ja-rk-rule e) idx)))
        (if (string=? ch "")
          e ; avoid to convert to "" (ex. "zk" in ja-rk-rule-basic)
          ch)))
    str-list))

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
  ;; join all dakuten not only for halfkana
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
  (japan-util-convert pc 'selection japan-util-string-list-to-ascii))

(define (japan-util-ascii-clipboard pc)
  (japan-util-convert pc 'clipboard japan-util-string-list-to-ascii))

;; convert wide alphabets in string list to ascii alphabets.
;; (cf. ja-string-list-to-wide-alphabet in japanese.scm)
(define (japan-util-string-list-to-ascii str-list)
  (map japan-util-wide-to-ascii str-list))
