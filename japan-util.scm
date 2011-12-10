;;; Hiragana-Katakana, Zenkaku-Hankaku conversion on selection or clipboard.

(require-extension (srfi 1 2))
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
      (let ((converted-str (convert str)))
        (if (not (string=? converted-str str))
          (im-commit pc converted-str))))))

(define (japan-util-hiragana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str)
      (japan-util-kana-convert str ja-type-hiragana))))

(define (japan-util-hiragana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str)
      (japan-util-kana-convert str ja-type-hiragana))))

(define (japan-util-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str)
      (japan-util-kana-convert str ja-type-katakana))))

(define (japan-util-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str)
      (japan-util-kana-convert str ja-type-katakana))))

(define (japan-util-halfwidth-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str)
      ;; XXX: exclude ascii symbols in ja-rk-rule-basic
      (japan-util-kana-convert str ja-type-halfkana))))

(define (japan-util-halfwidth-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str)
      ;; XXX: exclude ascii symbols in ja-rk-rule-basic
      (japan-util-kana-convert str ja-type-halfkana))))

(define (japan-util-kana-convert str idx)
  (string-list-concat
    (map
      (lambda (e)
        (let ((ch (list-ref (ja-find-kana-list-from-rule ja-rk-rule e) idx)))
          (if (string=? ch "")
            e ; avoid to convert to "" (ex. "zk" in ja-rk-rule-basic)
            ch)))
      (string-to-list str))))

(define (japan-util-fullwidth-katakana-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str)
      (string-list-concat
        (japan-util-halfkana-to-fullkana-convert str)))))

(define (japan-util-fullwidth-katakana-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str)
      (string-list-concat
        (japan-util-halfkana-to-fullkana-convert str)))))

(define (japan-util-wide-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str)
      (ja-string-list-to-wide-alphabet
        (japan-util-halfkana-to-fullkana-convert str)))))

(define (japan-util-wide-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str)
      (ja-string-list-to-wide-alphabet
        (japan-util-halfkana-to-fullkana-convert str)))))

(define (japan-util-halfkana-to-fullkana-convert str)
  ;; TODO: support Dakuten and Handakuten
  (map
    (lambda (e)
      (japan-util-halfkana-to-fullkana e))
    (string-to-list str)))

(define (japan-util-ascii-selection pc)
  (japan-util-convert pc 'selection
    (lambda (str)
      (japan-util-ascii-convert str))))

(define (japan-util-ascii-clipboard pc)
  (japan-util-convert pc 'clipboard
    (lambda (str)
      (japan-util-ascii-convert str))))

(define (japan-util-ascii-convert str)
  ;; convert wide alphabets in string list to ascii alphabets.
  ;; (cf. ja-string-list-to-wide-alphabet in japanese.scm)
  (define (ja-string-list-to-ascii res-list char-list)
    (if (null? char-list)
      res-list
      (ja-string-list-to-ascii
        (cons (japan-util-wide-to-ascii (car char-list)) res-list)
        (cdr char-list))))
  (apply string-append
    (ja-string-list-to-ascii '() (string-to-list str))))
