;;; Hiragana-Katakana, Zenkaku-Hankaku conversion on selection or clipboard.

(require-extension (srfi 1 2))
(require-custom "japan-util-custom.scm")
(require "japanese.scm")

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
      ((japan-util-zenkaku-selection-key? key key-state)
        (japan-util-zenkaku-selection pc))
      ((japan-util-zenkaku-clipboard-key? key key-state)
        (japan-util-zenkaku-clipboard pc))
      ((japan-util-ascii-selection-key? key key-state)
        (japan-util-ascii-selection pc))
      ((japan-util-ascii-clipboard-key? key key-state)
        (japan-util-ascii-clipboard pc))
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
    ((2) (list "->zenkaku" "z" ""))
    ((3) (list "->ascii" "a" ""))
    ((4) (list "->halfwidth-katakana" "x" ""))))

(define (japan-util-set-candidate-index-handler pc idx)
  (case idx
    ((0) (japan-util-hiragana-selection pc))
    ((1) (japan-util-katakana-selection pc))
    ((2) (japan-util-zenkaku-selection pc))
    ((3) (japan-util-ascii-selection pc))
    ((4) (japan-util-halfwidth-katakana-selection pc))))

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
  (im-activate-candidate-selector pc 5 5))

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

(define (japan-util-zenkaku-selection pc)
  #f)
(define (japan-util-zenkaku-clipboard pc)
  #f)
(define (japan-util-ascii-selection pc)
  #f)
(define (japan-util-ascii-clipboard pc)
  #f)
(define (japan-util-halfwidth-katakana-selection pc)
  #f)
(define (japan-util-halfwidth-katakana-clipboard pc)
  #f)

(define (japan-util-kana-convert str idx)
  (string-list-concat
    (map
      (lambda (e)
        (list-ref (ja-find-kana-list-from-rule ja-rk-rule e) idx))
      (string-to-list str))))
