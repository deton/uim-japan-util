(require "i18n.scm")

(define japan-util-im-name-label (N_ "japan-util"))
(define japan-util-im-short-desc
  (N_ "Hiragana-Katakana, Zenkaku-Hankaku conversion on selection or clipboard"))

(define-custom-group 'japan-util
                     japan-util-im-name-label
                     japan-util-im-short-desc)

(define-custom 'japan-util-show-help-key '("<IgnoreShift>?")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] show help")
	       (N_ "long description will be here"))

(define-custom 'japan-util-katakana-selection-key '("k")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] katakana selection")
	       (N_ "long description will be here"))

(define-custom 'japan-util-katakana-clipboard-key '("<IgnoreCase><Shift>k")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] katakana clipboard")
	       (N_ "long description will be here"))

(define-custom 'japan-util-hiragana-selection-key '("h")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] hiragana selection")
	       (N_ "long description will be here"))

(define-custom 'japan-util-hiragana-clipboard-key '("<IgnoreCase><Shift>h")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] hiragana clipboard")
	       (N_ "long description will be here"))

(define-custom 'japan-util-ascii-fullkana-selection-key '("n")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] ascii and fullwidth katakana selection")
	       (N_ "long description will be here"))

(define-custom 'japan-util-ascii-fullkana-clipboard-key '("<IgnoreCase><Shift>n")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] ascii and fullwidth katakana clipboard")
	       (N_ "long description will be here"))

(define-custom 'japan-util-ascii-selection-key '("a")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] ascii selection")
	       (N_ "long description will be here"))

(define-custom 'japan-util-ascii-clipboard-key '("<IgnoreCase><Shift>a")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] ascii clipboard")
	       (N_ "long description will be here"))

(define-custom 'japan-util-wide-selection-key '("w")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] wide selection")
	       (N_ "long description will be here"))

(define-custom 'japan-util-wide-clipboard-key '("<IgnoreCase><Shift>w")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] wide clipboard")
	       (N_ "long description will be here"))

(define-custom 'japan-util-fullwidth-katakana-selection-key '("z")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] fullwidth katakana selection")
	       (N_ "long description will be here"))

(define-custom 'japan-util-fullwidth-katakana-clipboard-key '("<IgnoreCase><Shift>z")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] fullwidth katakana clipboard")
	       (N_ "long description will be here"))

(define-custom 'japan-util-halfwidth-katakana-selection-key '("x")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] halfwidth katakana selection")
	       (N_ "long description will be here"))

(define-custom 'japan-util-halfwidth-katakana-clipboard-key '("<IgnoreCase><Shift>x")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] halfwidth katakana clipboard")
	       (N_ "long description will be here"))

(define-custom 'japan-util-undo-key '("u")
               '(japan-util)
	       '(key)
	       (N_ "[japan-util] undo last commit")
	       (N_ "long description will be here"))
