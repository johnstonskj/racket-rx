#lang racket/base

(require racket/bool
         racket/contract
         racket/string)

(provide (contract-out
          (named-group-form/c flat-contract?)
          (rx/named-group-form (parameter/c named-group-form/c named-group-form/c))
          (group-ref-form/c flat-contract?)
          (rx/group-ref-form (parameter/c group-ref-form/c group-ref-form/c))
          (rx/has-named-group-refs (parameter/c boolean? boolean?))
          (rx/has-relative-group-refs (parameter/c boolean? boolean?))
          (stringlike/c flat-contract?)
          (rx/and (-> stringlike/c stringlike/c ... string?))
          (rx/or (-> stringlike/c stringlike/c ... string?))
          (group-repeat/c flat-contract?)
          (rx/repeat-symbol (-> group-repeat/c string?))
          (rx/repeat-bounds (-> group-repeat/c string?))
          (rx/optional (-> stringlike/c string?))
          (rx/zero-or-more (-> stringlike/c string?))
          (rx/one-or-more (-> stringlike/c string?))
          (rx/repeat-safe? (-> stringlike/c boolean?))
          (rx/repeat
           (->* (stringlike/c)
                (#:lower (or/c exact-nonnegative-integer? #f)
                 #:upper (or/c exact-nonnegative-integer? #f))
                string?))
          (rx/repeat-safely
           (->* (stringlike/c)
                (#:lower (or/c exact-nonnegative-integer? #f)
                 #:upper (or/c exact-nonnegative-integer? #f))
                string?))
          (char>=/c contract?)
          (rx/range (->i ((from char?) (to (from) (and/c char? (char>=/c from)))) (result string?)))
          (rx/ranges (-> (cons/c char? char?) (cons/c char? char?) ... string?))
          (rx/range? (-> string? boolean?))
          (rx/match
           (->* (stringlike/c)
                (#:repeat group-repeat/c)
                #:rest (listof stringlike/c)
                string?))
          (rx/match? (-> string? boolean?))
          (rx/not-match
           (->* (stringlike/c)
                (#:repeat group-repeat/c)
                #:rest (listof stringlike/c)string?))
          (rx/not-match? (-> string? boolean?))
          (rx/escape (->* (stringlike/c) ((or/c 'in-match 'outside-match)) string?))
          (rx/string-prefix (-> stringlike/c string?))
          (rx/string-suffix (-> stringlike/c string?))
          (rx/group
           (->* (stringlike/c)
                (#:repeat group-repeat/c)
                #:rest (listof stringlike/c)
                string?))
          (rx/group? (-> string? boolean?))
          (rx/or-group
           (->* (stringlike/c)
                (#:repeat group-repeat/c)
                #:rest (listof stringlike/c)
                string?))
          (rx/and-group
           (->* (stringlike/c)
                (#:repeat group-repeat/c)
                #:rest (listof stringlike/c)
                string?))
          (rx/special-group
           (->* (stringlike/c stringlike/c)
                (#:repeat group-repeat/c)
                #:rest (listof stringlike/c)
                string?))
          (rx/special-group? (-> string? boolean?))
          (rx/named-group
           (->* (stringlike/c stringlike/c #:name stringlike/c)
                (#:repeat group-repeat/c)
                #:rest (listof stringlike/c)
                string?))
          (rx/named-group? (-> string? boolean?))
          (rx/non-capture-group
           (->* (stringlike/c stringlike/c)
                (#:repeat group-repeat/c)
                #:rest (listof stringlike/c)
                string?))
          (rx/non-capture-group? (-> string? boolean?))
          (mode/c flat-contract?)
          (rx/with-mode (-> stringlike/c #:mode (or/c mode/c (listof mode/c)) string?))
          (rx/conditional (->* (stringlike/c stringlike/c) (stringlike/c) string?))
          (rx/look-ahead (-> stringlike/c string?))
          (rx/not-look-ahead (-> stringlike/c string?))
          (rx/look-behind (-> stringlike/c string?))
          (rx/not-look-behind (-> stringlike/c string?))
          (rx/group-ref (-> integer? string?))
          (rx/named-group-ref (-> stringlike/c string?))
          ;; operators
          (rx/or-operator string?)
          (rx/range-operator string?)
          (rx/not-match-operator string?)
          (rx/anchor-at-start string?)
          (rx/anchor-at-end string?)
          ;; classes
          (rx/char-any string?)
          (rx/cclass-word-boundary string?)
          (rx/cclass-non-word-boundary string?)
          (rx/cclass-whitespace string?)
          (rx/cclass-non-whitespace string?)
          (rx/cclass-digit string?)
          (rx/cclass-non-digit string?)
          (rx/cclass-word-char string?)
          (rx/cclass-non-word-char string?)
          (rx/cclass-vertical-whitespace string?)
          (rx/cclass-non-vertical-whitespace string?)
          (rx/cclass-horizontal-whitespace string?)
          (rx/cclass-non-horizontal-whitespace string?)
          (rx/pclass-lower string?)
          (rx/pclass-upper string?)
          (rx/pclass-alpha string?)
          (rx/pclass-digit string?)
          (rx/pclass-alnum string?)
          (rx/pclass-xdigit string?)
          (rx/pclass-word string?)
          (rx/pclass-blank string?)
          (rx/pclass-space string?)
          (rx/pclass-graph string?)
          (rx/pclass-print string?)
          (rx/pclass-cntrl string?)
          (rx/pclass-ascii string?)
          (rx/uchar-any string?)
          (rx/uclass-one-data-unit string?)
          (rx/uclass-newlines string?)
          (rx/uclass-non-newlines string?)
          (rx/uclass-letter-lower string?)
          (rx/uclass-non-letter-lower string?)
          (rx/uclass-letter-upper string?)
          (rx/uclass-non-letter-upper string?)
          (rx/uclass-letter-title string?)
          (rx/uclass-non-letter-title string?)
          (rx/uclass-letter-modifier string?)
          (rx/uclass-non-letter-modifier string?)
          (rx/uclass-letter-no-other string?)
          (rx/uclass-non-letter-no-other string?)
          (rx/uclass-letter-other string?)
          (rx/uclass-non-letter-other string?)
          (rx/uclass-letter string?)
          (rx/uclass-non-letter string?)
          (rx/uclass-number-decimal string?)
          (rx/uclass-non-number-decimal string?)
          (rx/uclass-number-letter string?)
          (rx/uclass-non-number-letter string?)
          (rx/uclass-number-other string?)
          (rx/uclass-non-number-other string?)
          (rx/uclass-number string?)
          (rx/uclass-non-number string?)
          (rx/uclass-punctuation-open string?)
          (rx/uclass-non-punctuation-open string?)
          (rx/uclass-punctuation-close string?)
          (rx/uclass-non-punctuation-close string?)
          (rx/uclass-punctuation-quote-initial string?)
          (rx/uclass-non-punctuation-quote-initial string?)
          (rx/uclass-punctuation-quote-final string?)
          (rx/uclass-non-punctuation-quote-final string?)
          (rx/uclass-punctuation-connector string?)
          (rx/uclass-non-punctuation-connector string?)
          (rx/uclass-punctuation-dash string?)
          (rx/uclass-non-punctuation-dash string?)
          (rx/uclass-punctuation-other string?)
          (rx/uclass-non-punctuation-other string?)
          (rx/uclass-punctuation string?)
          (rx/uclass-non-punctuation string?)
          (rx/uclass-mark-non-spacing string?)
          (rx/uclass-non-mark-non-spacing string?)
          (rx/uclass-mark-space-combining string?)
          (rx/uclass-non-mark-space-combining string?)
          (rx/uclass-mark-enclosing string?)
          (rx/uclass-non-mark-enclosing string?)
          (rx/uclass-mark string?)
          (rx/uclass-non-mark string?)
          (rx/uclass-symbol-currency string?)
          (rx/uclass-non-symbol-currency string?)
          (rx/uclass-symbol-modifier string?)
          (rx/uclass-non-symbol-modifier string?)
          (rx/uclass-symbol-math string?)
          (rx/uclass-non-symbol-math string?)
          (rx/uclass-symbol-other string?)
          (rx/uclass-non-symbol-other string?)
          (rx/uclass-symbol string?)
          (rx/uclass-non-symbol string?)
          (rx/uclass-separator-line string?)
          (rx/uclass-non-separator-line string?)
          (rx/uclass-separator-paragraph string?)
          (rx/uclass-non-separator-paragraph string?)
          (rx/uclass-separator-space string?)
          (rx/uclass-non-separator-space string?)
          (rx/uclass-separator string?)
          (rx/uclass-non-separator string?)
          (rx/uclass-other-control string?)
          (rx/uclass-non-other-control string?)
          (rx/uclass-other-format string?)
          (rx/uclass-non-other-format string?)
          (rx/uclass-other-surrogate string?)
          (rx/uclass-non-other-surrogate string?)
          (rx/uclass-other-not-assigned string?)
          (rx/uclass-non-other-not-assigned string?)
          (rx/uclass-other-private-use string?)
          (rx/uclass-non-other-private-use string?)
          (rx/uclass-other string?)
          (rx/uclass-non-other string?)
          (rx/uclass-any string?)
          ;; matches
          (rx/range-lower string?)
          (rx/range-upper string?)
          (rx/range-alpha string?)
          (rx/range-digit string?)
          (rx/range-alnum string?)
          (rx/range-xdigit string?)
          (rx/range-word string?)
          (rx/match-lower string?)
          (rx/match-upper string?)
          (rx/match-alpha string?)
          (rx/match-digit string?)
          (rx/match-alnum string?)
          (rx/match-xdigit string?)
          (rx/match-word string?))
         ;; Aliases
         rx/&
         rx/?
         rx/*
         rx/+
         rx/^match
         rx/match-range
         rx/^match-range
         rx/string-exactly
         rx/nc-group
         rx/if
         rx/=>
         rx/!=>
         rx/<=
         rx/<!=)

;; =============================================================================
;; Internal
;; =============================================================================

(define stringlike/c (or/c string? symbol? char?))

(define (stringlike->string v)
  (cond
    ((string? v) v)
    ((symbol? v) (symbol->string v))
    ((char? v) (string v))
    (else (raise-argument-error 'v "stringlike/c" v))))

(define (stringlike-list->string-list lst)
  (map stringlike->string lst))

(define (stringlike-append str . lst)
  (stringlike-append* (cons str lst)))

(define (stringlike-append* lst)
  (string-append* (stringlike-list->string-list lst)))

(define (make-guard predicate?)
  (λ (v) (if (predicate? v) v (error))))

;; =============================================================================
;; Parameters
;; =============================================================================

(define named-group-form/c
  (or/c 'unsupported 'quoted 'bracketed 'p-bracketed))

(define rx/named-group-form
  (make-parameter 'unsupported
                  (make-guard named-group-form/c)
                  'named-group-form))

(define (rx/has-named-groups)
  (not (symbol=? (rx/named-group-form) 'unsupported)))

(define group-ref-form/c
  (or/c 'grouped 'escaped))

(define rx/group-ref-form
  (make-parameter 'escaped
                  (make-guard group-ref-form/c)
                  'group-ref-form))

(define rx/has-named-group-refs
  (make-parameter #f
                  (make-guard boolean?)
                  'has-named-group-refs))

(define rx/has-relative-group-refs
  (make-parameter #f
                  (make-guard boolean?)
                  'has-relative-group-refs))

;; =============================================================================
;; And and Or
;; =============================================================================

(define (rx/and expr . exprs)
  (stringlike-append* (cons expr exprs)))

(define rx/& rx/and)

(define rx/or-operator "|")

(define (rx/or expr . exprs)
  (string-join (stringlike-list->string-list (cons expr exprs))
               rx/or-operator))

;; =============================================================================
;; Repetition
;; =============================================================================

(define group-repeat/c (or/c 'one 'optional 'zero-or-more 'one-or-more))

(define (rx/repeat-symbol repeat)
  (cond
    ((symbol=? repeat 'one) "")
    ((symbol=? repeat 'optional) "?")
    ((symbol=? repeat 'zero-or-more) "*")
    ((symbol=? repeat 'one-or-more) "+")))

(define (rx/repeat-bounds repeat)
  (cond
    ((symbol=? repeat 'one) "")
    ((symbol=? repeat 'optional) "{0,1}")
    ((symbol=? repeat 'zero-or-more) "{0,}")
    ((symbol=? repeat 'one-or-more) "{1,}")))

(define (rx/optional expr)
  (rx/and expr (rx/repeat-symbol 'optional)))

(define rx/? rx/optional)

(define (rx/zero-or-more expr)
  (rx/and expr (rx/repeat-symbol 'zero-or-more)))

(define rx/* rx/zero-or-more)

(define (rx/one-or-more expr)
  (rx/and expr (rx/repeat-symbol 'one-or-more)))

(define rx/+ rx/one-or-more)

(define (rx/repeat-safe? expr)
  (or (char? expr)
      (= (string-length expr) 1)
      (rx/group? expr)
      (rx/match? expr)))

(define (rx/repeat expr #:lower (lower #f) #:upper (upper #f))
  (when (and (false? lower) (false? upper))
     (raise-argument-error 'upper (format "or/c ~s ~s" lower upper) (list lower upper)))
  (if (and (exact-nonnegative-integer? lower)
           (exact-nonnegative-integer? upper)
           (< upper lower))
      (raise-argument-error 'upper (format ">= ~s" lower) upper)
      (format "~a{~a~a}" expr
              (if lower lower "")
              (if upper
                  (if (and lower (= lower upper))
                      ""
                      (format ",~a" upper))
                  ","))))

(define (rx/repeat-safely expr #:lower (lower #f) #:upper (upper #f))
  (let ((expr (if (rx/repeat-safe? expr) expr (rx/non-capture-group expr))))
    (rx/repeat expr #:lower lower #:upper upper)))

;; =============================================================================
;; Match and Range
;; =============================================================================

(define rx/range-operator "-")

(define (char>=/c to-char)
  (lambda (from-char) (if (char>=? from-char to-char)
                          #t
                          (raise-argument-error 'to-char (format "char>=? ~s" to-char) from-char))))

(define (rx/range from to)
  (format "~a~a~a" from rx/range-operator to))


(define (rx/ranges range . ranges)
  (apply rx/and (map (λ (pair) (rx/range (car pair) (cdr pair)))
                     (cons range ranges))))

(define (rx/range? expr)
  (and (string? expr)
       (cond
         ((= (string-length expr) 3)
          (char=? (string-ref expr 1) #\-))
         ((= (string-length expr) 4)
          (or
           (and (char=? (string-ref expr 0) #\\)
                (char=? (string-ref expr 2) #\-))
           (and (char=? (string-ref expr 2) #\\)
                (char=? (string-ref expr 1) #\-))))
         ((= (string-length expr) 5)
          (and (char=? (string-ref expr 0) #\\)
               (char=? (string-ref expr 2) #\-)
               (char=? (string-ref expr 3) #\\)))
         (else #f))))

(define (rx/match char-or-range #:repeat (repeat 'one) . more)
  (format "[~a]~a"
          (stringlike-append* (cons char-or-range (map rx/escape more)))
          (rx/repeat-symbol repeat)))

(define (rx/match? expr)
  (and (string-prefix? expr "[") (string-suffix? expr "]")))

(define rx/not-match-operator "^")

(define (rx/not-match char-or-range #:repeat (repeat 'one) . more)
  (apply rx/match rx/not-match-operator char-or-range more #:repeat repeat))

(define (rx/not-match? expr)
  (and (string-prefix? expr "[^") (string-suffix? expr "]")))

(define rx/^match rx/not-match)

(define rx/match-range (compose rx/match rx/range))

(define rx/not-match-range (compose rx/not-match rx/range))

(define rx/^match-range rx/not-match-range)

(define *escaped-chars* (string->list ".^$*+?()[{\\|"))

(define *escaped-chars-in-match* (string->list "-]\\"))

(define (rx/escape char-or-string (location 'in-match ))
  (let ((escape-chars (if (symbol=? location 'in-match)
                          *escaped-chars-in-match*
                          *escaped-chars*)))
    (cond
      ((char? char-or-string) (if (member char-or-string escape-chars)
                                  (format "\\~a" char-or-string)
                                  (format "~a" char-or-string)))
      ((and (string? char-or-string)
            (equal? (string-length char-or-string) 1))
       (string-append* (map (λ (c) (rx/escape c location)) (string->list char-or-string))))
      (else char-or-string))))

(define rx/range-lower (rx/range #\a #\z))
(define rx/range-upper (rx/range #\A #\Z))
(define rx/range-alpha (rx/and rx/range-lower rx/range-upper))
(define rx/range-digit (rx/range #\0 #\9))
(define rx/range-alnum (rx/and rx/range-alpha rx/range-digit))
(define rx/range-xdigit (rx/and rx/range-digit (rx/range #\A #\F) (rx/range #\a #\f)))
(define rx/range-word (rx/and rx/range-alnum "_"))

(define rx/match-lower (rx/match rx/range-lower))
(define rx/match-upper (rx/match rx/range-upper))
(define rx/match-alpha (rx/match rx/range-alpha))
(define rx/match-digit (rx/match rx/range-digit))
(define rx/match-alnum (rx/match rx/range-alnum))
(define rx/match-xdigit (rx/match rx/range-xdigit))
(define rx/match-word (rx/match rx/range-word))

;; =============================================================================
;; Character Classes
;; =============================================================================

(define rx/char-any ".")

(define rx/cclass-word-boundary "\\b")
(define rx/cclass-non-word-boundary "\\B")
(define rx/cclass-whitespace "\\s")
(define rx/cclass-non-whitespace "\\S")
(define rx/cclass-digit "\\d")
(define rx/cclass-non-digit "\\D")
(define rx/cclass-word-char "\\w")
(define rx/cclass-non-word-char "\\W")
(define rx/cclass-vertical-whitespace "\\v")
(define rx/cclass-non-vertical-whitespace "\\V")
(define rx/cclass-horizontal-whitespace "\\h")
(define rx/cclass-non-horizontal-whitespace "\\H")

;; Posix classes

(define rx/pclass-lower "[:lower:]")
(define rx/pclass-upper "[:upper:]")
(define rx/pclass-alpha "[:alpha:]")
(define rx/pclass-digit "[:digit:]")
(define rx/pclass-alnum "[:alnum:]")
(define rx/pclass-xdigit "[:xdigit:]")
(define rx/pclass-word "[:word:]")
(define rx/pclass-blank "[:blank:]")
(define rx/pclass-space "[:space:]")
(define rx/pclass-graph "[:graph:]")
(define rx/pclass-print "[:print:]")
(define rx/pclass-cntrl "[:cntrl:]")
(define rx/pclass-ascii "[:ascii:]")

;; Unicode classes

(define rx/uchar-any "\\X")

(define rx/uclass-one-data-unit "\\C")
(define rx/uclass-newlines "\\R")
(define rx/uclass-non-newlines "\\N")

(define rx/uclass-letter-lower "\\p{Ll}")
(define rx/uclass-non-letter-lower "\\P{Ll}")
(define rx/uclass-letter-upper "\\p{Lu}")
(define rx/uclass-non-letter-upper "\\P{Lu}")
(define rx/uclass-letter-title "\\p{Lt}")
(define rx/uclass-non-letter-title "\\P{Lt}")
(define rx/uclass-letter-modifier "\\p{Lm}")
(define rx/uclass-non-letter-modifier "\\P{Lm}")
(define rx/uclass-letter-no-other "\\p{L&}")
(define rx/uclass-non-letter-no-other "\\P{L&}")
(define rx/uclass-letter-other "\\p{Lo}")
(define rx/uclass-non-letter-other "\\P{Lo}")
(define rx/uclass-letter "\\p{L}")
(define rx/uclass-non-letter "\\P{L}")

(define rx/uclass-number-decimal "\\p{Nd}")
(define rx/uclass-non-number-decimal "\\P{Nd}")
(define rx/uclass-number-letter "\\p{Nl}")
(define rx/uclass-non-number-letter "\\P{Nl}")
(define rx/uclass-number-other "\\p{No}")
(define rx/uclass-non-number-other "\\P{No}")
(define rx/uclass-number "\\p{N}")
(define rx/uclass-non-number "\\P{N}")

(define rx/uclass-punctuation-open "\\p{Ps}")
(define rx/uclass-non-punctuation-open "\\P{Ps}")
(define rx/uclass-punctuation-close "\\p{Pe}")
(define rx/uclass-non-punctuation-close "\\P{Pe}")
(define rx/uclass-punctuation-quote-initial "\\p{Pi}")
(define rx/uclass-non-punctuation-quote-initial "\\P{Pi}")
(define rx/uclass-punctuation-quote-final "\\p{Pf}")
(define rx/uclass-non-punctuation-quote-final "\\P{Pf}")
(define rx/uclass-punctuation-connector "\\p{Pc}")
(define rx/uclass-non-punctuation-connector "\\P{Pc}")
(define rx/uclass-punctuation-dash "\\p{Pd}")
(define rx/uclass-non-punctuation-dash "\\P{Pd}")
(define rx/uclass-punctuation-other "\\p{Po}")
(define rx/uclass-non-punctuation-other "\\P{Po}")
(define rx/uclass-punctuation "\\p{P}")
(define rx/uclass-non-punctuation "\\P{P}")

(define rx/uclass-mark-non-spacing "\\p{Mn}")
(define rx/uclass-non-mark-non-spacing "\\P{Mn}")
(define rx/uclass-mark-space-combining "\\p{Mc}")
(define rx/uclass-non-mark-space-combining "\\P{Mc}")
(define rx/uclass-mark-enclosing "\\p{Me}")
(define rx/uclass-non-mark-enclosing "\\P{Me}")
(define rx/uclass-mark "\\p{P}")
(define rx/uclass-non-mark "\\P{P}")

(define rx/uclass-symbol-currency "\\p{Sc}")
(define rx/uclass-non-symbol-currency "\\P{Sc}")
(define rx/uclass-symbol-modifier "\\p{Sk}")
(define rx/uclass-non-symbol-modifier "\\P{Sk}")
(define rx/uclass-symbol-math "\\p{Sm}")
(define rx/uclass-non-symbol-math "\\P{Sm}")
(define rx/uclass-symbol-other "\\p{So}")
(define rx/uclass-non-symbol-other "\\P{So}")
(define rx/uclass-symbol "\\p{S}")
(define rx/uclass-non-symbol "\\P{S}")

(define rx/uclass-separator-line "\\p{Zl}")
(define rx/uclass-non-separator-line "\\P{Zl}")
(define rx/uclass-separator-paragraph "\\p{Zp}")
(define rx/uclass-non-separator-paragraph "\\P{Zp}")
(define rx/uclass-separator-space "\\p{Zs}")
(define rx/uclass-non-separator-space "\\P{Zs}")
(define rx/uclass-separator "\\p{Z}")
(define rx/uclass-non-separator "\\P{Z}")

(define rx/uclass-other-control "\\p{Cc}")
(define rx/uclass-non-other-control "\\P{Cc}")
(define rx/uclass-other-format "\\p{Cf}")
(define rx/uclass-non-other-format "\\P{Cf}")
(define rx/uclass-other-surrogate "\\p{Cs}")
(define rx/uclass-non-other-surrogate "\\P{Cs}")
(define rx/uclass-other-not-assigned "\\p{Cn}")
(define rx/uclass-non-other-not-assigned "\\P{Cn}")
(define rx/uclass-other-private-use "\\p{Co}")
(define rx/uclass-non-other-private-use "\\P{Co}")
(define rx/uclass-other "\\p{C}")
(define rx/uclass-non-other "\\P{C}")

(define rx/uclass-any "\\p{.}")


;; =============================================================================
;; Anchors
;; =============================================================================

(define rx/anchor-at-start "^")

(define (rx/string-prefix expr)
  (rx/and rx/anchor-at-start expr))

(define rx/anchor-at-end "$")

(define (rx/string-suffix expr)
  (rx/and expr rx/anchor-at-end))

(define rx/string-exactly (compose rx/string-suffix rx/string-prefix))

;; =============================================================================
;; Groups
;; =============================================================================

;; non-capture group "(?:...)"
;; atomic (non-capture) group "(?>...)"

;; (? test expr-if-true | expr-if-false )
;;    ^^^^                ^^^^^^^^^^^^^^^ -- optional
;;     '------------------------------------ (n) if nth group matched
;;                                           (?=...) look ahead matches expr
;;                                           (?!...) look ahead does not match expr
;;                                           (?<=...) look behind matches expr
;;                                           (?<!...) look behind does not match expr
;; Note that expr matches take place directly after test match.

(define (rx/group expr #:repeat (repeat 'one) . more)
  (format "(~a)~a"
          (apply rx/and expr more)
          (rx/repeat-symbol repeat)))

(define (rx/group? expr)
  (and (string-prefix? expr "(") (string-suffix? expr ")")))

(define (rx/or-group expr #:repeat (repeat 'one) . more)
  (rx/group (apply rx/or expr more) #:repeat repeat))

(define (rx/and-group expr #:repeat (repeat 'one) . more)
  (rx/group (apply rx/and expr more) #:repeat repeat))

(define (rx/special-group special expr #:repeat (repeat 'one) . more)
  (rx/group "?" special (apply rx/and expr more) #:repeat repeat))

(define (rx/special-group? expr)
  (and (string-prefix? expr "(?") (string-suffix? expr ")")))

(define (rx/named-group expr #:name name #:repeat (repeat 'one) . more)
  (rx/special-group (if (and (rx/has-named-groups) name) (format "<~a>" name) "")
                    (apply rx/and expr more)
                    #:repeat repeat))

(define (rx/named-group? expr)
  (and (string-prefix? expr "(?<") (string-suffix? expr ")")))

(define (rx/non-capture-group expr #:repeat (repeat 'one) . more)
  (rx/special-group ":" (apply rx/and expr more) #:repeat repeat))

(define rx/nc-group rx/non-capture-group)

(define (rx/non-capture-group? expr)
  (and (string-prefix? expr "(?:") (string-suffix? expr ")")))

;; =============================================================================
;; Modes
;; =============================================================================

(define mode/c (or/c 'case-sensitive 'case-insensitive 'multi-line 'not-multi-line))

(define (rx/with-mode expr #:mode modes)
  (let* ((mode-list (if (list? modes) modes (list modes)))
         (mode-hash (hash 'case-sensitive "-i" 'case-insensitive "i" 'multi-line "m" 'not-multi-line "-m"))
         (mode-string (string-append* (map (λ (m) (hash-ref mode-hash m "")) mode-list))))
    (rx/special-group mode-string ":"  expr)))

;; =============================================================================
;; Conditionals
;; =============================================================================

(define (rx/conditional test if-true (if-false #f))
  (if if-false
      (rx/special-group "" test (rx/or if-true if-false))
      (rx/special-group "" test if-true)))

(define rx/if rx/conditional)

(define (rx/look-ahead expr)
  (rx/special-group "=" expr))

(define rx/=> rx/look-ahead)

(define (rx/not-look-ahead expr)
  (rx/special-group "!" expr))

(define rx/!=> rx/not-look-ahead)

(define (rx/look-behind expr)
  (rx/special-group "<=" expr))

(define rx/<= rx/look-behind)

(define (rx/not-look-behind expr)
  (rx/special-group "<!" expr))

(define rx/<!= rx/not-look-behind)

(define (rx/group-ref n)
  (if (and (symbol=? (rx/group-ref-form) 'grouped) (rx/has-relative-group-refs))
      (when (= n 0) (raise-argument-error 'n "(not (= 0))" n))
      (when (<= n 0)  (raise-argument-error 'n "> 0" n)))
  (cond
    ((symbol=? (rx/group-ref-form) 'grouped)
     (rx/group (number->string n)))
    ((symbol=? (rx/group-ref-form) 'escaped)
     (format "\\~a" n))))

(define (rx/named-group-ref name)
  (when (not (symbol=? (rx/group-ref-form) 'grouped))
    (raise-argument-error 'rx/group-ref-form "symbol=? 'grouped" (rx/group-ref-form)))
  (when (not (rx/has-named-group-refs))
    (raise-argument-error 'rx/has-named-group-refs "boolean=> #t" (rx/has-named-group-refs)))
  (rx/group name))
