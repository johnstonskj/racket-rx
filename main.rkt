#lang racket/base

(require racket/bool
         racket/contract
         racket/string)

(provide (except-out (all-defined-out)
                     *escaped-chars*
                     *escaped-chars-in-match*
                     make-guard))

;; =============================================================================
;; Parameters
;; =============================================================================

(define (make-guard predicate?)
  (λ (v) (if (predicate? v) v (error))))

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
  (string-append* expr exprs))

(define rx/& rx/and)

(define rx/or-operator "|")

(define (rx/or expr . exprs)
  (string-join (cons expr exprs) rx/or-operator))

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

(define/contract (rx/optional expr)
  (-> string? string?)
  (rx/and expr (rx/repeat-symbol 'optional)))

(define rx/? rx/optional)

(define/contract (rx/zero-or-more expr)
  (-> string? string?)
  (rx/and expr (rx/repeat-symbol 'zero-or-more)))

(define rx/* rx/zero-or-more)

(define/contract (rx/one-or-more expr)
  (-> string? string?)
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

(define/contract (rx/range from to)
  (->i ((from char?) (to (from) (and/c char? (char>=/c from)))) (result string?))
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
          (string-append* (if (string? char-or-range) char-or-range (string char-or-range))
                          (map rx/escape more))
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

(define/contract (rx/string-prefix expr)
  (-> string? string?)
  (rx/and rx/anchor-at-start expr))

(define rx/anchor-at-end "$")

(define/contract (rx/string-suffix expr)
  (-> string? string?)
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

(define/contract (rx/look-ahead expr)
  (-> string? string?)
  (rx/special-group "=" expr))

(define rx/=> rx/look-ahead)

(define/contract (rx/not-look-ahead expr)
  (-> string? string?)
  (rx/special-group "!" expr))

(define rx/!=> rx/not-look-ahead)

(define/contract (rx/look-behind expr)
  (-> string? string?)
  (rx/special-group "<=" expr))

(define rx/<= rx/look-behind)

(define/contract (rx/not-look-behind expr)
  (-> string? string?)
  (rx/special-group "<!" expr))

(define rx/<!= rx/not-look-behind)

(define/contract (rx/group-ref n)
  (-> integer? string?)
  (if (and (symbol=? (rx/group-ref-form) 'grouped) (rx/has-relative-group-refs))
      (when (= n 0) (raise-argument-error 'n "(not (= 0))" n))
      (when (<= n 0)  (raise-argument-error 'n "> 0" n)))
  (cond
    ((symbol=? (rx/group-ref-form) 'grouped)
     (rx/group (number->string n)))
    ((symbol=? (rx/group-ref-form) 'escaped)
     (format "\\~a" n))))

(define/contract (rx/named-group-ref name)
  (-> string? string?)
  (when (not (symbol=? (rx/group-ref-form) 'grouped))
    (raise-argument-error 'rx/group-ref-form "symbol=? 'grouped" (rx/group-ref-form)))
  (when (not (rx/has-named-group-refs))
    (raise-argument-error 'rx/has-named-group-refs "boolean=> #t" (rx/has-named-group-refs)))
  (rx/group name))

;; =============================================================================
;; =============================================================================
;; Local Tests
;; =============================================================================
;; =============================================================================

(module+ test

  (require rackunit
           rackunit/text-ui)

  (provide rx-test-suite)

  (define rx-test-suite
    (test-suite
     "Module rx"

     (test-case
         "README example"
       (check-equal? (rx/with-mode
                      (rx/string-prefix (rx/or-group "hello" "hi" "g'day"))
                      #:mode 'case-insensitive)
                     "(?i:^(hello|hi|g'day))"))
     (test-case
         "function rx/and"
       (check-equal? (rx/and "a" "b")
                     "ab"))

     (test-case
         "function rx/or (ex1)"
       (check-equal? (rx/or "a" "b")
                     "a|b"))

     (test-case
         "function rx/match (ex2)"
       (check-equal? (rx/match "a" "b") "[ab]")
       (check-equal? (rx/match #\a "b") "[ab]")
       (check-equal? (rx/match "a" #\b) "[ab]")
       (check-equal? (rx/match #\a #\b) "[ab]"))

     (test-case
         "function rx/zero-or-more (ex3)"
       (check-equal? (rx/and (rx/zero-or-more "ca") (rx/match "a" "t"))
                     "ca*[at]")
       (check-equal? (rx/and (rx/* "ca") (rx/match "a" "t"))
                     "ca*[at]"))

     (test-case
         "function rx/one-or-more (ex4)"
       (check-equal? (rx/and (rx/one-or-more "ca") (rx/match "a" "t"))
                     "ca+[at]")
       (check-equal? (rx/and (rx/+ "ca") (rx/match "a" "t"))
                     "ca+[at]"))

     (test-case
         "function rx/optional (ex5)"
       (check-equal? (rx/and (rx/optional "ca") (rx/optional "t"))
                     "ca?t?")
       (check-equal? (rx/and (rx/? "ca") (rx/? "t"))
                     "ca?t?"))

     (test-case
         "function * and ? (ex6)"
       (check-equal? (rx/and (rx/optional (rx/zero-or-more "ca")) (rx/match "a" "t"))
                     "ca*?[at]")
       (check-equal? (rx/and (rx/? (rx/* "ca")) (rx/match "a" "t"))
                     "ca*?[at]"))

     (test-case
         "function rx/repeat (ex7)"
       (check-equal? (rx/and "c" (rx/repeat "a" #:lower 2 #:upper 2))
                     "ca{2}"))

     (test-case
         "function rx/repeat (ex8)"
       (check-equal? (rx/and "c" (rx/repeat "a" #:lower 2) "t")
                     "ca{2,}t"))

     (test-case
         "function rx/repeat (ex9)"
       (check-equal? (rx/and "c" (rx/repeat "a" #:upper 2) "t")
                     "ca{,2}t"))

     (test-case
         "function rx/repeat (ex10)"
       (check-equal? (rx/and "c" (rx/repeat "a" #:lower 1 #:upper 2) "t")
                     "ca{1,2}t"))

     ;; ex11 "(c<*)(a*)"

     (test-case
         "function rx/not-match (ex12)"
       (check-equal? (rx/not-match "c" "a")
                     "[^ca]")
       (check-equal? (rx/^match "c" "a")
                     "[^ca]"))

     (test-case
         "function rx/char-any (ex13)"
       (check-equal? (rx/and rx/char-any (rx/group rx/char-any) rx/char-any)
                     ".(.)."))

     (test-case
         "function rx/string-prefix (ex14)"
       (check-equal? (rx/or (rx/string-prefix "a") (rx/string-prefix "c"))
                     "^a|^c"))

     (test-case
         "function rx/string-suffix (ex15)"
       (check-equal? (rx/or (rx/string-suffix "a") (rx/string-suffix "t"))
                     "a$|t$"))

     (test-case
         "function rx/group-ref (ex16)"
       (check-equal? (parameterize ((rx/group-ref-form 'escaped))
                       (rx/and "c" (rx/group rx/char-any) (rx/group-ref 1) "t"))
                     "c(.)\\1t"))

     (test-case
         "function rx/class-word-boundary (ex17)"
       (check-equal? (rx/and rx/char-any rx/cclass-word-boundary rx/char-any)
                     ".\\b."))

     (test-case
         "function rx/class-not-word-boundary (ex18)"
       (check-equal? (rx/and rx/char-any rx/cclass-non-word-boundary rx/char-any)
                     ".\\B."))

     (test-case
         "function rx/unicode-category (ex19)"
       (check-equal? rx/uclass-letter-lower
                     "\\p{Ll}"))

     (test-case
         "function rx/unicode-not-category (ex20)"
       (check-equal? rx/uclass-non-letter-lower
                     "\\P{Ll}"))

     (test-case
         "function rx/escape (ex21)"
       (check-equal? (rx/escape #\| 'outside-match)
                     "\\|")
       (check-equal? (rx/escape "|" 'outside-match)
                     "\\|"))

     (test-case
         "function rx/match-range (ex22)"
       (check-equal? (rx/zero-or-more (rx/match (rx/range #\a #\f)))
                     "[a-f]*")
       (check-equal? (rx/* (rx/match-range #\a #\f))
                     "[a-f]*"))

     (test-case
         "function rx/class-digit (ex23)"
       (check-equal? (rx/zero-or-more (rx/match (rx/range #\a #\f) rx/cclass-digit))
                     "[a-f\\d]*"))

     (test-case
         "function rx/class-word-char (ex24)"
       (check-equal? (rx/and " " (rx/match rx/cclass-word-char))
                     " [\\w]"))

     (test-case
         "function rx/class-whitespace (ex25)"
       (check-equal? (rx/and "t" (rx/match rx/cclass-whitespace))
                     "t[\\s]"))

     (test-case
         "function rx/match-posix-class (ex26)"
       (check-equal? (rx/one-or-more (rx/match rx/pclass-lower))
                     "[[:lower:]]+"))

     (test-case
         "function rx/match (ex27)"
       (check-equal? (rx/match #\])
                     "[]]")
       (check-equal? (rx/match "]")
                     "[]]"))

     (test-case
         "function rx/match (ex28)"
       (check-equal? (rx/match #\-)
                     "[-]")
       (check-equal? (rx/match "-")
                     "[-]"))

     (test-case
         "function rx/match (ex29)"
       (check-equal? (rx/one-or-more (rx/match #\] #\a #\[))
                     "[]a[]+")
       (check-equal? (rx/+ (rx/match "]a["))
                     "[]a[]+"))

     (test-case
         "function rx/match (ex30)"
       (check-equal? (rx/one-or-more (rx/match #\a #\^))
                     "[a^]+")
       (check-equal? (rx/+ (rx/match "a^"))
                     "[a^]+"))

     (test-case
         "function rx/look-ahead (ex31)"
       (check-equal? (rx/and rx/char-any "a" (rx/look-ahead "p"))
                     ".a(?=p)")
       (check-equal? (rx/and rx/char-any "a" (rx/=> "p"))
                     ".a(?=p)"))

     (test-case
         "function rx/not-look-ahead (ex32)"
       (check-equal? (rx/and rx/char-any "a" (rx/not-look-ahead "t"))
                     ".a(?!t)")
       (check-equal? (rx/and rx/char-any "a" (rx/!=> "t"))
                     ".a(?!t)"))

     (test-case
         "function rx/look-behind (ex33)"
       (check-equal? (rx/and (rx/look-behind "n") "a" rx/char-any)
                     "(?<=n)a.")
       (check-equal? (rx/and (rx/<= "n") "a"  rx/char-any)
                     "(?<=n)a."))

     (test-case
         "function rx/not-look-behind (ex34)"
       (check-equal? (rx/and (rx/not-look-behind "c") "a" rx/char-any)
                     "(?<!c)a.")
       (check-equal? (rx/and (rx/<!= "c") "a"  rx/char-any)
                     "(?<!c)a."))

     (test-case
         "function rx/not-match (ex35)"
       (check-equal? (rx/and (rx/with-mode "a" #:mode 'case-insensitive) (rx/match "tp"))
                     "(?i:a)[tp]"))

     (test-case
         "function rx/conditional (ex36)"
       (check-equal? (rx/+ (rx/conditional (rx/look-behind "c") "a" "b"))
                     "(?(?<=c)a|b)+"))

     (test-case
         "function rx/not-match (ex37)"
       (check-equal? (rx/one-or-more (rx/not-match #\^))
                     "[^^]+")
       (check-equal? (rx/+ (rx/^match "^"))
                     "[^^]+"))

     (test-case
         "function rx/ranges"
       (check-equal? (rx/ranges '(#\A . #\Z) '(#\a . #\z) '(#\0 . #\9))
                     "A-Za-z0-9"))

     (test-case
         "function rx/range exception"
       (check-exn exn:fail:contract?
                  (λ () (rx/range #\z #\a))))

     (test-case
         "function rx/group-ref exception"
       (check-exn exn:fail:contract?
                  (λ () (rx/group-ref 0))))

     (test-case
         "function rx/named-group-ref exception"
       (check-exn exn:fail:contract?
                  (λ () (rx/named-group-ref "id"))))

     (test-case
         "function rx/repeat exception"
       (check-exn exn:fail:contract?
                  (λ () (rx/repeat "something"))))

     )) ;; << test-suite

  (run-tests rx-test-suite))
