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

(define named-group-form/c (or/c 'unsupported 'quoted 'bracketed 'p-bracketed))
(define rx/named-group-form (make-parameter 'unsupported
                                            (make-guard named-group-form/c)
                                            'named-group-form))

(define (rx/has-named-groups)
  (not (symbol=? (rx/named-group-form) 'unsupported)))

(define group-ref-form/c (or/c 'grouped 'escaped))
(define rx/group-ref-form (make-parameter 'escaped
                                          (make-guard group-ref-form/c)
                                          'group-ref-form))

(define rx/has-named-group-refs (make-parameter #f
                                                (make-guard boolean?)
                                                'has-named-group-refs))

(define rx/has-relative-group-refs (make-parameter #f
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

(define (rx/match char-or-range . more)
  (format "[~a]" (string-append* (if (string? char-or-range) char-or-range (string char-or-range))
                                 (map rx/escape more))))

(define (rx/match? expr)
  (and (string-prefix? expr "[") (string-suffix? expr "]")))

(define rx/not-match-operator "^")

(define (rx/not-match char-or-range . more)
  (apply rx/match rx/not-match-operator char-or-range more))

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

;; =============================================================================
;; Character Classes
;; =============================================================================

(define rx/char-any ".")
(define rx/char-newline "\\n")
(define rx/char-carriage-return "\\r")
(define rx/char-form-feed "\\f")
(define rx/char-tab "\\t")
(define rx/char-null "\\0")

(define rx/class-word-boundary "\\b")
(define rx/class-non-word-boundary "\\B")
(define rx/class-whitespace "\\s")
(define rx/class-non-whitespace "\\S")
(define rx/class-digit "\\d")
(define rx/class-non-digit "\\D")
(define rx/class-word-char "\\w")
(define rx/class-non-word-char "\\W")
(define rx/class-any-unicode "\\X")
(define rx/class-one-data-unit "\\C")
(define rx/class-unicode-newlines "\\R")
(define rx/class-non-newlines "\\N")
(define rx/class-vertical-whitespace "\\v")
(define rx/class-non-vertical-whitespace "\\V")
(define rx/class-horizontal-whitespace "\\h")
(define rx/class-non-horizontal-whitespace "\\H")

(define rx/ascii-alpha-lower (rx/range #\a #\z))
(define rx/ascii-alpha-upper (rx/range #\A #\Z))
(define rx/ascii-alpha (rx/and rx/ascii-alpha-lower rx/ascii-alpha-upper))
(define rx/ascii-numeric (rx/range #\0 #\9))
(define rx/ascii-alpha-numeric (rx/and rx/ascii-alpha rx/ascii-numeric))

(define posix-class/c (or/c 'lower 'upper 'alpha 'digit 'alnum))

(define/contract (rx/posix-class->string cls)
  (-> posix-class/c string?)
  (format "[:~a:]" (symbol->string cls)))

(define/contract (rx/match-posix-class cls)
  (-> posix-class/c string?)
  (rx/match (rx/posix-class->string cls)))

(define/contract (rx/not-match-posix-class cls)
  (-> posix-class/c string?)
  (rx/not-match (rx/posix-class->string cls)))

(define rx/^match-posix-class rx/not-match-posix-class)

(define unicode-category/c
  (or/c 'C 'Cc 'Cf 'Cn 'Co 'Cs
        'L 'Ll 'Lm 'Lo 'Lt 'Lu 'L&
        'M 'Mc 'Me 'Mn
        'N 'Nd 'Nl 'No
        'P 'Pc 'Pd 'Pe 'Pf 'Pi 'Po 'Ps
        'S 'Sc 'Sk 'Sm 'So
        'Z 'Zl 'Zp 'Zs
        'any))

(define/contract (rx/unicode-category->string category)
  (-> unicode-category/c string?)
  (if (symbol=? category 'any) "." (symbol->string category)))

(define/contract (rx/unicode-category category)
  (-> unicode-category/c string?)
  (format "\\p{~a}" (rx/unicode-category->string category)))

(define/contract (rx/unicode-not-category category)
  (-> unicode-category/c string?)
  (format "\\P{~a}" (rx/unicode-category->string category)))

;; =============================================================================
;; Repetition
;; =============================================================================

(define rx/optional-operator "?")

(define/contract (rx/optional expr)
  (-> string? string?)
  (rx/and expr rx/optional-operator))

(define rx/? rx/optional)

(define rx/zero-or-more-operator "*")

(define/contract (rx/zero-or-more expr)
  (-> string? string?)
  (rx/and expr rx/zero-or-more-operator))

(define rx/* rx/zero-or-more)

(define rx/one-or-more-operator "+")

(define/contract (rx/one-or-more expr)
  (-> string? string?)
  (rx/and expr rx/one-or-more-operator))

(define rx/+ rx/one-or-more)

(define (rx/repeat-safe? expr)
  (or (char? expr)
      (= (string-length expr) 1)
      (rx/group? expr)
      (rx/match? expr)))

(define (rx/repeat expr #:lower (lower #f) #:upper (upper #f))
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

(define group-repeat/c (or/c 'one 'optional 'zero-or-more 'one-or-more))

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
          (cond
            ((symbol=? repeat 'optional) rx/optional-operator)
            ((symbol=? repeat 'zero-or-more) rx/zero-or-more-operator)
            ((symbol=? repeat 'one-or-more) rx/one-or-more-operator)
            (else ""))))

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
  ;; TODO: type check n
  (cond
    ((symbol=? (rx/group-ref-form) 'grouped)
     (rx/group (number->string n)))
    ((symbol=? (rx/group-ref-form) 'escaped)
     (format "\\~a" n))))

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
       (check-equal? (rx/and rx/char-any rx/class-word-boundary rx/char-any)
                     ".\\b."))

     (test-case
         "function rx/class-not-word-boundary (ex18)"
       (check-equal? (rx/and rx/char-any rx/class-non-word-boundary rx/char-any)
                     ".\\B."))

     (test-case
         "function rx/unicode-category (ex19)"
       (check-equal? (rx/unicode-category 'Ll)
                     "\\p{Ll}"))

     (test-case
         "function rx/unicode-not-category (ex20)"
       (check-equal? (rx/unicode-not-category 'Ll)
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
       (check-equal? (rx/zero-or-more (rx/match (rx/range #\a #\f) rx/class-digit))
                     "[a-f\\d]*"))

     (test-case
         "function rx/class-word-char (ex24)"
       (check-equal? (rx/and " " (rx/match rx/class-word-char))
                     " [\\w]"))

     (test-case
         "function rx/class-whitespace (ex25)"
       (check-equal? (rx/and "t" (rx/match rx/class-whitespace))
                     "t[\\s]"))

     (test-case
         "function rx/match-posix-class (ex26)"
       (check-equal? (rx/one-or-more (rx/match-posix-class 'lower))
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
                     "[^^]+"))))

  (run-tests rx-test-suite))
