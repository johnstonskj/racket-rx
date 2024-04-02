#lang racket/base

(require racket/bool
         racket/contract
         racket/list
         racket/string)

(provide (except-out (all-defined-out) *escaped-chars* *escaped-chars-in-match*))

(define rx/has-named-groups (make-parameter #f boolean? 'has-named-groups))

(define group-repeat/c (or/c 'one 'optional 'zero-or-more 'one-or-more))

(define (rx/or expr . exprs)
  (string-join (cons expr exprs) "|"))

(define (rx/and expr . exprs)
  (string-append* expr exprs))

(define (rx/optional expr)
  (rx/and expr "?"))

(define rx/? rx/optional)

(define (rx/zero-or-more expr)
  (rx/and expr "*"))

(define rx/* rx/zero-or-more)

(define (rx/one-or-more expr)
  (rx/and expr "+"))

(define rx/+ rx/one-or-more)

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

(define (rx/conditional test if-true (if-false #f))
  (if if-false
      (rx/and-group "?" test (rx/or if-true if-false))
      (rx/and-group "?" test if-true)))

(define (rx/look-ahead expr)
  (rx/and-group "?=" expr))
(define rx/=> rx/look-ahead)

(define (rx/not-look-ahead expr)
   (rx/and-group "?!" expr))
(define rx/!=> rx/not-look-ahead)

(define (rx/look-behind expr)
   (rx/and-group "?<=" expr))
(define rx/<= rx/look-behind)

(define (rx/not-look-behind expr)
   (rx/and-group "?<!" expr))
(define rx/<!= rx/not-look-behind)

(define (rx/group expr #:named (named #f) #:repeat (repeat 'one) . more)
  (let ((name-str (if (and (rx/has-named-groups) named) (format "?<~a>" named) ""))
        (repeat-str (cond
                      ((symbol=? repeat 'optional) "?")
                      ((symbol=? repeat 'zero-or-more) "*")
                      ((symbol=? repeat 'one-or-more) "+")
                      (else ""))))
    (format "(~a~a)~a" name-str (string-append* expr more) repeat-str)))

(define (rx/or-group expr #:named (named #f) #:repeat (repeat 'one) . more)
  (rx/group (apply rx/or expr more) #:named named #:repeat repeat))

(define (rx/and-group expr #:named (named #f) #:repeat (repeat 'one) . more)
  (rx/group (apply rx/and expr more) #:named named #:repeat repeat))

(define mode/c (or/c 'enclosing 'case-sensitive 'case-insensitive 'multi-line 'not-multi-line))

(define (rx/mode-group expr #:mode (mode '(enclosing)))
  (let* ((mode-list (if (list? mode) mode (list mode)))
         (mode-hash (hash 'case-sensitive "-i" 'case-insensitive "i" 'multi-line "m" 'not-multi-line "-m"))
         (mode-string (string-append* (map (λ (m) (hash-ref mode-hash m "")) mode-list))))
    (format "(?~a:~a)" mode-string expr)))

(define rx/anchor-at-start "^")

(define (rx/string-prefix expr)
  (rx/and rx/anchor-at-start expr))

(define rx/anchor-at-end "$")

(define (rx/string-suffix expr)
  (rx/and expr rx/anchor-at-end))

(define (rx/string-exactly expr)
  (rx/string-suffix (rx/string-prefix expr)))

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
      (string-append* (map rx/escape (string->list char-or-string))))
     (else char-or-string))))

(define (rx/range from to)
  (format "~a-~a" from to))

(define (rx/match char-or-range . more)
  (format "[~a]" (string-append* (map rx/escape
                                      (cons char-or-range more)))))

(define (rx/not-match char-or-range . more)
  (format "[^~a]" (string-append* (map rx/escape
                                       (cons char-or-range more)))))

(define rx/^match rx/not-match)

(define (rx/match-range from to)
  (rx/match (rx/range from to)))

(define (rx/not-match-range from to)
  (rx/not-match (rx/range from to)))

(define rx/^match-range rx/not-match-range)

;; digits
;; digits ","
;; digits "," digits
(define (rx/repeat expr #:lower (lower 0) #:upper (upper #f))
  (format "~a{~a~a}" expr lower (if upper (format ",~a" upper) "")))

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
(define rx/ascii-alpha (string-append rx/ascii-alpha-lower rx/ascii-alpha-upper))
(define rx/ascii-numeric (rx/range #\0 #\9))
(define rx/ascii-alpha-numeric (string-append rx/ascii-alpha rx/ascii-numeric))

(define (rx/posix-class name)
  (format "[:~a:]" name))

(define (rx/match-posix-class name)
  (rx/match (rx/posix-class name)))

(define (rx/not-match-posix-class name)
  (rx/not-match (rx/posix-class name)))

(define rx/^match-posix-class rx/not-match-posix-class)

(define rx/posix-alpha-lower (rx/posix-class "lower"))
(define rx/posix-alpha-upper (rx/posix-class "upper"))
(define rx/posix-alpha (rx/posix-class "alpha"))
(define rx/posix-numeric (rx/posix-class "digit"))
(define rx/posix-alpha-numeric (rx/posix-class "alnum"))

(define rx/unicode-category/c
  (or/c 'C 'Cc 'Cf 'Cn 'Co 'Cs
        'L 'Ll 'Lm 'Lo 'Lt 'Lu 'L&
        'M 'Mc 'Me 'Mn
        'N 'Nd 'Nl 'No
        'P 'Pc 'Pd 'Pe 'Pf 'Pi 'Po 'Ps
        'S 'Sc 'Sk 'Sm 'So
        'Z 'Zl 'Zp 'Zs
        'any))

(define (rx/unicode-category category)
  (format "\\p{~a}" (if (symbol=? category 'any) "." (symbol->string category))))

(define (rx/unicode-not-category category)
  (format "\\P{~a}" (if (symbol=? category 'any) "." (symbol->string category))))

(define/contract (rx/group-ref n)
  (-> exact-nonnegative-integer? string?)
  (format "\\~a" n))

(module+ test

  (require rackunit
           rackunit/text-ui)

  (provide rx-test-suite)

  ;; ---------------------------------------------------------------------------------------------
  ;; Test Suite(s)
  ;; ---------------------------------------------------------------------------------------------

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
       (check-equal? (rx/repeat "ca" #:lower 2)
                     "ca{2}"))

     (test-case
         "function rx/repeat (ex8)"
       (check-equal? (rx/and (rx/repeat "ca" #:lower 2) "t")
                     "ca{2,}t"))

     (test-case
         "function rx/repeat (ex9)"
       (check-equal? (rx/and (rx/repeat "ca" #:upper 2) "t")
                     "ca{,2}t"))

     (test-case
         "function rx/repeat (ex10)"
       (check-equal? (rx/and (rx/repeat "ca" #:lower 1 #:upper 2) "t")
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
       (check-equal? (rx/and "c" (rx/group rx/char-any) (rx/group-ref 1) "t")
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
       (check-equal? (rx/one-or-more (rx/match-posix-class rx/posix-alpha-lower))
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
       (check-equal? (rx/one-or-more (rx/match "]a["))
                     "[]a[]+"))

     (test-case
         "function rx/match (ex30)"
       (check-equal? (rx/one-or-more (rx/match #\a #\^))
                     "[a^]+")
       (check-equal? (rx/one-or-more (rx/match "a^"))
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
       (check-equal? (rx/and (rx/mode-group "a" #:mode 'case-insensitive) (rx/match "tp"))
                     "(?i:a)[tp]"))

     (test-case
         "function rx/conditional (ex36)"
       (check-equal? (rx/+ (rx/conditional (rx/look-behind "c") "a" "b"))
                     "(?(?<=c)a|b)+"))

     (test-case
         "function rx/not-match (ex37)"
       (check-equal? (rx/one-or-more (rx/not-match #\^))
                     "[^^]+")
       (check-equal? (rx/one-or-more (rx/^match "^"))
                     "[^^]+"))))

     ;; ---------------------------------------------------------------------------------------------
     ;; Test Runner
     ;; ---------------------------------------------------------------------------------------------

     (run-tests rx-test-suite))
