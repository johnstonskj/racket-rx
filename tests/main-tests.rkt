#lang racket/base

(require rackunit
         rackunit/text-ui
         "../main.rkt")

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

(run-tests rx-test-suite)
