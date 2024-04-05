#lang info
(define collection "rx")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib" "rackunit-lib"))
(define scribblings '(("scribblings/rx.scrbl" (multi-page))))
(define pkg-desc "Functional Regular Expressions")
(define version "1.0")
(define pkg-authors '(johnstonskj))
(define license 'Apache-2.0)
