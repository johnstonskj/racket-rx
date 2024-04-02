#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          rx
          (for-label racket/base
                     rx))

@;{============================================================================}

@(define example-eval (make-base-eval '(require rx)))

@;{============================================================================}

@title[#:version  "0.1.0"]{Functional RegExp}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]
@defmodule[rx]

@;{============================================================================}
@section[]{Character Classes}

@subsection[]{Posix}

@defthing[posix-class/c contract?]{TBD}

@defproc[(rx/posix-class->string [cls posix-class/c]) string?]{
TBD
}

@defproc[(rx/match-posix-class [cls posix-class/c]) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/match-posix-class 'alpha)
]
}

@deftogether[(
  @defproc[(rx/not-match-posix-class [cls posix-class/c]) string?]
  @defproc[(rx/^match-posix-class [cls posix-class/c]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/not-match-posix-class 'lower)
  (rx/^match-posix-class 'digit)
]
}

@subsection[]{Unicode}

@defthing[unicode-category/c contract?]{TBD}

@defproc[(rx/unicode-category->string [cat unicode-category/c]) string?]{
TBD
}

@defproc[(rx/unicode-category [cat unicode-category/c]) string?]{
TBD
}

@defproc[(rx/not-unicode-category [cat unicode-category/c]) string?]{
TBD
}

@;{============================================================================}
@section[]{And and Or}

@defproc[(rx/and [expr string?]) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/and "a" "b" "cd")
]
}

@deftogether[(
  @defthing[rx/or-operator string?]
  @defproc[(rx/or [expr string?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/or "a" "b" "cd")
]
}

@;{============================================================================}
@section[]{Match and Range}

@deftogether[(
  @defthing[rx/range-operator string?]
  @defproc[(rx/range [from char?] [to char?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/range #\A #\F)
]
}

@defthing[rx/negation-operator string?]

@defproc[(rx/match [expr string?] ...) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/match "a" "b" "cd")
]
}

@deftogether[(
  @defproc[(rx/not-match [expr string?] ...) string?]
  @defproc[(rx/^match [expr string?] ...) string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/not-match "a" "b" "cd")
  (rx/^match "a" "b" "cd")
]
}

@defproc[(rx/match-range [from char?] [to char?]) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/match-range #\x #\z)
]
}

@deftogether[(
  @defproc[(rx/not-match-range [from char?] [to char?]) string?]
  @defproc[(rx/^match-range [from char?] [to char?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/not-match-range #\g #\p)
  (rx/^match-range #\g #\p)
]
}


@defproc[(rx/escape
          [val (or/c char? string?)]
          [location (or/c 'in-match 'outside-match) 'in-match]) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/escape "-")
  (rx/escape "|")
  (rx/escape "-" 'outside-match)
  (rx/escape "|" 'outside-match)
]
}

@;{============================================================================}
@section[]{Repetition}

@deftogether[(
  @defproc[(rx/optional [expr string?]) string?]
  @defproc[(rx/? [expr string?]) string?]
)]
{
TBD

@examples[
  #:eval example-eval
  (rx/optional "maybe")
  (rx/? "maybe")
]
}

@deftogether[(
  @defproc[(rx/zero-or-more [expr string?]) string?]
  @defproc[(rx/* [expr string?]) string?]
)]
{
TBD

@examples[
  #:eval example-eval
  (rx/zero-or-more "maybe")
  (rx/* "maybe")
]
}

@deftogether[(
  @defproc[(rx/one-or-more [expr string?]) string?]
  @defproc[(rx/+ [expr string?]) string?]
)]
{
TBD

@examples[
  #:eval example-eval
  (rx/one-or-more "maybe")
  (rx/+ "maybe")
]
}

@defproc[(rx/repeat
          [expr string?]
          [#:lower lower exact-nonnegative-integer? 0]
          [#:upper upper (or/c exact-nonnegative-integer? #f) #f]) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/repeat "maybe")
  (rx/repeat "maybe" #:lower 1)
  (rx/repeat "maybe" #:upper 1)
  (rx/repeat "maybe" #:lower 1 #:upper 3)
]
}

@;{============================================================================}
@section[]{Groups and Anchors}

@defthing[group-repeat/c contract?]{
TBD
}

;; rx/group rx/or-group rx/and-group

@deftogether[(
  @defthing[rx/anchor-at-start string?]
  @defproc[(rx/string-prefix [expr string?]) string?]
)]{
TBD
}

@deftogether[(
  @defthing[rx/anchor-at-end string?]
  @defproc[(rx/string-suffix [expr string?]) string?]
)]{
TBD
}

@defproc[(rx/string-exactly [expr string?]) string?]{
TBD
}

@;{============================================================================}
@section[]{Conditionals}

;; rx/conditional

;; rx/look-ahead  rx/=>
;; rx/not-look-ahead  rx/!=>

;; rx/look-behind  rx/<=
;; rx/not-look-behind  rx/<!=

;; rx/group-ref

@;{============================================================================}
@section[]{Modes and Parameters}

;; rx/has-named-groups
