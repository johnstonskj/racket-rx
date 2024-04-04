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

The most primitive combination operators, @bold{@tt{and}} and  @bold{@tt{or}}. In regular expressions @italic{and}-ing
two sub-expressions simply concatenates them.

@deftogether[(
  @defproc[(rx/and [expr string?] ...+) string?]
  @defproc[(rx/& [expr string?] ...+) string?]
)]{
Returns a new expression that concatenates the list of sub-expressions in @italic{expr}. The function @racket[rx/&] is
simply an alias for @racket[rx/and].

@examples[
  #:eval example-eval
  (rx/and "a" "b" "cd")
  (rx/& "a" "b" "cd")
]
}

@defthing[rx/or-operator string?]{
The operator that separates sub-expressions @italic{or}-ed together.
}

@defproc[(rx/or [expr string?] ...+) string?]{
Returns a new expression that combines the list of sub-expressions in @italic{expr} separated by the
@racket[rx/or-operator].

@examples[
  #:eval example-eval
  (rx/or "a" "b" "cd")
]
}

@;{============================================================================}
@section[]{Match and Range}

A range expression is specified as @tt{char1 - char2}, where all characters between the first and second character are
considered, inclusively. A match expression is a set of characters, ranges, where any member of the set matches.

@defthing[rx/range-operator string?]{
The range operator is the @tt{-} in @tt{char1 - char2}.
}
@defthing[char>=/c contract?]{
A contract used in @racket[rx/range] which ensures the second parameter is greater than, or equal to, the first.
}

@defproc[(rx/range [from char?] [to (and/c char? (char>=/c from))]) string?]{
Create a character range including all characters in @italic{from} ... @italic{to}.

@examples[
  #:eval example-eval
  (rx/range #\A #\F)
  (rx/range #\F #\A)
]
}

@defproc[(rx/range? [expr string?]) boolean?]{
A predicate that returns true if @italic{expr} is a valid range string.
}

@defproc[(rx/match [expr (or/c char? string?)] ...) string?]{
Create a match expression that matches any character specified in the match set. The set can be denoted as single
@racket[char?]s, single-character @racket[string?]s, or unbounded strings.

@examples[
  #:eval example-eval
  (rx/match "a" "b" "cd")
  (rx/match #\a #\b "cd")
]
}

@defproc[(rx/match? [expr string?]) boolean?]{
A predicate that returns true if @italic{expr} is a valid match string.
}

@defthing[rx/not-match-operator string?]{
The string used as an operator to negate a match expression.
}

@deftogether[(
  @defproc[(rx/not-match [expr string?] ...) string?]
  @defproc[(rx/^match [expr string?] ...) string?]
)]{
Create a negated match expression that matches any character @italic{not} specified in the match set.

@examples[
  #:eval example-eval
  (rx/not-match "a" "b" "cd")
  (rx/^match "a" "b" "cd")
]
}

@defproc[(rx/not-match? [expr string?]) boolean?]{
A predicate that returns true if @italic{expr} is a valid negated match string.
}

@defproc[(rx/match-range [from char?] [to char?]) string?]{
Create a match expression which matches the range of characters in @italic{from} ... @italic{to}.

@examples[
  #:eval example-eval
  (rx/match-range #\x #\z)
  ((compose rx/match rx/range) #\x #\z)
]
}

@deftogether[(
  @defproc[(rx/not-match-range [from char?] [to char?]) string?]
  @defproc[(rx/^match-range [from char?] [to char?]) string?]
)]{
Create a match expression which matches any character except those in @italic{from} ... @italic{to}.


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

Repetition allows for a single expression to match more than once, or not at all. There are multiple ways in which
repetition may be indicated, using explicit procedures, using the @tt{#:repeat} argument on group procedures, or the
more specific arguments to the @racket[rx/repeat] procedure. These options are compared in the following table.

@tabular[#:style 'boxed
         #:sep @hspace[2]
         #:row-properties '(bottom-border ())
         (list (list @bold{@tt{rx/...}}    @bold{@tt{#:repeat}}  @bold{@tt{rx/repeat}})
               (list @tt{rx/optional}      @tt{'optional}        @tt{#:lower 0 #:upper 1})
               (list @tt{rx/zero-or-more}  @tt{'zero-or-more}    @tt{#:lower 0 #:upper #f})
               (list @tt{rx/one-or-more}   @tt{'one-or-more}     @tt{#:lower 1 #:upper #f})
               (list "N/A"                 @tt{'one}             @tt{#:lower 1 #:upper 1}))]


@deftogether[(
  @defproc[(rx/optional [expr string?]) string?]
  @defproc[(rx/? [expr string?]) string?]
)]{
This creates a new expression denoting that @italic{expr} may be matched @tt{0..1} times.

@examples[
  #:eval example-eval
  (rx/optional "maybe")
  (rx/? "maybe")
]
}

@deftogether[(
  @defproc[(rx/zero-or-more [expr string?]) string?]
  @defproc[(rx/* [expr string?]) string?]
)]{
This creates a new expression denoting that @italic{expr} may be matched @tt{0..∞} times. (Kleene star).

@examples[
  #:eval example-eval
  (rx/zero-or-more "maybe")
  (rx/* "maybe")
]
}

@deftogether[(
  @defproc[(rx/one-or-more [expr string?]) string?]
  @defproc[(rx/+ [expr string?]) string?]
)]{
This creates a new expression denoting that @italic{expr} may be matched @tt{1..∞} times. (Kleene Plus).

@examples[
  #:eval example-eval
  (rx/one-or-more "maybe")
  (rx/+ "maybe")
]
}

@defproc[(rx/repeat
          [expr string?]
          [#:lower lower (or/c exact-nonnegative-integer? #f) #f]
          [#:upper upper (or/c exact-nonnegative-integer? #f) #f]) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/repeat "maybe")
  (rx/repeat "maybe" #:lower 1)
  (rx/repeat "maybe" #:upper 1)
  (rx/repeat "maybe" #:lower 1 #:upper 3)
  (rx/repeat "maybe" #:lower 3 #:upper 1)
]
}

@defproc[(rx/repeat-safe?
          [expr string?]) boolean?]{
TBD

@examples[
  #:eval example-eval
  (rx/repeat-safe? #\a)
  (rx/repeat-safe? "(abc)")
  (rx/repeat-safe? "[abc]")
  (rx/repeat-safe? "maybe")
]
}

@defproc[(rx/repeat-safely
          [expr string?]
          [#:lower lower exact-nonnegative-integer? 0]
          [#:upper upper (or/c exact-nonnegative-integer? #f) #f]) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/repeat-safely #\a)
  (rx/repeat-safely "(abc)")
  (rx/repeat-safely "[abc]")
  (rx/repeat-safely "maybe")
]
}

@;{============================================================================}
@section[]{Anchors}

@deftogether[(
  @defthing[rx/anchor-at-start string?]
  @defproc[(rx/string-prefix [expr string?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/string-prefix "Hello")
]
}

@deftogether[(
  @defthing[rx/anchor-at-end string?]
  @defproc[(rx/string-suffix [expr string?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/string-suffix "goodbye")
]
}

@defproc[(rx/string-exactly [expr string?]) string?]{
TBD

@examples[
  #:eval example-eval
  (rx/string-exactly "Hello and Goodbye")
]
}

@;{============================================================================}
@section[]{Grouping}

@defthing[group-repeat/c contract?]{
TBD
}

@deftogether[(
  @defproc[(rx/group? [expr string?]) boolean?]
  @defproc[(rx/group
            [expr string?] ...+
            [#:repeat repeat group-repeat/c 'one])
           string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/group "cat")
  (rx/group "cat" #:repeat 'optional)
]
}

@defproc[(rx/and-group
          [expr string?] ...+
          [#:repeat repeat group-repeat/c 'one])
         string?]{
TBD

@examples[
  #:eval example-eval
  (rx/and-group "cat" "dog")
  (rx/and-group "cat" "dog" #:repeat 'optional)
]
}

@defproc[(rx/or-group
          [expr string?] ...+
          [#:repeat repeat group-repeat/c 'one])
         string?]{
TBD

@examples[
  #:eval example-eval
  (rx/or-group "cat" "dog")
  (rx/or-group "cat" "dog" #:repeat 'optional)
]
}

@defproc[(rx/special-group? [expr string?]) boolean?]{
TBD

@examples[
  #:eval example-eval
  (rx/special-group? (rx/group "cat"))
  (rx/special-group? (rx/non-capture-group "cat"))
]
}

@deftogether[(
  @defproc[(rx/non-capture-group? [expr string?]) boolean?]
  @defproc[(rx/non-capture-group
            [expr string?] ...+
            [#:repeat repeat group-repeat/c 'one])
           string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/non-capture-group "cat")
  (rx/non-capture-group "cat" #:repeat 'optional)
  (rx/non-capture-group? (rx/non-capture-group "cat"))
]
}

@deftogether[(
  @defproc[(rx/named-group? [expr string?]) boolean?]
  @defproc[(rx/named-group
            [expr string?] ...+
            [#:name name string?]
            [#:repeat repeat group-repeat/c 'one])
           string?]
)]{
TBD

@examples[
  #:eval example-eval
  (rx/named-group (rx/or "cat" "dog") #:name "animal")
  (rx/named-group (rx/or "cat" "dog") #:name "animal" #:repeat 'optional)
  (rx/named-group? (rx/named-group (rx/or "cat" "dog") #:name "animal"))
  (rx/special-group? (rx/named-group (rx/or "cat" "dog") #:name "animal"))
]
}

@subsection[]{Modes}

@defthing[mode/c contract?]{
TBD
}

@defproc[(rx/with-mode
          [expr string?] ...+
          [#:mode modes (or/c mode/c (listof mode/c))])
         string?]{
TBD

@examples[
  #:eval example-eval
  (rx/with-mode (rx/or "cat" "dog") #:mode 'case-insensitive)
]
}

@;{============================================================================}
@section[]{Conditionals}

@deftogether[(
  @defproc[(rx/conditional
            [test string?]
            [if-true string?]
            [if-false (or/c string? #f) #f])
           string?]
  @defproc[(rx/if
            [test string?]
            [if-true string?]
            [if-false (or/c string? #f) #f])
           string?]
)]{
TBD

@examples[
  #:eval example-eval
  '()
]
}

@deftogether[(
  @defproc[(rx/look-ahead [expr string?]) string?]
  @defproc[(rx/=> [expr string?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  '()
]
}

@deftogether[(
  @defproc[(rx/not-look-ahead [expr string?]) string?]
  @defproc[(rx/!=> [expr string?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  '()
]
}

@deftogether[(
  @defproc[(rx/look-behind [expr string?]) string?]
  @defproc[(rx/<= [expr string?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  '()
]
}

@deftogether[(
  @defproc[(rx/not-look-behind [expr string?]) string?]
  @defproc[(rx/<!= [expr string?]) string?]
)]{
TBD

@examples[
  #:eval example-eval
  '()
]
}

@deftogether[(
  @defproc[(rx/group-ref [n integer?]) string?]
)]{
TBD

The type of @tt{n} depends on the parameter @racket[rx/has-relative-group-refs]; if @racket[#t] the type allows negative
and positive values, otherwise it only allows positive.

The form of a group reference depends on the value of the parameter @racket[rx/group-ref-form].

@examples[
  #:eval example-eval
  (parameterize ((rx/group-ref-form 'grouped))
    (rx/group-ref 1))
  (parameterize ((rx/group-ref-form 'escaped))
    (rx/group-ref 1))
  (parameterize ((rx/group-ref-form 'grouped))
    (rx/conditional (rx/group-ref 1) "-bird" "bird"))
  (parameterize ((rx/group-ref-form 'escaped))
    (rx/conditional (rx/group-ref 1) "-bird" "bird"))
]
}

@;{============================================================================}
@section[]{Parameters}

@defthing[named-group-form/c contract?]{
TBD

@itemlist[
  @item[]{@racket['unsupported] -- ...}
  @item[]{@racket['quoted] -- ... @tt{(?'name'...)}.}
  @item[]{@racket['bracketed] -- ... @tt{(?<name>...)}.}
  @item[]{@racket['p-bracketed] -- ... @tt{(?P<name>...)}.}
]
}

@defparam[rx/named-group-form form named-group-form/c #:value 'unsupported]{
TBD
}

@defproc[(rx/has-named-groups ) boolean?]{
TBD
}

@defthing[group-ref-form/c contract?]{
TBD

@itemlist[
  @item[]{@racket['grouped] -- ... @tt{(1)}.}
  @item[]{@racket['escaped] -- ... @tt{\\1}.}
]
}

@defparam[rx/group-ref-form form group-ref-form/c #:value 'escaped]{
TBD
}

@defparam[rx/has-named-group-refs named-refs boolean? #:value #f]{
TBD

@tt{(name)}

Implies group reference form is @racket['grouped].
}

@defparam[rx/has-relative-group-refs relative-refs boolean? #:value #f]{
TBD

@tt{(-1)} or @tt{(+1)}


Implies group reference form is @racket['grouped].
}
