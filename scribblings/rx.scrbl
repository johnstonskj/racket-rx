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

Regular expressions are wonderful tools and very powerful; however, complex expressions can be hard to write and harder
to debug. The goal of this crate is to provide a set of functions that compose a regular expression as a set of strings.
While this approach is more verbose, as can be seen in the examples below, it is more readable and easier to compose.

@verbatim|{
"((([A-Za-z]{3,9}:(?:\/\/)?)(?:[-;:&=\+\$,\w]+@)?[A-Za-z0-9.-]+|
(?:www.|[-;:&=\+\$,\w]+@)[A-Za-z0-9.-]+)(:[0-9]{0,5})?(#[\w]*)?
((?:\/[\+~%\/.\w\-_]*)?\??(?:[-\+=&;%@.\w_]*)#?(?:[.\!\/\\w]*))?)"
}|

@examples[#:eval example-eval
(rx/group
  (rx/or-group
    (rx/and
      (rx/group
        (rx/repeat rx/match-alpha #:lower 3 #:upper 9)
          ":"
          (rx/nc-group "//" #:repeat 'optional))
      (rx/nc-group
        (rx/match "-;:&=\\+\\$,\\w" #:repeat 'one-or-more) "@"
        #:repeat 'optional)
      (rx/match rx/range-alnum ".-"
        #:repeat 'one-or-more))
    (rx/and
      (rx/nc-group
        (rx/or ":www."
               (rx/and
                 (rx/match rx/range-alnum ".-"
                           #:repeat 'one-or-more)
                 "@")))
      (rx/match rx/range-alnum ".-"
                #:repeat 'one-or-more)))
  (rx/group ":" (rx/repeat rx/match-digit #:lower 0 #:upper 5) #:repeat 'optional)
  (rx/group "#" (rx/* (rx/match "\\w")) #:repeat 'optional)
  (rx/group
    (rx/non-capture-group
      "/"
      (rx/match "+~%/.\\w-_" #:repeat 'zero-or-more)
      #:repeat 'optional)
    (rx/? "\\?")
    (rx/nc-group (rx/match "-+=&;%@.\\w_" #:repeat 'zero-or-more))
    (rx/? "#")
    (rx/nc-group
      (rx/match ".!/\\w" #:repeat 'zero-or-more))
    #:repeat 'optional))
]

@;{============================================================================}
@section[]{Character Classes}

@defthing[rx/char-any string?]{
TBD
}

@deftogether[(
  @defthing[rx/cclass-digit string?]
  @defthing[rx/cclass-horizontal-whitespace string?]
  @defthing[rx/cclass-vertical-whitespace string?]
  @defthing[rx/cclass-whitespace string?]
  @defthing[rx/cclass-word-boundary string?]
  @defthing[rx/cclass-word-char string?]
)]{
TBD
}

@deftogether[(
  @defthing[rx/cclass-non-digit string?]
  @defthing[rx/cclass-non-horizontal-whitespace string?]
  @defthing[rx/cclass-non-vertical-whitespace string?]
  @defthing[rx/cclass-non-whitespace string?]
  @defthing[rx/cclass-non-word-boundary string?]
  @defthing[rx/cclass-non-word-char string?]
)]{
TBD
}

@subsection[]{Posix}

@deftogether[(
  @defthing[rx/pclass-lower string?]
  @defthing[rx/pclass-upper string?]
  @defthing[rx/pclass-alpha string?]
  @defthing[rx/pclass-digit string?]
  @defthing[rx/pclass-alnum string?]
  @defthing[rx/pclass-xdigit string?]
  @defthing[rx/pclass-word string?]
  @defthing[rx/pclass-blank string?]
  @defthing[rx/pclass-space string?]
  @defthing[rx/pclass-graph string?]
  @defthing[rx/pclass-print string?]
  @defthing[rx/pclass-cntrl string?]
  @defthing[rx/pclass-ascii string?]
)]{
TBD
}

@subsection[]{Unicode}

@defthing[rx/uchar-any string?]{
TBD
}

@deftogether[(
  @defthing[rx/uclass-one-data-unit string?]
  @defthing[rx/uclass-newlines string?]
  @defthing[rx/uclass-non-newlines string?]
)]{
TBD
}

@deftogether[(
  @defthing[rx/uclass-letter-lower string?]
  @defthing[rx/uclass-letter-upper string?]
  @defthing[rx/uclass-letter-title string?]
  @defthing[rx/uclass-letter-modifier string?]
  @defthing[rx/uclass-letter-no-other string?]
  @defthing[rx/uclass-letter-other string?]
  @defthing[rx/uclass-letter string?]
  @defthing[rx/uclass-number-decimal string?]
  @defthing[rx/uclass-number-letter string?]
  @defthing[rx/uclass-number-other string?]
  @defthing[rx/uclass-number string?]
  @defthing[rx/uclass-punctuation-open string?]
  @defthing[rx/uclass-punctuation-close string?]
  @defthing[rx/uclass-punctuation-quote-initial string?]
  @defthing[rx/uclass-punctuation-quote-final string?]
  @defthing[rx/uclass-punctuation-connector string?]
  @defthing[rx/uclass-punctuation-dash string?]
  @defthing[rx/uclass-punctuation-other string?]
  @defthing[rx/uclass-punctuation string?]
  @defthing[rx/uclass-mark-non-spacing string?]
  @defthing[rx/uclass-mark-space-combining string?]
  @defthing[rx/uclass-mark-enclosing string?]
  @defthing[rx/uclass-mark string?]
  @defthing[rx/uclass-symbol-currency string?]
  @defthing[rx/uclass-symbol-modifier string?]
  @defthing[rx/uclass-symbol-math string?]
  @defthing[rx/uclass-symbol-other string?]
  @defthing[rx/uclass-symbol string?]
  @defthing[rx/uclass-separator-line string?]
  @defthing[rx/uclass-separator-paragraph string?]
  @defthing[rx/uclass-separator-space string?]
  @defthing[rx/uclass-separator string?]
  @defthing[rx/uclass-other-control string?]
  @defthing[rx/uclass-other-format string?]
  @defthing[rx/uclass-other-surrogate string?]
  @defthing[rx/uclass-other-not-assigned string?]
  @defthing[rx/uclass-other-private-use string?]
  @defthing[rx/uclass-other string?]
  @defthing[rx/uclass-any string?]
)]{
TBD
}

@deftogether[(
  @defthing[rx/uclass-non-letter-lower string?]
  @defthing[rx/uclass-non-letter-upper string?]
  @defthing[rx/uclass-non-letter-title string?]
  @defthing[rx/uclass-non-letter-modifier string?]
  @defthing[rx/uclass-non-letter-no-other string?]
  @defthing[rx/uclass-non-letter-other string?]
  @defthing[rx/uclass-non-letter string?]
  @defthing[rx/uclass-non-number-decimal string?]
  @defthing[rx/uclass-non-number-letter string?]
  @defthing[rx/uclass-non-number-other string?]
  @defthing[rx/uclass-non-number string?]
  @defthing[rx/uclass-non-punctuation-open string?]
  @defthing[rx/uclass-non-punctuation-close string?]
  @defthing[rx/uclass-non-punctuation-quote-initial string?]
  @defthing[rx/uclass-non-punctuation-quote-final string?]
  @defthing[rx/uclass-non-punctuation-connector string?]
  @defthing[rx/uclass-non-punctuation-dash string?]
  @defthing[rx/uclass-non-punctuation-other string?]
  @defthing[rx/uclass-non-punctuation string?]
  @defthing[rx/uclass-non-mark-non-spacing string?]
  @defthing[rx/uclass-non-mark-space-combining string?]
  @defthing[rx/uclass-non-mark-enclosing string?]
  @defthing[rx/uclass-non-mark string?]
  @defthing[rx/uclass-non-symbol-currency string?]
  @defthing[rx/uclass-non-symbol-modifier string?]
  @defthing[rx/uclass-non-symbol-math string?]
  @defthing[rx/uclass-non-symbol-other string?]
  @defthing[rx/uclass-non-symbol string?]
  @defthing[rx/uclass-non-separator-line string?]
  @defthing[rx/uclass-non-separator-paragraph string?]
  @defthing[rx/uclass-non-separator-space string?]
  @defthing[rx/uclass-non-separator string?]
  @defthing[rx/uclass-non-other-control string?]
  @defthing[rx/uclass-non-other-format string?]
  @defthing[rx/uclass-non-other-surrogate string?]
  @defthing[rx/uclass-non-other-not-assigned string?]
  @defthing[rx/uclass-non-other-private-use string?]
  @defthing[rx/uclass-non-other string?]
)]{
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

@defproc[(rx/ranges [from (cons/c char? char?)] ...+) string?]{
Create multiple @racket[rx/range]s at the same time.

@examples[
  #:eval example-eval
  (rx/ranges '(#\A . #\F) '(#\F . #\A))
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

@subsection[]{Named Ranges}

@deftogether[(
  @defthing[rx/range-lower string?]
  @defthing[rx/match-lower string?]
)]{
A pre-defined range, and match, for ASCII lower-case letter characters. Equivalent to @tt{[a-z]}.
}

@deftogether[(
  @defthing[rx/range-upper string?]
  @defthing[rx/match-upper string?]
)]{
A pre-defined range, and match, for ASCII upper-case letter characters. Equivalent to @tt{[A-Z]}.
}

@deftogether[(
  @defthing[rx/range-alpha string?]
  @defthing[rx/match-alpha string?]
)]{
A pre-defined range, and match, for ASCII upper-case and lower-case letter characters. Equivalent to @tt{[A-Za-z]}.
}

@deftogether[(
  @defthing[rx/range-digit string?]
  @defthing[rx/match-digit string?]
)]{
A pre-defined range, and match, for ASCII decimal digit characters. Equivalent to @tt{[0-9]}.
}

@deftogether[(
  @defthing[rx/range-alnum string?]
  @defthing[rx/match-alnum string?]
)]{
A pre-defined range, and match, for ASCII alpha-numeric characters. Equivalent to @tt{[A-Za-z0-9]}.
}

@deftogether[(
  @defthing[rx/range-xdigit string?]
  @defthing[rx/match-xdigit string?]
)]{
A pre-defined range, and match, for ASCII hex digit characters. Equivalent to @tt{[0-9A-Fa-f]}.
}

@deftogether[(
  @defthing[rx/range-word string?]
  @defthing[rx/match-word string?]
)]{
A pre-defined range, and match, for ASCII @italic{word} characters. Equivalent to @tt{[A-Fa-f0-9_]}.
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
  (rx/repeat "maybe" #:lower 1)
  (rx/repeat "maybe" #:upper 1)
  (rx/repeat "maybe" #:lower 1 #:upper 3)
  (rx/repeat "maybe" #:lower 3 #:upper 1)
  (rx/repeat "maybe")
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
  (rx/repeat-safely #\a #:lower 1 #:upper 3)
  (rx/repeat-safely "(abc)" #:lower 1 #:upper 3)
  (rx/repeat-safely "[abc]" #:lower 1 #:upper 3)
  (rx/repeat-safely "maybe" #:lower 1 #:upper 3)
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
  @defproc[(rx/nc-group
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
