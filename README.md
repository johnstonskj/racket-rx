# Racket Package rx

This package provides a *functional regular expression* experience. While this may be unnecessary for small expressions it
can be valuable in the composition of large ones.

[![raco pkg install rx](https://img.shields.io/badge/raco%20pkg%20install-rx-blue.svg)](http://pkgs.racket-lang.org/package/rx)
[![Documentation](https://img.shields.io/badge/raco%20docs-rx-blue.svg)](http://docs.racket-lang.org/rx/index.html)
[![Racket](https://github.com/johnstonskj/racket-rx/actions/workflows/racket.yml/badge.svg)](https://github.com/johnstonskj/racket-rx/actions/workflows/racket.yml)
[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-rx.svg?style=flat-square)](https://github.com/johnstonskj/racket-rx/releases)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-rx.svg)](https://github.com/johnstonskj/racket-rx/stargazers)

* Character Classes
* Composition: And/Or
* Matches and Ranges
* Repetition
* Anchors
* Grouping
* Conditionals
* Parameters

## Example

The following example shows the use of a simple expression with the functions `rx/with-mode` to set the

```racket
(require rx)

(display
  (pregexp
    (rx/with-mode
      (rx/string-prefix (rx/or-group "hello" "hi" "g'day"))
      #:mode 'case-insensitive)))
```

This *should* result in the string `#px"(?i:^(hello|hi|g'day))"`.

## Changes

**Version 1.0**

This release provides:

1. Major structural components: and, or, groups, matches, and ranges.
1. Character classes, plain regex, Posix and Unicode.
1. Tests based on the Racket documentation.
1. Documentation, descriptions are incomplete but all procedures are all covered.
