
# Parser Combinators in Clojure

Clojure library to provide simple parser combinators solution.

## Usage

```clojure
[mvc-works/parser-combinators "0.0.1"]
```

```clojure
'[parser-combinators.core :refer :all]
```

Examples: JSON parser(TODO), indentation parser(TODO).

#### Exposed APIs

The structure of the whole state(`:x`, `:y` not in use yet):

```clojure
(def initial-state
 {:y 0,
  :value nil,
  :msg "initial",
  :code "",
  :indentation 0,
  :x 0,
  :tab "",
  :failed false})
```

Some combinators to build larger parser:

```clojure
combine-some
combine-asterisk
combine-chain
combine-alternate
combine-or
combine-opposite
combine-peek
combine-times
```

Special function to handle values:

```clojure
handle-value
```

Generators of some very simple parser:

```clojure
generate-char
generate-char-in
```

Some parsers defined in parsing Cirru, you may use them.
However they may be not very suitable to many of your needs:

```clojure
parse-eof
parse-open-paren
parse-close-paren
parse-double-quote
parse-whitespace
parse-backslash
parse-line-break
parse-escaped-char
parse-blanks
parse-newlines
parse-token-special
parse-string-special
parse-token-end
parse-in-string-char
parse-in-token-char
parse-in-string-char
parse-string
parse-token
parse-empty-line
parse-line-breaks
parse-two-blanks
parse-indentation
parse-indent
parse-unindent
parse-align
```

## Where's Clojure code?

It's generated from Sepal.clj , try this command:

```bash
lein cirru-sepal
```

## Bugs

Or, to be improved:

* error messages look very bad

## License

Copyright © 2015 jiyinyiyong

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
