
# Parser Combinators in Clojure

Clojure library to provide simple parser combinators solution.

## Usage

[![Clojars Project](http://clojars.org/mvc-works/parser-combinators/latest-version.svg)](http://clojars.org/mvc-works/parser-combinators)

```clojure
[mvc-works/parser-combinators "0.0.3"]
```

```clojure
'[parser-combinators.core :refer :all]
```

Examples:

* [simplified JSON parser][json-example]
* [simplified HTML parser][html-example]
* indentation parser(TODO)
* S-expression parser(TODO)
* markdown parser(TODO)
* Old S-expression parser https://github.com/Cirru/minifier.clj
* Old indentation parser in Cirru https://github.com/Cirru/parser-combinator.clj

[json-example]: https://github.com/mvc-works/parser-combinators/blob/master/cirru-src/parser_combinators/examples/json.clj.cirru
[html-example]: https://github.com/mvc-works/parser-combinators/blob/master/cirru-src/parser_combinators/examples/html.clj.cirru

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
combine-interleave
combine-or
combine-opposite
combine-peek
combine-times
combine-optional
```

Special function to handle values:

```clojure
handle-value
transform-value
```

Generators of some very simple parser:

```clojure
generate-char
generate-char-in
generate-chars
generate-char-match
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
