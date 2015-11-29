
ns parser-combinators.core-test
  :require
    [] clojure.test :refer :all
    [] parser-combinators.core :refer :all

deftest parse-eof-test
  testing "|test parse eof" $ is $ =
    parse-eof
      assoc initial-state :code |
    assoc initial-state :code |

deftest parse-open-paren-test
  testing "|test open paren" $ is $ =
    parse-open-paren
      assoc initial-state :code "|(a"
    assoc initial-state :code |a :value "|("

deftest parse-close-paren-test
  testing "|test close paren" $ is $ =
    parse-close-paren
      assoc initial-state :code "|)a"
    assoc initial-state :code |a :value "|)"

deftest parse-double-quote-test
  testing "|test double quote" $ is $ =
    parse-double-quote
      assoc initial-state :code "|\"a"
    assoc initial-state :code |a :value "|\""

deftest parse-backslash-test
  testing "|test backslash" $ is $ =
    parse-backslash
      assoc initial-state :code "|\\a"
    assoc initial-state :code |a :value "|\\"

deftest parse-whitespace-test
  testing "|test whitespace" $ is $ =
    parse-whitespace
      assoc initial-state :code "| a"
    assoc initial-state :code |a :value "| "

deftest parse-line-break-test
  testing "|test line break" $ is $ =
    parse-line-break
      assoc initial-state :code "|\na"
    assoc initial-state :code |a :value "|\n"

deftest parse-escaped-char-test
  testing "|test escaped char" $ is $ =
    parse-escaped-char
      assoc initial-state :code "|\\\\a"
    assoc initial-state :code |a :value "|\\"

deftest parse-blanks-test
  testing "|test blanks" $ is $ =
    parse-blanks
      assoc initial-state :code "|  a"
    assoc initial-state :code |a :value nil

deftest parse-newlines-test
  testing "|test newlines" $ is $ =
    parse-newlines
      assoc initial-state :code "|\n\n\na"
    assoc initial-state :code |a :value nil

deftest parse-token-special-test
  testing "|test token special" $ is $ =
    parse-token-special
      assoc initial-state :code "|\na"
    assoc initial-state :code |a :value "|\n"

deftest parse-token-special-negative-test
  testing "|test token special nagtive" $ is $ =
    parse-token-special
      assoc initial-state :code |ba
    assoc initial-state :code |a :value |b :failed true
      , :msg "|not in char list"

deftest parse-string-special-test
  testing "|test string special" $ is $ =
    parse-string-special
      assoc initial-state :code "|\na"
    assoc initial-state :code |a :value "|\n"

deftest parse-token-end-test
  testing "|test token end" $ is $ =
    parse-token-end
      assoc initial-state :code |
    assoc initial-state :code | :value nil

deftest parse-in-token-char-test
  testing "|test in token char" $ is $ =
    parse-in-token-char
      assoc initial-state :code |ab
    assoc initial-state :code |b :value |a :msg "|recorvered in not"

deftest parse-in-token-char-reverse-test
  testing "|test in token char reverse" $ is $ =
    parse-in-token-char
      assoc initial-state :code "|\nb"
    assoc initial-state :code |b :value "|\n" :failed true
      , :msg "|should not be this"

deftest parse-in-string-char-test
  testing "|test in string char" $ is $ =
    parse-in-string-char
      assoc initial-state :code |ab
    assoc initial-state :code |b :value |a :msg "|recorvered in not"

deftest parse-token-test
  testing "|test token" $ is $ =
    parse-token
      assoc initial-state :code |ab
    assoc initial-state :code | :value |ab :msg "|recorvered in not"

deftest parse-string-test
  testing "|test string" $ is $ =
    parse-string
      assoc initial-state :code "|\"a\""
    assoc initial-state :code | :value |a :msg "|recorvered in not"

deftest parse-empty-line-test
  testing "|parse empty line" $ is $ =
    parse-empty-line
      assoc initial-state :code "|\n  \n"
    assoc initial-state :value nil :code "|\n"

deftest parse-line-breaks-test
  testing "|parse line breaks" $ is $ =
    parse-line-breaks
      assoc initial-state :code "|\n\na"
    assoc initial-state :code |a :value nil

deftest parse-two-blanks-test
  testing "|parse two blanks" $ is $ =
    parse-two-blanks
      assoc initial-state :code "|  "
    assoc initial-state :code | :value 1

deftest parse-indentation-test
  testing "|parse indentation" $ is $ =
    parse-indentation
      assoc initial-state :code "|\n  a"
    assoc initial-state :code |a :value 1

deftest parse-indent-test
  testing "|parse indent" $ is $ =
    parse-indent
      assoc initial-state :code "|\n  a"
    assoc initial-state :indentation 1 :code |a

deftest parse-unindent-test
  testing "|parse unindent" $ is $ =
    parse-unindent
      assoc initial-state :code "|\n  a" :indentation 2
    assoc initial-state :indentation 1 :code |a

deftest parse-align-test
  testing "|parse align" $ is $ =
    parse-align
      assoc initial-state :code "|\n  a" :indentation 1
    assoc initial-state :indentation 1 :code |a
