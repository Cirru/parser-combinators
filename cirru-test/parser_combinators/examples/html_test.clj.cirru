
ns parser-combinators.examples.html-test
  :require
    [] parser-combinators.core :refer :all
    [] parser-combinators.examples.html :refer :all
    [] clojure.test :refer :all

defn =state (a b)
  =
    assoc a :msg nil
    assoc b :msg nil

deftest parse-lt-test
  testing "|test parse lt" $ is $ =state
    parse-lt
      assoc initial-state :code "|<a"
    assoc initial-state :value "|<" :code "|a"

deftest parse-gt-test
  testing "|test parse gt" $ is $ =state
    parse-gt
      assoc initial-state :code "|>a"
    assoc initial-state :value "|>" :code "|a"

deftest parse-equal-test
  testing "|test parse equal" $ is $ =state
    parse-equal
      assoc initial-state :code "|=a"
    assoc initial-state :value "|=" :code "|a"

deftest parse-slash-test
  testing "|test parse slash" $ is $ =state
    parse-slash
      assoc initial-state :code "|/a"
    assoc initial-state :value "|/" :code "|a"

deftest parse-hyphen-test
  testing "|test parse hyphen" $ is $ =state
    parse-hyphen
      assoc initial-state :code "|-a"
    assoc initial-state :value "|-" :code "|a"

deftest parse-not-tag-test
  testing "|test parse not-tag" $ is $ =state
    parse-not-tag
      assoc initial-state :code "|ab"
    assoc initial-state :value "|a" :code "|b"

deftest parse-letter-test
  testing "|test parse letter" $ is $ =state
    parse-letter
      assoc initial-state :code "|ab"
    assoc initial-state :value "|a" :code "|b"

deftest parse-digit-test
  testing "|test parse digit" $ is $ =state
    parse-digit
      assoc initial-state :code "|1a"
    assoc initial-state :value "|1" :code "|a"

deftest parse-text-test
  testing "|test parse text" $ is $ =state
    parse-text
      assoc initial-state :code "|ab<"
    assoc initial-state :value "|ab" :code "|<"

deftest parse-seperation-test
  testing "|test parse seperation" $ is $ =state
    parse-seperation
      assoc initial-state :code "|  a"
    assoc initial-state :code "|a"

deftest parse-tag-name-test
  testing "|test parse tag-name" $ is $ =state
    parse-tag-name
      assoc initial-state :code "|a-b "
    assoc initial-state :value "|a-b" :code "| "

deftest parse-attr-test
  testing "|test parse attr" $ is $ =state
    parse-attr
      assoc initial-state :code "|a-b "
    assoc initial-state :value "|a-b" :code "| "

deftest parse-entry-test
  testing "|test parse entry" $ is $ =state
    parse-entry
      assoc initial-state :code "|a-b=\"\" "
    assoc initial-state :code "| "
      , :value $ [] "|a-b" |= |

deftest parse-attributes-test
  testing "|test parse attributes" $ is $ =state
    parse-attributes
      assoc initial-state :code "|a=\"true\" b=\"false\""
    assoc initial-state :code "|"
      , :value ([] ([] |a |= |true) ([] |b |= |false))

deftest parse-open-tag-test
  testing "|test parse open-tag" $ is $ =state
    parse-open-tag
      assoc initial-state :code "|<a>"
    assoc initial-state :code "|"
      , :value $ [] |a ([])

deftest parse-open-tag-with-attributes-test
  testing "|test parse open-tag-with-attributes" $ is $ =state
    parse-open-tag
      assoc initial-state :code "|<a b=\"c\">"
    assoc initial-state :code "|"
      , :value $ [] |a $ [] ([] |b |= |c)

deftest parse-close-tag-test
  testing "|test parse close-tag" $ is $ =state
    parse-close-tag
      assoc initial-state :code "|</a> "
    assoc initial-state :code "| " :value |a

deftest parse-self-close-tag-test
  testing "|test parse self-close-tag" $ is $ =state
    parse-self-close-tag
      assoc initial-state :code "|<a /> "
    assoc initial-state :code "| "
      , :value $ [] |a $ []

deftest parse-self-close-tag-2-test
  testing "|test parse self-close-tag-2" $ is $ =state
    parse-self-close-tag
      assoc initial-state :code "|<a b=\"c\"/> "
    assoc initial-state :code "| "
      , :value $ [] |a $ [] $ [] |b |= |c

deftest parse-html-test
  testing "|test parse html" $ is $ =state
    parse-html
      assoc initial-state :code "|a <a b=\"c\"/> b"
    assoc initial-state
      , :value $ [] "|a "
        [] |a $ [] $ [] |b |= |c
        , "| b"
