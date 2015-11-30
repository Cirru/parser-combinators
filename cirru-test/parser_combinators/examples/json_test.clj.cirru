
ns parser-combinators.examples.json-test
  :require
    [] clojure.test :refer :all
    [] parser-combinators.core :refer :all
    [] parser-combinators.examples.json :refer :all

deftest parse-number-simple-test
  testing "|test parse number" $ is $ =
    parse-number
      assoc initial-state :code |12
    assoc initial-state :value |12

deftest parse-number-test
  testing "|test parse number" $ is $ =
    parse-number
      assoc initial-state :code |12.34
    assoc initial-state :value |12.34
