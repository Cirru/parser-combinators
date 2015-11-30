
ns parser-combinators.examples.json-test
  :require
    [] clojure.test :refer :all
    [] parser-combinators.core :refer :all
    [] parser-combinators.examples.json :refer :all

deftest parse-number-simple-test
  testing "|test parse simple number" $ is $ =
    parse-number
      assoc initial-state :code |12
    assoc initial-state :value 12

deftest parse-number-test
  testing "|test parse number" $ is $ =
    parse-number
      assoc initial-state :code |12.34
    assoc initial-state :value 12.34

deftest parse-seperation-test
  testing "|test parse seperation" $ is $ =
    parse-seperation
      assoc initial-state :code "|  "
    , initial-state

deftest parse-seperation-complicated-test
  testing "|test parse complicated seperation" $ is $ =
    parse-seperation
      assoc initial-state :code "| \n "
    , initial-state

deftest parse-entry-test
  testing "|test parse entry" $ is $ =
    parse-entry
      assoc initial-state :code "|\"a\": 1"
    assoc initial-state :value ([] |a 1) :msg "|recorvered in not"

deftest parse-object-test
  testing "|test parse object" $ is $ =
    parse-object
      assoc initial-state :code "|{\"a\": 1, \"b\": \"c\"}"
    assoc initial-state
      , :value $ [] ([] |a 1) ([] |b |c)
      , :msg "|recorvered in not"

deftest parse-array-test
  testing "|test parse array" $ is $ =
    parse-array
      assoc initial-state :code "|[1,2,[3,4]]"
    assoc initial-state
      , :value $ [] 1 2 $ [] 3 4

deftest parse-json-test
  testing "|test json array" $ is $ =
    parse-array
      assoc initial-state :code "|[1,2,{\"a\": \"\"}]"
    assoc initial-state
      , :value $ [] 1 2 $ [] $ [] |a |
      , :msg "|recorvered in not"
