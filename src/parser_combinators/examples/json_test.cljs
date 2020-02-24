
(ns parser-combinators.examples.json-test
  (:require [clojure.test :refer [deftest is testing]]
            [parser-combinators.examples.json :as json-parser]
            [parser-combinators.core :as pc]))

(deftest
 parse-array-test
 (testing
  "test parse array"
  (is
   (=
    (json-parser/parse-array (assoc pc/initial-state :code "[1,2,[3,4]]"))
    (assoc pc/initial-state :value [1 2 [3 4]])))))

(deftest
 parse-entry-test
 (testing
  "test parse entry"
  (is
   (=
    (json-parser/parse-entry (assoc pc/initial-state :code "\"a\": 1"))
    (assoc pc/initial-state :value ["a" 1] :msg "recorvered in not")))))

(deftest
 parse-json-test
 (testing
  "test json array"
  (is
   (=
    (json-parser/parse-array (assoc pc/initial-state :code "[1,2,{\"a\": \"\"}]"))
    (assoc pc/initial-state :value [1 2 [["a" ""]]] :msg "recorvered in not")))))

(deftest
 parse-number-simple-test
 (testing
  "test parse simple number"
  (is
   (=
    (json-parser/parse-number (assoc pc/initial-state :code "12"))
    (assoc pc/initial-state :value 12)))))

(deftest
 parse-number-test
 (testing
  "test parse number"
  (is
   (=
    (json-parser/parse-number (assoc pc/initial-state :code "12.34"))
    (assoc pc/initial-state :value 12.34)))))

(deftest
 parse-object-test
 (testing
  "test parse object"
  (is
   (=
    (json-parser/parse-object (assoc pc/initial-state :code "{\"a\": 1, \"b\": \"c\"}"))
    (assoc pc/initial-state :value [["a" 1] ["b" "c"]] :msg "recorvered in not")))))

(deftest
 parse-seperation-complicated-test
 (testing
  "test parse complicated seperation"
  (is
   (= (json-parser/parse-seperation (assoc pc/initial-state :code " \n ")) pc/initial-state))))

(deftest
 parse-seperation-test
 (testing
  "test parse seperation"
  (is
   (= (json-parser/parse-seperation (assoc pc/initial-state :code "  ")) pc/initial-state))))
