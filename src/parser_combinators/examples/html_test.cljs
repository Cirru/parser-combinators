
(ns parser-combinators.examples.html-test
  (:require [parser-combinators.core :as pc]
            [parser-combinators.examples.html :as html-parser]
            [clojure.test :refer [deftest is testing]]))

(defn =state [a b] (= (assoc a :msg nil) (assoc b :msg nil)))

(deftest
 parse-lt-test
 (testing
  "test parse lt"
  (is
   (=state
    (html-parser/parse-lt (assoc pc/initial-state :code "<a"))
    (assoc pc/initial-state :value "<" :code "a")))))

(deftest
 parse-gt-test
 (testing
  "test parse gt"
  (is
   (=state
    (html-parser/parse-gt (assoc pc/initial-state :code ">a"))
    (assoc pc/initial-state :value ">" :code "a")))))

(deftest
 parse-equal-test
 (testing
  "test parse equal"
  (is
   (=state
    (html-parser/parse-equal (assoc pc/initial-state :code "=a"))
    (assoc pc/initial-state :value "=" :code "a")))))

(deftest
 parse-slash-test
 (testing
  "test parse slash"
  (is
   (=state
    (html-parser/parse-slash (assoc pc/initial-state :code "/a"))
    (assoc pc/initial-state :value "/" :code "a")))))

(deftest
 parse-hyphen-test
 (testing
  "test parse hyphen"
  (is
   (=state
    (html-parser/parse-hyphen (assoc pc/initial-state :code "-a"))
    (assoc pc/initial-state :value "-" :code "a")))))

(deftest
 parse-not-tag-test
 (testing
  "test parse not-tag"
  (is
   (=state
    (html-parser/parse-not-tag (assoc pc/initial-state :code "ab"))
    (assoc pc/initial-state :value "a" :code "b")))))

(deftest
 parse-letter-test
 (testing
  "test parse letter"
  (is
   (=state
    (html-parser/parse-letter (assoc pc/initial-state :code "ab"))
    (assoc pc/initial-state :value "a" :code "b")))))

(deftest
 parse-digit-test
 (testing
  "test parse digit"
  (is
   (=state
    (html-parser/parse-digit (assoc pc/initial-state :code "1a"))
    (assoc pc/initial-state :value "1" :code "a")))))

(deftest
 parse-text-test
 (testing
  "test parse text"
  (is
   (=state
    (html-parser/parse-text (assoc pc/initial-state :code "ab<"))
    (assoc pc/initial-state :value "ab" :code "<")))))

(deftest
 parse-seperation-test
 (testing
  "test parse seperation"
  (is
   (=state
    (html-parser/parse-seperation (assoc pc/initial-state :code "  a"))
    (assoc pc/initial-state :code "a")))))

(deftest
 parse-tag-name-test
 (testing
  "test parse tag-name"
  (is
   (=state
    (html-parser/parse-tag-name (assoc pc/initial-state :code "a-b "))
    (assoc pc/initial-state :value "a-b" :code " ")))))

(deftest
 parse-attr-test
 (testing
  "test parse attr"
  (is
   (=state
    (html-parser/parse-attr (assoc pc/initial-state :code "a-b "))
    (assoc pc/initial-state :value "a-b" :code " ")))))

(deftest
 parse-entry-test
 (testing
  "test parse entry"
  (is
   (=state
    (html-parser/parse-entry (assoc pc/initial-state :code "a-b=\"\" "))
    (assoc pc/initial-state :code " " :value ["a-b" "=" ""])))))

(deftest
 parse-attributes-test
 (testing
  "test parse attributes"
  (is
   (=state
    (html-parser/parse-attributes (assoc pc/initial-state :code "a=\"true\" b=\"false\""))
    (assoc pc/initial-state :code "" :value [["a" "=" "true"] ["b" "=" "false"]])))))

(deftest
 parse-open-tag-test
 (testing
  "test parse open-tag"
  (is
   (=state
    (html-parser/parse-open-tag (assoc pc/initial-state :code "<a>"))
    (assoc pc/initial-state :code "" :value ["a" []])))))

(deftest
 parse-open-tag-with-attributes-test
 (testing
  "test parse open-tag-with-attributes"
  (is
   (=state
    (html-parser/parse-open-tag (assoc pc/initial-state :code "<a b=\"c\">"))
    (assoc pc/initial-state :code "" :value ["a" [["b" "=" "c"]]])))))

(deftest
 parse-close-tag-test
 (testing
  "test parse close-tag"
  (is
   (=state
    (html-parser/parse-close-tag (assoc pc/initial-state :code "</a> "))
    (assoc pc/initial-state :code " " :value "a")))))

(deftest
 parse-self-close-tag-test
 (testing
  "test parse self-close-tag"
  (is
   (=state
    (html-parser/parse-self-close-tag (assoc pc/initial-state :code "<a /> "))
    (assoc pc/initial-state :code " " :value ["a" []])))))

(deftest
 parse-self-close-tag-2-test
 (testing
  "test parse self-close-tag-2"
  (is
   (=state
    (html-parser/parse-self-close-tag (assoc pc/initial-state :code "<a b=\"c\"/> "))
    (assoc pc/initial-state :code " " :value ["a" [["b" "=" "c"]]])))))

(deftest
 parse-html-test
 (testing
  "test parse html"
  (is
   (=state
    (html-parser/parse-html (assoc pc/initial-state :code "a <a b=\"c\"/> b"))
    (assoc pc/initial-state :value ["a " ["a" [["b" "=" "c"]]] " b"])))))
