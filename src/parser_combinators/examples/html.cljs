
(ns parser-combinators.examples.html
  (:require [clojure.string :as string]
            [parser-combinators.core :as pc]
            [parser-combinators.characters :as characters]))

(defn- call-value-with [value handler] (handler value))

(defn- helper-value-interleave [result value index]
  (if (> (count value) 0)
    (recur (if (even? index) (conj result (first value)) result) (rest value) (inc index))
    result))

(def parse-digit (pc/generate-char-match (re-pattern "[0-9]")))

(def parse-hyphen (pc/generate-char characters/hyphen))

(def parse-letter (pc/generate-char-match (re-pattern "[a-z]")))

(def parse-attr
  (pc/transform-value
   (pc/combine-chain
    parse-letter
    (pc/transform-value
     (pc/combine-asterisk (pc/combine-or parse-letter parse-digit parse-hyphen))
     (fn [value] (string/join "" value))))
   (fn [value] (string/join "" value))))

(def parse-equal (pc/generate-char characters/equal))

(def parse-entry (pc/combine-chain parse-attr parse-equal pc/parse-string))

(def parse-seperation
  (pc/transform-value (pc/combine-some pc/parse-whitespace) (fn [value] nil)))

(def parse-attributes
  (pc/transform-value
   (pc/combine-interleave parse-entry parse-seperation)
   (fn [value] (helper-value-interleave [] value 0))))

(def parse-gt (pc/generate-char characters/gt))

(def parse-lt (pc/generate-char characters/lt))

(def parse-slash (pc/generate-char characters/slash))

(def parse-tag-name
  (pc/transform-value
   (pc/combine-chain
    parse-letter
    (pc/transform-value
     (pc/combine-asterisk (pc/combine-or parse-letter parse-digit parse-hyphen))
     (fn [value] (string/join "" value))))
   (fn [value] (string/join "" value))))

(def parse-close-tag
  (pc/transform-value
   (pc/combine-chain
    parse-lt
    parse-slash
    (pc/combine-optional parse-seperation)
    parse-tag-name
    parse-gt)
   (fn [value] (get value 3))))

(def parse-open-tag
  (pc/transform-value
   (pc/combine-chain
    parse-lt
    parse-tag-name
    (pc/combine-optional parse-seperation)
    (pc/combine-optional parse-attributes)
    parse-gt)
   (fn [value] [(get value 1) (or (get value 3) [])])))

(def parse-normal-tag (pc/combine-chain parse-open-tag parse-close-tag))

(defn parse-not-tag [state]
  (if (= (:code state) "")
    (assoc state :failed true :msg "error eof")
    (pc/call-value-with state (pc/combine-opposite parse-lt))))

(def parse-self-close-tag
  (pc/transform-value
   (pc/combine-chain
    parse-lt
    parse-tag-name
    (pc/combine-optional parse-seperation)
    (pc/combine-optional parse-attributes)
    parse-slash
    parse-gt)
   (fn [value] [(get value 1) (or (get value 3) [])])))

(def parse-tag (pc/combine-or parse-self-close-tag parse-normal-tag))

(def parse-text
  (pc/transform-value (pc/combine-some parse-not-tag) (fn [value] (string/join "" value))))

(def parse-html (pc/combine-some (pc/combine-or parse-text parse-tag)))
