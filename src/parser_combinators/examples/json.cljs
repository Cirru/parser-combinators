
(ns parser-combinators.examples.json
  (:require [parser-combinators.core :as pc]
            [clojure.string :as string]
            [cljs.reader :refer [read-string]]))

(declare parse-item)

(declare parse-object)

(declare parse-array)

(declare parse-entry)

(defn- call-value-with [value handler] (handler value))

(def char-close-curly "}")

(def char-close-square "]")

(def char-colon ":")

(def char-comma ",")

(def char-dot ".")

(def char-open-curly "{")

(def char-open-square "[")

(defn- helper-value-interleave [result value index]
  (if (> (count value) 0)
    (recur (if (even? index) (conj result (first value)) result) (rest value) (inc index))
    result))

(defn join-list [value] (string/join "" value))

(def parse-close-curly (pc/generate-char char-close-curly))

(def parse-close-square (pc/generate-char char-close-square))

(def parse-colon (pc/generate-char char-colon))

(def parse-comma (pc/generate-char char-comma))

(def parse-digit (pc/generate-char-in "0123456789"))

(def parse-dot (pc/generate-char char-dot))

(def parse-null (pc/generate-chars "null"))

(defn transform-by-join [parser] (pc/transform-value parser join-list))

(def parse-number
  (pc/transform-value
   (pc/combine-chain
    (transform-by-join (pc/combine-some parse-digit))
    (pc/combine-optional
     (transform-by-join
      (pc/combine-chain parse-dot (transform-by-join (pc/combine-some parse-digit))))))
   (fn [value] (read-string (string/join "" value)))))

(def parse-open-curly (pc/generate-char char-open-curly))

(def parse-open-square (pc/generate-char char-open-square))

(def parse-seperation
  (pc/transform-value
   (pc/combine-asterisk (pc/combine-or pc/parse-whitespace pc/parse-empty-line))
   (fn [value] nil)))

(defn parse-entry [state]
  (call-value-with
   state
   (pc/transform-value
    (pc/combine-chain pc/parse-string parse-colon parse-seperation parse-item)
    (fn [value] [(get value 0) (get value 3)]))))

(defn parse-array [state]
  (call-value-with
   state
   (pc/transform-value
    (pc/combine-chain
     parse-open-square
     (pc/combine-optional
      (pc/transform-value
       (pc/combine-interleave
        parse-item
        (pc/transform-value
         (pc/combine-chain parse-comma parse-seperation)
         (fn [value] nil)))
       (fn [value] (helper-value-interleave [] value 0))))
     parse-close-square)
    (fn [value] (get value 1)))))

(def parse-item
  (pc/combine-or
   parse-null
   parse-number
   pc/parse-string
   parse-array
   (fn [x] (parse-object x))))

(def parse-object
  (pc/transform-value
   (pc/combine-chain
    parse-open-curly
    (pc/combine-optional
     (pc/combine-interleave parse-entry (pc/combine-chain parse-comma parse-seperation)))
    parse-close-curly)
   (fn [value] (helper-value-interleave [] (get value 1) 0))))

(def parse-json (pc/combine-or parse-array parse-object))
