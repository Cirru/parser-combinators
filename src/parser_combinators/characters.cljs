
(ns parser-combinators.characters (:require [clojure.string :as string]))

(def backslash "\\")

(def close-paren ")")

(def double-quote "\"")

(def equal "=")

(def gt ">")

(def hyphen "-")

(def line-break "\n")

(def lt "<")

(def open-paren "(")

(def slash "/")

(def specials-in-string "\"\\\n")

(def specials-in-token "\"() \n\t")

(def whitespace " ")
