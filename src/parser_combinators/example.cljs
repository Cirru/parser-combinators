
(ns parser-combinators.example
  (:require [parser-combinators.examples.json :as json-parser]
            [parser-combinators.core :as pc]))

(defn main! []
  (println (json-parser/parse-array (assoc pc/initial-state :code "[1,2,{\"a\": \"\"}]"))))
