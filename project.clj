(defproject parser-combinators "0.0.1"
  :description "Simple Parser Combinators solution in Clojure"
  :url "http://github.com/mvc-works/parser-combinators"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[cirru/lein-sepal "0.0.15"]
            [cirru/lein-sepal-repl "0.0.7"]]
  :cirru-sepal {:paths ["cirru-src", "cirru-test"]}
  :dependencies [[org.clojure/clojure "1.7.0"]])
