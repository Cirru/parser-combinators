
ns parser-combinators.characters
  :require
    [] clojure.string :as string

def open-paren "|("
def close-paren "|)"
def whitespace "| "
def line-break "|\n"
def double-quote "|\""
def slash "|/"
def gt "|>"
def lt "|<"
def backslash "|\\"
def specials-in-token "|\"() \n\t"
def specials-in-string "|\"\\\n"
def equal "|="
def hyphen "|-"
