
ns parser-combinators.examples.json
  :require
    [] parser-combinators.core :refer :all
    [] clojure.string :as string

declare parse-item

def char-open-square "|["
def char-close-square "|]"
def char-open-curly "|{"
def char-close-curly "|}"
def char-comma "|,"
def char-colon "|:"
def char-dot "|."

def parse-open-square $ generate-char char-open-square
def parse-close-square $ generate-char char-close-square
def parse-open-curly $ generate-char char-open-curly
def parse-close-curly $ generate-char char-close-curly
def parse-comma $ generate-char char-comma
def parse-colon $ generate-char char-colon
def parse-dot $ generate-char char-dot

-- "|helper"

defn join-list (value)
  string/join | value

defn transform-by-join (parser)
  transform-value parser join-list

-- "|parsers"

def parse-null $ generate-chars |null

def parse-digit $ generate-char-in "|0123456789"

def parse-number $ transform-by-join $ combine-chain
  transform-by-join
    combine-some parse-digit
  combine-optional $ transform-by-join
    combine-chain parse-dot
      transform-by-join
        combine-some parse-digit

def parse-seperation $ combine-optional
  combine-or parse-whitespace parse-empty-line

def parse-entry $ combine-chain
  , parse-string parse-colon parse-seperation parse-item

def parse-object $ combine-chain
  , parse-open-curly
  combine-optional $ combine-interleave parse-entry
    combine-chain parse-comma parse-seperation
  , parse-close-curly

def parse-array $ combine-chain
  , parse-open-square
  combine-optional $ combine-interleave parse-item
    combine-chain parse-comma parse-seperation
  , parse-close-square

def parse-item $ combine-or
  , parse-null parse-number parse-string parse-array parse-object

def parse-json $ combine-or parse-array parse-object
