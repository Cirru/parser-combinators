
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

defn- call-value-with (value handler)
  handler value

defn- helper-value-interleave (result value index)
  if (> (count value) 0)
    recur
      if (even? index)
        conj result (first value)
        , result
      rest value
      inc index
    , result

-- "|parsers"

def parse-null $ generate-chars |null

def parse-digit $ generate-char-in "|0123456789"

def parse-number $ transform-value
  combine-chain
    transform-by-join
      combine-some parse-digit
    combine-optional $ transform-by-join
      combine-chain parse-dot
        transform-by-join
          combine-some parse-digit
  fn (value) $ read-string $ string/join | value

def parse-seperation $ transform-value
  combine-asterisk
    combine-or parse-whitespace parse-empty-line
  fn (value) nil

defn parse-entry (state)
  call-value-with state $ transform-value
    combine-chain
      , parse-string parse-colon parse-seperation parse-item
    fn (value) $ [] (get value 0) (get value 3)

def parse-object $ transform-value
  combine-chain
    , parse-open-curly
    combine-optional $ combine-interleave parse-entry
      combine-chain parse-comma parse-seperation
    , parse-close-curly
  fn (value) $ helper-value-interleave ([]) (get value 1) 0

defn parse-array (state)
  call-value-with state $ transform-value
    combine-chain
      , parse-open-square
      combine-optional $ transform-value
        combine-interleave parse-item
          transform-value
            combine-chain parse-comma parse-seperation
            fn (value) nil
        fn (value) $ helper-value-interleave ([]) value 0
      , parse-close-square
    fn (value) (get value 1)

def parse-item $ combine-or
  , parse-null parse-number parse-string parse-array parse-object

def parse-json $ combine-or parse-array parse-object
