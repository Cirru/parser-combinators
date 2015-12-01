
ns parser-combinators.examples.html
  :require
    [] clojure.string :as string
    [] parser-combinators.core :refer :all
    [] parser-combinators.characters :as characters

-- "|helpers"

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

declare parse-chunk

def parse-lt $ generate-char characters/lt
def parse-gt $ generate-char characters/gt
def parse-slash $ generate-char characters/slash
def parse-equal $ generate-char characters/equal
def parse-hyphen $ generate-char characters/hyphen

defn parse-not-tag (state)
  if (= (:code state) |)
    assoc state :failed true :msg "|error eof"
    call-value-with state $ combine-opposite parse-lt

def parse-letter $ generate-char-match (re-pattern "|[a-z]")
def parse-digit $ generate-char-match (re-pattern "|[0-9]")

def parse-text $ transform-value
  combine-some parse-not-tag
  fn (value) $ string/join | value

def parse-seperation $ transform-value
  combine-some parse-whitespace
  fn (value) nil

def parse-tag-name $ transform-value
  combine-chain parse-letter
    transform-value
      combine-asterisk $ combine-or parse-letter parse-digit parse-hyphen
      fn (value) $ string/join | value
  fn (value) $ string/join | value

def parse-attr $ transform-value
  combine-chain parse-letter
    transform-value
      combine-asterisk $ combine-or parse-letter parse-digit parse-hyphen
      fn (value) $ string/join | value
  fn (value) $ string/join | value

def parse-entry $ combine-chain parse-attr parse-equal parse-string

def parse-attributes $ transform-value
  combine-interleave parse-entry parse-seperation
  fn (value) $ helper-value-interleave ([]) value 0

def parse-open-tag $ transform-value
  combine-chain parse-lt parse-tag-name
    combine-optional parse-seperation
    combine-optional parse-attributes
    , parse-gt
  fn (value) $ [] (get value 1)
    or (get value 3) ([])

def parse-close-tag $ transform-value
  combine-chain parse-lt
    , parse-slash (combine-optional parse-seperation) parse-tag-name parse-gt
  fn (value) (get value 3)

def parse-self-close-tag $ transform-value
  combine-chain parse-lt parse-tag-name
    combine-optional parse-seperation
    combine-optional parse-attributes
    , parse-slash parse-gt
  fn (value) $ [] (get value 1) (or (get value 3) ([]))

def parse-normal-tag $ combine-chain parse-open-tag
  , parse-chunk parse-close-tag

def parse-tag $ combine-or parse-self-close-tag parse-normal-tag

def parse-html $ combine-some
  combine-or parse-text parse-tag
