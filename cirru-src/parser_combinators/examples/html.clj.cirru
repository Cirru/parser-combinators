
ns parser-combinators.examples.html
  :require
    [] clojure.string :as string
    [] parser-combinators.core :refer :all
    [] parser-combinators.characters :as characters

-- "|helpers"

defn- call-value-with (value handler)
  handler value

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

def parse-text $ combine-asterisk parse-not-tag

def parse-seperation $ combine-some parse-whitespace

def parse-tag-name $ combine-chain parse-letter
  combine-asterisk $ combine-or parse-letter parse-digit parse-hyphen

def parse-attr $ combine-chain parse-letter
  combine-asterisk $ combine-or parse-letter parse-digit parse-hyphen

def parse-entry $ combine-chain parse-attr parse-equal parse-string

def parse-attributes $ combine-asterisk
  combine-some parse-entry

def parse-open-tag $ combine-chain parse-lt parse-tag-name
  , parse-seperation parse-attributes parse-seperation parse-gt

def parse-close-tag $ combine-chain parse-lt
  , parse-slash parse-seperation parse-tag-name parse-gt

def parse-self-close-tag $ combine-chain parse-lt parse-tag-name
  , parse-seperation parse-attributes parse-seperation
  , parse-slash parse-gt

def parse-normal-tag $ combine-chain parse-open-tag
  , parse-chunk parse-close-tag

def parse-tag $ combine-or parse-self-close-tag parse-normal-tag

def parse-chunk $ combine-some
  combine-or parse-text parse-tag

def parse-html $ combine-or parse-chunk parse-tag
