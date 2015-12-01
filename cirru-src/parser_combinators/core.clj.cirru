
ns parser-combinators.core
  :require
    [] clojure.string :as string
    [] parser-combinators.characters :as characters
    [] clojure.pprint :as pp

def initial-state $ {}
  :code |
  :value nil
  :indentation 0
  :tab |
  :failed false
  :msg |initial
  :x 0
  :y 0

-- "|utilities"

defn- subs-first (code)
  subs code 0 1

defn- subs-rest (code)
  subs code 1

defn- match-first (state character)
  = (subs-first (:code state)) character

defn- match-two (state character)
  = (subs (:code state) 0 2) character

defn- fail (state msg)
  assoc state :failed true :msg msg

defn- call-value-with (value handler)
  handler value

-- "helper functions"

defn- helper-many (state parser counter)
  let
      result $ parser state
    if (:failed result)
      if (> counter 0) state
        fail state "|matching 0 times"
      recur
        assoc result :value
          conj (into ([]) (:value state)) (:value result)
        , parser (+ counter 1)

defn- helper-asterisk (state parser)
  let
      result $ parser state
    if (:failed result) state
      recur
        assoc result :value
          conj (into ([]) (:value state)) (:value result)
        , parser

defn- helper-chain (state parsers)
  if (> (count parsers) 0)
    let
        parser $ first parsers
        result $ parser state
      if (:failed result)
        fail state "|failed apply chaining"
        recur
          assoc result :value
            conj (into ([]) (:value state)) (:value result)
          rest parsers
    , state

defn- helper-interleave (state parser-1 parser-2 counter)
  let
      result $ parser-1 state
    if (:failed result)
      if (> counter 0) state
        fail state "|not matching interleave rule"
      recur
        assoc result :value
          conj (into ([]) (:value state)) (:value result)
        , parser-2 parser-1 (+ counter 1)

defn- helper-or (state parsers)
  if (> (count parsers) 0)
    let
        parser $ first parsers
        result $ parser state
      if (:failed result)
        recur state $ rest parsers
        , result
    fail state "|no parser is successful"

-- "|combining functions"

defn combine-some (parser)
  fn (state)
    helper-many (assoc state :value (list)) parser 0

defn combine-asterisk (parser)
  fn (state)
    helper-asterisk (assoc state :value (list)) parser

defn combine-chain (& parsers)
  fn (state)
    helper-chain (assoc state :value ([])) parsers

defn combine-interleave (parser-1 parser-2)
  fn (state)
    helper-interleave
      assoc state :value (list)
      , parser-1 parser-2 0

defn combine-or (& parsers)
  fn (state)
    helper-or state parsers

defn combine-opposite (parser)
  fn (state)
    let
        result (parser state)
      if (:failed result)
        assoc result :failed false :msg "|recorvered in not"
        fail result "|should not be this"

defn combine-peek (parser)
  fn (state)
    let
        result $ parser state
      if (:failed result)
        fail state "|peek fail"
        , state

defn combine-times (parser n)
  fn (state)
    call-value-with state
      apply combine-chain (repeat n parser)

defn combine-optional (parser)
  fn (state)
    let
        inner-state (assoc state :value nil)
        result (parser inner-state)
      if (:failed result) inner-state result

-- "|handlers"

defn handle-value (parser handler)
  fn (state)
    let
        result $ parser state
      assoc result :value
        handler (:value result) (:failed result)

defn transform-value (parser handler)
  fn (state) $ let
      result $ parser state
    assoc result :value
      if (:failed result) nil
        handler (:value result)

-- "|generators"

defn generate-char (x)
  fn (state)
    if
      > (count (:code state)) 0
      if (match-first state x)
        assoc state
          , :code $ subs-rest (:code state)
          , :value x
        fail
          assoc state
            , :code $ subs-rest (:code state)
            , :value x
          , "|failed matching character"
      fail state "|error eof"

defn generate-char-in (xs)
  fn (state)
    if
      > (count (:code state)) 0
      if
        >= (.indexOf xs $ subs-first (:code state)) 0
        assoc state
          , :code $ subs-rest (:code state)
          , :value $ subs-first (:code state)
        fail
          assoc state
            , :code $ subs-rest (:code state)
            , :value $ subs-first (:code state)
          , "|not in char list"
      fail state "|error eof"

defn generate-chars (x)
  fn (state)
    if
      >= (count x) (count (:code state))
      if
        = (.indexOf (:code state) x) 0
        assoc state
          , :code $ subs (:code state) (count x)
          , :value x
        fail (assoc state :value nil) "|not expected pattern"
      fail state "|error eof"

defn generate-char-match (x)
  fn (state)
    if
      > (count (:code state)) 0
      if
        re-find (re-pattern "|\d") (subs (:code state) 0 1)
        assoc state
          , :value $ subs-first (:code state)
          , :code $ subs-rest (:code state)
        assoc state :failed true
          , :msg "|no matching pattern"
          , :value $ subs-first (:code state)
          , :code $ subs-rest (:code state)
      fail state "|error eof"

-- "|parsers"

declare parse-line

defn parse-eof (state)
  if (= (:code state) |)
    assoc state :value nil
    fail state "|expected eof"

def parse-open-paren $ generate-char characters/open-paren
def parse-close-paren $ generate-char characters/close-paren
def parse-double-quote $ generate-char characters/double-quote
def parse-whitespace $ generate-char characters/whitespace
def parse-backslash $ generate-char characters/backslash
def parse-line-break $ generate-char characters/line-break

defn parse-escaped-char (state)
  if (< (count (:code state)) 2)
    fail state "|error eof"
    cond
      (match-two state "|\\n")
        assoc state :value "|\n" :code (subs (:code state) 2)
      (match-two state "|\\t")
        assoc state :value "|\t" :code (subs (:code state) 2)
      (match-two state "|\\\"")
        assoc state :value "|\"" :code (subs (:code state) 2)
      (match-two state "|\\\\")
        assoc state :value "|\\" :code (subs (:code state) 2)
      :else $ assoc state :failed true
        , :value $ subs-first (:code state)
        , :code $ subs-rest (:code state)
        , :msg "|no escaped character"

def parse-blanks
  transform-value
    combine-some parse-whitespace
    fn (value) nil

def parse-newlines
  transform-value
    combine-some parse-line-break
    fn (value) nil

def parse-token-special $ generate-char-in characters/specials-in-token

def parse-string-special $ generate-char-in characters/specials-in-string

def parse-token-end
  combine-peek $ combine-or
    , parse-whitespace parse-close-paren parse-newlines parse-eof

defn parse-in-token-char (state)
  if (= (:code state) |)
    fail state "|error eof"
    (combine-opposite parse-token-special) state

defn parse-in-string-char (state)
  if (= (:code state) |)
    fail state "|error eof"
    let
        parser $ combine-or
          combine-opposite parse-string-special
          , parse-escaped-char
      parser state

defn parse-string (state)
  call-value-with state $ transform-value
    combine-chain parse-double-quote
      transform-value
        combine-asterisk parse-in-string-char
        fn (value) $ string/join | value
      , parse-double-quote
    fn (value) $ nth value 1

defn parse-token (state)
  call-value-with state $ transform-value
    combine-chain
      transform-value
        combine-some parse-in-token-char
        fn (value) $ string/join | value
      transform-value parse-token-end $ fn (value) nil
    fn (value) $ first value

defn parse-empty-line (state)
  call-value-with state $ transform-value
    combine-chain parse-line-break
      combine-asterisk parse-whitespace
      combine-peek (combine-or parse-line-break parse-eof)
    fn (value) nil

defn parse-line-breaks (state)
  call-value-with state $ transform-value
    combine-chain
      combine-asterisk parse-empty-line
      , parse-line-break
    fn (value) nil

defn parse-two-blanks (state)
  call-value-with state $ transform-value
    combine-times parse-whitespace 2
    fn (value) 1

defn parse-indentation (state)
  call-value-with state $ transform-value
    combine-chain
      transform-value parse-line-breaks $ fn (value) nil
      transform-value (combine-asterisk parse-two-blanks)
        fn (value) (count value)
    fn (value) (last value)

defn parse-indent (state)
  let
      result $ parse-indentation state
    if
      > (:value result) (:indentation result)
      assoc result
        , :indentation (+ (:indentation result) 1)
        , :value nil
      fail result "|no indent"

defn parse-unindent (state)
  let
      result $ parse-indentation state
    if
      < (:value result) (:indentation result)
      assoc result
        , :indentation (- (:indentation result) 1)
        , :value nil
      fail result "|no unindent"

defn parse-align (state)
  let
      result (parse-indentation state)
    if
      = (:value result) (:indentation state)
      assoc result :value nil
      fail result "|not aligned"
