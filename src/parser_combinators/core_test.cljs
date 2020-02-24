
(ns parser-combinators.core-test
  (:require [clojure.test :refer [deftest is testing]] [parser-combinators.core :as pc]))

(deftest
 parse-align-test
 (testing
  "parse align"
  (is
   (=
    (pc/parse-align (assoc pc/initial-state :code "\n  a" :indentation 1))
    (assoc pc/initial-state :indentation 1 :code "a")))))

(deftest
 parse-backslash-test
 (testing
  "test backslash"
  (is
   (=
    (pc/parse-backslash (assoc pc/initial-state :code "\\a"))
    (assoc pc/initial-state :code "a" :value "\\")))))

(deftest
 parse-blanks-test
 (testing
  "test blanks"
  (is
   (=
    (pc/parse-blanks (assoc pc/initial-state :code "  a"))
    (assoc pc/initial-state :code "a" :value nil)))))

(deftest
 parse-close-paren-test
 (testing
  "test close paren"
  (is
   (=
    (pc/parse-close-paren (assoc pc/initial-state :code ")a"))
    (assoc pc/initial-state :code "a" :value ")")))))

(deftest
 parse-double-quote-test
 (testing
  "test double quote"
  (is
   (=
    (pc/parse-double-quote (assoc pc/initial-state :code "\"a"))
    (assoc pc/initial-state :code "a" :value "\"")))))

(deftest
 parse-empty-line-test
 (testing
  "parse empty line"
  (is
   (=
    (pc/parse-empty-line (assoc pc/initial-state :code "\n  \n"))
    (assoc pc/initial-state :value nil :code "\n")))))

(deftest
 parse-eof-test
 (testing
  "test parse eof"
  (is
   (= (pc/parse-eof (assoc pc/initial-state :code "")) (assoc pc/initial-state :code "")))))

(deftest
 parse-escaped-char-test
 (testing
  "test escaped char"
  (is
   (=
    (pc/parse-escaped-char (assoc pc/initial-state :code "\\\"a"))
    (assoc pc/initial-state :code "a" :value "\"")))))

(deftest
 parse-in-string-char-test
 (testing
  "test in string char"
  (is
   (=
    (pc/parse-in-string-char (assoc pc/initial-state :code "ab"))
    (assoc pc/initial-state :code "b" :value "a" :msg "recorvered in not")))))

(deftest
 parse-in-token-char-reverse-test
 (testing
  "test in token char reverse"
  (is
   (=
    (pc/parse-in-token-char (assoc pc/initial-state :code "\nb"))
    (assoc pc/initial-state :code "b" :value "\n" :failed true :msg "should not be this")))))

(deftest
 parse-in-token-char-test
 (testing
  "test in token char"
  (is
   (=
    (pc/parse-in-token-char (assoc pc/initial-state :code "ab"))
    (assoc pc/initial-state :code "b" :value "a" :msg "recorvered in not")))))

(deftest
 parse-indent-test
 (testing
  "parse indent"
  (is
   (=
    (pc/parse-indent (assoc pc/initial-state :code "\n  a"))
    (assoc pc/initial-state :indentation 1 :code "a")))))

(deftest
 parse-indentation-test
 (testing
  "parse indentation"
  (is
   (=
    (pc/parse-indentation (assoc pc/initial-state :code "\n  a"))
    (assoc pc/initial-state :code "a" :value 1)))))

(deftest
 parse-line-break-test
 (testing
  "test line break"
  (is
   (=
    (pc/parse-line-break (assoc pc/initial-state :code "\na"))
    (assoc pc/initial-state :code "a" :value "\n")))))

(deftest
 parse-line-breaks-test
 (testing
  "parse line breaks"
  (is
   (=
    (pc/parse-line-breaks (assoc pc/initial-state :code "\n\na"))
    (assoc pc/initial-state :code "a" :value nil)))))

(deftest
 parse-newlines-test
 (testing
  "test newlines"
  (is
   (=
    (pc/parse-newlines (assoc pc/initial-state :code "\n\n\na"))
    (assoc pc/initial-state :code "a" :value nil)))))

(deftest
 parse-open-paren-test
 (testing
  "test open paren"
  (is
   (=
    (pc/parse-open-paren (assoc pc/initial-state :code "(a"))
    (assoc pc/initial-state :code "a" :value "(")))))

(deftest
 parse-string-special-test
 (testing
  "test string special"
  (is
   (=
    (pc/parse-string-special (assoc pc/initial-state :code "\na"))
    (assoc pc/initial-state :code "a" :value "\n")))))

(deftest
 parse-string-test
 (testing
  "test string"
  (is
   (=
    (pc/parse-string (assoc pc/initial-state :code "\"a\""))
    (assoc pc/initial-state :code "" :value "a" :msg "recorvered in not")))))

(deftest
 parse-token-end-test
 (testing
  "test token end"
  (is
   (=
    (pc/parse-token-end (assoc pc/initial-state :code ""))
    (assoc pc/initial-state :code "" :value nil)))))

(deftest
 parse-token-special-negative-test
 (testing
  "test token special nagtive"
  (is
   (=
    (pc/parse-token-special (assoc pc/initial-state :code "ba"))
    (assoc pc/initial-state :code "a" :value "b" :failed true :msg "not in char list")))))

(deftest
 parse-token-special-test
 (testing
  "test token special"
  (is
   (=
    (pc/parse-token-special (assoc pc/initial-state :code "\na"))
    (assoc pc/initial-state :code "a" :value "\n")))))

(deftest
 parse-token-test
 (testing
  "test token"
  (is
   (=
    (pc/parse-token (assoc pc/initial-state :code "ab"))
    (assoc pc/initial-state :code "" :value "ab" :msg "recorvered in not")))))

(deftest
 parse-two-blanks-test
 (testing
  "parse two blanks"
  (is
   (=
    (pc/parse-two-blanks (assoc pc/initial-state :code "  "))
    (assoc pc/initial-state :code "" :value 1)))))

(deftest
 parse-unindent-test
 (testing
  "parse unindent"
  (is
   (=
    (pc/parse-unindent (assoc pc/initial-state :code "\n  a" :indentation 2))
    (assoc pc/initial-state :indentation 1 :code "a")))))

(deftest
 parse-whitespace-test
 (testing
  "test whitespace"
  (is
   (=
    (pc/parse-whitespace (assoc pc/initial-state :code " a"))
    (assoc pc/initial-state :code "a" :value " ")))))
