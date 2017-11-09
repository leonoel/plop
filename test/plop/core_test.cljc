(ns plop.core-test
  (:require [clojure.test #?(:clj :refer :cljs :refer-macros) [deftest is]]
            [plop.core #?(:clj :refer :cljs :refer-macros) [place]])
  #?(:clj (:import (clojure.lang IFn))))

(deftest place-base
  (place [a 0]
    (set! a (inc a))
    (is (pos? a))))

(deftest place-quote
  (place [a 0]
    (is (zero? a))
    (is (= (symbol "a") (quote a)))))

(deftest place-let
  (place [a 0
          b 0]
    (let [a 1]
      (is (pos? a))
      (is (zero? b)))))

(deftest place-loop
  (place [a 0
          b 0]
    (loop [a 1]
      (is (pos? a))
      (is (zero? b)))))

(deftest place-fn-name
  (place [a 0
          b 0]
    ((fn a []
       (is (ifn? a))
       (is (zero? b))))))

(deftest place-fn-args
  (place [a 0
          b 0]
    ((fn [a]
       (is (pos? a))
       (is (zero? b))) 1)))

(deftest place-letfn-name
  (place [a 0
          c 0]
    (letfn [(b [] a)
            (a [] c)]
      (is (zero? (a)))
      (is (ifn? (b))))))

(deftest place-case
  (place [a 0]
    (is (zero? (case 'a a)))
    (is (zero? (case 'a a a :default)))))

(deftest place-catch
  (place [a 0]
    (is (= {} (try (is (zero? a))
                   (throw (ex-info "" {}))
                   (catch #?(:clj Throwable :cljs :default) a
                     (ex-data a)))))))
(deftest place-reify
  (place [a 0]
    (is (zero? ((reify IFn (#?(:clj invoke :cljs -invoke) [_] a)))))
    (is (ifn?  ((reify IFn (#?(:clj invoke :cljs -invoke) [a] a)))))))
