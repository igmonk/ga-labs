(ns ga-labs.lab1.ga-test
  (:require [clojure.test :refer :all]
            [ga-labs.lab1.ga :refer :all]))

(deftest random-pop-test
  (testing "random-pop"
    #_(let [population (random-pop (range 0 32768) 10 -9 9)]
      (is (= [] population)))))

(deftest code->gray-code-test
  (testing "code->gray-code"
    (is (= 0 (code->gray-code 0)))
    (is (= 1 (code->gray-code 1)))
    (is (= 3 (code->gray-code 2)))
    (is (= 2 (code->gray-code 3)))
    (is (= 6 (code->gray-code 4)))
    (is (= 7 (code->gray-code 5)))
    (is (= 5 (code->gray-code 6)))
    (is (= 4 (code->gray-code 7)))
    (is (= 12 (code->gray-code 8)))
    (is (= 13 (code->gray-code 9)))
    (is (= 15 (code->gray-code 10)))))

(deftest gray-code->code-test
  (testing "gray-code->code"
    (is (= 0 (gray-code->code 0)))
    (is (= 1 (gray-code->code 1)))
    (is (= 2 (gray-code->code 3)))
    (is (= 3 (gray-code->code 2)))
    (is (= 4 (gray-code->code 6)))
    (is (= 5 (gray-code->code 7)))
    (is (= 6 (gray-code->code 5)))
    (is (= 7 (gray-code->code 4)))
    (is (= 8 (gray-code->code 12)))
    (is (= 9 (gray-code->code 13)))
    (is (= 10 (gray-code->code 15)))))

(deftest inverse-bit-at-test
  (testing "inverse-bit-at"
    (is (= 0 (inverse-bit-at 2 1)))
    (is (= 3 (inverse-bit-at 2 0)))
    (is (= 6 (inverse-bit-at 2 2)))
    (is (= 5 (inverse-bit-at 7 1)))))
