(ns ga-labs.common.util-test
  (:require [clojure.test :refer :all]
            [ga-labs.common.util :refer :all]))

(deftest pow-of-2-ge-test
  (testing "pow-of-2-ge"
    (is (= 0 (pow-of-2-ge 1)))
    (is (= 1 (pow-of-2-ge 2)))
    (is (= 2 (pow-of-2-ge 3)))
    (is (= 2 (pow-of-2-ge 4)))
    (is (= 3 (pow-of-2-ge 5)))
    (is (= 4 (pow-of-2-ge 16)))
    (is (= 10 (pow-of-2-ge 1024)))
    (is (= 14 (pow-of-2-ge 10000)))))

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

(deftest first-segment-test
  (testing "first-segment"
    (let [a030 {:a 0.3}
          a025 {:a 0.25}
          a020 {:a 0.2}
          a015 {:a 0.15}
          a010 {:a 0.1}
          items [a030 a025 a020 a015 a010]]
      (is (= a030 (first-segment items :a 0.25)))
      (is (= a025 (first-segment items :a 0.35)))
      (is (= a020 (first-segment items :a 0.57)))
      (is (= a015 (first-segment items :a 0.8)))
      (is (= a010 (first-segment items :a 0.95)))
      (is (= nil (first-segment items :a 5))))))

(deftest min-max-test
  (testing "min-max"

    (testing "collection of maps"
      (let [maps [{:a 1 :b 10}
                  {:a 0 :b 100}
                  {:a 2 :b 20}
                  {:a -5 :b -50}
                  {:a 55 :b 9}]]
        (is (= {:min -5 :max 55} (min-max maps :a)))
        (is (= {:min -50 :max 100} (min-max maps :b)))))

    (testing "collection of vectors"
      (let [vectors [[1 2 3]
                     [10 20 30]
                     [-1 -2 -3]
                     [9 5 3]]]
        (is (= {:min -1 :max 10} (min-max vectors first)))
        (is (= {:min -3 :max 30} (min-max vectors last)))))))
