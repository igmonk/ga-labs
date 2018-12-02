(ns ga-labs.lab1.ga-test
  (:require [clojure.test :refer :all]
            [ga-labs.lab1.ga :refer :all]))

(deftest inverse-bit-at-test
  (testing "inverse-bit-at"
    (is (= 0 (inverse-bit-at 2 1)))
    (is (= 3 (inverse-bit-at 2 0)))
    (is (= 6 (inverse-bit-at 2 2)))
    (is (= 5 (inverse-bit-at 7 1)))))
