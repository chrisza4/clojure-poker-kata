(ns poker-kata.card-test
  (:require [clojure.test :refer :all]
            [poker-kata.card :refer :all]))

(deftest highs-test
  (testing "highs for two pairs"
    (let [map1 {14 2
                13 1
                11 2}]
      (is (= [14 11 13] (highs map1)))))

  (testing "highs for three of a kind"
    (let [map1 {2  3
                13 1
                11 1}]
      (is (= [2 13 11] (highs map1))))))

