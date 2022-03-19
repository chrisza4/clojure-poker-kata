(ns poker-kata.poker-test
  (:require [clojure.test :refer :all]
            [poker-kata.poker :refer :all]))

(deftest hand-power-test
  (let [four-card       [{:value 3 :suit :h}
                         {:value 3 :suit :s}
                         {:value 3 :suit :d}
                         {:value 3 :suit :c}
                         {:value 7 :suit :c}]
        straight-flush  [{:value 10 :suit :s}
                         {:value 11 :suit :s}
                         {:value 12 :suit :s}
                         {:value 13 :suit :s}
                         {:value 14 :suit :s}]
        straight-flush-2  [{:value 2 :suit :s}
                           {:value 3 :suit :s}
                           {:value 4 :suit :s}
                           {:value 5 :suit :s}
                           {:value 14 :suit :s}]
        straight        [{:value 10 :suit :s}
                         {:value 11 :suit :d}
                         {:value 12 :suit :d}
                         {:value 13 :suit :s}
                         {:value 14 :suit :s}]
        straight-2      [{:value 2  :suit :s}
                         {:value 3  :suit :d}
                         {:value 4  :suit :d}
                         {:value 5  :suit :s}
                         {:value 14 :suit :s}]
        flush           [{:value 10 :suit :s}
                         {:value 11 :suit :s}
                         {:value 3  :suit :s}
                         {:value 13 :suit :s}
                         {:value 14 :suit :s}]
        full-house      [{:value 3 :suit :h}
                         {:value 3 :suit :s}
                         {:value 3 :suit :d}
                         {:value 7 :suit :c}
                         {:value 7 :suit :c}]
        three           [{:value 3 :suit :h}
                         {:value 3 :suit :s}
                         {:value 3 :suit :d}
                         {:value 9 :suit :c}
                         {:value 7 :suit :c}]
        two-pair        [{:value 3 :suit :h}
                         {:value 3 :suit :s}
                         {:value 9 :suit :d}
                         {:value 9 :suit :c}
                         {:value 7 :suit :c}]
        pair            [{:value 3 :suit :h}
                         {:value 3 :suit :s}
                         {:value 8 :suit :d}
                         {:value 9 :suit :c}
                         {:value 7 :suit :c}]
        high            [{:value 3 :suit :h}
                         {:value 13 :suit :s}
                         {:value 8 :suit :d}
                         {:value 9 :suit :c}
                         {:value 7 :suit :c}]]

    (is (= (hand-power four-card) {:power :fourcard, :highs [3 7]}))
    (is (= (hand-power full-house) {:power :fullhouse, :highs [3 7]}))
    (is (= (hand-power flush) {:power :flush, :highs [14, 13, 11, 10, 3]}))
    (is (= (hand-power straight) {:power :straight, :highs [14 13 12 11 10]}))
    (is (= (hand-power straight-2) {:power :straight, :highs [5 4 3 2 1]}))
    (is (= (hand-power straight-flush) {:power :straight-flush, :highs [14 13 12 11 10]}))
    (is (= (hand-power straight-flush-2) {:power :straight-flush, :highs [5 4 3 2 1]}))
    (is (= (hand-power three) {:power :three, :highs [3 9 7]}))
    (is (= (hand-power two-pair) {:power :two-pairs, :highs [9 3 7]}))
    (is (= (hand-power pair) {:power :pair, :highs [3 9 8 7]}))
    (is (= (hand-power high) {:power :high, :highs [13 9 8 7 3]}))))

(deftest compare-poker-hand-test
  (testing "Some cases"
    (is (= (compare-poker-hand ["2H" "3D" "5S" "9C" "KD"] ["2C" "3H" "4S" "8C" "AH"]) :lose))
    (is (= (compare-poker-hand ["2H" "4S" "4C" "2D" "4H"] ["2S" "8S" "AS" "QS" "3S"]) :win))
    (is (= (compare-poker-hand ["2H" "3D" "5S" "9C" "KD"] ["2C" "3H" "4S" "8C" "KH"]) :win))
    (is (= (compare-poker-hand ["2H" "3D" "5S" "9C" "KD"] ["2D" "3H" "5C" "9S" "KH"]) :draw))
    (is (= (compare-poker-hand ["2H" "3D" "4S" "5C" "6D"] ["AD" "2H" "3C" "4S" "5H"]) :win))
    (is (= (compare-poker-hand ["2H" "3D" "4S" "5C" "6D"] ["AD" "10H" "JC" "QS" "KH"]) :lose))))
