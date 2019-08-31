(ns poker-kata.poker-test
  (:require [clojure.test :refer :all]
            [poker-kata.poker :refer :all]))

; (deftest compare-poker-hand-test
;   (let [flush-hand ["2H" "3H" "AH" "KH" "JH"]
;         fullhouse-hand ["2H" "2D" "2S" "3H" "3D"]]
;     (testing "Fullhouse with Flush"
;       (is (= (compare-poker-hand flush-hand fullhouse-hand) :lose))
;       (is (= (compare-poker-hand fullhouse-hand flush-hand) :win))
;       (is (= (compare-poker-hand fullhouse-hand fullhouse-hand) :draw)))))

(def- four-card-hand
  [{:value 3 :suit :h}
   {:value 3 :suit :s}
   {:value 3 :suit :d}
   {:value 3 :suit :c}
   {:value 7 :suit :c}])

(def- full-house-hand
  [{:value 3 :suit :h}
   {:value 3 :suit :s}
   {:value 3 :suit :d}
   {:value 7 :suit :c}
   {:value 7 :suit :c}])
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
                         {:value 7 :suit :c}]]

    (is (= (hand-power four-card) {:power :fourcard, :highs [3 7]}))
    (is (= (hand-power full-house) {:power :fullhouse, :highs [3 7]}))
    (is (= (hand-power flush) {:power :flush, :highs [14, 13, 11, 10, 3]}))
    (is (= (hand-power straight) {:power :straight, :highs [14 13 12 11 10]}))
    (is (= (hand-power straight-2) {:power :straight, :highs [5 4 3 2 1]}))))
    ; (is (= (hand-power straight-flush) {:power :fullhouse, :highs [14 13 12 11 10]}))))
