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

(deftest hand-power-test
  (let [four-card [{:value 3 :suit :h}
                   {:value 3 :suit :s}
                   {:value 3 :suit :d}
                   {:value 3 :suit :c}
                   {:value 7 :suit :c}]
        full-house [{:value 3 :suit :h}
                    {:value 3 :suit :s}
                    {:value 3 :suit :d}
                    {:value 7 :suit :c}
                    {:value 7 :suit :c}]]
    (is (= (hand-power four-card) {:power :fourcard, :highs [3 7]}))))
    ; (is (= (hand-power full-house) {:power :fullhouse, :highs [3 7]}))))
