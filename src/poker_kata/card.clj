(ns poker-kata.card
  (:require [clojure.string :as string]
            [poker-kata.utils :as utils]))

(defn- card-value-number [val]
  (let [card-value-map {"a" 14
                        "j" 11
                        "q" 12
                        "k" 13}]
    (card-value-map (string/lower-case val) (utils/parse-int val))))

(defn- butlast-str [str]
  (clojure.string/join (butlast str)))

(defn- extract-card [card]
  [(butlast-str card)
   (last card)])

(defn- cmp-freq [v1 v2]
  (let [[key1 val1] [(key v1) (val v1)]
        [key2 val2] [(key v2) (val v2)]]
    (cond
      (> val1 val2) true
      (< val1 val2) false
      (> key1 key2) true
      :else false)))

(defn card-str-to-map [card]
  (let [[card-value card-suit] (extract-card card)]
    {:value (card-value-number card-value)
     :suit  (-> (string/lower-case card-suit)
                (keyword))}))

(defn highs [card-freq-map]
  (->> (sort-by identity cmp-freq card-freq-map)
       (map first)))