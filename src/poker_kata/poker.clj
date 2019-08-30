(ns poker-kata.poker
  (:require [clojure.string :as string]))

(defn parse-int [s]
   (let [int-str (re-find  #"\d+" s)]
     (case int-str
       nil nil
       (Integer. int-str))))

(defn card-value-number [val]
  (case (string/lower-case val)
    "a" 1
    "j" 11
    "q" 12
    "k" 13
    (parse-int val)))

(defn extract-card [card]
  [(subs card 0 (- (count card) 1))
   (last card)])

(defn card-str-to-map [card]
  (let [[card-value card-suit] (extract-card card)]
    {:value (card-value-number card-value)
     :suit  (-> (string/lower-case card-suit)
                (keyword))}))

(defn inspect [v]
  (println "Inspect:" v)
  v)

(defn valuate-four-card [hand]
  ; (inspect hand)
  (let [card-freq (->> (map #(:value %) hand)
                       (frequencies)
                       (inspect))]
    (if (= 4 (apply max (map val card-freq)))
      {:power :fourcard
       :highs (->> (sort-by val #(compare %2 %1) card-freq)
                   (map first))}
      nil)))


(defn is-fullhouse [hand]
  (let [card-freq (->> (map #(:value %) hand)
                       (frequencies)
                       (map val))]
    (and (some #{3} card-freq) (some #{2} card-freq))))

(defn hand-power [hand]
  (let [fourcard (valuate-four-card hand)]
    (cond
      (not (nil? fourcard)) fourcard
      :else :high)))
  ; (cond
  ;   (is-four-card hand) :fourcard
  ;   (is-fullhouse hand) :fullhouse
  ;   :else :high))


(defn compare-poker-hand [first-hand second-hand]
  (let [first-hand  (map card-str-to-map first-hand)
        second-hand (map card-str-to-map second-hand)]
    true))


(comment
  (parse-int "xx")
  (ext "10S" 0 (- 3 1))
  (hand-str-to-map "AS")
  (let [hand [{:value 3, :suit :h} {:value 3, :suit :s} {:value 3, :suit :d} {:value 3, :suit :c} {:value 7, :suit :c}]]
    (valuate-four-card hand)))
