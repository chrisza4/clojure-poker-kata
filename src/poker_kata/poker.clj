(ns poker-kata.poker
  (:require [clojure.string :as string]))

(defn parse-int [s]
   (let [int-str (re-find  #"\d+" s)]
     (case int-str
       nil nil
       (Integer. int-str))))

(defn card-value-number [val]
  (case (string/lower-case val)
    "a" 14
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

(defn- sort-by-desc [func val]
  (sort-by func #(compare %2 %1) val))

(defn- highs [card-freq-map]
  (->> (sort-by-desc val card-freq-map)
       (map first)))

(defn valuate-four-card [hand]
  (let [card-freq-map (->> (map :value hand)
                           (frequencies))
        card-freq     (map val card-freq-map)]
    (if (= 4 (apply max card-freq))
      {:power :fourcard
       :highs (highs card-freq-map)}
      nil)))

(defn valuate-flush [hand]
  (if (= 1 (-> (map :suit hand)
               (distinct)
               (count)))
    {:power :flush
     :highs (->> (map :value hand)
                 (sort >))}))

(defn is-vals-straight [hand]
  (loop [current-hand (rest hand)
         current-val (first hand)
         is-straight true]
    (cond
      (not is-straight) false
      (zero?            (count current-hand)) is-straight
      :else             (and is-straight
                          (= current-val (+ (first current-hand) 1))
                          (recur (rest current-hand) (first current-hand) is-straight)))))



(defn valuate-straight [hand]
  (let [card-value-list           (->> (map :value hand) (sort >))
        card-value-list-ace-first (->> (map :value hand)
                                       (replace {14 1})
                                       (sort >))]
    (cond
      (is-vals-straight card-value-list)            {:power :straight, :highs card-value-list}
      (is-vals-straight card-value-list-ace-first)  {:power :straight, :highs card-value-list-ace-first}
      :else                                         nil)))


(defn valuate-fullhouse [hand]
  (let [card-freq-map (->> (map :value hand)
                           (frequencies))
        card-freq     (map val card-freq-map)]
    (if (and (some #{3} card-freq) (some #{2} card-freq))
      {:power :fullhouse
       :highs (highs card-freq-map)}
      nil)))

(defn valuate-highcard [hand]
  :high)

(defn- apply-if-not-nil [val f & args]
  (if (nil? val)
    (apply f args)
    val))

(defn hand-power [hand]
  (-> (apply-if-not-nil nil valuate-four-card hand)
      (apply-if-not-nil valuate-fullhouse hand)
      (apply-if-not-nil valuate-flush hand)
      (apply-if-not-nil valuate-straight hand)
      (apply-if-not-nil valuate-highcard hand)))


(defn compare-poker-hand [first-hand second-hand]
  (let [first-hand  (map card-str-to-map first-hand)
        second-hand (map card-str-to-map second-hand)]
    true))


(defmacro def- [item value]
  `(def ^{:private true} ~item ~value))


(comment
  (parse-int "xx")
  (macroexpand `(def- x 1))
  (ext "10S" 0 (- 3 1))
  (hand-str-to-map "AS")
  (sort > [1 3 4 8 3 4])
  (let [hand [{:value 3, :suit :h} {:value 3, :suit :s} {:value 3, :suit :d} {:value 3, :suit :c} {:value 7, :suit :c}]]
    (valuate-four-card hand)))
