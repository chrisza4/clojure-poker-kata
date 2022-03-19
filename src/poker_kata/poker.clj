(ns poker-kata.poker
  (:require [clojure.string :as string]
            [poker-kata.utils :as utils]
            [poker-kata.card :as card]
            [clojure.spec.alpha :as s]))

;------------ Valuators

(defn- valuate-flush [hand]
  (if (= 1 (-> (map :suit hand)
               (distinct)
               (count)))
    {:power :flush
     :highs (->> (map :value hand)
                 (sort >))}))

(defn- vals-straight? [hand-vals-sorted]
  (loop [current-hand-vals (rest hand-vals-sorted)
         current-val       (first hand-vals-sorted)
         is-straight true]
    (cond
      (not is-straight)   false
      (zero?              (count current-hand-vals)) is-straight
      :else               (and is-straight
                               (= current-val (+ (first current-hand-vals) 1))
                               (recur (rest current-hand-vals) (first current-hand-vals) is-straight)))))

(defn- valuate-straight [hand]
  (let [card-vals-list           (->> (map :value hand) (sort >))
        card-vals-list-ace-first (->> (map :value hand)
                                      (replace {14 1})
                                      (sort >))]
    (cond
      (vals-straight? card-vals-list)            {:power :straight, :highs card-vals-list}
      (vals-straight? card-vals-list-ace-first)  {:power :straight, :highs card-vals-list-ace-first}
      :else                                      nil)))

(defn- valuate-straight-flush [hand]
  (let [straight-res (valuate-straight hand)
        flush-res   (valuate-flush hand)]
    (if (and (not-empty straight-res) (not-empty flush-res))
      {:power :straight-flush
       :highs (:highs straight-res)}
      nil)))

(defn- is-card-set [card-set card-freq-map]
  (let [card-freq-sorted (sort (map val card-freq-map))]
    (= card-set card-freq-sorted)))

(defn- valuate-four-card [hand]
  (let [card-freq-map (->> (map :value hand)
                           (frequencies))]
    (if (is-card-set [1 4] card-freq-map)
      {:power :fourcard
       :highs (card/highs card-freq-map)}
      nil)))

(defn- valuate-fullhouse [hand]
  (let [card-freq-map (->> (map :value hand)
                           (frequencies))]
    (if (is-card-set [2 3] card-freq-map)
      {:power :fullhouse
       :highs (card/highs card-freq-map)}
      nil)))

(defn- valuate-three [hand]
  (let [card-freq-map (->> (map :value hand)
                           (frequencies))]
    (if (is-card-set [1 1 3] card-freq-map)
      {:power :three
       :highs (card/highs card-freq-map)}
      nil)))

(defn- valuate-two-pairs [hand]
  (let [card-freq-map (->> (map :value hand)
                           (frequencies))]
    (if (is-card-set [1 2 2] card-freq-map)
      {:power :two-pairs
       :highs (card/highs card-freq-map)}
      nil)))

(defn- valuate-a-pair [hand]
  (let [card-freq-map (->> (map :value hand)
                           (frequencies))]
    (if (is-card-set [1 1 1 2] card-freq-map)
      {:power :pair
       :highs (card/highs card-freq-map)}
      nil)))

(defn- valuate-highcard [hand]
  (let [card-freq-map (->> (map :value hand)
                           (frequencies))]
    {:power :high, :highs (card/highs card-freq-map)}))

;-------------- Valuate whole hand

(defn- apply-if-nil [val f & args]
  (if (nil? val)
    (apply f args)
    val))

(defn hand-power [hand]
  (-> nil
      (apply-if-nil valuate-straight-flush hand)
      (apply-if-nil valuate-four-card hand)
      (apply-if-nil valuate-fullhouse hand)
      (apply-if-nil valuate-flush hand)
      (apply-if-nil valuate-straight hand)
      (apply-if-nil valuate-three hand)
      (apply-if-nil valuate-two-pairs hand)
      (apply-if-nil valuate-a-pair hand)
      (apply-if-nil valuate-highcard hand)))

; ----------------- Valuator result comparison

(def power-ranking-map
  (->> [:straight-flush
        :fourcard
        :fullhouse
        :flush
        :straight
        :three
        :two-pairs
        :pair
        :high]
       (map-indexed #(-> [%2 %1]))
       (into (hash-map))))

; high, high -> :draw/:win/:lose
(defn- compare-high [high1 high2]
  (let [cmp-result (->> (map - high1 high2)
                        (utils/find-first #(not (zero? %))))]
    ; This is super funny way to use condp. I obviously misunderstand something
    (cond
      (nil? cmp-result) :draw
      (pos? cmp-result) :win
      (neg? cmp-result) :lose)))

; power, power -> :draw/:win/:lose
(defn- compare-hand-power [power1 power2]
  (let [[rank1 rank2] [((:power power1) power-ranking-map) ((:power power2) power-ranking-map)]]
    (cond
      (< rank1 rank2) :win
      (> rank1 rank2) :lose
      :else (compare-high (:highs power1) (:highs power2)))))

; ----------------------------- Main function

(defn compare-poker-hand [first-hand second-hand]
  (let [first-hand-power  (hand-power (map card/card-str-to-map first-hand))
        second-hand-power (hand-power (map card/card-str-to-map second-hand))]
    (compare-hand-power first-hand-power second-hand-power)))