(ns poker-kata.utils)

(defn inspect [v]
  (println "Inspect:" v)
  v)

(defn parse-int [s]
  (let [int-str (re-find  #"\d+" s)]
    (case int-str
      nil nil
      (Integer. int-str))))

(defn find-first [pred coll]
  (some #(when (pred %) %) coll))

(comment
  (-> (+ 1 1)
      (inspect)
      (+ 2)))