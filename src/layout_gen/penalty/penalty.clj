(ns layout-gen.penalty.penalty
  (:require [clojure.core.matrix :as matrix]
            [layout-gen.penalty.flow :as flow]
            [layout-gen.penalty.strength :as strength]
            [layout-gen.penalty.speed :as speed]
            [layout-gen.penalty.modifier :as modifier]
            [layout-gen.layout :as layout]
            [layout-gen.corpus :as corpus]))

(def penalty-matrix
  (matrix/mul flow/matrix
              speed/normalized-matrix
              strength/normalized-matrix))

(defn normalize [val]
  (let [raw-min (matrix/emin penalty-matrix)
        raw-max (matrix/emax penalty-matrix)
        new-max 1.0
        new-min (/ raw-min raw-max)]
    (+ new-min
       (/ (* (- val raw-min) (- new-max new-min))
          (- raw-max raw-min)))))

(def normalized-matrix
  (mapv (fn [row]
          (mapv normalize row))
        penalty-matrix))

(defn penalty-for-bigram [keya keyb]
  (let [positional-penalty (matrix/mget normalized-matrix (rem (:key/position keya) 20) (rem (:key/position keyb) 20))
        modifier-penalty-a (modifier/penalty (:key/val keya))
        modifier-penalty-b (modifier/penalty (:key/val keyb))]
    (* positional-penalty
       modifier-penalty-a
       modifier-penalty-b)))

(defn penalty-for-layout [corpus keyvec]
  (let [bigram-map (corpus/build-bigrams-map corpus)
        value->key (layout/derive-value->key keyvec)]
    (reduce (fn [acc [[valuea valueb] occurrences]]
              (let [keya (value->key valuea)
                    keyb (value->key valueb)]
                (+ acc (* occurrences (penalty-for-bigram keya keyb)))))
            0 bigram-map)))


(comment
  (def corp (corpus/gen-very-large-corpus))
  (def basic (layout/build-keyvec {:basekeys layout/lower-keys}))
  corp
  basic
  (penalty-for-layout corp basic)

  (/ (reduce + (repeatedly 50 #(penalty-for-layout corp basic))) 50)
  (+ 1 2)

  (pmap (fn [_] (penalty-for-layout corp basic)) (range 15000))

  (count (for [a (range 60)
               b (range 60)
               c (range 60)
               d (range 60)]
           (penalty-for-layout corp basic)))

  (/ (reduce + [25343.15602573232
                25275.607426242936
                25252.681787392903
                25316.33360332202
                25344.604131370255
                25324.45732898452
                25208.917455466006
                25255.249824482165
                25250.684259449423
                25154.915389348906]) 10))

(comment

  (matrix/mul [[1 2] [3 4]] [[2 2] [2 2]])
  (def combo (matrix/mul flow/matrix
                         speed/normalized-matrix
                         strength/normalized-matrix))
  (matrix/emax combo)
  normalized-matrix

  (matrix/mget normalized-matrix 0 4))
