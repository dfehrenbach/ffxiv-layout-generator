(ns layout-gen.simulate
  (:require [layout-gen.penalty.penalty :as penalty]
            [layout-gen.layout :as layout]
            [layout-gen.corpus :as corpus]))


(def corpus (corpus/gen-very-large-corpus))

(def base-layout (layout/build-keyvec {:basekeys layout/lower-keys}))

(def available-swap-range
  (remove (fn [i] (:key/masked (get base-layout i)))
          (range (count base-layout))))

(defn collect-swaps [nswaps]
  (partition 2 (repeatedly (* 2 nswaps) #(rand-nth available-swap-range))))

(defn simulate []
  (let [base-penalty (penalty/penalty-for-layout corpus base-layout)
        scaled-base-penalty (/ base-penalty (count corpus))]
    scaled-base-penalty
    #_(/ (penalty/penalty-for-layout
          corpus
          (reduce layout/swap-keys base-layout (collect-swaps 8)))
         (count corpus))))

(comment
  (collect-swaps 8)
  (simulate))
