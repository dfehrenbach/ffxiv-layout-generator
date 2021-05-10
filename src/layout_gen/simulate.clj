(ns layout-gen.simulate
  (:require [layout-gen.penalty.penalty :as penalty]
            [layout-gen.layout :as layout]
            [layout-gen.corpus :as corpus]
            #_[layout-gen.util :as util]
            #_[layout-gen.annealing :as annealing]))

(def base-layout (layout/build-keyvec {:basekeys layout/lower-keys}))

(def available-swap-range
  (remove (fn [i] (:key/masked (get base-layout i)))
          (range (count base-layout))))

(def corpus (corpus/gen-very-large-corpus))
(def corpus-size (dec (count corpus)))
(def applied-penalty (partial penalty/penalty-for-layout (corpus/build-bigrams-map corpus)))
(def base-penalty (/ (applied-penalty base-layout) corpus-size))
(defn penalty-fn [layout]
  (/ (/ (applied-penalty layout) corpus-size)
     base-penalty))
#_(defn non-scaled-penalty-fn [layout]
    (/ (applied-penalty layout) corpus-size))


(defn collect-swaps [nswaps]
  (partition 2 (repeatedly (* 2 nswaps) #(rand-nth available-swap-range))))

(defn calculate-swap [layout nswaps]
  (fn [_i]
    (let [next (reduce layout/swap-keys layout (collect-swaps nswaps))]
      [(penalty-fn next) next])))

(defn no-annealing-sim [{:keys [layout rounds round-i nswaps]
                         :or {layout base-layout
                              rounds 50
                              round-i 3000
                              nswaps 8}}]
  (loop [[penalty layout] [(penalty-fn layout) layout]
         i 0]
    (println i penalty)
    (if (= i rounds) [penalty layout]
        (recur (apply max-key first
                      (conj (pmap (calculate-swap layout nswaps) (range round-i))
                            [penalty layout]))
               (inc i)))))

(comment
  (def penguin (time (no-annealing-sim {:rounds 250 :round-i 1200 :nswaps 8})))
  (first penguin)
  ;; => 1.071019943059618

  #_(def otter (time (sim)))
  #_(/ (first (last otter)) base-penalty)
  ;; => 1.0691013710472124
  #_(first (last otter))
  ;; => 0.7488699966709774
  #_(map first otter)
  ;; => (0.3779222770315158 0.37798385439717896 0.37798945216164564 0.3779976383028081 0.37802327823709053)

  #_(defn try-layouts [ntries nswaps starting-layout]
      (second (apply max-key first
                     (pmap (fn [_]
                             (let [next (reduce layout/swap-keys starting-layout (collect-swaps nswaps))]
                               [(penalty-fn next) next]))
                           (range ntries)))))
  #_(defn add-to-top [top top-num layout layout-penalty]
      (if (not= top-num (count top))
        (assoc top layout-penalty layout)
        (let [lowest (ffirst top)]
          (if (<= layout-penalty lowest) top
              (-> top (dissoc lowest) (assoc layout-penalty layout))))))
  #_(defn sim
      ([]
       (sim {:layout base-layout :i 1 :end 15000 :top (sorted-map) :top-num 5}))
      ([{:keys [layout i end top top-num] :as data}]
       (if (= i (inc end)) top
           (do (when (zero? (rem i 100)) (println i (non-scaled-penalty-fn layout)))
               (let [scaled-base-penalty (non-scaled-penalty-fn layout)
                     next-layout (try-layouts 10 8 layout)
                     scaled-next-penalty (non-scaled-penalty-fn next-layout)]
                 (if (annealing/accept_transition (- scaled-next-penalty scaled-base-penalty) i)
                   (recur (-> data
                              (assoc :layout next-layout)
                              (update :i inc)
                              (assoc :top (add-to-top top top-num
                                                      next-layout scaled-next-penalty))))
                   (recur (-> data
                              (update :i inc)))))))))

  0)
