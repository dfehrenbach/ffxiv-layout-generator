(ns layout-gen.simulate
  (:require [layout-gen.penalty.penalty :as penalty]
            [layout-gen.layout :as layout]
            [layout-gen.corpus :as corpus]
            [layout-gen.annealing :as annealing]
            [criterium.core :as criterium]
            [clojure.core.reducers :as r]))


(def corpus (corpus/gen-very-large-corpus))
(def corpus-size (count corpus))
(def applied-penalty (partial penalty/penalty-for-layout (corpus/build-bigrams-map corpus)))
(defn penalty-fn [layout]
  (/ (applied-penalty layout) corpus-size))

(def base-layout (layout/build-keyvec {:basekeys layout/lower-keys}))

(def available-swap-range
  (remove (fn [i] (:key/masked (get base-layout i)))
          (range (count base-layout))))

(defn collect-swaps [nswaps]
  (partition 2 (repeatedly (* 2 nswaps) #(rand-nth available-swap-range))))

(defn try-layouts [ntries nswaps starting-layout]
  (second (apply max-key first
                 (pmap (fn [_]
                         (let [next (reduce layout/swap-keys starting-layout (collect-swaps nswaps))]
                           [(penalty-fn next) next]))
                       (range ntries)))))

(defn scaling-swap-num [i i-max swap-max]
  (inc (int (* swap-max (- 1 (/ i i-max))))))

(defn sim
  ([]
   (sim {:layout base-layout :i 0 :end 15000}))
  ([{:keys [layout i end] :as data}]
   (if (= i end) layout
       (let [scaled-base-penalty (penalty-fn layout)
             next-layout (try-layouts 10 8 layout)
             scaled-next-penalty (penalty-fn next-layout)]
         (if (annealing/accept_transition (- scaled-next-penalty scaled-base-penalty) i)
           (recur (-> data
                      (assoc :layout next-layout)
                      (update :i inc)))
           (recur (-> data
                      (update :i inc))))))))

(defn calculate-swap [layout i-max swap-max]
  (fn [_i]
    (let [next (reduce layout/swap-keys layout (collect-swaps 8))]
      [(penalty-fn next) next])))

(defn no-annealing-sim [{:keys [layout rounds round-i max-swaps]
                         :or {layout base-layout
                              rounds 50
                              round-i 3000
                              max-swaps 20}}]
  (loop [[penalty layout] [(penalty-fn layout) layout]
         i 0]
    (println i penalty)
    (if (= i rounds) [penalty layout]
        (recur (apply max-key first
                      (conj (pmap (calculate-swap layout round-i max-swaps) (range round-i))
                            [penalty layout]))
               (inc i)))))

(comment
  (criterium/with-progress-reporting (criterium/quick-bench (collect-swaps 8)))
  (criterium/with-progress-reporting
    (criterium/quick-bench
     (reduce layout/swap-keys base-layout (collect-swaps 8))))
             ;;Execution time mean : 127.714123 µs

  (criterium/with-progress-reporting
    (criterium/quick-bench
     (penalty-fn (reduce layout/swap-keys base-layout (collect-swaps 8)))))

  (conj (pmap identity (range 10)) 20)

  (time (loop [[penalty layout] [(penalty-fn base-layout) base-layout]
               i 0]
          (if (= i 50) [penalty layout]
              (recur (apply max-key first
                            (conj (pmap (calculate-swap layout 3000 20) (range 3000))
                                  [penalty layout]))
                     (inc i)))))


  (defn penguin [layout]
    (apply max-key first
           (pmap (fn [_]
                   (let [next (reduce layout/swap-keys layout (collect-swaps 8))]
                     [(penalty-fn next) next]))
                 (range 500))))

  (penguin (second (penguin base-layout)))

  (penalty-fn base-layout)

  (def penguin1 (time (no-annealing-sim {:rounds 5 :round-i 30000})))
  (first penguin1)

  (def penguin2 (time (no-annealing-sim {:rounds 50 :round-i 6000})))
  (first penguin2)

  (+ 1 2)

  (def penguin3 (time (no-annealing-sim {:rounds 100 :round-i 3000})))
  (first penguin3)

  (def penguin4 (time (no-annealing-sim {:rounds 500 :round-i 300})))
  (first penguin4)


  (def otter (time (sim)))
  (penalty-fn otter))
