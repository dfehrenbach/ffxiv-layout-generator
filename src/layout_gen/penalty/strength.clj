(ns layout-gen.penalty.strength
  (:require [clojure.core.matrix :as matrix]))

;;TODO: Add number row
;; 
(def little-force 1.84)
(def ring-force 2.02)
(def middle-force 2.36)
(def index-force 2.26)

(def middle-norm 1.0)
(def little-norm (/ little-force middle-force))
(def ring-norm (/ ring-force middle-force))
(def index-norm (/ index-force middle-force))

(def strength-vec
  [little-norm  ring-norm  middle-norm  index-norm  index-norm
   little-norm  ring-norm  middle-norm  index-norm  index-norm
   little-norm  ring-norm  middle-norm  index-norm  index-norm
   little-norm  ring-norm  middle-norm  index-norm  index-norm])

(def raw-strength-matrix
  (->> (for [av strength-vec
             bv strength-vec]
         (+ av  bv))
       (partition (count strength-vec))
       (mapv vec)))
raw-strength-matrix

(defn normalize-strength [val]
  (let [raw-min (matrix/emin raw-strength-matrix)
        raw-max (matrix/emax raw-strength-matrix)
        new-max 1.0
        new-min (/ raw-min raw-max)]
    (+ new-min
       (/ (* (- val raw-min) (- new-max new-min))
          (- raw-max raw-min)))))

(def normalized-matrix
  (mapv (fn [row]
          (mapv normalize-strength row))
        raw-strength-matrix))

(comment
  normalized-matrix)
