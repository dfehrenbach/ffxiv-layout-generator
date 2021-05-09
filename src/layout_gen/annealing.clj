; Stochaistic optimisation based on simulated annealing.
; Math is shamelessly taken from: http://mkweb.bcgsc.ca/carpalx/?simulated_annealing
; This code is written to be generic and can be reused for other applications.
(ns layout-gen.annealing
  (:require [clojure.math.numeric-tower :as math]))

;; These values are taken from Carpalx, with T0 adjusted for the scale that our
;; penalty model outputs.
(def T0 1.5)
(def K 10.0)
(def P0 1.0)
(def N 15000) ;; TODO: Config later

;; T(i) = T0 exp(-ik/N)
(defn temperature [i]
  (* T0 (Math/exp (* (- i) (/ K N)))))


(comment
  (Math/exp)
  (for [i (range 0 15001 1000)]
    (temperature i)))


;; p(dE, i) = p0 exp(-dE/T(i))
(defn cutoff_p [dE i]
  (* P0 (Math/exp (/ (- dE) (temperature i)))))

(comment
  (cutoff_p 0.1 4000))

;; For positive dE, accept if r < p_dE where r ~ Uniform(0, 1)
;; larger num is better
;; d1 - d0
(defn accept_transition [dE i]
  (if (< 0.0 dE) true
      (< (rand) (cutoff_p (- dE), i))))

(comment
  (accept_transition -0.1 4000))

(defn get_simulation_range []
  (range 1 (+ N 1)))
