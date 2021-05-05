(ns layout-gen.penalty.modifier
  (:require [layout-gen.layout :as layout]
            [clojure.math.numeric-tower :as math]))

;; shift 0.9
;; ctrl 0.9
;; alt 0.9^2
;; shift-ctrl 0.9^3
;; shift-alt 0.9^3
;; ctrl-alt 0.9^3
;; shift-ctrl-alt 0.9^5

(def base-penalty 0.9)

(defn penalty [val]
  (case (layout/derive-modifier val)
    :shift (math/expt 0.9 1)
    :ctrl (math/expt 0.9 1)
    :alt (math/expt 0.9 2)
    :shift-ctrl (math/expt 0.9 3)
    :shift-alt (math/expt 0.9 3)
    :ctrl-alt (math/expt 0.9 3)
    :shift-ctrl-alt (math/expt 0.9 5)
    :none 1.0
    :else 1.0))
