(ns layout-gen.penalty.flow
  (:require [layout-gen.layout :as layout]
            [clojure.math.numeric-tower :as math]))

; ;Direction:

;; - outward = 0.9: outward roll of fingers from the index to little finger (same hand)

; Dexterity:

;; - side_above_3away = 0.9
;;     - index and little finger type two keys, one or more rows apart (same hand)
;; - side_above_2away = 0.9^2 = 0.81
;;     - index finger types key one or more rows above ring finger key, or
;;     - little finger types key one or more rows above middle finger key (same hand)
;; - side_above_1away = 0.9^3 = 0.729
;;     - index finger types key one or more rows above middle finger key, or
;;     - little finger types key one or more rows above ring finger key (same hand)
;; - middle_above_ring = 0.9
;;     - middle finger types key one or more rows above ring finger key (same hand)
;; - ring_above_middle = 0.9^3 = 0.729
;;     - ring finger types key one or more rows above middle finger key (same hand)
;; - lateral = 0.9
;;     - lateral movement of (index or little) finger outside of 8 vertical columns

; Distance:

;; - skip_row_3away = 0.9       
;;     - index and little fingers type two keys that skip over home row (same hand)
;;     - (e.g., one on bottom row, the other on top row)
;; - skip_row_2away = 0.9^3 = 0.729
;;     - little and middle or index and ring fingers type two keys that skip over home row (same hand)
;; - skip_row_1away = 0.9^5 = 0.59049
;;     - little and ring or middle and index fingers type two keys that skip over home row (same hand)
;; - skip_row_0away = 0.9^7 = 0.47829
;;     - same finger types two keys that skip over home row

;; - skip_2rows_3away = 0.9^3 = 0.729
;;     - index and little fingers type two keys that skip over home and top row (same hand)
;;     - (e.g., one on bottom row, the other on top row)
;; - skip_2rows_2away = 0.9^5 = 0.59049
;;     - little and middle or index and ring fingers type two keys that skip over home and top row (same hand)
;; - skip_2rows_1away = 0.9^7 = 0.4782969
;;     - little and ring or middle and index fingers type two keys that skip over home and top row (same hand)
;; - skip_2rows_0away = 0.9^9 = 0.387
;;     - same finger types two keys that skip over home and top row

; MODIFIERS: 
; LIKELY pushing these to another file/function
; As it stands speed, strength, and flow will be each be functions of:
; position -> position -> penalty
; If all of the matrices can be multiplied together then We can have a [posa posb] -> penalty lookup
; The modifier positions can be applied afterwards

;; shift 0.9
;; ctrl 0.9
;; alt 0.9^2
;; shift-ctrl 0.9^3
;; shift-alt 0.9^3
;; ctrl-alt 0.9^3
;; shift-ctrl-alt 0.9^5

(def penalties
  {:outward 0.9
   :side-above 0.9
   :lateral 0.9
   :skip-row 0.9})

(comment
  layout/derive-center
  layout/derive-finger
  layout/derive-row
  layout/derive-modifier)

(comment
  (def pen {:bottom 0
            :home 1
            :top 2
            :number 3})

  (sort #(compare (pen %1) (pen %2)) [:bottom :number :home :top])
  (compare (pen :bottom) (pen :top))
  0)

(defn -progression-is-above [rowa rowb]
  (= -1 (compare (layout/compare-row-map rowa)
                 (layout/compare-row-map rowb))))
(def progression-is-above (memoize -progression-is-above))

(defn -progression-is-outward [fingera fingerb]
  (= 1 (compare (layout/compare-finger-map fingera)
                (layout/compare-finger-map fingerb))))
(def progression-is-outward (memoize -progression-is-outward))

(defn -row-diff [rowa rowb]
  (math/abs (- (layout/compare-row-map rowa)
               (layout/compare-row-map rowb))))
(def row-diff (memoize -row-diff))

(defn -finger-diff [fingera fingerb]
  (math/abs (- (layout/compare-finger-map fingera)
               (layout/compare-finger-map fingerb))))
(def finger-diff (memoize -finger-diff))


(defn side-and-above [posa posb]
  (let [[rowa rowb] [(layout/derive-row posa) (layout/derive-row posb)]
        [fingera fingerb] [(layout/derive-finger posa) (layout/derive-finger posb)]]
    (cond
    ;; 3 away
      (and (progression-is-above rowa rowb)
           (= 3 (finger-diff fingera fingerb)))
      (math/expt (penalties :side-above) 1)

    ;; 2 away
      (and (progression-is-above rowa rowb)
           (= 2 (finger-diff fingera fingerb)))
      (math/expt (penalties :side-above) 2)

    ;; 1 away
      (and (progression-is-above rowa rowb)
           (= 1 (finger-diff fingera fingerb)))
      (math/expt (penalties :side-above) 3)

      :else 1.0)))

(defn lateral [posa posb]
  (if (or (layout/derive-center posa) (layout/derive-center posb))
    (penalties :lateral)
    1.0))

(defn skip-row [posa posb]
  (let [[rowa rowb] [(layout/derive-row posa) (layout/derive-row posb)]
        [fingera fingerb] [(layout/derive-finger posa) (layout/derive-finger posb)]]
    (cond
      (and (= 2 (row-diff rowa rowb))
           (= 3 (finger-diff fingera fingerb)))
      (math/expt (penalties :skip-row) 1)

      (and (= 2 (row-diff rowa rowb))
           (= 2 (finger-diff fingera fingerb)))
      (math/expt (penalties :skip-row) 3)

      (and (= 2 (row-diff rowa rowb))
           (= 1 (finger-diff fingera fingerb)))
      (math/expt (penalties :skip-row) 5)

      (and (= 2 (row-diff rowa rowb))
           (= 0 (finger-diff fingera fingerb)))
      (math/expt (penalties :skip-row) 4)

      (and (= 3 (row-diff rowa rowb))
           (= 3 (finger-diff fingera fingerb)))
      (math/expt (penalties :skip-row) 3)

      (and (= 3 (row-diff rowa rowb))
           (= 2 (finger-diff fingera fingerb)))
      (math/expt (penalties :skip-row) 5)

      (and (= 3 (row-diff rowa rowb))
           (= 1 (finger-diff fingera fingerb)))
      (math/expt (penalties :skip-row) 7)

      (and (= 3 (row-diff rowa rowb))
           (= 0 (finger-diff fingera fingerb)))
      (math/expt (penalties :skip-row) 6)

      :else 1.0)))

(defn outward [posa posb]
  (if (progression-is-outward (layout/derive-finger posa) (layout/derive-finger posb))
    0.9
    1.0))

(defn calc-flow [posa posb]
  (->> [side-and-above lateral skip-row outward]
       (pmap #(% posa posb))
       (reduce *)))

(def matrix
  (let [base-matrix-20 (vec (repeat 20 (vec (repeat 20 0))))
        all-coords (for [posa (range 20)
                         posb (range 20)]
                     [posa posb])]
    (reduce
     (fn [acc [posa posb]]
       (assoc-in acc [posa posb] (calc-flow posa posb)))
     base-matrix-20 all-coords)))

(comment
  matrix)
