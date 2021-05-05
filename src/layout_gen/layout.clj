(ns layout-gen.layout
  (:require [malli.core :as m]
            [malli.generator :as mg]
            [clojure.string :as str]))


; SPECS 


(def Hand
  [:enum :left :right])
(def Finger
  [:enum :thumb :index :middle :ring :pinky])
(def Row
  [:enum :number :top :home :bottom :thumb])
(def Modifier
  [:enum :none :shift :ctrl :alt :shift-ctrl :shift-alt :ctrl-alt :shift-ctrl-alt])
(def Key
  [:map
   {:registry {:key/val string?
               :key/position nat-int? ;; Foreign Key
               :key/masked boolean?
               :key/used boolean?}}
   :key/val :key/position :key/masked :key/used])

(comment
  (mg/sample Hand)
  (mg/sample Finger)
  (mg/sample Row)
  (mg/sample Modifier)
  (mg/sample Key))


; CONSTANTS


(def lower-keys ["1" "2" "3" "4" "5"
                 "q" "w" "e" "r" "t"
                 "a" "s" "d" "f" "g"
                 "z" "x" "c" "v" "b"])
(defn upper-case-nums [s]
  (-> s
      (str/replace #"1" "!")
      (str/replace #"2" "@")
      (str/replace #"3" "#")
      (str/replace #"4" "\\$")
      (str/replace #"5" "%")))

; MODIFIER FNS

(defn shift [keys]
  (mapv (comp str/upper-case upper-case-nums) keys))

(defn ctrl [keys]
  (mapv #(str "c" %) keys))

(defn alt [keys]
  (mapv #(str "a" %) keys))

(defn keys-with-all-modifiers [keys]
  (concat keys
          (->> keys shift)
          (->> keys ctrl)
          (->> keys alt)
          (->> keys shift ctrl)
          (->> keys shift alt)
          (->> keys ctrl alt)
          (->> keys shift ctrl alt)))


; DEFAULTS


(def default-used
  (set (concat lower-keys
               (->> lower-keys shift)
               (->> lower-keys ctrl))))

(def default-masked (set (keys-with-all-modifiers ["e" "s" "d" "f"])))


; DERIVATION FNS


(defn -derive-hand [_pos]
  :left)
(def =>-derive-hand [:=> [:cat int?] Hand])
(def derive-hand
  "Always going to be left hand until expansion"
  (memoize -derive-hand))

(def compare-finger-map
  {:pinky 0
   :ring 1
   :middle 2
   :index 3})
(defn -derive-finger [pos]
  (case (rem pos 5)
    0 :pinky
    1 :ring
    2 :middle
    :index))
(def =>-derive-finger [:=> [:cat int?] Finger])
(def derive-finger
  "This is the naieve non-configured finger derivation. 
   Later, configuration driven process should inform this"
  (memoize -derive-finger))

(def compare-row-map
  {:bottom 0
   :home 1
   :top 2
   :number 3})
(defn -derive-row [pos]
  (case (quot (rem pos 20) 5)
    0 :number
    1 :top
    2 :home
    3 :bottom))
(def =>-derive-row [:=> [:cat int?] Row])
(def derive-row
  "Deriving row is just about immutable!"
  (memoize -derive-row))

(defn -derive-center [pos]
  (case (rem pos 5)
    4 true
    false))
(def =>-derive-center [:=> [:cat int?] boolean?])
(def derive-center
  "The center might change if you swap the movement keys. This function then may have to update"
  (memoize -derive-center))

(defn -derive-modifier [key]
  (letfn [(ctrl? [b] (when b "c"))
          (alt? [b] (when b "a"))
          (shift? [b] (if b "[A-Z!@#$%]{1}" "[a-z12345]{1}"))
          (build-pattern [{:keys [shift ctrl alt]}]
            (re-pattern (str (ctrl? ctrl) (alt? alt) (shift? shift))))]
    (cond
      (re-matches (build-pattern {:shift true}) key) :shift
      (re-matches (build-pattern {:ctrl true}) key) :ctrl
      (re-matches (build-pattern {:alt true}) key) :alt
      (re-matches (build-pattern {:shift true :ctrl true}) key) :shift-ctrl
      (re-matches (build-pattern {:shift true :alt true}) key) :shift-alt
      (re-matches (build-pattern {:ctrl true :alt true}) key) :ctrl-alt
      (re-matches (build-pattern {:shift true :ctrl true :alt true}) key) :shift-ctrl-alt
      :else :none)))
(def =>-derive-modifier [:=> [:cat int?] Modifier])
(def derive-modifier (memoize -derive-modifier))


; KEY/LAYOUT BUILDING


(defn build-key [{:keys [used masked]} pos val]
  {:key/position pos
   :key/val val
   :key/used (contains? (or used default-used) val)
   :key/masked (contains? (or masked default-masked) val)})

(defn build-keyvec [{:keys [used masked basekeys]}]
  (let [keyvals (keys-with-all-modifiers basekeys)]
    (->> (map-indexed (partial build-key {:used used :masked masked}) keyvals)
         (filter #((or used default-used) (:key/val %)))
         vec)))

(defn derive-index->key [keyvec]
  (reduce (fn [acc {position :key/position :as key}]
            (assoc acc position key))
          {} keyvec))

(defn derive-value->key [keyvec]
  (reduce (fn [acc {val :key/val :as key}]
            (assoc acc val key))
          {} keyvec))

; SWAP AND SORT FNS


(defn -swap-keys [keys [posa posb]]
  (let [[swapa swapb] [(assoc (nth keys posa) :key/position posb)
                       (assoc (nth keys posb) :key/position posa)]]
    (-> keys
        (assoc (:key/position swapa) swapa)
        (assoc (:key/position swapb) swapb))))
(def =>-swap-keys [:=> [:cat [:* Key] :cat [nat-int? nat-int?]] [:* Key]])
(def swap-keys (memoize -swap-keys))

(defn sort-keyvec [keys]
  (sort-by :key/position keys))

(comment

  (mg/sample Key)

  (build-keyvec {:basekeys lower-keys})

  (def key-vec (build-keyvec {:basekeys lower-keys}))

  (swap-keys key-vec 0 5))
