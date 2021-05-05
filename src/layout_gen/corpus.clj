(ns layout-gen.corpus
  (:require [clojure.java.io :as io]
            [clojure-csv.core :as csv]
            [semantic-csv.core :as sc]
            [clojure.string :as str]))

(defn ->skill-only [v]
  (if-let [[_all _player skill _target] (re-find #"^(.+) casts\s{1,2}(.+) on\s{1,2}(.+)$" v)]
    skill
    (let [[_all _player skill] (re-find #"^(.+) casts\s{1,2}(.+)" v)]
      skill)))

(defn extract-data [filename]
  (with-open [in-file (io/reader filename)]
    (->> in-file
         csv/parse-csv
         sc/remove-comments
         sc/mappify
         (sc/cast-with {:Event ->skill-only})
         doall)))

(defn get-data []
  (concat
   (extract-data "resources/blm/part1.csv")
   (extract-data "resources/blm/part2.csv")))

(def skill->key
  {"Triplecast"     "a"
   "Fire IV"        "r"
   "Despair"        "t"
   "Sharpcast"      "A"
   "Ley Lines"      "x"
   "Manafont"       "z"
   "Thunder III"    "g"
   "Fire"           "q"
   "Blizzard IV"    "R"
   "Transpose"      "!"
   "Addle"          "b"
   "Xenoglossy"     "T"
   "Umbral Soul"    "1"
   "Manaward"       "c"
   "Lucid Dreaming" "C"
   "Item_Fbfb3"     "X"
   "Enochian"       "2"
   "Swiftcast"      "G"
   "Fire III"       "w"
   "Blizzard III"   "W"
   "Surecast"       "V"})

(defn movement-sample [prob]
  (str/join (random-sample prob ["s" "d" "f" "e"])))
(def movement-prob 0.4)

(defn gen-corpus
  ([]
   (gen-corpus (get-data)))
  ([data]
   (->> data
        (map :Event)
        (map skill->key)
        (remove nil?)
        (map keyword)
        (interleave (repeatedly (partial movement-sample movement-prob)))
        (remove #{""})
        (mapcat (fn [x] (if (keyword? x) [x]
                            (str/split x #""))))
        (map (fn [x] (if (string? x) x
                         (str/replace (str x) #":" "")))))))

(defn gen-large-corpus
  ([]
   (gen-large-corpus (get-data)))
  ([data]
   (apply concat (repeatedly 10 (partial gen-corpus data)))))

(defn gen-very-large-corpus []
  (apply concat (repeatedly 10 gen-large-corpus)))

(defn build-bigrams-map [corpus]
  (frequencies (partition 2 corpus)))

(comment
  (def data (get-data))

  (defn pen [data]
    (mapcat (fn [x] (if (keyword? x) [x]
                        (str/split x #""))) data))

  (str/replace (str :r) #":" "")
  (->> data
       (map :Event)
       (map skill->key)
       (remove nil?)
       (map keyword)
       (interleave (repeatedly (partial movement-sample 0.4)))
       (remove #{""})
       (mapcat (fn [x] (if (keyword? x) [x]
                           (str/split x #""))))
       (map (fn [x] (if (string? x) x
                        (str/replace (str x) #":" "")))))

  (build-bigrams-map (gen-very-large-corpus)))
