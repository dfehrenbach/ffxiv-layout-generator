(ns layout-gen.util
  (:require [clojure.java.io :as io]))

(defn read-file [path]
  (with-open [r (java.io.PushbackReader. (io/reader path))]
    (binding [*read-eval* false]
      (read r))))

(defn write-file [path data]
  (with-open [w (clojure.java.io/writer path)]
    (binding [*out* w]
      (pr data))))
