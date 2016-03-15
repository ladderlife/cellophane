(ns cellophane.test-utils
  (:require [clojure.string :as str]))

(defn remove-whitespace [s]
  (->> (str/split s #">\s+<")
    (filter #(not (str/blank? %)))
    (str/join "><")))
