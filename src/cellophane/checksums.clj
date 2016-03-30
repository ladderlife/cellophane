(ns cellophane.checksums
  (:require [clojure.string :as str]))

;; ===================================================================
;; Checksums (data-react-checksum)

(def MOD 65521)

;; TODO: refactor this into more idiomatic Clojure. ATM it's a simple port of
;; https://github.com/facebook/react/blob/3b96650/src/shared/utils/adler32.js
(defn- adler32 [data]
  (let [a (atom 1)
        b (atom 0)
        l (count data)
        m (bit-and l (bit-not 0x3))
        i
        (loop [i 0]
          (if (< i m)
            (let [n (Math/min (+ i 4096) m)]
              (run!
                (fn [idx]
                  (let [a0 (swap! a + (nth data idx))
                        a1 (swap! a + (nth data (+ idx 1)))
                        a2 (swap! a + (nth data (+ idx 2)))
                        a3 (swap! a + (nth data (+ idx 3)))]
                    (swap! b + a0 a1 a2 a3)))
                (range i n 4))
              (swap! a mod MOD)
              (swap! b mod MOD)
              (recur (+ (last (range i n 4)) 4)))
            i))]
    (loop [i i]
      (if (< i l)
        (let [a' (swap! a + (nth data i))]
          (swap! b + a')
          (recur (inc i)))))
    (swap! a mod MOD)
    (swap! b mod MOD)
    (bit-or @a (unchecked-int (bit-shift-left @b 16)))))

(defn checksum [data]
  (adler32 (.getBytes data)))

(defn assign-react-checksum [markup]
  (->> (str/split markup #">" 2)
    (interpose (str " data-react-checksum=\"" (checksum markup) "\">"))
    str/join))
