(ns ga-labs.common.util)

(defn pow-of-2-ge [x]
  "Defines power which should be applied to the number 2 which gives N >= x"
  (cond
    (<= x 1) 0
    (<= x 2) 1
    :else (inc (pow-of-2-ge (/ x 2)))))

(defn code->gray-code [code]
  (bit-xor code (bit-shift-right code 1)))

(defn gray-code->code [gray-code]
  (loop [result gray-code
         mask (bit-shift-right gray-code 1)]
    (if (= mask 0)
      result
      (recur (bit-xor result mask)
             (bit-shift-right mask 1)))))

(defn first-segment [segments key n]
  (when-let [first-seg (first segments)]
    (let [value (get first-seg key)]
      (if (< n value)
        first-seg
        (recur (rest segments) key (- n value))))))

(defn create-roulette-seq [key segments]
  (repeatedly #(first-segment segments key (rand))))

(defn min-max [items get-value]
  (letfn [(min-max-reducer [acc item]
            (let [value (get-value item)]
              {:min (min (:min acc) value)
               :max (max (:max acc) value)}))]
    (reduce min-max-reducer
            {:min Integer/MAX_VALUE :max Integer/MIN_VALUE}
            items)))
