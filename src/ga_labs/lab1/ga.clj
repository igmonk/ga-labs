(ns ga-labs.lab1.ga
  (:require [ga-labs.lab1.chromosome :refer :all]
            [ga-labs.common.util :as u]))

; x [-9; 9]
(def x-min -9)
(def x-max 9)
(def x-step 0.01)

(def x-range (range x-min x-max x-step))

; f(t) = (0.5 * t - 1.4) * cos(0.5 * PI * t + 1.1)
(defn fitness-function [t]
  (* (- (* 0.5 t) 1.4)
     (Math/cos (+ (* 0.5 Math/PI t)
                  1.1))))

(defn plot-x [f x]
  [x (f x)])

(defn plot-xs []
  (map #(plot-x fitness-function %) x-range))

(defn plot-aux-x []
  (range x-min (inc x-max)))

(defn plot-aux-y [data-set-plot]
  (let [ys (map second data-set-plot)
        y-min (apply min ys)
        y-max (apply max ys)]
    (range y-min (inc y-max))))

(defn code->x [chromosomes-total code]
  (+ x-min
     (* code (/ (- x-max x-min)
                (- chromosomes-total 1)))))

(defn create-chromosome [chromosomes-total code]
  (let [gray-code (u/code->gray-code code)
        x (code->x chromosomes-total code)
        y (fitness-function x)]
    (->Chromosome code gray-code x y)))

(defn random-pop [chromosome-size n]
  (let [chromosomes-total (Math/pow 2 chromosome-size)
        chromosomes-range (range 0 chromosomes-total)
        indexes (take n (repeatedly #(rand-int chromosomes-total)))]
    (->> (map #(nth chromosomes-range %) indexes)
         (map (partial create-chromosome chromosomes-total)))))

(defn apply-selection [chromosomes]
  (letfn [(create-y-total-reducer [fitness-delta]
            (fn [acc {:keys [y]}]
              (+ acc y fitness-delta)))
          (define-probability [{:keys [y] :as chromosome} y-total fitness-delta]
            {:chromosome chromosome
             :probability (/ (+ y fitness-delta)
                             y-total)})
          (first-less [probability-sorted r]
            (when-let [first-item (first probability-sorted)]
              (let [probability (:probability first-item)]
                (if (< r probability)
                  first-item
                  (recur (rest probability-sorted) (- r probability))))))]
    (let [size (count chromosomes)
          min-y (:y (first (sort-by :y < chromosomes)))
          fitness-delta (+ (Math/abs min-y) 1)
          y-total (reduce (create-y-total-reducer fitness-delta) 0 chromosomes)
          probability-map (map #(define-probability % y-total fitness-delta) chromosomes)
          probability-sorted (sort-by :probability > probability-map)
          probability-based (take size (repeatedly #(first-less probability-sorted (rand))))]
      (map :chromosome probability-based))))

(defn parents-seq [chromosomes]
  (let [size (count chromosomes)
        parent-1 (nth chromosomes (rand-int size))
        parent-2 (nth chromosomes (rand-int size))]
    (lazy-seq (cons [parent-1 parent-2]
                    (parents-seq chromosomes)))))

(defn gray-code-crossing-over [code-1 code-2 code-size probability]
  "Crossing-Over.
  mask-right : 0000xxxx
  mask-left  : xxxx0000"
  (let [num-to-co (int (* code-size probability))
        num-to-stay (- code-size num-to-co)
        mask-right (int (- (Math/pow 2 num-to-co) 1))
        mask-left-base (int (- (Math/pow 2 num-to-stay) 1))
        mask-left (bit-shift-left mask-left-base num-to-co)
        left-1 (bit-and code-1 mask-left)
        right-1 (bit-and code-1 mask-right)
        left-2 (bit-and code-2 mask-left)
        right-2 (bit-and code-2 mask-right)]
    [(bit-or left-1 right-2)
     (bit-or left-2 right-1)]))

(defn chromosome-crossing-over [chromosome-1 chromosome-2 chromosome-size probability]
  (let [chromosomes-total (Math/pow 2 chromosome-size)
        [gray-code-1 gray-code-2] (gray-code-crossing-over (:gray-code chromosome-1)
                                                           (:gray-code chromosome-2)
                                                           chromosome-size
                                                           probability)
        code1 (u/gray-code->code gray-code-1)
        x1 (code->x chromosomes-total code1)
        y1 (fitness-function x1)
        code2 (u/gray-code->code gray-code-2)
        x2 (code->x chromosomes-total code2)
        y2 (fitness-function x2)]
    [(->Chromosome code1 gray-code-1 x1 y1)
     (->Chromosome code2 gray-code-2 x2 y2)]))

(defn apply-crossing-over [chromosomes chromosome-size probability]
  (let [size (count chromosomes)
        num-of-pairs (int (/ size 2))
        parents (take num-of-pairs (parents-seq chromosomes))
        parents-co (fn [[parent-1 parent-2]]
                     (chromosome-crossing-over parent-1 parent-2 chromosome-size probability))]
    (flatten (map parents-co parents))))

(defn inverse-bit-at [n index]
  (bit-xor n (bit-shift-left 1 index)))

(defn mutate-gray-code [gray-code chromosome-size]
  (inverse-bit-at gray-code (rand-int chromosome-size)))

(defn mutate-chromosome [chromosome chromosome-size probability]
  (let [r (rand)]
    (if (> r probability)
      (let [chromosomes-total (Math/pow 2 chromosome-size)
            gray-code (mutate-gray-code (:gray-code chromosome) chromosome-size)
            code (u/gray-code->code gray-code)
            x (code->x chromosomes-total code)]
        (->Chromosome code gray-code x (fitness-function x)))
      chromosome)))

(defn apply-mutation [chromosomes chromosome-size probability]
  (map #(mutate-chromosome % chromosome-size probability) chromosomes))

(defn next-pop [chromosomes chromosome-size co-probability mutation-probability]
  (-> chromosomes
      apply-selection
      (apply-crossing-over chromosome-size co-probability)
      (apply-mutation chromosome-size mutation-probability)))
