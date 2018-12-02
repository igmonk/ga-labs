(ns ga-labs.lab2.main
  (:require [ga-labs.lab2.main-frame :as mf]
            [ga-labs.common.util :as u]
            [ga-labs.lab2.ga :as ga]
            [ga-labs.lab2.graph :as g]))

; GA options:
; pop-size - size of population
; num-of-genes - number of genes in each chromosome
; co-prob - Crossing-Over probability
; mutation-prob - Mutation probability
; num-of-generations - Number of generations

; x [-50; 50]
(def x-min -50)
(def x-max 50)
(def x-step 0.1)
(def x-graph-step 0.05)

(def x-range (range x-min x-max x-step))
(def num-of-genes (u/pow-of-2-ge (count x-range)))
(def chromosomes-total (Math/pow 2 num-of-genes))
(def x-graph-range (range x-min x-max x-graph-step))

(def ga-options (atom {}))
(def population (atom []))
(def pop-data-set (atom []))

; f(x1, x2) = sqr(x1) + 2 * sqr(x2) - 0.3 * cos(3 * PI * x1) - 0.4 * cos(4 * PI * x2) + 0.7
(defn fitness-function [x1 x2]
  (+ (Math/pow x1 2)
     (* 2 (Math/pow x2 2))
     (* -0.3 (Math/cos (* 3 Math/PI x1)))
     (* -0.4 (Math/cos (* 4 Math/PI x2)))
     0.7))

(defn code->x [code]
  (+ x-min
     (* code (/ (- x-max x-min)
                (- chromosomes-total 1)))))

(defn individuals->data-set [individuals]
  (letfn [(xyz-mapper [{:keys [chromosome1 chromosome2]}]
            (let [x1 (code->x (:code chromosome1))
                  x2 (code->x (:code chromosome2))]
              {:x x1 :y x2 :z (fitness-function x1 x2)}))]
    (map xyz-mapper individuals)))

(add-watch population :kk #(reset! pop-data-set (individuals->data-set %4)))

(defn create-graph-ds []
  (for [x1 x-graph-range
        x2 x-graph-range]
    [x1 x2 (fitness-function x1 x2)]))

(defn init-callback [ga-opts]
  (reset! ga-options ga-opts)
  (reset! population (ga/random-pop num-of-genes (:pop-size ga-opts))))

(defn run-callback [_]
  (let [num-of-generations (:num-of-generations @ga-options)]
    (loop [n num-of-generations]
      (when (> n 0)
        (Thread/sleep 1000)
        (let [{:keys [co-prob mutation-prob]} @ga-options
              next-pop (ga/next-pop @population fitness-function x-min x-max num-of-genes co-prob mutation-prob)]
          (reset! population next-pop)
          (recur (dec n)))))))

(defn clear-callback [_]
  (reset! pop-data-set []))

(defn run []
  (let [graph-data-set (create-graph-ds)
        graph-contour-plot-ds (g/contour-plot graph-data-set)]
    (mf/show-frame-new graph-contour-plot-ds
                       pop-data-set
                       init-callback
                       run-callback
                       clear-callback)))

