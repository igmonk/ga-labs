(ns ga-labs.lab1.main
  (:require [ga-labs.lab1.main-frame :as mf]
            [ga-labs.lab1.ga :as ga]))

; GA options:
; pop-size - size of population
; num-of-genes - number of genes in each chromosome
; co-prob - Crossing-Over probability
; mutation-prob - Mutation probability
; num-of-generations - Number of generations

(def select-values (comp vals select-keys))

(defn plot-chromosomes [chromosomes]
  (map #(select-values % [:x :y]) chromosomes))

(def ga-options (atom {}))
(def population (atom []))
(def pop-data-set (atom []))

(add-watch population :kk #(reset! pop-data-set (plot-chromosomes %4)))

(defn init-callback [{:keys [pop-size num-of-genes] :as ga-opts}]
  (reset! ga-options ga-opts)
  (reset! population (ga/random-pop num-of-genes pop-size)))

(defn run-callback [_]
  (let [num-of-generations (:num-of-generations @ga-options)]
    (loop [n num-of-generations]
      (when (> n 0)
        (Thread/sleep 1000)
        (let [{:keys [num-of-genes co-prob mutation-prob]} @ga-options
              next-pop (ga/next-pop @population num-of-genes co-prob mutation-prob)]
          (reset! population next-pop)
          (recur (dec n)))))))

(defn clear-callback [_]
  (reset! pop-data-set []))

(defn run []
  (let [data-set (ga/plot-xs)
        data-set-aux-x (ga/plot-aux-x)
        data-set-aux-y (ga/plot-aux-y data-set)]
    (mf/show-frame-new data-set
                       data-set-aux-x
                       data-set-aux-y
                       pop-data-set
                       init-callback
                       run-callback
                       clear-callback)))
