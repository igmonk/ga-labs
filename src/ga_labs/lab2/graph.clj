(ns ga-labs.lab2.graph
  (:require [ga-labs.common.util :as u]))

(def level-curve-step 500)

(defn ds-vector->xyz-maps [[x y z]]
  {:x x :y y :z z})

(defn translate-to-xyz [data-set]
  (map ds-vector->xyz-maps data-set))

(defn create-level-curve [ds-xyz level]
  (letfn [(level-curve-pred [level]
            (fn [{:keys [z]}]
              (= (int z) (int level))))]
    {:level level
     :ds-xyz (filter (level-curve-pred level) ds-xyz)}))

(defn create-level-curves [ds-xyz min-z max-z]
  (let [levels (range min-z max-z level-curve-step)]
    (map #(create-level-curve ds-xyz %) levels)))

(defn contour-plot [data-set]
  (let [ds-xyz (translate-to-xyz data-set)
        {min-z :min max-z :max} (u/min-max ds-xyz :z)]
    (create-level-curves ds-xyz min-z max-z)))

(defn min-x-min-y [x-min y-min x-max y-max]
  {:x-min x-min :y-min y-min :x-max x-max :y-max y-max})

(defn define-xy-min-max [xy-values]
  (letfn [(min-max-reducer [{:keys [x-min y-min x-max y-max]} {:keys [x y]}]
            (min-x-min-y (min x-min x) (min y-min y) (max x-max x) (max y-max y)))]
    (reduce min-max-reducer
            (min-x-min-y Integer/MAX_VALUE Integer/MAX_VALUE Integer/MIN_VALUE Integer/MIN_VALUE)
            xy-values)))

(defn define-xy-scale [w h xy-values]
  (let [{:keys [x-min y-min x-max y-max]} (define-xy-min-max xy-values)
        xy-w (Math/abs (- x-max x-min))
        xy-h (Math/abs (- y-max y-min))
        x-scale (/ w xy-w)
        y-scale (/ h xy-h)]
    [x-scale y-scale]))
