(ns ga-labs.lab2.main-frame
  (:require [seesaw.core :refer :all]
            [seesaw.graphics :refer :all]
            [seesaw.color :refer :all]
            [ga-labs.lab2.graph :as g])
  (:import [java.awt Color]))

(defn scale-values [x-scale y-scale xy-values]
  (map #(hash-map :x (* (:x %) x-scale)
                  :y (* (:y %) y-scale))
       xy-values))

(defn position-values [w h xy-values]
  (let [w-half (* 0.5 w)
        h-half (* 0.5 h)]
    (map #(hash-map :x (+ (:x %) w-half)
                    :y (- h-half (:y %)))
         xy-values)))

(defn prepare-xy-values [w h x-scale y-scale xy-values]
  (->> xy-values
       (scale-values x-scale y-scale)
       (position-values w h)))

(defn draw-dots [g xy-values]
  (let [dx 1
        dy 1
        dx-double (* 2 dx)
        dy-double (* 2 dy)]
    (doseq [{:keys [x y]} xy-values]
      (.fill g (ellipse (- x dx) (- y dy) dx-double dy-double)))))

(defn draw-circles [g xy-values radius]
  (doseq [{:keys [x y]} xy-values]
    (.fill g (ellipse (- x radius) (- y radius) (* 2 radius) (* 2 radius)))))

(defn level->color [level]
  (let [lvl (int (Math/pow level 2))
        r (mod (+ lvl 50) 255)
        g (mod (+ lvl 100) 255)
        b (mod (+ lvl 150) 255)]
    (Color. r g b)))

(defn create-paint-fn [graph-contour-plot-ds population-ds]
  (let [gcp-ds (flatten (map :ds-xyz graph-contour-plot-ds))
        gcp-ds-xy-values (map #(select-keys % [:x :y]) gcp-ds)]
    (fn [c g]
      (let [w (.getWidth c)
            h (.getHeight c)
            population-color (color 255 0 0)
            [x-scale y-scale] (g/define-xy-scale w h gcp-ds-xy-values)
            population-ds-values (prepare-xy-values w h x-scale y-scale population-ds)
            #_gcp-values #_(prepare-xy-values w h x-scale y-scale gcp-ds-xy-values)]

        (doseq [{:keys [level ds-xyz]} graph-contour-plot-ds]
          (let [xyz-values (prepare-xy-values w h x-scale y-scale ds-xyz)]
            (.setColor g (level->color level))
            (draw-dots g xyz-values)))

        ; Draw auxiliary stuff
        #_(draw-aux-xs g aux-xs-values)
        #_(draw-aux-ys g aux-ys-values)

        ; Draw population
        (.setColor g population-color)
        (draw-circles g population-ds-values 3)))))

(defn fetch-value [f selector]
  (read-string (.getText (select f [selector]))))

(defn init-action [name callback]
  (action :name name
          :handler (fn [event]
                     (let [f (to-frame event)]
                       (callback {:pop-size (fetch-value f :#pop-size)
                                  :co-prob (fetch-value f :#co-prob)
                                  :mutation-prob (fetch-value f :#mutation-prob)
                                  :num-of-generations (fetch-value f :#num-of-generations)})))))

(defn run-action [name callback]
  (action :name name
          :handler #(future (callback %))))

(defn clear-action [name callback]
  (action :name name
          :handler #(future (callback %))))

(defn switch-paint [f paint]
  (-> f
      (select [:#canvas])
      (config! :paint paint)))

(defn show-frame-new [graph-contour-plot-ds population-data-set init-callback run-callback clear-callback]
  (let [f (frame
            :title "GA"
            :minimum-size [640 :by 480]
            :on-close :exit
            :content (border-panel :hgap 5 :vgap 5 :border 5
                                   ; Create the canvas with initial nil paint function, i.e. just canvas
                                   ; will be filled with it's background color and that's it.
                                   :center (canvas :id :canvas :background "#DCDCDC" :paint (create-paint-fn graph-contour-plot-ds @population-data-set))

                                   ; Some buttons to swap the paint function
                                   :north (horizontal-panel :items ["Population size: " (text :id :pop-size :text 40)
                                                                    "Crossing-Over Prob.: " (text :id :co-prob :text 0.5)
                                                                    "Mutation Prob.: " (text :id :mutation-prob :text 0.4)
                                                                    "Num. of generations: " (text :id :num-of-generations :text 20)
                                                                    (init-action "Init" init-callback)
                                                                    (run-action "Run" run-callback)
                                                                    (clear-action "Clear" clear-callback)])))]

    (add-watch population-data-set
               :key
               #(invoke-later
                  (switch-paint f (create-paint-fn graph-contour-plot-ds %4))))

    (invoke-later
      (-> f pack! show!))))
