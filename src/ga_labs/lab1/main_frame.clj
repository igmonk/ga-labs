(ns ga-labs.lab1.main-frame
  (:require [seesaw.core :refer :all]
            [seesaw.graphics :refer :all]
            [seesaw.color :refer :all]))

(defn create-xy-min-max [x-min y-min x-max y-max]
  {:x-min x-min :y-min y-min :x-max x-max :y-max y-max})

(defn xy-min-max-reducer [{:keys [x-min y-min x-max y-max]} [x y]]
  (create-xy-min-max (min x-min x)
                     (min y-min y)
                     (max x-max x)
                     (max y-max y)))

(defn define-xy-min-max [xy-values]
  (reduce xy-min-max-reducer
          (create-xy-min-max Integer/MAX_VALUE
                             Integer/MAX_VALUE
                             Integer/MIN_VALUE
                             Integer/MIN_VALUE)
          xy-values))

(defn scale-value [x-scale y-scale [x y]]
  [(* x x-scale) (* y y-scale)])

(defn scale-values [x-scale y-scale xy-values]
  (map #(scale-value x-scale y-scale %) xy-values))

(defn define-xy-scale [w h xy-values]
  (let [{:keys [x-min y-min x-max y-max]} (define-xy-min-max xy-values)
        xy-w (Math/abs (- x-max x-min))
        xy-h (Math/abs (- y-max y-min))
        x-scale (/ w xy-w)
        y-scale (/ h xy-h)]
    [x-scale y-scale]))

(defn position-value [w-half h-half [x y]]
  [(+ x w-half) (- h-half y)])

(defn position-values [w h values]
  (let [w-half (* 0.5 w)
        h-half (* 0.5 h)]
    (map #(position-value w-half h-half %) values)))

(defn prepare-dataset [w h x-scale y-scale data-set]
  (->> data-set
       (scale-values x-scale y-scale)
       (position-values w h)))

(defn draw-line [g x-from y-from x-to y-to]
  (.drawLine g x-from y-from x-to y-to))

(defn draw-polyline [g [[x-from y-from] & xy-rest]]
  (when-not (empty? xy-rest)
    (let [[[x-to y-to]] xy-rest]
      (draw-line g x-from y-from x-to y-to)
      (recur g xy-rest))))

(defn draw-circles [g xy-values]
  (let [dx 3
        dy 3
        dx-double (* 2 dx)
        dy-double (* 2 dy)]
    (doseq [[x y] xy-values]
      (.fill g (ellipse (- x dx) (- y dy) dx-double dy-double)))))

(defn draw-aux-xs [g data-set]
  (let [dy 3
        dyy 6
        dxx 2]
    (doseq [{:keys [pos value]} data-set]
      (let [[x y] pos]
        (.drawLine g x y x (- y dy))
        (.drawString g (str value) (int (- x dxx)) (int (- y dyy)))))))

(defn draw-aux-ys [g data-set]
  (let [dx 3
        dxx 6]
    (doseq [{:keys [pos value]} data-set]
      (let [[x y] pos]
        (.drawLine g x y (+ x dx) y)
        (.drawString g (format "%.2f" value) (+ x dxx) (int y))))))

(defn augment-ds-aux-x [data-set-aux-x w h]
  (let [size (count data-set-aux-x)
        xs (range 0 w (/ w size))
        positions (map #(vector % h) xs)]
    (map #(hash-map :pos %1 :value %2) positions data-set-aux-x)))

(defn augment-ds-aux-y [data-set-aux-y h]
  (let [size (count data-set-aux-y)
        ys (range 0 h (/ h size))
        positions (map #(vector 0 %) ys)]
    (map #(hash-map :pos %1 :value %2) positions data-set-aux-y)))

(defn create-paint-fn [data-set data-set-aux-x data-set-aux-y population-data-set]
  (fn [c g]
    (let [w (.getWidth c)
          h (.getHeight c)

          aux-xs-values (augment-ds-aux-x data-set-aux-x w h)
          aux-ys-values (augment-ds-aux-y data-set-aux-y h)

          [x-scale y-scale] (define-xy-scale w h (concat data-set population-data-set))

          graph-values (prepare-dataset w h x-scale y-scale data-set)
          population-values (prepare-dataset w h x-scale y-scale population-data-set)

          graph-color (color 50 50 50)
          population-color (color 255 0 0)]

      ; Draw function graph
      (.setColor g graph-color)
      (draw-polyline g graph-values)

      ; Draw auxiliary stuff
      (draw-aux-xs g aux-xs-values)
      (draw-aux-ys g aux-ys-values)

      ; Draw population
      (.setColor g population-color)
      (draw-circles g population-values))))

(defn switch-paint [f paint]
  (-> f
      (select [:#canvas])
      (config! :paint paint)))

; Create an action that swaps the paint handler for the canvas.
; Note that we can use (config!) to set the :paint handler just like
; properties on other widgets.
(defn switch-paint-action [n paint]
  (action :name n
          :handler #(-> (to-frame %)
                        (switch-paint paint))))

(defn fetch-value [f selector]
  (read-string (.getText (select f [selector]))))

(defn init-action [name callback]
  (action :name name
          :handler (fn [event]
                     (let [f (to-frame event)]
                       (callback {:pop-size (fetch-value f :#pop-size)
                                  :num-of-genes (fetch-value f :#num-of-genes)
                                  :co-prob (fetch-value f :#co-prob)
                                  :mutation-prob (fetch-value f :#mutation-prob)
                                  :num-of-generations (fetch-value f :#num-of-generations)})))))

(defn run-action [name callback]
  (action :name name
          :handler #(future (callback %))))

(defn clear-action [name callback]
  (action :name name
          :handler #(future (callback %))))

(defn show-frame-new [data-set data-set-aux-x data-set-aux-y population-data-set init-callback run-callback clear-callback]
  (let [f (frame
            :title "GA"
            :minimum-size [640 :by 480]
            :on-close :exit
            :content (border-panel :hgap 5 :vgap 5 :border 5
                                   ; Create the canvas with initial nil paint function, i.e. just canvas
                                   ; will be filled with it's background color and that's it.
                                   :center (canvas :id :canvas :background "#DCDCDC" :paint (create-paint-fn data-set
                                                                                                             data-set-aux-x
                                                                                                             data-set-aux-y
                                                                                                             @population-data-set))

                                   ; Some buttons to swap the paint function
                                   :north (horizontal-panel :items ["Population size: " (text :id :pop-size :text 40)
                                                                    "Number of genes: " (text :id :num-of-genes :text 15)
                                                                    "Crossing-Over Prob.: " (text :id :co-prob :text 0.5)
                                                                    "Mutation Prob.: " (text :id :mutation-prob :text 0.4)
                                                                    "Num. of generations: " (text :id :num-of-generations :text 20)
                                                                    (init-action "Init" init-callback)
                                                                    (run-action "Run" run-callback)
                                                                    (clear-action "Clear" clear-callback)])))]

    (add-watch population-data-set
               :key
               #(invoke-later
                  (switch-paint f (create-paint-fn data-set data-set-aux-x data-set-aux-y %4))))

    (invoke-later
      (-> f pack! show!))))
