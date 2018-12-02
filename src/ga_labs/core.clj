(ns ga-labs.core
  (:gen-class)
  (:require [ga-labs.lab1.main :as lab1]
            [ga-labs.lab2.main :as lab2]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  #_(lab1/run)
  (lab2/run))
