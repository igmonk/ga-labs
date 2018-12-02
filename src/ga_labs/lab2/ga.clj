(ns ga-labs.lab2.ga
  (:require [ga-labs.lab2.chromosome :refer :all]
            [ga-labs.lab2.individual :refer :all]
            [ga-labs.common.util :as u]))

(defn n-rand-int [n limit]
  (take n (repeatedly #(rand-int limit))))

(defn random-pop [chromosome-size n]
  (let [chromosomes-total (Math/pow 2 chromosome-size)
        codes1 (n-rand-int n chromosomes-total)
        codes2 (n-rand-int n chromosomes-total)
        mapper (fn [code1 code2]
                 (->Individual (->Chromosome code1)
                               (->Chromosome code2)))]
    (map mapper codes1 codes2)))

(defn code->x [chromosomes-total code x-min x-max]
  (+ x-min
     (* code (/ (- x-max x-min)
                (- chromosomes-total 1)))))

(defn get-ff-individuals [individuals chromosomes-total ff x-min x-max]
  (letfn [(ff-mapper [{:keys [chromosome1 chromosome2] :as individual}]
            (let [x1 (code->x chromosomes-total (:code chromosome1) x-min x-max)
                  x2 (code->x chromosomes-total (:code chromosome2) x-min x-max)]
              {:ff (ff x1 x2)
               :individual individual}))]
    (map ff-mapper individuals)))

(defn define-probability [{:keys [ff] :as ff-individual} ff-max-adj ff-total fitness-delta]
  (assoc ff-individual :probability (/ (- ff-max-adj (+ ff fitness-delta)) ff-total)))

(defn apply-selection [population chromosome-size x-min x-max ff]
  (let [size (count population)
        chromosomes-total (Math/pow 2 chromosome-size)
        ff-individuals (get-ff-individuals population chromosomes-total ff x-min x-max)
        ff-min (:ff (first (sort-by :ff < ff-individuals)))
        ff-delta (+ (Math/abs ff-min) 1)
        ff-max (:ff (first (sort-by :ff > ff-individuals)))
        ff-max-adj (+ ff-max ff-delta)
        ff-total (reduce #(+ %1 (- ff-max-adj (+ (:ff %2) ff-delta))) 0 ff-individuals)]
    (->> ff-individuals
         (map #(define-probability % ff-max-adj ff-total ff-delta))
         (sort-by :probability >)
         (u/create-roulette-seq :probability)
         (take size)
         (map :individual))))

(defn parents-seq [individuals]
  (let [size (count individuals)
        parent-1 (nth individuals (rand-int size))
        parent-2 (nth individuals (rand-int size))]
    (lazy-seq (cons [parent-1 parent-2]
                    (parents-seq individuals)))))

(defn individual-crossing-over [individual-1 individual-2 probability]
  (if (< (rand) probability)
    [individual-1 individual-2]
    (let [ind-1-code-1 (get-in individual-1 [:chromosome1 :code])
          ind-1-code-2 (get-in individual-1 [:chromosome2 :code])
          ind-2-code-1 (get-in individual-2 [:chromosome1 :code])
          ind-2-code-2 (get-in individual-2 [:chromosome2 :code])
          ind-1-code-1-co (if (< (rand) 0.5) ind-1-code-1 ind-2-code-1)
          ind-1-code-2-co (if (< (rand) 0.5) ind-1-code-2 ind-2-code-2)
          ind-2-code-1-co (if (< (rand) 0.5) ind-1-code-1 ind-2-code-1)
          ind-2-code-2-co (if (< (rand) 0.5) ind-1-code-2 ind-2-code-2)]
      [(->Individual (->Chromosome ind-1-code-1-co)
                     (->Chromosome ind-1-code-2-co))
       (->Individual (->Chromosome ind-2-code-1-co)
                     (->Chromosome ind-2-code-2-co))])))

(defn apply-crossing-over [population probability]
  (let [size (count population)
        num-of-pairs (int (/ size 2))
        parents (take num-of-pairs (parents-seq population))
        parents-co (fn [[parent-1 parent-2]]
                     (individual-crossing-over parent-1 parent-2 probability))]
    (flatten (map parents-co parents))))

(defn mutate-individual [individual probability]
  (if (< (rand) probability)
    individual
    (let [code-1 (get-in individual [:chromosome1 :code])
          code-2 (get-in individual [:chromosome2 :code])
          code-1-new (Math/abs (+ code-1 (- (rand-int 2) 1)))
          code-2-new (Math/abs (+ code-2 (- (rand-int 2) 1)))]
      (->Individual (->Chromosome code-1-new)
                    (->Chromosome code-2-new)))))

(defn apply-mutation [population probability]
  (map #(mutate-individual % probability) population))

(defn next-pop [pop ff x-min x-max chromosome-size co-probability mutation-probability]
  (-> pop
      (apply-selection chromosome-size x-min x-max ff)
      (apply-crossing-over co-probability)
      (apply-mutation mutation-probability)))
