

;;cd D:\fp\clojure-1.6.0
;;D:
;;java -cp clojure-1.6.0.jar clojure.main lab1.clj resources/butterfly.txt Euclidian

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(def radiusA 3)
(def radiusB (* radiusA 1.5))
(def lowerThreshold 0.15)
(def upperThreshold 0.5)

(def args *command-line-args*)
(if (.exists (io/file (first args))) (def file (slurp (first args))) (do (println "Can't find file")(System/exit 0)))
(def distanceType (last args))
(def lines (str/split-lines file))
(defn hamming [param1 param2]
   (count (filter false? (map = param1 param2))))

(defn euclid
  [param1 param2]
  (Math/sqrt (reduce + (map #(Math/pow % 2) (map - param1 param2)))))

(def distance (if (= distanceType "Euclidian") euclid))
(if (nil? distance)(def distance (if (= distanceType "Hamming") hamming)))



;(println distance)

(defn parse-float [str]
  (let [n (read-string str)]
       (if (number? n) n 0)))

(def removeWhiteSpace (fn[el](str/replace el " ", "")))
(def splitElement (fn[el](map parse-float (str/split (removeWhiteSpace el) #","))))
(def removeLastElement (fn[el](take (- (count el) 1) el)))

(def sourceData (map splitElement lines))
(def sourceData(map removeLastElement sourceData))

(defn createDistancePoint  [point dist]  (assoc {:coordinates (into [] point)} :distance dist))

(defn potential
  [distance]
  (Math/exp (- (* (/ 4 (* radiusA radiusA)) distance))))


(defn calculatePotential [item items]
  (reduce + (map #(potential %1) (map #(distance item %1)  items))))

(defn getPotentialsVector [elements]
  (map #(createDistancePoint %1 (calculatePotential %1 elements)) elements))

(def potentials (sort-by :distance (getPotentialsVector sourceData)))
;(println potentials)

(def potentials (sort-by :distance (getPotentialsVector sourceData)))

(defn revisedPotential
  [distance]
  (Math/exp (- (* (/ 4 (* radiusB radiusB)) distance))))

(defn recalculateCurrentPotential [el, kernel]
  (assoc el :distance (- (:distance el) (* (:distance kernel) (revisedPotential (distance (:coordinates el) (:coordinates kernel)))))))

(defn recalculatePotentials [potentials kernel]
  (map #(recalculateCurrentPotential %1 kernel) potentials))

(defn calculateMinDistance
  [point points]
  (->> (map #(distance (:coordinates point) (:coordinates %1)) points)(apply min)))

(let [firstPotential (last potentials)]
  (loop [kernels [firstPotential] elements (drop-last potentials)]
      (let [recalculatedPotentials (sort-by :distance (recalculatePotentials elements (first kernels)))]
       (let [newPotential (last recalculatedPotentials)]
         (cond
          (> (:distance newPotential) (* upperThreshold (:distance firstPotential))) (recur (cons newPotential kernels) (drop-last recalculatedPotentials))
          (< (:distance newPotential) (* lowerThreshold (:distance firstPotential))) (def result (sort-by :distance kernels))
          (>= (+ (/ (calculateMinDistance newPotential kernels) radiusA) (/ (:distance newPotential) (:distance firstPotential))) 1) (recur (cons newPotential kernels) (drop-last recalculatedPotentials))
          :else (recur kernels (cons (assoc newPotential :dist 0) (drop-last recalculatedPotentials)))
          )
         )
       )
     )

    (println result)
  )



;(if (=
;     (hamming [1 3] [1 5])
;     ) (println "Test 1 passed succesfully") )


;(if (=
;     (euclid [1 3] [5 2])
;     4.123105625617661) (println "Test 2 passed succesfully") )


;(if (=
;     (calculatePotential [5 3] [[2 2] [1 8] [6 6]])
;     1.2333368715215622) (println "Test 3 passed succesfully") )


