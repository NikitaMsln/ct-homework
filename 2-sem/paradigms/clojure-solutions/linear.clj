(defn equalSize? [args] (every? (fn [x] (every? (fn [y] (or (number? x) (number? y) (== (count x) (count y)) )) args)) args))
(defn isVector? [v] (and (vector? v) (every? number? v)))
(defn isEqualSizeVectors? [args] (and (every? isVector? args) (equalSize? args)))
(defn isMatrix? [m] (and (vector? m) (isEqualSizeVectors? m)))
(defn isEqualSizeMatrixes? [args] (and (every? isMatrix? args) (equalSize? args)))
(defn isEqualSizeTensors? [args]
      (letfn
        [(func [x, y]
               (or (number? x)
                   (number? y)
                   (and (== (count x) (count y)) (every? (fn [x] x) (mapv func x y)))
                   )
               )]
        (every? (fn [x] (every? (fn [y] (func x y)) args)) args) )
      )

(defn operator [pre? post? op]
      (letfn [
              (calc [& args]
                    {:pre [(or (every? number? args) (equalSize? args))]}
                    (if (every? number? args) (apply op args) (apply mapv calc (mapv (fn [x] (if (number? x) (vec (repeat ((fn [values] (apply max (mapv (fn [y] (if (vector? y) (count y) 0)) values))) args) x)) x)) args)))
                    )
              ]
             (fn [& args] {:pre [(pre? args)] :post [(post? %)]} (apply calc args))
             )
      )


(defn vectorOperator [f] (operator isEqualSizeVectors? isVector? f))
(def v+ (vectorOperator +))
(def v- (vectorOperator -))
(def v* (vectorOperator *))
(def vd (vectorOperator /))
(defn scalar [& args] {:pre [(isEqualSizeVectors? args)] :post [(number? %)]} (apply + (apply v* args)))

(defn v*s [a & s] {:pre [(isVector? a) (every? number? s)] :post [(isVector? %)]} (let [val (apply * s)] (mapv (partial * val) a)))

(defn matrixOperator [f] (operator isEqualSizeMatrixes? isMatrix? f))
(def m+ (matrixOperator +))
(def m- (matrixOperator -))
(def m* (matrixOperator *))
(def md (matrixOperator /))
(defn m*s [m & s] {:pre [(isMatrix? m) (every? number? s)] :post [(isMatrix? %)]}
      (let [val (apply * s)] (mapv #(v*s %1 val) m)))
(defn m*v [m v] {:pre [(isVector? v) (isMatrix? m) (= (count (first m)) (count v))] :post [(isVector? %) (= (count m) (count %))]}
      (mapv #(apply + (v* % v)) m))
(defn transpose [m]
      {:pre [(isMatrix? m)] :post [(isMatrix? %)]}
      (apply mapv vector m))
(defn m*m [& args] {:pre [(every? isMatrix? args)] :post [(isMatrix? %)]}  (reduce (fn [a b] {:pre [(= (count (first a)) (count b))]}
                                                                 (mapv (partial m*v (transpose b)) a)) args))

(defn vect [& args]
      {:pre [(isEqualSizeVectors? args)]}
      (reduce (fn [a b]
                  (let [[x y z] a
                        m [[0 (- z) y]
                           [z 0 (- x)]
                           [(- y) x 0]]]
                       (m*v m b))) args))

(defn tensorOperator [f] (operator isEqualSizeTensors? (fn [t] (or (number? t) (vector? t))) f))
(def tb+ (tensorOperator +))
(def tb- (tensorOperator -))
(def tb* (tensorOperator *))
(def tbd (tensorOperator /))

