(defn constant [value] (fn [variables] value))
(defn variable [name] (fn [variables] (get variables name)))
(defn negate [expr] (fn [variables] (- (expr variables))))
(defn operator [op] (fn [& args] (fn [variables] (apply op (mapv (fn [x] (x variables)) args)))))
(def add (operator +))
(def subtract (operator -))
(def multiply (operator *))
(def divide (operator (fn [x & args] (if (== 0 (count args)) (/ 1.0 (double x)) (/ (double x) (double (apply * args)))))))
(defn meansqnum [& args] (/ (apply + (mapv (fn [x] (* x x)) args)) (count args)))
(def meansq (operator meansqnum))
(defn rmsnum [& args] (Math/sqrt (apply meansqnum args)))
(def rms (operator rmsnum))

(def operators {'negate negate '+ add '- subtract '* multiply '/ divide 'meansq meansq 'rms rms})

(defn parseFunction [expr] (
                             letfn [(parse [expression] (cond
                                                          (number? expression) (constant expression)
                                                          (symbol? expression) (variable (name expression))
                                                          :else (apply (get operators (first expression)) (mapv parse (rest expression)))
                                                          ))]
                                   (parse (read-string expr))
                             ))