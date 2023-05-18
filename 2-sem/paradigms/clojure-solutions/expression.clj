(defn constant [value] (constantly value))
(defn variable [name] (fn [variables] (get variables name)))
(defn negate [expr] (fn [variables] (- (expr variables))))
(defn operator [op] (fn [& args] (fn [variables] (apply op (mapv (fn [x] (x variables)) args)))))

(def add (operator +))
(def subtract (operator -))
(def multiply (operator *))

(defn div [x & args] (if (== 0 (count args))
                       (/ 1.0 (double x))
                       (/ (double x) (double (apply * args)))))
(def divide (operator div))

(defn meansqnum [& args] (/ (apply + (mapv (fn [x] (* x x)) args)) (count args)))
(def meansq (operator meansqnum))
(defn rmsnum [& args] (Math/sqrt (apply meansqnum args)))
(def rms (operator rmsnum))

(def functionOperators {'negate negate '+ add '- subtract '* multiply '/ divide 'meansq meansq 'rms rms})
(defn getParser [operators const variab]
  (fn [expr] (letfn [(parse [expression]
                       (cond
                         (number? expression) (const expression)
                         (symbol? expression) (variab (name expression))
                         ; :NOTE: type check
                         :else (apply (get operators (first expression)) (mapv parse (rest expression)))
                         ))]
               (parse (read-string expr)))))
(def parseFunction (getParser functionOperators constant variable))

(definterface Expression (evaluate [variables]) (diff [variable]))
(defn toString [object] (.toString object))
(defn evaluate [expression variables] (.evaluate expression variables))
(defn diff [expression variable] (.diff expression variable))
(deftype ConstantConstructor [number]
         Expression
         (evaluate [this variables] number)
         (diff [this variable] (ConstantConstructor. 0))
         Object
         (toString [this] (str number)))
(defn Constant [number] (ConstantConstructor. number))

(deftype VariableConstructor [name]
         Expression
         (evaluate [this variables] (get variables name))
         (diff [this variable] (if (= variable name) (Constant 1) (Constant 0)))
         Object
         (toString [this] name))
(defn Variable [name] (VariableConstructor. name))

(deftype OperatorConstructor [name evaluator d args]
         Expression
         (evaluate [this variables] (apply evaluator (mapv (fn [element] (evaluate element variables)) args)))
         (diff [this variable] (apply d variable args))
         Object
         (toString [this] (str "(" name " " (clojure.string/join " " (mapv toString args)) ")")))
(defn Operator [name evaluator d] (fn [& args] (OperatorConstructor. name evaluator d args)))

(def Add (Operator "+" + (fn [variable & args] (apply Add (mapv (fn [element] (diff element variable)) args)))))
(def Subtract (Operator "-" - (fn [variable & args] (apply Subtract (mapv (fn [element] (diff element variable)) args)))))
(def Multiply (Operator "*" *
                        (fn [variable & args]
                          (apply Add (mapv (fn [x] (apply Multiply (mapv (fn [y] (if (= x y) (diff y variable) y)) args))) args)))))
(def Negate (Operator "negate" - (fn [variable & args] (Negate (diff (first args) variable)))))
(def Divide (Operator "/" div
                      (fn [variable & args]
                        (if
                          (= (count args) 1)
                          (Divide (Negate (diff (first args) variable))
                                  (first args) (first args))
                          (apply Divide (Subtract (apply Multiply (diff (first args) variable) (rest args)) (Multiply (first args) (diff (apply Multiply (rest args)) variable)))
                                 (into [] (concat (rest args) (rest args))))))))
(defn sumexp [& args] (apply + (mapv #(Math/exp %) args)))
(def Sumexp (Operator "sumexp" sumexp
                      (fn [variable & args]
                        (apply Add (mapv (fn [x] (Multiply (diff x variable) (Sumexp x))) args)))))
(def LSE (Operator "lse" (fn [& args] (Math/log (apply sumexp args)))
                   (fn [variable & args] (let [value (apply Sumexp args)]
                                           (Divide (diff value variable) value)))))

(def objectOperators {'negate Negate '+ Add '- Subtract '* Multiply '/ Divide 'sumexp Sumexp 'lse LSE})
(def parseObject (getParser objectOperators Constant Variable))


