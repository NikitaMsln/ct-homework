(def BUFFER_MAX_SIZE 1000)

(defn change [buffer now_pos input input_pos f] (vector (update-in buffer (vector now_pos) f) now_pos input input_pos))
(defn incr [buffer now_pos input input_pos] {:pre [(< (get buffer now_pos) 255)]} (change buffer now_pos input input_pos inc))
(defn decr [buffer now_pos input input_pos] {:pre [(> (get buffer now_pos) 0)]} (change buffer now_pos input input_pos dec))
(defn next_field [buffer now_pos input input_pos]
      {:pre [(< now_pos BUFFER_MAX_SIZE)]}
      (vector
        ; :NOTE: - cond вместо if
        (cond
          (= (count buffer) (inc now_pos)) (conj buffer 0)
          :else buffer)
        (inc now_pos)
        input input_pos))
(defn last_field [buffer now_pos input input_pos]
      {:pre [(or (< (count buffer) BUFFER_MAX_SIZE) (> now_pos 0))]}
      (cond
        (= 0 now_pos) (vector (apply conj (vector 0) buffer) now_pos input input_pos)
        :else (vector buffer (dec now_pos) input input_pos)))
(defn readin [buffer now_pos input input_pos]
      {:pre [(> (count input) input_pos)]}
      (change buffer now_pos input (inc input_pos) (constantly (int (get input input_pos)))))
(defn writeout [buffer now_pos input input_pos] (print (char (get buffer now_pos))) (vector buffer now_pos input input_pos))

(def commands {"1" incr "000" decr "010" next_field "011" last_field "0010110" readin "001010" writeout})
(def start_loop "00100")
(def start_loop_code 1)
(def end_loop "0011")
(def end_loop_code 0)

(defn compile_spoon [program pos buffer]
      {:pre [(or (> (count program) pos) (= buffer start_loop) (= buffer end_loop) (contains? commands buffer) (= buffer ""))]}

      (cond
        (and (= buffer "") (<= (count program) pos)) []
        (= buffer start_loop) (apply conj (vector start_loop_code) (compile_spoon program pos ""))
        (= buffer end_loop) (apply conj (vector end_loop_code) (compile_spoon program pos ""))
        (contains? commands buffer) (apply conj (vector (get commands buffer)) (compile_spoon program pos ""))
        :else (compile_spoon program (inc pos) (str buffer (get program pos)))))

; :NOTE: * Переполнение стека
(defn run_spoon [prog instr_ptr data loop_stack]
      (cond
        (>= instr_ptr (count prog)) 0
        (= start_loop_code (get prog instr_ptr)) (run_spoon prog (inc instr_ptr) data (conj loop_stack (inc instr_ptr)))
        (= end_loop_code (get prog instr_ptr)) (if
                                                 (= 0 (get (get data 0) (get data 1)))
                                                 (run_spoon prog (inc instr_ptr) data (pop loop_stack))
                                                 (run_spoon prog (peek loop_stack) data loop_stack))
        :else (run_spoon prog (inc instr_ptr) (apply (get prog instr_ptr) data) loop_stack)))

; :NOTE: # Не поддерживаются лимиты по числу операций и памяти
(defn runProgram [program input]
      (run_spoon (compile_spoon program 0 "") 0 (vector [0] 0 input 0) []))
