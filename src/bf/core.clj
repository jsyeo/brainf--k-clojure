(ns bf.core)
(defn exit [] (System/exit 0))

(defn read-lines []
  (doall (line-seq (clojure.java.io/reader *in*))))

(defn parse-bf-input [input]
  (doall (butlast input)))

(declare bf-eval)

(defn inc-mem-at-ptr [mem ptr]
  (let [val (nth mem ptr)]
    (assoc mem ptr (mod (+ val 1) 256))))

(defn dec-mem-at-ptr [mem ptr]
  (let [val (nth mem ptr)]
    (assoc mem ptr (mod (- val 1) 256))))

(defn bf-inc [code code-ptr input mem mem-ptr count]
  #(bf-eval code (inc code-ptr) input (conj mem 0) (inc mem-ptr) count))

(defn bf-dec [code code-ptr input mem mem-ptr count]
  #(bf-eval code (inc code-ptr) input mem (dec mem-ptr) count))

(defn bf-inc-byte [code code-ptr input mem mem-ptr count]
  #(bf-eval code (inc code-ptr) input (inc-mem-at-ptr mem mem-ptr) mem-ptr count))

(defn bf-dec-byte [code code-ptr input mem mem-ptr count]
  #(bf-eval code (inc code-ptr) input (dec-mem-at-ptr mem mem-ptr) mem-ptr count))

(defn bf-print [code code-ptr input mem mem-ptr count]
  (do
    (print (char (nth mem mem-ptr)))
    #(bf-eval code (inc code-ptr) input mem mem-ptr count)))

(defn read-into-mem [input mem mem-ptr]
  (assoc mem mem-ptr (int input)))


(defn bf-read [code code-ptr input mem mem-ptr count]
  #(bf-eval code (inc code-ptr) (rest input) (read-into-mem (first input) mem mem-ptr) mem-ptr count))

(defn move-to-loop-stop [code-ptr code]
  (loop [cur-ptr code-ptr
         inner-brack 0]
    (let [cur-instr (nth code cur-ptr)
          next-ptr (inc cur-ptr)]
      (cond
       (= cur-instr \]) (if (= inner-brack 1)
                          cur-ptr
                          (recur next-ptr (dec inner-brack)))
       (= cur-instr \[) (recur next-ptr (inc inner-brack))
       :else            (recur next-ptr inner-brack)))))

(defn bf-start-loop [code code-ptr input mem mem-ptr count]
  (let [loop-index (nth mem mem-ptr)]
    (if (= 0 loop-index)
      #(bf-eval code (move-to-loop-stop code-ptr code) input mem mem-ptr count)
      #(bf-eval code (inc code-ptr) input mem mem-ptr count))))

(defn move-to-loop-start [code-ptr code]
  (loop [cur-ptr code-ptr
         inner-brack 0]
    (let [cur-instr (nth code cur-ptr)
          next-ptr (dec cur-ptr)]
      (cond
       (= cur-instr \[) (if (= inner-brack 1)
                          cur-ptr
                          (recur next-ptr (dec inner-brack)))
       (= cur-instr \]) (recur next-ptr (inc inner-brack))
       :else            (recur next-ptr inner-brack)))))

(defn bf-stop-loop [code code-ptr input mem mem-ptr count]
  (let [val (nth mem mem-ptr)]
    (if (= val 0)
      #(bf-eval code (inc code-ptr) input mem mem-ptr count)
      #(bf-eval code (move-to-loop-start code-ptr code) input mem mem-ptr count))))

(defn bf-eval [code code-ptr input mem mem-ptr instr-count]
  (if (> instr-count 100000)
    (do (println "\nPROCESS TIME OUT. KILLED!!!") (exit))
    (if (< code-ptr (count code))
      (let [bf->fun {\> bf-inc
                     \< bf-dec
                     \+ bf-inc-byte
                     \- bf-dec-byte
                     \. bf-print
                     \, bf-read
                     \[ bf-start-loop
                     \] bf-stop-loop}
            fun (bf->fun (nth code code-ptr))]
        (if fun
          (do
            #(fun code code-ptr input mem mem-ptr (inc instr-count)))
          (recur code (inc code-ptr) input mem mem-ptr instr-count))))))

(defn -main [& args]
  (let [input (read-lines)
        bf-input (parse-bf-input (nth input 1))
        bf-code (apply str (drop 2 input))]
                                        ;    (println bf-input)
                                        ;    (print bf-code)
    (trampoline bf-eval bf-code 0 bf-input [0] 0 0)))
