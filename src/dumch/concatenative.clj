(ns dumch.concatenative
  (:require
    [clojure.string :as str]))

(defn vsplit-at [idx v]
  [(subvec v 0 idx) (subvec v idx)])

(defn sbutlast [symb]
  (let [s (name symb)]
    (symbol (subs s 0 (dec (count s))))))

(defn stack-walk [vars sq]
  (loop [env vars
         [head & tail] sq
         stack []]
    #_(println :stack stack \newline :seq (cons head tail))
    (cond (nil? head) stack

          (= head '<pop>)
          (recur env tail (pop stack))

          (and (symbol? head) 
               (-> head name count (> 2))
               (-> head name last (str/ends-with? "+")))
          (recur (assoc env (sbutlast head) (peek stack)) tail stack) 

          (and (seq? head) (= (first head) 'invoke>)) 
          (let [[_ fun cnt] head
                [stack args] (vsplit-at (- (count stack) cnt) stack)
                args (map (fn [a] (if (symbol? a) (get env a) a)) args)]
            (recur env tail (conj stack (apply (eval fun) args))))

          (and (seq? head) (= (first head) 'if>))
          (let [[[_ & if-body] [_ & el-body]] (split-with (partial not= 'else>) head)
                body (if (boolean (peek stack)) if-body el-body)]
            (recur env
                   (concat body tail)
                   (pop stack)))

          :else (recur env tail (conj stack (get env head head))))))

(defmacro defstackfn [f-name args & tail]
  `(defn ~f-name ~args
     (stack-walk (zipmap '~args ~args) '~tail)))

(comment 
  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)

  (macroexpand '(defstackfn f [!a !b] !a !b (invoke> + 2)))


  (defstackfn f 
    [!a !b !c]            ; !a=1 !b=2 !c=4
    !a                    ; 1
    !b                    ; 1 2
    (invoke> + 2)         ; 3
    !v1+                  ; 3 
    !c                    ; 3 4 
    !c                    ; 3 4 4
    <pop>                 ; 3 4
    2                     ; 3 4 2
    (invoke> * 2)         ; 3 8
    !v2+                  ; 3 8
    (invoke> = 2)         ; false
    (if>                  ; empty           
     !v1
     !v2
     (invoke> - 2)
     else>
     "false!!"           ; "false!!" 
     (invoke> println 1) ; nil
     <pop>               ; emtpy
     !v1                 ; 3
     !v2                 ; 8
     (invoke> * 2)       ; 24
     ))

  (f 1 2 4)
  ,)
