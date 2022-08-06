(ns dumch.concatenative
  "Stack based interpreter implementation
  
  having `State` with 
  `program` — list of forms;
  `stack` — typical stack machine;
  `env` — varibles scope;

  computes result by iterating through `program`.

  Entry points:
  - use DSL with `defstackfn` macro;
  - manually create `State` and pass to `interpreter` function.

  See docs and project README for details."

  (:require
    [clojure.string :as str]))

(defrecord State [program stack env])

(defprotocol IEval (evaluate [this state]))

(defn interpreter
  "Get `State`, returns new `State` or terminates with an exception. 

  Each expression from `State:program` gets evaluated;
  Varibales stored in `State:env`;
  Get evaluation result from `State:stack`"
  [{[h & tail] :program :as state}]
  (if (or (some? h) (some? tail)) 
    (recur 
      (try 
        (evaluate h (assoc state :program tail))
        (catch Exception e 
          (throw (ex-info "eval error" 
                          {:cause (ex-data e) :state state :eval h}
                          e)))))
    state))

(defn- state->result [{stack :stack}]
  (peek stack))

(defmacro defstackfn 
  "Like `defn` with DSL body. See README for the details"
  [f-name args & tail]
  `(defn ~f-name ~args
     (#'state->result 
       (#'interpreter (->State '~tail [] (zipmap '~args ~args))))))

(defn- vsplit-at [idx v]
  [(subvec v 0 idx) (subvec v idx)])

(defn- invoke [[f cnt] {:keys [stack] :as state}]
  (let [[stack* args] (vsplit-at (- (count stack) cnt) stack)
        args (reverse args)]
    (try 
      (assoc state :stack (conj stack* (apply (eval f) args)))
      (catch Exception e 
        (throw (ex-info "funciton application error" 
                        {:error-call {:fn f :args args}} 
                        e))))))

;; uses interpreter inside as a way to have local scope vars
(defn- interpret-if [tail {:keys [program stack env] :as state}] 
  (let [[if-body [_ & el-body]] (split-with (partial not= 'else>) tail)
        body (if (boolean (peek stack)) if-body el-body)
        result (-> (State. body (pop stack) env)
                   interpreter
                   state->result)]
    (-> (assoc state :program (cons result program))
        (update :stack pop))))

(extend-protocol IEval
  java.lang.Object
  (evaluate [this state]
    (update state :stack conj this))

  nil
  (evaluate [this state]
    (update state :stack conj this))

  clojure.lang.Symbol
  (evaluate [this {:keys [env stack] :as state}]
    (let [^String _name (name this)]
      (cond 
        (= this '<pop>)
        (update state :stack pop)

        (and (str/starts-with? _name "!") (str/ends-with? _name "+"))
        (assoc-in state
                  [:env (symbol (subs _name 0 (dec (.length _name))))]
                  (peek stack))

        (str/starts-with? _name "!")
        (if-let [value (get env this)]
          (update state :stack conj value)
          (throw (ex-info "undeclared var" {:data this})))

        :else (throw (ex-info "simpbols should start with '!'" {:data this})))))

  clojure.lang.ISeq
  (evaluate [[h & tail] state]
    (case h
      invoke> (invoke tail state)
      if> (interpret-if tail state))))

(comment
  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)

  (macroexpand '(defstackfn f [!a !b] !a !b (invoke> + 2)))

  (defstackfn f 
    [!a !b !c]               ; !a=1 !b=2 !c=4
    !a                       ; 1
    !b                       ; 1 2
    (invoke> + 2)            ; 3
    !v1+                     ; 3 
    !c                       ; 3 4 
    !c                       ; 3 4 4
    <pop>                    ; 3 4
    2                        ; 3 4 2
    (invoke> * 2)            ; 3 8
    !v2+                     ; 3 8
    (invoke> = 2)            ; false
    (if>                     ; empty           
         !v1
         0                   ; # divide by zero, but...
         !v2
         (invoke> / 2)       ; # this 'if-branch' will not be evaluated
         else>               ; # next will be ->

         "false!!"           ; "false!!" 
         (invoke> println 1) ; nil
         <pop>               ; emtpy
         !v1                 ; 3
         !v2                 ; 8
         !a+                 ; # !a is set to 24 in this local scope
         (invoke> * 2))      ; 24
    !a                       ; 24 1
    (invoke> str 2))         ; "124"

  (f 1 2 4)

  (defstackfn f2 [] 0 1 (invoke> / 2)) ;; divide by zero
  (f2)
  (ex-data *e)
  (ex-cause *e)

  (defstackfn f3 [] <pop>) ;; pop from empty
  (f3)
  (ex-data *e)
,)
