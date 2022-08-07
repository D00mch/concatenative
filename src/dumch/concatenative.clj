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

  See docs and project README for the details."

  {:author "Artur Dumchev"}
  (:require
    #_[criterium.core :refer [quick-bench]]
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

(defmacro defstackfn
  "Like `defn` with DSL body. See README for the details"
  [f-name args & tail]
  `(defn ~f-name ~args
     (peek
       (:stack
         (#'interpreter (->State '~tail [] (zipmap '~args ~args)))))))

(defn- invoke [[f cnt] {:keys [stack] :as state}]
  (let [stack* (subvec stack 0 (- (count stack) cnt))
        args (reverse (take-last cnt stack))]
    (try
      (assoc state :stack (conj stack* (apply (eval f) ; eval to get actual function 
                                              args)))  ; and not a symbol
      (catch Exception e
        (throw (ex-info "funciton application error"
                        {:error-call {:fn f :args args}}
                        e))))))

;; uses `interpreter` inside as a way to have local scope vars
(defn- interpret-if [tail {:keys [stack env] :as state}]
  (let [[if-body [_ & el-body]] (split-with (partial not= 'else>) tail)
        body (if (boolean (peek stack)) if-body el-body)
        result (interpreter (State. body (pop stack) env))]
    (assoc state :stack (:stack result))))

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
        (if-some [value (get env this)]
          (update state :stack conj value)
          (throw (ex-info "undeclared var" {:data this})))

        :else (throw (ex-info "symbols should start with '!'" {:data this})))))

  clojure.lang.ISeq
  (evaluate [[h & tail] state]
    (case h
      invoke> (invoke tail state)
      if> (interpret-if tail state))))

(comment
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
         else>               ; # next will be ->

         "false!!"           ; "false!!"
         (invoke> println 1) ; nil
         <pop>               ; emtpy
         !v1                 ; 3
         !v2                 ; 8
         !a+                 ; # !a is set to 24 in this local scope
         (invoke> * 2))      ; 24

    !a                       ; 24 1
    (invoke> str 2))         ; "124" # and not 2424, as !a from `else` is local



  ; (defstackfn f2 [] 0 1 (invoke> / 2)) ;; divide by zero
  ; (f2)
  (ex-data *e)
  (ex-cause *e)
,)
