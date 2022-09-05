(ns dumch.analyze
  (:require [clojure.core.specs.alpha :as specs]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [dumch.vars :as vars]))

(set! *warn-on-reflection* true)

(deftype Arity [params var-params body])
(deftype Defn [arities env fn-name])

(deftype Continuation [k])

(defprotocol IAnalyze
  (analyze [sepx]))

(defn- analyze-self-evaluating [sexp]
  (fn [_ ds k]
    #(k (conj ds sexp))))

(defn analyze-quoted [[op :as sexp]]
  (analyze-self-evaluating (case op
                             quote> (next sexp)
                             quote (second sexp))))

(defn- analyze-sequence [sq]
  (let [sequentially 
        (fn [f1 f2]
          (fn [env ds k]
            (trampoline f1
                        env
                        ds
                        (fn [ds2]
                          #_(println :ds ds)
                          #(f2 env ds2 k)))))
        [f & fs] (map analyze sq)]
    (if (nil? f) 
      (fn [_ ds k] #(k ds))
      (loop [f f
             fs fs]
        (if (seq fs)
          (recur (sequentially f (nth fs 0))
                 (next fs))
          f)))))

(defn- analyze-if [sexp]
  (let [[[_ & conseq] [_ & alt]] (split-with #(not= % 'else>) sexp)
        conseq-fn (analyze-sequence conseq)
        alt-fn (analyze-sequence alt)]
    (fn [env ds k]
      (let [pred (peek ds)
            stack* (pop ds)
            env* (vars/extend-env env)
            r (if (or (false? pred) (nil? pred))
                #(alt-fn env* stack* k)
                #(conseq-fn env* stack* k))]
        r))))

(defn- analyze-call [_]
  (fn [env ds k]
    (let [sq (peek ds)
          ds* (pop ds)
          env* (vars/extend-env env)]
      #((analyze sq) env* ds* k))))

(defn- analyze-call-cc [sexp]
  (let [sq-fn (analyze-sequence (next sexp))]
    (fn [env ds k]
      (sq-fn env
             (conj ds (Continuation. k))
             (fn [ds*]
               #(k ds*))))))

(defn- analyze-assignment [sexp]
  (fn [env ds k] 
    (let [value (peek ds)]
      (vars/set-var! env sexp value)
      #(k ds))))

(defn- analyze-def [sexp]
  (let [^String _name (name sexp)
        sym (symbol (subs _name 0 (dec (.length _name))))]
    (fn [env st k] 
      (let [value (peek st)]
        (vars/def-var! env sym value)
        #(k st)))))

(defn- analyze-lookup [sym]
  (fn [env stack k]
    (let [result (vars/lookup-var env sym)]
      #(k (conj stack result)))))

(defn- map-params [{{:keys [params var-params]} :params body :body}]
  (Arity. (mapv second params)
          (-> var-params :var-form second)
          (analyze-sequence (seq (second body)))))

(defn- match-defn-arity [^Defn proc args]
  (letfn [(better-match? [^Arity arity best]
            (and (.var-params arity)
                 (>= (count args) (count (.params arity)))
                 (> (count (.params arity))
                    (if best (count (.params ^Arity best)) 0))))
          (exact-match? [{:keys [params var-params]}]
            (and (= (count params) (count args)) (nil? var-params)))]
    (loop [[arity & tail] (.arities proc)
           best nil]
      (cond (nil? arity) best
            (exact-match? arity) arity
            (better-match? arity best) (recur tail arity)
            :else (recur tail best)))))

(defn- analyze-defn [[_ & f]]
  (let [{fn-name :fn-name [arity body] :fn-tail}
        (s/conform ::specs/defn-args f)

        arities 
        (cond (= arity :arity-1) [(map-params body)]
              (= arity :arity-n) (mapv map-params (:bodies body)))]
    (fn [env stack k]
      (vars/def-var!
        env
        fn-name
        (Defn. arities env fn-name))
      #(k stack))))

(defn- zip-params-args [^Arity arity args]
  (let [pcount (count (.params arity))
        varargs (seq (drop pcount args))
        params* (cond-> (.params arity) 
                  varargs (conj (.var-params arity)))
        args* (cond-> (vec (take pcount args))  varargs (conj varargs))]
    (zipmap params* args*)))

(defn- execute-applicaiton [proc args ds k]
  (cond (vars/primitive? proc) 
        (k (conj ds (apply proc args)))

        (instance? Continuation proc)
        ((.k ^Continuation proc) ds)

        (instance? Defn proc)
        (let [^Defn proc proc
              ^Arity arity (match-defn-arity proc args)
              env (vars/extend-env (.env proc) 
                                   (zip-params-args arity args))]
          ((.body arity)
           env
           ds
           k))
        :else (throw (ex-info  "Unknown procedure type: "  
                              {:proc proc :args args}))))

(defn- analyze-application [[_ op args-count]] ;; (invoke> + 2)
  (let [op-fn (analyze op)]
    (fn [env ds k]
      (let [args-count (if (number? args-count) 
                         args-count
                         (vars/lookup-var env args-count))
            args (take-last args-count ds)
            ds* (subvec ds 0 (- (count ds) args-count))]
        (op-fn env 
               ds 
               (fn [f]
                 #(execute-applicaiton
                    (peek f)
                    args
                    ds*
                    k)))))))

(defn- def? [^String sym]
  (and (str/starts-with? sym "!") (str/ends-with? sym "+")))

(defn- swap [v] 
  (let [c (count v)
        i1 (dec c)
        i2 (dec i1)]
    (assoc v i2 (v i1) i1 (v i2))))

(extend-protocol IAnalyze
  nil (analyze [s] (analyze-self-evaluating s))
  java.lang.Boolean (analyze [s] (analyze-self-evaluating s))
  java.lang.Long (analyze [s] (analyze-self-evaluating s))
  java.lang.Character (analyze [s] (analyze-self-evaluating s))
  java.lang.String (analyze [s] (analyze-self-evaluating s))
  clojure.lang.IPersistentVector (analyze [s] (analyze-self-evaluating s))

  clojure.lang.Symbol
  (analyze [sym]
    (let [^String _name (name sym)]
      (cond (= sym '<pop>) (fn [_ ds k] #(k (pop ds)))
            (= sym '<dup>) (fn [_ ds k] #(k (conj ds (last ds)))) 
            (= sym '<swap>) (fn [_ ds k] #(k (swap ds)))
            (= sym '<call>) (analyze-call sym)
            (def? _name) (analyze-def sym)
            :else (analyze-lookup sym)))) 

  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (case op
      set! (analyze-assignment sexp) 
      def (analyze-def sexp)
      defn> (analyze-defn sexp)
      call/cc> (analyze-call-cc sexp)
      if> (analyze-if sexp)
      quote> (analyze-quoted sexp) 
      quote (analyze-quoted sexp)
      invoke> (analyze-application sexp)
      (analyze-sequence sexp))))

(comment 
  
  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (fs-api/stop)

  )
