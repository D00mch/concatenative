(ns dumch.analyze
  (:require [clojure.core.specs.alpha :as specs]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [dumch.vars :as vars]))

(defrecord Arity [params var-params body])
(defrecord Defn [arities env fn-name])

(deftype JavaCall [sym])
(deftype Continuation [k])

(defprotocol IAnalyze
  (analyze [sepx]))

(defn- analyze-self-evaluating [exp]
  (fn [_ ds k]
    #(k (conj ds exp))))

(defn analyze-quoted [[op :as exp]]
  (analyze-self-evaluating (case op
                             quote> (next exp)
                             quote (second exp))))

(defn- analyze-sequence [sq]
  (let [sequentially 
        (fn [f1 f2]
          (fn [env ds k]
            (fn [] ;; wrap for a trampoline
              (f1
                env
                ds
                (fn [ds2]
                  #_(println :ds ds2)
                  (f2 env ds2 k))))))
        [f & fs] (map analyze sq)]
    (if (nil? f) 
      (fn [_ ds k] #(k ds))
      (loop [f f
             fs fs]
        (if (seq fs)
          (recur (sequentially f (nth fs 0))
                 (next fs))
          f)))))

(defn- analyze-if [exp]
  (let [[[_ & conseq] [_ & alt]] (split-with #(not= % 'else>) exp)
        conseq-fn (analyze conseq)
        alt-fn (analyze alt)]
    (fn [env ds k]
      (let [pred (peek ds)
            ds* (pop ds)
            env* (vars/extend-env env)
            r (if (or (false? pred) (nil? pred))
                #(alt-fn env* ds* k)
                #(conseq-fn env* ds* k))]
        r))))

(defn analyze-when [exp]
  (let [conseq-fn (analyze (rest exp))]
    (fn [env ds k]
      (let [pred (peek ds)
            ds* (pop ds)
            env* (vars/extend-env env)
            r (if (or (false? pred) (nil? pred))
                #(k (pop ds))
                #(conseq-fn env* ds* k))]
        r))))

(defn- analyze-call [_]
  (fn [env ds k]
    (let [sq (peek ds)
          ds* (pop ds)
          env* (vars/extend-env env)]
      #((analyze sq) env* ds* k))))

(defn- analyze-call-cc [exp]
  (let [sq-fn (analyze (rest exp))]
    (fn [env ds k]
      (sq-fn env
             (conj ds (Continuation. k))
             (fn [ds*]
               #(k ds*))))))

(defn- generate-call-cc [sym]
  (let [cc-function (analyze (list 'invoke> sym 0))]
    (fn [env ds k] 
      (cc-function env ds (fn [cc]
                            #(k (conj ds cc)))))))

(defn- loop-vals [body-fn [v & tail] env ds k]
  (if v
    #(let [k2 (fn [ds2]
                (loop-vals body-fn tail env ds2 k))]
       (vars/def-var! env '<continue_> (Continuation. k2))
       (body-fn env (conj ds v) k2))
    #(k ds)))

(defn- loop-count [body-fn cnt env ds k]
  (if (> cnt 0)
    #(let [k2 (fn [ds2]
                (loop-count body-fn (dec cnt) env ds2 k))]
       (vars/def-var! env '<continue_> (Continuation. k2))
       (body-fn env ds k2))
    #(k ds)))

(defn- analyze-loop [exp loop-fn]
  (let [for-fn (analyze (rest exp))]
    (fn [env ds k]
      (vars/def-var! env '<break_> (Continuation. k))
      #(loop-fn for-fn (peek ds) env (pop ds) k))))

(defn- analyze-assignment [exp]
  (fn [env ds k] 
    (let [value (peek ds)]
      (vars/set-var! env exp value)
      #(k ds))))

(defn- analyze-def [exp]
  (let [^String _name (name exp)
        sym (symbol (subs _name 0 (dec (.length _name))))]
    (fn [env st k] 
      (let [value (peek st)]
        (vars/def-var! env sym value)
        #(k st)))))

(defn- analyze-lookup [sym]
  (fn [env ds k]
    (let [result (vars/lookup-var env sym)]
      #(k (conj ds result)))))

(defn- map-params [{{:keys [params var-params]} :params body :body}]
  (Arity. (mapv second params)
          (-> var-params :var-form second)
          (analyze (seq (second body)))))

(defn- analyze-defn [[_ & f]]
  (let [{fn-name :fn-name [arity body] :fn-tail}
        (s/conform ::specs/defn-args f)

        arities 
        (cond (= arity :arity-1) [(map-params body)]
              (= arity :arity-n) (mapv map-params (:bodies body)))]
    (fn [env ds k]
      (vars/def-var!
        env
        fn-name
        (Defn. arities env fn-name))
      #(k ds))))

(defn- match-defn-arity [^Defn {:keys [arities]} args]
  (letfn [(better-match? [{:keys [params var-params] :as arity} best]
            (and var-params
                 (>= (count args) (count params))
                 (> (count (:params arity))
                    (count (:params best)))))
          (exact-match? [{:keys [params var-params]}]
            (and (= (count params) (count args)) (nil? var-params)))]
    (loop [[arity & tail] arities
           best nil]
      (cond (nil? arity) best
            (exact-match? arity) arity
            (better-match? arity best) (recur tail arity)
            :else (recur tail best)))))

(defn- zip-params-args [{:keys [params var-params]} args]
  (let [pcount (count params)
        varargs (seq (drop pcount args))
        params* (cond-> params varargs (conj var-params))
        args* (cond-> (vec (take pcount args))  varargs (conj varargs))]
    (zipmap params* args*)))

(defn analyze-java-call [exp]
  (let [call (JavaCall. exp)]
    (fn [_ ds k]
      #(k (conj ds call)))))

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
          ((.body arity) env ds k))

        (instance? JavaCall proc)
        #(k (conj ds (eval `(~(.sym ^JavaCall proc) ~@args))))

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

(defn- java-call? [sym]
  (let [^String nm (name sym)]
    (or (str/starts-with? nm ".")
        (str/ends-with? nm ".")
        (.contains (str sym) "/"))))

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
            (= sym '<continue>) (generate-call-cc '<continue_>) 
            (= sym '<break>) (generate-call-cc '<break_>)
            (def? _name) (analyze-def sym)
            (java-call? sym) (analyze-java-call sym)
            :else (analyze-lookup sym)))) 

  clojure.lang.ISeq
  (analyze [[op :as exp]]
    (case op
      set! (analyze-assignment exp) 
      def (analyze-def exp)
      defn> (analyze-defn exp)
      call/cc> (analyze-call-cc exp)
      each> (analyze-loop exp loop-vals)
      times> (analyze-loop exp loop-count)
      if> (analyze-if exp)
      when> (analyze-when exp)
      quote> (analyze-quoted exp) 
      quote (analyze-quoted exp)
      invoke> (analyze-application exp)
      (analyze-sequence exp))))

(comment 

  (set! *warn-on-reflection* true)
  
  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (fs-api/stop)

  )
