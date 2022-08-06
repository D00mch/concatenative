(ns hooks.defs
  (:require [clj-kondo.hooks-api 
             :refer [token-node list-node vector-node reg-finding!
                     token-node? list-node? vector-node? sexpr]]
            [clojure.string :as str]))

(defn- check! [condition node msg]
  (when (not condition) 
    (reg-finding! (assoc (meta node) :message msg :type :concatenative/defstackfn))))

(declare traverse-body)

(defn invoke> [head [f cnt :as body]]
  (check! (= (count body) 2) head "invoke> requires exactly 2 args")
  (check! (fn? (eval (sexpr f))) f "first arg should be a function")
  (check! (nat-int? (sexpr cnt)) cnt "last arg should be a natural number")
  f)

(defn if> [head body]
  (check! (some #(= (str %) "else>") body) head "no else> branch")
  (let [[_if [_ & el]] (split-with #(-> % str (not= "else>")) body)]
    (list-node (list* (token-node 'do) 
                      ;; to isolate if body into separate node
                      (list-node (list* (token-node 'do)  (traverse-body _if)))
                      (traverse-body el)))))

(defn let> [head tail]
  (let [h-str (str head)
        s-node (token-node (symbol (subs h-str 0 (dec (count h-str)))))]
    (check! (str/starts-with? h-str "!") head "vars should start with '!'")
    (list-node 
      (list* 
        (token-node 'let)
        (vector-node [s-node (token-node nil)])
        (traverse-body tail)))))

(defn traverse-body [[head & tail]]
  (if head
    (cond 
      (token-node? head) 
      (let [h-str (str head)]
        (cond (= h-str "<pop>") (traverse-body tail)

              (and (> (count h-str) 2) (str/ends-with? h-str "+")) 
              (list (let> head tail))

              (and (not (str/starts-with? h-str "!"))
                   (symbol? (sexpr head)))     
              (do (check! false head "unsupported symbol")
                  (cons head (traverse-body tail)))

              :else (cons head (traverse-body tail)))) 

      (list-node? head) 
      (let [[fn-name & body] (:children head)]
        (cons (case (str fn-name)
                "invoke>" (invoke> head body)
                "if>" (if> head body)
                (do (check! false head "unknown form")
                    head))

              (traverse-body tail)))

      :else (traverse-body tail))
    nil))

(defn defstackfn [{:keys [node]}]
  (let [[fn-name args & body] (rest (:children node))]
    (check! (vector-node? args) args "args should be a vector")
    (check! (every? #(and (token-node? %) (str/starts-with? (str %) "!")) 
                    (:children args))
            args
            "args should be symbols prefixed with '!'")
    (let [result (list-node (list* (token-node 'defn)
                                   fn-name
                                   args
                                   (traverse-body body)))]
      #_(println :result (sexpr result))
      {:node result})))
