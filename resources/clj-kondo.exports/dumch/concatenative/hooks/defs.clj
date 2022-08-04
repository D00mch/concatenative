(ns hooks.defs
  (:require [clj-kondo.hooks-api 
             :refer [token-node list-node vector-node reg-finding!
                     token-node? list-node? vector-node? sexpr]]
            [clojure.string :as str]))

(defn- check! [condition node msg]
  (when (not condition) 
    (reg-finding! (assoc (meta node) :message msg :type :concatenative/defstackfn))))

(declare traverse-body)

(defn invoke> [head body]
  (check! (= (count body) 2) head "invoke> requires exactly 2 args")
  (check! (fn? (eval (sexpr (first body)))) head "first arg should be a function")
  (list-node (list* (token-node 'do) (traverse-body body))))

(defn if> [head body]
  (check! (some #(= (str %) "else>") body) head "no else> branch")
  (token-node 11)
  (let [[if [_ & el]] (split-with #(-> % str (not= "else>")) body)]
    (list-node (list* (token-node 'do) (traverse-body (concat if el))))))

(defn let> [head tail]
  (let [h-str (str head)
        s-node (token-node (symbol (subs h-str 0 (dec (count h-str)))))]
    #_(check! (str/starts-with? h-str "!") head "vars should start with '!'")
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

              :else (cons head (traverse-body tail)))) 

      (list-node? head) 
      (let [[fn-name & body] (:children head)]
        (cons (case (str fn-name)
                "invoke>" (invoke> head body)
                "if>" (if> head body)
                head)

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
