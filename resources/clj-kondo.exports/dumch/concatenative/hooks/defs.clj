(ns hooks.defs
  (:require [clj-kondo.hooks-api
             :refer [token-node list-node vector-node reg-finding!
                     token-node? list-node? vector-node? sexpr]]
            [clojure.string :as str]))

(defn- check! [condition node msg]
  (when (not condition)
    (reg-finding! (assoc (meta node) :message msg :type :concatenative/defstackfn))))

(declare traverse-body)

(defn invoke->tranverse [head [f cnt :as body] tail stack]
  (check! (= (count body) 2) head "invoke> requires exactly 2 args")
  (check! (fn? (eval (sexpr f))) f "first arg should be a function")
  (check! (nat-int? (sexpr cnt)) cnt "last arg should be a natural number")
  (if (>= (count stack) (sexpr cnt))
    (let [stack* (subvec stack 0 (- (count stack) (sexpr cnt)))
          args* (map sexpr (reverse (take-last (sexpr cnt) stack)))]
      (check! (not (and (#{'/ 'quot 'rem 'mod} (sexpr f))
                        (some #(= % 0) (next args*))))
              f
              "divide by zero")
      (cons
        (list-node (list* f args*))
        (traverse-body tail (conj stack* :some-result))))
    (do
      (check! false head (str "Needs " (sexpr cnt) " arguments, but have only " (count stack)))
      (traverse-body tail stack))))

(defn if-node [head body stack]
  (check! (some #(= (str %) "else>") body) head "no else> branch")
  (let [[_if [_ & el]] (split-with #(-> % str (not= "else>")) body)]
    (list-node
      (list* (token-node 'do)
             ;; to isolate if body into separate node and have local bindings
             (list-node (list* (token-node 'do)  (traverse-body _if stack)))
             (traverse-body el stack)))))

(defn let-node [head tail stack]
  (let [h-str (str head)
        s-node (token-node (symbol (subs h-str 0 (dec (count h-str)))))]
    (check! (str/starts-with? h-str "!") head "vars should start with '!'")
    (list-node
      (list*
        (token-node 'let)
        (vector-node [s-node (token-node nil)])
        (traverse-body tail stack)))))

;; returns list of nodes
(defn traverse-body [[head & tail] stack]
  (if head
    (cond
      (token-node? head)
      (let [h-str (str head)]
        (cond (= h-str "<pop>")
              (let [_empty? (empty? stack)]
                (check! (not _empty?) head "can't pop from empty stack")
                (traverse-body tail (if _empty? stack (pop stack))))

              (and (> (count h-str) 2) (str/ends-with? h-str "+"))
              (list (let-node head tail stack))

              (and (not (str/starts-with? h-str "!"))
                   (symbol? (sexpr head)))
              (do (check! false head "unsupported symbol")
                  (cons head (traverse-body tail stack)))

              :else (cons head (traverse-body tail (conj stack head)))))

      (list-node? head)
      (let [[fn-name & body] (:children head)]
        (case (sexpr fn-name)
          invoke> (invoke->tranverse head body tail stack)
          if> (cons (if-node head body stack) (traverse-body tail stack))
          (do (check! false head "unknown form")
              (cons head (traverse-body tail stack)))))

      :else (traverse-body tail stack))
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
                                   (traverse-body body [])))]
      (println :result (sexpr result))
      {:node result})))
