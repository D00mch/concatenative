(ns dumch.vars
  (:require [clojure.pprint :refer [pprint]]))

(deftype Frame [^java.util.Map bindings parent]
  Object
  (toString [_]
    (with-out-str
      (pprint {:bindings bindings}))))

(def primitive-procedure-map 
  {'nil? nil? 'string? string? 'identity identity 'some? some?  
   'boolean? boolean? 'not not 'instance? instance? 'char? char?
   'str str 'print 'print 'prn prn 'pr pr 'println println
   '= = 'not= not=

   ;; numbers
   '== == '+ +, '- -, '* *, '/ / '> > '< < '>= >=
   '<= <= '+' +', '-' '-', '*' *', 'rem rem 'quot quot
   'even? even? 'odd? odd? 'dec dec 'inc inc 'number? number?

   ;; sequence
   'first first 'last last 'rest rest 'next next 'cons cons
   'count count 'seq seq 'pop pop 'conj conj 'peek peek
   'range range 'drop drop 'take take 'empty? empty?})


(def primitive? (set (vals primitive-procedure-map)))

(def global-env (Frame. primitive-procedure-map nil))

(defn lookup-var
  "Searching `x` variable in a Frame `env`
  first in `env.binding`, then in `env.parent`"
  [^Frame env x]
  (cond 
    (nil? env) (throw (ex-info "not found" {:var x :env env}))
    (contains? (.bindings env) x) (get (.bindings env) x)
    :else (recur (.parent env) x)))

(defn extend-env 
  ([base-env]
   (extend-env base-env {}))
  ([base-env ^java.util.Map m]
   (Frame. (java.util.HashMap. m) base-env)))

(defn ->env []
  (extend-env global-env))

(defn def-var! [^Frame env -name value]
  (.put ^java.util.HashMap (.bindings env) -name value)
  nil)

(defn set-var! [^Frame env -name value]
  (if (lookup-var env -name)
    (def-var! env -name value)
    (throw (ex-info "Unbound variable" {:name -name}))))
