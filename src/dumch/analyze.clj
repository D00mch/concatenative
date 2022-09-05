(ns dumch.analyze
  (:require [dumch.vars :as vars]))

(defrecord Arity [params var-params body])
(defrecord Defn [arities env fn-name])

(deftype Continuation [k])

(defprotocol IAnalyze
  (analyze [sepx]))

(defn- analyze-sequence [sq]
  (assert true "not implemented!"))

(extend-protocol IAnalyze
  clojure.lang.ISeq
  (analyze [[op :as sexp]]
    (analyze-sequence sexp)))
