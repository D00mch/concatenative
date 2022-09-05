(ns dumch.concatenative
  "Stack based interpreter CPS implementation.

  Entry points:
  - use DSL with `defstackfn` macro.

  See docs and project README for the details."

  {:author "Artur Dumchev"}
  (:require
    [dumch.analyze :refer [analyze]]
    [dumch.vars :refer [->env]])
  #_(:import (dumch.analyze IAnalyze)))

(defn eval-program 
  ([sexps]
   (eval-program sexps (->env)))
  ([sexps env]
   (trampoline (analyze sexps) env [] identity)))

(defmacro defstackfn
  "Like `defn` with DSL body. See README for the details"
  [f-name args & tail]
  `(defn ~f-name ~args
     (peek
       (#'eval-program '~tail (zipmap '~args ~args)))))

(comment
  (set! *warn-on-reflection* false)
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
    (invoke> str 2))         ; "241" # and not 2424, as !a from `else` is local



  ; (defstackfn f2 [] 1 0 (invoke> / 2)) ;; divide by zero
  ; (f2)
  (ex-data *e)
  (ex-cause *e)
,)
