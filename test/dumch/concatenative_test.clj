(ns dumch.concatenative-test
  (:require [clojure.test :refer [deftest testing is]]
            [dumch.concatenative :refer [defstackfn interpreter ->State]]))

(def new-state (->State '() [] {}))

(defn- program->stack [programm]
  (:stack (interpreter (assoc new-state :program programm))))

(deftest test-basic-stack-op
  (testing "put const in stack"
    (is (= (program->stack '(1))         [1]))
    (is (= (program->stack '(1 \2 "3"))  [1 \2 "3"]))
    (is (= (program->stack '([1 2 3]))   [[1 2 3]])))
  
  (testing "put var in stack"
    (is (= (program->stack '(1 !a+ !a))  [1 1])))

  (testing "pop stack"
    (is (= (program->stack '(1 <pop>))   [])))

  (testing "invoke"
    (is (= (program->stack 
             '(1 2 (invoke> str 2)))     ["21"]))
    (is (= (program->stack
             '((invoke> 
                 (constantly nil) 0)))   [nil])))

  (testing "if> dispatch on truthy value"
    (is (= (program->stack
             '(true (if> 1 else> 2)))    [1]))
    (is (= (program->stack
             '(0 (if> 1 else> 2)))       [1]))
    (is (= (program->stack
             '(false (if> 1 else> 2)))   [2]))
    (is (= (program->stack
             '(nil (if> 1 else> 2)))     [2]))))


 #_{:clj-kondo/ignore [:concatenative/defstackfn]} ; divide by zero in 'if' branch
(defstackfn default-example 
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
  (invoke> str 2))


(deftest integration-test
  (is (= (default-example 1 2 4) "124")))

;; TODO: test
;;  pop empty stack
;;  / by 0
;; invoke> with insufficient args 
