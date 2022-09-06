(ns dumch.concatenative-test
  (:require [clojure.test :refer [deftest testing is]]
            [dumch.concatenative :refer [defstackfn eval-]]))

(deftest test-basic-stack-op
  (testing "put const in stack"
    (is (= (eval- '(1))                  [1]))
    (is (= (eval- '(1 \2 "3"))           [1 \2 "3"]))
    (is (= (eval- '([1 2 3]))            [[1 2 3]])))

  (testing "put var in stack"
    (is (= (eval- '(1 !a+ !a))           [1 1])))

  (testing "pop stack"
    (is (= (eval- '(1 <pop>))            [])))

  (testing "invoke"
    (is (= (eval- 
             '(1 2 (invoke> str 2)))     ["12"]))
    (is (= (eval- 
             '((invoke> pr 0)))          [nil]))))

(deftest if-test
  (testing "if> dispatch on truthy value"
    (is (= (eval- '(true (if> 1 else> 2)))    [1]))
    (is (= (eval- '(0 (if> 1 else> 2)))       [1]))
    (is (= (eval- '(false (if> 1 else> 2)))   [2]))
    (is (= (eval- '(nil (if> 1 else> 2)))     [2])))

  (testing "if> fills the stack"
    (is (= (eval- '(1 (if> 1 1 2 3 else> 2))) [1 1 2 3]))
    (is (= (eval- '(1 (if> 1 [1 2] else> 2))) [1 [1 2]]))
    (is (= (eval- '(nil (if> 1 else> 2 2)))   [2 2]))))

(deftest when-test
  (testing "when> dispatch on truthy value"
    (is (= (eval- '(true (when> 1)))    [1]))
    (is (= (eval- '(0 (when> 1)))       [1]))
    (is (= (eval- '(false (when> 1)))   []))
    (is (= (eval- '(nil (when> 1)))     [])))
  (testing "when> fills the stack"
    (is (= (eval- '(1 (when> 1 1 [2]))) [1 1 [2]]))))

(deftest nil-test
  (testing "put nil in stack"
    (is (= (eval- '(nil !a+ !a)) 
           [nil nil]))
    (is (= (eval- '(nil (invoke> seq 1)))
           [nil]))))

(deftest local-vars-shadow
  (testing "shadow if vars"
    (is (= (eval-
             '(true !a+
                    (if> 1 !a+ !a else> 0)
                    !a))
           [1 1 true])))
  (testing "shadow else vars"
    (is (= (eval-
             '(false !a+
                     (if> 0 else> 1 !a+ !a)
                     !a))
           [1 1 false]))))

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
       !v2
       0                   ; # divide by zero, but...
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
  (is (= (default-example 1 2 4) "241")))

(deftest runtime-errors-test
  (testing "insufficient arguments"
    (is (thrown-with-msg?
          Exception
          #"IndexOutOfBoundsException"
          (eval- '(2 (invoke> / 2))))))
  (testing "divide by zero"
    (is (thrown-with-msg?
          Exception
          #"Divide by zero"
          (eval- '(2 0 (invoke> / 2))))))
  (testing "pop from empty stack"
    (is (thrown-with-msg?
          Exception
          #"Can't pop empty vector"
          (eval- '(<pop>))))))
