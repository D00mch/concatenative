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

(defn- check-local-bound [exp]
  (is (thrown-with-msg?
        Exception
        #"not found"
        (eval- exp))))

(deftest if-test
  (testing "if> dispatch on truthy value"
    (is (= (eval- '(true (if> 1 else> 2)))    [1]))
    (is (= (eval- '(0 (if> 1 else> 2)))       [1]))
    (is (= (eval- '(false (if> 1 else> 2)))   [2]))
    (is (= (eval- '(nil (if> 1 else> 2)))     [2])))

  (testing "if> fills the stack"
    (is (= (eval- '(1 (if> 1 1 2 3 else> 2))) [1 1 2 3]))
    (is (= (eval- '(1 (if> 1 [1 2] else> 2))) [1 [1 2]]))
    (is (= (eval- '(nil (if> 1 else> 2 2)))   [2 2])))

  (testing "if has local scope"
    (check-local-bound '(false
                          (if> 1 !a+ else> 1 !a+)
                          !a))))

(deftest when-test
  (testing "when> dispatch on truthy value"
    (is (= (eval- '(true (when> 1)))    [1]))
    (is (= (eval- '(0 (when> 1)))       [1]))
    (is (= (eval- '(false (when> 1)))   []))
    (is (= (eval- '(nil (when> 1)))     [])))

  (testing "when> fills the stack"
    (is (= (eval- '(1 (when> 1 1 [2]))) [1 1 [2]])))

  (testing "when has local scope"
    (check-local-bound '(true (when> 1 !a+) !a))))

(deftest quotation-test []
  (testing "using quotation as seq"
    (is (= (eval- '('(1 1 1) (invoke> count 1)))
           [3]))

    (is (= (eval- '((quote> 1 2)
                    (invoke> vec 1)))
           [[1 2]])))

  (testing "using quotation as lambda"
    (is (= (eval- '((defn> each
                      [!vc !quot]
                      !vc
                      (when>
                        !vc
                        (invoke> first 1)
                        !quot ;; quotation as lambda 
                        <call>
                        !vc
                        (invoke> next 1)
                        !quot
                        (invoke> each 2)))

                    '(1 2 3 4)
                    '("n:"
                       <swap>
                       (invoke> str 2))
                    (invoke> each 2)))
           ["n:1" "n:2" "n:3" "n:4"]))))

(deftest loops
  (testing "'times>' loop with empty body"
    (is (= (eval- '(99 (times> ) "TheEnd"))
           ["TheEnd"])))

  (testing "'each>' loop with empty body"
    (is (= (eval- '(3 (invoke> range 1) (each>)))
           [0 1 2])))

  (testing "'times>' loop"
    (is (= (eval- '(2 (times> 1 2)))
           [1 2 1 2])))

  (testing "'each>' loop with <continue> and <break>"
    (is (= (eval- '(15 (invoke> range 1)
                       (each>
                         ;; skip even
                         <dup>
                         (invoke> even? 1)
                         (when> <pop> <continue>)

                         ;; break after 7
                         <dup>
                         7 (invoke> = 2)
                         (when> <pop> <break>)

                         "n:"
                         <swap>
                         (invoke> str 2))
                       "TheEnd!"))
           ["n:1" "n:3" "n:5" "TheEnd!"]))))

(deftest call-cc
  (testing "simple call/cc"
    (is (= (eval- '(1
                    (call/cc>
                      ;; call/cc implicitly adds continuation to
                      ;; so we add a var `!c1` and pop from the stack
                      !c1+  
                      <pop>
                      2
                      (invoke> !c1 0)
                      3)
                    "end"))
           [1 2 "end"])))

  (testing "implement custom `each`, `break`, `continue`"
    (is (= (eval- '((defn> each
                      [!vc !quot]
                      !vc
                      (if>
                        !vc
                        (invoke> first 1)
                        !quot
                        <call>
                        !vc
                        (invoke> next 1)
                        !quot
                        (invoke> each 2)
                        else>
                        <pop>))

                    (call/cc>
                      !break+ <pop>
                      '(1 2 3 4 5 6 7)
                      '(call/cc>
                         !continue+ <pop>
                         !input+

                         ;; ignoring numbers more then 3
                         3
                         (invoke> > 2)
                         (when> (invoke> !break 0))
                         !input

                         ;; ignoring even
                         (invoke> even? 1)
                         (when> (invoke> !continue 0))

                         !input
                         "n:"
                         <swap>
                         (invoke> str 2))
                      (invoke> each 2))
                    "TheEnd!"))
           ["n:1" "n:3" "TheEnd!"]))))

(deftest defn-test []
  (testing "single-arity functino"
    (is (= (eval- '((defn> plus [!a !b]
                      !a !b (invoke> + 2))
                    1 2
                    (invoke> plus 2)))
           [3])))

  (testing "multi-arity funciton"
    (is (= (eval- '(
                    (defn> plus7 
                      "add 7 to sum of provided numbers"
                      ([!a]
                       !a 7 (invoke> + 2))
                      ([!a !b]
                       !a
                       !b
                       7
                       (invoke> + 3))
                      ([!a !b & !sq] ;; like varargs in Clojure
                       !sq 
                       (invoke> count 1)  ;; calculating args count
                       3                  ;; add 3 more args: !a, !b, 7 
                       (invoke> + 2)      ;; done
                       !args-count+
                       <pop>
                       !a !b
                       !sq <call> ;; move sequence to stack
                       7
                       (invoke> + !args-count)))
                    1 1 1 1
                    (invoke> plus7 4) ;; 1+1+1+1 + 7 = 11
                    (invoke> plus7 1) ;; 11 + 7 = 18
                    ))
           [18])))

  (testing "when has local scope"
    (is (= (eval- '((defn> f [] 1 !a+ !a)
                    (invoke> f 0)))
           [1 1]))
    (check-local-bound '((defn> f [] 1 !a+ !a)
                         (invoke> f 0)
                         !a))))

(deftest nil-test
  (testing "put nil in stack"
    (is (= (eval- '(nil !a+ !a)) 
           [nil nil]))
    (is (= (eval- '(nil (invoke> seq 1)))
           [nil]))))

(deftest local-vars-shadow
  (testing "shadow if vars"
    (is (= (eval- '(true
                     !a+
                     (if> 1 !a+ !a else> 0)
                     !a))
           [1 1 true])))

  (testing "shadow else vars"
    (is (= (eval- '(false
                     !a+
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

(deftest tailrec-test
  (testing "tail recursion will not produce stack oveflow"
    (is (= (eval- '([]
                    100000
                    (invoke> range 1)
                    (each>)
                    (invoke> conj 100001)
                    (invoke> count 1)))
           [100000]))
    (is (= (eval- '(100000
                     (times>)))
           []))
    (is (= (eval- '((defn> recursive [!n]
                      !n
                      0
                      (invoke> > 2)

                      (if> 
                        !n
                        (invoke> dec 1)
                        (invoke> recursive 1)
                        else> "end"))
                    100000
                    (invoke> recursive 1)))
           ["end"]))))

(deftest java-call
  (testing "constructor and virtual method"
    (is (= (eval- '((invoke> java.util.LinkedList. 0)
                    (invoke> .size 1)))
           [0]))
    (is (= (eval- '([1 2 3]
                    (invoke> java.util.ArrayList. 1)
                    2
                    (invoke> .get 2)))
           [3]))
    (is (= (eval- '("str"
                     (invoke> String. 1)
                     (invoke> .length 1)))
           (eval- '("str" (invoke> count 1))))))

  (testing "constructor and static methods"
    (is (= (eval- '([1 2 3]
                    (invoke> java.util.LinkedList. 1)
                    (invoke> java.util.Collections/max 1)))
           [3]))))
