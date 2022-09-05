(ns dumch.env-test
  (:require [clojure.test :refer [deftest testing is]]
            [dumch.env :refer [def-var! set-var! lookup-var
                               extend-env ->env global-env]])
  (:import (dumch.env Frame)
           (clojure.lang ExceptionInfo)))

(deftest priority-test
  (let [parent-env (->env)
        child-env (extend-env parent-env)]
    (testing "child-env should get vars from parent"
      (def-var! parent-env 'a 1)
      (def-var! parent-env 'b 10)
      (is (= 1 (lookup-var child-env 'a)))
      (is (= 10 (lookup-var child-env 'b))))

    (testing "resetting var in a child-env doesn't affect parent"
      (set-var! child-env 'a 3)
      (is (= 1 (lookup-var parent-env 'a))))

    (testing "defining var in child-env doesn't affect parent"
      (def-var! child-env 'b 20)
      (is (= 10 (lookup-var parent-env 'b)))
      (is (= 20 (lookup-var child-env 'b))))))

(deftest set-protection-test
  (let [e (->env)]
    (is (thrown? ExceptionInfo (set-var! e 'really-new-var 1)))
    (def-var! e 'really-new-var 1)
    (set-var! e 'really-new-var 2)))
