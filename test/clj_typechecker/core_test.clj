(ns clj-typechecker.core-test
  (:require [clojure.test :refer :all]
            [clj-typechecker.core :refer [typecheck]]))

(deftest typecheck-tests
  (testing "infer const (int)"
    (is (= (typecheck 42) [:int])))

  (testing "infer const (float)"
    (is (= (typecheck 42.0) [:float])))

  (testing "infer const (bool)"
    (is (= (typecheck true) [:bool])))

  (testing "infer const (unit)"
    (is (= (typecheck nil) [:unit])))

  (testing "ident when in context"
    (is (= (typecheck 'x '{x [:int]}) [:int])))

  (testing "ident when not in context"
    (is (= (typecheck 'x '{}) nil)))

  (testing "ident when not in context"
    (is (= (typecheck 'x '{}) nil)))

  (testing "application when function has the right type"
    (is (= (typecheck '(is-even 2) '{is-even [:-> [:int] [:bool]]})
           [:bool])))

  (testing "application when function doesn't have the right type"
    (is (= (typecheck '(not 2) '{not [:-> [:bool] [:bool]]})
           nil)))
  
  (testing "abstraction ignoring param"
    (is (= (typecheck '(fn x 42))
           '[:-> _0 [:int]])))
           
  (testing "abstraction returning param"
    (is (= (typecheck '(fn x x))
           '[:-> _0 _0])))
                    
  (testing "complex abstraction example"
    (is (= (typecheck '(fn f (f 42)))
           '[:-> [:-> [:int] _0] _0])))
  
  (testing "let expression ignoring param"
    (is (= (typecheck '(let [x 0] 42))
           [:int])))
           
  (testing "let expression ignoring param"
    (is (= (typecheck '(let [x true] x))
           [:bool])))

  (testing "let recursion"
    (is (= (typecheck '(let [f (fn x (f nil))] f))
           '[:-> [:unit] _0])))

  (testing "if expressions should infer the return type"
    (is (= (typecheck (if true 0 0))
           [:int])))

  (testing "if expressions should fail when condition has the wrong type "
    (is (= (typecheck '(if 42 nil nil))
           nil)))

  (testing "if expressions should infer the condition type as bool"
    (is (= (typecheck '(fn x (if x 0 0)))
           [:-> [:bool] [:int]])))
  
  (testing "if expressions should have have both arg as the same value"
    (is (= (typecheck '(fn x (if true x 0)))
           [:-> [:int] [:int]])))
           
  (testing "do expr should return the last value's type"
    (is (= (typecheck '(do true 42))
           [:int])))
           
  (testing "do expr should fail when one expr fails"
    (is (= (typecheck '(do k 42))
           nil))))
