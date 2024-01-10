(ns clj-typechecker.core
  (:require [clojure.core.logic :as l])
  (:gen-class))

(defn- typecheckeo [ast ctx t]
  (cond
    (integer? ast) (l/== t [:int])
    (float? ast) (l/== t [:float])
    (boolean? ast) (l/== t [:bool])
    (nil? ast) (l/== t [:unit])
    (symbol? ast) (if (contains? ctx ast)
                    (l/== (ctx ast) t)
                    l/fail)
    (list? ast)
      (case (first ast)
        lambda  (let [[_ param body] ast]
                    (l/fresh [t-param t-body]
                      (typecheckeo body (assoc ctx param t-param) t-body)
                      (l/== t [:-> t-param t-body])))
        let     (let [[_ [name value] body] ast]
                  (l/fresh [t-value]
                    (typecheckeo value (assoc ctx name t-value) t-value)
                    (typecheckeo body (assoc ctx name t-value) t)))
        if      (let [[_ b x y] ast]
                  (l/fresh []
                    (typecheckeo b ctx [:bool])
                    (typecheckeo x ctx t)
                    (typecheckeo y ctx t)))
        (let [[f arg] ast]
          (l/fresh [t-f t-arg]
            (typecheckeo f ctx [:-> t-arg t])
            (typecheckeo arg ctx t-arg))))

    :else l/fail))

(defn typecheck
  ([ast] (typecheck ast {}))
  ([ast ctx]
   (let [res (l/run* [t] (typecheckeo ast ctx t))]
     (first res))))
