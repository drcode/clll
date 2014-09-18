(ns clll.core
    (:require [clojure.pprint :refer :all]
              [clojure.set :refer :all]
              [clll.eval :refer :all]
              [clll.functions :refer :all]))

;;CLLL is a variant of the ethereum LLL dialect, modified as minimally as possible to be compatible with the clojure reader. See cll-examples.core for details on minor syntax differences.


(defn disambiguate-functions
      "This function disambiguates clojure<->lll functions & special forms."
      [exp]
      (cond (or (seq? exp) (vector? exp)) (let [[command & params] exp
                                                expand (fn [params]
                                                           (map disambiguate-functions params))]
                                               (cond (= command 'for) (cons 'lll-for (expand params))
                                                     (= command 'do) (cons 'lll-do (expand params))
                                                     (= command 'if) (cons 'lll-if (expand params))
                                                     (= command 'when) (cons 'lll-when (expand params))
                                                     (= command 'and) (cons 'lll-and (expand params))
                                                     (= command 'or) (cons 'lll-or (expand params))
                                                     (= command '=) (cons 'lll-eq (expand params))
                                                     (= command '!=) (cons 'lll-ne (expand params))
                                                     (= command '<) (cons 'lll-lt (expand params))
                                                     (= command '>) (cons 'lll-gt (expand params))
                                                     (= command '<=) (cons 'lll-lte (expand params))
                                                     (= command '>=) (cons 'lll-gte (expand params))
                                                     (= command '+) (cons 'lll-add (expand params))
                                                     (= command '*) (cons 'lll-mul (expand params))
                                                     (= command '/) (cons 'lll-div (expand params))
                                                     (vector? exp) (vec (expand exp))
                                                     :else (cons command (expand params))))
            (symbol? exp) (if (function-names (keyword exp))
                              exp
                              (str exp))
            :else exp))

(defn double-brackets? [exp]
  (and (vector? exp) (vector? (exp 0))))

(defn expand-sstores
  "This function resolves the weird ([[key]] val) forms found in lll."
  [exp]
  (if (seq? exp)
    (let [[command & params] exp
          expand (fn [params]
                   (map expand-sstores params))]
      (cons command
            (expand ((fn fun [params]
                       (when-let [[cur & more] (seq params)]
                         (if (double-brackets? cur)
                           (cons (list 'sstore ((cur 0) 0) (first more))
                                 (fun (rest more)))
                           (cons cur (fun more)))))
                     params))))
    exp))

(defn single-brackets? [exp]
  (vector? exp))

(defn expand-mstores
  "This function resolves ([key] val)"
  [exp]
  (if (seq? exp)
    (let [[command & params] exp
          expand (fn [params]
                   (map expand-mstores params))]
      (cons command
            (expand ((fn fun [params]
                       (when-let [[cur & more] (seq params)]
                         (if (single-brackets? cur)
                           (cons (list 'mstore (cur 0) (first more))
                                 (fun (rest more)))
                           (cons cur (fun more)))))
                     params))))
    exp))

(defn deref? [x]
      (= x 'clojure.core/deref))

(defn expand-derefs
  "This function handles @i and @@i"
  [exp]
  (if (seq? exp)
    (let [[command & params] exp
          expand (fn [params]
                   (map expand-derefs params))]
      (if (deref? command)
          (if (and (seq? (first params)) (deref? (first (first params))))
              (list 'sload (second (first params)))
              (list 'mload (first params)))
          (cons command (expand params))))
    exp))


(defn defcontract-helper [body]
  (-> (cons 'do body)
      disambiguate-functions
      expand-sstores
      expand-mstores
      expand-derefs))

(defn defcontract-helper-verbose [body]
  (let [show (fn [exp txt]
               (println)
               (println (str txt \:))
               (println)
               (pprint exp)
               exp)]
    (-> (cons 'do body)
        disambiguate-functions
        (show "disambiguated")
        expand-sstores
        (show "sstores")
        expand-mstores
        (show "mstores")
        expand-derefs
        (show "derefs"))))


(defmacro defcontract [nam & body]
  `(def ~nam
     (contract-eval (fn []
                      ~(defcontract-helper body)))))



