(ns clll.core-test
  (:require [clojure.test :refer :all]
            [clll.core :refer :all]
            [clll.functions :refer :all]
            [clll.test-utilities :refer :all]
            [clll-examples.core :refer :all]))

(deftest disambiguate-functions-test
         (let [df disambiguate-functions]
              (is (= (df '(do)) '(lll-do)))
              (is (= (df '(for)) '(lll-for)))
              (is (= (df '(do (for))) '(lll-do (lll-for))))
              (is (= (df '69) 69))))

(deftest expand-sstores-test
         (let [es expand-sstores]
              (is (= (es '(lll-do [[foo]] "bar")) '(lll-do (sstore foo "bar"))))
              (is (= (es '(lll-do (return [[foo]] "bar"))) '(lll-do (return (sstore foo "bar")))))))

(deftest expand-mstores-test
         (let [em expand-mstores]
              (is (= (em '(lll-do [i] 2)) '(lll-do (mstore i 2))))))

(deftest expand-derefs-test
         `(let [ed expand-derefs]
              (is (= (ed '(lll-do @i)) '(lll-do (mload i))))
              (is (= (ed '(lll-do @@i)) '(lll-do (sload i))))
              (is (= (ed '(lll-do @@(+ next-pod-stats @i))) '(lll-do (sload (+ next-pod-stats (mload i))))))
              (is (= (ed '(lll-do (sstore 69 5))) '(lll-do (sstore 69 5))))))

(deftest defcontract-helper-test
         (let [dch defcontract-helper]
              (is (= (dch '([[69]] (caller)
                              (return 0 (lll
                                         (lll-when (lll-eq (caller) @@69))))))
                     '(lll-do
                       (sstore 69 (caller))
                       (return 0 (lll (lll-when (lll-eq (caller) (sload 69))))))))))

(deftest key-value-publisher-test
         (is (= (test-init key-value-publisher "foo") {:storage {69 "foo"}}))
         (is (= (test-transaction key-value-publisher
                                  "foo"
                                  {:caller "foo"
                                   :data ["favorite-color" "green"]})
                {:result nil
                 :storage {69 "foo"
                           "favorite-color" "green"}}))
         #_(is (= (test-transaction key-value-publisher
                                  "foo"
                                  {:caller "bar"
                                   :data ["favorite-color" "green"]})
                {:result nil
                 :storage {69 "foo"}}))
         #_(is (= (test-transaction key-value-publisher
                                  "foo"
                                  {:caller "foo"
                                   :data ["favorite-color" "green" "favorite-animal" "cat"]})
                {:result nil
                 :storage {69 "foo"
                           "favorite-color" "green"
                           "favorite-animal" "cat"}})))

(deftest name-registrar-test
         (is (= (test-init name-registrar "foo")
                {:storage {69 "foo" "NameReg" "12345" "12345" "NameReg"}}))
         (is (= (test-transaction name-registrar
                                  "foo"
                                  {:caller "foo"})
                {:storage {69 "foo"
                           "NameReg" "12345"
                           "12345" "NameReg"}
                 :suicide true}))
         (is (= (test-transaction name-registrar
                                  "foo"
                                  {:caller "bar"
                                   :data ["amazon.com"]})
                {:storage {"bar" "amazon.com"
                           "amazon.com" "bar"
                           69 "foo"
                           "NameReg" "12345"
                           "12345" "NameReg"}}))
         (is (= (test-transaction name-registrar
                                  "foo"
                                  {:caller "bar"
                                   :data ["google.com"]}
                                  {"bar" "amazon.com"
                                   "amazon.com" "bar"
                                   69 "foo"
                                   "NameReg" "12345"
                                   "12345" "NameReg"})
                {:storage {"bar" "google.com"
                           "google.com" "bar"
                           69 "foo"
                           "NameReg" "12345"
                           "12345" "NameReg"}}))
         (is (= (test-transaction name-registrar
                                  "foo"
                                  {:caller "bar"
                                   }
                                  {"bar" "amazon.com"
                                   "amazon.com" "bar"
                                   69 "foo"
                                   "NameReg" "12345"
                                   "12345" "NameReg"})
                {:storage {69 "foo"
                           "NameReg" "12345"
                           "12345" "NameReg"}})))


(deftest bank-test
         (is (= (test-init bank "foo")
                {:storage {}, :transactions [{:gas -100, :to-address "name-registrar" :value 0, :send-location 0, :send-count 4, :return-location 0, :return-count 0}]}))
         (is (= (test-transaction bank
                                  "foo"
                                  {:caller "bar"
                                   :data [100]
                                   :value 100})
                {:storage {"bar" 100}, :result nil})
             "deposit")
         (is (= (test-transaction bank
                                  "foo"
                                  {:caller "bar"
                                   :data [100]
                                   :value 10})
                {:storage {"bar" 10}, :result nil})
             "withdraw nonexistent money")
         (is (= (test-transaction bank
                                  "foo"
                                  {:caller "bar"
                                   :data [100]}
                                  {"bar" 300})
                {:transactions [{:gas -100, :to-address "bar", :value 100, :send-location 0, :send-count 0, :return-location 0, :return-count 0}], :storage {"bar" 200}, :result nil})
             "withdraw to self")
         (is (= (test-transaction bank
                                  "foo"
                                  {:caller "bar"
                                   :data [100 "baz"]}
                                  {"bar" 300})
                {:transactions [{:gas -100, :to-address "baz", :value 100, :send-location 0, :send-count 0, :return-location 0, :return-count 0}], :storage {"bar" 200}, :result nil})
             "send to another accountholder"))
(deftest splitter-test
         (is (= (test-init splitter "foo")
                {:storage {}, :transactions [{:gas -100, :to-address "name-registrar" :value 0, :send-location 0, :send-count 8, :return-location 0, :return-count 0}]}))
         (is (= (test-transaction splitter
                                  "foo"
                                  {:caller "bar"
                                   :data ["bob" "jim" "suzy"]
                                   :value 120})
                {:transactions [{:gas -100, :to-address "bob", :value 40, :send-location 0, :send-count 0, :return-location 0, :return-count 0} 
                                {:gas -100, :to-address "jim", :value 40, :send-location 0, :send-count 0, :return-location 0, :return-count 0} 
                                {:gas -100, :to-address "suzy", :value 40, :send-location 0, :send-count 0, :return-location 0, :return-count 0}]
                 :storage {}
                 :result nil})
             ""))

(deftest gav-coin-test
         (is (= (test-init gav-coin "foo")
                {:transactions [{:gas -100, :to-address "name-registrar", :value 0, :send-location 0, :send-count 7, :return-location 0, :return-count 0}], :storage {"foo" 79228162514264337593543950336N}}))
         (is (= (test-transaction gav-coin
                                  "foo"
                                  {:caller "bar"
                                   :data [1]})
                {:storage {"foo" 79228162514264337593543950336N}})
             "not enough data")
         (is (= (test-transaction gav-coin
                                  "foo"
                                  {:caller "foo"
                                   :data ["lisa" 100]})
                {:storage {"lisa" 100, "foo" 79228162514264337593543950236N}, :result nil})
             "send money")
         (is (= (test-transaction gav-coin
                                  "foo"
                                  {:caller "gerb"
                                   :data ["jim" 200]}
                                  {"lisa" 100})
                {:storage {"lisa" 100
                           "foo" 79228162514264337593543950336N}})
             "not enough money"))

(deftest time-vault-test
         (is (= (test-init time-vault "foo")
                {:transactions [{:gas -100, :to-address "name-registrar", :value 0, :send-location 0, :send-count 9, :return-location 0, :return-count 0}], :storage {"waiting_period" 3600, "owner" "foo"}}))
         (is (= (test-transaction time-vault 
                                  "foo" 
                                  {:caller "foo"
                                   :data ["withdrawal"]}
                                  {"waiting_period" 3600, "owner" "foo"}
                                  {:timestamp 10000})
                {:storage {"withdrawal_start" 10000, "owner" "foo", "waiting_period" 3600}, :result nil})
             "initiate withdrawal")
         (is (= (test-transaction time-vault 
                                  "foo" 
                                  {:caller "bar"
                                   :data ["withdrawal"]}
                                  {"waiting_period" 3600, "owner" "foo"}
                                  {:timestamp 10000})
                {:storage {"owner" "foo", "waiting_period" 3600}, :result nil})
             "initiate withdrawal, wrong user")
         (is (= (test-transaction time-vault 
                                  "foo" 
                                  {:caller "foo"
                                   :data ["finalize"]}
                                  {"waiting_period" 3600, "owner" "foo", "withdrawal_start" 10000}
                                  {:timestamp 10010})
                {:storage {"withdrawal_start" 10000, "owner" "foo", "waiting_period" 3600} :result nil})
             "finalized too early")
         (is (= (test-transaction time-vault 
                                  "foo" 
                                  {:caller "foo"
                                   :data ["finalize"]}
                                  {"waiting_period" 3600, "owner" "foo", "withdrawal_start" 10000}
                                  {:timestamp 18000})
                {:storage {"withdrawal_start" 10000, "owner" "foo", "waiting_period" 3600}, :suicide true})
             "success")
         (is (= (test-transaction time-vault 
                                  "foo" 
                                  {:caller "bar"
                                   :data ["finalize"]}
                                  {"waiting_period" 3600, "owner" "foo", "withdrawal_start" 10000}
                                  {:timestamp 18000})
                {:storage {"withdrawal_start" 10000, "owner" "foo", "waiting_period" 3600}, :result nil})
             "wrong finalizer"))
