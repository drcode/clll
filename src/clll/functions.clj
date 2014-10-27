(ns clll.functions
    (:require [clll.eval :refer :all]))

(defn in-bytes [section-len]
      (quot section-len 32))

(defmacro pun [& body]
          (into {}
                (for [var body]
                     [(keyword var) var])))

(def function-names #{})

(defn caller []
  (*unsafe-message* :caller))

(defmacro lll [body pos]
          `(do (mstore ~pos (contract-eval (fn [] ~body)))
               32))

(defmacro lll-for [a b c & body]
          `(do ~a
               (loop []
                     (when (lll-truth ~b)
                           ~@body
                           ~c
                           (recur)))))

(defmacro lll-let [& body]
          `(let ~@body))

(defn lll-truth [x]
      (not (and (number? x) (zero? x))))

(defn sstore [key val]
      (if (lll-truth val)
          (swap! *unsafe-storage-atom* assoc key val)
          (swap! *unsafe-storage-atom* dissoc key)))

(defmacro lll-and [& body]
          `(if (and ~@(for [item body]
                           `(lll-truth ~item)))
               1
               0))

(defmacro lll-or [& body]
          `(if (or ~@(for [item body]
                          `(lll-truth ~item)))
               1
               0))

(defn coerce-integer [x]
      (if (integer? x)
          x
          (apply + (map * (map int (reverse (take 32 (concat (seq x) (repeat (char 0)))))) (iterate #(* % 2) 1)))))

(defn mstore [key val]
      (swap! *unsafe-memory* assoc key val))

(defn sload [key]
      (or (@*unsafe-storage-atom* key) (@*unsafe-memory* (coerce-integer key)) 0))

(defn mload [key]
      (or (@*unsafe-memory* key) (@*unsafe-memory* (coerce-integer key)) 0))

(defn lll-do [& x]
      (last x))

(defn calldatasize []
      (* (count (*unsafe-message* :data)) 32))

(defn calldataload [i]
  ((:data *unsafe-message*) (in-bytes i)))

(defn lll-dbg 
      ([x]
         (println)
         (println "dbg=" x)
         x)
      ([nam x]
         (println)
         (println nam "=" x)
         x))

(defn address []
  "12345")

(defn stop []
  (throw (ex-info "stopped"
                  {})))

(defn suicide [exp]
  (throw (ex-info "suicide"
                  {:suicide true})))

(defmacro lll-if [exp a b]
  `(if (lll-truth ~exp)
     ~a
     ~b))

(defmacro lll-when [exp & body]
  `(when (lll-truth ~exp)
     ~@body))

(defn lll-eq [a b]
  (if (= a b)
    1
    0))

(defn lll-ne [a b]
  (if (not= a b)
    1
    0))

(defn lll-lt [a b]
  (if (< a b)
    1
    0))

(defn lll-gt [a b]
  (if (> a b)
    1
    0))

(defn lll-lte [a b]
  (if (<= a b)
    1
    0))

(defn lll-gte [a b]
  (if (>= a b)
    1
    0))

(defn dump-memory []
      (println "memory" @*unsafe-memory*)
      (flush))

(defn lll-add [a b]
      (if (zero? b)
          a
          (+ (coerce-integer a) (coerce-integer b))))

(defn lll-mul [a b]
      (if (= b 1)
          a
          (* (coerce-integer a) (coerce-integer b))))

(defn lll-div [a b]
      (if (= b 1)
          a
          (quot (coerce-integer a) (coerce-integer b))))

(defn return [pos len]
      (throw (ex-info "early return"
                      {:result (vec (for [i (range (in-bytes len))]
                                         (mload (lll-add pos (* i 32)))))})))

(defn call-helper [gas to-address-direct to-address value send-location send-count return-location return-count]
      (when-let [child-contract-info (*unsafe-child-contracts* to-address-direct)]
                (let [initialized-contract (first (:result (to-address-direct {} {:caller nil})))
                      result (:result (initialized-contract (:storage child-contract-info)
                                                            {:caller (*unsafe-message* :caller)
                                                             :data (vec (for [i (range send-count)]
                                                                             (mload (+ send-location i))))
                                                             :value value}
                                                            *unsafe-block*
                                                            (or (:child-contracts child-contract-info) {})))]
                     (doseq [i (range (count result))]
                            (mstore (lll-add return-location (* i 32)) (result i)))))
      (swap! *unsafe-transactions* conj (pun gas to-address value send-location send-count return-location return-count)))

(defmacro call [gas to-address value send-location send-count return-location return-count]
          `(call-helper ~gas 
                        ~(if (string? to-address)
                             (symbol to-address)
                             to-address) 
                        ~to-address
                        ~value
                        ~send-location
                        ~send-count
                        ~return-location 
                        ~return-count))

(defn gas []
      0)

(defn callvalue []
      (or (*unsafe-message* :value) 0))

(defn gasprice []
      (throw (ex-info "not implemented" nil)))

(defn origin []
      (throw (ex-info "not implemented" nil)))

(defn balance []
      @*unsafe-balance*)

(defn coinbase []
      (throw (ex-info "not implemented" nil)))

(defn number []
      (:number *unsafe-block*))

(defn timestamp []
      (:timestamp *unsafe-block*))

(defn prevhash []
      (:hash  *unsafe-block*))

(defn difficulty []
      (throw (ex-info "not implemented" nil)))

(defn gaslimit []
      (throw (ex-info "not implemented" nil)))

(defn sha3 [n]
      (hash n))
