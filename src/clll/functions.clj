(ns clll.functions
  (:require [clll.eval :refer :all]))

(defmacro pun [& body]
          (into {}
                (for [var body]
                     [(keyword var) var])))

(def function-names #{'caller 'return 'lll 'lll-when 'lll-for 'lll-let 'sstore 'mstore 'sload 'mload 'lll-do 'calldatasize 'calldataload 'address})

(defn caller []
  (*unsafe-message* :caller))

(defn return [pos exp]
  (throw (ex-info "early return"
                  {:result exp})))

(defmacro lll [body _]
  `(contract-eval (fn [] ~body)))

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

(defn sstore-helper [key val]
  (if (lll-truth val)
    (swap! *unsafe-storage-atom* assoc key val)
    (swap! *unsafe-storage-atom* dissoc key)))

(defmacro lll-and [& body]
          `(if (and ~@(for [item body]
                         `(lll-truth ~item)))
               1
               0))

(defmacro sstore [key val]
          (if (symbol? key)
              `(sstore-helper ~(str key) ~val)
              `(sstore-helper ~key ~val)))

(defmacro mstore [key val]
          (if (symbol? key)
              `(swap! *unsafe-memory* assoc ~(str key) ~val)
              `(swap! *unsafe-memory* assoc ~key ~val)))

(defn sload-helper [key]
      (or (@*unsafe-storage-atom* key) 0))

(defmacro sload [key]
      (if (symbol? key)
          `(sload-helper ~(str key))
          `(sload-helper ~key)))

(defmacro mload [key]
          (if (symbol? key)
              `(or (@*unsafe-memory* ~(str key)) 0)
              `(or (@*unsafe-memory* ~key) 0)))

(defn lll-do [& x])

(defn calldatasize []
      (* (count (*unsafe-message* :data)) 32))

(defn calldataload [i]
  ((:data *unsafe-message*) (Integer. (/ i 32))))

(defn lll-dbg [x]
  (println)
  (println "dbg=" x)
  x)

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

(defn call-helper [gas to-address value send-location send-count return-location return-count]
      (swap! *unsafe-transactions* conj (pun gas to-address value send-location send-count return-location return-count)))

(defmacro call [gas to-address value send-location send-count return-location return-count]
          `(call-helper ~gas ~to-address ~value ~send-location ~send-count ~return-location ~return-count))

(defn gas []
      0)

(defn callvalue []
      (or (*unsafe-message* :value) 0))

(defn timestamp []
      (let [[cur & more] @*unsafe-timestamps*]
           (reset! *unsafe-timestamps* more)
           cur))
