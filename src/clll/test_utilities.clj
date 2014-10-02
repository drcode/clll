(ns clll.test-utilities
    (:require [clojure.data :refer [diff]]))

(defn test-init [contract caller & more]
      (dissoc (apply contract {} {:caller caller} more) :result))

(defn test-transaction
      ([contract caller message storage-override & more]
         (let [{:keys [storage result]} (apply contract {} {:caller caller} more)
               fun (first result)]
              (apply fun (merge storage storage-override) message more)))
      ([contract caller message]
         (let [{:keys [storage result]} (contract {} {:caller caller})
               fun (first result)]
              (fun storage message))))

(defn test-transaction-diff
      ([contract caller message storage-override & more]
         (let [{:keys [storage result]} (apply contract {} {:caller caller} more)
               fun (first result)
               storage (merge storage storage-override)]
              (let [[old new] (diff {:storage storage} (apply fun storage message more))
                    deleted (apply dissoc old (keys new))]
                   (if (seq deleted)
                       (assoc new :deletions deleted)
                       new)))))
