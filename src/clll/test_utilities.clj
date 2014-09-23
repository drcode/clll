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
              (take 2 (diff {:storage storage} (apply fun storage message more))))))
