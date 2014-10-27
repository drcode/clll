(ns clll.test-utilities
    (:require [clojure.data :refer [diff]]
              [clojure.set :refer [difference]]))

(defn test-init 
      ([contract caller block-info]
         (dissoc (contract {} {:caller caller} (merge {:hash 123 :number 1} block-info)) :result))
      ([contract caller]
         (test-init contract caller {})))

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
               fun                      (first result)
               storage                  (merge storage storage-override)]
              (let [[old new] (diff {:storage storage} (apply fun storage message more))
                    deleted (difference (set (keys (:storage old))) (set (keys (:storage new))))
                    storage-nu (merge (:storage new) 
                                 (into {}
                                       (for [k deleted]
                                            [k 0])))]
                   (if (seq storage-nu)
                       (assoc new :storage storage-nu)
                       new)))))
