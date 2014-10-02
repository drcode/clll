(ns clll.eval)

(def ^:dynamic *unsafe-storage-atom* nil)
(def ^:dynamic *unsafe-message* nil)
(def ^:dynamic *unsafe-memory* nil)
(def ^:dynamic *unsafe-transactions* nil)
(def ^:dynamic *unsafe-block* nil)
(def ^:dynamic *unsafe-child-contracts* nil)
(def ^:dynamic *unsafe-balance* nil)

(defn contract-eval [body]
      (fn fun 
          ([storage message block child-contracts balance]
             (binding [*unsafe-storage-atom*    (atom storage)
                       *unsafe-message*         message
                       *unsafe-memory*          (atom {})
                       *unsafe-transactions*    (atom [])
                       *unsafe-block*           block
                       *unsafe-child-contracts* child-contracts
                       *unsafe-balance*         (atom balance)]
                      (let [result (try
                                    (body)
                                    {:result nil}
                                    (catch clojure.lang.ExceptionInfo e#
                                           (-> e# ex-data)))
                            result (cond-> (assoc result :storage @*unsafe-storage-atom*)
                                           (not (zero? @*unsafe-balance*)) (assoc :balance @*unsafe-balance*)
                                           (seq @*unsafe-transactions*) (assoc :transactions @*unsafe-transactions*))]
                           (if (:result result)
                               result
                               (dissoc result :result)))))
          ([storage message block child-contracts]
             (fun storage message block child-contracts 0))
          ([storage message block]
             (fun storage message block {} 0))
          ([storage message]
             (fun storage message {} {} 0))))

