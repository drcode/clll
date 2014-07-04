(ns clll.eval)

(def ^:dynamic *unsafe-storage-atom* nil)
(def ^:dynamic *unsafe-message* nil)
(def ^:dynamic *unsafe-memory* nil)
(def ^:dynamic *unsafe-transactions* nil)
(def ^:dynamic *unsafe-timestamps* nil)

(defn contract-eval [body]
      (fn fun ([storage message timestamps]
             (binding [*unsafe-storage-atom* (atom storage)
                       *unsafe-message*      message
                       *unsafe-memory*       (atom {})
                       *unsafe-transactions* (atom [])
                       *unsafe-timestamps*   (atom timestamps)]
                      (let [result (try
                                    (body)
                                    {:result nil}
                                    (catch clojure.lang.ExceptionInfo e#
                                           (-> e# ex-data)))]
                           (cond-> (assoc result :storage @*unsafe-storage-atom*)
                                   (seq @*unsafe-transactions*) (assoc :transactions @*unsafe-transactions*)))))
          ([storage message]
             (fun storage message []))))

