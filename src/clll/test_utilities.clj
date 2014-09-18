(ns clll.test-utilities)

(defn test-init [contract caller]
      (dissoc (contract {} {:caller caller}) :result))

(defn test-transaction
      ([contract caller message storage-override & more]
         (let [{:keys [storage result]} (contract {} {:caller caller})
               fun (first result)]
              (apply fun storage-override message more)))
      ([contract caller message]
         (let [{:keys [storage result]} (contract {} {:caller caller})
               fun (first result)]
              (fun storage message))))
