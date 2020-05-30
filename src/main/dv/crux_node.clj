(ns dv.crux-node
  (:require
    [crux.api :as crux]
    [clojure.java.io :as io])
  (:import [crux.api ICruxAPI]))

(defn rocks-config [data-dir]
  {:crux.node/topology                 '[crux.standalone/topology
                                         ;crux.kv.rocksdb/kv-store
                                         crux.kv.rocksdb/kv-store-with-metrics
                                         ; enable http server
                                         ;crux.http-server/module
                                         ]
   ;:crux.http-server/port 8099
   :crux.kv/db-dir                     (str (io/file data-dir "db"))
   :crux.standalone/event-log-dir      (str (io/file data-dir "eventlog"))
   :crux.standalone/event-log-kv-store 'crux.kv.rocksdb/kv
   :crux.standalone/event-log-sync?    true
   :crux.kv/sync?                      true})

(defn start-crux-rocks-node ^ICruxAPI [data-dir]
  (crux/start-node (rocks-config data-dir)))
