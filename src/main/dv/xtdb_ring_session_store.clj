(ns dv.xtdb-ring-session-store
  (:require
    [xtdb.api :as xt]
    [dv.xtdb-util :as xt-util]
    [io.pedestal.http.csrf :refer [anti-forgery-token-str]]
    [taoensso.timbre :as log])
  (:import
    [ring.middleware.session.store SessionStore]
    [java.util UUID]))

;; Crux doesn't support strings as keys and pedestal csrf lib doesn't support
;; configuring the key, so we replace the string with a keyword.

(defn make-session-data
  [key data]
  (-> data
    (assoc :__anti-forgery-token (get data anti-forgery-token-str))
    (dissoc anti-forgery-token-str)
    (assoc :xt/id key ::session? true)))

(deftype XTDBSessionStore [crux-node]
  SessionStore

  (read-session [this key]
    (if (some? key)
      (try
        (let [sess (xt/entity (xt/db crux-node) (UUID/fromString key))]
          (-> sess
            (assoc anti-forgery-token-str (:__anti-forgery-token sess))
            (dissoc :__anti-forgery-token)))
        (catch Exception e
          (log/error "Invalid session. Error reading crux/entity for key: " key)
          {}))
      {}))

  (write-session [_ key data]
    (let [key     (try (cond-> key (some? key) UUID/fromString)
                       (catch Exception e (UUID/randomUUID)))
          key     (or key (UUID/randomUUID))
          tx-data (make-session-data key data)]
      (log/trace "Writing session data: " tx-data)
      (log/trace "At key : " key)
      (xt-util/put crux-node tx-data)
      key))

  (delete-session [_ key]
    (log/info "Deleting session: " key)
    (xt-util/delete crux-node key)
    nil))

(defn xtdb-session-store [crux-node]
  (XTDBSessionStore. crux-node))
