(ns dv.crux-ring-session-store
  (:require
    [crux.api :as crux]
    [dv.crux-util :as cutil]
    [io.pedestal.http.csrf :refer [anti-forgery-token-str]]
    [space.matterandvoid.util :as u]
    [taoensso.timbre :as log])
  (:import
    [ring.middleware.session.store SessionStore]
    [java.util UUID]))

;; Crux doesn't support strings as keys and pedestal csrf lib doesn't support
;; configuring the key, so we replace the string with a keyword.

(defn make-session-data [key data]
  (-> data
    (assoc :__anti-forgery-token (get data anti-forgery-token-str))
    (dissoc anti-forgery-token-str)
    (assoc :crux.db/id key ::session? true)))

(deftype CruxSessionStore [crux-node]
  SessionStore

  (read-session [this key]
    (if (some? key)
      (try
        (let [sess (crux/entity (crux/db crux-node) (UUID/fromString key))]
          (-> sess
            (assoc anti-forgery-token-str (:__anti-forgery-token sess))
            (dissoc :__anti-forgery-token)))
        (catch Exception e
          (log/error "Invalid session. Error reading crux/entity for key: " key)
          {}))
      {}))

  (write-session [_ key data]
    ;(log/info "writing session: at key: " key)
    (let [key     (try (cond-> key (some? key) UUID/fromString)
                       (catch Exception e (UUID/randomUUID)))
          key     (or key (u/uuid))
          tx-data (make-session-data key data)]
      ;(log/info "Writing session data: " tx-data)
      (cutil/put crux-node tx-data)
      ;(log/info "Read session: " (cutil/entity key))
      key))

  (delete-session [_ key]
    (log/info "Deleting session: " key)
    (cutil/delete crux-node key)
    nil))

(defn crux-session-store [crux-node]
  (CruxSessionStore. crux-node))

