(ns dv.pathom
  (:require
    [clojure.spec.alpha :as s]
    [clojure.core.async :as async :refer [go <! >! <!! chan]]
    [clojure.pprint :refer [pprint]]
    [com.wsscode.common.async-clj :refer [let-chan]]
    [com.wsscode.pathom.connect :as pc]
    [com.wsscode.pathom.core :as p]
    [com.wsscode.pathom.viz.ws-connector.core :as pathom-viz]
    [edn-query-language.core :as eql]
    [mount.core :refer [defstate]]
    [taoensso.timbre :as log]
    [clojure.walk :as w]))

(pc/defresolver index-explorer [env _]
  {::pc/input  #{:com.wsscode.pathom.viz.index-explorer/id}
   ::pc/output [:com.wsscode.pathom.viz.index-explorer/index]}
  {:com.wsscode.pathom.viz.index-explorer/index
   (-> (get env ::pc/indexes)
     (update ::pc/index-resolvers #(into {} (map (fn [[k v]] [k (dissoc v ::pc/resolve)])) %))
     (update ::pc/index-mutations #(into {} (map (fn [[k v]] [k (dissoc v ::pc/mutate)])) %)))})

(defn preprocess-parser-plugin
  "Helper to create a plugin that can view/modify the env/tx of a top-level request.

  f - (fn [{:keys [env tx]}] {:env new-env :tx new-tx})

  If the function returns no env or tx, then the parser will not be called (aborts the parse)"
  [f]
  {::p/wrap-parser
   (fn transform-parser-out-plugin-external [parser]
     (fn transform-parser-out-plugin-internal [env tx]
       (let [{:keys [env tx] :as req} (f {:env env :tx tx})]
         (if (and (map? env) (seq tx))
           (parser env tx)
           {}))))})

(defn log-requests [{:keys [env tx] :as req}]
  (println)
  (log/debug "Pathom transaction:")
  (pprint tx)
  (println)
  req)

;; Captures the last env so you can inspect it at the repl
(def -env (atom nil))
(comment
  (keys @-env)
  (@-env :target)
  (@-env :ring/request)
  (@-env ::p/entity-key)
  (@-env ::p/entity))

(defn mk-augment-env-request
  [get-config-map]
  (fn augment-env-request
    [env]
    (reset! -env env)
    (merge env (get-config-map env))))

;; Copied from fulcro-rad, but changed to also pass pararms for mutations.
(def query-params-to-env-plugin
  "Adds top-level load params to env, so nested parsing layers can see them."
  {::p/wrap-parser
   (fn [parser]
     (fn [env tx]
       (let [children     (-> tx eql/query->ast :children)
             query-params (reduce
                            (fn [qps {:keys [type params]}]
                              (cond-> qps
                                (seq params) (merge params)))
                            {}
                            children)
             env          (assoc env :query-params query-params)]
         (log/info "Query params are: " query-params)
         (parser env tx))))})

;;; TODO use the helpers here:
;;; https://github.com/fulcrologic/fulcro-rad/blob/develop/src/main/com/fulcrologic/rad/pathom.clj
;; deals with removing keys and logging responses etc.

(defn build-parser [{:keys [resolvers log-responses? enable-pathom-viz? env-additions]}]
  (when-not (fn? env-additions) (throw (Exception. "env-additions must be a function.")))
  (let [real-parser (p/parallel-parser
                      {::p/mutate  pc/mutate-async
                       ::p/env     {::p/reader               [p/map-reader
                                                              pc/parallel-reader
                                                              pc/open-ident-reader
                                                              p/env-placeholder-reader]
                                    ::p/placeholder-prefixes #{">"}}
                       ::p/plugins [(pc/connect-plugin {::pc/register resolvers})
                                    (p/env-wrap-plugin (mk-augment-env-request env-additions))
                                    (preprocess-parser-plugin log-requests)
                                    p/error-handler-plugin
                                    query-params-to-env-plugin
                                    (p/post-process-parser-plugin p/elide-not-found)
                                    ;p/trace-plugin
                                    ]})
        ;; NOTE: Add -Dtrace to the server JVM to enable Fulcro Inspect query performance traces to the network tab.
        ;; This makes the network responses much larger and should not be used in production.
        trace?      (not (nil? (System/getProperty "trace")))
        real-parser (cond->> real-parser
                      enable-pathom-viz?
                      (pathom-viz/connect-parser {::pathom-viz/parser-id ::parser}))]
    (fn wrapped-parser [env tx]
      (when-not (vector? tx) (throw (Exception. "You must pass a vector for the transaction.")))
      (let [tx   (if trace? (conj tx :com.wsscode.pathom/trace) tx)
            resp (async/<!! (real-parser env tx))]

        (when log-responses?
          (log/info "Pathom response:")
          (pprint (w/postwalk-replace {:user/password nil} resp)))
        resp))))
