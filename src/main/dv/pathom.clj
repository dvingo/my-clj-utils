(ns dv.pathom
  (:require
    [clojure.pprint :refer [pprint]]
    [com.wsscode.pathom.connect :as pc]
    [com.wsscode.pathom.core :as p]
    [com.wsscode.pathom.viz.ws-connector.core :as pathom-viz]
    [io.pedestal.interceptor.chain :as chain]
    [io.pedestal.interceptor.helpers :as ih]
    [edn-query-language.core :as eql]
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

;; Copied from fulcro-rad, but changed to also pass params for mutations.
(def query-params-to-env-plugin
  "Adds top-level load params to env, so nested parsing layers can see them."
  {::p/wrap-parser
   (fn [parser]
     (fn [env tx]
       (let [children     (-> tx eql/query->ast :children)
             query-params (reduce
                            (fn [qps {:keys [params]}]
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

(defn remove-omissions
  "Replaces black-listed keys from tx with ::omitted, meant for logging tx's
  without logging sensitive details like passwords."
  [sensitive-keys tx]
  (w/postwalk
    (fn [x]
      (if (and (vector? x) (= 2 (count x)) (contains? sensitive-keys (first x)))
        [(first x) ::omitted]
        x))
    tx))

(defn pprint-val [value]
  (binding [*print-level* 4 *print-length* 4]
    (try
      (with-out-str (pprint value))
      (catch Throwable e
        (log/error (.getMessage e))
        "<failed to serialize>"))))

(defn log-response!
  [sensitive-keys response]
  (log/info "Pathom response:\n"
    (pprint-val (remove-omissions sensitive-keys response))))

(defn build-parser
  [{:keys [resolvers
           log-responses?
           enable-pathom-viz?
           env-additions
           trace?
           index-explorer?
           sensitive-keys]}]
  (when (and env-additions (not (fn? env-additions)))
    (throw (Exception. "build-parser: env-additions must be a function.")))

  (let [sensitive-keys (conj (set sensitive-keys) :com.wsscode.pathom/trace)
        handle-errors? true
        parser         (p/parser
                         {::p/mutate  pc/mutate
                          ::p/env     {::p/reader
                                       [p/map-reader
                                        pc/reader2
                                        pc/index-reader
                                        pc/open-ident-reader
                                        p/env-placeholder-reader]
                                       ::p/placeholder-prefixes
                                       #{">"}}
                          ::p/plugins (cond->

                                        [(pc/connect-plugin
                                           {::pc/register
                                            (cond-> resolvers index-explorer? (conj index-explorer))})
                                         (preprocess-parser-plugin log-requests)
                                         query-params-to-env-plugin
                                         (p/post-process-parser-plugin p/elide-not-found)]

                                        handle-errors? (conj p/error-handler-plugin)
                                        env-additions (conj (p/env-wrap-plugin (mk-augment-env-request env-additions)))
                                        trace? (conj p/trace-plugin))})
        parser         (cond->> parser
                         enable-pathom-viz?
                         (pathom-viz/connect-parser {::pathom-viz/parser-id ::parser}))]
    (fn wrapped-parser [env tx]
      (when-not (vector? tx) (throw (Exception. "You must pass a vector for the transaction.")))
      ;; Add trace - pathom-viz already adds it so only add if that's not included.
      (let [tx   (if (and trace? (not enable-pathom-viz?))
                   (conj tx :com.wsscode.pathom/trace) tx)
            resp (parser env tx)]
        (when log-responses?
          (log-response! sensitive-keys resp))
        resp))))

(def response-key :dv/response)

(defn assoc-response [in-map response]
  (assoc-in in-map [:env response-key] response))

(defn get-response [in-map]
  (get-in in-map [:env response-key]))

(defn response-interceptor
  [{:keys [opts env params] :as in}]
  (let [{::pc/keys [mutate resolve]} opts
        response (get-response in)]
    (log/info "in response interceptor")

    (if response
      in
      (if resolve
        (assoc-response in (resolve env params))
        (assoc-response in (mutate env params))))))

(comment
  (macroexpand-1 '(go 5))
  (def i1 (i/interceptor {:enter (fn [c] (log/info "in i1") c)}))
  (def i2 (i/interceptor {:enter (fn [c] (go (log/info "in i2")) c)}))
  (def i3 (ih/before (fn [c] (log/info "in i3") c)))
  (def my-ints [i1 i2 i3])
  (chain/execute-only {} :enter my-ints)
  ;(def my-chain ())
  )

(defn interceptors->pathom-transform
  "Executes vector of interceptors on a pathom resolver or mutation.
  Each interceptor is passed a single map (the environment) which has the keys:
  :opts - The definition-time pathom resolver or mutation map of options.
  :env - The pathom connect environment for the resolver or mutation passed by pathom at request-time.
  :params - The params to the resolver or the mutation.

  Responses are set on the env like so:

  (assoc-response input-map {:my-pathom-resp :value})
  "
  [interceptors]
  (fn pathom-transform*
    [{::pc/keys [mutate resolve] :as opts}]
    (let [interceptors (conj interceptors (ih/after response-interceptor))]
      (log/info "In outer interceptors->transform" (keys opts))
      (log/info "interceptors: ")
      (pprint interceptors)
      (cond
        resolve
        (assoc opts ::pc/resolve
                    (fn [en params]
                      (log/info "In resolve params: " params)
                      (log/info "env is: ")
                      (pprint (keys en))
                      (let [respo (chain/execute {:opts opts :env en :params params} interceptors)]
                        (log/info "respo: " (keys respo))
                        respo
                        (log/info "final response: " (get-response respo))
                        (get-response respo))))

        mutate
        (assoc opts ::pc/mutate

                    (fn [en params]
                      (log/info "in mutate env keys: ")
                      (pprint (sort (keys en)))
                      (let [respo (chain/execute {:opts opts :env en :params params} interceptors)]
                        (log/info "respo: " (keys respo))
                        respo
                        (log/info "final response: " (get-response respo))
                        (get-response respo))))
        :else (throw
                (Exception.
                  (str "Attempting to use interceptor transform on a map that is not a resolve or mutate.")))))))

(defn unless-response
  [f]
  (fn unless-response* [in]
    (if (get-response in)
      in
      (f in))))
