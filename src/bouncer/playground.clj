(ns bouncer.playground
  (:require [bouncer.ip :as ip]
            [datascript.core :as ds]
            [datascript.query :as dq]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [criterium.core :as crit]))

(def ^:private -schema-named-entities
  {:name                                {:db/unique      :db.unique/identity}})

(def -schema-context
  {:context                             {:db/unique      :db.unique/identity}
   :auth                                {:db/valueType   :db.type/ref}
   :user                                {:db/valueType   :db.type/ref}
   :org                                 {:db/valueType   :db.type/ref}
   :capi.operation                      {:db/valueType   :db.type/ref}
   :requester                           {:db/valueType   :db.type/ref}
   :env                                 {:db/valueType   :db.type/ref}})

(def -schema-policy-attributes
  {:authMethod                          {:db/unique      :db.unique/identity}
   :auth/method                         {:db/valueType   :db.type/ref}
   :auth/expiration                     {}

   :user/id                             {}
   :user/realm                          {}
   :user/email                          {}

   :org/id                              {}
   :org/owner                           {}
   :org/role                            {:db/valueType   :db.type/ref
                                         :db/cardinality :db.cardinality/many}

   :orgRole                             {:db/unique      :db.unique/identity}
   :org.role/id                         {:db/valueType   :db.type/ref}
   :org.role.scope/shopId               {}

   :capiOperation                       {:db/unique      :db.unique/identity}
   :capi.operation/id                   {:db/valueType   :db.type/ref}
   :capi.operation/invoiceId            {}
   :capi.operation/partyId              {}
   :capi.operation/shopId               {}

   :requester/ip                        {}

   :env/timestamp                       {}})

(def -schema-ip-addresses
  {:ip/address                       {:db/cardinality :db.cardinality/one}
   :ip/range                         {:db/cardinality :db.cardinality/many}})

(def -auth-methods
  [{:authMethod :apiKey}
   {:authMethod :sessionToken}
   {:authMethod :invoiceAccessToken}])

(def -capi-operations
  [{:capiOperation :createInvoice}
   {:capiOperation :createPayment}])

(def -org-roles
  [{:orgRole :administrator}
   {:orgRole :manager}
   {:orgRole :integrator}])

(defn- rand-range []
  (ip/String->Range
   (str (rand-int 256)
        \. (rand-int 256)
        \. (rand-int 256)
        \. (rand-int 256)
        \/ (+ 16 (rand-int 16)))))

(def -ip-range-blacklist
  [{:name     :ipRangeBlacklist
    :ip/range (concat (map ip/String->Range
                           ["10.0.0.0/8"
                            "172.16.0.0/12"
                            "192.168.0.0/16"
                            "127.0.0.0/24"
                            "1.1.1.1/32"
                            "fc00::/64"])
                      (take 100
                            (repeatedly rand-range)))}])

(defn find-blacklist-ranges [db ip]
  (ds/q '[:find ?range .
          :in $ [?range ...]
          :where
          [?bl :name :ipRangeBlacklist]
          [?bl :ip/range ?range]]
        db (ip/ranges (ip/String->Address ip))))

;; (defn find-blacklist-ranges [db ip]
;;   (ds/q '[:find ?range .
;;           :in $ ?ip
;;           :where
;;           [(ip/ranges ?ip) [?range ...]]
;;           [?bl :name :ipRangeBlacklist]
;;           [?bl :ip/range ?range]]
;;         db (ip/String->Address ip)))

(def policies-db
  (-> (ds/empty-db
       (merge
        -schema-named-entities
        -schema-context
        -schema-policy-attributes
        -schema-ip-addresses))
      (ds/db-with
       (concat
        -auth-methods
        -org-roles
        -capi-operations
        -ip-range-blacklist
        [{:context
          42

          :auth
          {:auth/method               [:authMethod :sessionToken]
           :auth/expiration           #inst "2020-02-02T00:00:00Z"}

          :user
          {:user/id                   "knfwks"
           :user/email                "r.bykaev@example.org"}

          :org
          {:org/id                    "ZORG"
           :org/owner                 "ownr"
           :org/role
           [{:org.role/id               [:orgRole :integrator]
             :org.role.scope/shopId     "SHIP"}
            {:org.role/id               [:orgRole :manager]
             :org.role.scope/shopId     "ZHOP"}
            {:org.role/id               [:orgRole :manager]
             :org.role.scope/shopId     "ZHEP"}
            {:org.role/id               [:orgRole :administrator]}]}

          :capi.operation
          {:capi.operation/id         [:capiOperation :createInvoice]
           :capi.operation/invoiceId  "BLARG"
           :capi.operation/shopId     "ZHOP"
           :capi.operation/partyId    ""}

          :requester
          {:requester/ip              (ip/String->Address "172.16.16.172")}

          :env
          {:env/timestamp             #inst "2020-02-02T12:34:56Z"}}]))))

#_(time (find-blacklist-ranges policies-db "10.1.1.1"))

(defn thus
  [reason & meta]
  (identity [reason (apply hash-map meta)]))

(defn later?
  [d1 d2]
  (> (compare d1 d2) 0))

#_(later? #inst "2020-02-02T12:34:56Z" #inst "2020-02-02T12:34:56Z")

(defn- prepare-rules [& rules]
  (let [replacements {'thus 'bouncer.playground/thus
                      'later? 'bouncer.playground/later?}]
    (walk/postwalk #(get replacements % %) (apply concat rules))))

(def forbidden
  (prepare-rules
   '[[(forbidden ?ctx ?why)
      [?ctx :auth ?auth]
      (not [?auth :auth/method _])
      [(thus "Authorization is required") ?why]]

     [(forbidden ?ctx ?why)
      [?ctx :auth ?auth]
      [?auth :auth/expiration ?exp]
      [?ctx :env ?env]
      [?env :env/timestamp ?now]
      [(later? ?now ?exp)]
      [(thus "Authorization is expired"
             :at ?exp) ?why]]

     [(forbidden ?ctx ?why)
      [?ctx :requester ?requester]
      [?requester :requester/ip ?ip]
      [(ip/ranges ?ip) [?range ...]]
      [?blacklist :name :ipRangeBlacklist]
      [?blacklist :ip/range ?range]
      [(thus "Requester IP range is blacklisted"
             :range ?range) ?why]]]))

(def allowed
  (prepare-rules
   '[[(allowed ?ctx ?why)
      [?ctx :user ?user]
      [?user :user/id ?id]
      [?ctx :org ?org]
      [?org :org/owner ?id]
      [(thus "User is the organisation owner itself") ?why]]

     [(allowed ?ctx ?why)
      [?ctx :org ?org]
      [?ctx :capi.operation ?op]
      (or [?op :capi.operation/id [:capiOperation :createInvoice]]
          [?op :capi.operation/id [:capiOperation :createPayment]])
      [?op :capi.operation/shopId ?shop]
      [?org :org/role ?role]
      [?role :org.role/id [:orgRole :manager]]
      [?role :org.role.scope/shopId ?shop]
      [(thus "Operation permitted to the user with a manager's role") ?why]]]

   (doall (take 100 (repeat
                     '[(allowed ?ctx ?why)
                       [?ctx :env ?env]
                       [?env :env/timestamp ?ts]
                       [(nil? ?ts)]
                       [(thus "Just to see where it goes") ?why]])))))

;; (def allowed
;;   (prepare-rules
;;    '[[(allowed ?why)
;;       [_ :user/id ?id]
;;       [_ :org/owner ?id]
;;       [(thus "User is the organisation owner itself" :id ?id) ?why]]

;;      [(allowed ?why)
;;       (or [?op :capi.operation/id [:capiOperation :createInvoice]]
;;           [?op :capi.operation/id [:capiOperation :createPayment]])
;;       [?op :capi.operation/shopId ?shop]
;;       [_ :org/role ?role]
;;       [?role :org.role/id [:orgRole :manager]]
;;       [?role :org.role.scope/shopId ?shop]
;;       [(thus "Operation permitted to the user with a manager's role"
;;              :shop ?shop) ?why]]]

;;    (doall (take 100 (repeat
;;                      '[(allowed ?why)
;;                        [_ :env/timestamp ?ts]
;;                        [(thus "Just to see where it goes" :ts ?ts) ?why]])))))

(defn forbidden? [db]
  (ds/q '[:find ?ctx ?why
          :in $ %
          :where [?ctx :context] (forbidden ?ctx ?why)]
        db forbidden))

#_(time (forbidden? policies-db))
#_(crit/quick-bench (forbidden? policies-db))

;; (defn allowed? [db]
;;   (ds/q '[:find [?why ...]
;;           :in $ %
;;           :where [?ctx :name :context] (allowed ?ctx ?why)]
;;         db allowed))

(defn allowed? [db]
  (ds/q '[:find [?ctx ?why]
          :in $ %
          :where [?ctx :context] (allowed ?ctx ?why)]
        db allowed))

#_(allowed? policies-db)
#_(crit/quick-bench (allowed? policies-db))

(defn- attr [db attr]
  (-> (ds/datoms db :aevt attr)
      (first)
      (:v)))

(defn- attr-values [db attr]
  (->> (ds/datoms db :aevt attr)
       (map #(:v %))))

(defn- attr-entities [db attr]
  (->> (ds/datoms db :aevt attr)
       (map #(:e %))))

#_(crit/quick-bench (attr policies-db :capi.operation))

#_(crit/quick-bench
   (let [ctx (-> (attr-entities policies-db :context)
                 (first)
                 (as-> e (ds/pull policies-db '[*] e))
                 (dissoc :db/id))
         shops (concat ["SHIP" "ZHOP" "ZHEP"]
                       (mapv str (range 10)))
         ctxs (map #(assoc ctx
                           :context
                           %1
                           :capi.operation
                           {:capi.operation/id [:capiOperation :createInvoice]
                            :capi.operation/shopId %2
                            :capi.operation/partyId "BLARG"})
                   (range 43 1000)
                   shops)
         db (ds/db-with policies-db ctxs)]

     ;; (ds/q '[:find ?id (clojure.core/identity ?why)
     (ds/q '[:find ?id ?why
             :in $ %
             :where
             [?ctx :context ?id]
             (allowed ?ctx ?why)]
           db allowed)))

;; (time (ds/q '[:find [?allowed ...]
;;               :in $ [?shop ...] %
;;               :where [?ctx :name :context] (allowed-in-shop ?ctx ?shop ?allowed)]
;;             policies-db
;;             (concat ["SHIP" "ZHOP" "ZHEP"]
;;                     (mapv str (range 10)))
;;             allowed))

;;;

;; (defn defrule
;;   [why clauses]
;;   (concat (list 'and)
;;           clauses
;;           (list [(list 'ground why) '?why])))

;; (defn allow? [db]
;;   (let [rules (list 'or-join []
;;                     (defrule "WHY" '[[(identity false)]])
;;                     (defrule "LOL" '[[?op :capi.operation/id _]
;;                                      [?op :capi.operation.invoice/id ?id]
;;                                      [(clojure.string/blank? ?id)]])
;;                     (defrule "YEAH" '[]))]
;;     (ds/q [:find '[?why ...]
;;            :in '$
;;            :where rules]
;;           db)))

;; (defn allow? [db]
;;   (ds/q '[:find [?why ...]
;;           :in $
;;           :where (or-join [?why]
;;                           (and [(identity false)]
;;                                [(ground "WHY") ?why])
;;                           (and [(ground "YEAH") ?why]))]
;;         db))

;; (defn allow? [db]
;;   (ds/q '[:find [?why ...]
;;           :in $
;;           :where (or
;;                   (and [(identity false)]
;;                        [(ground "WHY") ?why])
;;                   (and [(ground "YEAH") ?why]))]
;;         db))

;; (def -rules-forbid?
;;   '[])

;; Experimenting w/ recursion

(def cycle-db
  (-> (ds/empty-db {:node {:db/unique :db.unique/identity}
                    :edge {:db/unique :db.unique/identity}
                    :in   {:db/valueType :db.type/ref}
                    :out  {:db/valueType :db.type/ref}})
      (ds/db-with  [{:node :a}
                    {:node :b}
                    {:node :c}
                    {:node :d}
                    {:node :e}
                    {:node :f}
                    {:edge 1 :in [:node :a] :out [:node :b]}
                    {:edge 2 :in [:node :b] :out [:node :c]}
                    {:edge 3 :in [:node :c] :out [:node :d]}
                    {:edge 4 :in [:node :d] :out [:node :c]}
                    {:edge 5 :in [:node :c] :out [:node :a]}
                    {:edge 6 :in [:node :d] :out [:node :a]}
                    {:edge 7 :in [:node :a] :out [:node :d]}
                    {:edge 8 :in [:node :f] :out [:node :f]}])))

(def cycle-db-inline
  (-> (ds/empty-db {:node {:db/unique :db.unique/identity}
                    :edge {:db/cardinality :db.cardinality/many
                           :db/valueType :db.type/ref}})
      (ds/db-with  [{:db/id 1 :node :a :edge [2 4]}
                    {:db/id 2 :node :b :edge [3]}
                    {:db/id 3 :node :c :edge [4 1]}
                    {:db/id 4 :node :d :edge [3 1]}])))

(defn enumerate-cycles [db node]
  (let [walk (ds/q '[:find (pull ?n [:db/id :node {:edge ...}]) .
                     :in $ ?node
                     :where [?n :node ?node]]
                   db node)
        edges   (:edge walk)
        acc-fn  (fn acc-fn [es acc path]
                  (reduce (fn [acc n]
                            (if (= (:node n) node)
                              (conj acc (conj path node))
                              (if-let [es (:edge n)]
                                (acc-fn es acc (conj path (:node n)))
                                acc)))
                          acc
                          es))]
    (acc-fn edges [] [node])))

(defn not-visited? [e path]
  (not-any? (partial = e) path))

(defn concat-rules
  [& rs]
  (vec (apply concat rs)))

(def rules-empty
  '[])

(def rules-node-path
  '[[(path ?ni ?nj ?r)
     ((vector ?ni) ?path)
     (path1 ?ni ?nj ?path ?r)]
    [(path1 ?ni ?nj ?path ?r)
     [?e :in ?ni]
     [?e :out ?nj]
     ((clojure.core/conj ?path ?nj) ?r)]
    [(path1 ?ni ?nj ?path ?r)
     [?e :in ?ni]
     [?e :out ?nk]
     [(!= ?nk ?nj)]
     [(bouncer.playground/not-visited? ?nk ?path)]
     ((clojure.core/conj ?path ?nk) ?path1)
     (path1 ?nk ?nj ?path1 ?r)]])

(defn find-path [db n1 n2]
  (->> (ds/q '[:find [?path ...]
               :in $ ?node1 ?node2 %
               :where
               [?n1 :node ?node1]
               [?n2 :node ?node2]
               (path ?n1 ?n2 ?path)]
             db n1 n2 rules-node-path)
       (map #(ds/pull-many db [:node] %))))

(defn find-answer [db node]
  (ds/q '[:find [?answer ...]
          :in $ ?node %
          :where
          [?n :node ?node]
          (answer ?n ?answer)]
        db node
        '[[(answer ?n ?answer)
           [?e :in ?n]
           [?e :edge _]
           [(ground "BLARG") ?answer]]
          [(answer ?n ?answer)
           [?e :out ?n]
           [?e :edge _]
           [(ground "BLÓRG") ?answer]]
          [(answer ?n ?answer)
           [?e :in ?n]
           [?e :out ?n]
           [?e :edge _]
           [(ground "BL↑RG") ?answer]]]))

(defn find-answer [db]
  (ds/q '[:find [?node ...]
          :in $ %
          :where
          [?n :node ?node]
          [(ground "BL↑RG") ?answer]
          (answer ?n ?answer)]
        db
        '[[(answer ?n ?answer)
           [?e :in ?n]
           [?e :edge _]
           [(ground "BLARG") ?answer]]
          [(answer ?n ?answer)
           [?e :out ?n]
           [?e :edge _]
           [(ground "BLÓRG") ?answer]]
          [(answer ?n ?answer)
           [?e :in ?n]
           [?e :out ?n]
           [?e :edge _]
           [(ground "BL↑RG") ?answer]]]))

(time (find-answer cycle-db))

(defn enumerate-w-rule [db n]
  (ds/q '[:find ?e
          :in $ ?n %
          :where
          (enumerate ?n ?e)]
        db n
        '[[(enumerate ?n ?r)
           (enumerate1 ?n [] ?r)]
          [(enumerate1 ?n ?acc ?r)
           [(= ?n 0)]
           [(identity ?acc) ?r]]
          [(enumerate1 ?n ?acc ?r)
           [(> ?n 0)]
           [(dec ?n) ?n1]
           [(clojure.core/conj ?acc ?n) ?acc1]
           (enumerate1 ?n1 ?acc1 ?r)]]))
