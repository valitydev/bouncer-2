(ns bouncer.main
  (:require [bouncer.core :as core]
            [bouncer.ip :as ip]
            [bouncer.rules :as rules]
            [datascript.core :as ds]
            [criterium.core :as criterium]
            [clj-async-profiler.core :as prof])
  (:gen-class))

;; Пример _одного_ авторизационного контекста.
(def -ctx-sample
  {:context/id 42

   :context/auth
   {:auth/method               [:auth.method/name :sessionToken]
    :auth/expiration           #inst "2020-02-02T00:00:00Z"}

   :context/user
   {:user/id                   "ownr"
    :user/email                "r.bykaev@example.org"
    :user/org
    [{:org/id                    "ZORG"
      :org/owner                 "ownr"
      :org/role
      [{:org.role/id               [:org.role.id/name :manager]
        :org.role.scope/shopId     "ZHOP"}
       {:org.role/id               [:org.role.id/name :manager]
        :org.role.scope/shopId     "ZHEP"}
       {:org.role/id               [:org.role.id/name :administrator]}]}
     {:org/id                    "ZERG"
      :org/owner                 "zerg"
      :org/role
      [{:org.role/id               [:org.role.id/name :manager]
        :org.role.scope/shopId     "SHOP"}]}]}

   :context.capi/operation
   {:capi.operation/id         [:capi.operation.id/name :createInvoice]
    :capi.operation/invoiceId  "BLARG"
    :capi.operation/shopId     "ZHOP"
    :capi.operation/partyId    "ZORG"}

   :context/requester
   {:requester/ip              (ip/String->Address "172.16.16.172")}

   :context/env
   {:env/timestamp             #inst "2020-02-02T12:34:56Z"}})

(defn- rand-range []
  (ip/String->Range
   (str (rand-int 256)
        \. (rand-int 256)
        \. (rand-int 256)
        \. (rand-int 256)
        \/ (+ 16 (rand-int 16)))))

(def -ip-range-blacklist
  {:name     :ipRangeBlacklist
   :ip/range (concat (map ip/String->Range
                          ["10.0.0.0/8"
                           "172.16.0.0/12"
                           "192.168.0.0/16"
                           "127.0.0.0/24"
                           "1.1.1.1/32"
                           "fc00::/64"])
                     (take 100
                           (repeatedly rand-range)))})

;; Собираем базу фактов из:
;; - базового набора,
;; - блэклиста диапазонов IP-адресов,
;; - одного авторизационного контекста.
(def -db-sample (-> (core/seed-db)
                    (ds/db-with [-ip-range-blacklist
                                 -ctx-sample])))

(defn -main
  [& _]
  ;; Выдаёт что-то типа:
  ;; ([42
  ;; (("Requester IP range is blacklisted" {:range 172.16.0.0/12})
  ;;  ("Authorization is expired" {:at #inst "2020-02-02T00:00:00.000-00:00"}))])
  (println (rules/forbidden? -db-sample))
  ;; Выдаёт что-то типа:
  ;; ([42
  ;; (("Operation permitted to the user with role" {:role :manager})
  ;;  ("User is the organisation owner itself" {}))])
  (println (rules/allowed? -db-sample))
  (flush))

#_(criterium/with-progress-reporting
    (criterium/quick-bench (rules/allowed? -db-sample) :verbose))

#_(do
  (prof/profile (dotimes [_ 100] (rules/allowed? -db-sample))))
