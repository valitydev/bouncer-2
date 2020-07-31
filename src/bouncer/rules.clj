(ns bouncer.rules
  (:require [bouncer.ip]
            [clojure.walk :refer [postwalk]]
            [datascript.core :as ds]))

(declare -rules-forbidden
         -rules-allowed)

;; Текущая модель правил предполагает, что авторы имеют возможность управлять
;; двумя наборами правил: правила явного запрета и правила явного разрешения.
;; Авторизация считается одобренной, если после применения правил нет ни одного
;; явного запрета и хотя бы одно явное разрешение.

;; Правила пишутся в виде DSL на [EDN](https://github.com/edn-format/edn).
;; Язык правил практически полностью по семантике аналогичен Datomic.
;; 
;; https://docs.datomic.com/on-prem/query.html
;; https://github.com/tonsky/datascript#differences-from-datomic
(defn forbidden?
  "Given datascript DB with some set (usually just one) of auth contexts
   tells which of them are explicitly forbidden and why."
  [db]
  (ds/q '[:find ?id (clojure.core/seq ?why)
          :in $ %
          :where
          [?ctx :context/id ?id]
          (forbidden ?ctx ?why)]
        db -rules-forbidden))

(defn allowed?
  "Given datascript DB with some set (usually just one) of auth contexts
   tells which of them are explicitly allowed and why."
  [db]
  (ds/q '[:find ?id (clojure.core/seq ?why)
          :in $ %
          :where
          [?ctx :context/id ?id]
          (allowed ?ctx ?why)]
        db -rules-allowed))

(defn thus
  "Constructs a list consisting of first arg (e.g. 'reason') and a hashmap of
   the rest of args.
   Useful when you need to state some reasoning behind a policy."
  [reason & meta]
  (identity (list reason (apply hash-map meta))))

(defn later?
  "Tells whether the first time instant is later than the second.
   Useful as a predicate in a policy."
  [inst1 inst2]
  (> (compare inst1 inst2) 0))

(defn trace
  "Make simple printout during rule execution.
   Used as a predicate, always returns true."
  [& args]
  (apply println args)
  true)

(defn prepare
  "Prepare rules by replacing shortcuts to qualified function calls."
  [& rules]
  (let [replacements
        {'thus 'bouncer.rules/thus
         'later? 'bouncer.rules/later?
         'trace 'bouncer.rules/trace
         'ip/ranges 'bouncer.ip/ranges}]
    (postwalk #(get replacements % %) (apply concat rules))))

;; Пример правил с явными запретами авторизационных контекстов.
;;
;; https://docs.datomic.com/on-prem/query.html#rules
(def ^:private -rules-forbidden
  (prepare
   ;; Каждое правило начинается со списка из символа `forbidden` и биндингов
   ;; для контекста и вывода правила. В большинстве случаев `?ctx` уже будет
   ;; связан с сущностью авторизационного контекста, а `?why` будет свободным.
   ;; 
   ;; Каждое правило заканчивается связыванием `?why` с выводом правила.
   '[[(forbidden ?ctx ?why)
      ; Свяжем `?auth `с сущностью средства авторизации.
      [?ctx :context/auth ?auth]
      ; Проверим, вдруг для средства авторизации не указан метод.
      (not [?auth :auth/method _])
      ; Это явный запрет: свяжем соответствующий вывод с `?why`.
      [(thus "Authorization is required") ?why]]

     [(forbidden ?ctx ?why)
      [?ctx :context/auth ?auth]
      [?auth :auth/expiration ?exp]
      [?ctx :context/env ?env]
      [?env :env/timestamp ?now]
      [(later? ?now ?exp)]
      [(thus "Authorization is expired" :at ?exp) ?why]]

     [(forbidden ?ctx ?why)
      ; Свяжем `?auth `с сущностью осуществляющего запрос.
      [?ctx :context/requester ?requester]
      ; Возьмём его IP-адрес, если он указан.
      ; 
      ; Важный момент: если его нет, правило не будет более вычисляться; чтобы
      ; и при его отсутствии сделать какой-либо вывод, можно либо ввести ещё
      ; одно правило, либо воспользоваться чем-то вроде `get-else`.
      [?requester :requester/ip ?ip]
      ; Продуцируем коллекцию диапазонов, в которые этот IP-адрес вообще может
      ; попасть.
      [(ip/ranges ?ip) [?range ...]]
      ; Возьмём сущность заблокированных диапазонов.
      [?blacklist :name :ipRangeBlacklist]
      ; Пересечём заблокированные диапазоны с возможными входными
      [?blacklist :ip/range ?range]
      ; Если есть пересечение, то явный запрет с указанием сработавшего
      ; диапазона.
      [(thus "Requester IP range is blacklisted" :range ?range) ?why]]]))

(def ^:private -rules-allowed
  (prepare
   ;; Правила можно выражать через другие правила.
   ;; 
   ;; Важный момент: текущий парсер правил datascript довольно лоялен к таким
   ;; ошибкам, как передача недостаточного количества аргументов или доступ к
   ;; атрибутам, не объявленным в схеме. Хорошая новость с другой стороны в том,
   ;; что дополнительный валидатор на входе поставить кажется не представляется
   ;; слишком сложным.
   '[[(allowed ?ctx ?why)
      ; [(trace :allowed ?ctx)]
      [?ctx :context/user ?user]
      [?user :user/org ?org]
      (org-by-op ?ctx ?org ?op)
      (org-allowed ?ctx ?user ?org ?op ?why)]

     [(org-allowed ?ctx ?user ?org ?op ?why)
      ; [(trace :org-allowed ?ctx ?user ?org ?op)]
      ; Пересечём идентификатор пользователя...
      [?user :user/id ?id]
      ; ...с идентификатором владельца.
      [?org :org/owner ?id]
      ; Есть пересечение: значит можно.
      [(thus "User is the organisation owner itself") ?why]]

     [(org-allowed ?ctx ?user ?org ?op ?why)
      ; [(trace :org-allowed ?ctx ?user ?org ?op)]
      [?org :org/role ?role]
      (op-role-allowed ?op ?role)
      (role-scope-allowed ?op ?role)
      [?role :org.role/id ?rid]
      [?rid :org.role.id/name ?rolename]
      [(thus "Operation permitted to the user with role" :role ?rolename) ?why]]

     ; Правило, соотносящее организацию с операцией над ней.
     [(org-by-op ?ctx ?org ?op)
      ; [(trace :org-by-op ?ctx ?org)]
      [?ctx :context.capi/operation ?op]
      [?org :org/id ?id]
      [?op :capi.operation/partyId ?id]]

     [(op-role-allowed ?op ?role)
      ; [(trace :op-role-allowed ?op ?role)]
      [?role :org.role/id [:org.role.id/name :manager]]
      (or [?op :capi.operation/id [:capi.operation.id/name :createInvoice]]
          [?op :capi.operation/id [:capi.operation.id/name :createPayment]])]
     [(op-role-allowed ?op ?role)
      ; [(trace :op-role-allowed ?op ?role)]
      [?role :org.role/id [:org.role.id/name :administrator]]
      (or [?op :capi.operation/id [:capi.operation.id/name :createContract]]
          [?op :capi.operation/id [:capi.operation.id/name :createShop]]
          [?op :capi.operation/id [:capi.operation.id/name :createInvoice]]
          [?op :capi.operation/id [:capi.operation.id/name :createPayment]]
          [?op :capi.operation/id [:capi.operation.id/name :createRefund]])]     

     [(role-scope-allowed ?op ?role)
      ; [(trace :role-scope-allowed ?op ?role)]
      [?role :org.role.scope/orgId ?org]
      [?op :capi.operation/partyId ?org]]
     [(role-scope-allowed ?op ?role)
      ; [(trace :role-scope-allowed ?op ?role)]
      [?role :org.role.scope/shopId ?shop]
      [?op :capi.operation/shopId ?shop]]]))
