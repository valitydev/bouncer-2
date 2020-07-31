(ns bouncer.core
  (:require [datascript.core :as ds]))

;; Datascript предлагает нам моделировать данные согласно концепции Datomic.
;; 
;; https://github.com/tonsky/datascript
;; https://docs.datomic.com/on-prem/schema.html
;; 
;; Согласно этой концпеции любой набор данных может быть представлен в виде
;; набора примитивных _фактов_, которые представляют собой кортежи
;; _сущность-атрибут-значение_. С каждой сущностью может быть связано
;; произвольное число атрибутов, от одного до бесконечности. Сущность с
;; нулём атрибутов в такой модели _не существует_, что в общем-то логично.
;; _Сущность_ в этой концпеции не более чем некий уникальный синтетический
;; идентификатор.
;; 
;; Схема — это не более чем указание, что _атрибуты_ собой представляют в
;; самом общем виде. Datascript не требует от нас исчерпывающего описания
;; схемы, исповедуя schema-on-read подход. Тем не менее, схема приведена здесь
;; для того, чтобы:
;; 1. У авторов политик было представление о данных, с которыми можно работать.
;; 2. Было проще преобразовывать входные данные в подходящий вид.
;; 3. Появилась возможность аугментировать атрибуты долонительными метаданными,
;;    например описаниями или парсерами и дефолтными значениями для входных
;;    данных.
(defn schema
  "Returns a datascript schema for a policy attribute store."
  []
  (let
   ;; Схема для сущностей, к которым можно обратиться по _имени_.
   [named-entities
    {;; Keyword `:db.unique/identity` служит для указания на то, что в базе
     ;; фактов значение этого атрибута _уникально_, то есть может быть указано
     ;; не более чем для одной сущности. Это очевидно позволяет адресовать
     ;; сущности по значению подобного атрибута.
     :name                           {:db/unique      :db.unique/identity}}

    ;; Схема для _контекстов авторизации_, по каждому из которых может быть
    ;; принятно определённое авторизационное решение.
    contexts
    {;; Уникальный идентификатор контекста.
     :context/id                     {:db/unique      :db.unique/identity}
     ;; Ссылка на сущность, описывающую детали средства авторизации.
     :context/auth                   {:db/valueType   :db.type/ref}
     ;; Ссылка на сущность, описывающую данные _пользователя_.
     :context/user                   {:db/valueType   :db.type/ref}
     ;; Ссылка на сущность, описывающую детали _осуществившего запрос
     ;; устройства_.
     :context/requester              {:db/valueType   :db.type/ref}
     ;; Ссылка на сущность, содержащую текущее состояние _окружения_.
     :context/env                    {:db/valueType   :db.type/ref}
     ;; Ссылка на сущность, описывающую детали _операции_ из домена
     ;; _Common API_.
     :context.capi/operation         {:db/valueType   :db.type/ref}}

    ;; Cхема для сущности _средства авторизации_.
    auths
    {;; Ссылка на уникальное название _метода_ авторизации.
     :auth/method                    {:db/valueType   :db.type/ref}
     ;; Атрибут для перечисления возможных названий _методов_.
     ;; Например:
     ;; - :sessionToken
     ;; - :apiKey
     ;; - ...
     ;; 
     ;; В некоторым смысле аналогичны union'ам в thrift. Строго говоря
     ;; перечислять возможные варианты методов авторизации необязтаельно.
     ;; Перечисление конечного  множества вариантов вместо указания
     ;; произвольного значения для атрибута :auth/method с одной стороны
     ;; повышает строгость проверок, а с другой — понижает гибкость системы в
     ;; целом. Кажется, что для компонента, отвечающего за политики доступа,
     ;; разумнее предпочесть первое.
     :auth.method/name               {:db/unique      :db.unique/identity}
     ;; Метка времени протухания средства авторизации.
     :auth/expiration                {}}

    ;; Cхема для сущности _пользователя_.
    users
    {;; Идентификатор пользователя.
     :user/id                        {}
     ;; Ссылка на сущность, идентифицирующую _область_ пользователя.
     :user/realm                     {:db/valueType   :db.type/ref}
     ;; Атрибут для перечисления возможных пользовательских областей.
     ;; Например:
     ;; - :external
     ;; - :staff
     :user.realm/name                {:db/unique      :db.unique/identity}
     ;;
     :user/email                     {}
     ;; Ссылка на детали _организации_, в которой состоит пользователь
     :user/org                       {:db/valueType   :db.type/ref
                                      ;; Keyword `:db.cardinality/many`
                                      ;; указывает на возможность членства
                                      ;; пользователя в более чем одной
                                      ;; организации.
                                      :db/cardinality :db.cardinality/many}}

    ;; Cхема для сущности _организации_.
    orgs
    {;; Идентификатор организации.
     :org/id                         {}
     ;; Идентификатор пользователя-владельца организации.
     :org/owner                      {}
     ;; Ссылка на детали _роли_ пользователя в организации.
     :org/role                       {:db/valueType   :db.type/ref
                                      ;; Ролей может быть много.
                                      :db/cardinality :db.cardinality/many}
     ;; Ссылка на сущность, идентифицирующую конкретную роль.
     :org.role/id                    {:db/valueType   :db.type/ref}
     ;; Атрибут для перечисления возможных названий ролей.
     ;; Например:
     ;; - administrator
     ;; - manager
     :org.role.id/name               {:db/unique      :db.unique/identity}
     ;; Указание на область действия роли в рамках организации.
     :org.role.scope/orgId           {}
     ;; Указание на область действия роли в рамках определённого магазина.
     :org.role.scope/shopId          {}}

    ;; Схема для сущности _осуществившего запрос устройства_.
    requesters
    {:requester/ip                   {}}

    ;; Схема для сущности _текущего окружения_.
    env
    {;; Текущее время.
     :env/timestamp                  {}}

    ;; Схема для сущности _операции Common API_.
    capi-operations
    {;; Ссылка на сущность, идентифицирующую конкретную операцию.
     :capi.operation/id              {:db/valueType   :db.type/ref}
     ;; Атрибут для перечисления возможных названий операций.
     ;; Например:
     ;; - createInvoice
     ;; - getShop
     :capi.operation.id/name         {:db/unique      :db.unique/identity}
     :capi.operation/invoiceId       {}
     :capi.operation/partyId         {}
     :capi.operation/shopId          {}}

    ip-addresses
    {:ip/range                       {:db/cardinality :db.cardinality/many}}]

    (merge named-entities

           contexts
           auths
           users
           orgs
           requesters
           env

           capi-operations

           ip-addresses)))

;; Базовый набор фактов, включающий схему и перечисления различных уникальных
;; названий, на которые можно будет ссылаться во входных данных и в политиках.
(defn seed-db
  "Instantiate a root datascript DB augmented with schema and populated
   with initial facts."
  []
  (let
   [auth-methods
    [{:auth.method/name :apiKey}
     {:auth.method/name :sessionToken}
     {:auth.method/name :invoiceAccessToken}]

    org-roles
    [{:org.role.id/name :administrator}
     {:org.role.id/name :manager}
     {:org.role.id/name :integrator}]

    capi-operations
    [{:capi.operation.id/name :createInvoice}
     {:capi.operation.id/name :createPayment}
     {:capi.operation.id/name :createRefund}
     {:capi.operation.id/name :createContract}
     {:capi.operation.id/name :createShop}]]

    (-> (ds/empty-db (schema))
        (ds/db-with (concat auth-methods
                            org-roles
                            capi-operations)))))
