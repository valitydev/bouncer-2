# Bouncer

> Does someone look like a troublemaker?

Primary [Arbiter](https://github.com/rbkmoney/bouncer-proto/blob/97dcad6f/proto/decisions.thrift#L42) thrift service implementation.

In a nutshell this service maps incoming contexts into [OPA input documents](https://www.openpolicyagent.org/docs/latest/philosophy/#the-opa-document-model) and asks OPA to compute a judgement allowing, restricting or forbidding actions under given input context.

From the service's point of view a **ruleset id** is a path to OPA document that define a subdocument named `judgement` with a rudimentary schema. See https://github.com/rbkmoney/bouncer-policies#authoring for more detailed information.

## Things to keep in mind

* When upgrading service to a newer Erlang OTP release please be aware of potential breakages in `bouncer_audit_log` not strictly related to usual API deprecations and removals which is an essintial part of a release. That is because this module relies on some OTP libraries' internal implementation details. See module-level notes for additional details.
