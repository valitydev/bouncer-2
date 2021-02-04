-module(bouncer_arbiter_pulse).

-type beat() ::
    {judgement,
        started
        | {completed, bouncer_arbiter:judgement()}
        | {failed, _Reason}}.

-type metadata() :: #{
    ruleset => id(),
    context => bouncer_context:ctx(),
    fragments => #{id() => fragment()},
    woody_ctx => woody_context:ctx()
}.

-type id() :: binary().
-type fragment() :: #{
    type => atom(),
    context => bouncer_context:ctx(),
    metadata => map()
}.

-export_type([beat/0]).
-export_type([metadata/0]).

%%

-type handler() :: {module(), _Opts}.
-type handler(St) :: {module(), St}.
-type handlers() :: [handler()].
-type handlers(St) :: [handler(St)].

-export_type([handler/0]).
-export_type([handler/1]).
-export_type([handlers/0]).
-export_type([handlers/1]).

-callback handle_beat(beat(), metadata(), _Opts) -> ok.

-export([handle_beat/3]).

-spec handle_beat(beat(), metadata(), handlers()) -> ok.
handle_beat(Beat, Metadata, [{Mod, Opts} | Rest]) ->
    % NOTE
    % Generally, we don't want some fault to propagate from event handler to the business logic
    % and affect it, causing failure. Hovewer here we deem it required because we actually need
    % this kind of behaviour when doing audit logging, as inability to append to the audit log
    % should cause whole operation to fail.
    _ = Mod:handle_beat(Beat, Metadata, Opts),
    handle_beat(Beat, Metadata, Rest);
handle_beat(_Beat, _Metadata, []) ->
    ok.
