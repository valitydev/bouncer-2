-module(ct_gun_event_h).
-behavior(gun_event).

-export([init/2]).
-export([domain_lookup_start/2]).
-export([domain_lookup_end/2]).
-export([connect_start/2]).
-export([connect_end/2]).
-export([tls_handshake_start/2]).
-export([tls_handshake_end/2]).
-export([request_start/2]).
-export([request_headers/2]).
-export([request_end/2]).
-export([push_promise_start/2]).
-export([push_promise_end/2]).
-export([response_start/2]).
-export([response_inform/2]).
-export([response_headers/2]).
-export([response_trailers/2]).
-export([response_end/2]).
-export([ws_upgrade/2]).
-export([ws_recv_frame_start/2]).
-export([ws_recv_frame_header/2]).
-export([ws_recv_frame_end/2]).
-export([ws_send_frame_start/2]).
-export([ws_send_frame_end/2]).
-export([protocol_changed/2]).
-export([transport_changed/2]).
-export([origin_changed/2]).
-export([cancel/2]).
-export([disconnect/2]).
-export([terminate/2]).

-type st() :: _.

-spec init(gun_event:init_event(), st()) ->
    st().
init(Event, State) ->
    _ = ct:pal("~p [gun] init: ~p", [self(), Event]),
    State.

-spec domain_lookup_start(gun_event:domain_lookup_event(), st()) ->
    st().
domain_lookup_start(Event, State) ->
    _ = ct:pal("~p [gun] domain lookup start: ~p", [self(), Event]),
    State.

-spec domain_lookup_end(gun_event:domain_lookup_event(), st()) ->
    st().
domain_lookup_end(Event, State) ->
    _ = ct:pal("~p [gun] domain lookup end: ~p", [self(), Event]),
    State.

-spec connect_start(gun_event:connect_event(), st()) ->
    st().
connect_start(Event, State) ->
    _ = ct:pal("~p [gun] connect start: ~p", [self(), Event]),
    State.

-spec connect_end(gun_event:connect_event(), st()) ->
    st().
connect_end(Event, State) ->
    _ = ct:pal("~p [gun] connect end: ~p", [self(), Event]),
    State.

-spec tls_handshake_start(gun_event:tls_handshake_event(), st()) ->
    st().
tls_handshake_start(Event, State) ->
    _ = ct:pal("~p [gun] tls handshake start: ~p", [self(), Event]),
    State.

-spec tls_handshake_end(gun_event:tls_handshake_event(), st()) ->
    st().
tls_handshake_end(Event, State) ->
    _ = ct:pal("~p [gun] tls handshake end: ~p", [self(), Event]),
    State.

-spec request_start(gun_event:request_start_event(), st()) ->
    st().
request_start(Event, State) ->
    _ = ct:pal("~p [gun] request start: ~p", [self(), Event]),
    State.

-spec request_headers(gun_event:request_start_event(), st()) ->
    st().
request_headers(Event, State) ->
    _ = ct:pal("~p [gun] request headers: ~p", [self(), Event]),
    State.

-spec request_end(gun_event:request_end_event(), st()) ->
    st().
request_end(Event, State) ->
    _ = ct:pal("~p [gun] request end: ~p", [self(), Event]),
    State.

-spec push_promise_start(gun_event:push_promise_start_event(), st()) ->
    st().
push_promise_start(Event, State) ->
    _ = ct:pal("~p [gun] push promise start: ~p", [self(), Event]),
    State.

-spec push_promise_end(gun_event:push_promise_end_event(), st()) ->
    st().
push_promise_end(Event, State) ->
    _ = ct:pal("~p [gun] push promise end: ~p", [self(), Event]),
    State.

-spec response_start(gun_event:response_start_event(), st()) ->
    st().
response_start(Event, State) ->
    _ = ct:pal("~p [gun] response start: ~p", [self(), Event]),
    State.

-spec response_inform(gun_event:response_headers_event(), st()) ->
    st().
response_inform(Event, State) ->
    _ = ct:pal("~p [gun] response inform: ~p", [self(), Event]),
    State.

-spec response_headers(gun_event:response_headers_event(), st()) ->
    st().
response_headers(Event, State) ->
    _ = ct:pal("~p [gun] response headers: ~p", [self(), Event]),
    State.

-spec response_trailers(gun_event:response_trailers_event(), st()) ->
    st().
response_trailers(Event, State) ->
    _ = ct:pal("~p [gun] response trailers: ~p", [self(), Event]),
    State.

-spec response_end(gun_event:response_end_event(), st()) ->
    st().
response_end(Event, State) ->
    _ = ct:pal("~p [gun] response end: ~p", [self(), Event]),
    State.

-spec ws_upgrade(gun_event:ws_upgrade_event(), st()) ->
    st().
ws_upgrade(Event, State) ->
    _ = ct:pal("~p [gun] ws upgrade: ~p", [self(), Event]),
    State.

-spec ws_recv_frame_start(gun_event:ws_recv_frame_start_event(), st()) ->
    st().
ws_recv_frame_start(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame start: ~p", [self(), Event]),
    State.

-spec ws_recv_frame_header(gun_event:ws_recv_frame_header_event(), st()) ->
    st().
ws_recv_frame_header(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame header: ~p", [self(), Event]),
    State.

-spec ws_recv_frame_end(gun_event:ws_recv_frame_end_event(), st()) ->
    st().
ws_recv_frame_end(Event, State) ->
    _ = ct:pal("~p [gun] ws recv frame end: ~p", [self(), Event]),
    State.

-spec ws_send_frame_start(gun_event:ws_send_frame_event(), st()) ->
    st().
ws_send_frame_start(Event, State) ->
    _ = ct:pal("~p [gun] ws send frame start: ~p", [self(), Event]),
    State.

-spec ws_send_frame_end(gun_event:ws_send_frame_event(), st()) ->
    st().
ws_send_frame_end(Event, State) ->
    _ = ct:pal("~p [gun] ws send frame end: ~p", [self(), Event]),
    State.

-spec protocol_changed(gun_event:protocol_changed_event(), st()) ->
    st().
protocol_changed(Event, State) ->
    _ = ct:pal("~p [gun] protocol changed: ~p", [self(), Event]),
    State.

-spec transport_changed(gun_event:transport_changed_event(), st()) ->
    st().
transport_changed(Event, State) ->
    _ = ct:pal("~p [gun] transport changed: ~p", [self(), Event]),
    State.

-spec origin_changed(gun_event:origin_changed_event(), st()) ->
    st().
origin_changed(Event, State) ->
    _ = ct:pal("~p [gun] origin changed: ~p", [self(), Event]),
    State.

-spec cancel(gun_event:cancel_event(), st()) ->
    st().
cancel(Event, State) ->
    _ = ct:pal("~p [gun] cancel: ~p", [self(), Event]),
    State.

-spec disconnect(gun_event:disconnect_event(), st()) ->
    st().
disconnect(Event, State) ->
    _ = ct:pal("~p [gun] disconnect: ~p", [self(), Event]),
    State.

-spec terminate(gun_event:terminate_event(), st()) ->
    st().
terminate(Event, State) ->
    _ = ct:pal("~p [gun] terminate: ~p", [self(), Event]),
    State.
