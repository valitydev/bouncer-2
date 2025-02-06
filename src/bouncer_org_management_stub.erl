%% TODO delete after org_management is done
-module(bouncer_org_management_stub).

-include_lib("bouncer_proto/include/bouncer_ctx_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_ctx_v1_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_base_thrift.hrl").

-behaviour(woody_server_thrift_handler).

-export([handle_function/4]).

-define(THRIFT_TYPE,
    {struct, struct, {bouncer_ctx_v1_thrift, 'ContextFragment'}}
).

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody_state:st()) ->
    {ok, woody:result()}.
handle_function('GetUserContext', {UserID}, _WoodyCtx, _Opts) ->
    ContextFragmentV1 = #ctx_v1_ContextFragment{
        user = #ctx_v1_User{
            id = UserID,
            orgs = [
                #ctx_v1_Organization{
                    id = UserID,
                    owner = #base_Entity{id = UserID},
                    party = #base_Entity{id = UserID}
                }
            ]
        }
    },
    {ok, #ctx_ContextFragment{
        type = v1_thrift_binary,
        content = encode_context_fragment(ContextFragmentV1)
    }}.

encode_context_fragment(ContextFragment) ->
    Codec = thrift_strict_binary_codec:new(),
    {ok, Codec1} = thrift_strict_binary_codec:write(Codec, ?THRIFT_TYPE, ContextFragment),
    thrift_strict_binary_codec:close(Codec1).
