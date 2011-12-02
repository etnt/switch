%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%% @copyright 2011 Torbjorn Tornkvist
%% @doc A simple phone switch simulator.

-module(switch_resource).
-export([init/1
         , allowed_methods/2
         , content_types_accepted/2
         , content_types_provided/2
         , malformed_request/2
         , resource_exists/2
         , to_text/2
        ]).

-ignore_xref([init/1
              , allowed_methods/2
              , content_types_accepted/2
              , content_types_provided/2
              , malformed_request/2
              , resource_exists/2
              , to_text/2
              , ping/2
             ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
          method,
          switch
         }).
          

init([]) ->
    {ok, #ctx{}}.
%  {{trace, "/tmp"}, #ct{}. % when debugging!

%%
allowed_methods(ReqData, State) ->
    {['GET','PUT'], ReqData, State#ctx{method = wrq:method(ReqData)}}.

%%
content_types_accepted(ReqData, State) ->
      {[{"text/plain", to_text}], ReqData, State}.

%%
content_types_provided(ReqData, State) ->
      {[{"text/plain", to_text}], ReqData, State}.

%%
to_text(ReqData, #ctx{method = 'GET', switch = SwitchName} = State) ->
    {ok, Status} = group_switch:status(SwitchName),
    {Status,
     ReqData,
     State};

to_text(ReqData, #ctx{method = 'PUT', switch = SwitchName} = State) ->
    Path = "/x/"++SwitchName,
    {{halt,201},
     wrq:set_resp_header("Location", Path, ReqData),
     State}.

%%
resource_exists(ReqData, #ctx{method = 'GET'} = State) ->
    SwitchName = wrq:path_info(switch_name, ReqData),
    case whereis(list_to_atom(SwitchName)) of
        Pid when is_pid(Pid) -> {true, ReqData, State#ctx{switch = SwitchName}};
        _                    -> {false, ReqData, State}
    end;

resource_exists(ReqData, State) ->
    {true, ReqData, State}.

%%
malformed_request(ReqData, #ctx{method = 'PUT'} = State) ->
    SwitchName = wrq:path_info(switch_name, ReqData),
    case whereis(list_to_atom(SwitchName)) of
        Pid when is_pid(Pid) ->
            {true, ReqData, State#ctx{switch = SwitchName}};
        _  ->
            group_switch:start(SwitchName),
            {false, ReqData, State#ctx{switch = SwitchName}}
    end;

malformed_request(ReqData, State) ->
    {false, ReqData, State}.

