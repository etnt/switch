%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%% @copyright 2011 Torbjorn Tornkvist
%% @doc A simple phone switch simulator.

-module(create_subscriber_resource).
-export([init/1
         , allowed_methods/2
         , content_types_accepted/2
         , content_types_provided/2
         , malformed_request/2
	 , process_post/2
         , to_text/2
        ]).

-import(switch, [to_binary/1]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
          method,
          switch,
	  data
         }).
          

init([]) ->
    {ok, #ctx{}}.
%%    {{trace, "/tmp"}, #ctx{}}. % when debugging!

%%
allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State#ctx{method = wrq:method(ReqData)}}.

%%
content_types_accepted(ReqData, State) ->
      {[{"text/plain", to_text}], ReqData, State}.

%%
content_types_provided(ReqData, State) ->
      {[{"text/plain", to_text}], ReqData, State}.

to_text(ReqData, #ctx{method = 'POST', data = Data} = State) ->
    {Data, ReqData, State}.

%%
process_post(ReqData, #ctx{switch = SwitchName} = State) ->
    {ok, Ano} = group_switch:create_subscriber(SwitchName),
    Data      = to_binary(Ano),
    {true,
     wrq:set_resp_body(Data, ReqData),
     State#ctx{data = Data}}.

%%
malformed_request(ReqData, #ctx{method = 'POST'} = State) ->
    SwitchName = wrq:path_info(switch_name, ReqData),
    case whereis(list_to_atom(SwitchName)) of
        Pid when is_pid(Pid) ->
            {false, ReqData, State#ctx{switch = SwitchName}};
        _  ->
            {true, ReqData, State}
    end;

malformed_request(ReqData, State) ->
    {false, ReqData, State}.





