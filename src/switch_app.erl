%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the switch application.

-module(switch_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for switch.
start(_Type, _StartArgs) ->
    switch_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for switch.
stop(_State) ->
    ok.
