%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%% @copyright 2011 Torbjorn Tornkvist
%% @doc A simple phone switch simulator.

-module(switch_app).

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
