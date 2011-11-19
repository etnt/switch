%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%% @copyright 2011 Torbjorn Tornkvist
%% @doc A simple phone switch simulator.

-module(hw_lib).

-export([new_switch/1
	 , new_switch/2
	 , new_subscriber/1
	 , show_switch/1
	 , start_dialtone/1
	 , stop_dialtone/1
	 , start_ringsignal/1
	 , stop_ringsignal/1
	 , start_ringtone/1
	 , stop_ringtone/1
	 , trace_off/1
	 , trace_on/1
	]).

-record(hw, {
	  switch_name = "",
	  switch_url  = "http://switch.redhoterlang.com/x/",
	  trace       = false
	 }).

trace_on(HW)  -> HW#hw{trace = true}.
trace_off(HW) -> HW#hw{trace = false}.
    

new_switch(SwitchName) when is_list(SwitchName) ->
    inets:start(),
    new(#hw{switch_name = SwitchName}).

new_switch(SwitchName, SwitchUrl) when is_list(SwitchName) andalso 
				       is_list(SwitchUrl) ->  
    inets:start(),
    new(#hw{switch_name = SwitchName,
	    switch_url  = SwitchUrl}).

new(#hw{switch_name = Name, switch_url = Url} = HW) ->
    case result(HW, http(put, Url++Name)) of
	{ok, {201,_}} -> {ok,HW};
	Else          -> Else
    end.

show_switch(#hw{switch_name = Name, switch_url = Url} = HW) ->
    case result(HW, http(get, Url++Name)) of
	{ok, {200, Body}} -> 
	    io:format("~s~n",[Body]),
	    ok;
	Else -> 
	    Else
    end.

new_subscriber(Hw) ->  tbd.
start_dialtone(Hw) ->  tbd.
stop_dialtone(Hw) ->  tbd.
start_ringsignal(Hw) ->  tbd.
stop_ringsignal(Hw) ->  tbd.
start_ringtone(Hw) ->  tbd.
stop_ringtone(Hw) ->  tbd.



http(get, Url) ->
    http:request(get, {Url,[]}, [], []);
http(Method, Url) ->
    Hdrs = [],
    Body = " ", % need 1 byte to get content-length header...grrr!!
    http:request(Method, {Url, Hdrs, "text/plain", Body}, [], []).


result(#hw{trace = false}, {ok,{{_,RC,_}, _Hdrs, Body}}) ->
    {ok, {RC, Body}};
result(#hw{trace = true}, {ok,{{_,RC,_}, _Hdrs, Body}} = Response) ->
    io:format("Response: ~p~n", [Response]),
    {ok, {RC, Body}};
result(#hw{trace = false}, Response) ->
    {error, Response};
result(#hw{trace = true}, Response) ->
    io:format("Response: ~p~n", [Response]),
    {error, Response}.
