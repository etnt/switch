%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%% @copyright 2011 Torbjorn Tornkvist
%% @doc API to the simple phone switch simulator.

-module(hw_lib).

-export([new_switch/1
	 , new_switch/2
	 , new_subscriber/1
	 , connect/3
	 , disconnect/3
	 , offhook/2
	 , onhook/2
	 , reset/2
	 , show_switch/1
	 , start_busytone/2
	 , stop_busytone/2
	 , start_dialtone/2
	 , stop_dialtone/2
	 , start_ringsignal/2
	 , stop_ringsignal/2
	 , start_ringtone/2
	 , stop_ringtone/2
	 , trace_off/1
	 , trace_on/1
	]).

-ignore_xref([new_switch/1
              , new_switch/2
              , new_subscriber/1
              , connect/3
              , disconnect/3
              , offhook/2
              , onhook/2
              , reset/2
              , show_switch/1
              , start_busytone/2
              , stop_busytone/2
              , start_dialtone/2
              , stop_dialtone/2
              , start_ringsignal/2
              , stop_ringsignal/2
              , start_ringtone/2
              , stop_ringtone/2
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

new_subscriber(#hw{switch_name = Name, switch_url = Url} = HW) ->
    case result(HW, http(post, Url++Name++"/subscriber")) of
	{ok, {200,Ano}} -> {ok,Ano};
	Else            -> Else
    end.

connect(#hw{switch_name = Name, switch_url = Url} = HW, Ano, Bno) when is_list(Ano) andalso is_list(Bno) ->
    curl(put, HW, Url++Name++"/"++Ano++"/connect/"++Bno).

disconnect(#hw{switch_name = Name, switch_url = Url} = HW, Ano, Bno) when is_list(Ano) andalso is_list(Bno) ->
    curl(delete, HW, Url++Name++"/"++Ano++"/connect/"++Bno).

offhook(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    curl(put, HW, Url++Name++"/"++Ano++"/offhook").

onhook(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    curl(delete, HW, Url++Name++"/"++Ano++"/offhook").

reset(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    curl(put, HW, Url++Name++"/"++Ano++"/reset").

start_busytone(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    start_tone(HW, Url++Name++"/"++Ano++"/busytone").

stop_busytone(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    stop_tone(HW, Url++Name++"/"++Ano++"/busytone").

start_dialtone(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    start_tone(HW, Url++Name++"/"++Ano++"/dialtone").

stop_dialtone(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    stop_tone(HW, Url++Name++"/"++Ano++"/dialtone").

start_ringsignal(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    start_tone(HW, Url++Name++"/"++Ano++"/ringsignal").

stop_ringsignal(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    stop_tone(HW, Url++Name++"/"++Ano++"/ringsignal").

start_ringtone(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    start_tone(HW, Url++Name++"/"++Ano++"/ringtone").

stop_ringtone(#hw{switch_name = Name, switch_url = Url} = HW, Ano) when is_list(Ano) ->
    stop_tone(HW, Url++Name++"/"++Ano++"/ringtone").


%%
%% H E L P E R S
%%

start_tone(HW, ToneUrl) ->
    curl(put, HW, ToneUrl).

stop_tone(HW, ToneUrl) ->
    curl(delete, HW, ToneUrl).

curl(Method, HW, ToneUrl) ->
    case result(HW, http(Method, ToneUrl)) of
	{ok, {200,_}} -> ok;
	Else          -> Else
    end.


http(get, Url) ->
    http:request(get, {Url,[]}, [], []);
http(delete, Url) ->
    http:request(delete, {Url,[]}, [], []);
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
