%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%% @copyright 2011 Torbjorn Tornkvist
%% @doc Phone SW, connected to the simple phone switch simulator
%%
%% Start by setting up a switch with some subscribers:
%%
%%   HW = phone:setup_switch(Name, NumOfSubscribers).
%%
%% Checkout the switch status:
%%
%%   hw_lib:show_switch(HW).
%%
%% Or run, from a Linux shell (substitute NAME):
%%
%%   watch curl -s -H 'Content-Type: text/plain' -X 'GET' http://switch.redhoterlang.com/x/NAME
%%
%% Now, control your phones via the API:
%%
%%   phone:offhook(TelNo).
%%   phone:onhook(TelNo).
%%   phone:digits(TelNo, Digits).
%%

-module(phone).

%% States to be exported.
-export([idle/1    % only one state so far...
	]).

-export([setup_switch/2, start/2, start_link/2, loop_sup/2]).
-export([offhook/1, onhook/1, digits/2]).


-record(store, {
	  ano,
	  bno,
	  my_phone,      % registered name for my phone process
	  other_phone,   % registered name for the B-side phone process
	  hw
	 }).

%% ---------------------------
%% T H E   P H O N E   C O D E
%% ---------------------------

%% In the beginning we were Idle...
idle(S) ->
    hw_lib:reset(S#store.hw, S#store.ano),
    receive

	offhook ->
	    hw_lib:offhook(S#store.hw, S#store.ano),
	    hw_lib:start_dialtone(S#store.hw, S#store.ano),
	    ?MODULE:wait_for_digits(S);

        %% We act as B-side here!
	%% User defined protocol between Phones.
	{connect_request, OtherPhone} ->
	    hw_lib:start_ringsignal(S#store.hw, S#store.ano),
	    OtherPhone ! {connect_accepted, S#store.my_phone},
	    ?MODULE:ringing(S#store{other_phone = OtherPhone});

	_ ->
	    ?MODULE:idle(S)

    end.

%% LOTS OF CODE TO BE WRITTEN HERE !!!


%% -----
%% A P I
%% -----
offhook(Ano) -> to_phone(Ano) ! offhook.
onhook(Ano) -> to_phone(Ano) ! onhook.
digits(Ano, Digits) when is_list(Digits) ->  to_phone(Ano) ! {digits, Digits}.

%%% -----------------------------------------
%%% S T A R T I N G   A N D   S T O P P I N G
%%% -----------------------------------------
start(HW, Ano) when is_list(Ano) ->
    spawn(fun() -> init(HW, Ano) end).

start_link(HW, Ano) when is_list(Ano) ->
    spawn_link(fun() -> init(HW, Ano) end).

init(HW, Ano) ->
    MyPhone = to_phone(Ano),
    register(MyPhone, self()),
    idle(#store{hw = HW, ano = Ano, my_phone = MyPhone}).

%%%
%%% H E L P E R S
%%%

setup_switch(Name, NumOfSubscribers)
  when (is_list(Name) andalso 
	NumOfSubscribers > 0 andalso 
	NumOfSubscribers < 30) ->
    Self = self(),
    Pid = spawn(fun() -> init_sup(Self, Name, NumOfSubscribers) end),
    receive {Pid, HW} -> HW end.
            
%% A simple supervisor of the Phone SW (intentionally not using OTP).
-record(pidano, {pid,ano}).

init_sup(Parent, Name, NumOfSubscribers) ->
    process_flag(trap_exit, true),
    {ok, HW} = hw_lib:new_switch(Name),
    L = [#pidano{pid=start_link(HW, Ano), ano=Ano} || 
            {ok, Ano} <- [hw_lib:new_subscriber(HW) || 
                             _ <- lists:seq(1, NumOfSubscribers)]],
    io:format("L=~p~n",[L]),
    Parent ! {self(), HW},
    loop_sup(HW, L).

loop_sup(HW, L) ->
    receive
        {'EXIT', Pid, Reason} ->
            Ano = pid2ano(Pid, L),
            io:format("Phone with telno(~s) crashed, will be restarted, "
                      "Reason: ~p~n", [Ano, Reason]),
            ?MODULE:loop_sup(HW, replace_pid(Ano, start_link(HW, Ano), L));

        _ ->
            ?MODULE:loop_sup(HW, L)
    end.

pid2ano(Pid, L) ->
    #pidano{ano=Ano} = lists:keyfind(Pid, #pidano.pid, L),
    Ano.

replace_pid(Ano, Pid, [#pidano{ano=Ano} = H | T]) -> [H#pidano{pid=Pid} | T];
replace_pid(Ano, Pid, [H|T])                      -> [H|replace_pid(Ano,Pid,T)];
replace_pid(_Ano, _Pid, [])                       -> [].
	
%% 'Phone' is a registered name: phone_<TELNO> 
phone2telno(Phone) when is_atom(Phone) ->
    "phone_"++Telno = atom_to_list(Phone),
    Telno.
		  
to_phone(Ano) when is_list(Ano) ->    
    list_to_atom("phone_"++Ano);
to_phone(Ano) when is_integer(Ano) ->    
    to_phone(integer_to_list(Ano)).

