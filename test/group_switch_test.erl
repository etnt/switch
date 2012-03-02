-module(group_switch_test).
-export([]).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

-define(server_name, ?MODULE).

-define(no_tone, "").
-define(dialtone,    "dialtone").
-define(busytone,    "busytone").
-define(ringtone,    "ringtone").
-define(ringsignal,  "ringsignal").


start_test_server() ->
  group_switch:start(?server_name).

stop_test_server(_) ->
  Res = group_switch:stop(?server_name),
  timer:sleep(10),
  error_logger:info_msg("........ ~p~n",[whereis(?server_name)]),
  undefined = whereis(?server_name),
  Res.
  

setup_subscribers() ->
  _Pid = start_test_server(),
  {ok, Ano} = group_switch:create_subscriber(?server_name),
  Dict = orddict:store(subscriber_1, integer_to_list(Ano), orddict:new()),
  {ok, Bno} = group_switch:create_subscriber(?server_name),
  orddict:store(subscriber_2, integer_to_list(Bno), Dict).


create_subscribers_test_() ->
  { setup,
    fun start_test_server/0,  % setup function
    fun stop_test_server/1,   % teardown function
    fun(_) ->
	{inorder,             % ensure that these are run in order
	 [ ?_assertEqual({ok, 1234}, group_switch:create_subscriber(?server_name))
	 , ?_assertEqual({ok, 1235}, group_switch:create_subscriber(?server_name))
	 , ?_assertEqual({ok, 1236}, group_switch:create_subscriber(?server_name))
	 ]}
    end
   }.

start_stop_tone_test_() ->
  { setup, fun setup_subscribers/0, fun stop_test_server/1,
    fun(Dict) ->
	{inorder,
	 [ ?_assertEqual(ok, 
			 group_switch:start_tone(?server_name, 
						 orddict:fetch(subscriber_1,Dict), 
						 ?dialtone))

	 , ?_assertEqual(ok, 
			 verify_dialtone(orddict:fetch(subscriber_1,Dict)))


	 , ?_assertEqual(ok, 
			 group_switch:stop_tone(?server_name, 
						orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 group_switch:start_tone(?server_name, 
						 orddict:fetch(subscriber_1,Dict), 
						 ?dialtone))

	 , ?_assertEqual(ok, 
			 verify_dialtone(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertMatch({error,_}, 
			 group_switch:start_tone(?server_name, 
						 orddict:fetch(subscriber_1,Dict), 
						 ?busytone))

	 , ?_assertEqual(ok, 
			 verify_dialtone(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 group_switch:stop_tone(?server_name, 
						orddict:fetch(subscriber_1,Dict)))
	   
	 , ?_assertEqual(ok, 
			 group_switch:start_tone(?server_name, 
						 orddict:fetch(subscriber_1,Dict), 
						 ?busytone))

	 , ?_assertEqual(ok, 
			 verify_busytone(orddict:fetch(subscriber_1,Dict)))

	  ]}
    end
   }.

on_off_hook_test_() ->
  { setup, fun setup_subscribers/0, fun stop_test_server/1,
    fun(Dict) ->
	{inorder,
	 [ ?_assertEqual(ok, 
			 group_switch:onhook(?server_name, 
					     orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 verify_onhook(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 group_switch:offhook(?server_name, 
					      orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 verify_offhook(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 group_switch:offhook(?server_name, 
					      orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 verify_offhook(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 group_switch:onhook(?server_name, 
					     orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
			 verify_onhook(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertMatch({error,_}, 
	 		 group_switch:offhook(?server_name, 
	 				      "0000000"))

	  ]}
    end
   }.

dis_connect_test_() ->
  { setup, fun setup_subscribers/0, fun stop_test_server/1,
    fun(Dict) ->
	{inorder,
	 [ ?_assertEqual(ok, 
			 group_switch:connect(?server_name, 
					      orddict:fetch(subscriber_1,Dict),
					      orddict:fetch(subscriber_2,Dict)))

	 , ?_assertEqual(ok, 
			 verify_connected(orddict:fetch(subscriber_1,Dict),
					  orddict:fetch(subscriber_2,Dict)))

	 , ?_assertEqual(ok, 
			 group_switch:disconnect(?server_name, 
						 orddict:fetch(subscriber_1,Dict),
						 orddict:fetch(subscriber_2,Dict)))

	 , ?_assertEqual(not_connected, 
			 verify_connected(orddict:fetch(subscriber_1,Dict),
					  orddict:fetch(subscriber_2,Dict)))

	 , ?_assertEqual(ok, 
			 group_switch:connect(?server_name, 
					      orddict:fetch(subscriber_1,Dict),
					      orddict:fetch(subscriber_2,Dict)))

	 , ?_assertEqual(ok, 
			 verify_connected(orddict:fetch(subscriber_1,Dict),
					  orddict:fetch(subscriber_2,Dict)))

	 , ?_assertMatch({error,_}, 
			 group_switch:connect(?server_name, 
					      orddict:fetch(subscriber_1,Dict),
					      orddict:fetch(subscriber_2,Dict)))

	 , ?_assertEqual(ok, 
			 group_switch:disconnect(?server_name, 
						 orddict:fetch(subscriber_1,Dict),
						 orddict:fetch(subscriber_2,Dict)))

	 , ?_assertEqual(not_connected, 
			 verify_connected(orddict:fetch(subscriber_1,Dict),
					  orddict:fetch(subscriber_2,Dict)))

	  ]}
    end
   }.

reset_test_arne() ->
  { setup, fun setup_subscribers/0, fun stop_test_server/1,
    fun(Dict) ->
	{inorder,
	 [ ?_assertEqual(ok, 
			 group_switch:start_tone(?server_name, 
						 orddict:fetch(subscriber_1,Dict), 
						 ?dialtone))

	 , ?_assertEqual(ok, 
	 		 verify_dialtone(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
	 		 group_switch:offhook(?server_name, 
	 				      orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
	 		 verify_offhook(orddict:fetch(subscriber_1,Dict)))

	 ,?_assertEqual(ok, 
	 		group_switch:connect(?server_name, 
	 				     orddict:fetch(subscriber_1,Dict),
	 				     orddict:fetch(subscriber_2,Dict)))

	 , ?_assertEqual(ok, 
	 		 verify_connected(orddict:fetch(subscriber_1,Dict),
	 				  orddict:fetch(subscriber_2,Dict)))

	 ,?_assertEqual(ok, 
	 		group_switch:reset(?server_name, 
	 				   orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
	 		 verify_no_tone(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(ok, 
	 		 verify_onhook(orddict:fetch(subscriber_1,Dict)))

	 , ?_assertEqual(not_connected, 
	 		 verify_connected(orddict:fetch(subscriber_1,Dict),
	 				  orddict:fetch(subscriber_2,Dict)))
	  ]}
    end
   }.


verify_onhook(StrAno)  -> verify_hook(StrAno, "onhook").
verify_offhook(StrAno) -> verify_hook(StrAno, "offhook").

verify_hook(StrAno, Hook) ->
  case parse_status() of
    [_Header,[StrAno,Hook|_]|_] -> ok;
    _                           -> not_onhook
  end.

verify_dialtone(StrAno) -> verify_tone(StrAno, ?dialtone).
verify_busytone(StrAno) -> verify_tone(StrAno, ?busytone).
verify_no_tone(StrAno)  -> verify_tone(StrAno, ?no_tone).

verify_tone(StrAno, Tone) ->
  case parse_status() of
    [_Header,[StrAno,_Hook,Tone|_]|_]                  -> ok;
    [_Header,[StrAno,_Hook|_]|_] when Tone == ?no_tone -> ok;
    _                                                  -> wrong_tone
  end.
  

verify_connected(Ano, Bno) ->
  case parse_status() of
    [_Header,[Ano,_Hook,Bno|_],[Bno,_Hook,Ano|_]|_] -> ok;
    _                                               -> not_connected
  end.
  

parse_status() ->
  {ok, Status} = group_switch:status(?server_name),
  [string:tokens(Line,"\t") || 
    Line <- string:tokens(binary_to_list(Status), "\n")].
	       


-endif.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% erlang-indent-level: 2 
%%% End:

