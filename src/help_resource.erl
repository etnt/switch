%% @author Torbjorn Tornkvist <tobbe@tornkvist.org>
%% @copyright 2011 Torbjorn Tornkvist
%% @doc A simple phone switch simulator

-module(help_resource).
-export([init/1
         , allowed_methods/2
         , content_types_provided/2
         , to_text/2
        ]).

-ignore_xref([init/1
              , allowed_methods/2
              , content_types_provided/2
              , to_text/2
              , ping/2
             ]).

-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {
          method,
          name
         }).


init([]) ->
    {ok, #ctx{}}.
%  {{trace, "/tmp"}, #ct{}. % when debugging!

%%
allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State#ctx{method = wrq:method(ReqData)}}.

%%
content_types_provided(ReqData, State) ->
      {[{"text/plain", to_text}], ReqData, State}.

%%
to_text(ReqData, State) ->
    {help_text(), ReqData, State}.

help_text() ->
    L =[
	"\n# WELCOME TO THE PHONE SWITCH SIMULATOR\n"
	"# feel free to play around with the commands below\n\n"
	"# Just to shorten the curl commands below\n"
        "export switch=\"http://switch.redhoterlang.com\"\n"
        "export copts=-\"s -H 'Content-Type: text/plain'\"\n"
        "\n"
        "# To create a Switch named: axe\n"
        "curl ${copts} -d \"\" -X 'PUT' ${switch}/x/axe\n"
        "\n"
        "# To get the Switch status. Returns: A textual status table.\n"
        "# (tip: run this via the 'watch' command for an 'animated' experience)\n"
        "curl ${copts} -X 'GET' ${switch}/x/axe\n"
        "\n"
        "# To create a new Subscriber. Returns: <Ano>\n"
        "curl ${copts} -X 'POST' ${switch}/x/axe/subscriber\n"
        "\n"
        "# To control the on/off-hook of the receiver for a particular Subscriber\n"
        "curl ${copts} -d \"\" -X 'PUT' ${switch}/x/axe/Ano/offhook\n"
        "curl ${copts} -d \"\" -X 'DELETE' ${switch}/x/axe/Ano/offhook\n"
        "\n"
        "# To control the tone generator and ringsignal in the Switch for a particular Subscriber\n"
        "curl ${copts} -d \"\" -X 'PUT' ${switch}/x/axe/Ano/dialtone\n"
        "curl ${copts} -d \"\" -X 'DELETE' ${switch}/x/axe/Ano/dialtone\n"
        "\n"
        "curl ${copts} -d \"\" -X 'PUT' ${switch}/x/axe/Ano/busytone\n"
        "curl ${copts} -d \"\" -X 'DELETE' ${switch}/x/axe/Ano/busytone\n"
        "\n"
        "curl ${copts} -d \"\" -X 'PUT' ${switch}/x/axe/Ano/ringtone\n"
        "curl ${copts} -d \"\" -X 'DELETE' ${switch}/x/axe/Ano/ringtone\n"
        "\n"
        "curl ${copts} -d \"\" -X 'PUT' ${switch}/x/axe/Ano/ringsignal\n"
        "curl ${copts} -d \"\" -X 'DELETE' ${switch}/x/axe/Ano/ringsignal\n"
        "\n"
        "# To (dis-)connect two subscribers\n"
        "curl ${copts} -d \"\" -X 'PUT' ${switch}/x/axe/Ano/connect/Bno\n"
        "curl ${copts} -X 'DELETE' ${switch}/x/axe/Ano/connect/Bno\n"
       ],
    list_to_binary(L).
