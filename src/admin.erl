%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019
%%% @doc
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(admin).
-author("Michal Stanisz").

%% API
-export([start/1, stop/0, send_message/1, handle_message/3]).

%%%===================================================================
%%% API
%%%===================================================================

start(Id) ->
    lists:map(fun(Ex) -> {<<Id/binary, "_", Ex/binary>>, <<"#">>, Ex, <<"topic">>}, [<<"res_tasks">>]
    gen_server:start({local, ?MODULE}, connection, 
        [Id, ?MODULE, [{<<Id/binary, "1">>, <<"#">>, <<"res_tasks">>, <<"topic">>}]], []).

stop() ->
    gen_server:stop(?MODULE).

send_message(Message) ->
    gen_server:cast(?MODULE, {<<"info">>, <<"">>, Message}).

handle_message(_, Key, Message) ->
    io:format("[L] ~p ~p~n", [Key, Message]).
