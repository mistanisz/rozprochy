%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019
%%% @doc
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(doctor).
-author("Michal Stanisz").

%% API
-export([start/1, stop/0, send_message/1, handle_message/1]).

%%%===================================================================
%%% API
%%%===================================================================

start(Id) ->
    gen_server:start({local, ?MODULE}, connection, 
        [Id, ?MODULE, [{<<"tasks">>, <<"topic">>}]], []).

stop() ->
    gen_server:stop(?MODULE).

send_message(Type, Patient) ->
    gen_server:cast(?MODULE, {<<"tasks">>, Type}).

handle_message(Message) ->
    io:format("message in doctor: ~p~n", [Message]).
