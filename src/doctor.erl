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
-export([start/1, stop/0, send_message/2, handle_message/3]).

%%%===================================================================
%%% API
%%%===================================================================

start(Id) ->
    gen_server:start({local, ?MODULE}, connection, 
        [Id, ?MODULE, [{<<"tasks">>, <<"topic">>}, {<<"info">>, <<"fanout">>}]], []).

stop() ->
    gen_server:stop(?MODULE).

send_message(Type, Patient) ->
    gen_server:cast(?MODULE, {<<"tasks">>, Type, Patient}).

handle_message(<<"info">>, _, Msg) ->
    [Admin, M | _] = Msg,
    io:format("[I] Received info message from ~p: ~p~n", [Admin, M]);
handle_message(<<"tasks">>, _, Message) ->
    io:format("Technician response: ~p~n", [Message]).
