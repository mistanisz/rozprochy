%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019
%%% @doc
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(technician).
-author("Michal Stanisz").

%% API
-export([start/2, stop/0, handle_message/3]).

%%%===================================================================
%%% API
%%%===================================================================

start(Id, Types) ->
    P = spawn(fun loop/0),
    register(loop, P),
    gen_server:start({local, ?MODULE}, connection, 
        [Id, ?MODULE, [{Type, <<"tasks">>, <<"topic">>} || Type <- Types] ++ [{<<"info">>, <<"fanout">>}]], []).

stop() ->
    gen_server:stop(?MODULE).

handle_message(<<"info">>, _, Msg) ->
    [Admin, M | _] = Msg,
    io:format("[I] Received info message from ~p: ~p~n", [Admin, M]);
handle_message(<<"tasks">>, Key, Msg) ->
    [Doctor, Patient | _] = Msg,
    io:format("Technician received: ~p for patient ~p~n", [Key, Patient]),
    loop ! fun() -> send_message(Doctor, [Patient, Key, <<"done">>]) end,
    ok.

send_message(Type, Payload) ->
    gen_server:cast(?MODULE, {<<"tasks">>, Type, Payload}).

loop() ->
    receive Fun ->
        timer:sleep(timer:seconds(1)),
        Fun(),
        loop()
    end.