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
-export([start/2, stop/0, send_message/1]).

%%%===================================================================
%%% API
%%%===================================================================

start(Id, Types) ->
    gen_server:start({local, ?MODULE}, connection, 
        [Id, ?MODULE, [{Type, <<"tasks">>, <<"topic">>} || Type <- Types]], []).

stop() ->
    gen_server:stop(?MODULE).

send_message(Type) ->
    gen_server:cast(?MODULE, {<<"tasks">>, Type}).