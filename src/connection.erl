%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2019
%%% @doc
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(connection).
-author("Michal Stanisz").

-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).

-record(state, {
    id,
    channel,
    connection,
    queues,
    plugin
}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Id, Plugin, QueuesWithExchanges]) ->
    %% Start a network connection
    {ok, Connection} = amqp_connection:start(#amqp_params_network{host = "localhost"}),
    %% Open a channel on the connection
    {ok, Channel} = amqp_connection:open_channel(Connection),

    %% Create and bind exchanges
    Queues = lists:map(fun
        ({Exchange, ExType}) ->
            create_queue_and_bind(Id, Exchange, ExType, Channel);
        ({Queue, Exchange, ExType}) ->
            create_queue_and_bind(Queue, Exchange, ExType, Channel)
    end, QueuesWithExchanges),
    
    io:format("queues: ~p~n", [Queues]),
    
    {ok, #state{plugin = Plugin, connection = Connection, id = Id, channel = Channel, queues = Queues}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({Exchange, Type}, #state{id = Id, channel = Channel} = State) ->
    Publish = #'basic.publish'{exchange = Exchange, routing_key = Type},
    amqp_channel:cast(Channel, Publish, #amqp_msg{payload = Id}),
    {noreply, State};
handle_cast(_Request, State) ->
    io:format("Unrecognized message:~p, ~p~n", [_Request, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(#'basic.consume_ok'{}, State) ->
    {nereply, State};
handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, State) ->
    #state{channel = Channel, plugin = Plugin} = State,
    Plugin:handle_message(Payload),
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
    {noreply, State};
handle_info(Info, State) ->
    io:format("unrecognized message: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{connection = Connection, channel = Channel, queues = Queues}) ->
    lists:foreach(fun(Q) -> 
        Delete = #'queue.delete'{queue = Q},
        #'queue.delete_ok'{} = amqp_channel:call(Channel, Delete)
    end, Queues),
    ok = amqp_channel:close(Channel),
    ok = amqp_connection:close(Connection).

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_queue_and_bind(Q, Exchange, ExType, Channel) ->
    #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{queue = Q}),
    
    ExDeclare = #'exchange.declare'{
        exchange = Exchange,
        type = ExType
    },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExDeclare),
    Binding = #'queue.bind'{
        queue = Q,
        exchange = Exchange,
        routing_key = Q
    },

    %% Bind queue
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    Sub = #'basic.consume'{queue = Q},
    #'basic.consume_ok'{} = amqp_channel:call(Channel, Sub),
    Q.
