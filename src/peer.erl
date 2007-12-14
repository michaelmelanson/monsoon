%%%-------------------------------------------------------------------
%%% File    : peer.erl
%%% Author  : Michael Melanson <michael@apollo.local>
%%% Description : Another node in the network
%%%
%%% Created : 10 Nov 2007 by Michael Melanson <michael@apollo.local>
%%%-------------------------------------------------------------------
-module(peer).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {socket, instance, update_timer}).

-define(SERVER, ?MODULE).

-define(UPDATE_INTERVAL, 10000).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link({connect, Host, Port}, Instance) ->
    gen_server:start_link(?MODULE, [{connect, Host, Port}, Instance], []);
start_link({listen, ListenSocket}, Instance) ->
    gen_server:start_link(?MODULE, [{listen, ListenSocket}, Instance], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([{connect, Host, Port}, Instance]) ->
    io:format("Connecting to ~p on port ~w~n", [Host, Port]),

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, line}]),
    ok = gen_tcp:send(Socket, "Hello\n"),

    timer:send_after(?UPDATE_INTERVAL, update_lists),
    {ok, #state{socket=Socket, instance=Instance}};

init([{listen, Port}, Instance]) ->
    gen_server:cast(self(), {listen, Port}),
    {ok, #state{instance=Instance}}.
    

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({listen, ListenSocket}, State) ->
    error_logger:info_msg("~p waiting for connection~n", [self()]),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    error_logger:info_msg("~p connection established~n", [self()]),

    ok = instance:peer_connected(State#state.instance, self()),
    {noreply, State#state{socket=Socket}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, _Port, <<"Hello\r\n">>}, State) ->
    ok = gen_tcp:send(State#state.socket,
                      "How can I help you?\n"),
    {noreply, State};

handle_info({tcp, _Port, <<"Send chunks\r\n">>}, State) ->
    case instance:chunk_list(State#state.instance) of
        [] ->
            ok = gen_tcp:send(State#state.socket,
                              "I don't have any chunks\n");
        Chunks ->
            lists:foreach(fun(Chunk) ->
                                  ok = gen_tcp:send(State#state.socket,
                                                    Chunk)
                          end, Chunks)
    end,

    {noreply, State};

handle_info({tcp, _Port, Data}, State) ->
    ok = gen_tcp:send(State#state.socket,
                      "I don't know what you mean\n"),
    error_logger:warning_msg("~p unknown TCP data received: ~p~n",
                             [self(), Data]),
    {noreply, State};

handle_info({tcp_closed, _Port}, State) ->
    {stop, "Other side terminated connection", State};
    

handle_info(update_lists, State) ->
    ok = gen_tcp:send(State#state.socket, "Send chunks\n"),
    
    timer:send_after(?UPDATE_INTERVAL, update_lists),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    % this may not succeed, so don't trap return
    case State#state.socket of
        undefined ->
            ok;
        Socket ->
            gen_tcp:send(Socket, "Goodbye\n"),
            ok
    end.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

