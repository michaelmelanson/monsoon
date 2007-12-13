%%%-------------------------------------------------------------------
%%% File    : instance.erl
%%% Author  : Michael Melanson <michael@apollo.local>
%%% Description : An instance of Monsoon
%%%
%%% Created : 18 Nov 2007 by Michael Melanson <michael@apollo.local>
%%%-------------------------------------------------------------------
-module(instance).

-behaviour(gen_server).

%% API
-export([start_link/0, chunk_list/1, connect/3, add_peer/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {peers=[], chunks=[]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


connect(Instance, Host, Port) ->
    gen_server:call(Instance, {connect, Host, Port}).

add_peer(Instance, Peer) ->
    gen_server:cast(Instance, {add_peer, Peer}).

%%--------------------------------------------------------------------
%% Function: chunk_list(Instance) -> [#chunk]
%% Description: Starts the server
%%--------------------------------------------------------------------
chunk_list(Instance) ->
    gen_server:call(Instance, chunk_list).


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
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) ->
%%                                      {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({connect, Host, Port}, _From, State) ->
    Peer = peer:start_link({connect, Host, Port}, self()),
    gen_server:cast(self(), {add_peer, Peer}),
    {reply, ok, State};

handle_call(chunk_list, _From, State) ->
    {reply, State#state.chunks, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({add_peer, Peer}, State) ->
    NewState = State#state{peers=[Peer|State#state.peers]},
    {noreply, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("Instance terminating~n"),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
