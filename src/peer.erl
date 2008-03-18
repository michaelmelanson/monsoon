%%%-------------------------------------------------------------------
%%% File    : peer.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-18 by Michael Melanson
%%%-------------------------------------------------------------------
-module(peer).

-behaviour(gen_server).

%% API
-export([start_link/1, node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {node=unknown, socket}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

node(Pid) -> gen_server:call(Pid, node).

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
init({connect, Node, IP, Port}) ->
    whiteboard:subscribe(),

    {ok, Sock} = gen_tcp:connect(IP, Port, [list]),
    
    io:format("~p[~p]: Connected to ~p~n", [?MODULE, self(), Node]),
    ok = gen_tcp:send(Sock, protocol:encode({node, node()})),
    
    {ok, #state{node=Node, socket=Sock}};
    
init({socket, Sock}) ->
    whiteboard:subscribe(),
    
    {ok, #state{socket=Sock}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({event, _}, _From, State) ->
    {reply, false, State};
handle_call(node, _From, State) ->
    {reply, State#state.node, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({tcp, _Sock, Data}, State) ->
    case protocol:decode(Data) of
        {node, Node} when State#state.node =:= unknown ->
            io:format("~p[~p]: Other end is ~p~n", [Node, self(), Node]),
            {noreply, State#state{node=Node}};
            
        _ ->
            io:format("~p[~p]: Bad data received~n", [?MODULE, self()]),
            {stop, invalid_data_received, State}
    end;
handle_info({tcp_closed, _Sock}, State) ->
    {stop, socket_closed, State}.
    
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
