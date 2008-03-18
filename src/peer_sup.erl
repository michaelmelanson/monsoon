%%%-------------------------------------------------------------------
%%% File    : peer_sup.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-18 by Michael Melanson
%%%-------------------------------------------------------------------
-module(peer_sup).

-behaviour(supervisor).

-import(util, [pmap/2]).

%% API
-export([start_link/0, ensure_connected/3, new_connect/3, new_on_socket/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

connected_to(Node) ->
    Nodes = lists:map(fun({_Id, Child, _Type, _Modules}) ->
                          Result = peer:node(Child),
                          io:format("peer:node(~p) = ~p~n", [Child, Result]),
                          Result
                      end, supervisor:which_children(?MODULE)),
                 
    lists:any(fun(X) -> X =:= Node end, Nodes).

ensure_connected(Node, _IP, _Port) when Node =:= node()->
    ok; % Don't connect to ourselves;
ensure_connected(Node, IP, Port) ->
    case connected_to(Node) of
        false ->
            io:format("~p: No connection to node ~p~n", [?MODULE, Node]),
            new_connect(Node, IP, Port);
        _ ->
            io:format("~p: Already connected to node ~p~n", [?MODULE, Node]),
            ok
    end.
    
%% Starts a new peer, and tells it to connect to an IP and port
new_connect(Node, IP, Port) ->
    supervisor:start_child(?SERVER, [{connect, Node, IP, Port}]).

new_on_socket(Socket) ->
    {ok, Pid} = supervisor:start_child(?SERVER, [{socket, Socket}]),
    % Transfer control of the socket to the new peer.
    ok = gen_tcp:controlling_process(Socket, Pid),
    {ok, Pid}.

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    Peer = {peer,{peer,start_link,[]},
            temporary,2000,worker,[peer]},
            
    {ok,{{simple_one_for_one,10,1}, [Peer]}}.

%%====================================================================
%% Internal functions
%%====================================================================
