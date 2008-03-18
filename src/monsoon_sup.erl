%%%-------------------------------------------------------------------
%%% File    : monsoon_sup.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-18 by Michael Melanson
%%%-------------------------------------------------------------------
-module(monsoon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    Broadcaster = {broadcaster,{broadcaster,start_link,[]},
                   permanent,2000,worker,[broadcaster]},
                   
    Connector   = {connector,{connector,start_link,[]},
                   permanent,2000,worker,[connector]},
                   
    Listener    = {listener,{listener,start_link,[]},
                   permanent,2000,worker,[listener]},
                   
    Database    = {database,{database,start_link,[]},
                   permanent,2000,worker,[database]},
                   
    PeerSup     = {peer_sup,{peer_sup,start_link,[]},
                   permanent,2000,supervisor,[peer_sup]},
                   
    Whiteboard  = {whiteboard,{whiteboard,start_link,[]},
                   permanent,2000,worker,[whiteboard]},
 
    {ok,{{one_for_all,1,1}, [Broadcaster,
                             Connector,
                             Listener,
                             Database,
                             PeerSup,
                             Whiteboard]}}.

%%====================================================================
%% Internal functions
%%====================================================================
