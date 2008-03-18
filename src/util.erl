%%%-------------------------------------------------------------------
%%% File    : util.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-18 by Michael Melanson
%%%-------------------------------------------------------------------
-module(util).

%% API
-compile(export_all).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: pmap(Fun, List)
%% Description: Parallel map implementation
%%--------------------------------------------------------------------
pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
    pmap_gather(Pids).

pmap_gather([H|T]) ->
    receive
        {H, Ret} -> [Ret|pmap_gather(T)]
    end;
pmap_gather([]) ->
    [].

pmap_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.
%%====================================================================
%% Internal functions
%%====================================================================
