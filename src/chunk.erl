%%%-------------------------------------------------------------------
%%% File    : chunk.erl
%%% Author  : Michael Melanson <michael@apollo.local>
%%% Description : Library for manipulating chunks
%%%
%%% Created : 14 Dec 2007 by Michael Melanson <michael@apollo.local>
%%%-------------------------------------------------------------------
-module(chunk).

-include("chunk.hrl").
%% API
-export([from_file/1]).

-define(CHUNK_SIZE, 65536).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: from_file(Path) -> {ok, [#chunk]}
%% Description: Loads a list of chunks from a file
%%--------------------------------------------------------------------
from_file(Path) ->
    {ok, File} = file:open(Path, [binary, read]),
    from_stream(File).

from_stream(Stream) ->
    case file:read(Stream, ?CHUNK_SIZE) of
        {ok, Data} ->
            Hash = crypto:sha(Data),
            [#chunk{hash=Hash, contents=Data}|from_stream(Stream)];
        _Other ->
            []
    end.

%%====================================================================
%% Internal functions
%%====================================================================
