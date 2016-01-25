-module(ct_client_riak).

-behavior(ct_client).

-export([initialize/1, read/1, write/2, delete/1, terminate/0]).


%%% Client API
initialize(_Args) ->
  ok.

read(Key) ->
  ok.

write(Key, Val) ->
  ok.

delete(Key) ->
  ok.

terminate() ->
  ok.
