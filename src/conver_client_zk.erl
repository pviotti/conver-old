-module(conver_client_zk).

-include_lib("erlzk/include/erlzk.hrl").

-behaviour(conver_client).

-export([initialize/1, read/1, write/2, delete/1, terminate/0]).

%% TODO connection string as conf

%%% Client API
initialize(_Args) ->
  erlzk:start(),
  {ok, Pid} = erlzk:connect([{"localhost", 2181}], 30000),
  case erlzk:exists(Pid, "/a") of
    {ok, _Stat} -> erlzk:set_data(Pid, "/key", integer_to_binary(0), -1);
    {error, no_node} -> erlzk:create(Pid, "/key")
  end,
  register(zk_proc, Pid).

read(_Key) ->
  {ok, {Val, _Stat}} = erlzk:get_data(whereis(zk_proc), "/key"),
  binary_to_integer(Val).

write(_Key, Val) ->
  {ok, _Stat} = erlzk:set_data(whereis(zk_proc), "/key", integer_to_binary(Val), -1).

delete(_Key) ->
  erlzk:delete(whereis(zk_proc), "/key").

terminate() ->
  erlzk:close(whereis(zk_proc)).

