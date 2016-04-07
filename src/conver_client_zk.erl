-module(conver_client_zk).

-include_lib("erlzk/include/erlzk.hrl").

-behaviour(conver_client).

-export([init/2, read/2, write/3, delete/2, terminate/1]).


%%% conver_client callbacks

init(ProcId, Conf) ->
  Server = lists:nth((hd(atom_to_list(ProcId)) rem length(Conf))+1, Conf),
  lager:notice("Zk client connecting to server: ~p.~n", [Server]),
  erlzk:start(),
  {ok, Pid} = erlzk:connect([Server], 30000),
  case erlzk:exists(Pid, "/key") of
    {ok, _Stat} -> erlzk:set_data(Pid, "/key", integer_to_binary(0), -1);
    {error, no_node} -> erlzk:create(Pid, "/key", integer_to_binary(0))
  end,
  Pid.

read(Pid, _Key) ->
  erlzk:sync(Pid, "/key"),
  {ok, {Val, _Stat}} = erlzk:get_data(Pid, "/key"),
  binary_to_integer(Val).

write(Pid, _Key, Val) ->
  {ok, _Stat} = erlzk:set_data(Pid, "/key", integer_to_binary(Val), -1).

delete(Pid, _Key) ->
  erlzk:delete(Pid, "/key").

terminate(Pid) ->
  erlzk:close(Pid).

