-module(conver_client_riak).

-behavior(conver_client).

-export([init/2, read/2, write/3, delete/2, terminate/1]).


%%% conver_client callbacks

init(ProcId, Conf) ->
  Server = lists:nth((hd(atom_to_list(ProcId)) rem length(Conf))+1, Conf),
  io:format("Riak client connecting to server: ~p.~n", [Server]),
  {ok, Pid} = riakc_pb_socket:start_link(element(1,Server), element(2,Server)),
  %% TODO test if bucket is already initialized
  Object = riakc_obj:new(<<"bucket">>, <<"key">>, integer_to_binary(0)),
  riakc_pb_socket:put(Pid, Object),
  riakc_pb_socket:set_bucket(Pid, <<"bucket">>, [{last_write_wins, true},
    {consistent, false}, {allow_mult, false}]),
  Pid.

read(Pid, _Key) ->
  {ok, Fetched} = riakc_pb_socket:get(Pid, <<"bucket">>, <<"key">>),
  binary_to_integer(riakc_obj:get_value(Fetched)).

write(Pid, _Key, Val) ->
  Object = riakc_obj:new(<<"bucket">>, <<"key">>, integer_to_binary(Val)),
  riakc_pb_socket:put(Pid, Object).

delete(Pid, _Key) ->
  riakc_pb_socket:delete(Pid, <<"bucket">>, <<"key">>).

terminate(Pid) ->
  riakc_pb_socket:stop(Pid).
