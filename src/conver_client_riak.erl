-module(conver_client_riak).

-behavior(conver_client).

-export([initialize/1, read/1, write/2, delete/1, terminate/0]).

%% TODO

%%% Client API
initialize(_Args) ->
  {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 10017),
  pong = riakc_pb_socket:ping(Pid),
  register(riak_proc, Pid),

  Object = riakc_obj:new(<<"bucket">>, <<"key">>, integer_to_binary(0)),
  riakc_pb_socket:put(whereis(riak_proc), Object),
  riakc_pb_socket:set_bucket(Pid, <<"bucket">>, [{last_write_wins, true},
    {consistent, false}, {allow_mult, false}]).

read(_Key) ->
  {ok, Fetched} = riakc_pb_socket:get(whereis(riak_proc), <<"bucket">>, <<"key">>),
  binary_to_integer(riakc_obj:get_value(Fetched)).

write(_Key, Val) ->
  Object = riakc_obj:new(<<"bucket">>, <<"key">>, integer_to_binary(Val)),
  riakc_pb_socket:put(whereis(riak_proc), Object).

delete(_Key) ->
  riakc_pb_socket:delete(whereis(riak_proc), <<"bucket">>, <<"key">>).

terminate() ->
  ok.
