-module(ct_client_riak).

-behavior(ct_client).

-export([initialize/1, read/1, write/2, delete/1, terminate/0]).

%% TODO

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


connect() ->
  riakc_pb_socket:start_link("127.0.0.1", 10017).

ping(Pid) ->
  riakc_pb_socket:ping(Pid).

ls(Pid, Bucket) ->
  riakc_pb_socket:list_keys(Pid, Bucket).

put(Pid, Bucket, Key, Value) ->
  Obj1 = riakc_obj:new(Bucket, Key, Value),
  riakc_pb_socket:put(Pid, Obj1).

get(Pid, Bucket, Key) ->
  {ok, Fetched} = riakc_pb_socket:get(Pid, Bucket, Key),
  riakc_obj:get_value(Fetched).

update(Pid, Bucket, Key, Value) ->
  {ok, Fetched} = riakc_pb_socket:get(Pid, Bucket, Key),
  UpdatedObj = riakc_obj:update_value(Fetched, Value),
  riakc_pb_socket:put(Pid, UpdatedObj, [return_body]).

delete(Pid, Bucket, Key) ->
  riakc_pb_socket:delete(Pid, Bucket, Key).
