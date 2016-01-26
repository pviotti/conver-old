-module(ct_client_mock).

-behaviour(ct_client).
-behaviour(gen_server).

-define(MAX_OP_LATENCY, 500).  % max operation latency

-export([initialize/1, read/1, write/2, delete/1, terminate/0]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).


%%% Client API
initialize(_Args) ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  register(db_proc, Pid).

read(Key) ->
  gen_server:call(whereis(db_proc), {read, Key}).

write(Key, Val) ->
  gen_server:call(whereis(db_proc), {write, Key, Val}).

delete(Key) ->
  gen_server:call(whereis(db_proc), {delete, Key}).

terminate() ->
  gen_server:call(whereis(db_proc), terminate).

%%% Server functions
init([]) ->
  ets:new(?MODULE, [set, named_table]),
  {ok, []}.

handle_call({write, Key, Val}, _From, _State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY)),
  Res = ets:insert(?MODULE, {Key, Val}),  % overwrites if table is of type 'set'
  {reply, Res, _State};
handle_call({read, Key}, _From, _State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY)),
  Res = ets:lookup_element(?MODULE, Key, 2),
  {reply, Res, _State};
handle_call({delete, Key}, _From, _State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY)),
  Res = ets:delete(?MODULE, Key),
  {reply, Res, _State};
handle_call(terminate, _From, _State) ->
  {stop, normal, ok, _State}.

handle_cast(_Msg, _State) -> {noreply, ok}.

terminate(normal, _State) ->
  ets:delete(?MODULE).

handle_info(_Msg, State) ->  {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.