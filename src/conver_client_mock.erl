-module(conver_client_mock).

-behaviour(conver_client).
-behaviour(gen_server).

-define(MAX_OP_LATENCY, 650).  % max operation latency
-define(MISREAD_PROBABILITY, 3).  % 1 out of X reads a previous value
                                  % to simulate non-lin behavior

-export([initialize/1, read/1, write/2, delete/1, terminate/0]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).


%%% conver_client callbacks

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?MODULE, [set, named_table]),
  ets:insert(?MODULE, {key, 0}),
  {ok, []}.

handle_call({write, Key, Val}, _From, _State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  Res = ets:insert(?MODULE, {Key, Val}),  % overwrites if table is of type 'set'
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  {reply, Res, _State};
handle_call({read, Key}, _From, _State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  case random:uniform(?MISREAD_PROBABILITY) of
    1 ->
      ResCorrect = ets:lookup_element(?MODULE, Key, 2),
      Res = case ResCorrect of
              0 -> 0;
              _ -> ResCorrect -2
            end;
    _ ->
      Res = ets:lookup_element(?MODULE, Key, 2)
  end,
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  {reply, Res, _State};
handle_call({delete, Key}, _From, _State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  Res = ets:delete(?MODULE, Key),
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  {reply, Res, _State};
handle_call(terminate, _From, _State) ->
  {stop, normal, ok, _State}.

handle_cast(_Msg, _State) -> {noreply, ok}.

terminate(normal, _State) ->
  ets:delete(?MODULE).

handle_info(_Msg, State) ->  {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
