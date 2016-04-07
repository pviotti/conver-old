-module(conver_tester).

-include("conver.hrl").

-behavior(gen_server).

-export([init/3, start/4]).

%% gen_server callbacks
-export([init/1,  handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).


%%% API

-spec init(atom(), atom(), term()) -> pid().  % TODO catch init errors
init(Proc, Store, StoreConf) ->
  ClientModule = get_client_module(Store),
  erlang:apply(ClientModule, init, [Proc, StoreConf]).  % Initialize store client

-spec start(atom(), atom(), pid(), integer()) ->
  {ok,pid()} | ignore | {error,{already_started,pid()} | term()}.
start(Proc, Store, ClientPid, StartTime) ->
  gen_server:start({local, Proc}, ?MODULE, [Proc, Store, ClientPid, StartTime], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Proc, Store, CPid, StartTime]) ->
  random:seed(erlang:timestamp()),  % To do once per process
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  NumOp = rnd_normal(?MEAN_OPS, ?SIGMA_OPS),
  lager:info("P~p (~p) init, n. ops: ~p~n", [Proc, CPid, NumOp]),
  ClientModule = get_client_module(Store),
  {ok, #state{proc=Proc, store=ClientModule, cpid=CPid,
    t0=StartTime, num_op=NumOp, ops=[]}, Timeout}.

handle_call(_Message, _From, S) -> {noreply, S, random:uniform(?MAX_OP_INTERVAL)}.

handle_cast(_Message, S) -> {noreply, S, random:uniform(?MAX_OP_INTERVAL)}.

handle_info(timeout, S = #state{num_op=0}) ->
  {stop, normal, S};
handle_info(timeout, S = #state{proc=Proc, cpid=CPid, t0=T0, num_op=NumOp, ops=Ops}) ->
  StartTime = erlang:monotonic_time(nano_seconds) - T0,
  case random:uniform(?READ_PROBABILITY) of   % TODO handle timeouts and errors
    1 ->
      OpType = read,
      Arg = erlang:apply(S#state.store, read, [CPid, key]),
      io:format("P~s:R:~p. ",[Proc,Arg]);
    _ ->
      OpType = write,
      Arg = erlang:unique_integer([monotonic,positive]), % unique value, monotonic
      erlang:apply(S#state.store, write, [CPid, key, Arg]),
      io:format("P~s:W(~p). ",[Proc,Arg])
  end,
  EndTime = erlang:monotonic_time(nano_seconds) - T0,
  StateNew=S#state{num_op=(NumOp-1), ops = [#op{proc=Proc, type=OpType,
    start_time=StartTime, end_time=EndTime, arg=Arg} | Ops]},
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, StateNew, Timeout};
handle_info(_Message, S) ->
  lager:warning("Process ~s received a message: ~s.~n",[S#state.proc,_Message]),
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, S, Timeout}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, S) ->
  erlang:apply(S#state.store, terminate, [S#state.cpid]),
  ets:insert(?ETS_TABLE, {S#state.proc, S#state.ops}),
  io:format("P~s ended.~n",[S#state.proc]);
terminate(Reason, S) ->
  lager:warning("Process ~s terminated, reason: ~p. State: ~p~n",[S#state.proc, Reason, S#state.ops]).


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Generates a Normal-distributed random variable using Box-Muller method
%% from: https://github.com/basho/basho_stats/blob/develop/src/basho_stats_rv.erl
-spec rnd_normal(integer(), integer()) -> non_neg_integer().
rnd_normal(Mean, Sigma) ->
  Rv1 = random:uniform(),
  Rv2 = random:uniform(),
  Rho = math:sqrt(-2 * math:log(1-Rv2)),
  abs(trunc(Rho * math:cos(2 * math:pi() * Rv1) * Sigma + Mean)).

-spec get_client_module(atom()) -> atom().
get_client_module(Store) ->
  list_to_atom("conver_client_" ++ atom_to_list(Store)).