-module(conver_tester).

-include("conver.hrl").

-behavior(gen_server).

-export([start/3]).

%% gen_server callbacks
-export([init/1,  handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).


%%% API

-spec start(atom(), atom(), integer()) ->
  {ok,pid()} | ignore | {error,{already_started,pid()} | term()}.
start(Proc, Store, InitTime) ->
  gen_server:start({local, Proc}, ?MODULE, [Proc, Store, InitTime], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Proc, StoreModule, InitTime]) ->
  process_flag(trap_exit, true),    % To know when the parent shuts down
  random:seed(erlang:timestamp()),  % To do once per process
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  NumOp = rnd_normal(?MEAN_OPS, ?SIGMA_OPS),
  %io:format("C-~p init, n. ops: ~p~n", [Id, NumOp]),
  {ok, #state{proc=Proc, store=StoreModule, t0=InitTime, num_op=NumOp, ops=[]}, Timeout}.

handle_call(_Message, _From, S) -> {noreply, S, random:uniform(?MAX_OP_INTERVAL)}.

handle_cast(_Message, S) -> {noreply, S, random:uniform(?MAX_OP_INTERVAL)}.

handle_info(timeout, S = #state{num_op=0}) ->
  {stop, normal, S};
handle_info(timeout, S = #state{proc=Proc, t0=T0, num_op=NumOp, ops=Ops}) ->
  StartTime = erlang:monotonic_time(nano_seconds) - T0,
  case random:uniform(?READ_PROBABILITY) of   % TODO handle timeouts and errors
    1 ->
      OpType = read,
      Arg = erlang:apply(S#state.store, read, [key]),
      io:format("P~s:R:~p. ",[Proc,Arg]);
    _ ->
      OpType = write,
      Arg = erlang:unique_integer([monotonic,positive]), % unique value, monotonic
      erlang:apply(S#state.store, write, [key, Arg]),
      io:format("P~s:W(~p). ",[Proc,Arg])
  end,
  EndTime = erlang:monotonic_time(nano_seconds) - T0,
  StateNew=S#state{num_op=(NumOp-1), ops = [#op{proc=Proc, type=OpType,
    start_time=StartTime, end_time=EndTime, arg=Arg} | Ops]},
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, StateNew, Timeout};
handle_info(_Message, S) ->
  io:format("Process ~s received a message: ~s.~n",[S#state.proc,_Message]),
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, S, Timeout}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, S) ->
  ets:insert(?ETS_TABLE, {S#state.proc, S#state.ops}),
  io:format("P~s terminated.~n",[S#state.proc]);
terminate(Reason, S) ->
  io:format("Process ~s terminated, reason: ~p. State: ~p~n",[S#state.proc, Reason, S#state.ops]).


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