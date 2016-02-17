-module(ct_tester).
-compile([debug_info]).

-include("ct_records.hrl").

-behavior(gen_server).

-export([start/3]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).

-ifdef(debug).  % TODO http://erlang.org/doc/reference_manual/macros.html#id85032
-define(LOG(X), io:format("[~p,~p]: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

-define(MAX_OP_INTERVAL, 800). % max inter-operation interval
-define(MEAN_OPS, 5).    % mean of uniformly distributed number of operations
-define(SIGMA_OPS, 2).    % sigma of uniformly distributed number of operations
-define(READ_PROBABILITY, 2).   % 1 out of X is a read

% External API
start(Id, Store, InitTime) when is_atom(Id), is_atom(Store) ->
  gen_server:start({local, Id}, ?MODULE, [Id, Store, InitTime], []).

% Functions called by gen_server
init([Id, StoreModule, InitTime]) ->
  process_flag(trap_exit, true), % To know when the parent shuts down
  random:seed(erlang:timestamp()), % To do once per process
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  NumOp = trunc(rnd_normal(?MEAN_OPS, ?SIGMA_OPS)),
  %io:format("C-~s init.~n", [Id]),
  {ok, #state{id=Id, store=StoreModule, t0=InitTime, num_op=NumOp, ops=[]}, Timeout}.

handle_call(_Message, _From, S) -> {noreply, S, random:uniform(?MAX_OP_INTERVAL)}.

handle_cast(_Message, S) -> {noreply, S, random:uniform(?MAX_OP_INTERVAL)}.

handle_info(timeout, S = #state{num_op=0}) ->
  {stop, normal, S};
handle_info(timeout, S = #state{id=N, t0=T0, num_op=NumOp, ops=Ops}) ->
  StartTime = erlang:monotonic_time(nano_seconds) - T0,
  case random:uniform(?READ_PROBABILITY) of   % TODO hadle timeouts and errors
    1 ->
      OpType = read,
      Arg = erlang:apply(S#state.store, read, [key]),
      io:format("C~s:R:~p. ",[N,Arg]);
    _ ->
      OpType = write,
      Arg = erlang:unique_integer([monotonic,positive]), % unique value, monotonic
      erlang:apply(S#state.store, write, [key, Arg]),
      io:format("C~s:W(~p). ",[N,Arg])
  end,
  EndTime = erlang:monotonic_time(nano_seconds) - T0,
  StateNew=S#state{num_op=(NumOp-1), ops = [#op{op_type=OpType, proc = S#state.id,
    start_time = StartTime, end_time = EndTime, arg = Arg} | Ops]},
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, StateNew, Timeout};
handle_info(_Message, S) ->
  io:format("Client ~s received a message: ~s.~n",[S#state.id,_Message]),
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, S, Timeout}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, S) ->
  ets:insert(ops_db, {S#state.id, S#state.ops}),
  io:format("C~s terminated.~n",[S#state.id]);
terminate(Reason, S) ->
  io:format("Client ~s terminated, reason: ~p. State: ~p~n",[S#state.id, Reason, S#state.ops]).


%% Private functions

%% Generates a Normal-distributed random variable, using Box-Muller method
%% from: https://github.com/basho/basho_stats/blob/develop/src/basho_stats_rv.erl
rnd_normal(Mean, Sigma) ->
  Rv1 = random:uniform(),
  Rv2 = random:uniform(),
  Rho = math:sqrt(-2 * math:log(1-Rv2)),
  Rho * math:cos(2 * math:pi() * Rv1) * Sigma + Mean.