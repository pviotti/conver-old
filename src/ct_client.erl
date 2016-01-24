%%  [ct_client:start_link(list_to_atom(X))|| X <- [[Y] || Y <- "abcde"]].

-module(ct_client).
-compile([debug_info]).

-behavior(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).

-define(MAX_OP_LATENCY, 2500).  % max operation latency
-define(MAX_OP_INTERVAL, 1500). % max inter-operation interval
-define(MAX_OPERATIONS, 10).    % max number of operations
-define(READ_PROBABILITY, 3).   % 1 out of X is a read

-type op_type() :: read | write.
-record(op, {op_type ::op_type(), duration :: non_neg_integer()}).
-type op() :: #op{}.
-record(state, {id="", num_op, ops=[] :: [op()]}).

% External API
start_link(Id) when is_atom(Id) ->
  gen_server:start_link({local, Id}, ?MODULE, [Id], []).

stop(Id) ->
  gen_server:call(Id, stop).

% Functions called by gen_server
init([Id]) ->
  process_flag(trap_exit, true), % To know when the parent shuts down
  %% Sets a seed for random number generation for the life of the process
  %% uses the current time to do it. Unique value guaranteed by now()
  random:seed(now()),
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  NumOp = random:uniform(?MAX_OPERATIONS),
  io:format("Client ~s, initialized.~n", [Id]),
  {ok, #state{id=Id, num_op=NumOp, ops=[]}, Timeout}.

handle_call(stop, _From, S=#state{}) ->
  {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
  io:format("Client ~s received a call: ~s.~n",[S#state.id,_Message]),
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, S, Timeout}.

handle_cast(_Message, S) ->
  io:format("Client ~s received a cast: ~s.~n",[S#state.id,_Message]),
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, S, Timeout}.

handle_info(timeout, S = #state{num_op=0}) ->
  {stop, normal, S};
handle_info(timeout, S = #state{id=N, num_op=NumOp, ops=Ops}) ->
  case random:uniform(?READ_PROBABILITY) of
    1 ->
      OpType = read,
      io:format("Client ~s reads.~n",[N]);
    _ ->
      OpType = write,
      io:format("Client ~s writes.~n",[N])
  end,
  OpLatency = random:uniform(?MAX_OP_LATENCY),
  timer:sleep(OpLatency),
  StateNew=S#state{num_op=(NumOp-1), ops = [#op{op_type=OpType, duration=OpLatency} | Ops]},
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, StateNew, Timeout};
handle_info(_Message, S) ->
  io:format("Client ~s received a message: ~s.~n",[S#state.id,_Message]),
  Timeout = random:uniform(?MAX_OP_INTERVAL),
  {noreply, S, Timeout}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, S) ->
  io:format("Client ~s terminated.~n",[S#state.id]),
  io:format("State: ~p~n", [S#state.ops]);
terminate(shutdown, S) ->
  io:format("Client ~s terminated with shutdown.~n",[S#state.id]);
terminate(Reason, S) ->
  io:format("Client ~s terminated, reason: ~p.~n",[S#state.id,Reason]).
