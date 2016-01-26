-module(ct_orchestrator).
-compile([debug_info]).

-export([run/2]).


run(Num, Store) ->
  % Initialize store
  StoreModule = list_to_atom("ct_client_" ++ Store),
  erlang:apply(StoreModule, initialize, [ok]),
  erlang:apply(StoreModule, write, [key, init_value]), % write initial value

  % Initialize ETS table to collect testers' results
  ets:new(ops_db, [set, named_table, public]),  % FIXME parametrize table name

  % Start testers
  AtomIds = [list_to_atom([X]) || X <- lists:seq($a,$a+Num-1)],
  TesterPids = proplists:get_all_values(ok,
    [ct_tester:start(X, StoreModule) || X <- AtomIds]),
  process_flag(trap_exit, true),
  lists:map(fun(X) -> erlang:monitor(process, X) end, TesterPids),
  loop(Num).


loop(Num) ->
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      io:format("Process ~p (~p) terminated, reason: ~p~n", [Ref,Pid,Reason]),
      case Num of
        1 ->
          io:format("Testcase terminated. Results: ~n~p~n", [ets:tab2list(ops_db)]);
        _ ->
          loop(Num-1)
      end;
    Unknown ->
      io:format("Received message: ~p~n",[Unknown]),
      loop(Num)
  end.
