-module(ct_orchestrator).
-compile([debug_info]).

-export([run/2]).


run(Num, Store) ->
  % Initialize store
  StoreModule = list_to_atom("ct_client_" ++ Store),
  erlang:apply(StoreModule, initialize, [ok]),

  % Initialize ETS table to collect testers' results
  ets:new(ops_db, [ordered_set, named_table, public]),  % FIXME parametrize table name

  % Start testers
  AtomIds = [list_to_atom([X]) || X <- lists:seq($a,$a+Num-1)],
  StartTime = erlang:monotonic_time(nano_seconds),
  TesterPids = proplists:get_all_values(ok,
    [ct_tester:start(X, StoreModule, StartTime) || X <- AtomIds]),
  process_flag(trap_exit, true),
  lists:map(fun(X) -> erlang:monitor(process, X) end, TesterPids),
  loop(Num, {StartTime,Store}).


loop(Num, {StartTime,Store}) ->
  receive
    {'DOWN', _Ref, process, _Pid, _Reason} ->
      %io:format("Process ~p (~p) terminated, reason: ~p~n", [Ref,Pid,Reason]),
      case Num of
        1 ->
          EndTime = erlang:monotonic_time(nano_seconds),
          OpList = ets:tab2list(ops_db),
          io:format("Testcase terminated. Results: ~n~p~n", [OpList]),
          ct_vis:draw_execution(OpList, EndTime-StartTime, Store),
          ct_graph:build_graphs(OpList);
        _ ->
          loop(Num-1, {StartTime,Store})
      end;
    Unknown ->
      io:format("Received message: ~p~n",[Unknown]),
      loop(Num, {StartTime,Store})
  end.
