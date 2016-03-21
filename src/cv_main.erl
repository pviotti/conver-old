-module(cv_main).

-export([run/2]).

-define(ETS_TABLE, ops_db).


run(Num, Store) ->
  % Initialize store
  StoreModule = list_to_atom("cv_client_" ++ atom_to_list(Store)),
  erlang:apply(StoreModule, initialize, [ok]),

  % Initialize ETS table to collect results
  ets:new(?ETS_TABLE, [ordered_set, named_table, public]),

  % Start testers
  AtomIds = [list_to_atom([X]) || X <- lists:seq($a,$a+Num-1)],
  StartTime = erlang:monotonic_time(nano_seconds),
  TesterPids = proplists:get_all_values(ok,
    [cv_tester:start(X, StoreModule, StartTime) || X <- AtomIds]),
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
          OpList = ets:tab2list(?ETS_TABLE),
          OpListChecked = cv_consistency:check_consistency(OpList),
          cv_vis:draw_execution(OpListChecked, EndTime-StartTime, Store);
        _ ->
          loop(Num-1, {StartTime,Store})
      end;
    Unknown ->
      io:format("Received message: ~p~n",[Unknown]),
      loop(Num, {StartTime,Store})
  end.
