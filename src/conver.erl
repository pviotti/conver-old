-module(conver).

-include("conver.hrl").

%% API exports
-export([main/2]).



%% API functions

main(Num, StoreAtom) ->
  % Initialize store
  Store = atom_to_list(StoreAtom),
  StoreModule = list_to_atom("conver_client_" ++ Store),
  erlang:apply(StoreModule, initialize, [ok]),

  % Initialize ETS table to collect results
  ets:new(?ETS_TABLE, [ordered_set, named_table, public]),

  % Start testers
  AtomIds = [list_to_atom([X]) || X <- lists:seq($a,$a+Num-1)],
  StartTime = erlang:monotonic_time(nano_seconds),
  TesterPids = proplists:get_all_values(ok,
    [conver_tester:start(X, StoreModule, StartTime) || X <- AtomIds]),
  process_flag(trap_exit, true),
  lists:map(fun(X) -> erlang:monitor(process, X) end, TesterPids),
  loop(Num, {StartTime,Store}),
  erlang:halt(0).


%% Internal functions

loop(Num, {StartTime,Store}) ->
  receive
    {'DOWN', _Ref, process, _Pid, _Reason} ->
      %io:format("Process ~p (~p) terminated, reason: ~p~n", [Ref,Pid,Reason]),
      case Num of
        1 ->
          EndTime = erlang:monotonic_time(nano_seconds),
          OpList = ets:tab2list(?ETS_TABLE),
          OpListChecked = conver_consistency:check_consistency(OpList),
          conver_vis:draw_execution(OpListChecked, EndTime-StartTime, Store);
        _ ->
          loop(Num-1, {StartTime,Store})
      end;
    Unknown ->
      io:format("Received message: ~p~n",[Unknown]),
      loop(Num, {StartTime,Store})
  end.
