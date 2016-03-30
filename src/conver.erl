-module(conver).

-include("conver.hrl").

%% API exports
-export([main/2]).


%%% API

-spec main(pos_integer(), atom()) -> no_return().
main(Num, StoreAtom) ->
  % Initialize utils
  random:seed(erlang:timestamp()),
  Store = atom_to_list(StoreAtom),
  % TODO Lager
  % Initialize ETS table to collect results
  ets:new(?ETS_TABLE, [ordered_set, named_table, public]),

  % Start testers
  ProcIdsAtoms = [list_to_atom([X]) || X <- lists:seq($a,$a+Num-1)],
  StartTime = erlang:monotonic_time(nano_seconds),
  TesterPids = proplists:get_all_values(ok,
    [conver_tester:start(X, Store, StartTime) || X <- ProcIdsAtoms]),
  process_flag(trap_exit, true),
  lists:map(fun(X) -> erlang:monitor(process, X) end, TesterPids),
  loop(Num, {StartTime,Store}),
  erlang:halt(0).


%%% Internal functions

-spec loop(non_neg_integer(), {integer(), string()}) -> term().
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
