-module(ct_orchestrator).
-compile([debug_info]).

-export([run/1]).


run(Num) ->
  AtomIds = [list_to_atom([X]) || X <- lists:seq($a,$a+Num)],
  TesterPids = proplists:get_all_values(ok,
    [ct_tester:start(X) || X <- AtomIds]),
  process_flag(trap_exit, true),
  lists:map(fun(X) -> erlang:monitor(process, X) end, TesterPids),
  loop(Num).


loop(Num) ->
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      io:format("Process ~p (~p) terminated message: ~p~n", [Ref,Pid,Reason]),
      case Num of
        1 -> io:format("Testcase terminated.~n");
        _ -> loop(Num-1)
      end;
    Unknown ->
      io:format("Received message: ~p~n",[Unknown]),
      loop(Num)
  end.

