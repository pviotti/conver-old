-module(ct_orchestrator).
-compile([debug_info]).

-export([run/0,loop/1]).


run() ->
  process_flag(trap_exit, true),
  {ok, Pid} = ct_tester:start(a),
  erlang:monitor(process, Pid),
  loop(ok).


loop(State) ->
  receive
    {'DOWN', Ref, process, Pid, Reason} ->
      io:format("Process ~p (~p) terminated message: ~p~n", [Ref,Pid,Reason]);
    code_change ->
      ok;
    Unknown ->
      io:format("Unknown message: ~p~n",[Unknown]),
      loop(State)
  end.

