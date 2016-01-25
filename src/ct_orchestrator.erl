-module(ct_orchestrator).
-compile([debug_info]).

-behaviour(supervisor).

-export([run/0]).
-export([init/1]).

run() ->
  supervisor:start_link({local,?MODULE}, ?MODULE, ok).

init(_Args) ->
  WorkersLst = [ { list_to_atom(X),
   {ct_client, start_link, [list_to_atom(X)]},
    temporary, 2000, worker, [ct_client]}
     || X <- [[Y] || Y <- "abcde"]],
  {ok, {{one_for_one, 3, 60}, WorkersLst }}.
  %{ok, { {one_for_one, 3, 60}, [{abc, {ct_client, start_link, [abc]},
  %                               temporary, 2000, worker, [ct_client]}]  } }.

