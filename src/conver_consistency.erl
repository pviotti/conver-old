-module(conver_consistency).

-include("conver.hrl").

-export([check_consistency/1]).


%%% API

-spec check_consistency([{atom(), [op()]}]) -> [{atom(), [op()]}].
check_consistency(Ops) ->
  Sessions = [lists:sort(fun cmp_rb/2, Session) || {_, Session} <- Ops],
  OpLst = lists:append(Sessions),

  G = digraph:new(),
  [digraph:add_vertex(G, V) || V <- OpLst],

  build_ordering(OpLst, G, fun cmp_so/2, so),
  build_ordering(OpLst, G, fun cmp_rb/2, rb),
  build_ordering(OpLst, G, fun cmp_vis/2, vis),

  build_ordering(OpLst, G, fun cmp_ar_opmedian/2, ar),
  true = (count_edges(G,ar) == (length(OpLst) * (length(OpLst)-1)) div 2),
  ArLst = lists:sort(fun cmp_ar_opmedian/2, OpLst),

  IsMR = check_monotonic_reads(G, Sessions),
  IsRYW = check_read_your_writes(G, Sessions),
  IsMW = check_monotonic_writes(G),
  IsWFR = check_writes_follow_reads(G),
  IsPRAM = IsMR andalso IsMW andalso IsRYW andalso is_subset(G, so, ar),
  IsCausal = IsPRAM andalso IsWFR,
  IsRealTime = check_real_time(G),
  IsRValF = check_rval(G, ArLst),
  IsLinearizable = IsCausal andalso IsRealTime andalso
    IsRValF andalso is_subset(G, vis, ar),

  io:format("~nMR: ~s~n", [print_bool(IsMR)]),
  io:format("RYW: ~s~n", [print_bool(IsRYW)]),
  io:format("MW: ~s~n", [print_bool(IsMW)]),
  io:format("WFR: ~s~n", [print_bool(IsWFR)]),
  io:format("PRAM: ~s~n", [print_bool(IsPRAM)]),
  io:format("Causal: ~s~n", [print_bool(IsCausal)]),
  io:format("RealTime: ~s~n", [print_bool(IsRealTime)]),
  io:format("RVal(F): ~s~n", [print_bool(IsRValF)]),
  io:format("Linearizable: ~s~n~n", [print_bool(IsLinearizable)]),

  OpsChecked = build_checked_proplist(G, Ops),
  io:format("Ar as list: ~p~n~n", [ArLst]),
  io:format("Consistency check: ~p~n", [OpsChecked]),
  OpsChecked.


%%%===================================================================
%%% Utility generic functions
%%%===================================================================

-spec build_checked_proplist(digraph:graph(), [{atom(), [op()]}]) ->
  [{atom(), [op()]}].
build_checked_proplist(G, Ops) ->
  FunGetMarkedOp = fun(V) ->
                    {V, Label} = digraph:vertex(G, V),
                    V#op{notes = Label}
                   end,
  [{Proc, lists:sort(fun cmp_rb/2, [FunGetMarkedOp(Op) ||
    Op <- digraph:vertices(G), Op#op.proc == Proc])}
    || Proc <- proplists:get_keys(Ops)].

-spec print_bool(boolean()) -> iolist().
print_bool(true) -> color:green("PASS");
print_bool(false) -> color:red("FAIL").


%%%===================================================================
%%% Consistency check functions
%%%===================================================================

%% Monotonic reads

-spec check_monotonic_reads(digraph:graph(), [[op()]]) -> boolean().
check_monotonic_reads(G, Sessions) ->
  [mark_mr_violations(G, 0, Session) || Session <- Sessions],
  is_semantics_respected(G, mr).

-spec mark_mr_violations(digraph:graph(), integer(), [op()]) -> 'ok'.
mark_mr_violations(_, _, []) -> ok;
mark_mr_violations(G, LastValueRead, [H|T]) when H#op.type == read ->
  LastValRead = case LastValueRead > H#op.arg of
                  true ->
                    add_label_to_vertex(G, H, mr),
                    LastValueRead;
                  false -> H#op.arg
                end,
  mark_mr_violations(G, LastValRead, T);
mark_mr_violations(G, LastValueRead, [H|T]) when H#op.type == write ->
  mark_mr_violations(G, LastValueRead, T).


%% Read-your-writes

-spec check_read_your_writes(digraph:graph(), [[op()]]) -> boolean().
check_read_your_writes(G, Sessions) ->
  [mark_ryw_violations(G, 0, Session) || Session <- Sessions],
  is_semantics_respected(G, ryw).

-spec mark_ryw_violations(digraph:graph(), integer(), [op()]) -> 'ok'.
mark_ryw_violations(_, _, []) -> ok;
mark_ryw_violations(G, LastValueRead, [H|T]) when H#op.type == read ->
  case LastValueRead > H#op.arg of
    true -> add_label_to_vertex(G, H, ryw);
    false -> ok
  end,
  mark_ryw_violations(G, LastValueRead, T);
mark_ryw_violations(G, _, [H|T]) when H#op.type == write ->
  mark_ryw_violations(G, H#op.arg, T).


%% Monotonic writes
-spec check_monotonic_writes(digraph:graph()) -> boolean().
check_monotonic_writes(G) ->
  FunFilterWW = fun({V1,V2}) ->
                  (V1#op.type == write) andalso (V2#op.type == write)
                end,
  SetSoWW = sets:from_list(lists:filter(FunFilterWW, filter_edges_by_rel(G, so))),
  SetAr = sets:from_list(filter_edges_by_rel(G, ar)),
  sets:is_subset(SetSoWW, SetAr).


%% Writes-follow-reads
-spec check_writes_follow_reads(digraph:graph()) -> boolean().
check_writes_follow_reads(G) ->
  FunFilterRW = fun({V1,V2}) ->
                  (V1#op.type == read) andalso (V2#op.type == write)
                end,
  SetSoRW = sets:from_list(lists:filter(FunFilterRW, filter_edges_by_rel(G, so))),
  SetVis = sets:from_list(filter_edges_by_rel(G, vis)),
  SetAr = sets:from_list(filter_edges_by_rel(G, ar)),
  SetVisSoRW = sets:union(SetSoRW, SetVis),
  sets:is_subset(SetVisSoRW, SetAr).


%% Real-time
-spec check_real_time(digraph:graph()) -> boolean().
check_real_time(G) ->
  is_subset(G, rb, ar).


% RVal

-spec check_rval(digraph:graph(), [op()]) -> boolean().
check_rval(G, ArLst) ->
  mark_rval_violations(G, 0, ArLst),
  is_semantics_respected(G, rval).

-spec mark_rval_violations(digraph:graph(), integer(), [op()]) -> 'ok'.
mark_rval_violations(_, _, []) -> ok;
mark_rval_violations(G, LastValueWritten, [H|T]) when H#op.type == read ->
  case H#op.arg == LastValueWritten of
    true -> ok;
    false -> add_label_to_vertex(G, H, rval)
  end,
  mark_rval_violations(G, LastValueWritten, T);
mark_rval_violations(G, _, [H|T]) when H#op.type == write ->
  mark_rval_violations(G, H#op.arg, T).


-spec is_semantics_respected(digraph:graph(), atom()) -> boolean().
is_semantics_respected(G, Model) ->
  lists:all(fun(X)->
              {_,L} = digraph:vertex(G,X),
              not(lists:member(Model, L))
            end,
    digraph:vertices(G)).


%%%===================================================================
%%% Functions to operate on graph entities
%%%===================================================================

-spec build_ordering([op()], digraph:graph(),
    fun((op(),op()) -> boolean()), atom()) -> [term()].
build_ordering(O, G, FunCmp, Label) ->
  [add_label_to_edge(G, V1, V2, Label) || V1<- O, V2 <- O, FunCmp(V1,V2)].


-spec add_label_to_edge(digraph:graph(), digraph:vertex(), digraph:vertex(), atom()) ->
  digraph:edge() | {error, digraph:add_edge_err_rsn()}.
add_label_to_edge(G, V1, V2, NewLabel) ->
  case digraph:edge(G, {V1,V2}) of
    {{V1,V2}, V1, V2, Label} ->
      digraph:add_edge(G, {V1,V2}, V1, V2, Label ++ [NewLabel]);
    false ->
      digraph:add_edge(G, {V1,V2}, V1, V2, [NewLabel])
  end.


-spec add_label_to_vertex(digraph:graph(), digraph:vertex(), atom()) -> digraph:vertex().
add_label_to_vertex(G, V, NewLabel) ->
  {_, Label} = digraph:vertex(G, V),
  digraph:add_vertex(G, V, Label ++ [NewLabel]).


-spec filter_edges_by_rel(digraph:graph(), atom()) -> [digraph:edge()].
filter_edges_by_rel(G, Rel) ->
  lists:filter(fun(E) ->
    {{_,_}, _, _, Label} = digraph:edge(G, E),
    lists:member(Rel, Label)
               end,
    digraph:edges(G)).


-spec count_edges(digraph:graph(), atom()) -> non_neg_integer().
count_edges(G, Rel) ->
  length(filter_edges_by_rel(G, Rel)).


-spec is_subset(digraph:graph(), atom(), atom()) -> boolean().
is_subset(G, Rel1, Rel2) ->
  SetRel1 = sets:from_list(filter_edges_by_rel(G, Rel1)),
  SetRel2 = sets:from_list(filter_edges_by_rel(G, Rel2)),
  sets:is_subset(SetRel1,SetRel2).


%%%===================================================================
%%% Functions to compare operations
%%%===================================================================

-spec cmp_so(op(),op()) -> boolean().
cmp_so(Op1, Op2) ->
  Op1#op.proc == Op2#op.proc andalso
    Op1#op.end_time =< Op2#op.start_time.

-spec cmp_rb(op(),op()) -> boolean().
cmp_rb(Op1, Op2) ->
  Op1#op.end_time < Op2#op.start_time.

-spec cmp_vis(op(),op()) -> boolean().
cmp_vis(Op1, Op2) ->
  Op1#op.type == write andalso
    Op2#op.type == read andalso
    Op1#op.arg == Op2#op.arg.

-spec cmp_ar_opmedian(op(),op()) -> boolean().
cmp_ar_opmedian(Op1, Op2) ->
  (Op1#op.start_time + Op1#op.end_time)/2 <
    (Op2#op.start_time + Op2#op.end_time)/2.

%%-spec cmp_ar_opstart(op(),op()) -> boolean().
%%cmp_ar_opstart(Op1, Op2) ->
%%  Op1#op.start_time < Op2#op.start_time.
%%
%%-spec cmp_ar_op14(op(),op()) -> boolean().
%%cmp_ar_op14(Op1, Op2) ->
%%  (Op1#op.end_time - Op1#op.start_time)/4 + Op1#op.start_time <
%%    (Op2#op.end_time - Op2#op.start_time)/4 + Op2#op.start_time.
%%
%%-spec cmp_ar_op34(op(),op()) -> boolean().
%%cmp_ar_op34(Op1, Op2) ->
%%  ((Op1#op.end_time - Op1#op.start_time)/4)*3 + Op1#op.start_time <
%%    ((Op2#op.end_time - Op2#op.start_time)/4)*3 + Op2#op.start_time.
%%
%%-spec cmp_ar_opend(op(),op()) -> boolean().
%%cmp_ar_opend(Op1, Op2) ->
%%  Op1#op.end_time < Op2#op.end_time.
