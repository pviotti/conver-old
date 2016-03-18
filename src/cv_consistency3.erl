-module(cv_consistency3).

-include("cv_records.hrl").

-export([check_consistency/1]).

check_consistency(Ops) ->
  O = lists:append([X || {_,X} <- Ops]),
  G = digraph:new(),
  [digraph:add_vertex(G, V) || V <- O],
  build_ordering(O, G, fun cmp_so/2, so),

  check_monotonic_reads(G), % TODO it may have to use vis too
  check_read_your_writes(G),

  io:format("Operations after session checks:~n"),
  lists:foreach(fun(X)->
                  {V,L} = digraph:vertex(G,X),
                  io:format("~p - ~p~n", [V,L])
                end,
    digraph:vertices(G)),

  build_ordering(O, G, fun cmp_rb/2, rb),
  build_ordering(O, G, fun cmp_vis/2, vis),

  build_ordering(O, G, fun cmp_ar_opmedian/2, ar),

  io:format("Edges:~n"),
  lists:foreach(fun(X)->
                  {E, _, _, L} = digraph:edge(G, X),
                  io:format("~p - ~p~n", [E,L])
                end,
    digraph:edges(G)),

  IsMR = is_consistent(G, mr),
  IsRYW = is_consistent(G, ryw),

  IsRealTime = is_subset(G, rb, ar),
  IsMW = check_monotonic_writes(G),
  IsWFR = check_writes_follow_reads(G),
  IsPRAM = IsMR andalso IsMW andalso IsRYW,
  IsCausal = IsPRAM andalso IsWFR,

  io:format("MR: ~p~n", [IsMR]),
  io:format("RYW: ~p~n", [IsRYW]),
  io:format("RealTime: ~p~n", [IsRealTime]),
  io:format("MW: ~p~n", [IsMW]),
  io:format("WFR: ~p~n", [IsWFR]),
  io:format("PRAM: ~p~n", [IsPRAM]),
  io:format("Causal: ~p~n", [IsCausal]),

  io:format("so in ar: ~p~n", [is_subset(G, so, ar)]),
  io:format("vis in ar: ~p~n", [is_subset(G, vis, ar)]).


is_subset(G, Rel1, Rel2) ->
  SetRel1 = sets:from_list(filter_edges_by_rel(G, Rel1)),
  SetRel2 = sets:from_list(filter_edges_by_rel(G, Rel2)),
  sets:is_subset(SetRel1,SetRel2).

filter_edges_by_rel(G, Rel) ->
  lists:filter(fun(E) ->
                  {{V1,V2}, V1, V2, Label} = digraph:edge(G, E),
                  0 =/= string:str(Label, [Rel])
               end,
          digraph:edges(G)).

build_ordering(O, G, FunCmp, Label) ->
  [add_label_to_edge(G, V1, V2, Label) || V1<- O, V2 <- O, FunCmp(V1,V2)].

add_label_to_edge(G, V1, V2, NewLabel) ->
  case digraph:edge(G, {V1,V2}) of
    {{V1,V2}, V1, V2, Label} ->
      digraph:add_edge(G, {V1,V2}, V1, V2, Label ++ [NewLabel]);
    false ->
      digraph:add_edge(G, {V1,V2}, V1, V2, [NewLabel])
  end.


is_consistent(G, Model) ->
  lists:all(fun(X)->
              {_,L} = digraph:vertex(G,X),
              0 == string:str(L,[Model])
            end,
    digraph:vertices(G)).


check_monotonic_writes(G) ->
  FunFilterWW = fun(E) ->
                  {V1,V2} = E,
                  (V1#op.type == write) and (V2#op.type == write)
                end,
  SetSoWW = sets:from_list(lists:filter(FunFilterWW, filter_edges_by_rel(G, so))),
  SetAr = sets:from_list(filter_edges_by_rel(G, ar)),
  sets:is_subset(SetSoWW, SetAr).


check_writes_follow_reads(G) ->
  FunFilterRW = fun(E) ->
                  {V1,V2} = E,
                  (V1#op.type == read) and (V2#op.type == write)
                end,
  SetSoRW = sets:from_list(lists:filter(FunFilterRW, filter_edges_by_rel(G, so))),
  SetVis = sets:from_list(filter_edges_by_rel(G, vis)),
  SetAr = sets:from_list(filter_edges_by_rel(G, ar)),
  SetVisSoRW = sets:union(SetSoRW, SetVis),
  sets:is_subset(SetVisSoRW, SetAr).

check_monotonic_reads(G) ->
  FunSortRb = fun(OpList) -> lists:sort(fun cmp_rb/2, OpList) end,
  Sessions = lists:map(FunSortRb, digraph_utils:components(G)),
  [check_session_mr(G, 0, Session) || Session <- Sessions].

check_session_mr(_, _, []) -> ok;
check_session_mr(G, LastValueRead, [H|T]) when H#op.type == read ->
  case LastValueRead > H#op.arg of
      true ->
        add_label_to_vertex(G, H, mr),
        check_session_mr(G, LastValueRead, T);
      false ->
        check_session_mr(G, H#op.arg, T)
  end;
check_session_mr(G, LastValueRead, [H|T]) when H#op.type == write ->
  check_session_mr(G, LastValueRead, T).


check_read_your_writes(G) ->
  FunSortRb = fun(OpList) -> lists:sort(fun cmp_rb/2, OpList) end,
  Sessions = lists:map(FunSortRb, digraph_utils:components(G)),
  [check_session_ryw(G, 0, Session) || Session <- Sessions].

check_session_ryw(_, _, []) -> ok;
check_session_ryw(G, LastValueRead, [H|T]) when H#op.type == read ->
  case LastValueRead > H#op.arg of
    true ->
      add_label_to_vertex(G, H, ryw),
      check_session_ryw(G, LastValueRead, T);
    false ->
      check_session_ryw(G, LastValueRead, T)
  end;
check_session_ryw(G, _, [H|T]) when H#op.type == write ->
  check_session_ryw(G, H#op.arg, T).


add_label_to_vertex(G, V, NewLabel) ->
  {_, Label} = digraph:vertex(G, V),
  digraph:add_vertex(G, V, Label ++ [NewLabel]).





check_ar_prec(Op, [Op|_]) when Op#op.type == read ->
  Op;
check_ar_prec(Op, [H|T]) when Op#op.type == read ->
      case H#op.type of
        write ->
          if H#op.arg > Op#op.arg andalso H#op.proc == Op#op.proc ->
              Op#op{notes = ryw};
            H#op.arg > Op#op.arg ->
              Op#op{notes = ko};
            true ->
              check_ar_prec(Op, T)
          end;
        read ->
          check_ar_prec(Op, T)
      end;
check_ar_prec(Op, _) when Op#op.type == write; Op#op.arg == 0 ->
  Op.


%% Functions to compare operations

cmp_so(Op1, Op2) ->
  Op1#op.proc == Op2#op.proc andalso
    Op1#op.end_time =< Op2#op.start_time.

cmp_rb(Op1, Op2) ->
  Op1#op.end_time < Op2#op.start_time.

cmp_vis(Op1, Op2) ->
  Op1#op.type == write andalso
    Op2#op.type == read andalso
    Op1#op.arg == Op2#op.arg.

cmp_ar_opstart(Op1, Op2) ->
  Op1#op.start_time < Op2#op.start_time.

cmp_ar_op14(Op1, Op2) ->
  (Op1#op.end_time - Op1#op.start_time)/4 + Op1#op.start_time <
    (Op2#op.end_time - Op2#op.start_time)/4 + Op2#op.start_time.

cmp_ar_opmedian(Op1, Op2) ->
  (Op1#op.start_time + Op1#op.end_time)/2 <
    (Op2#op.start_time + Op2#op.end_time)/2.

cmp_ar_op34(Op1, Op2) ->
  ((Op1#op.end_time - Op1#op.start_time)/4)*3 + Op1#op.start_time <
    ((Op2#op.end_time - Op2#op.start_time)/4)*3 + Op2#op.start_time.

cmp_ar_opend(Op1, Op2) ->
  Op1#op.end_time < Op2#op.end_time.
