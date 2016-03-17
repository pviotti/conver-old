-module(cv_consistency).

-include("cv_records.hrl").

-export([check_consistency/1]).

check_consistency(Ops) ->
  O = lists:append([X || {_,X} <- Ops]),
  Gso = build_ordering(O, fun cmp_so/2),
  Grb = build_ordering(O, fun cmp_rb/2),
  Gvis = build_ordering(O, fun cmp_vis/2),
  Gar = build_ordering(O, fun cmp_ar/2),
  io:format("n. ops: ~p. ", [length(O)]),
  io:format("so edges: ~p. ", [digraph:no_edges(Gso)]),
  io:format("rb edges: ~p. ", [digraph:no_edges(Grb)]),
  io:format("vis edges: ~p.~n", [digraph:no_edges(Gvis)]),

  io:format("ar edges: ~p (~p)~n", [digraph:no_edges(Gar),
    digraph:no_edges(Gar) == (length(O) * (length(O)-1)) div 2]),
  io:format("rb in ar: ~p. ", [check_ar(Gar, Grb)]),
  io:format("so in ar: ~p. ", [check_ar(Gar, Gso)]),
  io:format("vis in ar: ~p.~n", [check_ar(Gar, Gvis)]),
  Lar = lists:sort(fun cmp_ar/2, O),
  LarChecked = [check_ar_prec(X, Lar) || X <- Lar],
  io:format("ar as list: ~p~n", [LarChecked]),
  Fun = fun(Key) -> {Key, [X || X <- LarChecked, X#op.proc == Key]} end,
  DarChecked = lists:map(Fun, proplists:get_keys(Ops)),
  io:format("consistency check: ~p~n", [DarChecked]),
  DarChecked.


check_ar(Gar, G) ->
  Sar = sets:from_list(digraph:edges(Gar)),
  Sg = sets:from_list(digraph:edges(G)),
  sets:is_subset(Sg,Sar).


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


build_ordering(O, FunCmp) ->
  G = digraph:new(),
  [digraph:add_vertex(G, V) || V <- O],
  [digraph:add_edge(G, {V1,V2}, V1, V2, []) || V1<- O, V2 <- O, FunCmp(V1,V2)],
  G.

cmp_so(Op1, Op2) ->
  Op1#op.proc == Op2#op.proc andalso
    Op1#op.end_time =< Op2#op.start_time.

cmp_rb(Op1, Op2) ->
  Op1#op.end_time < Op2#op.start_time.

cmp_vis(Op1, Op2) ->
  Op1#op.type == write andalso
    Op2#op.type == read andalso
    Op1#op.arg == Op2#op.arg.

cmp_ar(Op1, Op2) ->
  (Op1#op.start_time + Op1#op.end_time)/2 <
    (Op2#op.start_time + Op2#op.end_time)/2.
