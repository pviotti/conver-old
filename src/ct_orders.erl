-module(ct_orders).

-include("ct_records.hrl").

% TODO having a single graph with different labels?

%% API
-export([build_orderings/1]).

check_orderings(Ops) ->
  O = lists:append([X || {_,X} <- Ops]),
  Gso = build_ordering(O, fun cmp_so/2),
  Grb = build_ordering(O, fun cmp_rb/2),
  Gvis = build_ordering(O, fun cmp_vis/2),
  Gar = build_ordering(O, fun cmp_ar/2),
  io:format("so edges: ~p~n", [digraph:no_edges(Gso)]),
  io:format("rb edges: ~p~n", [digraph:no_edges(Grb)]),
  io:format("vis edges: ~p~n", [digraph:no_edges(Gvis)]),
  io:format("ar edges: ~p~n", [digraph:no_edges(Gar)]),
  io:format("br in ar: ~p~n", [check_ar(Gar, Grb)]),
  io:format("so in ar: ~p~n", [check_ar(Gar, Gso)]),
  io:format("vis in ar: ~p~n", [check_ar(Gar, Gvis)]),
  Lar = lists:sort(fun cmp_ar/2, O),
  io:format("ar as list: ~p~n", [Lar]).

check_ar(Gar, G) ->
  Sar = sets:from_list(digraph:edges(Gar)),
  Sg = sets:from_list(digraph:edges(G)),
  sets:is_subset(Sg,Sar).


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
  Op1#op.op_type == write andalso
    Op2#op.op_type == read andalso
    Op1#op.arg == Op2#op.arg.

cmp_ar(Op1, Op2) ->
  (Op1#op.end_time + Op1#op.start_time)/2 <
    (Op2#op.end_time + Op2#op.start_time)/2.
