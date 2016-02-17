-module(ct_graph).

-include("ct_records.hrl").

%% API
-export([build_graphs/1]).

build_graphs(Ops) ->
  O = lists:append([X || {_,X} <- Ops]),
  Grb = build_rb(O),
  Gvis = build_vis(O).


build_rb(O) ->
  Grb = digraph:new(),
  [digraph:add_vertex(Grb, V) || V <- O],
  FunIsRB = fun(Op1, Op2) -> Op1#op.end_time < Op2#op.start_time end,
  [digraph:add_edge(Grb, {V1,V2}, V1, V2, []) || V1<- O, V2 <- O, FunIsRB(V1,V2)],
  io:format("RB edges: ~p ~p~n", [length(O),length(digraph:edges(Grb))]),
  Grb.

build_vis(O) ->
  Gvis = digraph:new(),
  [digraph:add_vertex(Gvis, V) || V <- O],
  FunIsVis = fun(Op1, Op2) ->
              Op1#op.op_type == write andalso
              Op2#op.op_type == read andalso
              Op1#op.arg == Op2#op.arg
             end,
  [digraph:add_edge(Gvis, {V1,V2}, V1, V2, []) || V1<- O, V2 <- O, FunIsVis(V1,V2)],
  io:format("Vis edges: ~p", [digraph:edges(Gvis)]),
  Gvis.