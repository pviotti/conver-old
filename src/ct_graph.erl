-module(ct_graph).

-include("ct_records.hrl").

% TODO having a single graph with different labels?

%% API
-export([build_graphs/1]).

build_graphs(Ops) ->
  O = lists:append([X || {_,X} <- Ops]),
  Gso = build_so(O),
  Grb = build_rb(O),
  Gvis = build_vis(O).


build_so(O) ->
  Gso = digraph:new(),
  [digraph:add_vertex(Gso, V) || V <- O],
  FunIsSo = fun(Op1, Op2) ->
              Op1#op.proc == Op2#op.proc andalso
              Op1#op.end_time =< Op2#op.start_time
            end,
  [digraph:add_edge(Gso, {V1,V2}, V1, V2, []) || V1<- O, V2 <- O, FunIsSo(V1,V2)],
  io:format("So edges: ~p~n", [length(digraph:edges(Gso))]),
  %io:format("So edges: ~p~n", [digraph:edges(Gso)]),
  Gso.

build_rb(O) ->
  Grb = digraph:new(),
  [digraph:add_vertex(Grb, V) || V <- O],
  FunIsRb = fun(Op1, Op2) -> Op1#op.end_time < Op2#op.start_time end,
  [digraph:add_edge(Grb, {V1,V2}, V1, V2, []) || V1<- O, V2 <- O, FunIsRb(V1,V2)],
  io:format("RB edges: ~p~n", [length(digraph:edges(Grb))]),
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
  %io:format("Vis edges: ~p", [digraph:edges(Gvis)]),
  io:format("Vis edges: ~p~n", [length(digraph:edges(Gvis))]),
  Gvis.