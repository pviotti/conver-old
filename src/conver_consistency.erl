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
  build_ordering(OpLst, G, fun are_concurrent/2, conc),

  build_ordering(OpLst, G, fun cmp_ar/2, ar),
  true = (count_edges(G,ar) == (length(OpLst) * (length(OpLst)-1)) div 2),
  ArLst = lists:sort(fun cmp_ar/2, OpLst),

  IsMR = check_monotonic_reads(G, Sessions),
  IsRYW = check_read_your_writes(G, Sessions),
  IsMW = check_monotonic_writes(G),
  IsWFR = check_writes_follow_reads(G),
  IsPRAM = IsMR andalso IsMW andalso IsRYW andalso is_subset(G, so, ar),
  IsCausal = IsPRAM andalso IsWFR,
  IsRealTime = check_real_time(G),
  IsRValF = check_rval(G, ArLst),
  IsRegular = IsCausal andalso IsRealTime andalso
    IsRValF andalso is_subset(G, vis, ar),

  OpsChecked = build_checked_proplist(G, Ops),
  %io:format("Ar as list: ~p~n~n", [ArLst]),
  io:format("Consistency check: ~p~n", [OpsChecked]),

  io:format("~nMonotonic Reads...................... ~s~n", [print_bool(IsMR)]),
  io:format("Read-Your-Writes..................... ~s~n", [print_bool(IsRYW)]),
  io:format("Monotonic Writes..................... ~s~n", [print_bool(IsMW)]),
  io:format("Writes-Follow-Reads.................. ~s~n", [print_bool(IsWFR)]),
  io:format("PRAM................................. ~s~n", [print_bool(IsPRAM)]),
  io:format("Causal............................... ~s~n", [print_bool(IsCausal)]),
  io:format("RealTime............................. ~s~n", [print_bool(IsRealTime)]),
  io:format("Regular.............................. ~s~n~n", [print_bool(IsRegular)]),

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
  [mark_mr_violations(G, #op{proc=init, type=write, arg=0}, Session) || Session <- Sessions],
  is_semantics_respected(G, mr).

-spec mark_mr_violations(digraph:graph(), op(), [op()]) -> 'ok'.
mark_mr_violations(_, _, []) -> ok;
mark_mr_violations(G, LastWriteRead, [H|T]) when H#op.type == read ->
  NewLastWriteRead = if
                       LastWriteRead#op.arg > H#op.arg,  H#op.arg =< 0 ->
                         LastWriteRead;
                       LastWriteRead#op.arg > H#op.arg,  H#op.arg > 0  ->
                         OriginalWrite = hd(get_in_neighbours_by_rel(G, H, vis)),
                         ConcNeighbours = get_in_neighbours_by_rel(G, OriginalWrite, conc),
                         case lists:member(LastWriteRead, ConcNeighbours) of
                           %% If the last write in ar is concurrent with the write whose value is read by this operation
                           %% then it's an error in the way we constructed the ar speculative total order.
                           true ->
                             lager:notice("[MR] Anomaly in the speculative total order ar: ~p", [H]),
                             OriginalWrite;
                           false ->
                             add_label_to_vertex(G, H, mr),        %% otherwise: mark the anomaly
                             LastWriteRead
                         end;
                       LastWriteRead#op.arg == H#op.arg ->
                         LastWriteRead;
                       LastWriteRead#op.arg < H#op.arg ->
                         hd(get_in_neighbours_by_rel(G, H, vis))
                     end,
  mark_mr_violations(G, NewLastWriteRead, T);
mark_mr_violations(G, LastValueRead, [H|T]) when H#op.type == write ->
  mark_mr_violations(G, LastValueRead, T).


%% Read-your-writes

-spec check_read_your_writes(digraph:graph(), [[op()]]) -> boolean().
check_read_your_writes(G, Sessions) ->
  [mark_ryw_violations(G, #op{proc=init, type=write, arg=0}, Session) || Session <- Sessions],
  is_semantics_respected(G, ryw).

-spec mark_ryw_violations(digraph:graph(), op(), [op()]) -> 'ok'.
mark_ryw_violations(_, _, []) -> ok;
mark_ryw_violations(G, LastWrite, [H|T]) when H#op.type == read ->
  case LastWrite#op.arg > H#op.arg of
    true ->
      OriginalWrite = hd(get_in_neighbours_by_rel(G, H, vis)),
      ConcNeighbours = get_in_neighbours_by_rel(G, OriginalWrite, conc),
      case lists:member(LastWrite, ConcNeighbours) of
        %% If the last write in ar is concurrent with the write whose value is read by this operation
        %% then it's an error in the way we constructed the ar speculative total order.
        true -> lager:notice("[RYW] Anomaly in the speculative total order ar: ~p", [H]);
        false ->  add_label_to_vertex(G, H, ryw)        %% otherwise: mark the anomaly
      end;
    false -> ok
  end,
  mark_ryw_violations(G, LastWrite, T);
mark_ryw_violations(G, _, [H|T]) when H#op.type == write ->
  mark_ryw_violations(G, H, T).


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
  case sets:is_subset(SetVisSoRW, SetAr) of
    true -> true;
    false ->
      lager:notice("WFR anomaly: ~p", [sets:subtract(SetVisSoRW, SetAr)]),
      false
  end.


%% Real-time
-spec check_real_time(digraph:graph()) -> boolean().
check_real_time(G) ->
  is_subset(G, rb, ar).


% RVal

-spec check_rval(digraph:graph(), [op()]) -> boolean().
check_rval(G, ArLst) ->
  mark_rval_violations(G, #op{proc=init, type=write, arg=0}, ArLst),
  is_semantics_respected(G, rval).

-spec mark_rval_violations(digraph:graph(), op(), [op()]) -> 'ok'.
mark_rval_violations(_, _, []) -> ok;
mark_rval_violations(G, LastWrite, [H|T]) when H#op.type == read ->
  case H#op.arg == LastWrite#op.arg of
    true -> ok;
    false ->
      if H#op.arg == 0 -> ok; %% XXX exception for initial write(0): otherwise hd() would throw an error
        true ->
          OriginalWrite = hd(get_in_neighbours_by_rel(G, H, vis)),
          OriginalWriteConc = get_in_neighbours_by_rel(G, OriginalWrite, conc),
          LastWriteConc = get_in_neighbours_by_rel(G, LastWrite, conc),
          IsWriteConcurrent = lists:member(LastWrite, OriginalWriteConc) or
                              lists:member(H, OriginalWriteConc) or
                              lists:member(H, LastWriteConc),
          if IsWriteConcurrent ->
              %% If the last write in ar or the current read is concurrent with the write whose value has been read
              %% then it's just an error in the way we constructed the ar speculative total order
              lager:notice("[RVAL] Anomaly in the speculative total order ar: ~p", [H]);
            true ->
              add_label_to_vertex(G, H, rval)       %% otherwise: mark the anomaly
          end
      end
  end,
  mark_rval_violations(G, LastWrite, T);
mark_rval_violations(G, _, [H|T]) when H#op.type == write ->
  mark_rval_violations(G, H, T).


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


-spec get_in_neighbours_by_rel(digraph:graph(), digraph:vertex(), atom()) ->
  [digraph:vertex()].
get_in_neighbours_by_rel(G, V, Rel) ->
  lists:filter(fun(VN) ->
                {{VN, V}, VN, V, Label} = digraph:edge(G, {VN, V}),
                lists:member(Rel, Label)
              end,
    digraph:in_neighbours(G, V)).


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

-spec cmp_ar(op(),op()) -> boolean().
cmp_ar(Op1, Op2) ->
  case are_concurrent(Op1, Op2) of
    false -> cmp_rb(Op1, Op2);
    true -> %% Concurrent operations
      if
        Op1#op.arg == Op2#op.arg, Op1#op.type == Op2#op.type ->   %% they can only be two reads, by design
          Op1#op.proc < Op2#op.proc;                              %%    use process id to break ties
        Op1#op.arg == Op2#op.arg, Op1#op.type =/= Op2#op.type ->  %% a read and a write with same argument, concurrent
          Op1#op.type == write;                                   %%    the write goes first

        Op1#op.arg < Op2#op.arg; Op1#op.arg > Op2#op.arg ->
          cmp_opmedian(Op1, Op2)                                  %% Speculative ordering based on operations' medians
      end
  end.

-spec are_concurrent(op(),op()) -> boolean().
are_concurrent(Op1, Op2) ->
  not cmp_rb(Op1, Op2) andalso not cmp_rb(Op2, Op1).

-spec cmp_opmedian(op(),op()) -> boolean().
cmp_opmedian(Op1, Op2) ->
  (Op1#op.start_time + Op1#op.end_time)/2 <
    (Op2#op.start_time + Op2#op.end_time)/2.
