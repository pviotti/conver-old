-module(cv_vis).

-include("cv_records.hrl").

-export([draw_execution/3]).

draw_execution(Ops, Duration, FileName) ->
  NProc = length(Ops),

  OpHeight = 45, VMargin = 38, HMargin = 50,
  GoldenRatio = (1 + math:sqrt(5))/2,
  H = (OpHeight + VMargin) * NProc *2,
  W = trunc(H + H/GoldenRatio),

  Im = egd:create(W,H),

  % Processes lines
  LineLength = W - (HMargin * 2),
  EbinDir = filename:dirname(code:which(?MODULE)), % HACK to get into priv dir
  Filename = filename:join([filename:dirname(EbinDir),"priv","fonts","Helvetica14.wingsfont"]),
  Font = egd_font:load(Filename),
  _ = [{egd:text(Im, {trunc(HMargin/2), trunc(H/(2*NProc)+(X-1)*(H/NProc))},
      Font, string:to_upper(atom_to_list(ProcName)), egd:color(black)),
    egd:line(Im,
      {HMargin, trunc(H/(2*NProc)+(X-1)*(H/NProc)+VMargin)},
      {W-HMargin, trunc(H/(2*NProc)+(X-1)*(H/NProc)+VMargin)},
      egd:color(black))}
    || {{ProcName,_},X} <- lists:zip(Ops, lists:seq(1, NProc))],

  % Operations rectangles
  FScaleTime = fun(X) -> trunc((LineLength * X)/ Duration + HMargin) end,
  FDrawOps =
    fun(OpProc, IdxP) ->
      OpDetails = convert_ops_details(FScaleTime, OpProc, []),
      [{egd:text(Im,
          {X1, trunc(H/(2*NProc)+(IdxP-1)*(H/NProc)+VMargin)},
          Font, Label, Color),
       egd:rectangle(Im,
        {X1, trunc(H/(2*NProc)+(IdxP-1)*(H/NProc)+VMargin)},
        {X2, trunc(H/(2*NProc)+(IdxP-1)*(H/NProc)+VMargin-OpHeight)},
        egd:color(black))} ||
        {X1,X2,Label,Color} <- OpDetails]
    end,
  [FDrawOps(X,Y) || {{_,X},Y} <- lists:zip(Ops, lists:seq(1, NProc))],

  egd:save(egd:render(Im, png), atom_to_list(FileName) ++ ".png"),
  egd:destroy(Im),
  os:cmd("see " ++ atom_to_list(FileName) ++ ".png &"). % XXX



%% Private functions

convert_ops_details(_FScaleTime, [], Acc) -> Acc;
convert_ops_details(FScaleTime, [H|T], Acc) ->
  Color = case H#op.notes of
            ok -> egd:color(black);
            ko -> egd:color(red);
            ryw -> egd:color(fuchia)
          end,
  Op = {FScaleTime(H#op.start_time), FScaleTime(H#op.end_time),
    get_op_label(H#op.type, H#op.arg), Color},
  convert_ops_details(FScaleTime, T, [Op|Acc]).

get_op_label(Type, Arg) ->
  case Type of
    read -> "R:" ++ integer_to_list(Arg);
    write -> "W (" ++ integer_to_list(Arg) ++ ")"
  end.