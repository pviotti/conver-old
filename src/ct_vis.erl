-module(ct_vis).

-include("ct_records.hrl").

%% API
-export([draw_execution/2]).
%-compile(export_all).


draw() ->
  Im = egd:create(200,200),
  Red = egd:color({255,0,0}),
  Green = egd:color({0,255,0}),
  Blue = egd:color({0,0,255}),
  Black = egd:color({0,0,0}),
  Yellow = egd:color({255,255,0}),

  % Line and fillRectangle
  egd:filledRectangle(Im, {20,20}, {180,180}, Red),
  egd:line(Im, {0,0}, {200,200}, Black),

  egd:save(egd:render(Im, png), "test1.png"),

  egd:filledEllipse(Im, {45, 60}, {55, 70}, Yellow),
  egd:filledEllipse(Im, {145, 60}, {155, 70}, Blue),

  egd:save(egd:render(Im, png), "test2.png"),

  R = 80,
  X0 = 99,
  Y0 = 99,

  Pts = [ { 	X0 + trunc(R*math:cos(A*math:pi()*2/360)),
    Y0 + trunc(R*math:sin(A*math:pi()*2/360))
  } || A <- lists:seq(0,359,5)],
  lists:map(
    fun({X,Y}) ->
      egd:rectangle(Im, {X-5, Y-5}, {X+5,Y+5}, Green)
    end, Pts),

  egd:save(egd:render(Im, png), "test3.png"),

  % Text
  Filename = filename:join([code:priv_dir(percept), "fonts", "6x11_latin1.wingsfont"]),
  Font = egd_font:load(Filename),
  {W,H} = egd_font:size(Font),
  String = "egd says hello",
  Length = length(String),

  egd:text(Im, {round(100 - W*Length/2), 200 - H - 5}, Font, String, Black),

  egd:save(egd:render(Im, png), "test4.png"),
  egd:destroy(Im).


draw_execution(Ops, [StartTime, EndTime]) ->

  NProc = length(Ops),
  TotTime = EndTime - StartTime,

  OpHeight = 45, VMargin = 38, HMargin = 50,
  GoldenRatio = (1 + math:sqrt(5))/2,
  H = (OpHeight + VMargin) * NProc *2,
  W = trunc(H + H/GoldenRatio),

  LineLength = W - (HMargin * 2),

  FScaleTime = fun(X) -> trunc((LineLength * X)/TotTime + HMargin) end,

  Im = egd:create(W,H),

  % Processes lines
  [egd:line(Im,
    {HMargin, trunc(H/(2*NProc)+(X-1)*(H/NProc)+VMargin)},
    {W-HMargin, trunc(H/(2*NProc)+(X-1)*(H/NProc)+VMargin)},
    egd:color(black))
    || X <- lists:seq(1,NProc)],

  % Operations rectangles
  FDrawRect =
    fun(OpProc, IdxP) ->
      OpTimes = get_scaled_xs(FScaleTime, StartTime, OpProc, []),
      [egd:rectangle(Im,
        {X1, trunc(H/(2*NProc)+(IdxP-1)*(H/NProc)+VMargin)},
        {X2, trunc(H/(2*NProc)+(IdxP-1)*(H/NProc)+VMargin-OpHeight)},
        egd:color(black)) ||
        {X1,X2} <- OpTimes]
    end,
  [FDrawRect(X,Y) || {{_,X},Y} <- lists:zip(Ops, lists:seq(1, length(Ops)))],

  egd:save(egd:render(Im, png), "proc.png"),
  egd:destroy(Im).


get_scaled_xs(FScaleTime, StartTime, [], Acc) -> Acc;
get_scaled_xs(FScaleTime, StartTime, [H|T], Acc) ->
  Op = {FScaleTime(H#op.start_time - StartTime), FScaleTime(H#op.end_time - StartTime)},
  get_scaled_xs(FScaleTime, StartTime, T, [Op|Acc]).
