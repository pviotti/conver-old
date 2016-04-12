%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Paolo Viotti. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
%%
%% @doc This module is used to produce a graphical representation of
%%      an execution.
%%

-module(conver_vis).

-include("conver.hrl").

-export([draw_execution/3]).


%%% API

%% @doc Draws the representation of the execution described in `Ops'.
%%
%%      The execution is represented in a canonical and straightforward way,
%%      having an horizontal line for each process, and rectangles for the operations.
%%      Consistency violations are highlighted as shades of red filling
%%      the operations' rectangles.
%%      The execution is drawn using the Egd module, and then written to a png file
%%      named as the given `StoreName'.
%%
-spec draw_execution([{atom(), [op()]}], integer(), string()) -> term().
draw_execution(Ops, Duration, StoreName) ->
  NProc = length(Ops),

  OpHeight = 45, VMargin = 38, HMargin = 50,
  GoldenRatio = (1 + math:sqrt(5))/2,
  H = (OpHeight + VMargin) * NProc *2,
  W = trunc(H + H/GoldenRatio),

  Im = egd:create(W, H),

  % Processes lines
  LineLength = W - (HMargin * 2),
  EbinDir = filename:dirname(code:which(?MODULE)), % HACK to get into priv dir
  Filename = filename:join([filename:dirname(EbinDir), "priv", "Helvetica14.wingsfont"]),
  Font = egd_font:load(Filename),
  _ = [{egd:text(Im, {trunc(HMargin/2), trunc(H/(2*NProc)+(X-1)*(H/NProc))},
        Font, string:to_upper(atom_to_list(ProcName)), egd:color(black)),
      egd:line(Im,
        {HMargin, trunc(H/(2*NProc)+(X-1)*(H/NProc)+VMargin)},
        {W-HMargin, trunc(H/(2*NProc)+(X-1)*(H/NProc)+VMargin)},
        egd:color(black))}
      || {{ProcName, _}, X} <- lists:zip(Ops, lists:seq(1, NProc))],

  % Operations rectangles
  FScaleTime = fun(X) -> trunc((LineLength * X)/ Duration + HMargin) end,
  FDrawOps =
    fun(OpProc, IdxP) ->
      OpDetails = convert_ops_details(FScaleTime, OpProc, []),
      [{egd:text(Im,
          {X1, trunc(H/(2*NProc)+(IdxP-1)*(H/NProc)+VMargin)},
          Font, Label, Color),
        FunRect(Im,
        {X1, trunc(H/(2*NProc)+(IdxP-1)*(H/NProc)+VMargin)},
        {X2, trunc(H/(2*NProc)+(IdxP-1)*(H/NProc)+VMargin-OpHeight)},
        Color)} ||
        {X1, X2, Label, Color, FunRect} <- OpDetails]
    end,
  [FDrawOps(X, Y) || {{_, X}, Y} <- lists:zip(Ops, lists:seq(1, NProc))],

  FileName = StoreName ++ ".png",
  egd:save(egd:render(Im, png), FileName),
  egd:destroy(Im),
  os:cmd("see " ++ FileName ++ " &"), % XXX
  ok.


%%% Internal functions

-spec convert_ops_details(fun((integer()) -> integer()), [op()], [tuple()]) -> [tuple()].
convert_ops_details(_FScaleTime, [], Acc) -> Acc;
convert_ops_details(FScaleTime, [H|T], Acc) ->
  {Color, FunRect} = case length(H#op.notes) of
                       %% shades of red depending on
                       %% how many anomalies were detected
            0 -> {egd:color(black), fun egd:rectangle/4} ;
            1 -> {egd:color({223, 123, 123}), fun egd:filledRectangle/4};
            2 -> {egd:color({178, 66, 66}), fun egd:filledRectangle/4};
            _ -> {egd:color({212, 17, 17}), fun egd:filledRectangle/4}
          end,
  Op = {FScaleTime(H#op.start_time), FScaleTime(H#op.end_time),
    get_op_label(H#op.type, H#op.arg), Color, FunRect},
  convert_ops_details(FScaleTime, T, [Op|Acc]).

-spec get_op_label(op_type(), integer()) -> string().
get_op_label(Type, Arg) ->
  case Type of
    read -> "R:" ++ integer_to_list(Arg);
    write -> "W (" ++ integer_to_list(Arg) ++ ")"
  end.