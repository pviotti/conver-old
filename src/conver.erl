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

-module(conver).

-include("conver.hrl").

%% API exports
-export([main/2]).


%%% API

-spec main(pos_integer(), atom()) -> no_return().
main(Num, Store) ->
  % Initialize utils
  application:set_env(lager, suppress_application_start_stop, true),
  lager:start(),
  random:seed(erlang:timestamp()),
  % Initialize ETS table to collect results
  ets:new(?ETS_TABLE, [ordered_set, named_table, public]),

  % Initialize client testers
  StoreConf = get_conf(Store),
  ProcIdsAtoms = [list_to_atom([X]) || X <- lists:seq($a, $a+Num-1)],
  ClientsPids = [conver_tester:init(ProcId, Store, StoreConf)
    || ProcId <- ProcIdsAtoms],

  % Start client testers
  StartTime = erlang:monotonic_time(nano_seconds),
  TesterRes = [conver_tester:start(Proc, Store, CPid, StartTime)
    || {Proc, CPid} <- lists:zip(ProcIdsAtoms, ClientsPids)],
  TesterPids = proplists:get_all_values(ok, TesterRes),
  process_flag(trap_exit, true),
  lists:map(fun(X) -> erlang:monitor(process, X) end, TesterPids),
  loop(Num, {StartTime, Store}).


%%% Internal functions

-spec loop(non_neg_integer(), {integer(), atom()}) -> term().
loop(Num, {StartTime, Store}) ->
  receive
    {'DOWN', _Ref, process, _Pid, _Reason} ->
      %lager:debug("Process ~p (~p) terminated, reason: ~p~n", [Ref,Pid,Reason]),
      case Num of
        1 ->
          EndTime = erlang:monotonic_time(nano_seconds),
          OpList = ets:tab2list(?ETS_TABLE),
          OpListChecked = conver_consistency:check_consistency(OpList),
          conver_vis:draw_execution(OpListChecked, EndTime-StartTime, atom_to_list(Store));
        _ ->
          loop(Num-1, {StartTime, Store})
      end;
    Unknown ->
      lager:warning("Received message: ~p~n",[Unknown]),
      loop(Num, {StartTime, Store})
  end.

-spec get_conf(atom()) -> term().
get_conf(Store) ->
  {ok, Cwd} = file:get_cwd(),
  {ok, Conf} = file:consult(filename:join([Cwd, ?CONVER_CONF])),
  proplists:get_value(Store, Conf).