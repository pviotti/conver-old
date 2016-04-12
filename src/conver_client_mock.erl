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
%% @doc Dummy in-memory data store.
%%
%%      This module implements a dummy in-memory data store
%%      based on a ETS set. It is used for testing purposes.
%%

-module(conver_client_mock).

-behaviour(conver_client).
-behaviour(gen_server).

-define(MAX_OP_LATENCY, 650).     % max operation latency
-define(MISREAD_PROBABILITY, 3).  % 1 out of X reads a previous value
                                  % to simulate non-lin behavior

-export([init/2, read/2, write/3, delete/2, terminate/1]).
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).


%%% conver_client callbacks

init(_Proc, Conf) ->
  try
    ets:new(?MODULE, [set, named_table, public])
  catch
    error:badarg -> ok
  end,
  ets:insert(?MODULE, {key, 0}),
  {ok, Pid} = gen_server:start(?MODULE, [Conf], []),
  Pid.

read(Pid, Key) ->
  gen_server:call(Pid, {read, Key}).

write(Pid, Key, Val) ->
  gen_server:call(Pid, {write, Key, Val}).

delete(Pid, Key) ->
  gen_server:call(Pid, {delete, Key}).

terminate(Pid) ->
  gen_server:call(Pid, terminate).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Conf]) ->
  random:seed(erlang:timestamp()),
  {ok, Conf}.

handle_call({write, Key, Val}, _From, State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  Res = ets:insert(?MODULE, {Key, Val}),  % overwrites if table is of type 'set'
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  {reply, Res, State};
handle_call({read, Key}, _From, State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  CorrectRes = ets:lookup_element(?MODULE, Key, 2),
  Res = case proplists:get_value(linearizable, State) of
          true -> CorrectRes;
          _ ->
            case random:uniform(?MISREAD_PROBABILITY)  of
              1 -> random:uniform(CorrectRes);
              _ -> CorrectRes
            end
        end,
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  {reply, Res, State};
handle_call({delete, Key}, _From, State) ->
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  Res = ets:delete(?MODULE, Key),
  timer:sleep(random:uniform(?MAX_OP_LATENCY div 2)),
  {reply, Res, State};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(_Msg, _State) -> {noreply, ok}.

terminate(normal, _State) ->
  ok;
terminate(Reason, S) ->
  lager:warning("Mock client terminated, reason: ~p. State: ~p~n",[Reason, S]).

handle_info(_Msg, State) ->  {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
