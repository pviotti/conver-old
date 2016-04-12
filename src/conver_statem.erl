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
%% @doc This module checks for consistency anomalies using an off-the-shelf property-based testing tool.
%%
%%      This module is a demonstrative attempt to use an off-the-shelf
%%      property-based testing tool (i.e. PropEr) to check consistency models
%%      implemented by a data store.
%%
%%      Given that the state of the data store is represented by a symbolic variable,
%%      the only consistency models that can be checked are those that suppose
%%      single-copy semantics, i.e. strong consistency models.
%%

-module(conver_statem).

-behaviour(proper_statem).
-dialyzer(no_undefined_callbacks).

-include_lib("proper/include/proper.hrl").

%% API
-export([test/0, sample/0]).

%% proper_statem callbacks
-export([initial_state/0, command/1,
  precondition/2, postcondition/3, next_state/3]).

-record(state, {val         :: integer(),
                client_pid  :: pid()}).

-define(SERVER, conver_client_mock).


%% API

test() ->
  proper:quickcheck(?MODULE:prop_consistency()).

sample() ->
  proper_gen:pick(commands(?MODULE)).


%%%===================================================================
%%% Properties
%%%===================================================================

prop_consistency() ->
  ?FORALL(Cmds, commands(?MODULE),
    ?TRAPEXIT(
      begin
        {History, State, Result} = run_commands(?MODULE, Cmds),
        ?WHENFAIL(io:format("History: ~w~nState: ~w\nResult: ~w~n",
          [History, State, Result]),
          aggregate(command_names(Cmds), Result =:= ok))
      end)).

prop_paral_consistency() ->
  ?FORALL(Cmds, proper_statem:parallel_commands(?MODULE),
    ?TRAPEXIT(
      begin
        {Sequential, Parallel, Result} = proper_statem:run_parallel_commands(?MODULE, Cmds),
        ?WHENFAIL(io:format("History: ~w~nState: ~w\nResult: ~w~n",
          [Sequential, Parallel, Result]),
          aggregate(command_names(Cmds), Result =:= ok))
      end)).


%%%===================================================================
%%% proper_statem callbacks
%%%===================================================================

initial_state() ->
  Pid = ?SERVER:init(ok, ok),
  #state{client_pid = Pid, val = 0}.

command(S) ->
  oneof([ % or: frequency
    {call, ?SERVER, read, [S#state.client_pid, key]},
    {call, ?SERVER, write, [S#state.client_pid, key, value()]}
  ]).

value() ->
  erlang:unique_integer([monotonic, positive]).

precondition(_, _) ->
  true.

next_state(S, _V, {call, _, read, [_Pid, key]}) ->
  S;
next_state(S, _, {call, _, write, [_Pid, key, Value]}) -> % XXX check if write succeded
  S#state{val = Value}.

postcondition(_S, {call, _, write, [_Pid, key, _Value]}, Result) ->
  Result =:= true;
postcondition(S, {call, _, read, [_Pid, key]}, Result) ->
  Result =:= S#state.val.

