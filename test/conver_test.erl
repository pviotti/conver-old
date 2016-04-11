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
-module(conver_test).

-include_lib("eunit/include/eunit.hrl").


%% Helper: return true if mock Mod:Fun was invoked with Arg as argument at least once.
meck_returned(Mod, Fun, Arg) ->
  meck_returned2(Mod, Fun, Arg, meck:history(Mod)).

meck_returned2(_Mod, _Fun, _Arg, _History = []) ->
  false;
meck_returned2(Mod, Fun, Arg, _History = [H|T]) ->
  case H of
    {_CallerPid, {Mod, Fun, MaybeArg}, _Result} ->
      case MaybeArg of
        Arg -> true;
        _   -> meck_returned2(Mod, Fun, Arg, T)
      end;
    _ -> meck_returned2(Mod, Fun, Arg, T)
  end.


conver_mock_test() ->
  ok =  meck:new(io, [unstick, passthrough]),
  meck:expect(io, format, 2, meck:passthrough()),
  ?assertMatch(ok, conver:main(3, mock)), %% XXX assume linearizable configuration
  %%?debugFmt("Meck history:~n~p", [meck:history(io)]),
  ?assert(meck_returned(io, format, ["~nMonotonic Reads...................... ~s~n",[[<<"\e[32m">>,"PASS",<<"\e[0m">>]]])),
  ?assert(meck_returned(io, format, ["Read-Your-Writes..................... ~s~n",[[<<"\e[32m">>,"PASS",<<"\e[0m">>]]])),
  ?assert(meck_returned(io, format, ["Monotonic Writes..................... ~s~n",[[<<"\e[32m">>,"PASS",<<"\e[0m">>]]])),
  ?assert(meck_returned(io, format, ["Writes-Follow-Reads.................. ~s~n",[[<<"\e[32m">>,"PASS",<<"\e[0m">>]]])),
  ?assert(meck_returned(io, format, ["PRAM................................. ~s~n",[[<<"\e[32m">>,"PASS",<<"\e[0m">>]]])),
  ?assert(meck_returned(io, format, ["Causal............................... ~s~n",[[<<"\e[32m">>,"PASS",<<"\e[0m">>]]])),
  ?assert(meck_returned(io, format, ["RealTime............................. ~s~n",[[<<"\e[32m">>,"PASS",<<"\e[0m">>]]])),
  ?assert(meck_returned(io, format, ["Regular.............................. ~s~n~n",[[<<"\e[32m">>,"PASS",<<"\e[0m">>]]])),
  meck:unload(io).
