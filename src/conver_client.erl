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
%% @doc Conver client behaviour.
%%
%%      This module provides the behaviour that
%%      has to be implemented in order to make a data store client
%%      supported by Conver.
%%

-module(conver_client).


-callback init(atom(), [term()]) -> pid().
-callback read(pid(), atom()) -> integer().
-callback write(pid(), atom(), integer()) -> term().
-callback delete(pid(), atom()) -> term().
-callback terminate(pid()) -> term().