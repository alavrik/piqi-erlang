%% Copyright 2009, 2010, 2011, 2012, 2013 Anton Lavrik
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%
%% @doc Piqi high-level interface
%%
-module(piqi).

-export([start/0, stop/0]).
% utilities
-export([find_piqi/0]).


% @doc start Piqi application
start() ->
    application:start(piqi).


% @doc stop Piqi application
stop() ->
    application:stop(piqi).


%
% Utilities
%

-spec find_piqi() -> string().
find_piqi() ->
    Cmds = [
        fun() -> os:getenv("PIQI") end,
        fun() -> os:find_executable("piqi") end,
        fun rebar_deps_dir/0,
        fun piqi_libdir/0
    ],
    case find_piqi(Cmds) of
        false ->
            erlang:error("failed to find 'piqi' executable");
        PiqiPath when is_list(PiqiPath) ->
            PiqiPath
    end.

rebar_deps_dir() ->
    case os:getenv("REBAR_DEPS_DIR") of
        false ->
            false;
        RebarDepsDir ->
            filename:join([RebarDepsDir, "piqi", apppath()])
    end.

piqi_libdir() ->
    case code:lib_dir(piqi) of
        {error, _Error} ->
            false;
        PiqiLibPath ->
            filename:join(PiqiLibPath, apppath())
    end.

-spec find_piqi(list(fun(() -> false | string()))) -> string().
find_piqi([]) ->
    false;
find_piqi([Cmd|Cmds]) ->
    case Cmd() of
        false ->
            find_piqi(Cmds);
        PiqiPath when is_list(PiqiPath) ->
            PiqiPath
    end.

apppath() ->
    KernelName = os:cmd("uname -s") -- "\n",
    Machine = os:cmd("uname -m") -- "\n",
    Arch = lists:concat([KernelName, "-", Machine]),
    % path to "piqi" executable within "piqi" application directory
    filename:join(["priv", "piqi-binary", Arch, "piqi"]).
