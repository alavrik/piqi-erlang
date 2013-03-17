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

% find the location of "piqi" executable
find_piqi() ->
    case os:getenv("PIQI") of
        false ->
            find_piqi_1();
        PiqiName ->
            PiqiName
    end.


find_piqi_1() ->
    case code:lib_dir(piqi) of
        {error, _Error} ->
            find_piqi_in_path();
        PiqiDir ->
            KernelName = os:cmd("uname -s") -- "\n",
            Machine = os:cmd("uname -m") -- "\n",
            Arch = lists:concat([KernelName, "-", Machine]),
            % path to "piqi" executable within "piqi" application directory
            FullName = filename:join([PiqiDir, "priv", "piqi-binary", Arch, "piqi"]),
            case filelib:is_regular(FullName) of
                true ->
                    FullName;
                false ->
                    error_logger:warning_msg("can't find 'piqi' executable at ~s~n", [FullName]),
                    find_piqi_in_path()
            end
    end.


% try looking for "piqi" executable in $PATH
find_piqi_in_path() ->
    case os:find_executable("piqi") of
        false ->
            erlang:error("failed to find 'piqi' executable");
        Filename ->
            Filename
    end.

