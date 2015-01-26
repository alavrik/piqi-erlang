%% Copyright 2009, 2010, 2011, 2012, 2013, 2014 Anton Lavrik
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

-ifndef(__PIQIC_HRL__).
-define(__PIQIC_HRL__, 1).


-include_lib("piqi/include/piqi_piqi.hrl").


-ifndef(__PIQIC_ERL__).

% import commonly used piqic functions
-import(piqic, [
    to_string/1,
    iod/2,

    typedef_name/1,
    typedef_erlname/1,
    erlname_of_field/2,
    erlname_of_option/2,

    resolve_type_name/2,
    scoped_name/2,
    gen_code/1
]).

-endif.


% indexes of Piqi module contents
-record(index, {
    piqi :: #piqi{},
    import,  % #import.name -> #import{}
    typedef  % typedef_name -> typedef()
}).


-record(context, {
    % the module being processed
    piqi :: #piqi{},
    % index of the piqi module being compiled
    index :: #index{},

    % whether the module being processes is a self-spec
    is_self_spec :: boolean(),

    % original modules being compiled (imported modules ++ [piqi])
    modules :: [#piqi{}],
    % index of imported modules: #piqi.module -> #index{}
    module_index,

    % piqic options (proplist)
    options :: list()
}).


%
% useful macro
%


-define(defined(A, B),
    case A of
        'undefined' -> B;
        _ -> A
    end
).


-define(if_true(Cond, Then),
    case Cond of
        true ->
            Then;
        false ->
            ok
    end
).


-define(if_true(Cond, Then, Else),
    case Cond of
        true ->
            Then;
        false ->
            Else
    end
).


-endif.
