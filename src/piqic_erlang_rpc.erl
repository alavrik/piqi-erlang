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

-module(piqic_erlang_rpc).
-compile([export_all, nowarn_export_all]).


-include("piqic.hrl").


% generating piqi-rpc server stubs
gen_piqi(Context) ->
    Piqi = Context#context.piqi,
    Mod = Piqi#piqi.module,

    FuncList = Piqi#piqi.func,
    FuncClauses = [ gen_func_clause(X, Mod) || X <- FuncList ],

    [
        "get_piqi(OutputFormat, Options) ->\n",
        "    piqi_rpc_runtime:get_piqi(piqi(), OutputFormat, Options).\n",
        "\n\n",
        "\n\n",
        "rpc(Mod, Name, InputData, _InputFormat, _OutputFormat, Options) ->\n",
        "    try\n",
        "    case Name of\n",
            iod("\n", FuncClauses), "\n",
        "        _ ->\n",
        "            piqi_rpc_runtime:handle_unknown_function()\n"
        "    end\n",
        "    catch\n",
        "        Class:Reason:Stacktrace -> piqi_rpc_runtime:handle_runtime_exception(Class, Reason, Stacktrace, Options)\n",
        "    end.\n"
    ].


gen_func_clause(F, Mod) ->
    Name = F#func.name,
    ErlName = F#func.erlang_name,
    ScopedName = [ Mod, "/", Name ],
    InputCode =
        case F#func.input of
            'undefined' -> % the function doesn't have input
                [
"            piqi_rpc_runtime:check_empty_input(InputData),\n",
"            case piqi_rpc_runtime:call(Mod, ",  func_name(ErlName), ", 'undefined') of\n"
                ];
            _ ->
                ParseFun = ["fun parse_", ErlName, "_input/1"],
                [
"            Input = piqi_rpc_runtime:decode_input(?MODULE, ", ParseFun, ",\n"
"               <<\"", ScopedName, "-input\">>, _InputFormat, InputData, Options),\n"
"            case piqi_rpc_runtime:call(Mod, ", func_name(ErlName), ", Input) of\n"
                ]
        end,

    OutputCode =
        case F#func.output of
            'undefined' -> % the function doesn't produce output
"                ok -> ok";
            _ ->
                EncodeFun = ["fun gen_", ErlName, "_output/1"],
                [
"                {ok, Output} ->\n"
"                    piqi_rpc_runtime:encode_output(?MODULE, ", EncodeFun, ",\n"
"                       <<\"", ScopedName, "-output\">>, _OutputFormat, Output, Options)"
                ]
        end,

    ErrorCode =
        case F#func.error of
            'undefined' -> % the function doesn't produce errors
                [];
            _ ->
                ErrEncodeFun = ["fun gen_", ErlName, "_error/1"],
                [[
"                {error, Error} ->\n"
"                    piqi_rpc_runtime:encode_error(?MODULE, ", ErrEncodeFun, ",\n",
"                       <<\"", ScopedName, "-error\">>, _OutputFormat, Error, Options)"
                ]]
        end,

    DefaultCaseCode = [
"                X -> piqi_rpc_runtime:handle_invalid_result(Name, X)"
    ],

    Code = [
"        <<\"", Name, "\">> ->\n",
                InputCode,
                iod(";\n", [OutputCode] ++ ErrorCode ++ [DefaultCaseCode]),
                "\n",
"            end;\n"
    ],
    Code.



func_name(ErlName) ->
    ["rpc_handle_", ErlName].


gen_callbacks(Context) ->
    Piqi = Context#context.piqi,
    FuncList = Piqi#piqi.func,

    iod("\n", [gen_callback(Context, F) || F <- FuncList]).


gen_callback(Context, F) ->
    Input =
        case F#func.input of
            'undefined' -> "'undefined'";
            _ ->
                InputType = gen_scoped_param_typename(Context, F, "input"),
                [ InputType, "()" ]
        end,

    Output =
        case F#func.output of
            'undefined' -> "ok";
            _ ->
                OutputType = gen_scoped_param_typename(Context, F, "output"),
                [ "{ok, ", OutputType, "()}" ]
        end,

    Error =
        case F#func.error of
            'undefined' -> "";
            _ ->
                ErrorType = gen_scoped_param_typename(Context, F, "error"),
                [ " |\n    {error, ", ErrorType, "()}" ]
        end,

    [
        "-callback ", func_name(F#func.erlang_name), "(", Input, ") ->\n",
        "    ", Output,
        Error, ".\n"
    ].


gen_scoped_param_typename(Context, F, Param) ->
    ErlTypeName = gen_param_typename(Context, F, Param),
    piqic:scoped_erlname(Context, ErlTypeName).


gen_param_typename(Context, F, Param) ->
    TypeName = to_string(F#func.name) ++ "-" ++ Param,
    {_Parent, Typedef} = piqic:resolve_type_name(Context, TypeName),
    piqic:typedef_erlname(Typedef).


% generate Piqi-RPC default implementation
gen_defaults(Context) ->
    Piqi = Context#context.piqi,
    ErlMod = Piqi#piqi.erlang_module,
    FuncList = Piqi#piqi.func,

    [
        "%-behaviour(", ErlMod, ").\n\n",
        [gen_export(F) || F <- FuncList],
        "\n",
        iod("\n", [gen_default_impl(Context, X) || X <- FuncList])
    ].


gen_export(F) ->
    ["%-export([", func_name(F#func.erlang_name), "/1]).\n"].


gen_default_impl(Context, F) ->
    Piqi = Context#context.piqi,
    ErlMod = Piqi#piqi.erlang_module,
    ErlName = F#func.erlang_name,
    Input =
        case F#func.input of
            'undefined' -> "'undefined'";
            _ -> "_Input"
        end,

    Output =
        case F#func.output of
            'undefined' -> "ok";
            _ ->
                [ "{ok, ", ErlMod, "_rpc:default_", ErlName, "_output()}" ]
        end,

    [ func_name(ErlName), "(", Input, ") -> ", Output, ".\n" ].
