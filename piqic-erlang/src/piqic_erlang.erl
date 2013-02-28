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

%% Piqi compiler for Erlang
%%
%% This module also defines common functions reused by code generators
%% implemented in piqic_erlang_*.erl

-module(piqic_erlang).
-compile(export_all).


-include("piqic.hrl").


% TODO: check that the version of this plugin and "piqic version" are exactly
% the same


% Escript entry point
main(Args) ->
    % call piqic_erlangwith command-line arguments and ?MODULE as the
    % callback module.
    piqic_erlang(?MODULE, Args).


usage() ->
    ScriptName = escript:script_name(),
    io:format(
"Usage: " ++ ScriptName ++ " [options] <.piqi file>\n"
"Options:
  -I <dir> add directory to the list of imported .piqi search paths
  --no-warnings don't print warnings
  --trace turn on tracing
  --debug <level> debug level; any number greater than 0 turns on debug messages
  --no-builtin-types don't include built-in type definitions
  -C <output directory> specify output directory
  --strict treat unknown and duplicate fields as errors
  -e <name> try including extension <name> for all loaded modules (can be used several times)
  --normalize-names turn CamlCase-style names into \"camel-case\" (lowercase & separate words with dashes)
  --gen-defaults generate default value constructors for generated types
  -h, --help  Display this list of options
"
    ).


args_error(Error) ->
    % TODO: print to stderr
    io:format("Error: ~s~n", [Error]),
    usage(),
    erlang:halt(1).

-record(args, {
    output_dir,  % -C
    input_file,  % 1st positional arguments
    other = []    % arguments to be passed to "piqi compile"
}).


% extract filename (last argument) and output directory (argument following -C)
parse_args([]) ->
    args_error("missing name of the input .piqi file");

parse_args([X]) when X == "-h" orelse X == "--help" ->
    usage(),
    erlang:halt(0);

parse_args(Args) ->
    parse_args(Args, #args{}).


parse_args([Filename], Args) ->
    Args#args{
        input_file = Filename,
        other = lists:reverse([Filename | Args#args.other])
    };

parse_args(["-C", Odir |T], Args) ->
    NewArgs = Args#args{
        output_dir = Odir
    },
    parse_args(T, NewArgs);

parse_args([A|T], Args) ->
    NewArgs = Args#args{
        other = [A | Args#args.other]
    },
    parse_args(T, NewArgs).


join_args(Args) ->
    Args1 = [escape_arg(X) || X <- Args],
    string:join(Args1, " ").


escape_arg(X) ->
    % TODO: escape $'
    case lists:member($\ , X) of
        true -> "'" ++ X ++ "'";
        false -> X
    end.


set_cwd('undefined') -> ok;
set_cwd(Dir) ->
    ok = file:set_cwd(Dir).


% `Mod` parameter is the name of the Erlang module that exports two callback
% functions:
%
%       gen_piqi(Piqi)
%
%       custom_args()
%
piqic_erlang(_Mod, Args) ->
    #args{
        input_file = Filename,
        output_dir = Odir,
        other = OtherArgs
    } = parse_args(Args),

    SelfSpec = "piqi.piqi.pb",
    % FIXME
    piqic:write_file(SelfSpec, piqi_piqi:piqi()),
    CompiledPiqi = Filename ++ ".pb",
    try
        % call the base compiler "piqi compile"
        PiqiCompile = lists:concat([
            %piqi:get_command("piqi"), " compile",
            "piqi compile",
            " --self-spec ", SelfSpec,
            " -o ", CompiledPiqi,
            " -t pb",
            " ", join_args(OtherArgs)
        ]),
        command(PiqiCompile),

        % read the compiled Piqi bundle: it is just a list containing the module
        % being compiled prepended by its import dependencies (if any)
        PiqiBundle = read_piqi_bundle(CompiledPiqi),
        PiqiList = PiqiBundle#piqi_bundle.piqi,

        % generate the code for the last module in the list 
        set_cwd(Odir),
        generate_code(PiqiList)
    after
        file:delete(CompiledPiqi),
        % FIXME
        file:delete(SelfSpec)
    end.


command(Cmd) ->
    %os:cmd(Cmd).
    case eunit_lib:command(Cmd) of
        {0, X} ->
            io:put_chars(X),
            ok;
        {_Code, Error} ->
            io:format("command \"~s\" failed with error: ~s~n", [Cmd, Error]),
            erlang:halt(1)
    end.


read_piqi_bundle(Filename) ->
    {ok, Bytes} = file:read_file(Filename),
    Buf = piqirun:init_from_binary(Bytes),
    piqi_piqi:parse_piqi_bundle(Buf).


% generate code for the last module in the list; the preceding modules (if any)
% are imported dependencies
generate_code(PiqiList) ->
    Context = piqic:init_context(PiqiList),
    gen_hrl(Context),
    gen_erl(Context),
    ok.


gen_hrl(Context) ->
    Piqi = Context#context.piqi,
    ErlMod = to_string(Piqi#piqi.erlang_module),
    Filename = ErlMod ++ ".hrl",
    HeaderMacro = ["__", string:to_upper(ErlMod), "_HRL__"],
    Code = [
        "-ifndef(", HeaderMacro, ").\n"
        "-define(", HeaderMacro, ", 1).\n\n\n",
        piqic_erlang_types:gen_piqi(Context),
        "\n\n-endif.\n"
    ],
    piqic:write_file(Filename, Code).


gen_erl(Context) ->
    Piqi = Context#context.piqi,
    ErlMod = to_string(Piqi#piqi.erlang_module),
    Filename = ErlMod ++ ".erl",
    Code = iod("\n\n", [
        [
            "-module(", ErlMod, ").\n",
            "-compile(export_all).\n\n",
            "-include_lib(\"piqi/include/piqirun.hrl\").\n",
            "-include(\"", ErlMod, ".hrl\").\n"
        ],
        piqic_erlang_out:gen_piqi(Context),
        piqic_erlang_in:gen_piqi(Context),
        piqic_erlang_defaults:gen_piqi(Context),
        gen_embedded_piqi(Context)
    ]),
    piqic:write_file(Filename, Code).


gen_embedded_piqi(Context) ->
    % TODO: the way we pass piqi modules to "piqi server" needs to be revised:
    % we should probably pass the whole piqi-bundle instead of a list of
    % pb-encoded piqi modules
    BinModules = [iolist_to_binary(piqi_piqi:gen_piqi(X)) || X <- Context#context.modules],
    [
        "piqi() ->\n",
        io_lib:format("~p", [BinModules]),
        ".\n"
    ].

