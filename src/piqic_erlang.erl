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

%% Piqi compiler for Erlang: top-level module and Escript entry point (see
%% main/1)

-module(piqic_erlang).
-compile([export_all, nowarn_export_all]).


-include("piqic.hrl").


% Escript entry point
main(Args) ->
    case piqic_erlang(Args) of
        ok ->
            ok;
        {error, ErrorStr} ->
            print_error(ErrorStr),
            erlang:halt(1)
    end.


% TODO: -v
usage() ->
    usage(standard_io).

usage(IoDevice) ->
    ScriptName = escript:script_name(),
    io:format(IoDevice, "~s", [
"Usage: " ++ ScriptName ++ " [options] <.piqi file>\n"
"Options:
  -I <dir> add directory to the list of imported .piqi search paths
  --include-lib <app>[/<path>] similar to -I but generate -include_lib(...) instead -include(...) for imported modules
  --no-warnings don't print warnings
  --trace turn on tracing
  --debug <level> debug level; any number greater than 0 turns on debug messages
  --no-builtin-types don't include built-in type definitions
  -C <output directory> specify output directory
  --strict treat unknown and duplicate fields as errors
  -e <name> try including extension <name> for all loaded modules (can be used several times)
  --normalize-names true|false turn CamlCase-style names into \"camel-case\" (default = true)
  --gen-preserve-unknown-fields generate code that preserves unknown Protobuf fields when they are serialized back
  --gen-embedded-runtime include serialization runtime in the generated .erl modules
  --gen-protobuf generate Protobuf serializers (overrides generating all multi-format serializers by default)
  --gen-defaults generate functions returning valid records with required fields & default implementations for piqi-rpc callback functions
  --gen-rpc generate piqi-rpc stubs
  -h, --help  Display this list of options
"
    ]).


print_error(ErrorStr) ->
    print_error(ErrorStr, []).

print_error(Format, Args) ->
    io:format(standard_error, "Error: " ++ Format ++ "\n", Args).


args_error(ErrorStr) ->
    args_error(ErrorStr, []).

args_error(Format, Args) ->
    print_error(Format, Args),
    erlang:halt(1).


-define(FLAG_TRACE, piqic_erlang_trace).
-record(args, {
    output_dir,         % -C
    trace = false,      % --trace
    input_file,         % last positional argument
    other = [],          % arguments to be passed to "piqi compile"
    normalize_names = true,
    gen_preserve_unknown_fields = false,
    gen_embedded_runtime = false,
    gen_protobuf = false,
    gen_defaults = false,
    gen_rpc = false,
    cc = false,        % compile spec for the piqic-erlang compiler

    % search path, similar to -I but generate -include_lib instead of -include
    % for imported modules
    include_lib = [] :: [ {AppPath :: string(), Path :: string()} ]
}).


% extract filename (last argument) and output directory (argument following -C)
parse_args(Args) ->
    parse_args(Args, #args{}).


parse_args([], _Args) ->
    args_error("missing name of the input .piqi file");

parse_args([X|_], _Args) when X == "-h" orelse X == "--help" ->
    usage(),
    erlang:halt(0);

parse_args([Filename], Args) ->
    Args#args{
        input_file = Filename,
        other = lists:reverse([Filename | Args#args.other]),
        include_lib = lists:reverse(Args#args.include_lib)
    };

parse_args(["-C", Odir |T], Args) ->
    NewArgs = Args#args{
        output_dir = Odir
    },
    parse_args(T, NewArgs);

parse_args(["--normalize-names" , Value |T], Args) ->
    Bool =
        case Value of
            "true" -> true;
            "false" -> false;
            _ -> args_error("true or false are expected as values for --normalize-names")
        end,
    NewArgs = Args#args{
        normalize_names = Bool
    },
    parse_args(T, NewArgs);

parse_args(["--gen-preserve-unknown-fields" |T], Args) ->
    NewArgs = Args#args{
        gen_preserve_unknown_fields = true
    },
    parse_args(T, NewArgs);

parse_args(["--gen-embedded-runtime" |T], Args) ->
    NewArgs = Args#args{
        gen_embedded_runtime = true
    },
    parse_args(T, NewArgs);

parse_args(["--gen-protobuf" |T], Args) ->
    NewArgs = Args#args{
        gen_protobuf = true
    },
    parse_args(T, NewArgs);

parse_args(["--gen-defaults" |T], Args) ->
    NewArgs = Args#args{
        gen_defaults = true
    },
    parse_args(T, NewArgs);

parse_args(["--gen-rpc" |T], Args) ->
    NewArgs = Args#args{
        gen_rpc = true
    },
    parse_args(T, NewArgs);

parse_args(["--cc" |T], Args) ->
    NewArgs = Args#args{
        cc = true
    },
    parse_args(T, NewArgs);

parse_args([A = "--trace" |T], Args) ->
    NewArgs = Args#args{
        trace = true,
        other = [A | Args#args.other]  % pass through
    },
    parse_args(T, NewArgs);

parse_args(["--include-lib", I |T], Args) ->
    {App, AppPath} =
        case filename:split(I) of
            [PathH | PathT] ->
                {list_to_atom(PathH), PathT};
            _ ->
                args_error("invalid --include-lib spec: ~s", [I])
        end,
    AppDir =
        case code:lib_dir(App) of
            {error, _} ->
                args_error("can't find app '~s' specified in --include-lib ~s", [App, I]);
            X ->
                X
        end,
    Path = filename:join([AppDir | AppPath]),
    NewArgs = Args#args{
        include_lib = [{I, Path} | Args#args.include_lib],

        % convert --include-lib to -I
        other = [Path, "-I" | Args#args.other]
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


piqic_erlang(Args) ->
    ParsedArgs = parse_args(Args),
    #args{
        input_file = Filename,
        output_dir = Odir,
        trace = Trace,
        other = OtherArgs
    } = ParsedArgs,

    % use process dictionary instead of carrying it through the stack
    put(?FLAG_TRACE, Trace),

    % NOTE: ideally, we should send self-spec to "piqic compile" stdin instead
    % of passing it as a temporary file. Unfortunately, Erlang port interface
    % doesn't support sending EOF so we can't use it without some kind of
    % adapter script.
    %
    % NOTE: using os:getpid() to avoid race between concurrent piqic-erlang
    % invocations (typically, during parallel builds)
    SelfSpec = "piqi.piqi." ++ os:getpid() ++ ".pb",
    piqic:write_file(SelfSpec, piqi_piqi:piqi()),

    % NOTE: it would be more better to read it from stdout, but it is not
    % possible to capture both stdout and sterr (which we need for warnings)
    % separately using Erlang standard library (so, we are capturing
    CompiledPiqi = Filename ++ ".pib",

    % capture the original Cwd so that we could restore it later
    Cwd = get_cwd(Odir),
    try
        PiqiCompile = lists:concat([
            find_piqi_executable(), " compile",
            " --self-spec ", SelfSpec,
            " -o ", CompiledPiqi,
            " -t pb",
            " -e erlang",  % automatically load .erlang.piqi extensions
            " ", join_args(OtherArgs)
        ]),
        run_piqi_compile(PiqiCompile),

        % read the compiled Piqi bundle: it is just a list containing the module
        % being compiled prepended by its import dependencies (if any)
        PiqiBundle = read_piqi_bundle(CompiledPiqi),
        PiqiList = PiqiBundle#piqi_bundle.piqi,

        % change CWD to output directory if it is defined
        set_cwd(Odir),

        % finally, generate code for the last module in the list; the preceding
        % modules (if any) represent imported dependencies
        Options = [
            {normalize_names, ParsedArgs#args.normalize_names},
            {gen_preserve_unknown_fields, ParsedArgs#args.gen_preserve_unknown_fields},
            {gen_embedded_runtime, ParsedArgs#args.gen_embedded_runtime},
            {gen_protobuf, ParsedArgs#args.gen_protobuf},
            {gen_defaults, ParsedArgs#args.gen_defaults},
            {gen_rpc, ParsedArgs#args.gen_rpc},
            {cc, ParsedArgs#args.cc},
            {include_lib, ParsedArgs#args.include_lib}
        ],
        Context = piqic:init_context(PiqiList, Options),
        generate(Context),
        ok
    catch
        {error, _} = Error -> Error
    after
        set_cwd(Cwd),  % restore the original CWD
        file:delete(SelfSpec),
        file:delete(CompiledPiqi)
    end.


set_cwd('undefined') ->ok;
set_cwd(Dir) ->
    ok = file:set_cwd(Dir).


get_cwd('undefined') -> 'undefined';
get_cwd(_NewCwd) ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.


% TODO: windows support
% TODO: this is almost exact copy of piqi:find_piqi()
% XXX: check for version compatibility
find_piqi_executable() ->
    KernelName = os:cmd("uname -s") -- "\n",
    Machine = os:cmd("uname -m") -- "\n",
    Arch = lists:concat([KernelName, "-", Machine]),
    % path to "piqi" executable within "piqi" application directory
    AppPath = filename:join(["priv", "piqi-binary", Arch, "piqi"]),
    try
        case os:getenv("PIQI") of
            false ->
                ok;
            PiqiName ->
                throw_return(PiqiName)
        end,
        case os:getenv("REBAR_DEPS_DIR") of
            false ->
                ok;
            RebarDepsDir ->
                file_exists(filename:join([RebarDepsDir, "piqi", AppPath]))
        end,
        case code:lib_dir(piqi) of
            {error, _Error} ->
                ok;
            PiqiLibPath ->
                file_exists(filename:join(PiqiLibPath, AppPath))
        end,
        case os:find_executable("piqi") of
            false ->
                throw_error("can't find \"piqi\" executable");
            Filename ->
                file_exists(Filename)
        end
    catch
        {return, X} -> X
    end.


% check if the file exists and do a non-local return if it does
file_exists(Name) ->
    trace("checking existence of \"~s\"~n", [Name]),
    case filelib:is_regular(Name) of
        true ->
            throw_return(Name);
        false ->
            ok
    end.


throw_return(X) -> throw({return, X}).
throw_error(X) -> throw({error, X}).


throw_error(Fmt, Args) ->
    throw_error(lists:flatten(io_lib:format(Fmt, Args))).


trace(Format, Args) ->
    case get(?FLAG_TRACE) of
        true ->
            io:format(standard_error, Format, Args);
        _ ->
            ok
    end.


run_piqi_compile(Cmd) ->
    trace("running: ~s~n", [Cmd]),
    case eunit_lib:command(Cmd) of
        {0, Output} ->
            % repeat "piqi compile" warnings and traces on stderr (the way we
            % call it, "piqi compile" shouldn't produce anything on stdout)
            io:put_chars(standard_error, Output),
            ok;
        {_Code, Error} ->
            throw_error("command \"" ++ Cmd ++ "\" failed with error: " ++ Error)
    end.


read_piqi_bundle(Filename) ->
    {ok, Bytes} = file:read_file(Filename),
    piqi_piqi:parse_piqi_bundle(Bytes).


generate(Context) ->
    gen_hrl(Context),
    gen_erl(Context),

    % only when compiling piqi.piqi for this library
    piqic:get_option(Context, cc) andalso gen_piqi_any_piqi_hrl(Context),
    ok.


gen_piqi_any_piqi_hrl(Context) ->
    ErlMod = "piqi_any_piqi",
    Filename = ErlMod ++ ".hrl",
    HeaderMacro = ["__", string:to_upper(ErlMod), "_HRL__"],
    Code = [
        "-ifndef(", HeaderMacro, ").\n"
        "-define(", HeaderMacro, ", 1).\n\n\n",
        piqic_erlang_types:gen_piqi_any(Context),
        "\n\n-endif.\n"
    ],
    piqic:write_file(Filename, Code).


gen_include_piqi_any_piqi_hrl(Context, Piqi) ->
    % decide if we need to include piqi_any() type definition or not
    case piqic:piqi_depends_on_piqi_any(Piqi) of
        false -> [];
        true ->
            % include #piqi_any{} and piqi_any() definitions
            case is_embedded_runtime(Context) of
                false ->
                    ["\n-include_lib(\"piqi/include/piqi_any_piqi.hrl\").\n"];
                true ->
                    Contents = read_embedded_runtime_file("include/piqi_any_piqi.hrl"),
                    [
                        "\n\n",
                        Contents
                    ]
            end
    end.


gen_hrl(Context) ->
    Piqi = Context#context.piqi,
    ErlMod = to_string(Piqi#piqi.erlang_module),
    Filename = ErlMod ++ ".hrl",
    HeaderMacro = ["__", string:to_upper(ErlMod), "_HRL__"],
    Code = [
        "-ifndef(", HeaderMacro, ").\n"
        "-define(", HeaderMacro, ", 1).\n",
        gen_include_piqi_any_piqi_hrl(Context, Piqi),
        "\n\n",
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
            "-compile([export_all, nowarn_export_all]).\n\n",
            "-include(\"", ErlMod, ".hrl\").\n"
        ],
	gen_include_piqirun_hrl(Context),

        case piqic:get_option(Context, gen_rpc) of
            false -> "";
            true -> piqic_erlang_rpc:gen_callbacks(Context)
        end,

        piqic_erlang_out:gen_piqi(Context),
        piqic_erlang_in:gen_piqi(Context),

        case piqic:get_option(Context, gen_defaults) of
            false -> "";
            true -> piqic_erlang_defaults:gen_piqi(Context)
        end,

        case piqic:is_protobuf_only(Context) of
            true -> "";
            false ->
                gen_embedded_piqi(Context)
        end,

        case piqic:get_option(Context, gen_rpc) of
            false -> "";
            true ->
                iod("\n\n", [
                    piqic_erlang_rpc:gen_piqi(Context),
                    case piqic:get_option(Context, gen_defaults) of
                        false -> "";
                        true ->
                            piqic_erlang_rpc:gen_defaults(Context)
                    end
                ])
        end,

	gen_piqirun_erl(Context)
    ]),
    piqic:write_file(Filename, Code).


gen_include_piqirun_hrl(Context) ->
    case is_embedded_runtime(Context) of
        false ->
            "-include_lib(\"piqi/include/piqirun.hrl\").\n-include_lib(\"piqi/include/piqi_tools.hrl\").";
        true ->
            Contents = read_embedded_runtime_file("include/piqirun.hrl"),
            [
                "\n\n",
                Contents,
                "\n\n"
            ]
    end.


gen_piqirun_erl(Context) ->
    case is_embedded_runtime(Context) of
        false ->
            "";
        true ->
            Contents = read_embedded_runtime_file("src/piqirun_embedded.hrl"),
            [
                "\n\n",
                Contents
            ]
    end.


is_embedded_runtime(Context) ->
    piqic:get_option(Context, gen_embedded_runtime).


read_embedded_runtime_file(Filename) ->
    Dir =
        case code:lib_dir(piqi) of
            {error, _} ->
                throw_error("can't find \"piqi\" app directory when generating embedded runtime");
            Dir_ ->
                Dir_
        end,

    Path = filename:join(Dir, Filename),
    filelib:is_regular(Path) orelse throw_error("can't find runtime file \"~s\" for embedding into generated .erl", [Path]),

    {ok, Bytes} = file:read_file(Path),
    Bytes.


gen_embedded_piqi(Context) ->
    % TODO: the way we pass piqi modules to "piqi server" needs to be revised:
    % we should probably pass the whole piqi-bundle instead of a list of
    % pb-encoded piqi modules
    BinModules = [iolist_to_binary(piqi_piqi:gen_piqi(X)) || X <- Context#context.modules],
    Hash = erlang:phash2(BinModules, 16#100000000),  % 32-bit hash of the piqi modules
    [
        "piqi_hash() -> ", integer_to_list(Hash), ".\n",
        "piqi() ->\n",
        io_lib:format("~p", [BinModules]),
        ".\n"
    ].
