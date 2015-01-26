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

%% This module defines common functions used by piqic_erlang*.erl (and,
%% potentially, other piqi compilers)

-module(piqic).
-compile(export_all).


-define(__PIQIC_ERL__, 1).  % prevent -import'ing our own functions
-include("piqic.hrl").


-define(DEBUG,1).
-include("debug.hrl").

-define(FLAG_NORMALIZE, piqic_erlang_normalize).

%
% Piqi-specific utility functions
%


typedef_name({piqi_record, X}) -> X#piqi_record.name;
typedef_name({variant, X}) -> X#variant.name;
typedef_name({enum, X}) -> X#enum.name;
typedef_name({alias, X}) -> X#alias.name;
typedef_name({piqi_list, X}) -> X#piqi_list.name.


typedef_erlname({piqi_record, X}) -> X#piqi_record.erlang_name;
typedef_erlname({variant, X}) -> X#variant.erlang_name;
typedef_erlname({enum, X}) -> X#enum.erlang_name;
typedef_erlname({alias, X}) -> X#alias.erlang_name;
typedef_erlname({piqi_list, X}) -> X#piqi_list.erlang_name.


% check whether the piqi module is a self-specification, i.e. piqi.piqi or
% piqi.X.piqi
%
% XXX: this check is an approximation of the orignal criteria that says a module
% is a self-spec if it is named piqi or includes a module named piqi. We can't
% know this for sure, because information about included modules is, by design,
% not preserved in "piqi compile" output
is_self_spec(Piqi) ->
    Basename = basename(Piqi#piqi.module),
    case string:tokens(Basename, ".") of
        ["piqi"|_] ->
            true;
        _ ->
            false
    end.


% check whther the piqi module depends on "piqi-any" type (i.e. one of its
% definitions has piqi-any as field/option/alias type
piqi_depends_on_piqi_any(Piqi) ->
    lists:any(fun typedef_depends_on_piqi_any/1, Piqi#piqi.typedef).


typedef_depends_on_piqi_any(_Typedef = {Type, X}) ->
    case Type of
        piqi_record ->
            lists:any(fun (F) -> is_piqi_any(F#field.type) end, X#piqi_record.field);
        variant ->
            lists:any(fun (O) -> is_piqi_any(O#option.type) end, X#variant.option);
        alias ->
            is_piqi_any(X#alias.type);
        piqi_list ->
            is_piqi_any(X#piqi_list.type);
        enum ->
            false
    end.


is_piqi_any(Name) ->
    Name =:= <<"piqi-any">>.


% initialize piqic context from the list of Piqi modules; the module being
% compiled is the last one in the list; the preceding modules are all of its
% imported dependencies
init_context(PiqiList, Options) ->
    put(?FLAG_NORMALIZE, proplists:get_value(normalize_names, Options)),

    % set erlang_name fields by turning each identifier into Erlang-compliant
    % identifier

    PiqiList2 = [erlname_piqi(X) || X <- PiqiList],

    {Imports, [Piqi]} = lists:split(length(PiqiList2) - 1, PiqiList2),

    IsSelfSpec = is_self_spec(Piqi),
    SelfSpec =
        case IsSelfSpec of
            true ->
                Piqi;
            false ->
                S = load_self_spec(),
                erlname_piqi(S)
        end,
    BuiltinTypedefs =
        case IsSelfSpec of
            true ->
                % for self-specs, all build-in types should be defined inside
                % XXX: remove unused built-in typedefs from generated self-spec?
                [];
            false ->
                [X || X <- SelfSpec#piqi.typedef, is_builtin_typedef(X)]
        end,
    BuiltinsIndex = index_typedefs(BuiltinTypedefs),

    Piqi2 = add_builtin_typedefs(Piqi, BuiltinsIndex),
    Imports2 = [add_builtin_typedefs(X, BuiltinsIndex) || X <- Imports],

    % index the compiled module's contents
    Index = index_module(Piqi2),

    % index imported modules
    ModIndex = make_module_index(Imports2),

    #context{
        piqi = Piqi2,
        index = Index,

        is_self_spec = IsSelfSpec,

        modules = PiqiList,
        module_index = ModIndex,

        options = Options
    }.


get_option(Context, Name) ->
    proplists:get_value(Name, Context#context.options).


switch_context(Context, Piqi) ->
    CurrentPiqi = Context#context.piqi,
    % efficient way to check whether we are dealing with the current module
    case CurrentPiqi#piqi.module =:= Piqi#piqi.module of
        true ->
            Context;
        false ->
            Index = fetch(Piqi#piqi.module, Context#context.module_index),
            Context#context{
                piqi = Piqi,
                index = Index
            }
    end.


gen_parent_mod(Context, ParentPiqi) ->
    CurrentPiqi = Context#context.piqi,
    % efficient way to compare whether we are dealing with the current module
    case CurrentPiqi#piqi.module =:= ParentPiqi#piqi.module of
        true ->
            "";
        false ->
            [ParentPiqi#piqi.erlang_module, ":"]
    end.


scoped_name(Context, Name) ->
    Piqi = Context#context.piqi,
    Piqi#piqi.erlang_type_prefix ++ to_string(Name).


typedef_scoped_name(Context, Typedef) ->
    Name = typedef_name(Typedef),
    case piqic:is_builtin_typedef(Typedef) of
        true ->
            Name;
        false ->
            Piqi = Context#context.piqi,
            Mod = Piqi#piqi.module,
            [Mod, "/", Name]
    end.


% append the list of built-in typedefs that are actually referenced by the
% module
add_builtin_typedefs(Piqi, BuiltinsIndex) ->
    % exclude builtin typedefs that are masked by the local typedefs
    TypedefNames = [typedef_name(X) || X <- Piqi#piqi.typedef],
    BuiltinsIndex2 = orddict:filter(
        fun(Name, _) -> not lists:member(Name, TypedefNames) end,
        BuiltinsIndex
    ),
    UsedBuiltinTypedefs = get_used_builtin_typedefs(Piqi#piqi.typedef, BuiltinsIndex2),
    % change the module as if the built-ins were defined locally
    Piqi#piqi{
        typedef = UsedBuiltinTypedefs ++ Piqi#piqi.typedef
    }.


is_builtin_alias(X) ->
    % presence of piqi_type field means this alias typedef corresponds to one of
    % built-in types
    X#alias.piqi_type =/= 'undefined'.


is_builtin_typedef({alias, X}) ->
    is_builtin_alias(X);
is_builtin_typedef(_) ->
    false.


get_used_builtin_typedefs([], _BuiltinsIndex) ->
    [];
get_used_builtin_typedefs(Typedefs, BuiltinsIndex) ->
    TypeNames = lists:usort(lists:append([get_used_typenames(X) || X <- Typedefs])),
    BuiltinTypeNames = [N || N <- TypeNames, find(N, BuiltinsIndex) =/= 'undefined'],
    BuiltinTypedefs = [fetch(N, BuiltinsIndex) || N <- BuiltinTypeNames],
    % get built-in types' dependencies (that are also built-in types) -- usually
    % no more than 2-3 recursion steps is needed
    Res = get_used_builtin_typedefs(BuiltinTypedefs, BuiltinsIndex) ++ BuiltinTypedefs,
    lists:usort(Res).


get_used_typenames(_Typedef = {Type, X}) ->
    L =
        case Type of
            piqi_record ->
                [F#field.type || F <- X#piqi_record.field];
            variant ->
                [O#option.type || O <- X#variant.option];
            alias ->
                [X#alias.type];
            piqi_list ->
                [X#piqi_list.type];
            enum ->
                []
        end,
    lists:usort([N || N <- L, N =/= 'undefined']).


load_self_spec() ->
    PiqiBin = hd(piqi_piqi:piqi()),
    Buf = piqirun:init_from_binary(PiqiBin),
    piqi_piqi:parse_piqi(Buf).


% NOTE: this function is currently unused
load_self_spec_from_file() ->
    {ok, Bytes} = file:read_file("piqi.piqi.pb"),
    Buf = piqirun:init_from_binary(Bytes),
    piqi_piqi:parse_piqi(Buf).


% make an index of #piqi.module -> #index{}
make_module_index(PiqiList) ->
    L2 = [{X#piqi.module, index_module(X)} || X <- PiqiList],
    orddict:from_list(L2).


% generate an index of all imports and definitions of a given module
index_module(Piqi) ->
    #index{
        piqi = Piqi,
        import = index_imports(Piqi#piqi.import),
        typedef = index_typedefs(Piqi#piqi.typedef)
    }.


% index imports by name
index_imports(L) ->
    L2 = [{make_import_name(X), X} || X <- L],
    orddict:from_list(L2).


make_import_name(X) ->
    case X#import.name of
        'undefined' ->
            X#import.module;
        Name ->
            Name
    end.


% index typedefs by name
index_typedefs(L) ->
    L2 = [{typedef_name(X), X} || X <- L],
    orddict:from_list(L2).


fetch(Key, Orddict) ->
    BinKey = to_binary(Key),
    orddict:fetch(BinKey, Orddict).


find(Key, Orddict) ->
    BinKey = to_binary(Key),
    case orddict:find(BinKey, Orddict) of
        {ok, Value} ->
            Value;
        error ->
            'undefined'
    end.


-spec resolve_type_name(#context{}, binary() | string()) -> {Parent :: piqi(), typedef()}.

% resolve type name to its type definition and the module where it was defined
resolve_type_name(Context, TypeName) ->
    %?PRINT({resolve_type_name, Context#context.piqi#piqi.module, TypeName}),
    S = to_string(TypeName),
    Index = Context#context.index,
    case string:tokens(S, "/") of
        [Name] ->
            % NOTE: this will also resolve built-in types
            resolve_local_type_name(Index, Name);
        [ImportName, Name] ->  % imported type
            Import = fetch(ImportName, Index#index.import),
            ImportedIndex = resolve_import(Context, Import),
            resolve_local_type_name(ImportedIndex, Name)
    end.


resolve_local_type_name(Index, Name) ->
    Typedef = fetch(Name, Index#index.typedef),
    {Index#index.piqi, Typedef}.


-spec resolve_import(#context{}, import()) -> #index{}.

% resolve import to the imported module's index
resolve_import(Context, Import) ->
    _ImportedIndex = fetch(Import#import.module, Context#context.module_index).


% set erlang_name fields by turning each identifier into Erlang-compliant
% identifier
erlname_piqi(Piqi) ->
    Mod = Piqi#piqi.module,  % always defined in "piqi compile" output

    % Erlang module name derived from Piqi module name
    DerivedMod = erlname(basename(Mod)),

    ErlTypePrefix = ?defined(
        Piqi#piqi.erlang_type_prefix,
        ?defined(Piqi#piqi.erlang_module, DerivedMod) ++ "_"
    ),
    ErlMod = ?defined(Piqi#piqi.erlang_module, DerivedMod ++ "_piqi"),

    Piqi#piqi{
        erlang_module = ErlMod,
        erlang_type_prefix = to_string(ErlTypePrefix),
        typedef = [erlname_typedef(X) || X <- Piqi#piqi.typedef],
        func = [erlname_func(X) || X <- Piqi#piqi.func]
    }.


erlname_func(X) ->
    X#func{
        erlang_name = erlname_undefined(X#func.erlang_name, X#func.name)
    }.


erlname_typedef({Type, X}) ->
    X2 =
        case Type of
            piqi_record -> erlname_record(X);
            variant -> erlname_variant(X);
            enum -> erlname_enum(X);
            alias -> erlname_alias(X);
            piqi_list -> erlname_list(X)
        end,
    {Type, X2}.


erlname_record(X) ->
    X#piqi_record{
        erlang_name = erlname_undefined(X#piqi_record.erlang_name, X#piqi_record.name),
        field = [erlname_field(F) || F <- X#piqi_record.field]
    }.


erlname_field(X) ->
    X#field{
        erlang_name = erlname_undefined(X#field.erlang_name, X#field.name)
    }.


erlname_variant(X) ->
    X#variant{
        erlang_name = erlname_undefined(X#variant.erlang_name, X#variant.name),
        option = [erlname_option(O) || O <- X#variant.option]
    }.


erlname_option(X) ->
    X#option{
        erlang_name = erlname_undefined(X#option.erlang_name, X#option.name)
    }.


erlname_enum(X) ->
    X#enum{
        erlang_name = erlname_undefined(X#enum.erlang_name, X#enum.name),
        option = [erlname_option(O) || O <- X#enum.option]
    }.


erlname_alias(X) ->
    X#alias{
        erlang_name = erlname_undefined(X#alias.erlang_name, X#alias.name)
    }.


erlname_list(X) ->
    X#piqi_list{
        erlang_name = erlname_undefined(X#piqi_list.erlang_name, X#piqi_list.name)
    }.


erlname_undefined(ErlName, Name) ->
    ?defined(ErlName, erlname(Name)).


erlname('undefined') ->
    'undefined';
erlname(X) ->
    case get(?FLAG_NORMALIZE) of
        true -> normalize(to_string(X));
        false -> dashes_to_underscores(uncapitalize(X))
    end.

capitalize(X) ->
    S = to_string(X),
    case S of
        [H | T] when H >= $a andalso H =< $z ->
            [H - $a + $A | T];
        _ ->
            S
    end.

is_upper(X) when X >= $A andalso X =< $Z -> true;
is_upper(_) -> false.

normalize([]) ->
    [];
normalize([H|_]=S) ->
    normalize(is_upper(H), [], S).

normalize(_, Acc, []) ->
    lists:reverse(Acc);
normalize(_, Acc, [H|T]) when H =:= $_ orelse H =:= $- ->
    normalize(true, [$_|Acc], T);
normalize(_, Acc, [H|T]) when H =:= $. orelse H =:= $: orelse H =:= $/ ->
    normalize(true, [H|Acc], T);
normalize(true, Acc, [H|T]) ->
    normalize(is_upper(H), [uncapitalize_c(H)|Acc], T);
normalize(false, Acc, [H|T]) ->
    case is_upper(H) of
        true -> normalize(true, [uncapitalize_c(H), $_ |Acc], T);
        false -> normalize(false, [H|Acc], T)
    end.

uncapitalize(X) ->
    S = to_string(X),
    case S of
        [H | T] ->
            [uncapitalize_c(H) | T];
        _ ->
            S
    end.

uncapitalize_c(V) ->
    case is_upper(V) of
        true -> V - $A + $a;
        false -> V
    end.

basename(X) ->
    S = to_string(X),
    case string:rchr(S, $/) of
        0 ->
            S;
        I ->
            string:substr(S, I)
    end.


is_localname(X) ->
    S = to_string(X),
    case string:rchr(S, $/) of
        0 ->
            true;
        _ ->
            false
    end.


can_be_protobuf_packed(Context, Typedef) ->
    case unalias(Context, Typedef) of
        int -> true;
        float -> true;
        bool -> true;
        {enum, _} -> true;  % enum values can be packed in Protobuf
        _ -> false
    end.


-spec unalias(#context{}, typedef()) -> typedef() | piqi_type().

% unwind aliases to the lowest-level non-alias typedef or one of the built-in
% primitive Piqi types
unalias(Context, {alias, A}) when A#alias.type =/= 'undefined' ->
    {ParentPiqi, AliasedTypedef} = resolve_type_name(Context, A#alias.type),
    ParentContext = switch_context(Context, ParentPiqi),
    unalias(ParentContext, AliasedTypedef);

unalias(_Context, {alias, A}) -> % when A#alias.piqi_type =/= 'undefined'
    A#alias.piqi_type;

unalias(_Context, Typedef) ->
    Typedef.


erlname_of_field(Context, X) ->
  erlname_of(Context, X#field.erlang_name, X#field.type).


erlname_of_option(Context, X) ->
  erlname_of(Context, X#option.erlang_name, X#option.type).


erlname_of(Context, Name, TypeName) ->
    ?defined(Name, type_erlname(Context, TypeName)).


type_erlname(Context, TypeName) ->
    {_ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
    typedef_erlname(Typedef).


gen_convert_value(Context, ErlType, Direction, TypeName, Value) ->
    case ErlType =/= 'undefined' andalso TypeName =/= 'undefined' of
        true ->  % custom Erlang type
            Name = type_erlname(Context, TypeName),
            [
                ErlType, Direction, Name, "(", Value, ")"
            ];
        false ->
            Value
    end.


gen_code(I) -> integer_to_list(I).


gen_field_mode(X) ->
    case X#field.mode of
        required ->
            "required";
        optional ->
            "optional";
        repeated ->
            ?if_true(X#field.protobuf_packed, "packed_repeated", "repeated")
    end.


get_default_wire_type(PiqiType) ->
    case PiqiType of
        int -> zigzag_varint;
        float -> fixed64;
        bool -> varint;
        _ -> block
    end.


gen_wire_type_name(PiqiType, WireType) ->
    WireType2 = ?defined(WireType, get_default_wire_type(PiqiType)),
    atom_to_list(WireType2).


% these cases are only used during bootstrap; normally .erlang-name property
% should always be defined and we'll never get to this point
gen_builtin_type_name(PiqiType) ->
    case PiqiType of
        int -> "integer";
        float -> "float";
        bool -> "boolean";
        string -> "string";
        binary -> "binary"
    end.


gen_builtin_type_name(PiqiType, ErlType) ->
    ?defined(  % always choose ErlType if it is defined
        ErlType,
        gen_builtin_type_name(PiqiType)
    ).


%
% General purpose utility functions
%

iod(_Delim, []) -> [];
iod(Delim, [[]|T]) ->  % skip empty lists
    iod(Delim, T);
iod(Delim, [H|T]) ->
    lists:foldl(
        fun ([], Accu) ->  % skip empty lists
                Accu;
            (X, Accu) ->
                [Accu, Delim, X]
        end,
        H, T
    ).


% convert all dashes to underscores in the input string
dashes_to_underscores(X) ->
    S = to_string(X),
    case lists:member($-, X) of
        true ->
            [dash_to_underscore(C) || C <- S];
        false ->
            X
    end.


dash_to_underscore($-) -> $_;
dash_to_underscore(X) -> X.


to_string(X) when is_binary(X) -> binary_to_list(X);
to_string(X) when is_list(X) -> X.


to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_list(X) -> list_to_binary(X).


write_file(Filename, Body) ->
    % TODO: error handing
    ok = file:write_file(Filename, iolist_to_binary(Body)).

