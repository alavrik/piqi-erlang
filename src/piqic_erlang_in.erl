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

% generation of parse_*/1,2,3 functions for Protocol Buffers, JSON, XML and Piq
% deserialization
-module(piqic_erlang_in).
-compile(export_all).


-include("piqic.hrl").


-define(DEBUG,1).
-include("debug.hrl").


gen_piqi(Context) ->
    Piqi = Context#context.piqi,
    gen_typedefs(Context, Piqi#piqi.typedef).


gen_typedefs(Context, Typedefs) ->
    iod("\n\n",
        [gen_typedef(Context, X) || X <- Typedefs]
        ++
        [gen_typedef_multiformat(Context, X) || X <- Typedefs]
    ).


% Protobuf parsers
gen_typedef(Context, Typedef = {Type, X}) ->
    Spec = gen_spec(Context, Typedef),
    ParseFun =
        case Type of
            piqi_record ->
                gen_record(Context, X);
            variant ->
                gen_variant(Context, X);
            piqi_list ->
                gen_list(Context, X);
            enum ->
                gen_enum(X);
            alias ->
                gen_alias(Context, X)
        end,
    [
        Spec, "\n",
        ParseFun
    ].


% mutliformat parsers: parse_*/2, parse_*/3
% TODO: generate -specs
gen_typedef_multiformat(Context, Typedef) ->
    Piqi = Context#context.piqi,
    Mod = Piqi#piqi.module,
    Name = typedef_name(Typedef),
    ScopedName = [Mod, "/", Name],
    ErlName = typedef_erlname(Typedef),
    [
        gen_typedef_2(ScopedName, ErlName),
        "\n\n",
        gen_typedef_3(ScopedName, ErlName)
    ].


% mutliformat parsers
gen_typedef_2(_Name, ErlName) ->
    [
        "parse_", ErlName, "(X, Format) ->\n",
        "    ", "parse_", ErlName, "(X, Format, []).\n"
    ].


% mutliformat parsers with additional options
gen_typedef_3(Name, ErlName) ->
    [
        "parse_", ErlName, "(X, Format, Options) ->\n",
        "    ", "parse_", ErlName, "(\n",
        "        ", piqic_erlang_out:gen_convert(Name, "Format", "'pb'", "X, Options"), ").\n"
    ].


gen_spec(Context, Typedef) ->
    [
        "-spec parse_", typedef_erlname(Typedef), "/1 :: ",
        "(X :: piqirun_buffer()) -> ",
        gen_input_type_name(Context, Typedef),
        ".\n"
    ].


gen_input_type_name(Context, Typedef) ->
    % un-alias to avoid Dialyzer complaints like this one: "... states that the
    % function might also return string() but the inferred return is binary()"
    {Context2, AliasedTypeName} = unalias_input_typedef(Context, Typedef),
    T = piqic_erlang_types:gen_type(Context2, AliasedTypeName),

    % strings can be parsed either as lists or as binaries depending on the
    % "erlang-string-type" per-module setting
    Piqi = Context2#context.piqi,
    case to_string(T) of
        "string" ->
            case Piqi#piqi.erlang_string_type of
                list ->
                    "string()";
                binary ->
                    "binary()"
            end;
        S ->
            S ++ "()"
    end.


unalias_input_typedef(Context, {alias, A})
        when A#alias.erlang_type =:= 'undefined' andalso A#alias.type =/= 'undefined' ->
    {ParentPiqi, Typedef} = resolve_type_name(Context, A#alias.type),
    ParentContext = piqic:switch_context(Context, ParentPiqi),
    unalias_input_typedef(ParentContext, Typedef);

unalias_input_typedef(Context, Typedef) ->
    {Context, typedef_name(Typedef)}.


gen_alias(Context, X) ->
    case piqic:can_be_protobuf_packed(Context, {alias, X}) of
        false ->
            gen_alias(Context, X, _IsPacked = false);
        true ->
            % if a value can be packed, we need to generate two functions: one
            % for parsing regular (unpacked) representation, and another one for
            % parsing packed form
            iod("\n\n", [
                gen_alias(Context, X, false),
                gen_alias(Context, X, true)
            ])
    end.


gen_alias(Context, X, IsPacked) ->
    PackedPrefix = ?if_true(IsPacked, "packed_", ""),
    Expr = [
        gen_alias_type(Context, X, X#alias.protobuf_wire_type, IsPacked),
        "(X)"
    ],
    Body =
        case IsPacked of
            false ->
                [
                    "    ",
                    piqic:gen_convert_value(X#alias.erlang_type, "_of_", X#alias.type, Expr)
                ];
            true ->
                [
                    "    ", "{Res, Rest} = ", Expr, ",\n",
                    "    ", "{",
                        piqic:gen_convert_value(X#alias.erlang_type, "_of_", X#alias.type, "Res"),
                        ", Rest}"
                ]
        end,
    [
        PackedPrefix, "parse_", X#alias.erlang_name, "(X) ->\n",
            Body, ".\n"
    ].


gen_list(Context, X) ->
    IsPacked = X#piqi_list.protobuf_packed,
    PackedPrefix = ?if_true(IsPacked, "packed_", ""),
    TypeName = X#piqi_list.type,
    [
        "parse_", X#piqi_list.erlang_name, "(X) ->\n",
        "    ", "piqirun:parse_", PackedPrefix, "list(",
            "fun ", gen_type(Context, TypeName, IsPacked), "/1, ",
            % when parsing packed repeated fields, we should also accept fields
            % in unpacked representation; therefore, specifying an unpacked
            % field parser as another parameter
            case IsPacked of
                false ->
                    "";
                true ->
                    [
                        "fun ", gen_type(Context, TypeName, _IsPacked = false), "/1, "
                    ]
            end,
            " X).\n"
    ].


gen_enum(X) ->
    % generate two functions: one for parsing normal value; another one -- for
    % packed value
    iod("\n\n", [
        gen_unpacked_enum(X),
        gen_packed_enum(X)
    ]).


gen_unpacked_enum(X) ->
    Consts = [gen_const(C) || C <- X#enum.option],
    Clauses = Consts ++ [ "Y -> piqirun:error_enum_const(Y)" ],
    [
        "parse_", X#enum.erlang_name, "(X) ->\n",
        "    ", "case piqirun:integer_of_signed_varint(X) of\n",
        "        ", iod(";\n        ", Clauses),
        "\n    end.\n"
    ].


gen_packed_enum(X) ->
    Consts = [gen_const(C) || C <- X#enum.option],
    Clauses = Consts ++ [ "Y -> piqirun:error_enum_const(Y)" ],
    [
        "packed_parse_", X#enum.erlang_name, "(X) ->\n",
        "    ", "{Code, Rest} = piqirun:integer_of_packed_signed_varint(X),\n",
        "    ", "{case Code of\n",
        "        ", iod(";\n        ", Clauses),
        "\n    end, Rest}.\n"
    ].


gen_const(X) ->
    [
        gen_code(X#option.code), " -> ", X#option.erlang_name
    ].


gen_variant(Context, X) ->
    Options = [gen_option(Context, O) || O <- X#variant.option],
    Clauses = Options ++ [ "_ -> piqirun:error_option(Obj, Code)" ],
    [
        "parse_", X#variant.erlang_name, "(X) ->\n",
        "    ", "{Code, Obj} = piqirun:parse_variant(X),\n",
        "    ", "case Code of\n",
        "        ", iod(";\n        ", Clauses),
        "\n    end.\n"
    ].


gen_option(Context, X) ->
    Name = erlname_of_option(Context, X),
    Code = gen_code(X#option.code),
    case X#option.type of
        'undefined' ->
            [
                Code, " when Obj =:= 1 -> ", Name
            ];
        TypeName ->
            {_Piqi, Typedef} = resolve_type_name(Context, TypeName),
            case Typedef of
                {Type, _} when X#option.erlang_name =:= 'undefined', (Type =:= variant orelse Type =:= enum) ->
                    % handle variant and enum subtyping cases
                    [
                        Code, " -> ", gen_type(Context, TypeName), "(Obj)"
                    ];
                _ ->
                    % general case
                    [
                        Code, " -> "
                        "{",
                            Name, ", ", gen_type(Context, TypeName), "(Obj)",
                        "}"
                    ]
            end
    end.


gen_record(Context, X) ->
    % order fields by their codes
    Fields = lists:sort(
        fun (A, B) -> A#field.code =< B#field.code end,
        X#piqi_record.field
    ),
    ConstructorsCode =
        case Fields of
            [] ->
                "";
            _ ->
                FieldConstructors = [gen_field_constructor(Context, F) || F <- Fields],
                [
                    "        ", iod(",\n        ", FieldConstructors), "\n"
                ]
        end,
    ParsersCode =
        case Fields of
            [] ->
                "";
            _ ->
                FieldParsers = gen_field_parsers(Context, Fields),
                [
                    "    ", iod(",\n    ", FieldParsers), ",\n"
                ]
        end,
    Name = X#piqi_record.erlang_name,
    [
        "parse_", Name, "(X) ->\n",
        "    ", "R0 = piqirun:parse_record(X),\n",
        ParsersCode,
        "    ", "piqirun:check_unparsed_fields(", record_variable(length(Fields)), "),\n",
        "    ", "#", scoped_name(Context, Name), "{\n",
        ConstructorsCode,
        "    ", "}.\n"
    ].


record_variable(I) ->
    ["R", integer_to_list(I)].


field_variable(X) ->
    ["_", piqic:capitalize(X)].


gen_field_constructor(Context, X) ->
    Name = erlname_of_field(Context, X),
    [Name, " = ", field_variable(Name)].


gen_field_parsers(Context, Fields) ->
    % enumerate the fields
    FieldNumbers = lists:seq(0, length(Fields) - 1),
    FieldNumberPairs = lists:zip(Fields, FieldNumbers),

    [gen_field_parser(Context, P) || P <- FieldNumberPairs].


gen_field_parser(Context, {X, I}) ->
    Name = erlname_of_field(Context, X),
    Mode = piqic:gen_field_mode(X),
    Code = gen_code(X#field.code),
    ParseExpr =
        case X#field.type of
            'undefined' ->  % flag, i.e. field w/o type
                [ 
                    "piqirun:parse_flag(", Code, ", ", record_variable(I), ")"
                ];
            TypeName ->
                [
                    % "parse_(req|opt|rep)_field" function invocation
                    "piqirun:parse_", Mode, "_field(",
                        Code, ", ",
                        "fun ", gen_type(Context, TypeName, X#field.protobuf_packed), "/1, ",
                        % when parsing packed repeated fields, we should also
                        % accept fields in unpacked representation; therefore,
                        % specifying an unpacked field parser as another
                        % parameter
                        case X#field.protobuf_packed of
                            false ->
                                "";
                            true ->
                                [
                                    "fun ", gen_type(Context, TypeName, _IsPacked = false), "/1, "
                                ]
                        end,
                        record_variable(I),
                        gen_default(X#field.default),
                    ")"
                ]
        end,
    [
        "{", field_variable(Name), ", ", record_variable(I+1), "} = ", ParseExpr
    ].


% TODO: precompute defaults of primitive and enum types -- see
% piqic_erlang_types:gen_field_default
gen_default('undefined') ->
    [];

gen_default(X) ->
    Bin = X#piqi_any.protobuf,
    [
        ", ",  % separate Default from the previous parameter
        io_lib:format("~p", [Bin])  % generate Erlang binary
    ].


gen_type(Context, TypeName) ->
    gen_type(Context, TypeName, _IsPacked = false).


gen_type(Context, TypeName, IsPacked) ->
    {ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
    ParentMod = piqic:gen_parent_mod(Context, ParentPiqi),
    PackedPrefix = ?if_true(IsPacked, "packed_", ""),
    [
        ParentMod, PackedPrefix, "parse_", typedef_erlname(Typedef)
    ].


% copy-pasted piqic_erlang_out:gen_alias_type -- not sure how to avoid this
gen_alias_type(Context, Alias, WireType, IsPacked) ->
    case Alias#alias.type of
        'undefined' ->  % we are dealing with built-in type
            gen_builtin_type(Context,
                Alias#alias.piqi_type,
                Alias#alias.erlang_type,
                WireType, IsPacked);
        TypeName ->
            {ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
            case Typedef of
                {alias, A} when WireType =/= 'undefined' ->
                    % need special handing in case when higher-level alias
                    % overrides protobuf_wire_type
                    ParentContext = piqic:switch_context(Context, ParentPiqi),
                    gen_alias_type(ParentContext, A, WireType, IsPacked);
                _ ->
                    gen_type(Context, TypeName, IsPacked)
            end
    end.


gen_builtin_type(Context, PiqiType, ErlType, WireType, IsPacked) ->
    case PiqiType of
        any ->
            case Context#context.is_self_spec of
                true when ErlType =/= 'undefined' ->
                    ["parse_", ErlType];
                true ->
                    "parse_any";
                false ->
                    "piqi_piqi:parse_piqi_any"
            end;
        _ ->
            PackedPrefix = ?if_true(IsPacked, "packed_", ""),
            TypeName = gen_builtin_type_name(Context, PiqiType, ErlType),
            WireTypeName = piqic:gen_wire_type_name(PiqiType, WireType),
            [
                "piqirun:", TypeName, "_of_", PackedPrefix, WireTypeName
            ]
    end.


gen_builtin_type_name(Context, PiqiType, ErlType) ->
    TypeName = piqic:gen_builtin_type_name(PiqiType, ErlType),
    case to_string(TypeName) of
        "string" ->
            Piqi = Context#context.piqi,
            case Piqi#piqi.erlang_string_type of
                binary ->
                    "binary_string";
                list ->
                    "list_string"
            end;
        X ->
            X
    end.

