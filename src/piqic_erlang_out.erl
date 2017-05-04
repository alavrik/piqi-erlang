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

% generation of gen_*/1,2,3 functions for Protocol Buffers, JSON, XML and Piq
% deserialization
-module(piqic_erlang_out).
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
        [gen_typedef_1(Context, X) || X <- Typedefs]
        ++
        [gen_typedef_multiformat(Context, X) || X <- Typedefs]
    ).


% field serializer
gen_typedef(Context, Typedef = {Type, X}) ->
    Spec = gen_spec(Context, Typedef),
    GenFun =
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
        GenFun
    ].


% Protobuf serializer
gen_typedef_1(Context, Typedef) ->
    Spec = gen_spec_1(Context, Typedef),
    ErlName = typedef_erlname(Typedef),
    [
        Spec, "\n",
        "gen_", ErlName, "(X) ->\n",
        "    ", "field_gen_", ErlName, "('undefined', X).\n"
    ].


gen_spec(Context, Typedef) ->
    [
        "-spec field_gen_", typedef_erlname(Typedef), "(",
            "Code :: piqirun_code(), "
            "X :: ", gen_output_type_name(Context, Typedef), ") -> iolist().\n"
    ].


% generate gen_<name>/1 spec
gen_spec_1(Context, Typedef) ->
    [
        "-spec gen_", typedef_erlname(Typedef), "(",
            "X :: ", gen_output_type_name(Context, Typedef), ") -> iolist().\n"
    ].


gen_output_type_name(Context, Typedef) ->
    piqic_erlang_types:gen_out_type(Context, typedef_name(Typedef)).


% mutliformat serializers: gen_*/2, gen_*/3
% TODO: generate -specs
gen_typedef_multiformat(Context, Typedef) ->
    ScopedName = piqic:typedef_scoped_name(Context, Typedef),
    ErlName = typedef_erlname(Typedef),
    [
        gen_typedef_2(ScopedName, ErlName),
        "\n\n",
        gen_typedef_3(ScopedName, ErlName)
    ].


% mutliformat serializer
gen_typedef_2(_Name, ErlName) ->
    [
        "gen_", ErlName, "(X, Format) ->\n",
        "    ", "gen_", ErlName, "(X, Format, []).\n"
    ].


% mutliformat serializer with additional options
gen_typedef_3(Name, ErlName) ->
    [
        "gen_", ErlName, "(X, Format, Options) ->\n",
        "    Iolist = gen_", ErlName, "(X),\n",
        "    ", gen_convert(Name, "'pb'", "Format", "iolist_to_binary(Iolist), Options"), ".\n"
    ].


gen_convert(ScopedName, InputFormat, OutputFormat, Data) ->
    [
        "piqirun_ext:convert(?MODULE, ",
        iod(", ", [
            ["<<\"", ScopedName, "\">>"], InputFormat, OutputFormat, Data
        ]),
        ")"
    ].


gen_alias(Context, X) ->
    case piqic:can_be_protobuf_packed(Context, {alias, X}) of
        false ->
            gen_unpacked_alias(Context, X);
        true ->
            % if a value can be packed, we need to generate two functions: one
            % for generating regular (unpacked) representation, and another one
            % for generating packed form
            iod("\n\n", [
                gen_unpacked_alias(Context, X),
                gen_packed_alias(Context, X)
            ])
    end.


gen_unpacked_alias(Context, X) ->
    [
        "field_gen_", X#alias.erlang_name, "(Code, X) ->\n"
        "    ",
            gen_alias_type(Context, X, X#alias.protobuf_wire_type, _IsPacked = false),
            "(Code, ",
                piqic:gen_convert_value(Context, X#alias.erlang_type, "_to_", X#alias.type, "X"),
            ").\n"
    ].


gen_packed_alias(Context, X) ->
    % packed_field_gen_* functino has arity 1, because values of such fields can
    % not be encoded independently: all values for a repeated packed field must
    % be encoded all at once
    [
        "packed_field_gen_", X#alias.erlang_name, "(X) ->\n"
        "    ",
            gen_alias_type(Context, X, X#alias.protobuf_wire_type, _IsPacked = true),
            "(",
                piqic:gen_convert_value(Context, X#alias.erlang_type, "_to_", X#alias.type, "X"),
            ").\n"
    ].


gen_list(Context, X) ->
    IsPacked = X#piqi_list.protobuf_packed,
    PackedPrefix = ?if_true(IsPacked, "packed_", ""),
    TypeName = X#piqi_list.type,
    [
        "field_gen_", X#piqi_list.erlang_name, "(Code, X) ->\n",
        "    ", "piqirun:gen_", PackedPrefix, "list(Code, ",
            "fun ", gen_type(Context, TypeName, IsPacked), "/", ?if_true(IsPacked, "1", "2"),  % arity
            ", X).\n"
    ].


gen_enum(X) ->
    % generate two functions: one for parsing normal value; another one -- for
    % packed value
    iod("\n\n", [
        gen_unpacked_enum(X),
        gen_packed_enum(X)
    ]).


gen_unpacked_enum(X) ->
    Consts = gen_consts(X#enum.option),
    [
        "field_gen_", X#enum.erlang_name, "(Code, X) ->\n",
        "    ", "piqirun:integer_to_signed_varint(Code,\n",
        Consts,
        "    ).\n"
    ].


gen_packed_enum(X) ->
    Consts = gen_consts(X#enum.option),
    [
        "packed_field_gen_", X#enum.erlang_name, "(X) ->\n",
        "    ",  "piqirun:integer_to_packed_signed_varint(\n",
        Consts,
        "    ).\n"
    ].


gen_consts(Consts) ->
    Clauses = [gen_const(C) || C <- Consts],
    [
        "        ", "case X of\n",
        "            ", iod(";\n            ", Clauses), "\n"
        "        ", "end\n"
    ].


gen_const(X) ->
    [
        X#option.erlang_name, " -> ", gen_code(X#option.code)
    ].


gen_variant(Context, X) ->
    L = [gen_option(Context, O) || O <- X#variant.option],
    Options = lists:append(L),  % flatten
    [
        "field_gen_", X#variant.erlang_name, "(Code, X) ->\n",
        "    ", "piqirun:gen_variant(Code,\n",
        "        ", "case X of\n",
        "            ", iod(";\n            ", Options), "\n"
        "        ", "end\n"
        "    ).\n"
    ].


gen_option(Context, X) ->
    gen_option(Context, X, _OuterOption = 'undefined').


gen_option(Context, X, OuterOption) ->
    Name = erlname_of_option(Context, X),
    Code = gen_code(X#option.code),
    case X#option.type of
        'undefined' ->
            case OuterOption =/= 'undefined' of
                true ->
                    gen_inner_option(Name, OuterOption);
                false ->
                    Clause = [
                        Name, " -> ", "piqirun:gen_bool_field(", Code, ", true)"
                    ],
                    [Clause]
            end;
        TypeName ->
            {ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
            case Typedef of
                {Type, Y} when X#option.erlang_name =:= 'undefined', (Type =:= variant orelse Type =:= enum) ->
                    % handle variant and enum subtyping cases by lifting their
                    % labels and clauses to the top level
                    Options =
                        case Type of
                            variant -> Y#variant.option;
                            enum -> Y#enum.option
                        end,
                    % recursively generate cases from "included" variants and
                    % enums
                    OuterOption2 = ?defined(OuterOption, {Context, X}),
                    ParentContext = piqic:switch_context(Context, ParentPiqi),
                    L = [gen_option(ParentContext, O, OuterOption2) || O <- Options],
                    lists:append(L);  % flatten
                _ ->
                    % general case
                    case OuterOption =/= 'undefined' of
                        true ->
                            Pattern = ["{", Name, ", _}"],
                            gen_inner_option(Pattern, OuterOption);
                        false ->
                            Res = [
                                "{", Name, ", Y} -> ",
                                 gen_type(Context, TypeName), "(", Code, ", Y)"
                            ],
                            [Res]
                    end
            end
    end.


gen_inner_option(Pattern, {Context, X}) ->
    Code = gen_code(X#option.code),
    Clause = [
        Pattern, " -> ", gen_type(Context, X#option.type), "(", Code, ", X)"
    ],
    [Clause].


gen_record(Context, X) ->
    % order fields by their codes
    Fields = lists:sort(
        fun (A, B) -> A#field.code =< B#field.code end,
        X#piqi_record.field
    ),
    Name = X#piqi_record.erlang_name,
    ScopedName = scoped_name(Context, Name),
    UnknownFields =
        case piqic:get_option(Context, gen_preserve_unknown_fields) of
            false -> [];
            true ->
                ["[piqirun:gen_parsed_field(F) || F <- X#", ScopedName, ".piqi_unknown_pb]"]
        end,
    GeneratorsCode =
        case Fields of
            [] when UnknownFields =:= [] ->
                "[]";
            [] ->
                UnknownFields;
            _ ->
                FieldGenerators = [gen_field(Context, ScopedName, F) || F <- Fields],
                case UnknownFields of
                    [] ->
                        [
                            "[\n",
                            "        ", iod(",\n        ", FieldGenerators), "\n",
                            "    ", "]"
                        ];
                    _ ->
                        [
                            "[\n",
                            "        ", iod(",\n        ", FieldGenerators), "\n",
                            "    |", UnknownFields, "]"
                        ]
                end
        end,
    % prevent Erlang warning on unused variable
    ArgVariable =
        case Fields of
            [] when UnknownFields =:= [] ->
                ["#", ScopedName, "{}"];
            _ ->
                "X"
        end,
    [
        "field_gen_", Name, "(Code, ", ArgVariable, ") ->\n",
        "    ", "piqirun:gen_record(Code, ", GeneratorsCode, ").\n"
    ].


gen_field(Context, RecordName, X) ->
    Name = erlname_of_field(Context, X),
    ScopedName = [
        "X#", RecordName, ".", Name
    ],
    Mode = piqic:gen_field_mode(X),
    Code = gen_code(X#field.code),
    IsPacked = X#field.protobuf_packed,
    case X#field.type of
        'undefined' ->  % flag, i.e. field w/o type
            [ 
                "piqirun:gen_flag(", Code, ", ", ScopedName, ")"
            ];
        TypeName ->
            [ 
                "piqirun:gen_", Mode, "_field(", Code, ", ",
                    "fun ", gen_type(Context, TypeName, IsPacked), "/", ?if_true(IsPacked, "1", "2"),  % arity
                    ", ",
                    ScopedName,
                ")"
            ]
    end.


gen_type(Context, TypeName) ->
    gen_type(Context, TypeName, _IsPacked = false).


gen_type(Context, TypeName, IsPacked) ->
    {ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
    ParentMod = piqic:gen_parent_mod(Context, ParentPiqi),
    PackedPrefix = ?if_true(IsPacked, "packed_", ""),
    [
        ParentMod, PackedPrefix, "field_gen_", typedef_erlname(Typedef)
    ].


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
                    ["field_gen_", ErlType];
                true ->
                    "field_gen_any";
                false ->
                    "piqi_piqi:field_gen_piqi_any"
            end;
        _ ->
            PackedPrefix = ?if_true(IsPacked, "packed_", ""),
            TypeName = piqic:gen_builtin_type_name(PiqiType, ErlType),
            WireTypeName = piqic:gen_wire_type_name(PiqiType, WireType),
            [
                "piqirun:", TypeName, "_to_", PackedPrefix, WireTypeName
            ]
    end.

