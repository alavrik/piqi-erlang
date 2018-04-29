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

% generation of -type ... and -record(...) Erlang forms
-module(piqic_erlang_types).
-compile([export_all, nowarn_export_all]).


-include("piqic.hrl").


-define(DEBUG,1).
-include("debug.hrl").


gen_piqi(Context) ->
    Piqi = Context#context.piqi,
    % excluding #piqi_any{}, because its definition will be included from piqi_any_piqi.hrl --- see
    % piqic_erlang:gen_include_piqi_any_piqi_hrl() for details
    Typedefs = [X || X <- Piqi#piqi.typedef, not piqic:is_piqi_any_record_typedef(X)],
    iod("\n\n", [
        gen_imports(Context, Piqi#piqi.import),
        gen_typedefs(Context, Typedefs)
    ]).


% generate #piqi_any{} typedef, used only in with --cc when compiling self-spec as a part of the piqi application -- see
% piqic_erlang:gen_piqi_any_piqi_hrl() for details
gen_piqi_any(Context) ->
    Piqi = Context#context.piqi,
    PiqiAnyTypedef = piqic:find_piqi_any_record_typedef(Piqi),
    true = PiqiAnyTypedef =/= 'undefined',
    iod("\n\n", [
        gen_typedefs(Context, [PiqiAnyTypedef])
    ]).


gen_imports(Context, Imports) ->
    % generate the list of Erlang includes of .hrl files of imported modules
    Includes = [gen_import(Context, X) || X <- Imports],
    iod("\n", Includes).


gen_import(Context, Import) ->
    ImportedIndex = piqic:resolve_import(Context, Import),
    ImportedPiqi = ImportedIndex#index.piqi,
    ErlMod = ImportedPiqi#piqi.erlang_module,
    IncludeLib = piqic:get_option(Context, include_lib),
    case find_include_lib(IncludeLib, ImportedPiqi#piqi.file) of
        'undefined' ->
            ["-include(\"", ErlMod, ".hrl\")."];
        {AppPath, _Path} ->
            ["-include_lib(\"", AppPath, "/", ErlMod, ".hrl\")."]
    end.


find_include_lib(_IncludeLib, _File = 'undefined') ->
    % backward compatibility: in older versions of piqi the "file" attribute may
    % not be present
    'undefined';

find_include_lib([], _File) ->
    % not found
    'undefined';

find_include_lib([H = {_AppPath, Path} |T], File) ->
    % was the .piqi module (File) found in Path?
    case lists:prefix(Path, binary_to_list(File)) of
        true ->  % found
            H;
        false -> % not found => keep going through the list
            find_include_lib(T, File)
    end.


gen_typedefs(Context, Typedefs) ->
    ErlDefs = [gen_typedef(Context, X) || X <- Typedefs],

    % generate -type(...) aliases for -record(...) definitions
    Records = [X || {piqi_record, X} <- Typedefs],
    ErlRecordTypes = [gen_record_type(Context, X) || X <- Records],

    iod("\n", ErlDefs ++ ErlRecordTypes).


gen_typedef(Context, {Type, X}) ->
    case Type of
        piqi_record ->
            gen_record(Context, X);
        variant ->
            gen_variant(Context, X);
        piqi_list ->
            gen_list(Context, X);
        enum ->
            gen_enum(Context, X);
        alias ->
            % skip generation of aliases of built-in types (this matters only
            % for compiling self-spec); the reason we skip aliases of built-in
            % types is because we don't want other modules include on
            % piqi_piqi.hrl; on the other hand, we don't want to built-in type
            % definitions here, because we don't want to pollute the global
            % namespace when erlang_type_prefix = "" and with non-empty
            % erlan_type_prefix, aliases for pritimive types would look ugly
            case piqic:is_builtin_alias(X) of
                true ->
                    [];
                false ->
                    gen_alias(Context, X)
            end
    end.


gen_alias(Context, X) ->
    TypeExpr = gen_out_alias_type(Context, X),
    make_typedef_1(Context, X#alias.erlang_name, TypeExpr).


gen_list(Context, X) ->
    TypeExpr = [
        "[", gen_out_type(Context, X#piqi_list.type), "]"
    ],
    make_typedef_1(Context, X#piqi_list.erlang_name, TypeExpr).


gen_enum(Context, X) ->
    TypeExpr = gen_options(Context, X#enum.option),
    make_typedef(Context, X#enum.erlang_name, TypeExpr).


gen_variant(Context, X) ->
    TypeExpr = gen_options(Context, X#variant.option),
    make_typedef(Context, X#variant.erlang_name, TypeExpr).


gen_options(Context, Options) ->
    ErlOptions = [gen_option(Context, X) || X <- Options],
    [
        "\n      ",
        iod("\n    | ", ErlOptions)
    ].


gen_option(Context, X) ->
    Name = erlname_of_option(Context, X),
    case X#option.type of
        'undefined' ->
            Name;
        TypeName ->
            {_Piqi, Typedef} = resolve_type_name(Context, TypeName),
            case Typedef of
                {Type, _} when X#option.erlang_name =:= 'undefined', (Type =:= variant orelse Type =:= enum) ->
                    % handle variant and enum subtyping cases by omitting the
                    % option label
                    gen_out_type(Context, TypeName);
                _ ->
                    % general case
                    [
                        "{", Name, ", ", gen_out_type(Context, TypeName), "}"
                    ]
            end
    end.


gen_record_type(Context, X) ->
    Name = X#piqi_record.erlang_name,
    make_typedef_1(Context, Name, ["#", piqic:scoped_erlname(Context, Name), "{}"]).


make_typedef(Context, Name, TypeExpr) ->
    [
        "-type ", piqic:scoped_erlname(Context, Name), "() ::", TypeExpr, ".\n"
    ].


% fit it on one line
make_typedef_1(Context, Name, TypeExpr) ->
    make_typedef(Context, Name, [" ", TypeExpr]).


gen_record(Context, X) ->
    Name = X#piqi_record.erlang_name,
    ImplicitFields =
        case piqic:get_option(Context, gen_preserve_unknown_fields) of
            false -> [];
            true ->
                ["piqi_unknown_pb = [] :: [piqirun_parsed_field()]"]
        end,
    Fields = [gen_field(Context, F) || F <- X#piqi_record.field] ++ ImplicitFields,
    FieldsCode =
        case Fields of
            [] ->
                "";
            _ ->
                [
                    "\n    ", iod(",\n    ", Fields), "\n"
                ]
        end,
    [
        "-record(", piqic:scoped_erlname(Context, Name), ", {",
        FieldsCode,
        "}).\n"
    ].


gen_field(Context, X) ->
    Name = erlname_of_field(Context, X),
    % initialize repeated fields with [] & populate default values if they are
    % defined in Piqi
    Default = gen_field_default(Context, X),
    [
        Name, Default, " :: ", gen_field_type(Context, X#field.mode, X#field.type),
            case _CanBeUndefined = (X#field.mode =:= optional) of
                true -> " | 'undefined'";
                false -> ""
            end
    ].


gen_field_default(Context, X0) ->
    % TODO: remove eventually -- keeping for backward compatibility with older
    % piqi which expects flags to only be true if present, and never false
    X = piqic:transform_flag(X0),

    case X#field.mode of
        repeated ->
            " = []";
        optional when X#field.default =/= 'undefined' ->
            Value = gen_field_default(Context, X#field.type, X#field.default, _WireType = 'undefined'),
            % NOTE: we need 'undefined' here, because otherwise Dialyzer won't
            % treat it as a valid field value
            [" = ", Value];
        _ ->
            ""
    end.


gen_field_default(Context, TypeName, Default, WireType) ->
    {ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
    case Typedef of
        {alias, A} ->
            % handing protobuf_wire_type override by a higher-level alias
            WireType2 = ?defined(WireType, A#alias.protobuf_wire_type),
            case A#alias.type of
                'undefined' ->  % we are dealing with built-in type
                    gen_field_default_builtin_value(Context,
                        A#alias.piqi_type, % must be defined when type is undefined
                        A#alias.erlang_type,
                        WireType2,
                        Default);
                _ ->
                    case A#alias.erlang_type =/= 'undefined' of
                        true ->  % custom Erlang type
                            gen_field_default_value(ParentPiqi, Typedef, Default);
                        false ->
                            ParentContext = piqic:switch_context(Context, ParentPiqi),
                            gen_field_default(ParentContext, A#alias.type, Default, WireType2)
                    end
            end;
        {enum, E} ->
            gen_field_default_enum_value(E, Default);
        _ ->
            gen_field_default_value(ParentPiqi, Typedef, Default)
    end.


% defaults for non-primitive and custom types are resolved by calling
% _piqi:parse_X functions from the module generated by piqic_erlang_in.erl
gen_field_default_value(ParentPiqi, Typedef, Default) ->
    ErlName = typedef_erlname(Typedef),
    % format Erlang binary
    Value = io_lib:format("~p", [Default#piqi_any.protobuf]),
    [ParentPiqi#piqi.erlang_module, ":parse_", ErlName, "(", Value, ")"].


gen_field_default_builtin_value(Context, PiqiType, ErlType, WireType, Default) ->
    % construct the name of the function for parsing values of the built-in type
    TypeName = piqic_erlang_in:gen_builtin_type_name(Context, PiqiType, ErlType),
    WireTypeName = piqic:gen_wire_type_name(PiqiType, WireType),
    ConvertFun = list_to_atom(TypeName ++ "_of_" ++ WireTypeName),

    % obtain Erlang term that corresponds to the default value
    Term = piqirun:ConvertFun(Default#piqi_any.protobuf),

    % convert the term into Erlang lexical representation
    io_lib:format("~p", [Term]).


gen_field_default_enum_value(Enum, Default) ->
    Code = piqirun:integer_of_signed_varint(Default#piqi_any.protobuf),
    % find the option by code
    Option = lists:keyfind(Code, #option.code, Enum#enum.option),
    Option#option.erlang_name.


gen_field_type(Context, Mode, TypeName) ->
  case TypeName of
      'undefined' ->
          "boolean()";  % flags are represented as booleans
      _ ->
          TypeExpr = gen_out_type(Context, TypeName),
          case Mode of
              repeated ->
                  [
                      "[",  TypeExpr, "]"
                  ];
              _ -> % required or optional
                  TypeExpr
          end
  end.


gen_out_type(Context, TypeName) ->
    T = gen_type(Context, TypeName),
    output_type(T).


gen_out_typedef_type(Context, Typedef) ->
    T = gen_typedef_type(Context, Typedef),
    output_type(T).


gen_out_alias_type(Context, Alias) ->
    T = gen_alias_type(Context, Alias),
    output_type(T).


output_type(T) ->
    % allow more flexible typing for convenience
    case to_string(T) of
        "string" ->
            "string() | binary()";
        "float" ->
            "number()";
        S ->
            S ++ "()"
    end.


gen_type(Context, TypeName) ->
    {ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
    ParentContext = piqic:switch_context(Context, ParentPiqi),
    gen_typedef_type(ParentContext, Typedef).


gen_typedef_type(Context, Typedef) ->
    case Typedef of
        {alias, Alias} ->
            case piqic:is_builtin_alias(Alias) of
                true ->
                    % we need to recurse, because we don't generate -type
                    % definitions for built-in types (see below)
                    gen_alias_type(Context, Alias);
                false ->
                    gen_non_builtin_typedef_type(Context, Typedef)
            end;
        _ -> % piqi_record | variant | list | enum
            gen_non_builtin_typedef_type(Context, Typedef)
    end.


gen_non_builtin_typedef_type(Context, Typedef) ->
    % make scoped name based on the parent module's type prefix
    Name = typedef_erlname(Typedef),
    piqic:scoped_erlname(Context, Name).


gen_alias_type(Context, Alias) ->
    case {Alias#alias.erlang_type, Alias#alias.type} of
        {ErlType, _} when ErlType =/= 'undefined' ->
            ErlType;
        {'undefined', 'undefined'} ->
            % this is an alias for a built-in type (piqi_type field must be
            % defined when neither of type and erlang_type fields are present)
            gen_builtin_type(Alias#alias.piqi_type);
        {'undefined', TypeName} ->
            gen_type(Context, TypeName)
    end.


gen_builtin_type(PiqiType) ->
    piqic:gen_builtin_type_name(PiqiType).
