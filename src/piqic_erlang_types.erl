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

% generation of -type ... and -record(...) Erlang forms
-module(piqic_erlang_types).
-compile(export_all).


-include("piqic.hrl").


-define(DEBUG,1).
-include("debug.hrl").


gen_piqi(Context) ->
    Piqi = Context#context.piqi,
    iod("\n\n", [
        gen_imports(Context, Piqi#piqi.import),
        gen_typedefs(Context, Piqi#piqi.typedef)
    ]).


gen_imports(Context, Imports) ->
    % generate the list of Erlang includes of .hrl files of imported modules 
    Includes = [gen_import(Context, X) || X <- Imports],

    % decide if we need to include piqi_any() type definition or not
    Piqi = Context#context.piqi,
    PiqiAny =
        case not Context#context.is_self_spec andalso piqic:piqi_depends_on_piqi_any(Piqi) of
            true ->
                % NOTE: we do not want to include the whole "piqi_piqi.hrl",
                % because definitions from it can conflict with the compiled
                % module's local definitions; just copy-pasting the piqi_any()
                % definition here
                iod("\n", [
                    "-type(piqi_any() :: #piqi_any{}).",
                    "-record(piqi_any, {",
                    "    type :: string() | binary(),",
                    "    protobuf :: binary(),",
                    "    json :: string() | binary(),",
                    "    xml :: string() | binary()",
                    "})."
                ]);
            false ->
                []
        end,
    iod("\n", Includes ++ [PiqiAny]).


gen_import(Context, Import) ->
    ImportedIndex = piqic:resolve_import(Context, Import),
    ImportedPiqi = ImportedIndex#index.piqi,
    ["-include(\"", ImportedPiqi#piqi.erlang_module, ".hrl\")."].


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
        list ->
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
    TypeExpr = gen_out_type(Context, X#alias.type, X#alias.erlang_type),
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
    make_typedef_1(Context, Name, ["#", scoped_name(Context, Name), "{}"]).


make_typedef(Context, Name, TypeExpr) ->
    [
        "-type ", scoped_name(Context, Name), "() ::", TypeExpr, ".\n"
    ].


% fit it on one line
make_typedef_1(Context, Name, TypeExpr) ->
    make_typedef(Context, Name, [" ", TypeExpr]).


gen_record(Context, X) ->
    Name = X#piqi_record.erlang_name,
    Fields = [gen_field(Context, F) || F <- X#piqi_record.field],
    [
        "-record(", scoped_name(Context, Name), ", {",
        "\n    ",
        iod(",\n    ", Fields),
        "\n}).\n"
    ].


gen_field(Context, X) ->
    Name = erlname_of_field(Context, X),
    % initialize repreated fields with []
    %
    % TODO: populate default values for primitive types if they are defined in
    % Piqi
    Default =
        case X#field.mode of
            repeated ->
                " = []";
            _ ->
                ""
        end,
    [
        Name, Default, " :: ", gen_field_type(Context, X#field.mode, X#field.type)
    ].


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
    gen_out_type(Context, TypeName, _ErlType = 'undefined').


gen_out_type(Context, TypeName, ErlType) ->
    T = gen_type(Context, TypeName, ErlType),
    % allow more flexible typing for convenience
    case to_string(T) of
        "string" ->
            "string() | binary()";
        "float" ->
            "number()";
        S ->
            S ++ "()"
    end.


gen_type(_Context, _TypeName, ErlType) when ErlType =/= 'undefined' ->
    ErlType;

gen_type(Context, TypeName, _ErlType) ->
    gen_type(Context, TypeName).


gen_type(Context, TypeName) ->
    {ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
    case Typedef of
        {alias, X} ->
            gen_alias_type(Context, ParentPiqi, X);
        _ -> % piqi_record | variant | list | enum
            LocalName = typedef_erlname(Typedef),
            % make scoped name based on the parent module's type prefix
            _ScopedName = ParentPiqi#piqi.erlang_type_prefix ++ to_string(LocalName)
    end.

        
gen_alias_type(Context, ParentPiqi, Alias) ->
    case Alias#alias.type =:= 'undefined' andalso Alias#alias.erlang_type =:= 'undefined' of
        true ->
            % this is an alias for a built-in type (piqi_type field must be
            % defined when neither of type and erlang_type fields are present)
            gen_builtin_type(ParentPiqi, Alias#alias.piqi_type);
        false ->
            case piqic:is_builtin_alias(Alias) of
                true ->
                    % we need to recurse, because we don't generate -type
                    % definitions for built-in types
                    gen_type(Context, Alias#alias.type, Alias#alias.erlang_type);
                false ->
                    % for non-builtin types, we just use the name of already
                    % generated -type
                    _ScopedName = ParentPiqi#piqi.erlang_type_prefix ++ Alias#alias.erlang_name
            end
    end.


gen_builtin_type(ParentPiqi, PiqiType) ->
    case PiqiType of
        any ->
            case piqic:is_self_spec(ParentPiqi) of
                true ->
                    % we can't know upfront if this self-spec uses prefixes or
                    % not
                    _ScopedName = ParentPiqi#piqi.erlang_type_prefix ++ "piqi_any";
                false ->
                    % but the default piqi_piqi.erl definitely doesn't have type
                    % prefixes, therefore we can use the standard Erlang name
                    % for "piqi-any" type
                    "piqi_any"
            end;
        _ ->
            piqic:gen_builtin_type_name(PiqiType)
    end.

