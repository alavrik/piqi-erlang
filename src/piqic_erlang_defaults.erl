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

% generation of default_*/0 functions returning minimal serializable values of
% Piqi types
%
% CAVEAT: the logic is very primitive and it doesn't guarantee that default
% variant values are finite

-module(piqic_erlang_defaults).
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
    ).


gen_typedef(Context, _Typedef = {Type, X}) ->
    case Type of
        piqi_record ->
            gen_record(Context, X);
        variant ->
            gen_variant(Context, X);
        piqi_list ->
            gen_list(X);
        enum ->
            gen_enum(X);
        alias ->
            gen_alias(Context, X)
    end.


gen_alias(Context, X) ->
    Expr = gen_alias_type(Context, X),
    Body =
        case X#alias.erlang_default of
            'undefined' ->
                piqic:gen_convert_value(X#alias.erlang_type, "_of_", X#alias.type, Expr);
            Default ->
                Default
        end,
    [
        "default_", X#alias.erlang_name, "() -> ", Body, ".\n"
    ].


gen_list(X) ->
    [
        "default_", X#piqi_list.erlang_name, "() -> [].\n"
    ].


gen_enum(X) ->
    O = hd(X#enum.option),  % there is always at least one option
    [
        "default_", X#enum.erlang_name, "() -> ", O#option.erlang_name, ".\n"
    ].


gen_variant(Context, X) ->
    % NOTE: this choice of default option doesn't guarantee that the resulting
    % value is finite
    Option = hd(X#variant.option),  % there is always at least one option
    DefaultOption = gen_option(Context, Option),
    [
        "default_", X#variant.erlang_name, "() -> ", DefaultOption, ".\n"
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
                    % handle variant and enum subtyping cases
                    gen_type(Context, TypeName);
                _ ->
                    [
                        "{", Name, ", ", gen_type(Context, TypeName), "}"
                    ]
            end
    end.


gen_record(Context, X) ->
    Name = X#piqi_record.erlang_name,
    Fields = [gen_field(Context, F) || F <- X#piqi_record.field],
    ConstructorsCode =
        case Fields of
            [] ->
                "";
            _ ->
                [
                    "        ", iod(",\n        ", Fields), "\n"
                ]
        end,
    [
        "default_", Name, "() ->\n",
        "    ", "#", scoped_name(Context, Name), "{\n",
        ConstructorsCode,
        "    ", "}.\n"
    ].


gen_field(Context, X) ->
    Name = erlname_of_field(Context, X),
    DefaultExpr =
        case X#field.mode of
            required ->
                gen_type(Context, X#field.type);
            optional when X#field.type =:= 'undefined' ->  % flag
                "false";
            optional ->
                "'undefined'";
            repeated ->
                "[]"
        end,
    [Name, " = ", DefaultExpr].


gen_type(Context, TypeName) ->
    {ParentPiqi, Typedef} = resolve_type_name(Context, TypeName),
    ParentMod = piqic:gen_parent_mod(Context, ParentPiqi),
    [
        ParentMod, "default_", typedef_erlname(Typedef), "()"
    ].


gen_alias_type(Context, Alias) ->
    case Alias#alias.type of
        'undefined' ->  % we are dealing with built-in type
            gen_builtin_type(Context, Alias#alias.piqi_type);
        TypeName ->
            gen_type(Context, TypeName)
    end.


gen_builtin_type(Context, PiqiType) ->
    case PiqiType of
        any ->
            case Context#context.is_self_spec of
                true ->
                    "default_piqi_any()";
                false ->
                    "piqi_piqi:default_piqi_any()"
            end;
        % there is only one way to represent values of these types in Erlang
        int ->
            "0";
        float ->
            "0.0";
        bool ->
            "false";
        binary ->
            "<<>>";
        string ->
            Piqi = Context#context.piqi,
            case Piqi#piqi.erlang_string_type of
                binary ->
                    "<<>>";
                list ->
                    "\"\""
            end
    end.

