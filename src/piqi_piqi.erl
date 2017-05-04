-module(piqi_piqi).
-compile(export_all).

-include("piqi_piqi.hrl").


-spec field_gen_piq_format(Code :: piqirun_code(), X :: piq_format()) -> iolist().

field_gen_piq_format(Code, X) ->
    piqirun:gen_variant(Code,
        case X of
            word -> piqirun:gen_bool_field(251462090, true);
            text -> piqirun:gen_bool_field(217697453, true)
        end
    ).


-spec field_gen_protobuf_int32(Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_protobuf_int32(Code, X) ->
    piqirun:integer_to_signed_varint(Code, X).


packed_field_gen_protobuf_int32(X) ->
    piqirun:integer_to_packed_signed_varint(X).


-spec field_gen_protobuf_int64(Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_protobuf_int64(Code, X) ->
    piqirun:integer_to_signed_varint(Code, X).


packed_field_gen_protobuf_int64(X) ->
    piqirun:integer_to_packed_signed_varint(X).


-spec field_gen_protobuf_wire_type(Code :: piqirun_code(), X :: protobuf_wire_type()) -> iolist().

field_gen_protobuf_wire_type(Code, X) ->
    piqirun:integer_to_signed_varint(Code,
        case X of
            varint -> 329594984;
            zigzag_varint -> 99211597;
            fixed32 -> 136997651;
            fixed64 -> 136998322;
            signed_varint -> 441915897;
            signed_fixed32 -> 488499298;
            signed_fixed64 -> 488499969;
            block -> 352089421
        end
    ).


packed_field_gen_protobuf_wire_type(X) ->
    piqirun:integer_to_packed_signed_varint(
        case X of
            varint -> 329594984;
            zigzag_varint -> 99211597;
            fixed32 -> 136997651;
            fixed64 -> 136998322;
            signed_varint -> 441915897;
            signed_fixed32 -> 488499298;
            signed_fixed64 -> 488499969;
            block -> 352089421
        end
    ).


-spec field_gen_bool(Code :: piqirun_code(), X :: boolean()) -> iolist().

field_gen_bool(Code, X) ->
    piqirun:boolean_to_varint(Code, X).


packed_field_gen_bool(X) ->
    piqirun:boolean_to_packed_varint(X).


-spec field_gen_string(Code :: piqirun_code(), X :: string() | binary()) -> iolist().

field_gen_string(Code, X) ->
    piqirun:string_to_block(Code, X).


-spec field_gen_binary(Code :: piqirun_code(), X :: binary()) -> iolist().

field_gen_binary(Code, X) ->
    piqirun:binary_to_block(Code, X).


-spec field_gen_piqi_piqi_any(Code :: piqirun_code(), X :: piqi_any()) -> iolist().

field_gen_piqi_piqi_any(Code, X) ->
    field_gen_piqi_any(Code, X).


-spec field_gen_int(Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_int(Code, X) ->
    piqirun:integer_to_zigzag_varint(Code, X).


packed_field_gen_int(X) ->
    piqirun:integer_to_packed_zigzag_varint(X).


-spec field_gen_uint(Code :: piqirun_code(), X :: non_neg_integer()) -> iolist().

field_gen_uint(Code, X) ->
    piqirun:non_neg_integer_to_varint(Code, X).


packed_field_gen_uint(X) ->
    piqirun:non_neg_integer_to_packed_varint(X).


-spec field_gen_int32(Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_int32(Code, X) ->
    piqirun:integer_to_zigzag_varint(Code, X).


packed_field_gen_int32(X) ->
    piqirun:integer_to_packed_zigzag_varint(X).


-spec field_gen_uint32(Code :: piqirun_code(), X :: non_neg_integer()) -> iolist().

field_gen_uint32(Code, X) ->
    piqirun:non_neg_integer_to_varint(Code, X).


packed_field_gen_uint32(X) ->
    piqirun:non_neg_integer_to_packed_varint(X).


-spec field_gen_int64(Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_int64(Code, X) ->
    piqirun:integer_to_zigzag_varint(Code, X).


packed_field_gen_int64(X) ->
    piqirun:integer_to_packed_zigzag_varint(X).


-spec field_gen_uint64(Code :: piqirun_code(), X :: non_neg_integer()) -> iolist().

field_gen_uint64(Code, X) ->
    piqirun:non_neg_integer_to_varint(Code, X).


packed_field_gen_uint64(X) ->
    piqirun:non_neg_integer_to_packed_varint(X).


-spec field_gen_float64(Code :: piqirun_code(), X :: number()) -> iolist().

field_gen_float64(Code, X) ->
    piqirun:float_to_fixed64(Code, X).


packed_field_gen_float64(X) ->
    piqirun:float_to_packed_fixed64(X).


-spec field_gen_float32(Code :: piqirun_code(), X :: number()) -> iolist().

field_gen_float32(Code, X) ->
    piqirun:float_to_fixed32(Code, X).


packed_field_gen_float32(X) ->
    piqirun:float_to_packed_fixed32(X).


-spec field_gen_int32_fixed(Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_int32_fixed(Code, X) ->
    piqirun:integer_to_signed_fixed32(Code, X).


packed_field_gen_int32_fixed(X) ->
    piqirun:integer_to_packed_signed_fixed32(X).


-spec field_gen_uint32_fixed(Code :: piqirun_code(), X :: non_neg_integer()) -> iolist().

field_gen_uint32_fixed(Code, X) ->
    piqirun:non_neg_integer_to_fixed32(Code, X).


packed_field_gen_uint32_fixed(X) ->
    piqirun:non_neg_integer_to_packed_fixed32(X).


-spec field_gen_int64_fixed(Code :: piqirun_code(), X :: integer()) -> iolist().

field_gen_int64_fixed(Code, X) ->
    piqirun:integer_to_signed_fixed64(Code, X).


packed_field_gen_int64_fixed(X) ->
    piqirun:integer_to_packed_signed_fixed64(X).


-spec field_gen_uint64_fixed(Code :: piqirun_code(), X :: non_neg_integer()) -> iolist().

field_gen_uint64_fixed(Code, X) ->
    piqirun:non_neg_integer_to_fixed64(Code, X).


packed_field_gen_uint64_fixed(X) ->
    piqirun:non_neg_integer_to_packed_fixed64(X).


-spec field_gen_float(Code :: piqirun_code(), X :: number()) -> iolist().

field_gen_float(Code, X) ->
    field_gen_float64(Code, X).


packed_field_gen_float(X) ->
    packed_field_gen_float64(X).


-spec field_gen_word(Code :: piqirun_code(), X :: word()) -> iolist().

field_gen_word(Code, X) ->
    field_gen_string(Code, X).


-spec field_gen_name(Code :: piqirun_code(), X :: name()) -> iolist().

field_gen_name(Code, X) ->
    field_gen_word(Code, X).


-spec field_gen_typedef(Code :: piqirun_code(), X :: typedef()) -> iolist().

field_gen_typedef(Code, X) ->
    piqirun:gen_variant(Code,
        case X of
            {piqi_record, Y} -> field_gen_piqi_record(502036113, Y);
            {variant, Y} -> field_gen_variant(484589701, Y);
            {enum, Y} -> field_gen_enum(51800833, Y);
            {alias, Y} -> field_gen_alias(26300816, Y);
            {piqi_list, Y} -> field_gen_piqi_list(129178718, Y)
        end
    ).


-spec field_gen_piqi_type(Code :: piqirun_code(), X :: piqi_type()) -> iolist().

field_gen_piqi_type(Code, X) ->
    piqirun:integer_to_signed_varint(Code,
        case X of
            int -> 5246191;
            float -> 43435420;
            bool -> 18580522;
            string -> 288368849;
            binary -> 218872833;
            any -> 4848364
        end
    ).


packed_field_gen_piqi_type(X) ->
    piqirun:integer_to_packed_signed_varint(
        case X of
            int -> 5246191;
            float -> 43435420;
            bool -> 18580522;
            string -> 288368849;
            binary -> 218872833;
            any -> 4848364
        end
    ).


-spec field_gen_type(Code :: piqirun_code(), X :: type()) -> iolist().

field_gen_type(Code, X) ->
    field_gen_name(Code, X).


-spec field_gen_piqi_record(Code :: piqirun_code(), X :: piqi_record()) -> iolist().

field_gen_piqi_record(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_repeated_field(9671866, fun field_gen_field/2, X#piqi_record.field),
        piqirun:gen_optional_field(31586877, fun field_gen_string/2, X#piqi_record.erlang_name),
        piqirun:gen_optional_field(90072013, fun field_gen_string/2, X#piqi_record.protobuf_name),
        piqirun:gen_repeated_field(112352691, fun field_gen_string/2, X#piqi_record.protobuf_custom),
        piqirun:gen_required_field(150958667, fun field_gen_name/2, X#piqi_record.name),
        piqirun:gen_optional_field(197354217, fun field_gen_bool/2, X#piqi_record.piq_positional),
        piqirun:gen_optional_field(515275216, fun field_gen_string/2, X#piqi_record.json_name)
    ]).


-spec field_gen_field(Code :: piqirun_code(), X :: field()) -> iolist().

field_gen_field(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(29667629, fun field_gen_int32/2, X#field.code),
        piqirun:gen_optional_field(31586877, fun field_gen_string/2, X#field.erlang_name),
        piqirun:gen_flag(69402483, X#field.deprecated),
        piqirun:gen_optional_field(90072013, fun field_gen_string/2, X#field.protobuf_name),
        piqirun:gen_optional_field(140563299, fun field_gen_field_mode/2, X#field.mode),
        piqirun:gen_optional_field(150958667, fun field_gen_name/2, X#field.name),
        piqirun:gen_flag(179842426, X#field.protobuf_packed),
        piqirun:gen_optional_field(197354217, fun field_gen_bool/2, X#field.piq_positional),
        piqirun:gen_optional_field(201807079, fun field_gen_bool/2, X#field.json_omit_missing),
        piqirun:gen_optional_field(215188758, fun field_gen_word/2, X#field.getopt_letter),
        piqirun:gen_optional_field(218690234, fun field_gen_type/2, X#field.type),
        piqirun:gen_optional_field(296833484, fun field_gen_piq_format/2, X#field.piq_format),
        piqirun:gen_optional_field(434682011, fun field_gen_name/2, X#field.piq_alias),
        piqirun:gen_optional_field(442330184, fun field_gen_string/2, X#field.getopt_doc),
        piqirun:gen_optional_field(465819841, fun field_gen_piqi_piqi_any/2, X#field.default),
        piqirun:gen_optional_field(515275216, fun field_gen_string/2, X#field.json_name)
    ]).


-spec field_gen_field_mode(Code :: piqirun_code(), X :: field_mode()) -> iolist().

field_gen_field_mode(Code, X) ->
    piqirun:integer_to_signed_varint(Code,
        case X of
            required -> 308449631;
            optional -> 510570400;
            repeated -> 274054266
        end
    ).


packed_field_gen_field_mode(X) ->
    piqirun:integer_to_packed_signed_varint(
        case X of
            required -> 308449631;
            optional -> 510570400;
            repeated -> 274054266
        end
    ).


-spec field_gen_variant(Code :: piqirun_code(), X :: variant()) -> iolist().

field_gen_variant(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(31586877, fun field_gen_string/2, X#variant.erlang_name),
        piqirun:gen_optional_field(90072013, fun field_gen_string/2, X#variant.protobuf_name),
        piqirun:gen_repeated_field(112352691, fun field_gen_string/2, X#variant.protobuf_custom),
        piqirun:gen_required_field(150958667, fun field_gen_name/2, X#variant.name),
        piqirun:gen_repeated_field(192598901, fun field_gen_option/2, X#variant.option),
        piqirun:gen_optional_field(515275216, fun field_gen_string/2, X#variant.json_name)
    ]).


-spec field_gen_option(Code :: piqirun_code(), X :: option()) -> iolist().

field_gen_option(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(29667629, fun field_gen_int32/2, X#option.code),
        piqirun:gen_optional_field(31586877, fun field_gen_string/2, X#option.erlang_name),
        piqirun:gen_flag(69402483, X#option.deprecated),
        piqirun:gen_optional_field(90072013, fun field_gen_string/2, X#option.protobuf_name),
        piqirun:gen_optional_field(150958667, fun field_gen_name/2, X#option.name),
        piqirun:gen_optional_field(215188758, fun field_gen_word/2, X#option.getopt_letter),
        piqirun:gen_optional_field(218690234, fun field_gen_type/2, X#option.type),
        piqirun:gen_optional_field(296833484, fun field_gen_piq_format/2, X#option.piq_format),
        piqirun:gen_optional_field(434682011, fun field_gen_name/2, X#option.piq_alias),
        piqirun:gen_optional_field(442330184, fun field_gen_string/2, X#option.getopt_doc),
        piqirun:gen_optional_field(515275216, fun field_gen_string/2, X#option.json_name)
    ]).


-spec field_gen_enum(Code :: piqirun_code(), X :: enum()) -> iolist().

field_gen_enum(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(31586877, fun field_gen_string/2, X#enum.erlang_name),
        piqirun:gen_optional_field(90072013, fun field_gen_string/2, X#enum.protobuf_name),
        piqirun:gen_repeated_field(112352691, fun field_gen_string/2, X#enum.protobuf_custom),
        piqirun:gen_required_field(150958667, fun field_gen_name/2, X#enum.name),
        piqirun:gen_repeated_field(192598901, fun field_gen_option/2, X#enum.option),
        piqirun:gen_optional_field(366391188, fun field_gen_string/2, X#enum.protobuf_prefix),
        piqirun:gen_optional_field(515275216, fun field_gen_string/2, X#enum.json_name)
    ]).


-spec field_gen_alias(Code :: piqirun_code(), X :: alias()) -> iolist().

field_gen_alias(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(31586877, fun field_gen_string/2, X#alias.erlang_name),
        piqirun:gen_optional_field(90072013, fun field_gen_string/2, X#alias.protobuf_name),
        piqirun:gen_optional_field(99318444, fun field_gen_string/2, X#alias.erlang_type),
        piqirun:gen_required_field(150958667, fun field_gen_name/2, X#alias.name),
        piqirun:gen_optional_field(157803580, fun field_gen_string/2, X#alias.protobuf_type),
        piqirun:gen_optional_field(198202944, fun field_gen_protobuf_wire_type/2, X#alias.protobuf_wire_type),
        piqirun:gen_optional_field(198318774, fun field_gen_piqi_type/2, X#alias.piqi_type),
        piqirun:gen_optional_field(218690234, fun field_gen_type/2, X#alias.type),
        piqirun:gen_optional_field(296833484, fun field_gen_piq_format/2, X#alias.piq_format),
        piqirun:gen_optional_field(400905231, fun field_gen_string/2, X#alias.erlang_default),
        piqirun:gen_optional_field(515275216, fun field_gen_string/2, X#alias.json_name)
    ]).


-spec field_gen_piqi_list(Code :: piqirun_code(), X :: piqi_list()) -> iolist().

field_gen_piqi_list(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(31586877, fun field_gen_string/2, X#piqi_list.erlang_name),
        piqirun:gen_optional_field(90072013, fun field_gen_string/2, X#piqi_list.protobuf_name),
        piqirun:gen_repeated_field(112352691, fun field_gen_string/2, X#piqi_list.protobuf_custom),
        piqirun:gen_required_field(150958667, fun field_gen_name/2, X#piqi_list.name),
        piqirun:gen_flag(179842426, X#piqi_list.protobuf_packed),
        piqirun:gen_required_field(218690234, fun field_gen_type/2, X#piqi_list.type),
        piqirun:gen_optional_field(296833484, fun field_gen_piq_format/2, X#piqi_list.piq_format),
        piqirun:gen_optional_field(515275216, fun field_gen_string/2, X#piqi_list.json_name)
    ]).


-spec field_gen_piqi(Code :: piqirun_code(), X :: piqi()) -> iolist().

field_gen_piqi(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(13841580, fun field_gen_word/2, X#piqi.module),
        piqirun:gen_optional_field(19040580, fun field_gen_erlang_string_type/2, X#piqi.erlang_string_type),
        piqirun:gen_optional_field(62639740, fun field_gen_string/2, X#piqi.file),
        piqirun:gen_repeated_field(112352691, fun field_gen_string/2, X#piqi.protobuf_custom),
        piqirun:gen_repeated_field(142778725, fun field_gen_import/2, X#piqi.import),
        piqirun:gen_repeated_field(162247646, fun field_gen_word/2, X#piqi.custom_field),
        piqirun:gen_optional_field(330902611, fun field_gen_string/2, X#piqi.erlang_type_prefix),
        piqirun:gen_repeated_field(340962072, fun field_gen_func/2, X#piqi.func),
        piqirun:gen_optional_field(376215364, fun field_gen_string/2, X#piqi.protobuf_package),
        piqirun:gen_repeated_field(416823115, fun field_gen_typedef/2, X#piqi.typedef),
        piqirun:gen_optional_field(492641566, fun field_gen_string/2, X#piqi.erlang_module)
    ]).


-spec field_gen_import(Code :: piqirun_code(), X :: import()) -> iolist().

field_gen_import(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(13841580, fun field_gen_word/2, X#import.module),
        piqirun:gen_optional_field(150958667, fun field_gen_name/2, X#import.name)
    ]).


-spec field_gen_piqi_any(Code :: piqirun_code(), X :: piqi_any()) -> iolist().

field_gen_piqi_any(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(5991895, fun field_gen_string/2, X#piqi_any.xml),
        piqirun:gen_optional_field(6461771, fun field_gen_binary/2, X#piqi_any.protobuf),
        piqirun:gen_optional_field(107495976, fun field_gen_string/2, X#piqi_any.json),
        piqirun:gen_optional_field(218690234, fun field_gen_string/2, X#piqi_any.type)
    ]).


-spec field_gen_func(Code :: piqirun_code(), X :: func()) -> iolist().

field_gen_func(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_optional_field(31586877, fun field_gen_string/2, X#func.erlang_name),
        piqirun:gen_required_field(150958667, fun field_gen_name/2, X#func.name),
        piqirun:gen_optional_field(209784577, fun field_gen_type/2, X#func.output),
        piqirun:gen_optional_field(321506248, fun field_gen_type/2, X#func.error),
        piqirun:gen_optional_field(505267210, fun field_gen_type/2, X#func.input)
    ]).


-spec field_gen_piqi_bundle(Code :: piqirun_code(), X :: piqi_bundle()) -> iolist().

field_gen_piqi_bundle(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_repeated_field(1, fun field_gen_piqi/2, X#piqi_bundle.piqi)
    ]).


-spec field_gen_erlang_string_type(Code :: piqirun_code(), X :: erlang_string_type()) -> iolist().

field_gen_erlang_string_type(Code, X) ->
    piqirun:integer_to_signed_varint(Code,
        case X of
            binary -> 218872833;
            list -> 129178718
        end
    ).


packed_field_gen_erlang_string_type(X) ->
    piqirun:integer_to_packed_signed_varint(
        case X of
            binary -> 218872833;
            list -> 129178718
        end
    ).


-spec gen_piq_format(X :: piq_format()) -> iolist().

gen_piq_format(X) ->
    field_gen_piq_format('undefined', X).


-spec gen_protobuf_int32(X :: integer()) -> iolist().

gen_protobuf_int32(X) ->
    field_gen_protobuf_int32('undefined', X).


-spec gen_protobuf_int64(X :: integer()) -> iolist().

gen_protobuf_int64(X) ->
    field_gen_protobuf_int64('undefined', X).


-spec gen_protobuf_wire_type(X :: protobuf_wire_type()) -> iolist().

gen_protobuf_wire_type(X) ->
    field_gen_protobuf_wire_type('undefined', X).


-spec gen_bool(X :: boolean()) -> iolist().

gen_bool(X) ->
    field_gen_bool('undefined', X).


-spec gen_string(X :: string() | binary()) -> iolist().

gen_string(X) ->
    field_gen_string('undefined', X).


-spec gen_binary(X :: binary()) -> iolist().

gen_binary(X) ->
    field_gen_binary('undefined', X).


-spec gen_piqi_piqi_any(X :: piqi_any()) -> iolist().

gen_piqi_piqi_any(X) ->
    field_gen_piqi_piqi_any('undefined', X).


-spec gen_int(X :: integer()) -> iolist().

gen_int(X) ->
    field_gen_int('undefined', X).


-spec gen_uint(X :: non_neg_integer()) -> iolist().

gen_uint(X) ->
    field_gen_uint('undefined', X).


-spec gen_int32(X :: integer()) -> iolist().

gen_int32(X) ->
    field_gen_int32('undefined', X).


-spec gen_uint32(X :: non_neg_integer()) -> iolist().

gen_uint32(X) ->
    field_gen_uint32('undefined', X).


-spec gen_int64(X :: integer()) -> iolist().

gen_int64(X) ->
    field_gen_int64('undefined', X).


-spec gen_uint64(X :: non_neg_integer()) -> iolist().

gen_uint64(X) ->
    field_gen_uint64('undefined', X).


-spec gen_float64(X :: number()) -> iolist().

gen_float64(X) ->
    field_gen_float64('undefined', X).


-spec gen_float32(X :: number()) -> iolist().

gen_float32(X) ->
    field_gen_float32('undefined', X).


-spec gen_int32_fixed(X :: integer()) -> iolist().

gen_int32_fixed(X) ->
    field_gen_int32_fixed('undefined', X).


-spec gen_uint32_fixed(X :: non_neg_integer()) -> iolist().

gen_uint32_fixed(X) ->
    field_gen_uint32_fixed('undefined', X).


-spec gen_int64_fixed(X :: integer()) -> iolist().

gen_int64_fixed(X) ->
    field_gen_int64_fixed('undefined', X).


-spec gen_uint64_fixed(X :: non_neg_integer()) -> iolist().

gen_uint64_fixed(X) ->
    field_gen_uint64_fixed('undefined', X).


-spec gen_float(X :: number()) -> iolist().

gen_float(X) ->
    field_gen_float('undefined', X).


-spec gen_word(X :: word()) -> iolist().

gen_word(X) ->
    field_gen_word('undefined', X).


-spec gen_name(X :: name()) -> iolist().

gen_name(X) ->
    field_gen_name('undefined', X).


-spec gen_typedef(X :: typedef()) -> iolist().

gen_typedef(X) ->
    field_gen_typedef('undefined', X).


-spec gen_piqi_type(X :: piqi_type()) -> iolist().

gen_piqi_type(X) ->
    field_gen_piqi_type('undefined', X).


-spec gen_type(X :: type()) -> iolist().

gen_type(X) ->
    field_gen_type('undefined', X).


-spec gen_piqi_record(X :: piqi_record()) -> iolist().

gen_piqi_record(X) ->
    field_gen_piqi_record('undefined', X).


-spec gen_field(X :: field()) -> iolist().

gen_field(X) ->
    field_gen_field('undefined', X).


-spec gen_field_mode(X :: field_mode()) -> iolist().

gen_field_mode(X) ->
    field_gen_field_mode('undefined', X).


-spec gen_variant(X :: variant()) -> iolist().

gen_variant(X) ->
    field_gen_variant('undefined', X).


-spec gen_option(X :: option()) -> iolist().

gen_option(X) ->
    field_gen_option('undefined', X).


-spec gen_enum(X :: enum()) -> iolist().

gen_enum(X) ->
    field_gen_enum('undefined', X).


-spec gen_alias(X :: alias()) -> iolist().

gen_alias(X) ->
    field_gen_alias('undefined', X).


-spec gen_piqi_list(X :: piqi_list()) -> iolist().

gen_piqi_list(X) ->
    field_gen_piqi_list('undefined', X).


-spec gen_piqi(X :: piqi()) -> iolist().

gen_piqi(X) ->
    field_gen_piqi('undefined', X).


-spec gen_import(X :: import()) -> iolist().

gen_import(X) ->
    field_gen_import('undefined', X).


-spec gen_piqi_any(X :: piqi_any()) -> iolist().

gen_piqi_any(X) ->
    field_gen_piqi_any('undefined', X).


-spec gen_func(X :: func()) -> iolist().

gen_func(X) ->
    field_gen_func('undefined', X).


-spec gen_piqi_bundle(X :: piqi_bundle()) -> iolist().

gen_piqi_bundle(X) ->
    field_gen_piqi_bundle('undefined', X).


-spec gen_erlang_string_type(X :: erlang_string_type()) -> iolist().

gen_erlang_string_type(X) ->
    field_gen_erlang_string_type('undefined', X).


gen_piq_format(X, Format) ->
    gen_piq_format(X, Format, []).


gen_piq_format(X, Format, Options) ->
    Iolist = gen_piq_format(X),
    piqirun_ext:convert(?MODULE, <<"piqi/piq-format">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_protobuf_int32(X, Format) ->
    gen_protobuf_int32(X, Format, []).


gen_protobuf_int32(X, Format, Options) ->
    Iolist = gen_protobuf_int32(X),
    piqirun_ext:convert(?MODULE, <<"protobuf-int32">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_protobuf_int64(X, Format) ->
    gen_protobuf_int64(X, Format, []).


gen_protobuf_int64(X, Format, Options) ->
    Iolist = gen_protobuf_int64(X),
    piqirun_ext:convert(?MODULE, <<"protobuf-int64">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_protobuf_wire_type(X, Format) ->
    gen_protobuf_wire_type(X, Format, []).


gen_protobuf_wire_type(X, Format, Options) ->
    Iolist = gen_protobuf_wire_type(X),
    piqirun_ext:convert(?MODULE, <<"piqi/protobuf-wire-type">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_bool(X, Format) ->
    gen_bool(X, Format, []).


gen_bool(X, Format, Options) ->
    Iolist = gen_bool(X),
    piqirun_ext:convert(?MODULE, <<"bool">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_string(X, Format) ->
    gen_string(X, Format, []).


gen_string(X, Format, Options) ->
    Iolist = gen_string(X),
    piqirun_ext:convert(?MODULE, <<"string">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_binary(X, Format) ->
    gen_binary(X, Format, []).


gen_binary(X, Format, Options) ->
    Iolist = gen_binary(X),
    piqirun_ext:convert(?MODULE, <<"binary">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_piqi_piqi_any(X, Format) ->
    gen_piqi_piqi_any(X, Format, []).


gen_piqi_piqi_any(X, Format, Options) ->
    Iolist = gen_piqi_piqi_any(X),
    piqirun_ext:convert(?MODULE, <<"piqi-any">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_int(X, Format) ->
    gen_int(X, Format, []).


gen_int(X, Format, Options) ->
    Iolist = gen_int(X),
    piqirun_ext:convert(?MODULE, <<"int">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_uint(X, Format) ->
    gen_uint(X, Format, []).


gen_uint(X, Format, Options) ->
    Iolist = gen_uint(X),
    piqirun_ext:convert(?MODULE, <<"uint">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_int32(X, Format) ->
    gen_int32(X, Format, []).


gen_int32(X, Format, Options) ->
    Iolist = gen_int32(X),
    piqirun_ext:convert(?MODULE, <<"int32">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_uint32(X, Format) ->
    gen_uint32(X, Format, []).


gen_uint32(X, Format, Options) ->
    Iolist = gen_uint32(X),
    piqirun_ext:convert(?MODULE, <<"uint32">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_int64(X, Format) ->
    gen_int64(X, Format, []).


gen_int64(X, Format, Options) ->
    Iolist = gen_int64(X),
    piqirun_ext:convert(?MODULE, <<"int64">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_uint64(X, Format) ->
    gen_uint64(X, Format, []).


gen_uint64(X, Format, Options) ->
    Iolist = gen_uint64(X),
    piqirun_ext:convert(?MODULE, <<"uint64">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_float64(X, Format) ->
    gen_float64(X, Format, []).


gen_float64(X, Format, Options) ->
    Iolist = gen_float64(X),
    piqirun_ext:convert(?MODULE, <<"float64">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_float32(X, Format) ->
    gen_float32(X, Format, []).


gen_float32(X, Format, Options) ->
    Iolist = gen_float32(X),
    piqirun_ext:convert(?MODULE, <<"float32">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_int32_fixed(X, Format) ->
    gen_int32_fixed(X, Format, []).


gen_int32_fixed(X, Format, Options) ->
    Iolist = gen_int32_fixed(X),
    piqirun_ext:convert(?MODULE, <<"int32-fixed">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_uint32_fixed(X, Format) ->
    gen_uint32_fixed(X, Format, []).


gen_uint32_fixed(X, Format, Options) ->
    Iolist = gen_uint32_fixed(X),
    piqirun_ext:convert(?MODULE, <<"uint32-fixed">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_int64_fixed(X, Format) ->
    gen_int64_fixed(X, Format, []).


gen_int64_fixed(X, Format, Options) ->
    Iolist = gen_int64_fixed(X),
    piqirun_ext:convert(?MODULE, <<"int64-fixed">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_uint64_fixed(X, Format) ->
    gen_uint64_fixed(X, Format, []).


gen_uint64_fixed(X, Format, Options) ->
    Iolist = gen_uint64_fixed(X),
    piqirun_ext:convert(?MODULE, <<"uint64-fixed">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_float(X, Format) ->
    gen_float(X, Format, []).


gen_float(X, Format, Options) ->
    Iolist = gen_float(X),
    piqirun_ext:convert(?MODULE, <<"float">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_word(X, Format) ->
    gen_word(X, Format, []).


gen_word(X, Format, Options) ->
    Iolist = gen_word(X),
    piqirun_ext:convert(?MODULE, <<"piqi/word">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_name(X, Format) ->
    gen_name(X, Format, []).


gen_name(X, Format, Options) ->
    Iolist = gen_name(X),
    piqirun_ext:convert(?MODULE, <<"piqi/name">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_typedef(X, Format) ->
    gen_typedef(X, Format, []).


gen_typedef(X, Format, Options) ->
    Iolist = gen_typedef(X),
    piqirun_ext:convert(?MODULE, <<"piqi/typedef">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_piqi_type(X, Format) ->
    gen_piqi_type(X, Format, []).


gen_piqi_type(X, Format, Options) ->
    Iolist = gen_piqi_type(X),
    piqirun_ext:convert(?MODULE, <<"piqi/piqi-type">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_type(X, Format) ->
    gen_type(X, Format, []).


gen_type(X, Format, Options) ->
    Iolist = gen_type(X),
    piqirun_ext:convert(?MODULE, <<"piqi/type">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_piqi_record(X, Format) ->
    gen_piqi_record(X, Format, []).


gen_piqi_record(X, Format, Options) ->
    Iolist = gen_piqi_record(X),
    piqirun_ext:convert(?MODULE, <<"piqi/record">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_field(X, Format) ->
    gen_field(X, Format, []).


gen_field(X, Format, Options) ->
    Iolist = gen_field(X),
    piqirun_ext:convert(?MODULE, <<"piqi/field">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_field_mode(X, Format) ->
    gen_field_mode(X, Format, []).


gen_field_mode(X, Format, Options) ->
    Iolist = gen_field_mode(X),
    piqirun_ext:convert(?MODULE, <<"piqi/field-mode">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_variant(X, Format) ->
    gen_variant(X, Format, []).


gen_variant(X, Format, Options) ->
    Iolist = gen_variant(X),
    piqirun_ext:convert(?MODULE, <<"piqi/variant">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_option(X, Format) ->
    gen_option(X, Format, []).


gen_option(X, Format, Options) ->
    Iolist = gen_option(X),
    piqirun_ext:convert(?MODULE, <<"piqi/option">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_enum(X, Format) ->
    gen_enum(X, Format, []).


gen_enum(X, Format, Options) ->
    Iolist = gen_enum(X),
    piqirun_ext:convert(?MODULE, <<"piqi/enum">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_alias(X, Format) ->
    gen_alias(X, Format, []).


gen_alias(X, Format, Options) ->
    Iolist = gen_alias(X),
    piqirun_ext:convert(?MODULE, <<"piqi/alias">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_piqi_list(X, Format) ->
    gen_piqi_list(X, Format, []).


gen_piqi_list(X, Format, Options) ->
    Iolist = gen_piqi_list(X),
    piqirun_ext:convert(?MODULE, <<"piqi/list">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_piqi(X, Format) ->
    gen_piqi(X, Format, []).


gen_piqi(X, Format, Options) ->
    Iolist = gen_piqi(X),
    piqirun_ext:convert(?MODULE, <<"piqi/piqi">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_import(X, Format) ->
    gen_import(X, Format, []).


gen_import(X, Format, Options) ->
    Iolist = gen_import(X),
    piqirun_ext:convert(?MODULE, <<"piqi/import">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_piqi_any(X, Format) ->
    gen_piqi_any(X, Format, []).


gen_piqi_any(X, Format, Options) ->
    Iolist = gen_piqi_any(X),
    piqirun_ext:convert(?MODULE, <<"piqi/any">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_func(X, Format) ->
    gen_func(X, Format, []).


gen_func(X, Format, Options) ->
    Iolist = gen_func(X),
    piqirun_ext:convert(?MODULE, <<"piqi/function">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_piqi_bundle(X, Format) ->
    gen_piqi_bundle(X, Format, []).


gen_piqi_bundle(X, Format, Options) ->
    Iolist = gen_piqi_bundle(X),
    piqirun_ext:convert(?MODULE, <<"piqi/piqi-list">>, 'pb', Format, iolist_to_binary(Iolist), Options).


gen_erlang_string_type(X, Format) ->
    gen_erlang_string_type(X, Format, []).


gen_erlang_string_type(X, Format, Options) ->
    Iolist = gen_erlang_string_type(X),
    piqirun_ext:convert(?MODULE, <<"piqi/erlang-string-type">>, 'pb', Format, iolist_to_binary(Iolist), Options).


-spec parse_piq_format(X :: piqirun_buffer()) -> piq_format().

parse_piq_format(X) ->
    {Code, Obj} = piqirun:parse_variant(X),
    case Code of
        251462090 when Obj =:= 1 -> word;
        217697453 when Obj =:= 1 -> text;
        _ -> piqirun:error_option(Obj, Code)
    end.


-spec parse_protobuf_int32(X :: piqirun_buffer()) -> integer().

parse_protobuf_int32(X) ->
    piqirun:integer_of_signed_varint(X).


packed_parse_protobuf_int32(X) ->
    {Res, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {Res, Rest}.


-spec parse_protobuf_int64(X :: piqirun_buffer()) -> integer().

parse_protobuf_int64(X) ->
    piqirun:integer_of_signed_varint(X).


packed_parse_protobuf_int64(X) ->
    {Res, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {Res, Rest}.


-spec parse_protobuf_wire_type(X :: piqirun_buffer()) -> protobuf_wire_type().

parse_protobuf_wire_type(X) ->
    case piqirun:integer_of_signed_varint(X) of
        329594984 -> varint;
        99211597 -> zigzag_varint;
        136997651 -> fixed32;
        136998322 -> fixed64;
        441915897 -> signed_varint;
        488499298 -> signed_fixed32;
        488499969 -> signed_fixed64;
        352089421 -> block;
        Y -> piqirun:error_enum_const(Y)
    end.


packed_parse_protobuf_wire_type(X) ->
    {Code, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {case Code of
        329594984 -> varint;
        99211597 -> zigzag_varint;
        136997651 -> fixed32;
        136998322 -> fixed64;
        441915897 -> signed_varint;
        488499298 -> signed_fixed32;
        488499969 -> signed_fixed64;
        352089421 -> block;
        Y -> piqirun:error_enum_const(Y)
    end, Rest}.


-spec parse_bool(X :: piqirun_buffer()) -> boolean().

parse_bool(X) ->
    piqirun:boolean_of_varint(X).


packed_parse_bool(X) ->
    {Res, Rest} = piqirun:boolean_of_packed_varint(X),
    {Res, Rest}.


-spec parse_string(X :: piqirun_buffer()) -> binary().

parse_string(X) ->
    piqirun:binary_string_of_block(X).


-spec parse_binary(X :: piqirun_buffer()) -> binary().

parse_binary(X) ->
    piqirun:binary_of_block(X).


-spec parse_piqi_piqi_any(X :: piqirun_buffer()) -> piqi_any().

parse_piqi_piqi_any(X) ->
    parse_piqi_any(X).


-spec parse_int(X :: piqirun_buffer()) -> integer().

parse_int(X) ->
    piqirun:integer_of_zigzag_varint(X).


packed_parse_int(X) ->
    {Res, Rest} = piqirun:integer_of_packed_zigzag_varint(X),
    {Res, Rest}.


-spec parse_uint(X :: piqirun_buffer()) -> non_neg_integer().

parse_uint(X) ->
    piqirun:non_neg_integer_of_varint(X).


packed_parse_uint(X) ->
    {Res, Rest} = piqirun:non_neg_integer_of_packed_varint(X),
    {Res, Rest}.


-spec parse_int32(X :: piqirun_buffer()) -> integer().

parse_int32(X) ->
    piqirun:integer_of_zigzag_varint(X).


packed_parse_int32(X) ->
    {Res, Rest} = piqirun:integer_of_packed_zigzag_varint(X),
    {Res, Rest}.


-spec parse_uint32(X :: piqirun_buffer()) -> non_neg_integer().

parse_uint32(X) ->
    piqirun:non_neg_integer_of_varint(X).


packed_parse_uint32(X) ->
    {Res, Rest} = piqirun:non_neg_integer_of_packed_varint(X),
    {Res, Rest}.


-spec parse_int64(X :: piqirun_buffer()) -> integer().

parse_int64(X) ->
    piqirun:integer_of_zigzag_varint(X).


packed_parse_int64(X) ->
    {Res, Rest} = piqirun:integer_of_packed_zigzag_varint(X),
    {Res, Rest}.


-spec parse_uint64(X :: piqirun_buffer()) -> non_neg_integer().

parse_uint64(X) ->
    piqirun:non_neg_integer_of_varint(X).


packed_parse_uint64(X) ->
    {Res, Rest} = piqirun:non_neg_integer_of_packed_varint(X),
    {Res, Rest}.


-spec parse_float64(X :: piqirun_buffer()) -> float().

parse_float64(X) ->
    piqirun:float_of_fixed64(X).


packed_parse_float64(X) ->
    {Res, Rest} = piqirun:float_of_packed_fixed64(X),
    {Res, Rest}.


-spec parse_float32(X :: piqirun_buffer()) -> float().

parse_float32(X) ->
    piqirun:float_of_fixed32(X).


packed_parse_float32(X) ->
    {Res, Rest} = piqirun:float_of_packed_fixed32(X),
    {Res, Rest}.


-spec parse_int32_fixed(X :: piqirun_buffer()) -> integer().

parse_int32_fixed(X) ->
    piqirun:integer_of_signed_fixed32(X).


packed_parse_int32_fixed(X) ->
    {Res, Rest} = piqirun:integer_of_packed_signed_fixed32(X),
    {Res, Rest}.


-spec parse_uint32_fixed(X :: piqirun_buffer()) -> non_neg_integer().

parse_uint32_fixed(X) ->
    piqirun:non_neg_integer_of_fixed32(X).


packed_parse_uint32_fixed(X) ->
    {Res, Rest} = piqirun:non_neg_integer_of_packed_fixed32(X),
    {Res, Rest}.


-spec parse_int64_fixed(X :: piqirun_buffer()) -> integer().

parse_int64_fixed(X) ->
    piqirun:integer_of_signed_fixed64(X).


packed_parse_int64_fixed(X) ->
    {Res, Rest} = piqirun:integer_of_packed_signed_fixed64(X),
    {Res, Rest}.


-spec parse_uint64_fixed(X :: piqirun_buffer()) -> non_neg_integer().

parse_uint64_fixed(X) ->
    piqirun:non_neg_integer_of_fixed64(X).


packed_parse_uint64_fixed(X) ->
    {Res, Rest} = piqirun:non_neg_integer_of_packed_fixed64(X),
    {Res, Rest}.


-spec parse_float(X :: piqirun_buffer()) -> float().

parse_float(X) ->
    parse_float64(X).


packed_parse_float(X) ->
    {Res, Rest} = packed_parse_float64(X),
    {Res, Rest}.


-spec parse_word(X :: piqirun_buffer()) -> binary().

parse_word(X) ->
    parse_string(X).


-spec parse_name(X :: piqirun_buffer()) -> binary().

parse_name(X) ->
    parse_word(X).


-spec parse_typedef(X :: piqirun_buffer()) -> typedef().

parse_typedef(X) ->
    {Code, Obj} = piqirun:parse_variant(X),
    case Code of
        502036113 -> {piqi_record, parse_piqi_record(Obj)};
        484589701 -> {variant, parse_variant(Obj)};
        51800833 -> {enum, parse_enum(Obj)};
        26300816 -> {alias, parse_alias(Obj)};
        129178718 -> {piqi_list, parse_piqi_list(Obj)};
        _ -> piqirun:error_option(Obj, Code)
    end.


-spec parse_piqi_type(X :: piqirun_buffer()) -> piqi_type().

parse_piqi_type(X) ->
    case piqirun:integer_of_signed_varint(X) of
        5246191 -> int;
        43435420 -> float;
        18580522 -> bool;
        288368849 -> string;
        218872833 -> binary;
        4848364 -> any;
        Y -> piqirun:error_enum_const(Y)
    end.


packed_parse_piqi_type(X) ->
    {Code, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {case Code of
        5246191 -> int;
        43435420 -> float;
        18580522 -> bool;
        288368849 -> string;
        218872833 -> binary;
        4848364 -> any;
        Y -> piqirun:error_enum_const(Y)
    end, Rest}.


-spec parse_type(X :: piqirun_buffer()) -> binary().

parse_type(X) ->
    parse_name(X).


-spec parse_piqi_record(X :: piqirun_buffer()) -> piqi_record().

parse_piqi_record(X) ->
    R0 = piqirun:parse_record(X),
    {_Field, R1} = piqirun:parse_repeated_field(9671866, fun parse_field/1, R0),
    {_Erlang_name, R2} = piqirun:parse_optional_field(31586877, fun parse_string/1, R1),
    {_Protobuf_name, R3} = piqirun:parse_optional_field(90072013, fun parse_string/1, R2),
    {_Protobuf_custom, R4} = piqirun:parse_repeated_field(112352691, fun parse_string/1, R3),
    {_Name, R5} = piqirun:parse_required_field(150958667, fun parse_name/1, R4),
    {_Piq_positional, R6} = piqirun:parse_optional_field(197354217, fun parse_bool/1, R5),
    {_Json_name, R7} = piqirun:parse_optional_field(515275216, fun parse_string/1, R6),
    _ = R7,
    #piqi_record{
        field = _Field,
        erlang_name = _Erlang_name,
        protobuf_name = _Protobuf_name,
        protobuf_custom = _Protobuf_custom,
        name = _Name,
        piq_positional = _Piq_positional,
        json_name = _Json_name
    }.


-spec parse_field(X :: piqirun_buffer()) -> field().

parse_field(X) ->
    R0 = piqirun:parse_record(X),
    {_Code, R1} = piqirun:parse_optional_field(29667629, fun parse_int32/1, R0),
    {_Erlang_name, R2} = piqirun:parse_optional_field(31586877, fun parse_string/1, R1),
    {_Deprecated, R3} = piqirun:parse_flag(69402483, R2),
    {_Protobuf_name, R4} = piqirun:parse_optional_field(90072013, fun parse_string/1, R3),
    {_Mode, R5} = piqirun:parse_optional_field(140563299, fun parse_field_mode/1, R4, <<8,223,162,138,147,1>>),
    {_Name, R6} = piqirun:parse_optional_field(150958667, fun parse_name/1, R5),
    {_Protobuf_packed, R7} = piqirun:parse_flag(179842426, R6),
    {_Piq_positional, R8} = piqirun:parse_optional_field(197354217, fun parse_bool/1, R7),
    {_Json_omit_missing, R9} = piqirun:parse_optional_field(201807079, fun parse_bool/1, R8),
    {_Getopt_letter, R10} = piqirun:parse_optional_field(215188758, fun parse_word/1, R9),
    {_Type, R11} = piqirun:parse_optional_field(218690234, fun parse_type/1, R10),
    {_Piq_format, R12} = piqirun:parse_optional_field(296833484, fun parse_piq_format/1, R11),
    {_Piq_alias, R13} = piqirun:parse_optional_field(434682011, fun parse_name/1, R12),
    {_Getopt_doc, R14} = piqirun:parse_optional_field(442330184, fun parse_string/1, R13),
    {_Default, R15} = piqirun:parse_optional_field(465819841, fun parse_piqi_piqi_any/1, R14),
    {_Json_name, R16} = piqirun:parse_optional_field(515275216, fun parse_string/1, R15),
    _ = R16,
    #field{
        code = _Code,
        erlang_name = _Erlang_name,
        deprecated = _Deprecated,
        protobuf_name = _Protobuf_name,
        mode = _Mode,
        name = _Name,
        protobuf_packed = _Protobuf_packed,
        piq_positional = _Piq_positional,
        json_omit_missing = _Json_omit_missing,
        getopt_letter = _Getopt_letter,
        type = _Type,
        piq_format = _Piq_format,
        piq_alias = _Piq_alias,
        getopt_doc = _Getopt_doc,
        default = _Default,
        json_name = _Json_name
    }.


-spec parse_field_mode(X :: piqirun_buffer()) -> field_mode().

parse_field_mode(X) ->
    case piqirun:integer_of_signed_varint(X) of
        308449631 -> required;
        510570400 -> optional;
        274054266 -> repeated;
        Y -> piqirun:error_enum_const(Y)
    end.


packed_parse_field_mode(X) ->
    {Code, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {case Code of
        308449631 -> required;
        510570400 -> optional;
        274054266 -> repeated;
        Y -> piqirun:error_enum_const(Y)
    end, Rest}.


-spec parse_variant(X :: piqirun_buffer()) -> variant().

parse_variant(X) ->
    R0 = piqirun:parse_record(X),
    {_Erlang_name, R1} = piqirun:parse_optional_field(31586877, fun parse_string/1, R0),
    {_Protobuf_name, R2} = piqirun:parse_optional_field(90072013, fun parse_string/1, R1),
    {_Protobuf_custom, R3} = piqirun:parse_repeated_field(112352691, fun parse_string/1, R2),
    {_Name, R4} = piqirun:parse_required_field(150958667, fun parse_name/1, R3),
    {_Option, R5} = piqirun:parse_repeated_field(192598901, fun parse_option/1, R4),
    {_Json_name, R6} = piqirun:parse_optional_field(515275216, fun parse_string/1, R5),
    _ = R6,
    #variant{
        erlang_name = _Erlang_name,
        protobuf_name = _Protobuf_name,
        protobuf_custom = _Protobuf_custom,
        name = _Name,
        option = _Option,
        json_name = _Json_name
    }.


-spec parse_option(X :: piqirun_buffer()) -> option().

parse_option(X) ->
    R0 = piqirun:parse_record(X),
    {_Code, R1} = piqirun:parse_optional_field(29667629, fun parse_int32/1, R0),
    {_Erlang_name, R2} = piqirun:parse_optional_field(31586877, fun parse_string/1, R1),
    {_Deprecated, R3} = piqirun:parse_flag(69402483, R2),
    {_Protobuf_name, R4} = piqirun:parse_optional_field(90072013, fun parse_string/1, R3),
    {_Name, R5} = piqirun:parse_optional_field(150958667, fun parse_name/1, R4),
    {_Getopt_letter, R6} = piqirun:parse_optional_field(215188758, fun parse_word/1, R5),
    {_Type, R7} = piqirun:parse_optional_field(218690234, fun parse_type/1, R6),
    {_Piq_format, R8} = piqirun:parse_optional_field(296833484, fun parse_piq_format/1, R7),
    {_Piq_alias, R9} = piqirun:parse_optional_field(434682011, fun parse_name/1, R8),
    {_Getopt_doc, R10} = piqirun:parse_optional_field(442330184, fun parse_string/1, R9),
    {_Json_name, R11} = piqirun:parse_optional_field(515275216, fun parse_string/1, R10),
    _ = R11,
    #option{
        code = _Code,
        erlang_name = _Erlang_name,
        deprecated = _Deprecated,
        protobuf_name = _Protobuf_name,
        name = _Name,
        getopt_letter = _Getopt_letter,
        type = _Type,
        piq_format = _Piq_format,
        piq_alias = _Piq_alias,
        getopt_doc = _Getopt_doc,
        json_name = _Json_name
    }.


-spec parse_enum(X :: piqirun_buffer()) -> enum().

parse_enum(X) ->
    R0 = piqirun:parse_record(X),
    {_Erlang_name, R1} = piqirun:parse_optional_field(31586877, fun parse_string/1, R0),
    {_Protobuf_name, R2} = piqirun:parse_optional_field(90072013, fun parse_string/1, R1),
    {_Protobuf_custom, R3} = piqirun:parse_repeated_field(112352691, fun parse_string/1, R2),
    {_Name, R4} = piqirun:parse_required_field(150958667, fun parse_name/1, R3),
    {_Option, R5} = piqirun:parse_repeated_field(192598901, fun parse_option/1, R4),
    {_Protobuf_prefix, R6} = piqirun:parse_optional_field(366391188, fun parse_string/1, R5),
    {_Json_name, R7} = piqirun:parse_optional_field(515275216, fun parse_string/1, R6),
    _ = R7,
    #enum{
        erlang_name = _Erlang_name,
        protobuf_name = _Protobuf_name,
        protobuf_custom = _Protobuf_custom,
        name = _Name,
        option = _Option,
        protobuf_prefix = _Protobuf_prefix,
        json_name = _Json_name
    }.


-spec parse_alias(X :: piqirun_buffer()) -> alias().

parse_alias(X) ->
    R0 = piqirun:parse_record(X),
    {_Erlang_name, R1} = piqirun:parse_optional_field(31586877, fun parse_string/1, R0),
    {_Protobuf_name, R2} = piqirun:parse_optional_field(90072013, fun parse_string/1, R1),
    {_Erlang_type, R3} = piqirun:parse_optional_field(99318444, fun parse_string/1, R2),
    {_Name, R4} = piqirun:parse_required_field(150958667, fun parse_name/1, R3),
    {_Protobuf_type, R5} = piqirun:parse_optional_field(157803580, fun parse_string/1, R4),
    {_Protobuf_wire_type, R6} = piqirun:parse_optional_field(198202944, fun parse_protobuf_wire_type/1, R5),
    {_Piqi_type, R7} = piqirun:parse_optional_field(198318774, fun parse_piqi_type/1, R6),
    {_Type, R8} = piqirun:parse_optional_field(218690234, fun parse_type/1, R7),
    {_Piq_format, R9} = piqirun:parse_optional_field(296833484, fun parse_piq_format/1, R8),
    {_Erlang_default, R10} = piqirun:parse_optional_field(400905231, fun parse_string/1, R9),
    {_Json_name, R11} = piqirun:parse_optional_field(515275216, fun parse_string/1, R10),
    _ = R11,
    #alias{
        erlang_name = _Erlang_name,
        protobuf_name = _Protobuf_name,
        erlang_type = _Erlang_type,
        name = _Name,
        protobuf_type = _Protobuf_type,
        protobuf_wire_type = _Protobuf_wire_type,
        piqi_type = _Piqi_type,
        type = _Type,
        piq_format = _Piq_format,
        erlang_default = _Erlang_default,
        json_name = _Json_name
    }.


-spec parse_piqi_list(X :: piqirun_buffer()) -> piqi_list().

parse_piqi_list(X) ->
    R0 = piqirun:parse_record(X),
    {_Erlang_name, R1} = piqirun:parse_optional_field(31586877, fun parse_string/1, R0),
    {_Protobuf_name, R2} = piqirun:parse_optional_field(90072013, fun parse_string/1, R1),
    {_Protobuf_custom, R3} = piqirun:parse_repeated_field(112352691, fun parse_string/1, R2),
    {_Name, R4} = piqirun:parse_required_field(150958667, fun parse_name/1, R3),
    {_Protobuf_packed, R5} = piqirun:parse_flag(179842426, R4),
    {_Type, R6} = piqirun:parse_required_field(218690234, fun parse_type/1, R5),
    {_Piq_format, R7} = piqirun:parse_optional_field(296833484, fun parse_piq_format/1, R6),
    {_Json_name, R8} = piqirun:parse_optional_field(515275216, fun parse_string/1, R7),
    _ = R8,
    #piqi_list{
        erlang_name = _Erlang_name,
        protobuf_name = _Protobuf_name,
        protobuf_custom = _Protobuf_custom,
        name = _Name,
        protobuf_packed = _Protobuf_packed,
        type = _Type,
        piq_format = _Piq_format,
        json_name = _Json_name
    }.


-spec parse_piqi(X :: piqirun_buffer()) -> piqi().

parse_piqi(X) ->
    R0 = piqirun:parse_record(X),
    {_Module, R1} = piqirun:parse_optional_field(13841580, fun parse_word/1, R0),
    {_Erlang_string_type, R2} = piqirun:parse_optional_field(19040580, fun parse_erlang_string_type/1, R1, <<8,129,248,174,104>>),
    {_File, R3} = piqirun:parse_optional_field(62639740, fun parse_string/1, R2),
    {_Protobuf_custom, R4} = piqirun:parse_repeated_field(112352691, fun parse_string/1, R3),
    {_Import, R5} = piqirun:parse_repeated_field(142778725, fun parse_import/1, R4),
    {_Custom_field, R6} = piqirun:parse_repeated_field(162247646, fun parse_word/1, R5),
    {_Erlang_type_prefix, R7} = piqirun:parse_optional_field(330902611, fun parse_string/1, R6),
    {_Func, R8} = piqirun:parse_repeated_field(340962072, fun parse_func/1, R7),
    {_Protobuf_package, R9} = piqirun:parse_optional_field(376215364, fun parse_string/1, R8),
    {_Typedef, R10} = piqirun:parse_repeated_field(416823115, fun parse_typedef/1, R9),
    {_Erlang_module, R11} = piqirun:parse_optional_field(492641566, fun parse_string/1, R10),
    _ = R11,
    #piqi{
        module = _Module,
        erlang_string_type = _Erlang_string_type,
        file = _File,
        protobuf_custom = _Protobuf_custom,
        import = _Import,
        custom_field = _Custom_field,
        erlang_type_prefix = _Erlang_type_prefix,
        func = _Func,
        protobuf_package = _Protobuf_package,
        typedef = _Typedef,
        erlang_module = _Erlang_module
    }.


-spec parse_import(X :: piqirun_buffer()) -> import().

parse_import(X) ->
    R0 = piqirun:parse_record(X),
    {_Module, R1} = piqirun:parse_required_field(13841580, fun parse_word/1, R0),
    {_Name, R2} = piqirun:parse_optional_field(150958667, fun parse_name/1, R1),
    _ = R2,
    #import{
        module = _Module,
        name = _Name
    }.


-spec parse_piqi_any(X :: piqirun_buffer()) -> piqi_any().

parse_piqi_any(X) ->
    R0 = piqirun:parse_record(X),
    {_Xml, R1} = piqirun:parse_optional_field(5991895, fun parse_string/1, R0),
    {_Protobuf, R2} = piqirun:parse_optional_field(6461771, fun parse_binary/1, R1),
    {_Json, R3} = piqirun:parse_optional_field(107495976, fun parse_string/1, R2),
    {_Type, R4} = piqirun:parse_optional_field(218690234, fun parse_string/1, R3),
    _ = R4,
    #piqi_any{
        xml = _Xml,
        protobuf = _Protobuf,
        json = _Json,
        type = _Type
    }.


-spec parse_func(X :: piqirun_buffer()) -> func().

parse_func(X) ->
    R0 = piqirun:parse_record(X),
    {_Erlang_name, R1} = piqirun:parse_optional_field(31586877, fun parse_string/1, R0),
    {_Name, R2} = piqirun:parse_required_field(150958667, fun parse_name/1, R1),
    {_Output, R3} = piqirun:parse_optional_field(209784577, fun parse_type/1, R2),
    {_Error, R4} = piqirun:parse_optional_field(321506248, fun parse_type/1, R3),
    {_Input, R5} = piqirun:parse_optional_field(505267210, fun parse_type/1, R4),
    _ = R5,
    #func{
        erlang_name = _Erlang_name,
        name = _Name,
        output = _Output,
        error = _Error,
        input = _Input
    }.


-spec parse_piqi_bundle(X :: piqirun_buffer()) -> piqi_bundle().

parse_piqi_bundle(X) ->
    R0 = piqirun:parse_record(X),
    {_Piqi, R1} = piqirun:parse_repeated_field(1, fun parse_piqi/1, R0),
    _ = R1,
    #piqi_bundle{
        piqi = _Piqi
    }.


-spec parse_erlang_string_type(X :: piqirun_buffer()) -> erlang_string_type().

parse_erlang_string_type(X) ->
    case piqirun:integer_of_signed_varint(X) of
        218872833 -> binary;
        129178718 -> list;
        Y -> piqirun:error_enum_const(Y)
    end.


packed_parse_erlang_string_type(X) ->
    {Code, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {case Code of
        218872833 -> binary;
        129178718 -> list;
        Y -> piqirun:error_enum_const(Y)
    end, Rest}.


parse_piq_format(X, Format) ->
    parse_piq_format(X, Format, []).


parse_piq_format(X, Format, Options) ->
    parse_piq_format(
        piqirun_ext:convert(?MODULE, <<"piqi/piq-format">>, Format, 'pb', X, Options)).


parse_protobuf_int32(X, Format) ->
    parse_protobuf_int32(X, Format, []).


parse_protobuf_int32(X, Format, Options) ->
    parse_protobuf_int32(
        piqirun_ext:convert(?MODULE, <<"protobuf-int32">>, Format, 'pb', X, Options)).


parse_protobuf_int64(X, Format) ->
    parse_protobuf_int64(X, Format, []).


parse_protobuf_int64(X, Format, Options) ->
    parse_protobuf_int64(
        piqirun_ext:convert(?MODULE, <<"protobuf-int64">>, Format, 'pb', X, Options)).


parse_protobuf_wire_type(X, Format) ->
    parse_protobuf_wire_type(X, Format, []).


parse_protobuf_wire_type(X, Format, Options) ->
    parse_protobuf_wire_type(
        piqirun_ext:convert(?MODULE, <<"piqi/protobuf-wire-type">>, Format, 'pb', X, Options)).


parse_bool(X, Format) ->
    parse_bool(X, Format, []).


parse_bool(X, Format, Options) ->
    parse_bool(
        piqirun_ext:convert(?MODULE, <<"bool">>, Format, 'pb', X, Options)).


parse_string(X, Format) ->
    parse_string(X, Format, []).


parse_string(X, Format, Options) ->
    parse_string(
        piqirun_ext:convert(?MODULE, <<"string">>, Format, 'pb', X, Options)).


parse_binary(X, Format) ->
    parse_binary(X, Format, []).


parse_binary(X, Format, Options) ->
    parse_binary(
        piqirun_ext:convert(?MODULE, <<"binary">>, Format, 'pb', X, Options)).


parse_piqi_piqi_any(X, Format) ->
    parse_piqi_piqi_any(X, Format, []).


parse_piqi_piqi_any(X, Format, Options) ->
    parse_piqi_piqi_any(
        piqirun_ext:convert(?MODULE, <<"piqi-any">>, Format, 'pb', X, Options)).


parse_int(X, Format) ->
    parse_int(X, Format, []).


parse_int(X, Format, Options) ->
    parse_int(
        piqirun_ext:convert(?MODULE, <<"int">>, Format, 'pb', X, Options)).


parse_uint(X, Format) ->
    parse_uint(X, Format, []).


parse_uint(X, Format, Options) ->
    parse_uint(
        piqirun_ext:convert(?MODULE, <<"uint">>, Format, 'pb', X, Options)).


parse_int32(X, Format) ->
    parse_int32(X, Format, []).


parse_int32(X, Format, Options) ->
    parse_int32(
        piqirun_ext:convert(?MODULE, <<"int32">>, Format, 'pb', X, Options)).


parse_uint32(X, Format) ->
    parse_uint32(X, Format, []).


parse_uint32(X, Format, Options) ->
    parse_uint32(
        piqirun_ext:convert(?MODULE, <<"uint32">>, Format, 'pb', X, Options)).


parse_int64(X, Format) ->
    parse_int64(X, Format, []).


parse_int64(X, Format, Options) ->
    parse_int64(
        piqirun_ext:convert(?MODULE, <<"int64">>, Format, 'pb', X, Options)).


parse_uint64(X, Format) ->
    parse_uint64(X, Format, []).


parse_uint64(X, Format, Options) ->
    parse_uint64(
        piqirun_ext:convert(?MODULE, <<"uint64">>, Format, 'pb', X, Options)).


parse_float64(X, Format) ->
    parse_float64(X, Format, []).


parse_float64(X, Format, Options) ->
    parse_float64(
        piqirun_ext:convert(?MODULE, <<"float64">>, Format, 'pb', X, Options)).


parse_float32(X, Format) ->
    parse_float32(X, Format, []).


parse_float32(X, Format, Options) ->
    parse_float32(
        piqirun_ext:convert(?MODULE, <<"float32">>, Format, 'pb', X, Options)).


parse_int32_fixed(X, Format) ->
    parse_int32_fixed(X, Format, []).


parse_int32_fixed(X, Format, Options) ->
    parse_int32_fixed(
        piqirun_ext:convert(?MODULE, <<"int32-fixed">>, Format, 'pb', X, Options)).


parse_uint32_fixed(X, Format) ->
    parse_uint32_fixed(X, Format, []).


parse_uint32_fixed(X, Format, Options) ->
    parse_uint32_fixed(
        piqirun_ext:convert(?MODULE, <<"uint32-fixed">>, Format, 'pb', X, Options)).


parse_int64_fixed(X, Format) ->
    parse_int64_fixed(X, Format, []).


parse_int64_fixed(X, Format, Options) ->
    parse_int64_fixed(
        piqirun_ext:convert(?MODULE, <<"int64-fixed">>, Format, 'pb', X, Options)).


parse_uint64_fixed(X, Format) ->
    parse_uint64_fixed(X, Format, []).


parse_uint64_fixed(X, Format, Options) ->
    parse_uint64_fixed(
        piqirun_ext:convert(?MODULE, <<"uint64-fixed">>, Format, 'pb', X, Options)).


parse_float(X, Format) ->
    parse_float(X, Format, []).


parse_float(X, Format, Options) ->
    parse_float(
        piqirun_ext:convert(?MODULE, <<"float">>, Format, 'pb', X, Options)).


parse_word(X, Format) ->
    parse_word(X, Format, []).


parse_word(X, Format, Options) ->
    parse_word(
        piqirun_ext:convert(?MODULE, <<"piqi/word">>, Format, 'pb', X, Options)).


parse_name(X, Format) ->
    parse_name(X, Format, []).


parse_name(X, Format, Options) ->
    parse_name(
        piqirun_ext:convert(?MODULE, <<"piqi/name">>, Format, 'pb', X, Options)).


parse_typedef(X, Format) ->
    parse_typedef(X, Format, []).


parse_typedef(X, Format, Options) ->
    parse_typedef(
        piqirun_ext:convert(?MODULE, <<"piqi/typedef">>, Format, 'pb', X, Options)).


parse_piqi_type(X, Format) ->
    parse_piqi_type(X, Format, []).


parse_piqi_type(X, Format, Options) ->
    parse_piqi_type(
        piqirun_ext:convert(?MODULE, <<"piqi/piqi-type">>, Format, 'pb', X, Options)).


parse_type(X, Format) ->
    parse_type(X, Format, []).


parse_type(X, Format, Options) ->
    parse_type(
        piqirun_ext:convert(?MODULE, <<"piqi/type">>, Format, 'pb', X, Options)).


parse_piqi_record(X, Format) ->
    parse_piqi_record(X, Format, []).


parse_piqi_record(X, Format, Options) ->
    parse_piqi_record(
        piqirun_ext:convert(?MODULE, <<"piqi/record">>, Format, 'pb', X, Options)).


parse_field(X, Format) ->
    parse_field(X, Format, []).


parse_field(X, Format, Options) ->
    parse_field(
        piqirun_ext:convert(?MODULE, <<"piqi/field">>, Format, 'pb', X, Options)).


parse_field_mode(X, Format) ->
    parse_field_mode(X, Format, []).


parse_field_mode(X, Format, Options) ->
    parse_field_mode(
        piqirun_ext:convert(?MODULE, <<"piqi/field-mode">>, Format, 'pb', X, Options)).


parse_variant(X, Format) ->
    parse_variant(X, Format, []).


parse_variant(X, Format, Options) ->
    parse_variant(
        piqirun_ext:convert(?MODULE, <<"piqi/variant">>, Format, 'pb', X, Options)).


parse_option(X, Format) ->
    parse_option(X, Format, []).


parse_option(X, Format, Options) ->
    parse_option(
        piqirun_ext:convert(?MODULE, <<"piqi/option">>, Format, 'pb', X, Options)).


parse_enum(X, Format) ->
    parse_enum(X, Format, []).


parse_enum(X, Format, Options) ->
    parse_enum(
        piqirun_ext:convert(?MODULE, <<"piqi/enum">>, Format, 'pb', X, Options)).


parse_alias(X, Format) ->
    parse_alias(X, Format, []).


parse_alias(X, Format, Options) ->
    parse_alias(
        piqirun_ext:convert(?MODULE, <<"piqi/alias">>, Format, 'pb', X, Options)).


parse_piqi_list(X, Format) ->
    parse_piqi_list(X, Format, []).


parse_piqi_list(X, Format, Options) ->
    parse_piqi_list(
        piqirun_ext:convert(?MODULE, <<"piqi/list">>, Format, 'pb', X, Options)).


parse_piqi(X, Format) ->
    parse_piqi(X, Format, []).


parse_piqi(X, Format, Options) ->
    parse_piqi(
        piqirun_ext:convert(?MODULE, <<"piqi/piqi">>, Format, 'pb', X, Options)).


parse_import(X, Format) ->
    parse_import(X, Format, []).


parse_import(X, Format, Options) ->
    parse_import(
        piqirun_ext:convert(?MODULE, <<"piqi/import">>, Format, 'pb', X, Options)).


parse_piqi_any(X, Format) ->
    parse_piqi_any(X, Format, []).


parse_piqi_any(X, Format, Options) ->
    parse_piqi_any(
        piqirun_ext:convert(?MODULE, <<"piqi/any">>, Format, 'pb', X, Options)).


parse_func(X, Format) ->
    parse_func(X, Format, []).


parse_func(X, Format, Options) ->
    parse_func(
        piqirun_ext:convert(?MODULE, <<"piqi/function">>, Format, 'pb', X, Options)).


parse_piqi_bundle(X, Format) ->
    parse_piqi_bundle(X, Format, []).


parse_piqi_bundle(X, Format, Options) ->
    parse_piqi_bundle(
        piqirun_ext:convert(?MODULE, <<"piqi/piqi-list">>, Format, 'pb', X, Options)).


parse_erlang_string_type(X, Format) ->
    parse_erlang_string_type(X, Format, []).


parse_erlang_string_type(X, Format, Options) ->
    parse_erlang_string_type(
        piqirun_ext:convert(?MODULE, <<"piqi/erlang-string-type">>, Format, 'pb', X, Options)).


default_piq_format() -> word.


default_protobuf_int32() -> default_int32().


default_protobuf_int64() -> default_int64().


default_protobuf_wire_type() -> varint.


default_bool() -> false.


default_string() -> <<>>.


default_binary() -> <<>>.


default_piqi_piqi_any() -> default_piqi_any().


default_int() -> 0.


default_uint() -> 0.


default_int32() -> 0.


default_uint32() -> 0.


default_int64() -> 0.


default_uint64() -> 0.


default_float64() -> 0.0.


default_float32() -> 0.0.


default_int32_fixed() -> default_int32().


default_uint32_fixed() -> default_uint32().


default_int64_fixed() -> default_int64().


default_uint64_fixed() -> default_uint64().


default_float() -> default_float64().


default_word() -> default_string().


default_name() -> default_word().


default_typedef() -> {piqi_record, default_piqi_record()}.


default_piqi_type() -> int.


default_type() -> default_name().


default_piqi_record() ->
    #piqi_record{
        name = default_name(),
        field = [],
        piq_positional = 'undefined',
        protobuf_name = 'undefined',
        protobuf_custom = [],
        json_name = 'undefined',
        erlang_name = 'undefined'
    }.


default_field() ->
    #field{
        name = 'undefined',
        type = 'undefined',
        mode = 'undefined',
        default = 'undefined',
        deprecated = false,
        piq_format = 'undefined',
        piq_positional = 'undefined',
        piq_alias = 'undefined',
        protobuf_name = 'undefined',
        code = 'undefined',
        protobuf_packed = false,
        json_name = 'undefined',
        json_omit_missing = 'undefined',
        getopt_letter = 'undefined',
        getopt_doc = 'undefined',
        erlang_name = 'undefined'
    }.


default_field_mode() -> required.


default_variant() ->
    #variant{
        name = default_name(),
        option = [],
        protobuf_name = 'undefined',
        protobuf_custom = [],
        json_name = 'undefined',
        erlang_name = 'undefined'
    }.


default_option() ->
    #option{
        name = 'undefined',
        type = 'undefined',
        deprecated = false,
        piq_format = 'undefined',
        piq_alias = 'undefined',
        protobuf_name = 'undefined',
        code = 'undefined',
        json_name = 'undefined',
        getopt_letter = 'undefined',
        getopt_doc = 'undefined',
        erlang_name = 'undefined'
    }.


default_enum() ->
    #enum{
        name = default_name(),
        option = [],
        protobuf_name = 'undefined',
        protobuf_custom = [],
        protobuf_prefix = 'undefined',
        json_name = 'undefined',
        erlang_name = 'undefined'
    }.


default_alias() ->
    #alias{
        name = default_name(),
        type = 'undefined',
        piqi_type = 'undefined',
        piq_format = 'undefined',
        protobuf_name = 'undefined',
        protobuf_type = 'undefined',
        protobuf_wire_type = 'undefined',
        json_name = 'undefined',
        erlang_name = 'undefined',
        erlang_type = 'undefined',
        erlang_default = 'undefined'
    }.


default_piqi_list() ->
    #piqi_list{
        name = default_name(),
        type = default_type(),
        piq_format = 'undefined',
        protobuf_name = 'undefined',
        protobuf_custom = [],
        protobuf_packed = false,
        json_name = 'undefined',
        erlang_name = 'undefined'
    }.


default_piqi() ->
    #piqi{
        module = 'undefined',
        typedef = [],
        import = [],
        func = [],
        custom_field = [],
        protobuf_custom = [],
        protobuf_package = 'undefined',
        file = 'undefined',
        erlang_module = 'undefined',
        erlang_type_prefix = 'undefined',
        erlang_string_type = 'undefined'
    }.


default_import() ->
    #import{
        module = default_word(),
        name = 'undefined'
    }.


default_piqi_any() ->
    #piqi_any{
        type = 'undefined',
        protobuf = 'undefined',
        json = 'undefined',
        xml = 'undefined'
    }.


default_func() ->
    #func{
        name = default_name(),
        input = 'undefined',
        output = 'undefined',
        error = 'undefined',
        erlang_name = 'undefined'
    }.


default_piqi_bundle() ->
    #piqi_bundle{
        piqi = []
    }.


default_erlang_string_type() -> binary.


piqi() ->
[<<226,202,230,52,4,112,105,113,105,160,148,209,72,129,248,174,104,226,231,
   249,238,1,9,112,105,113,105,46,112,105,113,105,154,197,165,238,9,0,218,244,
   134,182,12,72,170,136,200,184,14,66,218,164,238,191,4,10,112,105,113,45,
   102,111,114,109,97,116,170,183,218,222,5,19,232,146,150,113,148,135,232,
   239,1,218,164,238,191,4,4,119,111,114,100,170,183,218,222,5,19,232,146,150,
   113,218,178,206,207,1,218,164,238,191,4,4,116,101,120,116,218,244,134,182,
   12,66,130,153,170,100,61,218,164,238,191,4,14,112,114,111,116,111,98,117,
   102,45,105,110,116,51,50,226,195,252,217,4,5,105,110,116,51,50,128,228,138,
   244,5,249,179,220,210,1,176,171,195,244,5,239,153,192,2,210,171,158,194,6,
   5,105,110,116,51,50,218,244,134,182,12,66,130,153,170,100,61,218,164,238,
   191,4,14,112,114,111,116,111,98,117,102,45,105,110,116,54,52,226,195,252,
   217,4,5,105,110,116,54,52,128,228,138,244,5,249,179,220,210,1,176,171,195,
   244,5,239,153,192,2,210,171,158,194,6,5,105,110,116,54,52,218,244,134,182,
   12,149,2,138,176,205,197,1,142,2,218,164,238,191,4,18,112,114,111,116,111,
   98,117,102,45,119,105,114,101,45,116,121,112,101,170,183,218,222,5,21,232,
   146,150,113,208,225,169,186,2,218,164,238,191,4,6,118,97,114,105,110,116,
   170,183,218,222,5,27,232,146,150,113,154,229,206,94,218,164,238,191,4,13,
   122,105,103,122,97,103,45,118,97,114,105,110,116,170,183,218,222,5,22,232,
   146,150,113,166,172,211,130,1,218,164,238,191,4,7,102,105,120,101,100,51,
   50,170,183,218,222,5,22,232,146,150,113,228,182,211,130,1,218,164,238,191,
   4,7,102,105,120,101,100,54,52,170,183,218,222,5,28,232,146,150,113,242,231,
   184,165,3,218,164,238,191,4,13,115,105,103,110,101,100,45,118,97,114,105,
   110,116,170,183,218,222,5,29,232,146,150,113,196,161,239,209,3,218,164,238,
   191,4,14,115,105,103,110,101,100,45,102,105,120,101,100,51,50,170,183,218,
   222,5,29,232,146,150,113,130,172,239,209,3,218,164,238,191,4,14,115,105,
   103,110,101,100,45,102,105,120,101,100,54,52,170,183,218,222,5,20,232,146,
   150,113,154,213,227,207,2,218,164,238,191,4,5,98,108,111,99,107,218,244,
   134,182,12,24,130,153,170,100,19,218,164,238,191,4,4,98,111,111,108,176,
   171,195,244,5,170,136,238,8,218,244,134,182,12,27,130,153,170,100,22,218,
   164,238,191,4,6,115,116,114,105,110,103,176,171,195,244,5,209,209,192,137,
   1,218,244,134,182,12,26,130,153,170,100,21,218,164,238,191,4,6,98,105,110,
   97,114,121,176,171,195,244,5,129,248,174,104,218,244,134,182,12,60,130,153,
   170,100,55,234,163,191,120,13,112,105,113,105,95,112,105,113,105,95,97,110,
   121,226,170,239,250,2,8,112,105,113,105,95,97,110,121,218,164,238,191,4,8,
   112,105,113,105,45,97,110,121,176,171,195,244,5,236,245,167,2,218,244,134,
   182,12,57,130,153,170,100,52,226,170,239,250,2,7,105,110,116,101,103,101,
   114,218,164,238,191,4,3,105,110,116,226,195,252,217,4,6,115,105,110,116,51,
   50,128,228,138,244,5,205,178,167,47,176,171,195,244,5,239,153,192,2,218,
   244,134,182,12,67,130,153,170,100,62,226,170,239,250,2,15,110,111,110,95,
   110,101,103,95,105,110,116,101,103,101,114,218,164,238,191,4,4,117,105,110,
   116,226,195,252,217,4,6,117,105,110,116,51,50,128,228,138,244,5,232,240,
   148,157,1,176,171,195,244,5,239,153,192,2,218,244,134,182,12,59,130,153,
   170,100,54,226,170,239,250,2,7,105,110,116,101,103,101,114,218,164,238,191,
   4,5,105,110,116,51,50,226,195,252,217,4,6,115,105,110,116,51,50,128,228,
   138,244,5,205,178,167,47,176,171,195,244,5,239,153,192,2,218,244,134,182,
   12,69,130,153,170,100,64,226,170,239,250,2,15,110,111,110,95,110,101,103,
   95,105,110,116,101,103,101,114,218,164,238,191,4,6,117,105,110,116,51,50,
   226,195,252,217,4,6,117,105,110,116,51,50,128,228,138,244,5,232,240,148,
   157,1,176,171,195,244,5,239,153,192,2,218,244,134,182,12,59,130,153,170,
   100,54,226,170,239,250,2,7,105,110,116,101,103,101,114,218,164,238,191,4,5,
   105,110,116,54,52,226,195,252,217,4,6,115,105,110,116,54,52,128,228,138,
   244,5,205,178,167,47,176,171,195,244,5,239,153,192,2,218,244,134,182,12,69,
   130,153,170,100,64,226,170,239,250,2,15,110,111,110,95,110,101,103,95,105,
   110,116,101,103,101,114,218,164,238,191,4,6,117,105,110,116,54,52,226,195,
   252,217,4,6,117,105,110,116,54,52,128,228,138,244,5,232,240,148,157,1,176,
   171,195,244,5,239,153,192,2,218,244,134,182,12,48,130,153,170,100,43,218,
   164,238,191,4,7,102,108,111,97,116,54,52,226,195,252,217,4,6,100,111,117,
   98,108,101,128,228,138,244,5,178,219,169,65,176,171,195,244,5,156,139,219,
   20,218,244,134,182,12,47,130,153,170,100,42,218,164,238,191,4,7,102,108,
   111,97,116,51,50,226,195,252,217,4,5,102,108,111,97,116,128,228,138,244,5,
   147,214,169,65,176,171,195,244,5,156,139,219,20,218,244,134,182,12,66,130,
   153,170,100,61,218,164,238,191,4,11,105,110,116,51,50,45,102,105,120,101,
   100,226,195,252,217,4,8,115,102,105,120,101,100,51,50,128,228,138,244,5,
   226,208,247,232,1,176,171,195,244,5,239,153,192,2,210,171,158,194,6,5,105,
   110,116,51,50,218,244,134,182,12,66,130,153,170,100,61,218,164,238,191,4,
   12,117,105,110,116,51,50,45,102,105,120,101,100,226,195,252,217,4,7,102,
   105,120,101,100,51,50,128,228,138,244,5,147,214,169,65,176,171,195,244,5,
   239,153,192,2,210,171,158,194,6,6,117,105,110,116,51,50,218,244,134,182,12,
   66,130,153,170,100,61,218,164,238,191,4,11,105,110,116,54,52,45,102,105,
   120,101,100,226,195,252,217,4,8,115,102,105,120,101,100,54,52,128,228,138,
   244,5,129,214,247,232,1,176,171,195,244,5,239,153,192,2,210,171,158,194,6,
   5,105,110,116,54,52,218,244,134,182,12,66,130,153,170,100,61,218,164,238,
   191,4,12,117,105,110,116,54,52,45,102,105,120,101,100,226,195,252,217,4,7,
   102,105,120,101,100,54,52,128,228,138,244,5,178,219,169,65,176,171,195,244,
   5,239,153,192,2,210,171,158,194,6,6,117,105,110,116,54,52,218,244,134,182,
   12,38,130,153,170,100,33,218,164,238,191,4,5,102,108,111,97,116,176,171,
   195,244,5,156,139,219,20,210,171,158,194,6,7,102,108,111,97,116,54,52,218,
   244,134,182,12,39,130,153,170,100,34,218,164,238,191,4,4,119,111,114,100,
   210,171,158,194,6,6,115,116,114,105,110,103,226,156,170,236,8,6,208,156,
   160,191,7,1,218,244,134,182,12,25,130,153,170,100,20,218,164,238,191,4,4,
   110,97,109,101,210,171,158,194,6,4,119,111,114,100,218,244,134,182,12,166,
   1,170,136,200,184,14,159,1,234,188,204,215,2,12,112,105,113,105,95,116,121,
   112,101,100,101,102,218,164,238,191,4,7,116,121,112,101,100,101,102,170,
   183,218,222,5,21,232,146,150,113,162,218,227,222,3,210,171,158,194,6,6,114,
   101,99,111,114,100,170,183,218,222,5,22,232,146,150,113,138,130,146,206,3,
   210,171,158,194,6,7,118,97,114,105,97,110,116,170,183,218,222,5,18,232,146,
   150,113,130,172,179,49,210,171,158,194,6,4,101,110,117,109,170,183,218,222,
   5,19,232,146,150,113,160,198,138,25,210,171,158,194,6,5,97,108,105,97,115,
   170,183,218,222,5,18,232,146,150,113,188,241,152,123,210,171,158,194,6,4,
   108,105,115,116,218,244,134,182,12,187,1,138,176,205,197,1,180,1,218,164,
   238,191,4,9,112,105,113,105,45,116,121,112,101,170,183,218,222,5,17,232,
   146,150,113,222,179,128,5,218,164,238,191,4,3,105,110,116,170,183,218,222,
   5,19,232,146,150,113,184,150,182,41,218,164,238,191,4,5,102,108,111,97,116,
   170,183,218,222,5,18,232,146,150,113,212,144,220,17,218,164,238,191,4,4,98,
   111,111,108,170,183,218,222,5,21,232,146,150,113,162,163,129,147,2,218,164,
   238,191,4,6,115,116,114,105,110,103,170,183,218,222,5,21,232,146,150,113,
   130,240,221,208,1,218,164,238,191,4,6,98,105,110,97,114,121,170,183,218,
   222,5,17,232,146,150,113,216,235,207,4,218,164,238,191,4,3,97,110,121,162,
   249,213,245,10,10,112,105,113,105,95,116,121,112,101,95,218,244,134,182,12,
   25,130,153,170,100,20,218,164,238,191,4,4,116,121,112,101,210,171,158,194,
   6,4,110,97,109,101,218,244,134,182,12,242,2,138,233,142,251,14,235,2,210,
   203,242,36,29,232,146,150,113,150,201,251,143,1,152,182,154,152,4,223,162,
   138,147,1,210,171,158,194,6,4,110,97,109,101,210,203,242,36,29,232,146,150,
   113,244,210,156,9,152,182,154,152,4,250,248,214,130,1,210,171,158,194,6,5,
   102,105,101,108,100,210,203,242,36,49,232,146,150,113,210,139,155,188,1,
   152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,14,112,105,113,45,
   112,111,115,105,116,105,111,110,97,108,210,171,158,194,6,4,98,111,111,108,
   210,203,242,36,49,232,146,150,113,154,143,243,85,152,182,154,152,4,160,223,
   186,243,1,218,164,238,191,4,13,112,114,111,116,111,98,117,102,45,110,97,
   109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,51,232,
   146,150,113,230,246,146,107,152,182,154,152,4,250,248,214,130,1,218,164,
   238,191,4,15,112,114,111,116,111,98,117,102,45,99,117,115,116,111,109,210,
   171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,46,232,146,150,113,
   160,231,179,235,3,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,9,
   106,115,111,110,45,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,
   103,210,203,242,36,47,232,146,150,113,250,232,143,30,152,182,154,152,4,160,
   223,186,243,1,218,164,238,191,4,11,101,114,108,97,110,103,45,110,97,109,
   101,210,171,158,194,6,6,115,116,114,105,110,103,234,163,191,120,11,112,105,
   113,105,95,114,101,99,111,114,100,218,164,238,191,4,6,114,101,99,111,114,
   100,218,244,134,182,12,175,6,138,233,142,251,14,168,6,210,203,242,36,29,
   232,146,150,113,150,201,251,143,1,152,182,154,152,4,160,223,186,243,1,210,
   171,158,194,6,4,110,97,109,101,210,203,242,36,29,232,146,150,113,244,202,
   199,208,1,152,182,154,152,4,160,223,186,243,1,210,171,158,194,6,4,116,121,
   112,101,210,203,242,36,83,232,146,150,113,198,205,134,134,1,152,182,154,
   152,4,160,223,186,243,1,218,164,238,191,4,4,109,111,100,101,210,171,158,
   194,6,10,102,105,101,108,100,45,109,111,100,101,138,140,251,240,13,32,218,
   148,211,24,6,8,223,162,138,147,1,210,171,158,194,6,15,112,105,113,105,47,
   102,105,101,108,100,45,109,111,100,101,210,203,242,36,46,232,146,150,113,
   130,227,158,188,3,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,7,
   100,101,102,97,117,108,116,210,171,158,194,6,8,112,105,113,105,45,97,110,
   121,210,203,242,36,34,232,146,150,113,230,253,151,66,152,182,154,152,4,160,
   223,186,243,1,218,164,238,191,4,10,100,101,112,114,101,99,97,116,101,100,
   210,203,242,36,35,232,146,150,113,152,199,138,155,2,152,182,154,152,4,160,
   223,186,243,1,210,171,158,194,6,10,112,105,113,45,102,111,114,109,97,116,
   210,203,242,36,49,232,146,150,113,210,139,155,188,1,152,182,154,152,4,160,
   223,186,243,1,218,164,238,191,4,14,112,105,113,45,112,111,115,105,116,105,
   111,110,97,108,210,171,158,194,6,4,98,111,111,108,210,203,242,36,44,232,
   146,150,113,182,226,197,158,3,152,182,154,152,4,160,223,186,243,1,218,164,
   238,191,4,9,112,105,113,45,97,108,105,97,115,210,171,158,194,6,4,110,97,
   109,101,210,203,242,36,49,232,146,150,113,154,143,243,85,152,182,154,152,4,
   160,223,186,243,1,218,164,238,191,4,13,112,114,111,116,111,98,117,102,45,
   110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,
   39,232,146,150,113,218,196,165,28,152,182,154,152,4,160,223,186,243,1,218,
   164,238,191,4,4,99,111,100,101,210,171,158,194,6,5,105,110,116,51,50,210,
   203,242,36,40,232,146,150,113,244,181,193,171,1,152,182,154,152,4,160,223,
   186,243,1,218,164,238,191,4,15,112,114,111,116,111,98,117,102,45,112,97,99,
   107,101,100,210,203,242,36,46,232,146,150,113,160,231,179,235,3,152,182,
   154,152,4,160,223,186,243,1,218,164,238,191,4,9,106,115,111,110,45,110,97,
   109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,52,232,
   146,150,113,206,211,186,192,1,152,182,154,152,4,160,223,186,243,1,218,164,
   238,191,4,17,106,115,111,110,45,111,109,105,116,45,109,105,115,115,105,110,
   103,210,171,158,194,6,4,98,111,111,108,210,203,242,36,48,232,146,150,113,
   172,148,156,205,1,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,13,
   103,101,116,111,112,116,45,108,101,116,116,101,114,210,171,158,194,6,4,119,
   111,114,100,210,203,242,36,47,232,146,150,113,144,177,235,165,3,152,182,
   154,152,4,160,223,186,243,1,218,164,238,191,4,10,103,101,116,111,112,116,
   45,100,111,99,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,
   47,232,146,150,113,250,232,143,30,152,182,154,152,4,160,223,186,243,1,218,
   164,238,191,4,11,101,114,108,97,110,103,45,110,97,109,101,210,171,158,194,
   6,6,115,116,114,105,110,103,218,164,238,191,4,5,102,105,101,108,100,218,
   244,134,182,12,109,138,176,205,197,1,103,218,164,238,191,4,10,102,105,101,
   108,100,45,109,111,100,101,170,183,218,222,5,23,232,146,150,113,190,197,
   148,166,2,218,164,238,191,4,8,114,101,113,117,105,114,101,100,170,183,218,
   222,5,23,232,146,150,113,192,190,245,230,3,218,164,238,191,4,8,111,112,116,
   105,111,110,97,108,170,183,218,222,5,23,232,146,150,113,244,241,173,133,2,
   218,164,238,191,4,8,114,101,112,101,97,116,101,100,218,244,134,182,12,175,
   2,138,233,142,251,14,168,2,210,203,242,36,29,232,146,150,113,150,201,251,
   143,1,152,182,154,152,4,223,162,138,147,1,210,171,158,194,6,4,110,97,109,
   101,210,203,242,36,31,232,146,150,113,234,205,214,183,1,152,182,154,152,4,
   250,248,214,130,1,210,171,158,194,6,6,111,112,116,105,111,110,210,203,242,
   36,49,232,146,150,113,154,143,243,85,152,182,154,152,4,160,223,186,243,1,
   218,164,238,191,4,13,112,114,111,116,111,98,117,102,45,110,97,109,101,210,
   171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,51,232,146,150,113,
   230,246,146,107,152,182,154,152,4,250,248,214,130,1,218,164,238,191,4,15,
   112,114,111,116,111,98,117,102,45,99,117,115,116,111,109,210,171,158,194,6,
   6,115,116,114,105,110,103,210,203,242,36,46,232,146,150,113,160,231,179,
   235,3,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,9,106,115,111,
   110,45,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,
   242,36,47,232,146,150,113,250,232,143,30,152,182,154,152,4,160,223,186,243,
   1,218,164,238,191,4,11,101,114,108,97,110,103,45,110,97,109,101,210,171,
   158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,7,118,97,114,105,97,
   110,116,218,244,134,182,12,137,4,138,233,142,251,14,130,4,210,203,242,36,
   29,232,146,150,113,150,201,251,143,1,152,182,154,152,4,160,223,186,243,1,
   210,171,158,194,6,4,110,97,109,101,210,203,242,36,29,232,146,150,113,244,
   202,199,208,1,152,182,154,152,4,160,223,186,243,1,210,171,158,194,6,4,116,
   121,112,101,210,203,242,36,34,232,146,150,113,230,253,151,66,152,182,154,
   152,4,160,223,186,243,1,218,164,238,191,4,10,100,101,112,114,101,99,97,116,
   101,100,210,203,242,36,35,232,146,150,113,152,199,138,155,2,152,182,154,
   152,4,160,223,186,243,1,210,171,158,194,6,10,112,105,113,45,102,111,114,
   109,97,116,210,203,242,36,44,232,146,150,113,182,226,197,158,3,152,182,154,
   152,4,160,223,186,243,1,218,164,238,191,4,9,112,105,113,45,97,108,105,97,
   115,210,171,158,194,6,4,110,97,109,101,210,203,242,36,49,232,146,150,113,
   154,143,243,85,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,13,
   112,114,111,116,111,98,117,102,45,110,97,109,101,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,39,232,146,150,113,218,196,165,28,152,
   182,154,152,4,160,223,186,243,1,218,164,238,191,4,4,99,111,100,101,210,171,
   158,194,6,5,105,110,116,51,50,210,203,242,36,46,232,146,150,113,160,231,
   179,235,3,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,9,106,115,
   111,110,45,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,
   203,242,36,48,232,146,150,113,172,148,156,205,1,152,182,154,152,4,160,223,
   186,243,1,218,164,238,191,4,13,103,101,116,111,112,116,45,108,101,116,116,
   101,114,210,171,158,194,6,4,119,111,114,100,210,203,242,36,47,232,146,150,
   113,144,177,235,165,3,152,182,154,152,4,160,223,186,243,1,218,164,238,191,
   4,10,103,101,116,111,112,116,45,100,111,99,210,171,158,194,6,6,115,116,114,
   105,110,103,210,203,242,36,47,232,146,150,113,250,232,143,30,152,182,154,
   152,4,160,223,186,243,1,218,164,238,191,4,11,101,114,108,97,110,103,45,110,
   97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,6,
   111,112,116,105,111,110,218,244,134,182,12,244,2,138,233,142,251,14,237,2,
   210,203,242,36,29,232,146,150,113,150,201,251,143,1,152,182,154,152,4,223,
   162,138,147,1,210,171,158,194,6,4,110,97,109,101,210,203,242,36,31,232,146,
   150,113,234,205,214,183,1,152,182,154,152,4,250,248,214,130,1,210,171,158,
   194,6,6,111,112,116,105,111,110,210,203,242,36,49,232,146,150,113,154,143,
   243,85,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,13,112,114,
   111,116,111,98,117,102,45,110,97,109,101,210,171,158,194,6,6,115,116,114,
   105,110,103,210,203,242,36,51,232,146,150,113,230,246,146,107,152,182,154,
   152,4,250,248,214,130,1,218,164,238,191,4,15,112,114,111,116,111,98,117,
   102,45,99,117,115,116,111,109,210,171,158,194,6,6,115,116,114,105,110,103,
   210,203,242,36,52,232,146,150,113,168,190,181,221,2,152,182,154,152,4,160,
   223,186,243,1,218,164,238,191,4,15,112,114,111,116,111,98,117,102,45,112,
   114,101,102,105,120,210,171,158,194,6,6,115,116,114,105,110,103,210,203,
   242,36,46,232,146,150,113,160,231,179,235,3,152,182,154,152,4,160,223,186,
   243,1,218,164,238,191,4,9,106,115,111,110,45,110,97,109,101,210,171,158,
   194,6,6,115,116,114,105,110,103,210,203,242,36,47,232,146,150,113,250,232,
   143,30,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,11,101,114,
   108,97,110,103,45,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,
   103,234,188,204,215,2,9,112,105,113,105,95,101,110,117,109,218,164,238,191,
   4,4,101,110,117,109,218,244,134,182,12,149,4,138,233,142,251,14,142,4,210,
   203,242,36,29,232,146,150,113,150,201,251,143,1,152,182,154,152,4,223,162,
   138,147,1,210,171,158,194,6,4,110,97,109,101,210,203,242,36,29,232,146,150,
   113,244,202,199,208,1,152,182,154,152,4,160,223,186,243,1,210,171,158,194,
   6,4,116,121,112,101,210,203,242,36,34,232,146,150,113,236,234,144,189,1,
   152,182,154,152,4,160,223,186,243,1,210,171,158,194,6,9,112,105,113,105,45,
   116,121,112,101,210,203,242,36,35,232,146,150,113,152,199,138,155,2,152,
   182,154,152,4,160,223,186,243,1,210,171,158,194,6,10,112,105,113,45,102,
   111,114,109,97,116,210,203,242,36,49,232,146,150,113,154,143,243,85,152,
   182,154,152,4,160,223,186,243,1,218,164,238,191,4,13,112,114,111,116,111,
   98,117,102,45,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,
   210,203,242,36,50,232,146,150,113,248,144,191,150,1,152,182,154,152,4,160,
   223,186,243,1,218,164,238,191,4,13,112,114,111,116,111,98,117,102,45,116,
   121,112,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,43,
   232,146,150,113,128,217,130,189,1,152,182,154,152,4,160,223,186,243,1,210,
   171,158,194,6,18,112,114,111,116,111,98,117,102,45,119,105,114,101,45,116,
   121,112,101,210,203,242,36,46,232,146,150,113,160,231,179,235,3,152,182,
   154,152,4,160,223,186,243,1,218,164,238,191,4,9,106,115,111,110,45,110,97,
   109,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,47,232,
   146,150,113,250,232,143,30,152,182,154,152,4,160,223,186,243,1,218,164,238,
   191,4,11,101,114,108,97,110,103,45,110,97,109,101,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,47,232,146,150,113,216,234,219,94,152,
   182,154,152,4,160,223,186,243,1,218,164,238,191,4,11,101,114,108,97,110,
   103,45,116,121,112,101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,
   242,36,51,232,146,150,113,158,208,170,254,2,152,182,154,152,4,160,223,186,
   243,1,218,164,238,191,4,14,101,114,108,97,110,103,45,100,101,102,97,117,
   108,116,210,171,158,194,6,6,115,116,114,105,110,103,218,164,238,191,4,5,97,
   108,105,97,115,218,244,134,182,12,141,3,138,233,142,251,14,134,3,210,203,
   242,36,29,232,146,150,113,150,201,251,143,1,152,182,154,152,4,223,162,138,
   147,1,210,171,158,194,6,4,110,97,109,101,210,203,242,36,29,232,146,150,113,
   244,202,199,208,1,152,182,154,152,4,223,162,138,147,1,210,171,158,194,6,4,
   116,121,112,101,210,203,242,36,35,232,146,150,113,152,199,138,155,2,152,
   182,154,152,4,160,223,186,243,1,210,171,158,194,6,10,112,105,113,45,102,
   111,114,109,97,116,210,203,242,36,49,232,146,150,113,154,143,243,85,152,
   182,154,152,4,160,223,186,243,1,218,164,238,191,4,13,112,114,111,116,111,
   98,117,102,45,110,97,109,101,210,171,158,194,6,6,115,116,114,105,110,103,
   210,203,242,36,51,232,146,150,113,230,246,146,107,152,182,154,152,4,250,
   248,214,130,1,218,164,238,191,4,15,112,114,111,116,111,98,117,102,45,99,
   117,115,116,111,109,210,171,158,194,6,6,115,116,114,105,110,103,210,203,
   242,36,40,232,146,150,113,244,181,193,171,1,152,182,154,152,4,160,223,186,
   243,1,218,164,238,191,4,15,112,114,111,116,111,98,117,102,45,112,97,99,107,
   101,100,210,203,242,36,46,232,146,150,113,160,231,179,235,3,152,182,154,
   152,4,160,223,186,243,1,218,164,238,191,4,9,106,115,111,110,45,110,97,109,
   101,210,171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,47,232,146,
   150,113,250,232,143,30,152,182,154,152,4,160,223,186,243,1,218,164,238,191,
   4,11,101,114,108,97,110,103,45,110,97,109,101,210,171,158,194,6,6,115,116,
   114,105,110,103,234,163,191,120,9,112,105,113,105,95,108,105,115,116,218,
   164,238,191,4,4,108,105,115,116,218,244,134,182,12,207,4,138,233,142,251,
   14,200,4,210,203,242,36,40,232,146,150,113,216,210,153,13,152,182,154,152,
   4,160,223,186,243,1,218,164,238,191,4,6,109,111,100,117,108,101,210,171,
   158,194,6,4,119,111,114,100,210,203,242,36,32,232,146,150,113,150,221,193,
   141,3,152,182,154,152,4,250,248,214,130,1,210,171,158,194,6,7,116,121,112,
   101,100,101,102,210,203,242,36,31,232,146,150,113,202,133,149,136,1,152,
   182,154,152,4,250,248,214,130,1,210,171,158,194,6,6,105,109,112,111,114,
   116,210,203,242,36,33,232,146,150,113,176,172,149,197,2,152,182,154,152,4,
   250,248,214,130,1,210,171,158,194,6,8,102,117,110,99,116,105,111,110,210,
   203,242,36,47,232,146,150,113,188,207,221,154,1,152,182,154,152,4,250,248,
   214,130,1,218,164,238,191,4,12,99,117,115,116,111,109,45,102,105,101,108,
   100,210,171,158,194,6,4,119,111,114,100,210,203,242,36,51,232,146,150,113,
   230,246,146,107,152,182,154,152,4,250,248,214,130,1,218,164,238,191,4,15,
   112,114,111,116,111,98,117,102,45,99,117,115,116,111,109,210,171,158,194,6,
   6,115,116,114,105,110,103,210,203,242,36,53,232,146,150,113,136,221,228,
   230,2,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,16,112,114,111,
   116,111,98,117,102,45,112,97,99,107,97,103,101,210,171,158,194,6,6,115,116,
   114,105,110,103,210,203,242,36,40,232,146,150,113,248,185,222,59,152,182,
   154,152,4,160,223,186,243,1,218,164,238,191,4,4,102,105,108,101,210,171,
   158,194,6,6,115,116,114,105,110,103,210,203,242,36,50,232,146,150,113,188,
   244,232,213,3,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,13,101,
   114,108,97,110,103,45,109,111,100,117,108,101,210,171,158,194,6,6,115,116,
   114,105,110,103,210,203,242,36,55,232,146,150,113,166,177,201,187,2,152,
   182,154,152,4,160,223,186,243,1,218,164,238,191,4,18,101,114,108,97,110,
   103,45,116,121,112,101,45,112,114,101,102,105,120,210,171,158,194,6,6,115,
   116,114,105,110,103,210,203,242,36,87,232,146,150,113,136,165,148,18,152,
   182,154,152,4,160,223,186,243,1,210,171,158,194,6,18,101,114,108,97,110,
   103,45,115,116,114,105,110,103,45,116,121,112,101,138,140,251,240,13,39,
   218,148,211,24,5,8,129,248,174,104,210,171,158,194,6,23,112,105,113,105,47,
   101,114,108,97,110,103,45,115,116,114,105,110,103,45,116,121,112,101,218,
   164,238,191,4,4,112,105,113,105,218,244,134,182,12,97,138,233,142,251,14,
   91,210,203,242,36,40,232,146,150,113,216,210,153,13,152,182,154,152,4,223,
   162,138,147,1,218,164,238,191,4,6,109,111,100,117,108,101,210,171,158,194,
   6,4,119,111,114,100,210,203,242,36,29,232,146,150,113,150,201,251,143,1,
   152,182,154,152,4,160,223,186,243,1,210,171,158,194,6,4,110,97,109,101,218,
   164,238,191,4,6,105,109,112,111,114,116,218,244,134,182,12,213,1,138,233,
   142,251,14,206,1,210,203,242,36,41,232,146,150,113,244,202,199,208,1,152,
   182,154,152,4,160,223,186,243,1,218,164,238,191,4,4,116,121,112,101,210,
   171,158,194,6,6,115,116,114,105,110,103,210,203,242,36,44,232,146,150,113,
   150,229,148,6,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,8,112,
   114,111,116,111,98,117,102,210,171,158,194,6,6,98,105,110,97,114,121,210,
   203,242,36,40,232,146,150,113,208,136,194,102,152,182,154,152,4,160,223,
   186,243,1,218,164,238,191,4,4,106,115,111,110,210,171,158,194,6,6,115,116,
   114,105,110,103,210,203,242,36,39,232,146,150,113,174,183,219,5,152,182,
   154,152,4,160,223,186,243,1,218,164,238,191,4,3,120,109,108,210,171,158,
   194,6,6,115,116,114,105,110,103,234,163,191,120,8,112,105,113,105,95,97,
   110,121,218,164,238,191,4,3,97,110,121,218,244,134,182,12,252,1,138,233,
   142,251,14,245,1,210,203,242,36,29,232,146,150,113,150,201,251,143,1,152,
   182,154,152,4,223,162,138,147,1,210,171,158,194,6,4,110,97,109,101,210,203,
   242,36,40,232,146,150,113,148,144,238,225,3,152,182,154,152,4,160,223,186,
   243,1,218,164,238,191,4,5,105,110,112,117,116,210,171,158,194,6,4,116,121,
   112,101,210,203,242,36,41,232,146,150,113,130,188,136,200,1,152,182,154,
   152,4,160,223,186,243,1,218,164,238,191,4,6,111,117,116,112,117,116,210,
   171,158,194,6,4,116,121,112,101,210,203,242,36,40,232,146,150,113,144,175,
   206,178,2,152,182,154,152,4,160,223,186,243,1,218,164,238,191,4,5,101,114,
   114,111,114,210,171,158,194,6,4,116,121,112,101,210,203,242,36,47,232,146,
   150,113,250,232,143,30,152,182,154,152,4,160,223,186,243,1,218,164,238,191,
   4,11,101,114,108,97,110,103,45,110,97,109,101,210,171,158,194,6,6,115,116,
   114,105,110,103,234,163,191,120,4,102,117,110,99,218,164,238,191,4,8,102,
   117,110,99,116,105,111,110,218,244,134,182,12,67,138,233,142,251,14,61,210,
   203,242,36,25,232,146,150,113,2,152,182,154,152,4,250,248,214,130,1,210,
   171,158,194,6,4,112,105,113,105,234,163,191,120,11,112,105,113,105,95,98,
   117,110,100,108,101,218,164,238,191,4,9,112,105,113,105,45,108,105,115,116,
   218,244,134,182,12,81,138,176,205,197,1,75,218,164,238,191,4,18,101,114,
   108,97,110,103,45,115,116,114,105,110,103,45,116,121,112,101,170,183,218,
   222,5,21,232,146,150,113,130,240,221,208,1,218,164,238,191,4,6,98,105,110,
   97,114,121,170,183,218,222,5,18,232,146,150,113,188,241,152,123,218,164,
   238,191,4,4,108,105,115,116>>].
