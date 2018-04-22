% non-embedded version of the piqi runtime

-module(piqirun).
-compile(export_all).

-include("piqirun.hrl").
-include("piqirun_embedded.hrl").


% below functions are for backward compatibility with earlier versions of generated stubs
gen_bool_field(Code, X) ->
    piqirun_gen_bool_field(Code, X).

gen_record(Code, X) ->
    piqirun_gen_record(Code, X).

gen_variant(Code, X) ->
    piqirun_gen_variant(Code, X).

gen_list(Code, GenValue, L) ->
    piqirun_gen_list(Code, GenValue, L).

gen_packed_list(Code, GenValue, L) ->
    piqirun_gen_packed_list(Code, GenValue, L).

gen_required_field(Code, GenValue, X) ->
    piqirun_gen_required_field(Code, GenValue, X).

gen_optional_field(Code, GenValue, X) ->
    piqirun_gen_optional_field(Code, GenValue, X).

gen_repeated_field(Code, GenValue, L) ->
    piqirun_gen_repeated_field(Code, GenValue, L).

gen_packed_repeated_field(Code, GenValue, L) ->
    piqirun_gen_packed_repeated_field(Code, GenValue, L).

gen_flag(Code, X) ->
    piqirun_gen_flag(Code, X).

gen_parsed_field(X) ->
    piqirun_gen_parsed_field(X).


parse_block(X) ->
    piqirun_parse_block(X).

parse_record(X) ->
    piqirun_parse_record(X).

parse_record_buf(X) ->
    piqirun_parse_record_buf(X).

parse_variant(X) ->
    piqirun_parse_variant(X).

parse_list(ParseValue, X) ->
    piqirun_parse_list(ParseValue, X).

parse_packed_list(ParsePackedValue, ParseValue, X) ->
    piqirun_parse_packed_list(ParsePackedValue, ParseValue, X).

parse_required_field(Code, ParseValue, L) ->
    piqirun_parse_required_field(Code, ParseValue, L).

parse_optional_field(Code, ParseValue, L, Default) ->
    piqirun_parse_optional_field(Code, ParseValue, L, Default).

parse_optional_field(Code, ParseValue, L) ->
    piqirun_parse_optional_field(Code, ParseValue, L).

parse_repeated_field(Code, ParseValue, L) ->
    piqirun_parse_repeated_field(Code, ParseValue, L).

parse_packed_repeated_field(Code, ParsePackedValue, ParseValue, L) ->
    piqirun_parse_packed_repeated_field(Code, ParsePackedValue, ParseValue, L).

parse_flag(Code, L) ->
    piqirun_parse_flag(Code, L).


error_enum_const(X) ->
    piqirun_error_enum_const(X).

error_option(X, Code) ->
    piqirun_error_option(X, Code).


binary_of_block(X) ->
    piqirun_binary_of_block(X).
binary_string_of_block(X) ->
    piqirun_binary_string_of_block(X).
binary_to_block(Code, X) ->
    piqirun_binary_to_block(Code, X).
boolean_of_packed_varint(X) ->
    piqirun_boolean_of_packed_varint(X).
boolean_of_varint(X) ->
    piqirun_boolean_of_varint(X).
boolean_to_packed_varint(X) ->
    piqirun_boolean_to_packed_varint(X).
boolean_to_varint(Code, X) ->
    piqirun_boolean_to_varint(Code, X).
float_of_fixed32(X) ->
    piqirun_float_of_fixed32(X).
float_of_fixed64(X) ->
    piqirun_float_of_fixed64(X).
float_of_packed_fixed32(X) ->
    piqirun_float_of_packed_fixed32(X).
float_of_packed_fixed64(X) ->
    piqirun_float_of_packed_fixed64(X).
float_to_fixed32(Code, X) ->
    piqirun_float_to_fixed32(Code, X).
float_to_fixed64(Code, X) ->
    piqirun_float_to_fixed64(Code, X).
float_to_packed_fixed32(X) ->
    piqirun_float_to_packed_fixed32(X).
float_to_packed_fixed64(X) ->
    piqirun_float_to_packed_fixed64(X).
integer_of_packed_signed_fixed32(X) ->
    piqirun_integer_of_packed_signed_fixed32(X).
integer_of_packed_signed_fixed64(X) ->
    piqirun_integer_of_packed_signed_fixed64(X).
integer_of_packed_signed_varint(X) ->
    piqirun_integer_of_packed_signed_varint(X).
integer_of_packed_zigzag_varint(X) ->
    piqirun_integer_of_packed_zigzag_varint(X).
integer_of_signed_fixed32(X) ->
    piqirun_integer_of_signed_fixed32(X).
integer_of_signed_fixed64(X) ->
    piqirun_integer_of_signed_fixed64(X).
integer_of_signed_varint(X) ->
    piqirun_integer_of_signed_varint(X).
integer_of_zigzag_varint(X) ->
    piqirun_integer_of_zigzag_varint(X).
integer_to_packed_signed_fixed32(X) ->
    piqirun_integer_to_packed_signed_fixed32(X).
integer_to_packed_signed_fixed64(X) ->
    piqirun_integer_to_packed_signed_fixed64(X).
integer_to_packed_signed_varint(X) ->
    piqirun_integer_to_packed_signed_varint(X).
integer_to_packed_zigzag_varint(X) ->
    piqirun_integer_to_packed_zigzag_varint(X).
integer_to_signed_fixed32(Code, X) ->
    piqirun_integer_to_signed_fixed32(Code, X).
integer_to_signed_fixed64(Code, X) ->
    piqirun_integer_to_signed_fixed64(Code, X).
integer_to_signed_varint(Code, X) ->
    piqirun_integer_to_signed_varint(Code, X).
integer_to_zigzag_varint(Code, X) ->
    piqirun_integer_to_zigzag_varint(Code, X).
list_string_of_block(X) ->
    piqirun_list_string_of_block(X).
non_neg_integer_of_fixed32(X) ->
    piqirun_non_neg_integer_of_fixed32(X).
non_neg_integer_of_fixed64(X) ->
    piqirun_non_neg_integer_of_fixed64(X).
non_neg_integer_of_packed_fixed32(X) ->
    piqirun_non_neg_integer_of_packed_fixed32(X).
non_neg_integer_of_packed_fixed64(X) ->
    piqirun_non_neg_integer_of_packed_fixed64(X).
non_neg_integer_of_packed_varint(X) ->
    piqirun_non_neg_integer_of_packed_varint(X).
non_neg_integer_of_varint(X) ->
    piqirun_non_neg_integer_of_varint(X).
non_neg_integer_to_fixed32(Code, X) ->
    piqirun_non_neg_integer_to_fixed32(Code, X).
non_neg_integer_to_fixed64(Code, X) ->
    piqirun_non_neg_integer_to_fixed64(Code, X).
non_neg_integer_to_packed_fixed32(X) ->
    piqirun_non_neg_integer_to_packed_fixed32(X).
non_neg_integer_to_packed_fixed64(X) ->
    piqirun_non_neg_integer_to_packed_fixed64(X).
non_neg_integer_to_packed_varint(X) ->
    piqirun_non_neg_integer_to_packed_varint(X).
non_neg_integer_to_varint(Code, X) ->
    piqirun_non_neg_integer_to_varint(Code, X).
string_of_block(X) ->
    piqirun_string_of_block(X).
string_to_block(Code, X) ->
    piqirun_string_to_block(Code, X).
