-module(piqi_tools_piqi).
-compile([export_all, nowarn_export_all]).

-include("piqi_tools_piqi.hrl").


-include_lib("piqi/include/piqirun.hrl").


-spec field_gen_binary(Code :: piqirun_code(), X :: binary()) -> iolist().

field_gen_binary(Code, X) ->
    piqirun:binary_to_block(Code, X).


-spec field_gen_bool(Code :: piqirun_code(), X :: boolean()) -> iolist().

field_gen_bool(Code, X) ->
    piqirun:boolean_to_varint(Code, X).


packed_field_gen_bool(X) ->
    piqirun:boolean_to_packed_varint(X).


-spec field_gen_string(Code :: piqirun_code(), X :: string() | binary()) -> iolist().

field_gen_string(Code, X) ->
    piqirun:string_to_block(Code, X).


-spec field_gen_format(Code :: piqirun_code(), X :: piqi_tools_format()) -> iolist().

field_gen_format(Code, X) ->
    piqirun:integer_to_signed_varint(Code,
        case X of
            piq -> 1;
            json -> 2;
            pb -> 3;
            xml -> 4
        end
    ).


packed_field_gen_format(X) ->
    piqirun:integer_to_packed_signed_varint(
        case X of
            piq -> 1;
            json -> 2;
            pb -> 3;
            xml -> 4
        end
    ).


-spec field_gen_add_piqi_input(Code :: piqirun_code(), X :: piqi_tools_add_piqi_input()) -> iolist().

field_gen_add_piqi_input(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_format/2, X#piqi_tools_add_piqi_input.format),
        piqirun:gen_repeated_field(2, fun field_gen_binary/2, X#piqi_tools_add_piqi_input.data)
    ]).


-spec field_gen_add_piqi_error(Code :: piqirun_code(), X :: piqi_tools_add_piqi_error()) -> iolist().

field_gen_add_piqi_error(Code, X) ->
    field_gen_string(Code, X).


-spec field_gen_convert_input(Code :: piqirun_code(), X :: piqi_tools_convert_input()) -> iolist().

field_gen_convert_input(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#piqi_tools_convert_input.type_name),
        piqirun:gen_required_field(2, fun field_gen_binary/2, X#piqi_tools_convert_input.data),
        piqirun:gen_required_field(3, fun field_gen_format/2, X#piqi_tools_convert_input.input_format),
        piqirun:gen_required_field(4, fun field_gen_format/2, X#piqi_tools_convert_input.output_format),
        piqirun:gen_optional_field(5, fun field_gen_bool/2, X#piqi_tools_convert_input.pretty_print),
        piqirun:gen_optional_field(6, fun field_gen_bool/2, X#piqi_tools_convert_input.json_omit_missing_fields),
        piqirun:gen_optional_field(7, fun field_gen_bool/2, X#piqi_tools_convert_input.use_strict_parsing),
        piqirun:gen_optional_field(8, fun field_gen_bool/2, X#piqi_tools_convert_input.piq_frameless_output),
        piqirun:gen_optional_field(9, fun field_gen_bool/2, X#piqi_tools_convert_input.piq_frameless_input),
        piqirun:gen_optional_field(10, fun field_gen_bool/2, X#piqi_tools_convert_input.piq_relaxed_parsing)
    ]).


-spec field_gen_convert_output(Code :: piqirun_code(), X :: piqi_tools_convert_output()) -> iolist().

field_gen_convert_output(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_binary/2, X#piqi_tools_convert_output.data)
    ]).


-spec field_gen_convert_error(Code :: piqirun_code(), X :: piqi_tools_convert_error()) -> iolist().

field_gen_convert_error(Code, X) ->
    field_gen_string(Code, X).


-spec gen_binary(X :: binary()) -> iolist().

gen_binary(X) ->
    field_gen_binary('undefined', X).


-spec gen_bool(X :: boolean()) -> iolist().

gen_bool(X) ->
    field_gen_bool('undefined', X).


-spec gen_string(X :: string() | binary()) -> iolist().

gen_string(X) ->
    field_gen_string('undefined', X).


-spec gen_format(X :: piqi_tools_format()) -> iolist().

gen_format(X) ->
    field_gen_format('undefined', X).


-spec gen_add_piqi_input(X :: piqi_tools_add_piqi_input()) -> iolist().

gen_add_piqi_input(X) ->
    field_gen_add_piqi_input('undefined', X).


-spec gen_add_piqi_error(X :: piqi_tools_add_piqi_error()) -> iolist().

gen_add_piqi_error(X) ->
    field_gen_add_piqi_error('undefined', X).


-spec gen_convert_input(X :: piqi_tools_convert_input()) -> iolist().

gen_convert_input(X) ->
    field_gen_convert_input('undefined', X).


-spec gen_convert_output(X :: piqi_tools_convert_output()) -> iolist().

gen_convert_output(X) ->
    field_gen_convert_output('undefined', X).


-spec gen_convert_error(X :: piqi_tools_convert_error()) -> iolist().

gen_convert_error(X) ->
    field_gen_convert_error('undefined', X).


-spec parse_binary(X :: piqirun_buffer()) -> binary().

parse_binary(X) ->
    piqirun:binary_of_block(X).


-spec parse_bool(X :: piqirun_buffer()) -> boolean().

parse_bool(X) ->
    piqirun:boolean_of_varint(X).


packed_parse_bool(X) ->
    {Res, Rest} = piqirun:boolean_of_packed_varint(X),
    {Res, Rest}.


-spec parse_string(X :: piqirun_buffer()) -> binary().

parse_string(X) ->
    piqirun:binary_string_of_block(X).


-spec parse_format(X :: piqirun_buffer()) -> piqi_tools_format().

parse_format(X) ->
    case piqirun:integer_of_signed_varint(X) of
        1 -> piq;
        2 -> json;
        3 -> pb;
        4 -> xml;
        Y -> piqirun:error_enum_const(Y)
    end.


packed_parse_format(X) ->
    {Code, Rest} = piqirun:integer_of_packed_signed_varint(X),
    {case Code of
        1 -> piq;
        2 -> json;
        3 -> pb;
        4 -> xml;
        Y -> piqirun:error_enum_const(Y)
    end, Rest}.


-spec parse_add_piqi_input(X :: piqirun_buffer()) -> piqi_tools_add_piqi_input().

parse_add_piqi_input(X) ->
    R0 = piqirun:parse_record(X),
    {_Format, R1} = piqirun:parse_required_field(1, fun parse_format/1, R0),
    {_Data, R2} = piqirun:parse_repeated_field(2, fun parse_binary/1, R1),
    _ = R2,
    #piqi_tools_add_piqi_input{
        format = _Format,
        data = _Data
    }.


-spec parse_add_piqi_error(X :: piqirun_buffer()) -> binary().

parse_add_piqi_error(X) ->
    parse_string(X).


-spec parse_convert_input(X :: piqirun_buffer()) -> piqi_tools_convert_input().

parse_convert_input(X) ->
    R0 = piqirun:parse_record(X),
    {_Type_name, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Data, R2} = piqirun:parse_required_field(2, fun parse_binary/1, R1),
    {_Input_format, R3} = piqirun:parse_required_field(3, fun parse_format/1, R2),
    {_Output_format, R4} = piqirun:parse_required_field(4, fun parse_format/1, R3),
    {_Pretty_print, R5} = piqirun:parse_optional_field(5, fun parse_bool/1, R4, <<8,1>>),
    {_Json_omit_missing_fields, R6} = piqirun:parse_optional_field(6, fun parse_bool/1, R5, <<8,1>>),
    {_Use_strict_parsing, R7} = piqirun:parse_optional_field(7, fun parse_bool/1, R6, <<8,0>>),
    {_Piq_frameless_output, R8} = piqirun:parse_optional_field(8, fun parse_bool/1, R7, <<8,0>>),
    {_Piq_frameless_input, R9} = piqirun:parse_optional_field(9, fun parse_bool/1, R8, <<8,0>>),
    {_Piq_relaxed_parsing, R10} = piqirun:parse_optional_field(10, fun parse_bool/1, R9, <<8,0>>),
    _ = R10,
    #piqi_tools_convert_input{
        type_name = _Type_name,
        data = _Data,
        input_format = _Input_format,
        output_format = _Output_format,
        pretty_print = _Pretty_print,
        json_omit_missing_fields = _Json_omit_missing_fields,
        use_strict_parsing = _Use_strict_parsing,
        piq_frameless_output = _Piq_frameless_output,
        piq_frameless_input = _Piq_frameless_input,
        piq_relaxed_parsing = _Piq_relaxed_parsing
    }.


-spec parse_convert_output(X :: piqirun_buffer()) -> piqi_tools_convert_output().

parse_convert_output(X) ->
    R0 = piqirun:parse_record(X),
    {_Data, R1} = piqirun:parse_required_field(1, fun parse_binary/1, R0),
    _ = R1,
    #piqi_tools_convert_output{
        data = _Data
    }.


-spec parse_convert_error(X :: piqirun_buffer()) -> binary().

parse_convert_error(X) ->
    parse_string(X).
