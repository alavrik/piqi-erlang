-module(piqi_rpc_piqi).
-compile([export_all, nowarn_export_all]).

-include("piqi_rpc_piqi.hrl").


-include_lib("piqi/include/piqirun.hrl").


-spec field_gen_binary(Code :: piqirun_code(), X :: binary()) -> iolist().

field_gen_binary(Code, X) ->
    piqirun:binary_to_block(Code, X).


-spec field_gen_string(Code :: piqirun_code(), X :: string() | binary()) -> iolist().

field_gen_string(Code, X) ->
    piqirun:string_to_block(Code, X).


-spec field_gen_request(Code :: piqirun_code(), X :: piqi_rpc_request()) -> iolist().

field_gen_request(Code, X) ->
    piqirun:gen_record(Code, [
        piqirun:gen_required_field(1, fun field_gen_string/2, X#piqi_rpc_request.name),
        piqirun:gen_optional_field(2, fun field_gen_binary/2, X#piqi_rpc_request.data)
    ]).


-spec field_gen_response(Code :: piqirun_code(), X :: piqi_rpc_response()) -> iolist().

field_gen_response(Code, X) ->
    piqirun:gen_variant(Code,
        case X of
            ok -> piqirun:gen_bool_field(1, true);
            {ok, Y} -> field_gen_binary(2, Y);
            {error, Y} -> field_gen_binary(3, Y);
            {rpc_error, Y} -> field_gen_rpc_error(4, Y)
        end
    ).


-spec field_gen_client_error(Code :: piqirun_code(), X :: piqi_rpc_client_error()) -> iolist().

field_gen_client_error(Code, X) ->
    piqirun:gen_variant(Code,
        case X of
            unknown_function -> piqirun:gen_bool_field(404, true);
            {invalid_input, Y} -> field_gen_string(400, Y);
            missing_input -> piqirun:gen_bool_field(411, true);
            {protocol_error, Y} -> field_gen_string(1, Y)
        end
    ).


-spec field_gen_server_error(Code :: piqirun_code(), X :: piqi_rpc_server_error()) -> iolist().

field_gen_server_error(Code, X) ->
    piqirun:gen_variant(Code,
        case X of
            {invalid_output, Y} -> field_gen_string(502, Y);
            {internal_error, Y} -> field_gen_string(500, Y);
            {service_unavailable, Y} -> field_gen_string(503, Y)
        end
    ).


-spec field_gen_rpc_error(Code :: piqirun_code(), X :: piqi_rpc_rpc_error()) -> iolist().

field_gen_rpc_error(Code, X) ->
    piqirun:gen_variant(Code,
        case X of
            unknown_function -> field_gen_client_error(1, X);
            {invalid_input, _} -> field_gen_client_error(1, X);
            missing_input -> field_gen_client_error(1, X);
            {protocol_error, _} -> field_gen_client_error(1, X);
            {invalid_output, _} -> field_gen_server_error(2, X);
            {internal_error, _} -> field_gen_server_error(2, X);
            {service_unavailable, _} -> field_gen_server_error(2, X)
        end
    ).


-spec gen_binary(X :: binary()) -> iolist().

gen_binary(X) ->
    field_gen_binary('undefined', X).


-spec gen_string(X :: string() | binary()) -> iolist().

gen_string(X) ->
    field_gen_string('undefined', X).


-spec gen_request(X :: piqi_rpc_request()) -> iolist().

gen_request(X) ->
    field_gen_request('undefined', X).


-spec gen_response(X :: piqi_rpc_response()) -> iolist().

gen_response(X) ->
    field_gen_response('undefined', X).


-spec gen_client_error(X :: piqi_rpc_client_error()) -> iolist().

gen_client_error(X) ->
    field_gen_client_error('undefined', X).


-spec gen_server_error(X :: piqi_rpc_server_error()) -> iolist().

gen_server_error(X) ->
    field_gen_server_error('undefined', X).


-spec gen_rpc_error(X :: piqi_rpc_rpc_error()) -> iolist().

gen_rpc_error(X) ->
    field_gen_rpc_error('undefined', X).


-spec parse_binary(X :: piqirun_buffer()) -> binary().

parse_binary(X) ->
    piqirun:binary_of_block(X).


-spec parse_string(X :: piqirun_buffer()) -> binary().

parse_string(X) ->
    piqirun:binary_string_of_block(X).


-spec parse_request(X :: piqirun_buffer()) -> piqi_rpc_request().

parse_request(X) ->
    R0 = piqirun:parse_record(X),
    {_Name, R1} = piqirun:parse_required_field(1, fun parse_string/1, R0),
    {_Data, R2} = piqirun:parse_optional_field(2, fun parse_binary/1, R1),
    _ = R2,
    #piqi_rpc_request{
        name = _Name,
        data = _Data
    }.


-spec parse_response(X :: piqirun_buffer()) -> piqi_rpc_response().

parse_response(X) ->
    {Code, Obj} = piqirun:parse_variant(X),
    case Code of
        1 when Obj =:= 1 -> ok;
        2 -> {ok, parse_binary(Obj)};
        3 -> {error, parse_binary(Obj)};
        4 -> {rpc_error, parse_rpc_error(Obj)};
        _ -> piqirun:error_option(Obj, Code)
    end.


-spec parse_client_error(X :: piqirun_buffer()) -> piqi_rpc_client_error().

parse_client_error(X) ->
    {Code, Obj} = piqirun:parse_variant(X),
    case Code of
        404 when Obj =:= 1 -> unknown_function;
        400 -> {invalid_input, parse_string(Obj)};
        411 when Obj =:= 1 -> missing_input;
        1 -> {protocol_error, parse_string(Obj)};
        _ -> piqirun:error_option(Obj, Code)
    end.


-spec parse_server_error(X :: piqirun_buffer()) -> piqi_rpc_server_error().

parse_server_error(X) ->
    {Code, Obj} = piqirun:parse_variant(X),
    case Code of
        502 -> {invalid_output, parse_string(Obj)};
        500 -> {internal_error, parse_string(Obj)};
        503 -> {service_unavailable, parse_string(Obj)};
        _ -> piqirun:error_option(Obj, Code)
    end.


-spec parse_rpc_error(X :: piqirun_buffer()) -> piqi_rpc_rpc_error().

parse_rpc_error(X) ->
    {Code, Obj} = piqirun:parse_variant(X),
    case Code of
        1 -> parse_client_error(Obj);
        2 -> parse_server_error(Obj);
        _ -> piqirun:error_option(Obj, Code)
    end.
