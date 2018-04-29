-module(normalize_piqi_test).
-compile([export_all, nowarn_export_all]).
-include("normalize_piqi.hrl").
-include("nonormalize_piqi.hrl").
get_pb() ->
	get_pb(normalize_piqi),
	get_pb(nonormalize_piqi).

get_pb(Mod) ->
	Pib = Mod:piqi(),
	file:write_file(atom_to_list(Mod) ++ "_from_erl.pb", Pib).

test_convert_normalize() ->
	{ok, Bin} = piqi_tools:convert(normalize_piqi, <<"normalize/test-dashCap-CAPDash">>, 'json', 'pb', json()),
	case normalize_piqi:parse_test_dash_cap_capdash(Bin) of 
		#normalize_test_dash_cap_capdash{
			field_one = <<"value1">>,
			field_two = <<"value2">>, 
			field_three = <<"value3">>, 
			field_four = <<"value4">>
		} -> ok;
		Record -> io:format("test_convert_normalize failed: ~p~n", [Record]), error
	end.

test_convert_nonormalize() ->
	{ok, Bin} = piqi_tools:convert(nonormalize_piqi, <<"nonormalize/test-dashCap-CAPDash">>, 'json', 'pb', json()),
	case nonormalize_piqi:parse_test_dashCap_CAPDash(Bin) of 
		#nonormalize_test_dashCap_CAPDash{
			fieldOne = <<"value1">>,
			fieldTWO = <<"value2">>, 
			field_Three = <<"value3">>, 
			field_four = <<"value4">>
		} -> ok;
		Record -> io:format("test_convert_nonormalize failed: ~p~n", [Record]), error
	end.


json() -> 
	<<"{\"fieldOne\":\"value1\",\"fieldTWO\":\"value2\",\"Field_Three\":\"value3\",\"field_four\":\"value4\"}">>.
%{\"fieldOne\":\"value1\",\"fieldTWO\":\"value2\",\"Field-Three\":\"value3\",\"field-four\":\"value4\"}
%{"fieldOne":"value1","fieldTWO":"value2","Field-Three":"value3","field-four":"value4"}

main([Fun|_Args]) ->	
	piqi:start(),
	Res = case Fun of
		"get_pb" -> get_pb();
		"test_convert_normalize" -> test_convert_normalize();
		"test_convert_nonormalize" -> test_convert_nonormalize();
		_ -> io:format("Invalid input")
	end,
	case Res of
		error -> halt(1);
		_ -> ok
	end;

main(_) ->
	io:format("Invalid input").
