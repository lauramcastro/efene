-module(efene_SUITE).
-compile(export_all).
-include_lib("stdlib/include/assert.hrl").

-define(input, "/home/gabriel/Escritorio/VVS/efene/test/resources/input_resources/").
-define(output, "/home/gabriel/Escritorio/VVS/efene/test/resources/output_resources/").


all() -> [to_lex_test, to_raw_lex_test, to_ast_test, to_mod_test, to_erlast_test, to_erl_test, erl2ast_test, erl2erl_test].

check([],[])	-> true;
check(L,[])		-> ?assert(true =:= false);
check([],L)		-> ?assert(true =:= false);
check([H|T],[H|T])	-> check(T,T);
check([H1|T],[H2|T])	-> if H1==H2 -> check(T,T);
						   true ->  ?assert(H1 =:= H2)
						   end.

removeN([_|[]],ListAux)	->   lists:reverse(ListAux);
removeN([H|T], ListAux)-> removeN(T, [H|ListAux]).

%% Tests

to_raw_lex_test(Config)->
	case efene:to_raw_lex(?input++"example_to_remove") of
		{ok, Data, _}-> case file:read_file(?output++"to_raw_lex") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~w", [Data])),
											OutputN = removeN(Output,[]),
											check(Input, OutputN);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.

to_lex_test(Config)->
	case efene:to_lex(?input++"example_to_remove") of
		{ok, Data, _}-> case file:read_file(?output++"example_to_remove") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~w", [Data])),
											OutputN = removeN(Output,[]),
											check(Input, OutputN);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.


to_ast_test(Config)	->
	case efene:to_ast("/home/gabriel/Escritorio/VVS/efene/test/resources/input_resources/ast") of
		{ok,Data}-> case file:read_file(?output++"ast") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											check(Input, Output);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.

to_mod_test(Config)	->
	case efene:to_mod(?input++"ast") of
		{ok, Data}-> case file:read_file(?output++"mod") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											check(Input, Output);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.


to_erlast_test(Config)	->
	case efene:to_erl_ast(?input++"ast") of
		{ok, Data}-> case file:read_file(?output++"erlast") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											check(Input, Output);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.
	
	
to_erl_test(Config)	->
	case efene:to_erl(?input++"ast") of
		Data -> case file:read_file(?output++"erl") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											OutputN = removeN(Output,[]),
											check(Data, OutputN);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.


erl2ast_test(Config) ->
	case efene:from_erl(?input++"erl2.erl") of
		{ok, Data}-> case file:read_file(?output++"erl2ast") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											check(Input, Output);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.
	
erl2erl_test(Config) ->
	case efene:from_erl(?input++"erl2.erl") of
		{ok, Data} -> case file:read_file(?output++"erl2erl") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = erl_prettypr:format(erl_syntax:form_list(Data)),
											check(Input, Output);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.


