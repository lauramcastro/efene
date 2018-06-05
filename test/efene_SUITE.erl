-module(efene_SUITE).
-compile(export_all).
-include_lib("stdlib/include/assert.hrl").

-define(input, "/resources/input_resources/").
-define(output, "/resources/output_resources/").


all() -> [to_lex_test, to_raw_lex_test, to_ast_test, to_mod_test, to_erl_test, erl2ast_test, erl2erl_test].

check([],[])	-> true;
check(L,[])		-> ?assert(true =:= false);
check([],L)		-> ?assert(true =:= false);
check([H|T],[H|T])	-> check(T,T);
check([H1|T],[H2|T])	-> false.

removeN([_|[]],ListAux)	->   lists:reverse(ListAux);
removeN([H|T], ListAux)-> removeN(T, [H|ListAux]).




removePaths([_|[]],ListAux, IsPath)		-> lists:reverse(ListAux);

removePaths([H|T],ListAux, true) 	->	if 
										  H == 10	->	removePaths(T,ListAux,false);
										  true		->	removePaths(T,ListAux,true)
										end;
										
										 
removePaths([H|T],ListAux, false) 	->	if 
										  H == 47	->	removePaths(T,ListAux,true);
										  true		->	removePaths(T,[H|ListAux],false)
										end.
										


removeSpecialPaths([_|[]],ListAux, _, _)					-> lists:reverse(ListAux);

removeSpecialPaths([H|T],ListAux, false, false) 			->	removeSpecialPaths(T,[H|ListAux],false,false);	

removeSpecialPaths([H|T],ListAux, false, true) 				->	if 
																  H == 41	->	removeSpecialPaths(T,ListAux,false,false);
																  true		->	removeSpecialPaths(T,ListAux,false,true)
																end;	
										 
removeSpecialPaths([H|T],ListAux, FirstTime, StartRemove) 	->	if 
																  H == 40 	->	removeSpecialPaths(T,ListAux,false,true);
																  true		->	removeSpecialPaths(T,[H|ListAux],true,false)
																end.
%% Tests

to_raw_lex_test(_)->
	Path = filename:dirname(code:which(?MODULE)),
	case efene:to_raw_lex(Path++?input++"example_to_remove") of
		{ok, Data, _}-> case file:read_file(Path++?output++"to_raw_lex") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~w", [Data])),
											OutputN = removeN(Output,[]),
											check(Input, OutputN);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.

to_lex_test(_)->
	Path = filename:dirname(code:which(?MODULE)),
	case efene:to_lex(Path++?input++"example_to_remove") of
		{ok, Data, _}-> case file:read_file(Path++?output++"example_to_remove") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~w", [Data])),
											OutputN = removeN(Output,[]),
											check(Input, OutputN);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.


to_ast_test(_)	->
	Path = filename:dirname(code:which(?MODULE)),
	case efene:to_ast(Path++?input++"ast") of
		{ok,Data}-> case file:read_file(Path++?output++"ast") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											check(Input, Output);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.

to_mod_test(_)	->
	Path = filename:dirname(code:which(?MODULE)),
	case efene:to_mod(Path++?input++"ast") of
		{ok, Data}-> case file:read_file(Path++?output++"mod") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											Input2 = removePaths(Input,[],false),
											Output2 = removePaths(Output,[],false),
											check(Input2, Output2);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.

	
to_erl_test(_)	->
	Path = filename:dirname(code:which(?MODULE)),
	case efene:to_erl(Path++?input++"ast") of
		Data -> case file:read_file(Path++?output++"erl") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											OutputN = removeN(Output,[]),
											check(Data, OutputN);
							_ -> ?assert(true =:= false)
						end
	end.


erl2ast_test(_) ->
	Path = filename:dirname(code:which(?MODULE)),
	case efene:from_erl(Path++?input++"erl2.erl") of
		{ok, Data}-> case file:read_file(Path++?output++"erl2ast") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = lists:flatten(io_lib:format("~p~n", [Data])),
											Input2 = removePaths(Input, [], false),
											Output2 = removePaths(Output, [], false),
											check(Input2, Output2);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.
	
erl2erl_test(_) ->
	Path = filename:dirname(code:which(?MODULE)),
	case efene:from_erl(Path++?input++"erl2.erl") of
		{ok, Data} -> case file:read_file(Path++?output++"erl2erl") of
							{ok, Content} -> Output = unicode:characters_to_list(Content, utf8),
											Input = erl_prettypr:format(erl_syntax:form_list(Data)),
											Input2 = removeSpecialPaths(Input, [], true, false),
											Output2 = removeSpecialPaths(Output, [],true,false),
											check(Input2, Output2);
							_ -> ?assert(true =:= false)
						end;
		_-> ?assert(true =:= false)
	end.


