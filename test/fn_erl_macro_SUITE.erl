-module(fn_erl_macro_SUITE).
-compile(export_all).
-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-define(NUM_HEADERS, 1).
-define(NUM_MACROS, 14).


all() -> [parse_to_include_ok, call_macro_no_args, call_macro_with_args,
          call_macro_with_args_not_found, eval_macro, expand_macro_no_args,
          can_expand_var_with_ref, can_expand_macro_str,
          can_expand_simple_inner_macro_call, can_expand_macro_lex_str,
          can_expand_inner_macro_call_in_expr].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

macros(Config) ->
    MacroPath = macro_path(Config),
    {ok, Macros} = fn_erl_macro:macro_defs(MacroPath),
    Macros.

macro_path(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    filename:join(DataDir, "ms.hrl").

print(Thing) ->
    ct:print("~p~n", [Thing]).

exp(Macros, Key, Args) ->
    fn_erl_macro:expand_macro(Macros, Key, Args).

exp(Macros, Key) ->
  fn_erl_macro:expand_macro(Macros, Key).

rand_number() ->
  SignProb = rand:uniform(),
  Sign = if
     (SignProb > 0.5) -> 1;
      true -> -1
  end,
  Num = rand:uniform()*10000000,
  Sign*Num.

check_prop({Prop, Args, Des}) ->
  Result = proper:quickcheck(?MODULE:Prop(Args)),
  if
    Result == false -> ct:fail({Prop, Des, proper:counterexample()});
    Result == true -> true;
    true -> ct:fail({Prop, Des, Result})
  end.

%% Tests

parse_to_include_ok(Config) ->
  MacroPath = macro_path(Config),
  {ok, Ast, Macros} = fn_erl_macro:parse_to_include(MacroPath),

  % Test number of macros retreived
  {_,Num,_,_,_,_,_,_,_} = Macros,
  ?assert(Num =:= ?NUM_MACROS+2),

  % Test number of headers retreived
  L = length(Ast),
  ?assert(L =:= ?NUM_HEADERS+1).

call_macro_no_args(Config) ->
  Macros = macros(Config),
  MacroName = 'AUTHOR',
  {ok, [{string, _, "bob"}]} = fn_erl_macro:call_macro(Macros, MacroName, []).

call_macro_with_args(Config) ->
  Macros = macros(Config),
  MacroName = 'Inc',
  Num = rand_number(),
  {ok,[{op,_,'+',Num,{integer,_,1}}]} = fn_erl_macro:call_macro(Macros, MacroName, [Num]).

call_macro_with_args_not_found(Config) ->
  Macros = macros(Config),
  MacroName = 'Inc',
  {error,{macro_not_found,'Inc'}} = fn_erl_macro:call_macro(Macros, MacroName, []).

eval_macro(Config) ->
    Macros = macros(Config),

    {ok, [{string, _, "bob"}]} = exp(Macros, 'AUTHOR', #{}),

    {ok, [{op, _, '*', {integer, _, 2}, {integer, _, 3}}]} = exp(Macros, 'Const', #{}),

    check_prop({prop_inc_integer_macro, Macros, "inc_integer_macro"}),

    check_prop({prop_inc_float_macro, Macros, "inc_float_macro"}),

    { ok, [ {tuple, _, [{string, _, "bob"}, {integer, _, 42}]} ]
    } = exp(Macros, {'AUTHOR', 1}, #{'A' => {integer, 1, 42}}),


    R1 = exp(Macros, {'V', 3}, 
                #{ 'Line' => {integer, 1, 1}, 
                   'Type' => {integer, 2, 2}, 
                   'Val' => {integer, 3, 3} 
                }),
    { ok, 
      [{ tuple, _, 
         [ {atom, _, val}, {integer, 1, 1}, {integer, 2, 2}, {integer, 3, 3} ]
      }]
    } = R1,


    R2 = exp(Macros, {'IncConst', 1}, #{ 'A' => {integer, 1, 42} }),
    { ok, 
      [{ op, _, '+', {integer, _, 42}, 
         { op, _, '*', {integer, _, 2}, {integer, _, 3} }
      }]
    } = R2.

prop_inc_integer_macro(Macros) ->
  ?FORALL({Int},
    {integer()},
    case exp(Macros, {'Inc', 1}, #{'A' => {integer, 1, Int}}) of
      {ok, [{op, _, '+', {integer, _, Int}, {integer, _, 1}}]} -> true;
      _ -> false
    end).

prop_inc_float_macro(Macros) ->
  ?FORALL({Float},
    {float()},
    case exp(Macros, {'Inc', 1}, #{'A' => {float, 1, Float}}) of
      {ok, [{op, _, '+', {float, _, Float}, {integer, _, 1}}]} -> true;
      _ -> false
    end).

expand_macro_no_args(Config) ->
  Macros = macros(Config),
  {ok, [{string, _, "bob"}]} = exp(Macros, 'AUTHOR').

can_expand_var_with_ref(Config) ->
    Macros = macros(Config),
    Ref = make_ref(),
    
    { ok, 
      [ {op, _, '+', {var, 2, Ref}, {integer, _, 1}} ]
    } = exp(Macros, {'Inc', 1}, #{ 'A' => {var, 2, Ref} }),

    AstNode = {op, 1, '-', {integer, 1, 42}, {integer, 1, 43}},
    { ok, 
      [ {op, _, '+', AstNode, {integer, _, 1}} ]
    } = exp(Macros, {'Inc', 1}, #{'A' => AstNode}).

can_expand_macro_str(Config) ->
    Macros = macros(Config),
    AstNode = {op, 1, '-', {integer, 1, 42}, {integer, 1, 43}},
    {ok, [{string, _, "42 - 43"}]} = exp(Macros, {'Text', 1}, #{'Val' => AstNode}),

    {ok, [Ast]} = exp(Macros, {'TESTCALL', 1}, #{'Call' => AstNode}),
    "io:format(\"Call ~s: ~w~n\", [\"42 - 43\",42 - 43])" = lists:flatten(erl_pp:expr(Ast)).

can_expand_macro_lex_str(Config) ->
    Macros = macros(Config),
    AstNode = {string, 1, "foo"},
    {ok, [Ast]} = exp(Macros, {'NestedText', 1}, #{'A' => AstNode}),
    "\"foo\" ++ \"1 - 2\"" = lists:flatten(erl_pp:expr(Ast)).

can_expand_simple_inner_macro_call(Config) ->
    Macros = macros(Config),
    AstNode = {op, 1, '-', {integer, 1, 42}, {integer, 1, 43}},
    {ok, [Ast]} = exp(Macros, {'Int', 1}, #{'Val' => AstNode}),
    "{val,_,integer,42 - 43}" = lists:flatten(erl_pp:expr(Ast)),

    {ok, [Ast1]} = exp(Macros, {'Int', 2}, #{'Line' => AstNode, 'Val' => {integer, 1, 12}}),
    "{val,42 - 43,integer,12}" = lists:flatten(erl_pp:expr(Ast1)).

can_expand_inner_macro_call_in_expr(Config) ->
    Macros = macros(Config),
    AstNode = {op, 1, '-', {integer, 1, 42}, {integer, 1, 43}},
    {ok, [Ast]} = exp(Macros, {'Int1', 2}, #{'Line' => AstNode, 'Val' => {integer, 1, 12}}),
    "1 + {val,42 - 43,integer,12}" = lists:flatten(erl_pp:expr(Ast)).
