-module(erl2).

-export([first_example/1, stop/1, start/2]).

-behaviour(application).

start(_StartType, _StartArgs) ->
    lager:info("Starting app myfnapp"),
    name_sup:start_link().

stop(_State) -> lager:info("Stopping app myfnapp"), ok.

first_example(42) -> 42;
first_example(_) -> 0.


