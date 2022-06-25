%%
%%
%%

-module(acerl).

-export([
    create/1,
    evaluate/1
]).

create(Name) ->
    ok.

load_package(Name, Package) ->
    ok.

evaluate(#{}=Input) ->
    evaluate(default, Input).

evaluate(Name, #{}=Input) ->
    #{}.

