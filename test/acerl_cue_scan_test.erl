
-module(acerl_cue_scan_test).

-include_lib("eunit/include/eunit.hrl").

scan_empty_test() ->
    ?assertEqual({ok, [{'$eof', #{ line => 1, column => 1}}]}, acerl_cue_scan:scan(<<>>)),
    ok.


scan_ellipsis_test() ->
    ?assertEqual({ok,
                  [
                   {ellipsis, #{ column => 1,line => 1}},
                   {'$eof', #{ line => 1, column => 4}}
                  ]}, acerl_cue_scan:scan(<<"...">>)),
    ok.


scan_tokens_test() ->
    Input = ",
_|_
&|
&&
||
<-
->
==
<
>
=
!
!=
<=
>=
:=
...
(
[
[[
{
{{
.
)
]
]]
}
}}
:
;",
    {ok, Tokens} =  acerl_cue_scan:scan(Input),

    [{comma, _},
     {bottom, _},
     {'and', _},
     {'or', _},
     {land, _},
     {lor, _},
     {arrow, _} | Rest
    ] = Tokens,

    ok.

