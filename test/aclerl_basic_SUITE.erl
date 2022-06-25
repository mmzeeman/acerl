-module(aclerl_basic_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0]).

-export([empty_string_test/1, multiple_vars_test/1, vars_with_keyword_prefix/1,
         keywords/1, package_default/1, input_method_GET/1, hello_world_example/1
]).

suite() ->
    [
        {timetrap, {seconds, 30}}
    ].

all() ->
    [
     {group, basic}
    ].

groups() ->
    [
     {basic, [],
      [
       empty_string_test,
       multiple_vars_test,
       vars_with_keyword_prefix,
       keywords,
       package_default,
       input_method_GET,
       hello_world_example
      ]
     }
    ].

empty_string_test(_Config) ->
    {ok, [
          {'$eof',#{column := 1,
                    line := 1}}
         ]
    } = acerl_scan:scan(<<>>).

multiple_vars_test(_Config) ->
   {ok, [
         {var, #{column := 1, line := 1}, <<"test">>},
         {dot, #{column := 5, line := 1}},
         {var, #{column := 6, line := 1}, <<"one">>},
         {dot, #{column := 9, line := 1}},
         {var, #{column := 10, line := 1}, <<"two">>},
         {dot, #{column := 13, line := 1}},
         {var, #{column := 14, line := 1}, <<"three">>},
         {'$eof', #{column := 19, line := 1}}
        ]
   } = acerl_scan:scan(<<"test.one.two.three">>),

   ok.

vars_with_keyword_prefix(_Config) ->
   {ok, [
         {var, _, <<"trueism">>},
         {'$eof', _}
        ]
   } = acerl_scan:scan(<<"trueism">>),

   {ok, [
         {var, _, <<"package_a">>},
         {'$eof', _}
        ]
   } = acerl_scan:scan(<<"package_a">>),

   ok.

keywords(_Config) ->
    {ok, [
          {boolean, _, true},
          {'$eof', _}
         ]
    } = acerl_scan:scan(<<"true">>),

    {ok, [
          {boolean, _, true},
          {boolean, _, false},
          {package, _},
          {as, _},
          {'$eof', _}
         ]
    } = acerl_scan:scan(<<"true false package as">>),

    ok.

package_default(_Config) ->
    Policy = "
package test.default_rule

default allow = true",

    {ok, Tokens} = acerl_scan:scan(Policy),
    {ok, #{} = Module} = acerl_parser:parse(Tokens),

    #{ package :=
       #{
         path := [
                  #{ value := <<"test">> },
                  #{ value := <<"default_rule">> }
                 ] 
        },
       rules := [
                 #{ default := true ,
                    head := #{
                              name := #{ value := <<"allow">> },
                              value := #{ type := boolean, value := true }
                             },
                    body := []
                  }
                ]
     } = Module,

    ok.

input_method_GET(_Config) ->
    Policy = "
package test.default_rule

default allow = true

allow {
    input.method == \"GET\";
}
",

    {ok, Tokens} = acerl_scan:scan(Policy),
    {ok, #{} = Module} = acerl_parser:parse(Tokens),

    #{ package :=
       #{
         path := [
                  #{ value := <<"test">> },
                  #{ value := <<"default_rule">> }
                 ] 
        },
       rules := [
                 #{ default := true ,
                    head := #{
                              name := #{ value := <<"allow">> },
                              value := #{ type := boolean, value := true }
                             },
                    body := []
                  },
                 #{ head := #{
                              name := #{ value := <<"allow">> }
                             },
                     body := Body
                  }
                ]
     } = Module,

     [Terms] = Body,

     ok = Terms,

    ok.

hello_world_example(_Config) ->
    Policy = "
package play

default hello = false

hello {
    m := input.message
    m == \"world\"
}
",

    {ok, Tokens} = acerl_scan:scan(Policy),
    {ok, #{} = Module} = acerl_parser:parse(Tokens),

    #{ package :=
       #{
         path := [
                  #{ value := <<"play">> }
                 ] 
        },
       rules := [ ]
     } = Module,


    ok.

