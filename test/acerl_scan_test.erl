
-module(acerl_scan_test).

-include_lib("eunit/include/eunit.hrl").



% Run test module
% $ rebar3 eunit -v -m acerl_scan_test 

scan_test() ->
    ?assertEqual({ok, [{'$eof', #{ line => 1, column => 1}}]}, acerl_rego_scan:scan(<<>>)),
    ok.

scan_example_test() ->
    Example = "
# Test package
package authz

allow {
    input.path == [\"users\"]
    input.method == \"POST\"
}

allow {
    input.path == [\"users\", input.user_id]
    input.method == \"GET\"
}",

    {ok, Tokens} = acerl_rego_scan:scan(Example),
    ok = acerl_rego_parser:parse(Tokens),

    ok.

scan_example2_test() ->
    Example = "package opa.examples

    import data.servers
    import data.networks
    import data.ports

    public_servers[server] {
        server = servers[_]
        server.ports[_] = ports[k].id
        ports[k].networks[_] = networks[m].id
        networks[m].public = true
    }",

    {ok, Tokens} = acerl_rego_scan:scan(Example),
    ok = acerl_rego_parser:parse(Tokens),

    ok.

zotonic_test() ->
    Example = "package zotonic.z_acl

update_actions := {\"admin\", \"insert\", \"update\", \"delete\", \"link\"}

default allow = false

allow {
    input.context.acl == \"admin\" 
}

allow {
    input.context.user_id == 1  # must be a number
}

deny {
    input.context.acl != \"admin\"
    input.context.acl_is_read_only
    input.action == update_actions[input.action]
}


is_allowed {
    allow 
    not deny
}
",

    {ok, Tokens} = acerl_rego_scan:scan(Example),
    {ok, Ast} = acerl_rego_parser:parse(Tokens),

    io:fwrite(standard_error, "~s~n", [jason:encode(Ast)]),

    ok.

