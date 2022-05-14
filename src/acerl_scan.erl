%%
%%
%%

-module(acerl_scan).

-export([scan/1, scan/2]).

-define(IS_WHITESPACE(C), (C =:= $\t orelse C =:= $\r orelse C =:= $\s)).

-define(IS_ALPHA(C), ((C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z))).
-define(IS_DIGIT(C), ((C >= $0 andalso C =< $9))).

scan(Source) ->
    scan(undefined, Source).

scan(SourceRef, Source) when is_binary(Source) ->
    scan(Source, [], {SourceRef, 1, 1}, in_source);
scan(SourceRef, Source) when is_list(Source) ->
    scan(SourceRef, unicode:characters_to_binary(Source)).

scan(<<>>, Scanned, Pos, ScanState) when ScanState =:= in_source orelse ScanState =:= in_comment ->
    %% [TODO] Some post precessing
    {ok, lists:reverse([{eof, Pos} | Scanned])};

%% Booleans
scan(<<"true", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{boolean, Pos, true} | Scanned], {SourceRef, Row, Column + 4}, in_source);
scan(<<"false", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{boolean, Pos, false} | Scanned], {SourceRef, Row, Column + 5}, in_source);

scan(<<"null", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{null, Pos} | Scanned], {SourceRef, Row, Column + 4}, in_source);
scan(<<"as", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{as, Pos} | Scanned], {SourceRef, Row, Column + 2}, in_source);
scan(<<"default", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{default, Pos} | Scanned], {SourceRef, Row, Column + 7}, in_source);
scan(<<"else", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{else, Pos} | Scanned], {SourceRef, Row, Column + 4}, in_source);
scan(<<"import", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{import, Pos} | Scanned], {SourceRef, Row, Column + 6}, in_source);
scan(<<"package", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{package, Pos} | Scanned], {SourceRef, Row, Column + 7}, in_source);
scan(<<"not", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{'not', Pos} | Scanned], {SourceRef, Row, Column + 3}, in_source);
scan(<<"with", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{with, Pos} | Scanned], {SourceRef, Row, Column + 4}, in_source);

scan(<<"set(", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{set, Pos} | Scanned], {SourceRef, Row, Column + 4}, in_source);

scan(<<$[, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{lsbrace, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<${, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{lcbrace, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$(, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{lparen, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);

scan(<<$], Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{rsbrace, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$}, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{rcbrace, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$), Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{rparen, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);

scan(<<$|, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{mid, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);

scan(<<$*, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{factor_operator, Pos, $*} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$/, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{factor_operator, Pos, $/} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$%, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{factor_operator, Pos, $%} | Scanned], {SourceRef, Row, Column + 1}, in_source);

scan(<<"==", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{relation_operator, Pos, '=='} | Scanned], {SourceRef, Row, Column + 2}, in_source);
scan(<<"!=", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{relation_operator, Pos, '!='} | Scanned], {SourceRef, Row, Column + 2}, in_source);
scan(<<"<=", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{relation_operator, Pos, '<='} | Scanned], {SourceRef, Row, Column + 2}, in_source);
scan(<<">=", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{relation_operator, Pos, '>='} | Scanned], {SourceRef, Row, Column + 2}, in_source);
scan(<<$>, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{relation_operator, Pos, '>'} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$<, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{relation_operator, Pos, '>'} | Scanned], {SourceRef, Row, Column + 1}, in_source);

scan(<<":=", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{eq_oper, Pos, ':='} | Scanned], {SourceRef, Row, Column + 2}, in_source);
scan(<<$=, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{eq_oper, Pos, '='} | Scanned], {SourceRef, Row, Column + 1}, in_source);

scan(<<$,, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{comma, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$;, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{semicolon, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$:, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{colon, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$&, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{ampersand, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);
scan(<<$., Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{dot, Pos} | Scanned], {SourceRef, Row, Column + 1}, in_source);

%% Strings between quotes, a quote must be escaped.
scan(<<$", Rest/binary>>, Scanned, {SourceRef, Row, Column}, in_string) ->
    scan(Rest, Scanned, {SourceRef, Row, Column+1}, in_source);
scan(<<$", Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{string, Pos, <<>>} | Scanned], {SourceRef, Row, Column+1}, in_string);
%% [TODO] add escape sequences
scan(<<C/utf8, Rest/binary>>, [{string, Pos, Acc} | Scanned], {SourceRef, Row, Column}, in_string) ->
    scan(Rest, [{string, Pos, <<Acc/binary, C/utf8>>} | Scanned], {SourceRef, Row, Column+1}, in_string);

%% Raw string between backticks, everything is consumed.
scan(<<$`, Rest/binary>>, Scanned, {SourceRef, Row, Column}, in_raw_string) ->
    scan(Rest/binary, Scanned, {SourceRef, Row, Column+1}, in_source);
scan(<<$`, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) ->
    scan(Rest, [{string, Pos, <<>>} | Scanned], {SourceRef, Row, Column+1}, in_raw_string);
scan(<<C/utf8, Rest/binary>>, [{string, Pos, Acc} | Scanned], {SourceRef, Row, Column}, in_raw_string) ->
    scan(Rest, [{string, Pos, <<Acc/binary, C/utf8>>} | Scanned], {SourceRef, Row, Column+1}, in_raw_string);

%% Vars 
scan(<<C/utf8, Rest/binary>>, [{var, Pos, Acc} | Scanned], {SourceRef, Row, Column}, in_var) when ?IS_ALPHA(C) orelse ?IS_DIGIT(C) orelse C =:= $_ ->
    scan(Rest, [{var, Pos, <<Acc/binary, C/utf8>>} | Scanned], {SourceRef, Row, Column+1}, in_var);
scan(Rest, Scanned, Pos, in_var) ->
    scan(Rest, Scanned, Pos, in_source);
scan(<<C/utf8, Rest/binary>>, Scanned, {SourceRef, Row, Column}=Pos, in_source) when ?IS_ALPHA(C) orelse C =:= $_ ->
    scan(Rest, [{var, Pos, <<C/utf8>>} | Scanned], {SourceRef, Row, Column+1}, in_var);

%% Whitespace
scan(<<"\r\n", Rest/binary>>, Scanned, {SourceRef, Row, _Column}, in_source) ->
    scan(Rest, Scanned, {SourceRef, Row+1, 1}, in_source);
scan(<<"\n", Rest/binary>>, Scanned, {SourceRef, Row, _Column}, in_source) ->
    scan(Rest, Scanned, {SourceRef, Row+1, 1}, in_source);
scan(<<C, Rest/binary>>, Scanned, {SourceRef, Row, Column}, in_source) when ?IS_WHITESPACE(C) ->
    scan(Rest, Scanned, {SourceRef, Row, Column+1}, in_source);

%% Comments
scan(<<"\r\n", Rest/binary>>, Scanned, {SourceRef, Row, _Column}, in_comment) ->
    scan(Rest, Scanned, {SourceRef, Row+1, 1}, in_source);
scan(<<"\n", Rest/binary>>, Scanned, {SourceRef, Row, _Column}, in_comment) ->
    scan(Rest, Scanned, {SourceRef, Row+1, 1}, in_source);
scan(<<"#", Rest/binary>>, Scanned, {SourceRef, Row, Column}, in_source) ->
    scan(Rest, Scanned, {SourceRef, Row, Column + 1}, in_comment);
scan(<<_C/utf8, Rest/binary>>, Scanned, {SourceRef, Row, Column}, in_comment) ->
    scan(Rest, Scanned, {SourceRef, Row, Column + 1}, in_comment);

scan(_X, Scanned, SRC, In) ->
    io:format("~n~n~p~n~n", [{Scanned, SRC, In}]),
    a = _X.







