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

scan(undefined, Source) when is_binary(Source) ->
    scan(Source, [], #{ line => 1, column => 1 }, in_source);
scan(SourceRef, Source) when is_binary(Source) ->
    scan(Source, [], #{ file => SourceRef, line => 1, column => 1 }, in_source);
scan(SourceRef, Source) when is_list(Source) ->
    scan(SourceRef, unicode:characters_to_binary(Source)).

scan(<<>>, Scanned, Pos, ScanState) when ScanState =:= in_source orelse ScanState =:= in_comment ->
    %% [TODO] Some post precessing
    {ok, lists:reverse([{'$eof', Pos} | Scanned])};



%% Booleans
scan(<<"true", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{boolean, Pos, true} | Scanned], inc_column(Pos, 4), in_source);
scan(<<"false", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{boolean, Pos, false} | Scanned], inc_column(Pos, 5), in_source);
%% Null
scan(<<"null", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{null, Pos} | Scanned], inc_column(Pos, 4), in_source);
%% Keywords
scan(<<"as", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{as, Pos} | Scanned], inc_column(Pos, 2), in_source);
scan(<<"default", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{default, Pos} | Scanned], inc_column(Pos, 7), in_source);
scan(<<"else", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{else, Pos} | Scanned], inc_column(Pos, 4), in_source);
scan(<<"import", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{import, Pos} | Scanned], inc_column(Pos, 6), in_source);
scan(<<"package", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{package, Pos} | Scanned], inc_column(Pos, 7), in_source);
scan(<<"not", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{'not', Pos} | Scanned], inc_column(Pos, 3), in_source);
scan(<<"with", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{with, Pos} | Scanned], inc_column(Pos, 4), in_source);
%% Empty set start.
scan(<<"set(", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{set, Pos} | Scanned], inc_column(Pos, 4), in_source);
%% Various braces
scan(<<$[, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{lsbrace, Pos} | Scanned], inc_column(Pos), in_source);
scan(<<${, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{lcbrace, Pos} | Scanned], inc_column(Pos), in_source);
scan(<<$(, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{lparen, Pos} | Scanned], inc_column(Pos), in_source);

scan(<<$], Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{rsbrace, Pos} | Scanned], inc_column(Pos), in_source);
scan(<<$}, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{rcbrace, Pos} | Scanned], inc_column(Pos), in_source);
scan(<<$), Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{rparen, Pos} | Scanned], inc_column(Pos), in_source);

%% Bin operators
scan(<<$|, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{bin_operator, Pos, $|} | Scanned], inc_column(Pos), in_source);
scan(<<$&, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{bin_poperator, Pos, $&} | Scanned], inc_column(Pos), in_source);

%% Arith operators
scan(<<$*, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{arith_operator, Pos, $*} | Scanned], inc_column(Pos), in_source);
scan(<<$/, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{arith_operator, Pos, $/} | Scanned], inc_column(Pos), in_source);
scan(<<$-, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{arith_operator, Pos, $-} | Scanned], inc_column(Pos), in_source);
scan(<<$+, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{arith_operator, Pos, $+} | Scanned], inc_column(Pos), in_source);

%% Comparison
scan(<<"==", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{bool_operator, Pos, '=='} | Scanned], inc_column(Pos, 2), in_source);
scan(<<"!=", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{bool_operator, Pos, '!='} | Scanned], inc_column(Pos, 2), in_source);
scan(<<"<=", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{bool_operator, Pos, '<='} | Scanned], inc_column(Pos, 2), in_source);
scan(<<">=", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{bool_operator, Pos, '>='} | Scanned], inc_column(Pos, 2), in_source);
scan(<<$>, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{bool_operator, Pos, '>'} | Scanned], inc_column(Pos), in_source);
scan(<<$<, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{bool_operator, Pos, '>'} | Scanned], inc_column(Pos), in_source);

%% Equality
scan(<<":=", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{eq_operator, Pos, ':='} | Scanned], inc_column(Pos, 2), in_source);
scan(<<$=, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{eq_operator, Pos, '='} | Scanned], inc_column(Pos, 1), in_source);

%% Various separators
scan(<<$,, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{comma, Pos} | Scanned], inc_column(Pos), in_source);
scan(<<$;, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{semicolon, Pos} | Scanned], inc_column(Pos), in_source);
scan(<<$:, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{colon, Pos} | Scanned], inc_column(Pos), in_source);
scan(<<$., Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{dot, Pos} | Scanned], inc_column(Pos), in_source);

%% Numbers...

scan(<<Ws, Rest/binary>>, [{number, StartPos, Number} | Scanned], Pos, InNumberFloatOrExponent) when
      ?IS_WHITESPACE(Ws)
      andalso (InNumberFloatOrExponent =:= in_number orelse InNumberFloatOrExponent =:= in_float orelse InNumberFloatOrExponent =:= in_exponent) ->
    scan(Rest, [{number, StartPos, jason:decode(Number)} | Scanned], Pos, in_source);

%% Number start
scan(<<$-, N, Rest/binary>>, Scanned, Pos, in_source) when ?IS_DIGIT(N) ->
    scan(Rest, [{number, Pos, <<"-", N>>} | Scanned], inc_column(Pos, 2), in_number);
scan(<<N, Rest/binary>>, Scanned, Pos, in_source) when ?IS_DIGIT(N) ->
    scan(Rest, [{number, Pos, <<N>>} | Scanned], inc_column(Pos, 2), in_number);

%% Exponent detection
scan(<<E, $+, N, Rest/binary>>, [{number, StartPos, Number} | Scanned], Pos, InNumberOrFloat)  when
      (InNumberOrFloat =:= in_number orelse InNumberOrFloat =:= in_float)
      andalso (E =:= $E orelse E =:= $e) 
      andalso ?IS_DIGIT(N) ->
    scan(Rest, [{number, StartPos, <<Number/binary, E, $+, N>>} | Scanned], inc_column(Pos, 3), in_exponent);
scan(<<E, $-, N, Rest/binary>>, [{number, StartPos, Number} | Scanned], Pos, InNumberOrFloat) when 
      (InNumberOrFloat =:= in_number orelse InNumberOrFloat =:= in_float)
      andalso (E =:= $E orelse E =:= $e) 
      andalso ?IS_DIGIT(N) ->
    scan(Rest, [{number, StartPos, <<Number/binary, E, $-, N>>} | Scanned], inc_column(Pos, 3), in_exponent);
scan(<<E, N, Rest/binary>>, [{number, StartPos, Number} | Scanned], Pos, InNumberOrFloat) when
      (InNumberOrFloat =:= in_number orelse InNumberOrFloat =:= in_float)
      andalso (E =:= $E orelse E =:= $e) 
      andalso ?IS_DIGIT(N) ->
    scan(Rest, [{number, StartPos, <<Number/binary, E, N>>} | Scanned], inc_column(Pos, 2), in_exponent);

%% Decimal dot detection
scan(<<$., N, Rest/binary>>, [{number, StartPos, Number} | Scanned], Pos, in_number) ->
    scan(Rest, [{number, StartPos, <<Number/binary, $., N>>} | Scanned], inc_column(Pos, 2), in_float);

%% Consume a number
scan(<<N, Rest/binary>>, [{number, StartPos, Number} | Scanned], Pos, InNumberFloatOrExponent) when
      ?IS_DIGIT(N)
      andalso (InNumberFloatOrExponent =:= in_number orelse InNumberFloatOrExponent =:= in_float orelse InNumberFloatOrExponent =:= in_exponent) ->
    scan(Rest, [{number, StartPos, <<Number/binary,  N>>} | Scanned], inc_column(Pos), InNumberFloatOrExponent);

%% Recognized a number, use jason to decode the number
scan(Rest, [{number, StartPos, Number} | Scanned], Pos, InNumberFloatOrExponent) when
      InNumberFloatOrExponent =:= in_number orelse InNumberFloatOrExponent =:= in_float orelse InNumberFloatOrExponent =:= in_exponent ->
    scan(Rest, [{number, StartPos, jason:decode(Number)} | Scanned], Pos, in_source);

%% Strings between quotes, a quote must be escaped.
scan(<<$", Rest/binary>>, Scanned, Pos, in_string) ->
    scan(Rest, Scanned, inc_column(Pos), in_source);
scan(<<$", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{string, Pos, <<>>} | Scanned], inc_column(Pos), in_string);
%% [TODO] add escape sequences, newline checks etc.
scan(<<C/utf8, Rest/binary>>, [{string, StringStartPos, Acc} | Scanned], Pos, in_string) ->
    scan(Rest, [{string, StringStartPos, <<Acc/binary, C/utf8>>} | Scanned], inc_column(Pos), in_string);

%% Raw string between backticks, everything is consumed.
scan(<<$`, Rest/binary>>, Scanned, Pos, in_raw_string) ->
    scan(Rest/binary, Scanned, inc_column(Pos), in_source);
scan(<<$`, Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, [{string, Pos, <<>>} | Scanned], inc_column(Pos), in_raw_string);
scan(<<C/utf8, Rest/binary>>, [{string, StringStartPos, Acc} | Scanned], Pos, in_raw_string) ->
    %% Todo, count newlines in raw strings
    scan(Rest, [{string, StringStartPos, <<Acc/binary, C/utf8>>} | Scanned], inc_column(Pos), in_raw_string);

%% Vars 
scan(<<C/utf8, Rest/binary>>, [{var, VarStartPos, Acc} | Scanned], Pos, in_var) when ?IS_ALPHA(C) orelse ?IS_DIGIT(C) orelse C =:= $_ ->
    scan(Rest, [{var, VarStartPos, <<Acc/binary, C/utf8>>} | Scanned], inc_column(Pos), in_var);
scan(Rest, Scanned, Pos, in_var) ->
    scan(Rest, Scanned, Pos, in_source);
scan(<<C/utf8, Rest/binary>>, Scanned, Pos, in_source) when ?IS_ALPHA(C) orelse C =:= $_ ->
    scan(Rest, [{var, Pos, <<C/utf8>>} | Scanned], inc_column(Pos), in_var);

%% Whitespace
scan(<<"\r\n", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, Scanned, inc_line(Pos), in_source);
scan(<<"\n", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, Scanned, inc_line(Pos), in_source);
scan(<<C, Rest/binary>>, Scanned, Pos, in_source) when ?IS_WHITESPACE(C) ->
    scan(Rest, Scanned, inc_column(Pos), in_source);

%% Comments
scan(<<"\r\n", Rest/binary>>, Scanned, Pos, in_comment) ->
    scan(Rest, Scanned, inc_line(Pos), in_source);
scan(<<"\n", Rest/binary>>, Scanned, Pos, in_comment) ->
    scan(Rest, Scanned, inc_line(Pos), in_source);
scan(<<"#", Rest/binary>>, Scanned, Pos, in_source) ->
    scan(Rest, Scanned, inc_column(Pos), in_comment);
scan(<<_C/utf8, Rest/binary>>, Scanned, Pos, in_comment) ->
    scan(Rest, Scanned, inc_column(Pos), in_comment);

scan(_X, Scanned, SRC, In) ->
    io:format("~n~n~p~n~n", [{Scanned, SRC, In}]),
    a = _X.

%%
%% Helpers
%%

inc_line(#{ line := Line }=Position) ->
    Position#{ line := Line + 1, column := 1 }. 

inc_column(Position) ->
    inc_column(Position, 1).

inc_column(#{ column := Column}=Position, N) ->
    Position#{ column := Column + N}.


