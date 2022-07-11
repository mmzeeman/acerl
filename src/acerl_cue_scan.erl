%%
%% CUE scanner.
%%

-module(acerl_cue_scan).

-export([scan/1, scan/2]).

-define(IS_WHITESPACE(C), (C =:= $\t orelse C =:= $\r orelse C =:= $\s)).

-define(IS_DIGIT(C), ((C >= $0 andalso C =< $9))).
-define(IS_LETTER(C), ((C >= $a andalso C =< $Z) orelse (C >= $A andalso C =< $Z) orelse (C =:= $_) orelse (C =:= $$))).

scan(Source) ->
    scan(undefined, Source).

scan(undefined, Source) when is_binary(Source) ->
    scan(Source, 
         [],
         #{ line => 1, column => 1 },
         #{ insert_comma => false });
scan(SourceRef, Source) when is_binary(Source) ->
    scan(Source,
         [],
         #{ line => 1, column => 1 },
         #{ file => SourceRef, insert_comma => false});
scan(SourceRef, Source) when is_list(Source) ->
    scan(unicode:characters_to_binary(Source),
         [],
         #{ line => 1, column => 1 },
         #{ file => SourceRef, insert_comma => false }).

%% Reached end of input
scan(<<>>, Scanned, Pos, _ScanState) ->
    {ok, lists:reverse([{'$eof', Pos} | Scanned])};

scan(<<C/utf8, Rest/binary>>, Scanned, Pos, #{}=ScanState) when C =:= $\s orelse C =:= $\t ->
    scan(Rest, Scanned, inc_column(Pos), ScanState);

scan(<<$\n, Rest/binary>>, Scanned, Pos, #{ insert_comma := true }=ScanState)  ->
    scan(Rest, [{comma, Pos} | Scanned], inc_line(Pos), ScanState#{ insert_comma := false });
scan(<<$\n, Rest/binary>>, Scanned, Pos, ScanState)  ->
    scan(Rest, Scanned, inc_line(Pos), ScanState);

scan(<<$\r, Rest/binary>>, Scanned, Pos, ScanState)  ->
    scan(Rest, Scanned, inc_column(Pos), ScanState);

%% Numbers
scan(<<C/utf8, Rest/binary>>, Scanned, Pos, ScanState) when ?IS_DIGIT(C) ->
    %% in_number
    scan_number();

%% Identifiers.
scan(<<$#, C/utf8, Rest/binary>>, Scanned, Pos, ScanState) when ?IS_LETTER(C) ->
    {Lit, Pos1, Rest1} = scan_field_identifier(Rest, Pos, <<$#, C/utf8>>),
    Token = {identifier, Pos, Lit},
    scan(Rest1, [Token | Scanned], Pos1, ScanState#{ insert_comma => true });

scan(<<$_,  $#, C/utf8, Rest/binary>>, Scanned, Pos, ScanState) when ?IS_LETTER(C) ->
    {Lit, Pos1, Rest1} = scan_field_identifier(Rest, Pos, <<$_, $#, C/utf8>>),
    Token = {identifier, Pos, Lit},
    scan(Rest1, [Token | Scanned], Pos1, ScanState#{ insert_comma => true });

scan(<<C/utf8, Rest/binary>>, Scanned, Pos, ScanState) when ?IS_LETTER(C) orelse C =:= $$ ->
    %% in_field_identifier
    {Lit, Pos1, Rest1} = scan_field_identifier(Rest, Pos, <<C/utf8>>),
    Token = literal_to_token(Lit, Pos), %% maybe this is an identifier.
    scan(Rest1, [Token | Scanned], Pos1, ScanState);

scan(<<"_|_", Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{bottom, Pos} | Scanned], inc_column(Pos, 3), ScanState);

scan(<<"_|", Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{illegal, Pos, <<"got _|, expected _|_">>} | Scanned], inc_column(Pos, 2), ScanState);

%scan(<<"\n", Rest/binary>>, Scanned, Pos, ScanState) ->
%    ok;

%% Strings
scan(<<$#, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan_string(Rest, $");

scan(<<$", Rest/binary>>, Scanned, Pos, ScanState) ->
    scan_string(Rest, $");

scan(<<$', Rest/binary>>, Scanned, Pos, ScanState) ->
    scan_string(Rest, $');

%% 
scan(<<$@, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan_attribute();

scan(<<$:, $:, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{isa, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$:, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{colon, Pos} | Scanned], inc_column(Pos), ScanState);

scan(<<$;, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{semicolon, Pos} | Scanned], inc_column(Pos), ScanState);
    
scan(<<$?, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{option, Pos} | Scanned], inc_column(Pos), ScanState);

scan(<<$., $., $., Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{ellipsis, Pos} | Scanned], inc_column(Pos, 3), ScanState);

scan(<<$., $., Rest/binary>>, Scanned, Pos, ScanState) ->
    %% illegal-token... got .., expected ...
    scan(Rest, [{illegal, Pos, <<"got .., expected ...">>} | Scanned], inc_column(Pos, 2), ScanState);

scan(<<$., C/utf8, Rest/binary>>, Scanned, Pos, ScanState) when ?IS_DIGIT(C) ->
    %% [TODO]
    scan_number();
scan(<<$., Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{period, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$,, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{comma, Pos} | Scanned], inc_column(Pos), ScanState);

scan(<<$(, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{rparen, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$), Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{rparen, Pos} | Scanned], inc_column(Pos), ScanState#{ insert_comma => true });

scan(<<$[, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{lbrack, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$], Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{rbrack, Pos} | Scanned], inc_column(Pos), ScanState#{ insert_comma => true });

scan(<<${, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{lbrace, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$}, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{rbrace, Pos} | Scanned], inc_column(Pos), ScanState#{ insert_comma => true });

scan(<<$+, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{add, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$-, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{sub, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$*, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{mul, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$/, $/, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, Scanned, inc_column(Pos, 2), ScanState);
scan(<<$/, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{quo, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$<, $-, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{arrow, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$<, $=, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{leq, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$<, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{lss, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$>, $=, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{geq, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$>, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{gtr, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$=, $~, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{mat, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$=, $=, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{eql, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$=, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{bind, Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$!, $~,  Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{nmat, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$!, $=,  Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{neq, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$!, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{'not', Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$&, $&,  Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{land, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$&, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{'and', Pos} | Scanned], inc_column(Pos), ScanState);
scan(<<$|, $|,  Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{lor, Pos} | Scanned], inc_column(Pos, 2), ScanState);
scan(<<$|, Rest/binary>>, Scanned, Pos, ScanState) ->
    scan(Rest, [{'or', Pos} | Scanned], inc_column(Pos), ScanState).

scan_string(_, _Quote) ->
    ok.

scan_number() ->
    ok.

scan_attribute() ->
    ok.

scan_field_identifier(<<C/utf8, Rest/binary>>, Pos, Acc) when ?IS_LETTER(C) orelse ?IS_DIGIT(C) ->
    scan_field_identifier(Rest, inc_column(Pos), <<Acc/binary, C/utf8>>);
scan_field_identifier(Rest, Pos, Acc) ->
    {Acc, Pos, Rest}.

%%
%% Helpers
%%

inc_line(#{ line := Line }=Position) ->
    Position#{ line := Line + 1, column := 1 }. 

inc_column(Position) ->
    inc_column(Position, 1).

inc_column(#{ column := Column}=Position, N) ->
    Position#{ column := Column + N}.


literal_to_token(<<"if">>, Pos) -> {'if', Pos};
literal_to_token(<<"for">>, Pos) -> {'for', Pos};
literal_to_token(<<"in">>, Pos) -> {'in', Pos};
literal_to_token(<<"let">>, Pos) -> {'let', Pos};
literal_to_token(<<"true">>, Pos) -> {true, Pos};
literal_to_token(<<"false">>, Pos) -> {false, Pos};
literal_to_token(<<"null">>, Pos) -> {null, Pos};
literal_to_token(Ident, Pos) when is_binary(Ident) ->
    {identifier, Pos, Ident}.


