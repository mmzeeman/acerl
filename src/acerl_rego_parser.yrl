%%
%% Yecc Rego Grammar
%%

Nonterminals
    Module Package Imports Import Policy Term Terms OptEqTerm Rules Rule
    OptRuleArgs OptRuleTerm RuleArgs RuleHead RuleBodies RuleBody RuleElse
    Ref RefArgs RefArg RefArgDot RefArgBrack Query Scalar Array Object
    ObjectItems ObjectItem Set ArrayCompr ObjectCompr SetCompr ExprBuiltIn
    ExprInfix Expr Literal LiteralExpr SomeDecl WithModifiers WithModifier
    Vars InfixOperator.

Terminals
    as default package import var else lcbrace rcbrace lsbrace rsbrace
    lparen rparen dot semi_colon string number comma boolean null colon
    set pipe not with some bin_operator arith_operator bool_operator
    eq_operator.


Rootsymbol Module.
Endsymbol '$eof'.

Module -> Package                : #{ package => '$1' }.
Module -> Package Imports        : #{ package => '$1', imports => '$2' }.
Module -> Package Policy         : #{ package => '$1', rules => '$2'}.
Module -> Package Imports Policy : #{ package => '$1', imports => '$2', rules => '$3' }.

Package -> package Ref : #{ path => '$2', type => ref }.

Imports -> Import         : [ '$1' ].
Imports -> Imports Import : '$1' ++ [ '$2' ].

% import          = "import" ref [ "as" var ]
Import -> import Ref        : #{ type => ref, path => '$2' }.
Import -> import Ref as var : #{ type => ref, path => '$2', alias => token_to_value('$4') }.

Policy -> Rules : '$1'.

Rules -> Rule       : [ '$1' ].
Rules -> Rules Rule : '$1' ++ [ '$2' ].

%% rule            = [ "default" ] rule-head { rule-body }
Rule -> default RuleHead RuleBodies : #{ default => true, head => '$2', body => '$3' }.
Rule -> RuleHead RuleBodies         : #{ head => '$1', body => '$2' }.

%% rule-head       = var [ "(" rule-args ")" ] [ "[" term "]" ] [ = term ]
RuleHead -> var OptRuleArgs OptRuleTerm OptEqTerm :
    Head = #{name => token_to_value('$1') },
    Head1 = case '$4' of
        undefined ->
            Head;
        #{ value := Value, operator := #{ value := '=' }} ->
            Head#{ value => Value };
        #{ value := Value, operator := #{ value := ':=' }} ->
            Head#{ value => Value, assing => true }
    end,
    Head2 = maybe_add(rule_args, '$2', Head1),
    maybe_add(rule_term, '$3', Head2).

OptRuleArgs -> '$empty' : undefined.
OptRuleArgs -> lparen RuleArgs rparen : '$2'.

%% rule-args       = term { "," term }
RuleArgs -> Terms : '$1'.

RuleBodies -> '$empty' : [].
RuleBodies -> RuleBody : [ '$1' ].
RuleBodies -> RuleBodies RuleBody : '$1' ++ [ '$2' ].

%% rule-body       = [ else [ = term ] ] "{" query "}"
RuleBody -> RuleElse lcbrace Query rcbrace : #{ query => '$3', 'else' => '$1' }. 
RuleBody -> lcbrace Query rcbrace          : #{ query => '$2' }. 
 
Terms -> Term : [ '$1' ].
Terms -> Terms comma Term : '$1' ++ [ '$3' ].

OptRuleTerm -> '$empty' :  undefined.
OptRuleTerm -> lsbrace Term rsbrace : '$2'.

OptEqTerm -> '$empty'     : undefined. 
OptEqTerm -> eq_operator Term : #{ operator => token_to_value('$1'), value => '$2' }.  

RuleElse -> else OptEqTerm : #{ type => else, value => '$2' }.

%% query           = literal { ( ";" | ( [CR] LF ) ) literal }
Query -> Literal                  : [ '$1' ].
% Query -> Query Literal            : '$1' ++ [ '$2' ].
Query -> Query semi_colon Literal : '$1' ++ [ '$3' ].


% Term -> var. 
Term -> Ref         : '$1'.
Term -> Scalar      : '$1'.
Term -> Array       : '$1'.
Term -> Object      : '$1'.
Term -> Set         : '$1'.
Term -> ArrayCompr  : '$1'.
Term -> ObjectCompr : '$1'.
Term -> SetCompr    : '$1'.

Ref -> var         : [ token_to_value('$1') ].
Ref -> var RefArgs : [ token_to_value('$1') | '$2' ].

RefArgs -> RefArg         : [ '$1' ] .
RefArgs -> RefArgs RefArg : '$1' ++ [ '$2' ].

RefArg -> RefArgDot   : '$1'.
RefArg -> RefArgBrack : '$1'.

RefArgDot -> dot var : token_to_value('$2') .

RefArgBrack -> lsbrace Ref rsbrace    : '$2'.  %% Includes "_"
RefArgBrack -> lsbrace Scalar rsbrace : '$2'.
RefArgBrack -> lsbrace Array rsbrace  : '$2'.
RefArgBrack -> lsbrace Object rsbrace : '$2'.
RefArgBrack -> lsbrace Set rsbrace    : '$2'.

%% literal         = ( some-decl | expr | "not" expr ) { with-modifier }
%% literal       ::= ( some-decl | literal-expr | "not" literal-expr )  with-modifier*

Literal -> SomeDecl                      : #{ type => some_decl, value => '$1' }.
Literal -> SomeDecl WithModifiers        : #{ type => some_decl, value => '$1', with => '$2' }.
Literal -> LiteralExpr                   : #{ type => literal_expr, value => '$1' }.
Literal -> LiteralExpr WithModifiers     : #{ type => literal_expr, value => '$1', with => '$2' }.
Literal -> not LiteralExpr               : Expr = '$2', Expr#{ negate => true }.
Literal -> not LiteralExpr WithModifiers : Expr = '$2', Expr#{ negate => true }.

LiteralExpr -> Expr                  : '$1'.
LiteralExpr -> Expr eq_operator Term : #{ type => eq_expr, left => '$1', right => '$3', op => token_to_value('$2') }.

WithModifiers -> WithModifier               : [ '$1' ].
WithModifiers -> WithModifiers WithModifier : '$1' ++ [ '$1' ].

WithModifier -> with Term as Term : #{ type => with, term => '$3', as => '$4' }.

%% some-decl       = "some" var { "," var }
SomeDecl -> some Vars : #{ type => some, value => token_to_value('$2') }.

Vars -> var            : [ token_to_value('$1') ].
Vars -> Vars comma var : '$1' ++ [ token_to_value('$3') ].

%% scalar          = string | NUMBER | TRUE | FALSE | NULL
Scalar -> string  : token_to_value('$1').
Scalar -> number : token_to_value('$1').
Scalar -> boolean : token_to_value('$1').
Scalar -> null    : token_to_value('$1').

%% array           = "[" term { "," term } "]"
Array -> lsbrace Terms rsbrace : #{ type => array, value => '$2' }.


%% object          = "{" object-item { "," object-item } "}"
%% object-item     = ( scalar | ref | var ) ":" term
Object -> lcbrace ObjectItems rcbrace : '$2'. 
ObjectItems -> ObjectItem : {Key, Value} = '$1', #{ Key => Value } .
ObjectItems -> ObjectItems comma ObjectItem : Map = '$1', {Key, Value} = '$3', Map#{ Key => Value }.

ObjectItem -> Scalar colon Term : {'$1', '$3' }.
ObjectItem -> Ref colon Term : {'$1', '$3' }.
ObjectItem -> var colon Term : {'$1', '$3' }.

%% set             = empty-set | non-empty-set
%% non-empty-set   = "{" term { "," term } "}"
%% empty-set       = "set(" ")"

Set -> set rparen            : #{ type => set, value => [] }.
Set -> lcbrace Terms rcbrace : #{ type => set, value => '$2' }.

%% object-compr    = "{" object-item "|" rule-body "}"
ObjectCompr -> lcbrace ObjectItem pipe RuleBody rcbrace : todo_object_compr.

%% array-compr     = "[" term "|" rule-body "]"
ArrayCompr -> lsbrace Term pipe RuleBody rsbrace : todo_array_compr.

%% set-compr       = "{" term "|" rule-body "}"
SetCompr -> lcbrace Term pipe rcbrace : todo_set_compr.

% expr            = term | expr-call | expr-infix | expr-every
Expr -> Term        : '$1'.
Expr -> ExprBuiltIn : '$1'.
Expr -> ExprInfix   : '$1'.

% expr-built-in   = var [ "." var ] "(" [ term { , term } ] ")"
ExprBuiltIn -> Vars lparen rparen       : #{ type => expr, value => '$1' }.
ExprBuiltIn -> Vars lparen Terms rparen : #{ type => expr, value => '$1', terms => '$3' }.

% expr-infix      = [ term "=" ] term infix-operator term
ExprInfix -> Term InfixOperator Term : #{ type => infix_expr, left => '$1', op => '$2', right => '$3' }.

%% infix-operator  = bool-operator | arith-operator | bin-operator
%% bool-operator   = "==" | "!=" | "<" | ">" | ">=" | "<="
%% arith-operator  = "+" | "-" | "*" | "/"
%% bin-operator    = "&" | "|"
InfixOperator -> bool_operator: token_to_value('$1').
InfixOperator -> arith_operator: token_to_value('$1').
InfixOperator -> bin_operator: token_to_value('$1').

Erlang code.
-compile(inline).

token_to_value({Category, Pos}) ->
    #{ type => Category, position => Pos };
token_to_value({Category, Pos, Value}) ->
    #{ type => Category, value => Value, position => Pos }.

maybe_add(_Key, undefined, Map) -> Map;
maybe_add(Key, Value, Map) -> Map#{ Key => Value}.

