%%
%%

Nonterminals
    Module
    Package
    Imports
    Import
    Policy
    Term
    Terms
    OptEqTerm
    Rules
    Rule
    OptRuleArgs
    OptRuleTerm
    RuleArgs
    RuleHead
    RuleBodies
    RuleBody
    RuleElse
    Ref
    RefArgs
    RefArg
    RefArgDot
    RefArgBrack
    Query
    Scalar
    Array
    Object
    ObjectItems
    ObjectItem
    Set
    ArrayCompr
    ObjectCompr
    SetCompr
    ExprBuiltIn
    ExprInfix
    Expr
    Literal
    LiteralExpr
    SomeDecl
    WithModifiers
    WithModifier
    Vars
    InfixOperator.


Terminals
    as
    default
    package
    import
    var
    eq_operation
    else
    lcbrace rcbrace
    lsbrace rsbrace
    lparen rparen
    dot
    semi_colon
    string
    comma
    boolean
    null
    colon
    set
    pipe
    not
    with
    some
    bin_operator
    arith_operator
    bool_operator
    eq_operator
    .


Rootsymbol Module.
Endsymbol '$eof'.

Module -> Package                : #{ type => module, package => '$1' }.
Module -> Package Imports        : #{ type => module, package => '$1', imports => '$2' }.
Module -> Package Policy         : #{ type => module, package => '$1', policy => '$2'}.
Module -> Package Imports Policy : #{ type => module, package => '$1', imports => '$2', policy => '$3' }.

Package -> package Ref : '$2'.

Imports -> Import         : [ '$1' ].
Imports -> Imports Import : '$1' ++ [ '$2' ].

% import          = "import" ref [ "as" var ]
Import -> import Ref        : #{ type => import, package => '$2' }.
Import -> import Ref as var : #{ type => import, package => '$2', as => '$4' }.

Policy -> Rules : #{ type => policy, rules => '$1' }.

Rules -> Rule       : [ '$1' ].
Rules -> Rules Rule : '$1' ++ [ '$2' ].

%% rule            = [ "default" ] rule-head { rule-body }
Rule -> default RuleHead RuleBodies : #{ type => default_rule, head => '$2', body => '$3' }.
Rule -> RuleHead RuleBodies         : #{ type => rule, head => '$1', body => '$2' }.

%% rule-head       = var [ "(" rule-args ")" ] [ "[" term "]" ] [ = term ]
RuleHead -> var OptRuleArgs OptRuleTerm OptEqTerm : #{type => head, name => '$1', rule_args => '$2', rule_term => '$3', eq_term => '$4' }.

OptRuleArgs -> '$empty'.
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
OptEqTerm -> eq_operation Term : #{ type => eq_operator, value => '$2' }.  

RuleElse -> else OptEqTerm : #{ type => else, value => '$2' }.

%% query           = literal { ( ";" | ( [CR] LF ) ) literal }
Query -> Literal                  : [ '$1' ].
Query -> Query Literal            : '$1' ++ [ '$2' ].
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

Ref -> var         : #{ type => ref, value => token_to_value('$1') }.
Ref -> var RefArgs : #{ type => ref, value => token_to_value('$1'), args => '$2' }.

RefArgs -> RefArg         : [ '$1' ] .
RefArgs -> RefArgs RefArg : '$1' ++ [ '$2' ].

RefArg -> RefArgDot   : '$1'.
RefArg -> RefArgBrack : '$1'.

RefArgDot -> dot var : '$2' .

RefArgBrack -> lsbrace var rsbrace    : '$2'.  %% Includes "_"
RefArgBrack -> lsbrace Scalar rsbrace : '$2'.
RefArgBrack -> lsbrace Array rsbrace  : '$2'.
RefArgBrack -> lsbrace Object rsbrace : '$2'.
RefArgBrack -> lsbrace Set rsbrace    : '$2'.

%% literal         = ( some-decl | expr | "not" expr ) { with-modifier }
%% literal             ::= ( some-decl | literal-expr | "not" literal-expr )  with-modifier*

Literal -> SomeDecl                      : #{ value => '$1' }.
Literal -> SomeDecl WithModifiers        : #{ value => '$1', with => '$2' }.
Literal -> LiteralExpr                   : #{ value => '$1' }.
Literal -> LiteralExpr WithModifiers     : #{ value => '$1', with => '$2' }.
Literal -> not LiteralExpr               : #{ negate => true, value => '$2' }.
Literal -> not LiteralExpr WithModifiers : #{ negate => true, value => '$2', with => '$3' }.

LiteralExpr -> Expr                  : '$1'.
LiteralExpr -> Expr eq_operator Term : #{ type => eq_expr, left => '$1', right => '$3', op => token_to_value('$2') }.

WithModifiers -> WithModifier               : [ '$1' ].
WithModifiers -> WithModifiers WithModifier : '$1' ++ [ '$1' ].

WithModifier -> with Term as Term : #{ type => with, term => '$3', as => '$4' }.

%% some-decl       = "some" var { "," var }
SomeDecl -> some Vars : #{ type => some, value => '$2' }.

Vars -> var            : [ '$1' ].
Vars -> Vars comma var : '$1' ++ [ '$3' ].

%% scalar          = string | NUMBER | TRUE | FALSE | NULL
Scalar -> string  : token_to_value('$1').
% Scalar -> number. [todo]
Scalar -> boolean : token_to_value('$1').
Scalar -> null    : token_to_value('$1').

%% array           = "[" term { "," term } "]"
Array -> lsbrace Terms rsbrace : #{ type => array, value => '$2' }.


%% object          = "{" object-item { "," object-item } "}"
%% object-item     = ( scalar | ref | var ) ":" term
Object -> lcbrace ObjectItems rcbrace : todo_object. 
ObjectItems -> ObjectItem .
ObjectItems -> ObjectItems comma ObjectItem.

ObjectItem -> Scalar colon Term.
ObjectItem -> Ref colon Term.
ObjectItem -> var colon Term.

%% set             = empty-set | non-empty-set
%% non-empty-set   = "{" term { "," term } "}"
%% empty-set       = "set(" ")"

Set -> set rparen            : todo_set_empty.
Set -> lcbrace Terms rcbrace : todo_set.

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

token_to_value({Category, Pos}) ->
    #{ type => Category, position => Pos };
token_to_value({Category, Pos, Value}) ->
    #{ type => Category, value => Value, position => Pos }.

