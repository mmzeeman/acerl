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
    SomeDecl
    OptWithModifiers
    WithModifiers
    WithModifier
    Vars
    InfixOperator
.


Terminals
    as
    default
    package
    import
    var
    eq_oper
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
    relation_operator    
.


Rootsymbol Module.
Endsymbol '$eof'.

Module -> Package.
Module -> Package Imports.
Module -> Package Policy.
Module -> Package Imports Policy.

Package -> package Ref.

Imports -> Import.
Imports -> Imports Import.

Import -> import Package.
Import -> import Package as var.

Policy -> Rules.

Rules -> Rule.
Rules -> Rules Rule.

Rule -> default RuleHead RuleBody.
Rule -> RuleHead.

RuleHead -> var OptRuleArgs OptRuleTerm OptEqTerm.

OptRuleArgs -> '$empty'.
OptRuleArgs -> lparen RuleArgs rparen.

RuleArgs -> Terms.

Terms -> Term.
Terms -> Terms comma Term.

OptRuleTerm -> '$empty'.
OptRuleTerm -> lsbrace Term rsbrace.

OptEqTerm -> '$empty'. 
OptEqTerm -> eq_oper Term.  

RuleBody -> RuleElse lcbrace Query rcbrace. 
RuleBody -> lcbrace Query rcbrace. 
 
RuleElse -> else OptEqTerm.

%% query           = literal { ( ";" | ( [CR] LF ) ) literal }
Query -> Literal.
Query -> Query Literal.
Query -> Query semi_colon Literal.


% Term -> var. 
Term -> Ref.
Term -> Scalar.
Term -> Array.
Term -> Object.
Term -> Set.
Term -> ArrayCompr.
Term -> ObjectCompr.
Term -> SetCompr.

Ref -> var.
Ref -> var RefArgs.

RefArgs -> RefArg.
RefArgs -> RefArgs RefArg.

RefArg -> RefArgDot.
RefArg -> RefArgBrack.

RefArgDot -> dot var.

RefArgBrack -> lsbrace var rsbrace.  %% Includes "_"
RefArgBrack -> lsbrace Scalar rsbrace.
RefArgBrack -> lsbrace Array rsbrace.
RefArgBrack -> lsbrace Object rsbrace.
RefArgBrack -> lsbrace Set rsbrace.

%% literal         = ( some-decl | expr | "not" expr ) { with-modifier }
Literal -> SomeDecl OptWithModifiers.
Literal -> Expr OptWithModifiers.
Literal -> not Expr OptWithModifiers.

OptWithModifiers -> '$empty'.
OptWithModifiers -> WithModifiers.

WithModifiers -> WithModifier.
WithModifiers -> WithModifiers WithModifier.

WithModifier -> with Term as Term.

%% some-decl       = "some" var { "," var }
SomeDecl -> some Vars.

Vars -> var.
Vars -> Vars comma var.

%% scalar          = string | NUMBER | TRUE | FALSE | NULL
Scalar -> string.
% Scalar -> number. [todo]
Scalar -> boolean.
Scalar -> null.

%% array           = "[" term { "," term } "]"
Array -> lsbrace Terms rsbrace.


%% object          = "{" object-item { "," object-item } "}"
%% object-item     = ( scalar | ref | var ) ":" term
Object -> lcbrace ObjectItems rcbrace. 
ObjectItems -> ObjectItem.
ObjectItems -> ObjectItems comma ObjectItem.

ObjectItem -> Scalar colon Term.
ObjectItem -> Ref colon Term.
ObjectItem -> var colon Term.

%% set             = empty-set | non-empty-set
%% non-empty-set   = "{" term { "," term } "}"
%% empty-set       = "set(" ")"

Set -> set rparen.
Set -> lcbrace Terms rcbrace.

%% object-compr    = "{" object-item "|" rule-body "}"
ObjectCompr -> lcbrace ObjectItem pipe RuleBody rcbrace.

%% array-compr     = "[" term "|" rule-body "]"
ArrayCompr -> lsbrace Term pipe RuleBody rsbrace.

%% set-compr       = "{" term "|" rule-body "}"
SetCompr -> lcbrace Term pipe rcbrace.


% expr            = term | expr-call | expr-infix | expr-every
Expr -> Term.
Expr -> ExprBuiltIn.
Expr -> ExprInfix.

% expr-built-in   = var [ "." var ] "(" [ term { , term } ] ")"
ExprBuiltIn -> Vars lparen rparen.
ExprBuiltIn -> Vars lparen Terms rparen.

% expr-infix      = [ term "=" ] term infix-operator term
ExprInfix -> Term InfixOperator Term.

%% infix-operator  = bool-operator | arith-operator | bin-operator
%% bool-operator   = "=" | "!=" | "<" | ">" | ">=" | "<="
%% arith-operator  = "+" | "-" | "*" | "/"
%% bin-operator    = "&" | "|"
InfixOperator -> relation_operator. % todo.



