%%
%%

Nonterminals
    Module
    Package
    OptImports
    Imports
    Import
    Policy
    Term
    Terms
    OptEqTerm
    OptRules
    OptImport
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
    ExprCall
    Literal
   
 
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

.


Rootsymbol 
    Module.

Module -> Package OptImports Policy.

Package -> package Ref.

OptImports -> '$empty'.
OptImport -> Imports.

Imports -> Import.
Imports -> Imports Import.

Import -> import Package.
Import -> import Package as var.

Policy -> OptRules.

OptRules -> '$empty'.
OptRules -> Rules.

Rules -> Rule.
Rules -> Rules Rule.

Rule -> default RuleHead RuleBody.
Rule -> RuleHead.

RuleHead -> var lparen RuleArgs rparen rsbrace Term lsbrace.
RuleHead -> var OptRuleArgs OptRuleTerm OptEqTerm.

OptRuleArgs -> '$empty'.
OptRuleArgs -> lparen RuleArgs rparen.

RuleArgs -> Terms.

Terms -> Term.
Terms -> Terms Term.

OptRuleTerm -> '$empty'.
OptRuleTerm -> lsbrace Term rsbrace.

RuleBody -> RuleElse lcbrace Query rcbrace. 
RuleBody -> lcbrace Query rcbrace. 
 
RuleElse -> else OptEqTerm.

%% query           = literal { ( ";" | ( [CR] LF ) ) literal }
Query -> Literal.
Query -> Query Literal.
Query -> Query semi_colon Literal.

OptEqTerm -> '$empty'. 
OptEqTerm -> eq_oper Term.  

% Term -> var.
Term -> Ref.
Term -> Scalar.
Term -> Array.
Term -> Object.
Term -> Set.
Term -> ArrayCompr.
Term -> ObjectCompr.
Term -> SetCompr.

Ref -> var RefArgs.
Ref -> Array RefArgs.
Ref -> Object RefArgs.
Ref -> Set RefArgs.
Ref -> ArrayCompr RefArgs.
Ref -> ObjectCompr RefArgs.
Ref -> SetCompr RefArgs.
Ref -> ExprCall RefArgs.

RefArgs -> '$empty'.
RefArgs -> RefArg.
RefArgs -> RefArgs RefArg.

RefArg -> RefArgDot.
RefArg -> RefArgBrack.

RefArgDot -> dot var.

RefArgBrack -> lsbrace var rsbrace.
RefArgBrack -> lsbrace Scalar rsbrace.
RefArgBrack -> lsbrace Array rsbrace.
RefArgBrack -> lsbrace Object rsbrace.
RefArgBrack -> lsbrace Set rsbrace.

%% literal         = ( some-decl | expr | "not" expr ) { with-modifier }
Literal -> string.

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
ObjectCompr -> lcbrace ObjectItem rcbrace.
ObjectCompr -> lcbrace RuleBody rcbrace.

%% array-compr     = "[" term "|" rule-body "]"
ArrayCompr -> lsbrace Term rsbrace.
ArrayCompr -> lsbrace RuleBody rsbrace.

%% set-compr       = "{" term "|" rule-body "}"
SetCompr -> lcbrace Term rcbrace.
SetCompr -> lcbrace RuleBody rcbrace.

%% expr-call       = var [ "." var ] "(" [ expr { "," expr } ] ")"
ExprCall -> var. %% [TODO]
