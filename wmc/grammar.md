# Windmill Grammar

## Top Level Items

```
Program = Item *

Item =
  | FunctionDef
  | StructDef
  | VariantDef
  | InterfaceDef
  | Implementation
  | StaticDecl
```

### Functions

```
Function = FN IDENTIFIER Generics? "(" FunctionArgs? ")" FunctionReturnType? BlockExpression

Generics = "<" Identifier ("," Identifier)* ","? ">"

FunctionArgs = FunctionArg ("," FunctionArg)* ","?

FunctionArg = IDENTIFIER ":" Type

FunctionReturyType =
  | epsilon
  | ":" Type

BlockExpression = "{" Expression "}"
```

### Types

```
Type =
  | Path ParameterTypes? Array?
  | TupleOrFnType
  | AnonStructType

Path = IDENTIFIER ("::" IDENTIFIER)*

ParameterTypes = "<" Type ("," Type)* ","? ">"

Array = "[]"

TupleOrFnType = TupleType FunctionReturnType?

TupleType =
  | "(" ")"
  | "(" Type ("," Type)* ","? ")"

AnonStructType =
  | "{" "}"
  | "{" AnonStructField ("," AnonStructField)* ","? ")"

AnonStructField = IDENTIFIER ":" Type

How to tell difference between function that returns an array and an array of functions?

(T, V) -> S[][]
(T, V) -> S[]

[(T, V) => S]
(T, V) => [S]
```

### Expressions

```
Expression =
  | LiteralExpression
  | ParenthesizedExpression
  | ArrayExpression
  | IndexExpression
  | TupleExpression
  | CallExpression
  | MethodCallExpression
  | FieldAccessExpression
  | ClosureExpression
  | IfExpression
  | IfLetExpression
  | WhileExpression
  | WhileLetExpression
  | ForExpression
  | ForInExpression
  | ImplicitMethodCallExpression
  | CurryExpression
  | ReturnExpression

LiteralExpression = NUMBER | STRING | CHARACTER

ParenthesizedExpression = "(" Expression ")"

ArrayExpression = "[" CommaList<Expression>? "]"

IndexExpression = Expression "[" Expression "]"

TupleExpression =
  | "(" ")"
  | "(" (Expression ",")+ Expression? ")"

CallExpression = Expression "(" CommaList<Expression>? ")"

MethodCallExpression = Expression "." IDENTIFIER ParameterType? "(" CommaList<Expression>? ")"

FieldAccessExpression = Expression "." IDENTIFER

ClosureExpression = ???

IfExpression

IfLetExpression

WhileExpression

WhileLetExpression

ForExpression

ForInExpression

ImplicitFieldExpression = "&" "." IDENTIFIER

ImplicitMethodCallExpression = "&" "." IDENTIFIER "(" CommaList<Expression>? ")"

CurryExpression = "~" Expression "(" CommaList<Expression | CurryArg>* ")"

ReturnExpression = RETURN Expression?

CurryArg = "~" DECIMAL
```
