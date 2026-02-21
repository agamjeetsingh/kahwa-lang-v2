# Grammar of Kahwa

## Notation

Rules use EBNF notation:
- `A B`   — sequence
- `A | B` — alternation
- `A?`    — zero or one
- `A*`    — zero or more
- `A+`    — one or more
- `(A B)` — grouping

---

## Lexical Rules

```
Digit     = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
Letter    = any ASCII letter or "_"
AlphaNum  = Letter | Digit

Character = any graphic ASCII character except "\", "'", and '"'
          | "\"
```

### Whitespace & Comments

```
Whitespace   = (" " | "\t" | "\n" | "\r")+
LineComment  = "//" (any character except "\n")*
BlockComment = "/*" (any character sequence not containing "*/") "*/"
```

Whitespace and comments are ignored between tokens.

### Identifiers

```
Identifier = Letter AlphaNum*
```

### Literals

```
EscapeSequence  = "\" ("n" | "t" | "r" | "0" | "\" | "'" | '"')

StringLiteral   = '"' (Character | EscapeSequence)* '"'
CharLiteral     = "'" (Character | EscapeSequence) "'"
IntegerLiteral  = Digit+
FloatLiteral    = Digit+ "." Digit+
BooleanLiteral  = "true" | "false"

Literal         = BooleanLiteral
                | FloatLiteral
                | IntegerLiteral
                | StringLiteral
                | CharLiteral
```

`FloatLiteral` takes priority over `IntegerLiteral` when a `.` follows digits.

---

## Names

```
QualifiedIdent = Identifier ("." Identifier)*
```

---

## Types

```
TypeArgList  = "[" TypeRef ("," TypeRef)* "]"
AtomTypeRef  = QualifiedIdent TypeArgList?
TupleTypeRef = "(" TypeRef ("," TypeRef)+ ")"
FuncParams   = "(" (TypeRef ("," TypeRef)*)? ")" | TypeRef
FuncTypeRef  = FuncParams "=>" TypeRef

TypeRef      = FuncTypeRef
             | TupleTypeRef
             | AtomTypeRef
             | "(" TypeRef ")"
```

`FuncTypeRef` takes priority when `=>` follows its `FuncParams`. A single parenthesised type `(T)` is treated as `T` (grouping), not a one-element tuple.

### Type Parameters

```
Variance      = "+" | "-"
UpperBounds   = "<:" TypeRef ("," TypeRef)*
LowerBounds   = ">:" TypeRef ("," TypeRef)*
TypeBound     = UpperBounds LowerBounds?
              | LowerBounds UpperBounds?
TypeParam     = Variance? Identifier TypeBound?
TypeParamList = "[" TypeParam ("," TypeParam)* "]"
```

---

## Modifiers

```
VisibilityMod = "public" | "private" | "protected"
ModalityMod   = "open" | "final" | "abstract"
Modifier      = VisibilityMod | ModalityMod | "override"
```

---

## Expressions

```
Expr = PrefixOp Expr
     | Expr BinaryOp Expr
     | Expr PostfixOp
     | Expr "(" (Expr ("," Expr)*)? ")"   // call
     | Expr "." Identifier                // member access
     | AtomExpr

PrefixOp  = "+" | "-" | "!" | "++" | "--"
PostfixOp = "++" | "--"
BinaryOp  = see precedence table below

AtomExpr = Literal
         | Identifier
         | VariableDecl
         | "(" Expr ")"
         | TupleExpr
         | BlockExpr
         | IfExpr
         | WhileExpr
         | LambdaExpr
         | "break"
         | "continue"

TupleExpr = "(" Expr "," Expr ("," Expr)* ")"
BlockExpr = "{" Statement* "}"
IfExpr    = "if" "(" Expr ")" BlockExpr ("else" (BlockExpr | IfExpr))?
WhileExpr = "while" "(" Expr ")" BlockExpr
```

### Operator Precedence

Operators listed from **lowest** to **highest** precedence. All binary operators are left-associative except assignment which is right-associative.

| Level | Operator(s)                                              | Associativity  |
|-------|----------------------------------------------------------|----------------|
| 1     | `=` `+=` `-=` `*=` `/=` `%=` `<<=` `>>=` `&=` `\|=` `^=` | right          |
| 2     | `\|\|`                                                   | left           |
| 3     | `&&`                                                     | left           |
| 4     | `\|`                                                     | left           |
| 5     | `^`                                                      | left           |
| 6     | `&`                                                      | left           |
| 7     | `==` `!=`                                                | left           |
| 8     | `<` `>` `<=` `>=`                                        | left           |
| 9     | `<<` `>>`                                                | left           |
| 10    | `+` `-`                                                  | left           |
| 11    | `*` `/` `%`                                              | left           |
| 12    | `+` `-` `!` `++` `--` (prefix)                           | right (unary)  |
| 13    | `++` `--` (postfix), call `(...)`, member `.`            | left (postfix) |

### Lambdas

```
LambdaParam     = Identifier (":" TypeRef)?
LambdaParamList = "(" (LambdaParam ("," LambdaParam)*)? ")"
LambdaExpr      = LambdaParamList "=>" (Expr | BlockExpr)
```

---

## Statements

```
Statement = Expr ";"
          | BlockExpr
```

`VariableDecl` (below) is an expression, so `val x = 5;` is a valid statement via `Expr ";"`.

---

## Declarations

```
Param         = Identifier ":" TypeRef ("=" Expr)?
ParamList     = "(" (Param ("," Param)*)? ")"
SupertypeList = ":" TypeRef ("," TypeRef)*
```

### Local Variable

```
VariableDecl = ("val" | "var") Identifier (":" TypeRef)? ("=" Expr)?
```

No trailing semicolon — as an expression inside a block it is terminated by the surrounding `Statement` rule.

### Field (class/object member)

```
FieldDecl = Modifier* ("val" | "var") Identifier (":" TypeRef)? ("=" Expr)? ";"
```

### Function

```
FuncDecl = Modifier* "def" Identifier TypeParamList? ParamList (":" TypeRef) BlockExpr
```

### Class

```
ClassMember = FieldDecl | FuncDecl | ClassDecl | ObjectDecl

ClassDecl   = Modifier* "class" Identifier TypeParamList? SupertypeList?
              "{" ClassMember* "}"
```

### Interface

```
InterfaceDecl = Modifier* "interface" Identifier TypeParamList? SupertypeList?
                "{" ClassMember* "}"
```

### Object

```
ObjectDecl = Modifier* "object" Identifier SupertypeList?
             "{" ClassMember* "}"
```

### Type Alias

```
TypedefDecl = Modifier* "typedef" Identifier TypeParamList? "=" TypeRef ";"
```

---

## File

```
TopLevelDecl = FuncDecl
             | ClassDecl
             | InterfaceDecl
             | ObjectDecl
             | TypedefDecl
             | FieldDecl

KahwaFile = TopLevelDecl*
```