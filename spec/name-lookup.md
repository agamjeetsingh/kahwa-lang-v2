# Name Lookup

## Terms and Types

Kahwa has two distinct namespaces: a term name space and a type name space.

The following form the term name space:
1. Variables (local, fields, top-level variables)
2. Functions and Methods
3. Objects

The following form the type name space:
1. Class and Interface Declarations
2. Type Parameters declared in classes, functions or type definitions
3. Type definitions (typedef)

These two name spaces are completely independent and can share the same names.
For example, it is valid to have a top-level class (a type) named `A` and a top-level function named `A` in the same file.

In a type name space, all names must be unique. For example, you cannot have two classes with the same name in the same scope.

In a term name space, it *is* valid to have multiple terms with the same name. The exact rules are:

There may exist multiple functions and methods with the same name in the same scope OR
there exists a unique term with that name (whether an object or a variable).

Hence, there can be multiple functions or methods in the same scope (for the sake of overloading) but two variables of the same name are not allowed.
Neither is a function and a variable with the same name.

## Who defines a scope?

The following language constructs define their own scopes:
1. Files
2. Functions and Methods
3. Classes, Objects and Interfaces
4. Type definitions (for generic parameters)
5. Block Expressions

In general, anything that can contain a Declaration is a scope.

| Construct / Element            | Files | Functions | Methods | Block Expr. | Classes | Objects | Interfaces | Type Defs |
|:-------------------------------|:-----:|:---------:|:-------:|:-----------:|:-------:|:-------:|:----------:|:---------:|
| **Local Variable**             |   ✓   |     ✓     |    ✓    |      ✓      |         |         |            |           |
| **Fields**                     |       |           |         |             |    ✓    |    ✓    |     ✓      |           |
| **Functions**                  |   ✓   |           |         |             |         |         |            |           |
| **Methods**                    |       |           |         |             |    ✓    |    ✓    |     ✓      |           |
| **Classes/Objects/Interfaces** |   ✓   |           |         |             |    ✓    |    ✓    |     ✓      |           |
| **Type Definitions**           |   ✓   |           |         |             |         |         |            |           |
| **Generic Type Params**        |       |     ✓     |    ✓    |             |    ✓    |         |     ✓      |     ✓     |


## Parent/Outer Scopes

Every scope has a (possibly empty) list of outer scopes.

TODO

## Look up within a function/method body

Define the **DIRECTLY ENCLOSING CLASS** as the class that directly encloses the function we are performing look up in.

### Looking up a term


