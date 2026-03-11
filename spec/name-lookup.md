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


## Name Spaces

Any language construct that can contain publicly visible declarations is a name space. A comprehensive list:
1. Packages
2. Classes
3. Objects
4. Interfaces

## Parent/Outer Scopes

Every scope has a (possibly empty) list of outer scopes.

TODO

## Look up within a function/method body

Define the **DIRECTLY ENCLOSING CLASS** as the class/interface/object that directly encloses the function we are performing look up in.

### Restrictions

Whenever we are looking up the meaning of a name, certain kinds of restrictions apply to that search.

#### Name Space

During a chain name lookup like `foo.bar` it might have been concluded that `foo` is a namespace.
Then `bar`'s search is restricted to be inside this particular namespace.

#### Location of Search (Visibility)

All terms (including private ones) from the **DIRECTLY ENCLOSING CLASS**'s corresponding object or class/interface are visible.
For example, in a lookup in `class A`, `foo` from `object A { foo = ... }` is visible.

Additionally, protected and public members of the super classes of the **DIRECTLY ENCLOSING CLASS** are also visible.

#### Function Context

If the name being looked up is used in a function context, like `foo()`, 
then we must look for a function/method or a callable term (that has the `apply` method).

### Looking up a name

TODO

#### Method Body

Inside a method body, the directly enclosing scope is a BlockExpr.
The precise look-up order is defined

BlockExpr →* Method → **DIRECTLY ENCLOSING CLASS** → 


