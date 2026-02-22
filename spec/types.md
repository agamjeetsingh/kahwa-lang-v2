# Type System

We would use "<:" to denote "is subtype of" and ">:" to denote "is super type of".
As expected, the subtyping relation is reflexive (`T <: T` for all types `T`) and transitive (`T <: U` and `U <: V` implies `T <: V`).

## Type declarations

All of these declare a type in their scopes:
1. Class Declaration
2. Type Parameters declared in classes, functions or type definitions
3. Type definitions (typedef)

## Language defined types

There are certain types that are language defined. These are:
1. Nothing
2. Any
3. Char
4. Int
5. Long
6. Float
7. Double
8. Bool
9. String
10. Unit
11. TupleN for N = 2..32
12. FunctionN for N = 0..32

The 32 limit for tuples and functions is arbitrary but fixed.
Primitive types such as Char, Int etc. have standard meanings.

Nothing is the bottom most type and is the subtype of any other type. Formally:
```
Nothing <: T for all types T
```
Any is the top most type which every class implicitly extends.
```
T <: Any for all types T
```
The "void" keyword that exists in other languages like Java doesn't exist in Kahwa.
Functions that don't seem to return a concrete value (as will be detailed later) return Unit.

Kahwa has tuples in its grammar which are nothing but syntactic sugar for the internal TupleN classes.
It is a semantic error to have a tuple of size > 32 and tuples of size 0 or 1 should be illegal by grammar rules.
TupleN is generic with N type parameters. For example:
```
Tuple3[+T1, +T2, +T3]
```
Kahwa also has lambda functions in its grammar which are syntactic sugar for the internal FunctionN.
FunctionN refers to a function with N parameters. Similar to tuples, it is a semantic error to have > 32 parameters. 
FunctionN has N + 1 type parameters. For example:
```
Function2[+R, -T1, -T2]
```
This has return type R and parameter types T1 and T2. It is covariant in its return type and contravariant in all its parameter types.

## Class declarations

When a class is declared, it declares optionally a list of type parameters and a list of super classes.
The list of super classes impose a lot of rules, one of which is subtyping.

If a class `A` extends a class `B`, then `A <: B` in the type system. Generics can also appear when inheriting.
So `class A[T] : B[T]` implies that `A[T] <: B[T]` for all types `T`.
It is worth noting that variance does *not* play a rule in deciding subtyping of different classes. Variance only comes into
play when deciding the subtyping relationship between the same class but with different type parameters (like `A[T, U] and A[V, W]`)

The only types that a class is allowed to extend are other class symbols. So `class A[T] : T` is not allowed.
Cycles are not allowed in inheritance. 
So in case `B <: A` holds, it is illegal for `A` to extend `B`. This also means that `class A : A` is illegal.
However, a class is allowed to appear in one of the generic parameters of the classes it extends, like `A : B[A]`

## Type Parameters

At declaration, each type parameter has its own variance and possibly a list of type upper bounds and lower bounds.

### Variance

There are 3 types of variance:
1. Invariant
2. Covariant (+)
3. Contravariant (-)

These decide when `A[T] <: A[U]` holds for two types `T` and `U`.
If `A` is invariant in that type parameter, then `T = U`.
If it was covariant, then `T <: U` and if it was contravariant, then `T >: U`.

### Bounds

If a type parameter `T` has a (possibly empty) list of upper bounds `Us` and lower bounds `Ls` then this resolves to a
an upper bound `U` where `U` is the most specific super type of all the upper bounds in `Us` or the Any type if there is no upper bound
and to a lower bound `L` where `L` is the least specific sub type of all the lower bounds in `Ls` or the Nothing type if there is no lower bound.

This means that a type substitution `T := T'` is allowed if and only if `L <: T' <: U`.

### Cycles

Cycles are not allowed for the bounds declared by type parameters.
So for a type parameter `T`, an upper bound of `U` is not allowed if `U <: T`
and similarly a lower bound of `V` is not allowed if `T <: V`. An example of this situation:
```
def foo[T <: U, U <: T](): Unit = { ... }
```

## Type definitions

Typedefs define a type that aliases another type. They can have type parameters which themselves can have variance and bounds.
Typedefs can alias to another alias as well. The only restriction is that cycles aren't allowed. So these would be illegal:
```
typedef A = B;
typedef B = A;
typedef C[T] = D[String]
typedef D[T] = C[Bool]
```

## Type Coercion

Kahwa has type coercion for some numeric primitive types.

These coercions are permitted:
```
Int -> Char
Char -> Int
Int -> Long
Long -> Float
Float -> Double
Any -> Unit // Value discarding
```

