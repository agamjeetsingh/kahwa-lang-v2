# Kahwa

**A statically-typed, expression-oriented language targeting the JVM — implemented in Scala 3.**

---

## What is Kahwa?

Kahwa is a compiled, statically-typed language with Scala-like syntax and Java-like semantics. It features a rich type system with generics, variance annotations, type inference, and a `Nothing`/`Any` type hierarchy. The compiler is implemented in Scala 3 and is built around [Parsel](#parsel--a-custom-parser-combinator-library), a parser combinator library written from scratch.

A work-in-progress language specification lives in [`spec/`](spec/).

---

## Language at a Glance

```
class Stack[+T] {
  def push(item: T): Unit { /* ... */ }
  def pop(): T { /* ... */ }
}

def factorial(n: Int): Int {
  if (n <= 1) { 1; } else { n * factorial(n - 1); };
}

val double: (Int) => Int = (x: Int) => x * 2;
```

Key features:

- **Classes, interfaces, objects, and type aliases** (`typedef`)
- **Generics** with variance annotations (`+` covariant, `-` contravariant) and upper/lower type bounds
- **Type inference** for local variables
- **First-class functions and lambdas**, desugared to `FunctionN[R, T1, ...]`
- **Tuples**, desugared to `TupleN[T1, T2, ...]`
- **Expression-oriented**: `if`, `while`, and block expressions all evaluate to a value
- **`Nothing`/`Any` type hierarchy** with numeric coercion (e.g. `Int` promotes to `Float` automatically)
- Full operator precedence table with 13 levels

See [`spec/grammar.md`](spec/grammar.md) and [`spec/types.md`](spec/types.md) for the full specification.

---

## Implementation Highlights

### Parsel — A Custom Parser Combinator Library

Rather than reaching for an off-the-shelf library like [Parsley](https://github.com/j-mie6/parsley), I wrote **Parsel**: a parser combinator library built on top of a custom tokeniser.

The motivation was **error recovery**. How a parser recovers from a syntax error is an opinionated problem with several defensible answers — panic-mode resynchronisation, phrase-level repair, explicit error productions — so general-purpose combinator libraries tend to leave it out rather than impose one. I wanted to take my own approach to it, and to learn by building the library rather than consuming one.

Parsel supports:

- Composition via `~`, `<~`, `~>`, `map`, and `flatMap`
- `or` with discriminator-based dispatch for efficient, non-backtracking alternation
- `sepBy`, `list`, `optional`, and `delay` for common patterns
- A `precedence` combinator implementing Pratt-style precedence climbing, with support for prefix, postfix, left-associative, right-associative, and non-associative operators

#### Error Recovery

Every parser returns `(Option[A], Input, Iterable[Error])` — failure is a value, not an exception, so one bad construct never unwinds the whole parse.

- `sync` advances the input to the next **safe point**: a token where parsing can plausibly restart.
- The safe-point predicate is a **contextual parameter** (`using SafePointFunction[Token]`), so recovery granularity is determined lexically by where you are in the grammar, without threading it through every combinator. At file level the parser resynchronises to `class` / `typedef` / a modifier; inside a block, `parseBlock` locally overrides the given so a broken statement resynchronises to the next `;` or `}` instead of skipping the rest of the class.
- `commit` marks a point of no return — past it, a failure resynchronises rather than backtracking, which keeps a malformed construct from being silently misreported as a different one. (`parseTypedefDecl` commits once the `typedef` keyword is consumed.)
- `fully` guarantees forward progress — if `sync` fails to advance past a position already tried, it force-skips one token — so a malformed file can never hang the parser, and it always returns a partial AST alongside the accumulated errors.

The result: a file containing several syntax errors yields *all* of them plus a usable AST, instead of stopping at the first.

### Multi-Phase Semantic Analysis with Error Recovery

The semantic analyser runs in a sequence of passes over the AST, each with a precisely scoped responsibility. All phases share a `SemanticContext` and accumulate diagnostics rather than failing on the first error, much like production compilers such as GCC or Clang.

| # | Phase                            | Responsibility                                                                      |
|---|----------------------------------|-------------------------------------------------------------------------------------|
| 1 | **Access Compressor**            | Normalises chained member access expressions                                        |
| 2 | **Name Declaration**             | Declares all top-level, class-level, and function-level symbols                     |
| 3 | **Scope Generation**             | Attaches an enclosing scope to every AST node                                       |
| 4 | **Type Reference Qualification** | Resolves all type mentions to their declared symbols according to scope             |
| 5 | **Typedef Cycle Detection**      | Detects illegal cycles in type alias definitions *(WIP)*                            |
| 6 | **Typedef Replacement**          | Substitutes all type aliases with their canonical types *(WIP)*                     |
| 7 | **Type Checking & Inference**    | Checks and infers types for all expressions, resolves local name references *(WIP)* |

---

## Current Progress

| Component                             | Status      |
|---------------------------------------|-------------|
| Tokenisation                          | Done        |
| Parsing (via Parsel)                  | Done        |
| Name Declaration                      | Done        |
| Scope Generation                      | Done        |
| Type Reference Qualification          | Done        |
| Type Checking & Inference             | In Progress |
| Typedef Cycle Detection & Replacement | In Progress |
| Code Generation (JVM bytecode)        | Planned     |

---

## Kahwa v1 (C++)

Kahwa v1 was [implemented in C++](https://github.com/agamjeetsingh/kahwa-lang) with a hand-written recursive descent parser and a Java-like grammar. As the type system grew more complex, C++ became increasingly painful: no pattern matching, no algebraic data types, and the lack of a type system that could mirror the compiler's own abstractions created significant friction.

Kahwa v2 migrates to Scala 3, which is a much more natural fit for compiler construction. Pattern matching, case classes, and Scala's own type system map cleanly to compiler internals. The grammar was also revised from Java-like to Scala-like. The result is a significantly more compact and maintainable codebase.

---

## Building & Running

**Prerequisites**: [sbt](https://www.scala-sbt.org/) and a JDK.

```bash
# Compile
sbt compile

# Run on a source file
sbt "run myfile.kw"

# Run tests
sbt test
```