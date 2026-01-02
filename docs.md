# YASME IR and Evaluation Model

This document describes the design of YASME’s assembler core.

## Expressions

Expressions are trees with spans:

- literals: integers and strings
- identifiers (names)
- unary operators
- binary operators

Key operators supported in the IR:

- arithmetic and bitwise: `+ - * / % ~`
- comparisons: `== != < <= > >=`
- shifts: `<< >>`
- logical: `&& || !`
- **name operator**: `@`
- **concatenation operator**: `#`

Two evaluation modes exist conceptually:

- **value mode**: “what is the numeric/string value of this expression?”
- **name mode**: “what identifier name does this expression denote?”

Name mode is needed for symbolic variables, dereferencing, and concatenation.

## Variable Model

YASME distinguishes three kinds of variables:

### Numeric variable
Holds a number.

Created by assignment:
```
x = 10
```

### String variable
Holds a string.

Created by assignment:
```
s = "hello"
```

### Symbolic variable
A symbolic variable is a reference to another identifier (variable or label).

Created by assignment when the RHS denotes a name:
```
p = target
```

The symbolic variable’s “pointee” is the identifier `target`. It can be:
- dereferenced to obtain the pointee’s value
- inspected with `@` to obtain the pointee’s name

## `@` & `#` operator

`@expr` asks for the **name** of what `expr` denotes.
- If `expr` is a symbolic variable, `@expr` yields the identifier name of its pointee.
- If `expr` already denotes a name expression (identifier / concat result), `@expr` will produce the name of the variable in identifier form.

Example:
```
numeric = 23
string = "Hello_World"

hello_numeric_string = 23 ; actually a numeric variable
hello_23_Hello_World = 24

symbolic = hello_#@numeric#@string ; symbolic points to a numeric variable holding `23`
symbolic2 = hello_#numeric#string ; symbolic points to a numeric variable holding `24`
```

## `=` assignment vs `define`

### Assignment (`=`)
- Creates **numeric**, **string**, or **symbolic** variable depending on the RHS.
- It is allowed to change across passes as expressions become known.
- This is the main vehicle for multipass stabilization.

### `define`
- Creates a **constant** binding.
- The value must be a **numeric constant** or a **string constant**.
- It cannot be symbolic.
- It cannot depend on unresolved symbols.

## Addressing and `org`

`org <expr>` sets the current assembly virtual address space queriable by `$`.
- This should only be used for instruction memory operand addressing and in-memory localization of labels.
- This should *not* be used for in-file locations.

## `virtual ... end virtual`

`virtual` starts a new **virtual stream** scope:
- bytes emitted inside do **not** go into the output binary
- bytes are stored in a separate virtual stream, intended for macro read/write operations
- the current offset in a virtual stream can be queried by `@$`
- the actual file output is also considered an implicit virtual stream
- `virtual <expr>` may evaluate to a name that becomes the stream identifier

## `postpone ... end postpone`

`postpone` defers assembly of its body.

Two modes exist:

### `postpone ... end postpone`
- The body assembles at the end of the program **each pass**
- This allows the postponed output to participate in convergence naturally

### `postpone ! ... end postpone`
- The body assembles **once**, after the assembler detects stabilization of symbolic and numeric state
- This is useful for “finalization” blocks that must not affect earlier convergence steps

## Data directives: `db`, `dw`, `dd`, `dq`

These are the first emission primitives and are intentionally simple.

- `db` — emit 1-byte units (u8)
- `dw` — emit 2-byte units (u16)
- `dd` — emit 4-byte units (u32)
- `dq` — emit 8-byte units (u64)

Syntax:
```
db expr, expr, ...
dw expr, expr, ...
dd expr, expr, ...
dq expr, expr, ...
```
