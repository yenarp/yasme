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

## Label coordinates

As mentioned above, every stream maintains two counters:
* `@$` — **stream offset** (byte index inside the current stream)
* `$` — **memory address** (the current “org space” address)

Emission (`db/dw/dd/dq`) advances both counters by the number of bytes emitted **in that stream**.

`org expr` sets **only** the `$` counter of the *current* stream. It does not affect `@$`.

When you define a label:
```asm
foo:
```

the assembler records:
* `@$foo` = value of `@$` at that point (stream offset)
* `$foo`  = value of `$`  at that point (memory address)

This is true for output and for virtual streams (each stream has its own pair of counters).

## Converting labels to numeric values

### Explicit operators

* `$name`  → numeric **memory address** of `name`
* `@$name` → numeric **stream offset** of `name`

Examples:

```asm
addr = $foo
off  = @$foo
dd $foo        ; explicitly emit address
dd @$foo       ; explicitly emit stream offset
```

### Default numeric coercion

In any **numeric-value context** (arithmetic, comparisons, `db/dw/dd/dq`, instruction immediates, etc.), a *name expression* that refers to a label is coerced as:
* `foo` in numeric context ≡ `$foo`

So:

```asm
dd foo         ; emits $foo
x = foo + 4    ; uses $foo + 4
```

If you want the stream offset in arithmetic/emission, you must say so:

```asm
x = @$foo + 4
dd @$foo
```

## Converting labels to symbolic variables

Assignment has the standard “RHS denotes a name” rule where:

```asm
p = foo
```

creates a **symbolic variable** `p` whose pointee is the symbol `foo` (label or variable).

## Forward refs and multipass

`$foo` / `@$foo` are ordinary numeric expressions that may be **unresolved** until `foo:` is seen (or until later passes stabilize). They’re allowed anywhere numeric expressions are allowed, and they converge naturally across passes.

## Macro definition and call syntax

### Define

```asm
macro mov dst, src
  ; body
end macro
```

### Call

```asm
mov eax, 1
emit_opcode 0x90
```

### Parameters

* plain: expression (value mode when used normally)
* `name`: argument must denote a **name** (identifier or concat result)
* `ref`: argument must be an **identifier** and assignments inside macro write back to caller
* `tokens`: argument will be parsed as a token list

```asm
macro alias name new, name old
	new = old          ; symbolic var: new points to old
end macro

macro inc ref x
	x = x + 1
end macro
```

### Locals

```asm
macro foo a, b
	local t, i
	t = a + b
end macro
```

### Token list semantics

The `tokens` parameter type captures the macro argument as a token list rather than an expression tree.

#### Parameter rule

`tokens` parameters must be the last parameters in the macro signature.
A macro may have at most one `tokens` parameter.

Examples
```
macro m a, b, tokens rest
  ; ok
end macro

macro bad tokens t, x
  ; invalid, tokens must be last
end macro
```

#### What gets captured

The captured token list is exactly the token sequence of the argument text as written at the call site.
The list includes identifiers, numeric literals, string literals, operators, punctuation, and keywords as tokens.

Whitespace and comments are not tokens.
The token list is not parsed as an expression at capture time.
Argument splitting still uses commas at the top nesting level

```
call foo, 1+2, [eax, ebx], bar(1,2), baz
```

In this call, commas inside bracketed or parenthesized constructs belong to the token list and do not split macro parameters.

#### Eval

`eval` parses a token list as an expression and evaluates it in the current context.

Syntax:
```
eval out_variable, tokens_param_name
```

##### Semantics

- Parse tokens_param_name using the normal expression grammar.
- Evaluate the resulting expression in value mode.
- Assign the result to out_variable.
- If parsing fails, error at the token span.
- If evaluation is unresolved this pass, out_variable becomes unresolved like any other numeric assignment. 

Example:
```
macro imm8 tokens t
	eval x, t
	db x
end macro

imm8 1 + 2*3
```

#### Match

`match` compares a token list against a pattern and optionally binds sublists.

Syntax:
```
match tokens_param_name, pattern terminated by newline
	; match body
else ; optional
	; runs if the match fails
end match
```

##### Semantics

- `pattern terminated by newline` is lexed as a token pattern.
- If the token list matches the pattern, execute the match body.
- If it does not match, execute the `else` body if present.
- `end match` closes the construct.

##### Pattern elements

- Literal tokens in the pattern must match identical tokens in the input.
- `_` matches any single token.
- `...` matches any sequence of tokens, including empty.
- `{name}` binds a sequence of tokens matched at that position to a new local token-list variable `name`.
- A binding may be empty only if it is matched by `...` or by an explicitly empty position.

Example:
```
macro split_add tokens t
	match t, {lhs} + {rhs}
		eval a, lhs
		eval b, rhs
		dd a
		dd b
	else
		; not an addition form
		eval x, t
		dd x
	end match
end macro
```

##### Binding scope

- `{name}` bindings exist only inside the matching `match` body.
- Bindings are token-list variables and may be fed into `eval` or nested `match`.

##### Unresolved values

- `match` is purely lexical and never depends on resolution.
- Only `eval` introduces evaluation and can become unresolved.

### Macro diagnostics: `error ... end error`

Macros can emit rich, user-facing diagnostics during expansion using an `error` block. This is intended for “checked” macros that validate argument shapes and context and can fail with a custom messages.

> Note: `error` is **only** valid inside macro bodies. Using it at top level is a parse/expand error.

### Syntax

```asm
error <message_expr> [, <tokens_binding>]
  note        <message_expr> [, <tokens_binding>]
  help        <message_expr> [, <tokens_binding>]
  suggestion  <message_expr> [, <tokens_binding>]
  ref         <message_expr> [, <tokens_binding>]
end error
```

#### Header

* `<message_expr>` is required.
* `[, <tokens_binding>]` is optional; if present it selects the primary span for the diagnostic.

#### Items

* Any number of items may appear before `end error`.
* Each item is one line (terminated by newline).
* `note/help/suggestion/ref` are only recognized inside an `error` block.

### Message rules

Both the header message and each item message must expand to a string literal expression at macro-expansion time.

That means:

* `"literal"` works.
* A macro parameter or local that expands to a string literal works.
* Non-string expressions are rejected and produce an error diagnostic (and the macro error message falls back to a generic string).

### Control-flow effect

Emitting an `error` diagnostic aborts the remainder of the current macro expansion. Expansion then resumes at the macro call site.
This makes `error` behave like a structured “fail fast” for macros.

### Example

```asm
macro split_add tokens t
	match t, {lhs} + {rhs}
		eval a, lhs
		eval b, rhs
		dd a
		dd b
	else
		error "expected form: <lhs> + <rhs>", t
			note "the whole argument is highlighted", t
			help "write something like: 1 + 2"
			suggestion "if you need commas, wrap with parentheses"
		end error
	end match
end macro
```

### Example with precise subspans

```asm
macro require_ident_plus_ident tokens t
	match t, {a} + {b}
		; ok
	else
		error "expected: <ident> + <ident>", t
			note "left side should be an identifier", a
			note "right side should be an identifier", b
		end error
	end match
end macro
```

Here, `a` and `b` are token-list bindings produced by `match`, so they can be used to point notes at the exact subranges that failed expectations.

## Stream interaction primitives

### Load from a stream into a numeric variable

```asm
load db x, stream_name, offset_expr   ; u8  -> x
load dw x, stream_name, offset_expr   ; u16 -> x
load dd x, stream_name, offset_expr   ; u32 -> x
load dq x, stream_name, offset_expr   ; u64 -> x
```

Example:

```asm
load db opcode, scratch, 0
```

### Store to a stream at an offset

```asm
store db stream_name, offset_expr, value_expr
store dw stream_name, offset_expr, value_expr
store dd stream_name, offset_expr, value_expr
store dq stream_name, offset_expr, value_expr
```

Example:

```asm
store dd scratch, 16, NUMERIC_VARIABLE_HOLDING_LABEL_STREAM_ADDRESS
```

### Stream metadata semantics

```asm
stream_emit src_stream        ; append src_stream bytes to current stream
stream_clear stream_name
stream_sizeof n, stream_name         ; n = stream length in bytes
```

## Patching — simplified store <!-- Yes this is an em dash -->

```asm
patch db label, value_expr
patch dw label, value_expr
patch dd label, value_expr
patch dq label, value_expr
```

Example:

```asm
jmp32:
  db 0xe9
disp32:
  dd 0              ; placeholder

; later (same pass or later passes)
patch dd disp32, target - (disp32 + 4)
```

## Assembly time Control flow

### If / else

```asm
if expr
  ...
else
  ...
end if
```

### Repeat / while

```asm
repeat count_expr
  ...
end repeat

while expr
  ...
end while
```

### For (numeric)

```asm
for i = start, end ; inclusive
  ...
end for

for i = start, end, step
  ...
end for
```

### For (string chars)

```asm
for ch in str_expr
  ; ch is a 1-char string
end for
```

### Break/continue

```asm
break
continue
```

### Defer rule

* If a control-flow condition/bound is **unresolved**, the whole block **does nothing this pass** (no partial emission/patching). Next pass it re-attempts.
* `patch` and `store` are allowed each pass; they simply overwrite the same bytes again.
