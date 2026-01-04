# YASME — Yeint's Assembler Engine

YASME is a small assembler core with a macro system, a multi-pass evaluation model, and a “stream” abstraction model that lets you perform complex evaluation logic on assembly-time.

## Build

This project is CMake-based.

```sh
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
```

## Run

### Assembling (driver)

There’s a CLI built to `build/yasme_cli` which can be used to assemble code.

### Golden test runner

A standalone runner exists (built as `yasme_test_runner`) that assembles an input `.asm` and compares the output against an expected file, however it is intended to be used as a part of the tests.

## Tests

The `tests/cases/` folder contains many small `.asm` programs with matching `.bytes` or `.err` expectations.
The simplest way to run the test suite is to use the provided `t` shell script.

> The full language & evaluation model is described in `docs.md`. 
