# Examples

`invalid-syntax-examples` shouldn't parse, `ill-typed-examples` shouldn't typecheck.

Both `valid-syntax-examples` and `well-typed-examples` should parse, typecheck, and evaluate.

The reason `valid-syntax-examples` typecheck as well as parse is as a sanity check that they were actually parsed correctly.
