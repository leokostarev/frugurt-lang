All types have fixed schema, that means
- All fields must be declared at once
- Any other fields can never be declared
- Methods are part of type, not object

Each object holds inforamtion of it's class, and it can not be altered

Frugurt lang has builtin data validation, using `watch`-es


Control flow statements have their expression counterpart, denoted by `v` suffix.
- `if` -> `ifv`
- `for` -> `forv`
- `while` -> `whilev`
- `each` -> `eachv`

There are 2 types of names:
- variables
- operators

Variables are just primitives, values of builtin and user types, and the types themselves.

Operators are just binary functions, that are strict in their arguments types
