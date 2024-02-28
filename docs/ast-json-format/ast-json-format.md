# Statements

Composite
```json
{
    "node": "composite",
    "body": [<statement>]
}
```

ExpressonStatement
```json
{
    "node": "expression",
    "value": <expression>
}
```

Let
```json
{
    "node": "let",
    "ident": <string>,
    "value": <expression>
}
```

Set
```json
{
    "node": "set",
    "ident": <string>,
    "value": <expression>
}
```

If
```json
{
    "node": "if",
    "cond": <expression>,
    "then": <statement>,
    "else": <statement>
}
```

While
```json
{
    "node": "while",
    "cond": <expression>,
    "body": <statement>
}
```

Return
```json
{
    "node": "return",
    "value": <expression>
}
```

Break
```json
{
    "node": "break"
}
```

Continue
```json
{
    "node": "continue"
}
```


# Expressions

Literal
```json
{
    "node": "literal",
    "value": <float> | <bool> | <string>
}
```

Variable
```json
{
    "node": "variable",
    "ident": <string>
}
```

Call
```json
{
    "node": "call",
    "what": <expression>,
    "args": [<expression>]
}
```

Binary
```json
{
    "node": "binary",
    "operator": <string>,
    "left": <expression>,
    "right": <expression>
}
```

FnDef
```json
{
    "node": "function",
    "args": [<expression>],
    "body": <statement>
}
```