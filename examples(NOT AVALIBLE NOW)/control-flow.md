```
let a = 5;

if a > 4 {
    print("yes");
} else {
    print("no");
}
```

```

let a = 10;

while a > 3 {
    a -= 1;
}
assert a == 3;
```

```

let a = 4;

for (let i = 0; i < a; i += 1) {
    print(i); // prints 0 to 3
}

```

```
let a = 9;

each i : 0..a {
    print(i); // prints 0 to 8
}

each i : 0..=a {
    print(i); // prints 0 to 9
}
```

```
let a = 5;

print(
    ifv a > 4 {
        "yes"
    } else {
        "no"
    }
); // prints "yes"
```

```
print(
    for (let x = 7; x < 10; x += 2) {
        if x % 5 == 0 {
            breakv x; // special keyword to break from expression
        }
    } nobreak {
        -1
    }
); // prints -1
   // if x was 1 it would print 5
```

```
let a = 1;

if a > 0 {
    let b = 9;
}
print(b); // raises error "Name is not declared"

let x = 1;
let x = 2; // raises error "Name is redeclared"
```
