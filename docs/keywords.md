# Variables

`let` - variable declaration
```
let x = 1;
```

`const` - constant declaration
```
const y = 1;
```

# Conditional Statement

`if` - execute code if condition is true

`else` - execute otherwise
```
let a = 10;
if a > 5 {
    print("yep");
} else {
    print("nah");
}
```
# Conditional Expression

`ifv` - execute one of the branches, and return the value, must have `else`
```
let age = 45;

let role = 
    ifv a > 50 {
        "senior" // no semicolon = return from scope
    } else if a > 25 {
        "middle"
    } else {
        print("someone is young");
        "junior" //  all branches must 'return' something
    };

assert role == "middle";
```

# Loop Statements

`while` - execude code while condition is true

`break` - break from loop

`continue` - skip to next iteration
```
let a = 2;
while a < 5 {
    print(a); // 2 3 4
    a += 1;
}
```

`for` - loop over collection or range
```
for i : 0..10 {
    print(i); // 0 1 2 3 4 5 6 7 8 9
}
```

`nobreak` - executed when no `break` is hit
```
isPrime :: Int -> Bool // isPrime is defined elsewhere

for i : 14..=16 {
    if isPrime(i) {
        print(i);
        break;
    }
} nobreak {
    print("no prime found");
}

// "no prime found" is printed
```

`loop` - syntax sugar for `while`
```
loop (let i = 1; i < 35; i = i * 2 + 1) {
    print(i); // 1 3 7 15 31
}
```

### Loop Expressions

`forv`, `whilev`, `loopv` - same as their corresponding statements, but returns value

`breakv` - break from loop expression with value
```
let firstEven =
    forv i : [5, 7, 13] {
        if i % 2 == 0 {
            breakv i;
        }
    } nobreak {
        print("not found even number!");
        -1
    }
assert firstEven == -1;
// also prints "not found even number!"
```

# Functions

`fn` - function declaration

`return` - return from function
```
fn add(x, y) {
    return x + y;
}

fn multiply(x, y) {
    x * y // no semicolon at the end = return
}
```


# Types

`struct` - type declaration, mutable, passed by value

`pub` - mark field or method as public(only for linters and user convinience), you must never access private fields or methods from outside
```
struct Point {
    pub x;
    pub y;
}

let p = Point{1, 2};
print(p.x); // 1
````

`class` - class declaration, mutable, passed by reference

`data` - data declaration, immutable, passed by reference, all fields are public by default

`-----static-----` - denotes start of static methods

`-----impl-----` - denotes start of methods

`-----constraints-----` - constraints for fields

`watch` - runs on any change to given fields, all `watch`es are fired on creation

### Look examples for concrete code