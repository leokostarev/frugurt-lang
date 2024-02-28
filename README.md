Frugurt is interpreted language, with focus on functional and OOP.

Note: MOST OF THE FEATURES ARE NOT IMPLEMENTED YET!

The main purpose of Frugurt is to present different approach to OOP, than in other languages like Python or JavaScript.

Example
```
let sq = fn(i) {
    return (i) * (i);
};

(print)((sq)(7)); // prints 49

```
Note: the heavy use of parentheses is not because thay are special syntax, but because I am bad at making parsers. :sob:


My main goal is to make objects strictly typed (not variables!).

All types have fixed schema, that means:
- All fields must be declared at once
- Any other fields can never be declared

Also, there are 3 flavors of types:
- struct - mutable, passed by value
- classs - mutable, passed by reference
- record - immutable, passed by reference

Also, there is builtin data validation, using "watches"

Example 
```
struct Vector {
    x;
    y;

    fn new(x, y) {
        return Vector {
            x = x,
            y = y,
        };
    }

    -----impl-----

    fn add(v1, v2) {
        return Vector {
            x = v1.x + v2.x,
            y = v1.y + v2.y,
        };
    }

    -----constraints-----

    watch (x) {
        if (x) < (1000) {
            x = 1000;
        }
        if (x) > (1000) {
            x = 1000;
        }
    }

    watch (y) {
        if (y) < (1000) {
            y = 1000;
        }
        if (y) > (1000) {
            y = 1000;
        }
    }
}


operator <+> (v1 : Vector, v2 : Vector) { // you can define ANY operator name you want!
    v1.add(v2) // no semicolon = return
}

let v1 = Vector.new(1, 2);
let v2 = Vector.new(3, 4);
let v3 = v1 <+> v2;

print(v3.x, v3.y); // prints 4 6

```
Note: `-----constraints-----` and `-----impl-----` are keywords, you can find full keyword specification in [keywords.md](docs/keywords.md)


# THE FEATURES THAT HAVE BEEN DONE:
- keywords: fn, if, while, return, break, continue
- types: int, bool
- concepts: functions, control flow
- fuctions: print