# Language description

A statically typed imperative language
with syntax similar to C, C++, and, Java.
Basic statements (loops, conditionals,
variable declaration itp.)
are semantically similar to their counterparts in those languages.

# Simple types

- int
- bool
- string
- void (for functions)

# Complex types

## Arrays

- constant length arrays of int, bool, string (no arrays of arrays or arrays of tuples)
- syntax similar to Javy
- len() function
- can be initialized with a literal or length and default value
- item access via square brackets
- items can also be updated via square brackets

```
int[] a = {1,2,3};
int[] b = new int[10];

int c = a[0];
b[0] = 1;
int e = len(a);
```

## Nested tuples

- nested tuples of int, bool, string (arrays are not allowed)
- variable assignment from tuple, tie, inspired by std::tie from C++,
  (but nested assignment is allowed, unlike in C++)
- item access via square brackets
- because of static control, access index must be a constant,
  not an expression evaluated at runtime

```
<int, bool, string> x = new tuple(1, true, "string");

int i; bool a; string s;
tie (i, b, s) = y;

bool d = y[1];
```

# Functions

arbitrarily nested functions with static name binding (similar to Pascal)

# Errors

error handling, including printing the line number in code where the error occurred

# Implementation details

Some implementation details are described in the `IMPLEMENTATION_DETAILS.md`.


# Examples

Correct program examples are in the `good` folder.
Incorrect program examples are in the `bad` folder.

