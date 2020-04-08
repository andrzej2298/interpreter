// TYPES, VARIABLES
int i;
bool b;
string s;

// LITERALS
int i' = 1;
bool b' = true;
string s' = "string";

// ARITHMETIC, ASSIGNMENTS
i = i' + 10 - 2 * 8;

// COMPARISON OPERATORS
b = 7 < 8;

// STANDARD OUTPUT
print("Hello, world!\n");

// CONDITIONALS AND LOOPS
if (true && b') {}
else {}

while (false) {
    i++;
}

// FUNCTIONS, PASS BY REFERENCE/VALUE
int f(int &i, int j) {
    i = 10;
    return i + j;
}

int fact(int i) {
    if (i <= 0) {
        return 0;
    }
    else {
        return i * fact(i - 1);
    }
}

int a = f(i, 1);
