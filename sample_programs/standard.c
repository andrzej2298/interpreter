// types, variables
int i;
bool b;
string s;

// literals
int i' = 1;
bool b' = true;
string s' = "string";

// arithmetic, assignments
i = i' + 10 - 2 * 8;

// comparison operators
b = 7 < 8;

// standard output
print("Hello, world!\n");

// conditionals and loops
if (true && b') {}
else {}

while (false) {
    i++;
}

// functions, pass by reference/value
int f(int &i, int j) {
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
