// NESTED FUNCTION DECLARATIONS
// e() == 0
int e() {
    int x = 0;

    int f() {
        return x;
    }

    int g() {
        int x = 1;
        return f();
    }

    return g();
}

// ARRAYS
int[] a = {1, 2, 3, 4};

int sum(int[] array) {
    int i = 0, sum = 0;
    while (i < len(array)) {
        sum = sum + array[i];
        i++;
    }
    return sum;
}

int[] b = new int[10]; // default 0
string[] c = new string[10]; // default ""
bool[] d = new bool[10]; // default false

// TUPLES
<int, bool, string> x = new tuple(1, true, "string");
<int, <int, bool, string>> y = new tuple(1, x);
<int, <int, bool, string>> z = new tuple(1, new tuple(1, true, "string"));

int i, j, k, l; bool a; string s;
tie (i, (j, b, s)) = y; // variable assignment from tuple
tie (k, _) = y; // variable assignment from tuple, one value ignored

l = y[0];

<int, int> double_int(int x) {
    return new tuple(x, x);
}

int sum_pair(<int, int> pair) {
    int x, y;
    tie (x, y) = pair;
    return x + y;
}

int get_first(<int, int> pair) {
    return pair[0];
}

// BREAK, CONTINUE
while (true) {
    break;
}

int i = 0;
while (i < 10) {
    if (i == 5) {
        continue;
    }
    print(i);
    i++;
}
