<int, bool, string> x = new tuple(1, true, "string");
<int, <int, bool, string>> y = new tuple(1, x);
<int, <int, bool, string>> z = new tuple(1, new tuple(1, true, "string"));

print(x);
print(y);
print(z);
int i, j, k, l; bool b; string s;
tie (i, (j, b, s)) = y; // variable assignment from tuple
tie (k, _) = y; // variable assignment from tuple, one value ignored

l = y[0];

print(i);
print(j);
print(k);
print(l);
print(b);
print(s);

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

print(double_int(10));
print(sum_pair(new tuple(10, 5)));
print(get_first(new tuple(42, 5)));

<int, bool, string> def;
print("defaults");
print(def);
