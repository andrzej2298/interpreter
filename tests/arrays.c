int[] a = new int[10]; // default 0
string[] b = new string[10]; // default ""
bool[] c = new bool[10]; // default false

int[] d = {1, 2, 3, 4};
string[] e = {"a", "b", "c"};
bool[] f = {true, false, true};

print(d[3]);
// print(d[4]);

print(a);
print(b);
print(c);
print(d);
print(e);
print(f);

// return
int[] fib(int n) {
	// int[] values = new int[n];
	// int i = 0;
	// while (
	return new int[5];
}
// pass
void pass_by_value(int[] ns) {
	ns[0] = 50;
}
void pass_by_reference(int[] &ns) {
	ns[0] = 50;
}
print("fib up to 5");
print(fib(5));

int[] ns = {1,2,3,4};
int[] ms = {1,2,3,4};
pass_by_value(ns);
pass_by_reference(ms);
print("pass by value");
print(ns);
print("pass by reference");
print(ms);

print("length");
print(len(d));
d[0] = 42;
print(d[0]);

int[] arr = {1, 2, 3, 4};
int sum(int[] array) {
    int i = 0, sum = 0;
    while (i < len(array)) {
        sum = sum + array[i];
        i++;
    }
    return sum;
}

print(arr);
print("sum: ");
print(sum(arr));

print({1, 2, 3} == {"a", "b", "c"});
