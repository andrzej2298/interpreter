bool b = true || false, c = 0 < 1;

print(b);
print(c);

print("or");
print(true || true);
print(true || false);
print(false || true);
print(false || false);

print("and");
print(true && true);
print(true && false);
print(false && true);
print(false && false);

print("not");
print(!true);
print(!false);

print("leniwa ewaluacja wyrazen logicznych");
int[] arr = {1};
print(true || arr[10] < 0);


print("literaly");
int x = 0;
bool b = false;
string s = "string";
int[] ints = {1, 2, 3, 4};
<int, int> pair = new tuple(1, 1);
print(x);
print(b);
print(s);
print(ints);
print(pair);

print("konkatenacja");
print("abc" + "def");

print("porownania");
int[] empty;
print(empty == new int[0]);
print({0, 0, 0} == new int[3]);
print(new tuple(1, 2, 3) == new tuple(1, 3, 2));
print({0} == {0, 0});
