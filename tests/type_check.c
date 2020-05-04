<int, <int, int>> tup1, tup2 = new tuple(2, new tuple(3, 3));
int[] a, b = {1,2,3}, c = new int[10];

bool s = 1 < 0;

<int, int> v = tup1[1];
int w = v[1];

s = false;

bool x = true && false;

1+1*1;

if (1 < 0) {
}
else {
	string s = "";
}
;;;


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

int i = 0;
while (i < 10) {
	print(i);
	i++;
}

int f(int &x) {
	if (true) {
		return 1;
	}
	else {
		return 0;
	}
}

int x = 10;
int y = 10;
int qwerty = f(x);
print(qwerty);

int g() {
	return 10;
}

g();

<int, int> int_pair = new tuple(1, 1);
c[0] = 7;
print(int_pair);
print(c);
