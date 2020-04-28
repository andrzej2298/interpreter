int constant() {
	int x = 10;
	print("inside function");
	print(x);
	return 1;
}

void procedure() {
	int x = 11;
	print("inside procedure");
	print(x);
	return;
}

int x = -1;
print(constant());
procedure();

print("here");

int range(int &i, int &n) {
    if (i < n) {
        print(i);
	i++;
	range(i, n);
    }
    else {}
    return 1;
}

int i = 0;
int n = 3;
range(i, n);
print("after function");
print(i);
print(n);

print("return");

int f() {
	return 42;
	return 43;
}

print(f());

int fact(int n) {
	if (n <= 0) {
		return 1;
	}
	else {
		return n * fact(n - 1);
	}
}

print("fact of 5:");
print(fact(5));

int double_return() {
	return 0;
	return 1;
}

print(double_return());

int fancy_double_return() {
	if (false) {
		return 0;
	}
	else {
		if (true) {
			return 1;
		}
		else {
			return 2;
		}
	}
}
print("fancy double return");
print(fancy_double_return());
