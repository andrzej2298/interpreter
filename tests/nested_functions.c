
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

print(e());

int n;

int h(int n) {
	int i() {
		return n;
	}
	return i();
}

n = 1;
print(h(2));

int start = 100;
int nest(int start) {
	print("nest");
	print(start);
	if (start > 0) {
		int inside(int start) {
			print("inside");
			print(start);
			if (start > 0) {
				return nest(start - 1);
			}
		}
		return inside(start - 1);
	}
	return 42;
}

print(nest(10));

int x = 10;
int y = 90;

void a() {
	int y = 40;
	print("a");
	print(x);
	// int x = 0;
	void b() {
		int y = 41;
		print("b");
		print(x);
		// int x = 1;
		void c() {
			int y = 42;
			print("c");
			print(x);
			// int x = 2;
			if (x >= 0) {
				x--;
				a();
			}
			print("cy");
			print(y);
		}
		if (x >= 0) {
			x--;
			c();
		}
		print("by");
		print(y);
	}
	if (x >= 0) {
		x--;
		b();
	}
	print("ay");
	print(y);
}

a();
