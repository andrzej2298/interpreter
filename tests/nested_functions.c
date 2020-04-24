
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
