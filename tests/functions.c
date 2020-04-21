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

// void infinite() {
// 	print("infinite");
// 	infinite();
// }
//
// infinite();

print("here");

int range(int &i, int &n) {
    if (i < n) {
        print(i);
	i++;
	range(i, n);
    }
    else {}
}

int i = 0;
int n = 10;
range(i, n);
print("after function");
print(i);
print(n);
