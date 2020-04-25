void f(int x, int &y) {
	x++;
	y++;
}

int i = 0, j = 0;
f(i, j);
print(i);
print(j);
