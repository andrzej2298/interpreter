int i = 0;
while (i < 10) {
	void f() {
		print("i");
		print(i);
	}
	int j = 0;
	print("js");
	while (j < 3) {
		void f() {
			print(j);
		}
		f();
		j++;
	}
	i++;
	f();
}
