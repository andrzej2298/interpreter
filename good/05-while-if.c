if (true) {
	print(true);
}
else {
	print(false);
}

int i = 0;
while (i < 10) {
	print(i);
	i++;
}

int i = 0;
int N = 2;
while (i < N) {
	print("i");
	print(i);
	int j = 0;
	while (j < N) {
		print("j");
		print(j);
		int k = 0;
		while (k < N) {
			print("k");
			print(k);
			k++;
		}
		j++;
	}
	i++;
}
