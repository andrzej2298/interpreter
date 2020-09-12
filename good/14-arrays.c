print(len(new int[100]));

int sum(int[] arr) {
	int i = 0, s = 0;
	while (i < len(arr)) {
		s = s + arr[i];
		i++;
	}
	return s;
}

print(sum({1, 2, 3, 4, 5}));

string[] s = {"a", "b", "c"};
print(s);
s[0] = "d";
print(s);

bool[] even(int[] arr) {
	int N = len(arr);
	bool[] flags = new bool[N];
	int i = 0;
	while (i < N) {
		flags[i] = arr[i] % 2 == 0;
		i++;
	}
	return flags;
}

print(even({1, 2, 3, 4, 5, 6, 7, 8, 9, 10}));
