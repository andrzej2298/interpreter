void by_value(int x) {
	x = 10;
}

void by_reference(int &x) {
	x = 10;
}

int x = 0;

by_value(x);
print(x);
by_reference(x);
print(x);

void modify_array_by_value(int[] arr) {
	arr[0] = 42;
}

void modify_array_by_reference(int[] &arr) {
	arr[0] = 42;
}

void discard_array(int [] &arr) {
	arr = {9, 9, 9};
}

int[] arr = {1, 2, 3, 4};
modify_array_by_value(arr);
print(arr);
modify_array_by_reference(arr);
print(arr);
discard_array(arr);
print(arr);
