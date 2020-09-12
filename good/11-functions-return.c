int[] arr() {
	return {1, 2, 3};
}

<int, int> tup() {
	return new tuple(1, 2);
}

int integer() {
	return 0;
}

string str() {
	return "abc";
}

bool boolean() {
	return true;
}

print(arr());
print(tup());
print(integer());
print(str());
print(boolean());

void arr_in(int[] a) {
	print(a);
}

void tup_in(<int, int> t) {
	print(t);
}

void integer_in(int i) {
	print(i);
}

void str_in(string s) {
	print(s);
}

void boolean_in(bool b) {
	print(b);
}

arr_in(arr());
tup_in(tup());
integer_in(integer());
str_in(str());
boolean_in(boolean());
