
// NESTED FUNCTION DECLARATIONS
// e() == 0
print ("abc");

int e() {
    int x = 0;
    print("e");
    print(x);

    int f() {
        print("f");
        print(x);
    }

    int g() {
        int x = 1;
        f();
    	print("g");
        print(x);
    }

    g();
}

e();
