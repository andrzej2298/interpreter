<string, <int, int>> person = new tuple("John", new tuple(1, 2));

print(person);
print(person[0]);
print(person[1]);

<int, int> coordinates;
int x, y;

tie(_, coordinates) = person;
tie(_, (x, y)) = person;
print(coordinates[0]);
print(x);
print(coordinates[1]);
print(y);

int a, b, c, d, e, f, g, h;

tie(a, (b, (c, (d, (e, (f, (g, h))))))) = new tuple(1,
					  new tuple(2,
					  new tuple(3,
					  new tuple(4,
					  new tuple(5,
					  new tuple(6,
					  new tuple(7, 8)))))));
print("nested");
print(a);
print(b);
print(c);
print(d);
print(e);
print(f);
print(g);
print(h);
