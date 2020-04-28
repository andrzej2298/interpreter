// // BREAK, CONTINUE
// while (true) {
//     break;
// }

int i = -1;
while (i < 10) {
    i++;
    if (i == 5) {
        continue;
    }
    print(i);
}

print("here");

i = 0;
while (i < 2) {
	print("i");
	print(i);
	int j = 0;
	while (j < 2) {
		if (j == 1) {
			break;
		}
		print("j");
		print(j);
		j++;
	}

	i++;
}
