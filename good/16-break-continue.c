// no-op outside of a loop
break;
continue;

print("break");
int i = 0;
int N = 3;
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

			if (k == 1) {
				break;
			}
		}
		j++;

		if (j == 1) {
			break;
		}
	}
	i++;

	if (i == 1) {
		break;
	}
}

print("continue");
int i = -1;
int N = 3;
while (i < N) {
	i++;
	if (i == 1) {
		continue;
	}
	if (i < N) {
		print("i");
		print(i);
		int j = -1;
		while (j < N) {
			j++;
			if (j == 1) {
				continue;
			}
			if (j < N) {
				print("j");
				print(j);
				int k = -1;
				while (k < N) {
					k++;
					if (k == 1) {
						continue;
					}
					if (k < N) {
						print("k");
						print(k);
					}
				}
			}
		}
		print("");
	}
}

