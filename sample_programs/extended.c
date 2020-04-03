while (true) {
    break;
}

int i = 0;
while (i < 10) {
    if (i == 5) {
        continue;
    }
    print(i);
    i++;
}

int[] a = {1, 2, 3, 4};

int sum(int[] array) {
    int i = 0, sum = 0;
    while (i < len(array)) {
        sum = sum + array[i];
        i++;
    }
    return sum;
}
