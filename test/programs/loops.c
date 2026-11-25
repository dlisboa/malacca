// while, do-while, for, break, continue, nested loops, scoped loop variables.
// 1..10 sums to 55; the do-while body runs once although its condition is
// false (+1); even k below 10 sum to 20; the nested loops add 36; the infinite
// loop adds 1 then breaks; the last loop reuses the name k (its scope ended)
// and adds 4. Total: 55 + 1 + 20 + 36 + 1 + 4 = 117.
// exit: 117
int main()
{
    int total = 0;

    int i = 1;
    while (i <= 10) {
        total = total + i;
        i = i + 1;
    }

    do
        total = total + 1;
    while (0);

    for (int k = 0; k < 10; k = k + 1) {
        if (k % 2 == 1)
            continue;
        total = total + k;
    }

    for (int a = 1; a <= 3; a = a + 1)
        for (int b = 1; b <= 3; b = b + 1)
            total = total + a * b;

    for (;;) {
        total = total + 1;
        if (total > 0)
            break;
    }
    for (int k = 0; k < 4; k = k + 1)
        total = total + 1;
    return total;
}
