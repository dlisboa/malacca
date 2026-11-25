// Global variables: initialised (constant expressions), zero-initialised,
// of several types, arrays, and modified from functions.
// expect: 27 -5 hi
// exit: 10
int printf(const char *fmt, ...);

int answer = 3 * 9;            // folded at compile time
int negative = -5;
char greeting[1];              // uninitialised: zero
char *message = "hi";
long counter;
int table[4];

void bump(void)
{
    counter = counter + 1;
    table[counter] = counter * 2;
}

int main()
{
    printf("%d %d %s\n", answer, negative, message);
    bump();
    bump();
    bump();                    // counter = 3, table = {0, 2, 4, 6}
    return counter + table[3] + greeting[0] + table[1] - 1;
}
