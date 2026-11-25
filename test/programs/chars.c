// char is 1 byte and signed: stores truncate, loads sign-extend. Walking a
// string with a char pointer.
// expect: 5 -1 65
// exit: 3
int printf(const char *fmt, ...);

int length(char *s)
{
    int n = 0;
    while (*s) {
        n++;
        s++;
    }
    return n;
}

int main()
{
    char c = 255;                // wraps to -1
    char letter = 'A';
    printf("%d %d %d\n", length("hello"), c, letter);
    char buf[4];
    buf[0] = 'h';
    buf[1] = 'i';
    buf[2] = 0;
    return length(buf) + 1;
}
