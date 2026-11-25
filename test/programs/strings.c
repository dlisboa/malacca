// String literals and calls into libc, including variadic ones (printf's
// extra arguments travel on the stack on Apple arm64). Prototypes are written
// out here; later tests use #include <stdio.h> instead.
// expect: hello from a string
// expect: 42 and 7 and 'x'
// expect: tab:	| quote:" | backslash:\ | hex:A | octal:B
// expect: 1 2 3 4 5 6 7 8 9 10
int puts(const char *s);
int printf(const char *format, ...);

int main()
{
    puts("hello from a string");
    printf("%d and %d and '%c'\n", 42, 3 + 4, 'x');
    printf("tab:\t| quote:\" | backslash:\\ | hex:\x41 | octal:\102\n");
    // ten arguments after the format: more than fit in registers
    printf("%d %d %d %d %d %d %d %d %d %d\n", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    return 0;
}
