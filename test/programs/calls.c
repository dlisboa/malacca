// Function calls: arguments, nesting, prototypes, and temporaries (the pending
// `1 +`) surviving a call.
// exit: 9
int add3(int a, int b, int c);

int twice(int x)
{
    return x * 2;
}

int add3(int a, int b, int c)
{
    return a + b + c;
}

int main()
{
    // 1 + add3(0 + 0 + 0, 1, 1) + twice(add3(1, 1, 1)) = 1 + 2 + 6
    return 1 + add3(add3(0, 0, 0), 1, 1) + twice(add3(1, 1, 1));
}
