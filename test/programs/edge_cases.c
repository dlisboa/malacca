// Things that stress instruction encodings: frames larger than 4095 bytes,
// big and negative constants, 64-bit arithmetic, int overflow wrap-around,
// 8 arguments, char parameters and deep temporaries.
// expect: 7 5000000000 -3 -2147483648 36 1 120
// exit: 0
#include <stdio.h>

int sum8(int a, int b, int c, int d, int e, int f, int g, int h)
{
    return a + b + c + d + e + f + g + h;
}

int char_param(char c, long l)
{
    return c + l;
}

int main()
{
    char big[100000];               // offsets far beyond what ldr/str can encode
    big[99999] = 7;
    long five_billion = 5000000000;
    int wrapped = 2147483647;
    wrapped = wrapped + 1;          // wraps to INT_MIN like on a real machine
    int deep = (1 + (2 + (3 + (4 + (5 + (6 + (7 + (8 + 0))))))));
    int neg = -3;
    printf("%d %ld %d %d %d %d %d\n", big[99999], five_billion, neg, wrapped,
           sum8(1, 2, 3, 4, 5, 6, 7, 8), char_param(-1, 2), 1 << 3 | 2 + 5 * 22);
    return deep - 36;
}
