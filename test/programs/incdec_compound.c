// ++/-- (prefix and postfix: different values), compound assignment,
// and the left side being evaluated only once.
// a=5 b=7 c=7 d=5 i=5 x=15 arr[0]=7 k=1 arr[1]=0 -> 52
// exit: 52
int main()
{
    int i = 5;
    int a = i++;        // a = 5, i = 6
    int b = ++i;        // b = 7, i = 7
    int c = i--;        // c = 7, i = 6
    int d = --i;        // d = 5, i = 5
    int x = 10;
    x += 5;             // 15
    x -= 1;             // 14
    x *= 2;             // 28
    x /= 4;             // 7
    x %= 5;             // 2
    x <<= 3;            // 16
    x >>= 1;            // 8
    x |= 3;             // 11
    x &= 14;            // 10
    x ^= 5;             // 15
    int arr[3];
    arr[0] = arr[1] = arr[2] = 0;
    int k = 0;
    arr[k++] += 7;      // k++ must run once: arr[0] = 7, k = 1
    return a + b + c + d + i + x + arr[0] + k + arr[1];
}
