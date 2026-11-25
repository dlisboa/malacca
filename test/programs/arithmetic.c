// Operator precedence, associativity, division and remainder, negatives.
// 2 + 3*4 = 14; (2+3)*4 = 20; 100/7 = 14, 100%7 = 2; -(-3) = 3
// 14 + 20 + 14 + 2 + 3 = 53; 20 - 14 - 3 (left assoc) = 3 -> 56
// exit: 56
int main()
{
    int a = 2 + 3 * 4;
    int b = (2 + 3) * 4;
    int c = 100 / 7;
    int d = 100 % 7;
    int e = -(-3);
    int f = 20 - 14 - 3;
    return a + b + c + d + e + f;
}
