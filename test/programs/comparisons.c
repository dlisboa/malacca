// Comparison operators yield 0 or 1.
// exit: 31
int main()
{
    return (1 < 2) + 2 * (2 > 1) + 4 * (3 <= 3) + 8 * (3 >= 4 == 0)
         + 16 * (1 != 2) + 32 * (5 == 6);
}
