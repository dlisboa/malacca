// sizeof (types and expressions, without evaluating them) and casts.
// sizes 4+1+8+8+10+12 = 43, small 1, n 4, side 0, (c == 65) 1 -> 49
// exit: 49
int main()
{
    char buf[10];
    int arr[3];
    long big = 4294967297;       // 2^32 + 1: needs 64 bits
    int small = (int)big;        // cast truncates to 1
    char c = (char)321;          // 321 - 256 = 65
    int side = 0;
    int n = sizeof(side = 99);   // not evaluated: side stays 0
    return sizeof(int) + sizeof(char) + sizeof(long) + sizeof(int *)
         + sizeof buf + sizeof arr + small + n + side + (c == 65);
}
