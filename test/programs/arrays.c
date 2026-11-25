// Arrays, indexing, pointer arithmetic (scaled by element size), pointer
// difference and comparison, and array-to-pointer decay when passing.
// exit: 83
int sum(int *a, int n)
{
    int total = 0;
    for (int i = 0; i < n; i++)
        total += a[i];
    return total;
}

int main()
{
    int a[5];
    for (int i = 0; i < 5; i++)
        a[i] = i * i;                  // 0 1 4 9 16
    int s = sum(a, 5);                 // 30
    int *p = a + 2;
    int d = (a + 4) - p;               // 2 elements apart
    int m[2][3];                       // arrays of arrays
    m[1][2] = 40;
    int ok = p > a && a < &a[4];       // pointer comparison
    return s + *p + d + m[1][2] + *(a + 3) + ok - 3;  // 30+4+2+40+9+1-3
}
