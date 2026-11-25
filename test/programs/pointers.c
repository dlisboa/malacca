// Address-of, dereference, writing through pointers, pointers to pointers,
// and passing a pointer so a callee can modify the caller's variable.
// exit: 52
void set(int *p, int v)
{
    *p = v;
}

int main()
{
    int x = 10;
    int *p = &x;
    int **pp = &p;
    *p = *p + 1;        // x = 11
    **pp = **pp + 1;    // x = 12
    int y = 0;
    set(&y, 40);        // y = 40
    return x + y;       // 52
}
