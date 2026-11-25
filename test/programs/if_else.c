// if / else if / else, including the dangling else.
// exit: 6
int classify(int n)
{
    if (n < 0)
        return 1;
    else if (n == 0)
        return 2;
    else
        return 3;
}

int main()
{
    int r = 0;
    if (1)
        if (0)
            r = 100;
        else        // belongs to the inner if
            r = r + 1;
    // r=1, classify gives 1, 2, 3: 1 + 1 + 2 + 3 - 1 = 6
    return r + classify(-5) + classify(0) + classify(9) - 1 + 0 * r;
}
