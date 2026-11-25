// A bigger program mixing many features: heap memory, strings, sorting,
// recursion, pointers to pointers, bool, and header functions.
// expect: sorted: 1 2 3 5 8 9
// expect: found 8 at 4, 7 missing
// expect: reversed: dlrow olleh
// expect: primes below 30: 2 3 5 7 11 13 17 19 23 29
// expect: gcd(84, 36) = 12, 10! = 3628800
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

void swap(int *a, int *b)
{
    int t = *a;
    *a = *b;
    *b = t;
}

// bubble sort
void sort(int *v, int n)
{
    for (int i = 0; i < n - 1; i++)
        for (int j = 0; j < n - 1 - i; j++)
            if (v[j] > v[j + 1])
                swap(&v[j], &v[j + 1]);
}

int bsearch_idx(int *v, int n, int key)
{
    int lo = 0, hi = n - 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        if (v[mid] == key)
            return mid;
        if (v[mid] < key)
            lo = mid + 1;
        else
            hi = mid - 1;
    }
    return -1;
}

void reverse(char *s)
{
    int n = strlen(s);
    for (int i = 0; i < n / 2; i++) {
        char t = s[i];
        s[i] = s[n - 1 - i];
        s[n - 1 - i] = t;
    }
}

bool is_prime(int n)
{
    if (n < 2)
        return false;
    for (int d = 2; d * d <= n; d++)
        if (n % d == 0)
            return false;
    return true;
}

int gcd(int a, int b)
{
    return b == 0 ? a : gcd(b, a % b);
}

long factorial(int n)
{
    return n <= 1 ? 1 : n * factorial(n - 1);
}

int main()
{
    int *v = malloc(6 * sizeof(int));
    int init[6];
    init[0] = 5; init[1] = 2; init[2] = 9; init[3] = 1; init[4] = 8; init[5] = 3;
    memcpy(v, init, 6 * sizeof(int));
    sort(v, 6);
    printf("sorted:");
    for (int i = 0; i < 6; i++)
        printf(" %d", v[i]);
    printf("\n");
    printf("found 8 at %d, 7 %s\n", bsearch_idx(v, 6, 8),
           bsearch_idx(v, 6, 7) < 0 ? "missing" : "present");
    free(v);

    char *s = malloc(16);
    strcpy(s, "hello world");
    reverse(s);
    printf("reversed: %s\n", s);
    free(s);

    printf("primes below 30:");
    for (int n = 0; n < 30; n++)
        if (is_prime(n))
            printf(" %d", n);
    printf("\n");
    printf("gcd(84, 36) = %d, 10! = %ld\n", gcd(84, 36), factorial(10));
    return 0;
}
