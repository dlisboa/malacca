// Array initializers: lists, omitted sizes, partial lists (rest is zero),
// strings for char arrays, and the same for globals.
// expect: 3 1 4 1 5 | 9 0 0 | hi! 4 | 6 28
// expect: 10 20 30 5 | names: ann bob
// exit: 0
#include <stdio.h>

int g_nums[] = {10, 20, 30};
int g_zero_tail[5] = {1, 2};
char g_word[] = "global";
char *g_names[] = {"ann", "bob"};

int main()
{
    int pi[] = {3, 1, 4, 1, 5,};              // trailing comma; size is 5
    int part[3] = {9};                         // the rest is zero
    char s[] = "hi!";                          // size 4: includes the NUL
    int total = 0;
    for (int i = 0; i < 3; i++)
        total += g_nums[i];
    printf("%d %d %d %d %d | %d %d %d | %s %d | %d %d\n",
           pi[0], pi[1], pi[2], pi[3], pi[4],
           part[0], part[1], part[2], s, (int)sizeof(s),
           (int)sizeof(pi) / 4 + 1, g_zero_tail[1] * 14);
    printf("%d %d %d %d | names: %s %s\n", g_nums[0], g_nums[1], g_nums[2],
           (int)sizeof(g_word) - 2 + g_zero_tail[4], g_names[0], g_names[1]);
    return total - 60;
}
