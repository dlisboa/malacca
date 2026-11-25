// Macros, conditionals and includes.
// expect: 25 3 11 hello MAX_SIZE
// expect: line 38 in preprocessor.c
// exit: 112
#include <stdio.h>
#include "local_header.h"
#include "local_header.h"      // second time: skipped by the include guard

#define MAX_SIZE 10
#define SQUARE(x) ((x) * (x))
#define ADD(a, b) ((a) + (b))
#define TWICE(x) ADD(x, x)
#define EMPTY

#define DEBUG
#undef MAX_SIZE
#define MAX_SIZE 11

#ifdef DEBUG
int mode = 1;
#else
int mode = 2;
#endif

#ifndef NOT_DEFINED
#if defined(DEBUG) && MAX_SIZE > 10 && !defined(NOT_DEFINED)
#define BRANCH 1
#elif 1
#define BRANCH 2
#else
#define BRANCH 3
#endif
#endif

int main()
{
    printf("%d %d %d %s MAX_SIZE\n", SQUARE(2 + 3), ADD(1, 2), MAX_SIZE, "hello");
    printf("line %d in preprocessor.c\n", __LINE__);
    EMPTY
    // 100 from the header + 5 + mode(1) + BRANCH(1) + TWICE(2)=4 + 1
    return FROM_HEADER + from_header() + mode + BRANCH + TWICE(2) + 1;
}
