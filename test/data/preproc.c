#include "local.h"
#include <stdio.h>

#define MAX_SIZE 100
#define SQUARE(x) ((x) * (x))
#define DEBUG

#ifdef DEBUG
int debug_mode = 1;
#else
int debug_mode = 0;
#endif

#ifndef BUFFER_SIZE
#define BUFFER_SIZE 1024
#endif

int main()
{
    int size = MAX_SIZE;
    int squared = SQUARE(5);
    return 0;
}
