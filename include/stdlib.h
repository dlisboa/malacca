#ifndef _MALACCA_STDLIB_H
#define _MALACCA_STDLIB_H

#include <stddef.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#define RAND_MAX 2147483647

void *malloc(size_t size);
void *calloc(size_t count, size_t size);
void *realloc(void *ptr, size_t size);
void free(void *ptr);
void exit(int status);
int abs(int n);
long labs(long n);
int atoi(const char *s);
long atol(const char *s);
int rand(void);
void srand(int seed);

#endif
