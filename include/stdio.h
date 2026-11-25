/* A small slice of <stdio.h>: the functions that need no FILE* or typedefs. */
#ifndef _MALACCA_STDIO_H
#define _MALACCA_STDIO_H

#include <stddef.h>

#define EOF (-1)

int printf(const char *format, ...);
int sprintf(char *buffer, const char *format, ...);
int snprintf(char *buffer, size_t size, const char *format, ...);
int scanf(const char *format, ...);
int puts(const char *s);
int putchar(int c);
int getchar(void);

#endif
