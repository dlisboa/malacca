/* Used by preprocessor.c. The guard makes a second #include a no-op. */
#ifndef LOCAL_HEADER_H
#define LOCAL_HEADER_H

#define FROM_HEADER 100
int from_header(void) { return 5; }

#endif
