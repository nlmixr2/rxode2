#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
char * _strdup_printf(char * fmt, ...) {
  if (fmt == NULL) {
    char * rt = malloc(2*sizeof(char));
    rt[0] = '\0';
    return rt;
  }
  va_list va;
  va_start(va, fmt);
#if defined(_WIN32) || defined(WIN32)
  int s = vsnprintf(NULL, 0, fmt, va);
#else
  char zero[2];
  int s = vsnprintf(zero, 0, fmt, va);
#endif
  va_end(va);
  char * rt = malloc(s*sizeof(char));
  va_start(va, fmt);
  vsnprintf(rt, s, fmt, va);
  va_end(va);
  return rt;
}
