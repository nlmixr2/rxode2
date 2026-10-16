#ifndef __RXODE2_SBUF_FORMAT_H__
#define __RXODE2_SBUF_FORMAT_H__
// The sbuf writers with a printf format attribute, so -Wformat checks rxode2's
// own calls.  The installed declarations (inst/include/rxode2parseSbuf.h) stay
// without it, as other packages include that header.  A redeclaration covers
// only the calls parsed after it, so rxode2's headers include this one in place
// of rxode2parseSbuf.h: it must come before every header whose inline functions
// call the writers, and before any dparser header that defines printf.
#include "../inst/include/rxode2parseSbuf.h"
#include "rxPrintfFormat.h"
#if defined(__cplusplus)
extern "C" {
#endif
void sAppend(sbuf *sbb, const char *format, ...) R_PRINTF_FORMAT(2, 3);
void sPrint(sbuf *sbb, const char *format, ...) R_PRINTF_FORMAT(2, 3);
void addLine(vLines *sbb, const char *format, ...) R_PRINTF_FORMAT(2, 3);
#if defined(__cplusplus)
}
#endif
#endif
