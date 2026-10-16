#ifndef __RXODE2_SBUF_FORMAT_H__
#define __RXODE2_SBUF_FORMAT_H__
// The sbuf writers with a printf format attribute, so -Wformat checks rxode2's
// own calls.  The installed declarations (inst/include/rxode2parseSbuf.h) stay
// without it, as other packages include that header; include this one before
// any dparser header that defines printf.
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
