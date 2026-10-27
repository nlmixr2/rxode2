#ifndef __SBUF_H__
#define __SBUF_H__
#pragma once
#if defined(__cplusplus)
extern "C" {
#endif

#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#include <unistd.h>
#include <errno.h>
#ifdef _isrxode2parse_
#define _(String) (String)
#endif
#include "rxode2parse.h"

// A package that defines RXODE2_SBUF_FORMAT (PKG_CPPFLAGS =
// -DRXODE2_SBUF_FORMAT) has its sAppend(), sPrint() and addLine() calls checked
// by -Wformat, with R's printf format attribute; R's default
// -Werror=format-security then refuses a format that is not a literal and has
// no arguments.  Without it the declarations carry no attribute.  rxode2's own
// calls are checked through src/rxSbufFormat.h.
#ifdef RXODE2_SBUF_FORMAT
#include <R_ext/Print.h>
#ifndef R_PRINTF_FORMAT
#define R_PRINTF_FORMAT(M, N)
#endif
#define RXODE2_SBUF_PRINTF(M, N) R_PRINTF_FORMAT(M, N)
#else
#define RXODE2_SBUF_PRINTF(M, N)
#endif

#define SBUF_MXBUF 48000
#define SBUF_MXLINE 100

#ifdef _isrxode2parse_
#define NODOT _("'.' in variables and states not supported, use '_' instead or set 'options(rxode2.syntax.allow.dots = TRUE)'")
#endif


int rc_buf_read(const char *pathname, char **buf, int *len);

char * rc_sbuf_read(const char *pathname);

void sIniTo(sbuf *sbb, int to);

void sIni(sbuf *sbb);

void sFree(sbuf *sbb);

void sFreeIni(sbuf *sbb);

void sAppendN(sbuf *sbb, const char *what, int n);

static inline void sPut(sbuf *sbb, char what) {
  if (sbb->sN <= 2 + sbb->o) {
    int mx = sbb->o + 2 + SBUF_MXBUF;
    sbb->s = R_Realloc(sbb->s, mx, char);
    sbb->sN = mx;
  }
  snprintf(sbb->s+sbb->o, sbb->sN - sbb->o, "%c", what);
  sbb->o++;
}

void sAppend(sbuf *sbb, const char *format, ...) RXODE2_SBUF_PRINTF(2, 3);

void sPrint(sbuf *sbb, const char *format, ...) RXODE2_SBUF_PRINTF(2, 3);

static inline void sClear(sbuf *sbb){
  sbb->s[0]='\0';
  sbb->o=0;
}

void lineIni(vLines *sbb);

void lineFree(vLines *sbb);

void addLine(vLines *sbb, const char *format, ...) RXODE2_SBUF_PRINTF(2, 3);

void curLineProp(vLines *sbb, int propId);

void curLineType(vLines *sbb, int propId);
void doDot(sbuf *out, char *buf);

void doDot2(sbuf *sb, sbuf *sbDt, char *buf);

#if defined(__cplusplus)
}
#endif

#endif // __SBUF_H__
