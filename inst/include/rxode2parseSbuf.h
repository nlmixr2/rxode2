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
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2parse", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#endif
#include "rxode2parse.h"

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

void sAppend(sbuf *sbb, const char *format, ...);

void sPrint(sbuf *sbb, const char *format, ...);

static inline void sClear(sbuf *sbb){
  sbb->s[0]='\0';
  sbb->o=0;
}

void lineIni(vLines *sbb);

void lineFree(vLines *sbb);

void addLine(vLines *sbb, const char *format, ...);

void curLineProp(vLines *sbb, int propId);

void curLineType(vLines *sbb, int propId);

void doDot(sbuf *out, char *buf);

void doDot2(sbuf *sb, sbuf *sbDt, char *buf);

#if defined(__cplusplus)
}
#endif

#endif // __SBUF_H__
