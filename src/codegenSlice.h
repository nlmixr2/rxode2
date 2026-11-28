#ifndef __CODEGENSLICE_H__
#define __CODEGENSLICE_H__
// Per-function statement slicing (options(rxode2.fullOpt = "slice")).
//
// Every generated function used to evaluate every model statement.  Here each
// function keeps only the statements its outputs depend on: the lines it must
// emit (d/dt() in dydt, the lhs written back by calc_lhs, f()/alag()/... in
// their own functions, and every line whose type is not an ordinary assignment)
// seed a backward closure over the variables they read.  A needed variable
// pulls in every line that assigns it, an if/else/while chain is kept whole
// when any line in it is kept, and a line that calls anything with a side
// effect (random draws, linCmt(), user functions, ...) is never dropped.  The
// kept lines run in their original order on identical inputs, so the result
// is the same as evaluating every line.
//
// The analysis reads the sbPmDt form of each line, where d/dt() and Jacobian
// values are plain locals (__DDtStateVar_k__, __PDStateVar_*__).

#include "pkTime.h"
#include "../inst/include/rxMemAlloc.h"

typedef struct cslHash {
  char **key;
  int *id;
  int cap;
  int n;
} cslHash;

typedef struct cslModel {
  int ok;        // 1 when the analysis is usable
  int n;         // number of lines
  int *def;      // identifier assigned by line i, -1 when none
  int *rdStart;  // reads of line i are rd[rdStart[i] .. rdStart[i+1]-1]
  int *rd;
  int *slice;    // 1 when line i may be dropped (pure, understood)
  int *out;      // 1 when line i assigns a d/dt() value, 2 a Jacobian element
  int *span;     // top-level if/else/while chain of line i, -1 when none
  int *spanFirst;
  int *spanLast;
  int nSpan;
  int *dlStart;  // lines assigning identifier v: dl[dlStart[v] .. dlStart[v+1]-1]
  int *dl;
  cslHash h;
} cslModel;

static cslModel _csl;
// Lines the function being generated keeps; NULL keeps everything
static int *_cslKeep = NULL;
static int _cslLevel = 0;

static inline unsigned int cslHashStr(const char *s, int len) {
  unsigned int h = 2166136261u;
  for (int i = 0; i < len; ++i) {
    h ^= (unsigned char)s[i];
    h *= 16777619u;
  }
  return h;
}

static inline void cslHashInit(cslHash *h, int cap) {
  h->cap = cap;
  h->n = 0;
  h->key = (char**)rxMemRalloc(fixed, cap, sizeof(char*));
  h->id = (int*)rxMemRalloc(fixed, cap, sizeof(int));
  for (int i = 0; i < cap; ++i) h->key[i] = NULL;
}

static inline int cslHashSlot(cslHash *h, const char *s, int len) {
  unsigned int m = (unsigned int)h->cap - 1u;
  unsigned int k = cslHashStr(s, len) & m;
  while (h->key[k] != NULL &&
         !((int)strlen(h->key[k]) == len && !strncmp(h->key[k], s, len))) {
    k = (k + 1u) & m;
  }
  return (int)k;
}

static inline void cslHashGrow(cslHash *h) {
  cslHash g;
  cslHashInit(&g, h->cap * 2);
  for (int i = 0; i < h->cap; ++i) {
    if (h->key[i] == NULL) continue;
    int k = cslHashSlot(&g, h->key[i], (int)strlen(h->key[i]));
    g.key[k] = h->key[i];
    g.id[k] = h->id[i];
  }
  g.n = h->n;
  *h = g;
}

// id of an identifier; -1 when absent and !add
static inline int cslId(cslHash *h, const char *s, int len, int add) {
  int k = cslHashSlot(h, s, len);
  if (h->key[k] != NULL) return h->id[k];
  if (!add) return -1;
  if (2 * (h->n + 1) > h->cap) {
    cslHashGrow(h);
    k = cslHashSlot(h, s, len);
  }
  char *c = rxMemRalloc(fixed, len + 1, sizeof(char));
  memcpy(c, s, len);
  c[len] = '\0';
  h->key[k] = c;
  h->id[k] = h->n++;
  return h->id[k];
}

static inline int cslIdStr(cslHash *h, const char *s) {
  return cslId(h, s, (int)strlen(s), 0);
}

// growable int vector
typedef struct cslVec {
  int *v;
  int n;
  int cap;
} cslVec;

static inline void cslVecPush(cslVec *x, int val) {
  if (x->n + 1 > x->cap) {
    int cap = x->cap == 0 ? 256 : x->cap * 2;
    int *v = (int*)rxMemRalloc(fixed, cap, sizeof(int));
    for (int i = 0; i < x->n; ++i) v[i] = x->v[i];
    x->v = v;
    x->cap = cap;
  }
  x->v[x->n++] = val;
}

// Impure call names (from .rxFullOptInfo()); a line calling one is kept.  An R
// user function is matched by the name quoted in its _udf("name", ...) call
static cslHash _cslImpure;

static inline int cslIsImpureCall(const char *s, int len) {
  return _cslImpure.cap > 0 && cslId(&_cslImpure, s, len, 0) >= 0;
}

// Model variables that are macros for fields of the individual
// (rxode2_model_shared.h): assigning one is a side effect read after the call
static inline int cslIndField(const char *s, int len) {
  return (len == 10 && !strncmp(s, "rx_lambda_", 10)) ||
    (len == 6 && !strncmp(s, "rx_yj_", 6)) ||
    (len == 6 && !strncmp(s, "rx_hi_", 6)) ||
    (len == 7 && !strncmp(s, "rx_low_", 7));
}

static const char *_cslKeywords[] = {"if", "else", "while", "for", "return",
                                      "sizeof", "break", "double", "int"};

static inline int cslIsKeyword(const char *s, int len) {
  for (int i = 0; i < (int)(sizeof(_cslKeywords) / sizeof(_cslKeywords[0])); ++i) {
    if ((int)strlen(_cslKeywords[i]) == len && !strncmp(s, _cslKeywords[i], len)) {
      return 1;
    }
  }
  return 0;
}

// struct members are not model variables
static inline int cslIsMember(const char *s, int k) {
  return (k > 0 && s[k-1] == '.') || (k > 1 && s[k-1] == '>' && s[k-2] == '-');
}

// Is the call at s[k] (len characters, `(` at s[m]) an R user function
// _udf("name", ...) whose name is registered as impure?
static inline int cslIsImpureUdf(const char *s, int k, int len, int m) {
  if (len != 4 || strncmp(s + k, "_udf", 4)) return 0;
  int p = m + 1;
  while (s[p] == ' ' || s[p] == '\t') p++;
  if (s[p] != '"') return 0;
  int q = ++p;
  while (s[q] != '\0' && s[q] != '"') q++;
  return s[q] == '"' && cslIsImpureCall(s + p, q - p);
}

// The identifier at s[k]: a call is checked against the impure names, any
// other non-member read except the one assigned at defStart is recorded.
// Returns the index just past it.
static inline int cslScanId(const char *s, int k, int defStart, cslVec *rd,
                            int *impure) {
  int j = k;
  while (pkTimeIdChar(s[j])) j++;
  int len = j - k;
  if (cslIsKeyword(s + k, len)) return j;
  int m = j;
  while (s[m] == ' ' || s[m] == '\t') m++;
  if (s[m] == '(') {
    if (cslIsImpureCall(s + k, len) || cslIsImpureUdf(s, k, len, m)) *impure = 1;
    return j;
  }
  if (!cslIsMember(s, k) && k != defStart) {
    cslVecPush(rd, cslId(&_csl.h, s + k, len, 1));
  }
  return j;
}

// Scan line i: record the identifiers it reads (skipping the one it assigns at
// defStart) and report whether it calls anything impure or holds more than one
// statement.
static inline void cslScanLine(const char *s, int defStart, cslVec *rd,
                               int *impure, int *nSemi, int *nBrace) {
  int k = 0;
  *impure = 0;
  *nSemi = 0;
  *nBrace = 0;
  while (s[k] != '\0') {
    int j = pkTimeSkipLiteral(s, k);
    if (j != k) {
      k = j;
    } else if (pkTimeIdStart(s[k])) {
      k = cslScanId(s, k, defStart, rd, impure);
    } else {
      if (s[k] == ';') (*nSemi)++;
      else if (s[k] == '{' || s[k] == '}') (*nBrace)++;
      k++;
    }
  }
}

typedef struct cslSpanState {
  int depth;
  int cur;
  int prev;
} cslSpanState;

// A brace at s[k] of line i; returns 1 when it is one
static inline int cslSpanBrace(cslSpanState *st, const char *s, int k, int i) {
  if (s[k] == '{') {
    if (st->depth == 0) {
      if (st->prev >= 0 && pkTimeHasElse(s, k)) {
        st->cur = st->prev;
      } else {
        st->cur = _csl.nSpan++;
        _csl.spanFirst[st->cur] = i;
      }
    }
    st->depth++;
    return 1;
  }
  if (s[k] == '}' && st->depth > 0) {
    st->depth--;
    if (st->depth == 0) {
      _csl.spanLast[st->cur] = i;
      st->prev = st->cur;
    }
    return 1;
  }
  return 0;
}

// Track the braces of line i; returns 1 when it has any
static inline int cslSpanLine(cslSpanState *st, int i) {
  if (sbPm.lType[i] != TLOGIC) return 0;
  const char *s = sbPm.line[i];
  int touched = 0;
  int k = 0;
  while (s[k] != '\0') {
    int j = pkTimeSkipLiteral(s, k);
    if (j != k) {
      k = j;
      continue;
    }
    touched |= cslSpanBrace(st, s, k, i);
    k++;
  }
  return touched;
}

// Top-level if/else/while chains: an `else` opening at depth 0 joins the chain
// that just closed.
static inline void cslFindSpans(int n) {
  _csl.span = (int*)rxMemRalloc(fixed, n, sizeof(int));
  _csl.spanFirst = (int*)rxMemRalloc(fixed, n, sizeof(int));
  _csl.spanLast = (int*)rxMemRalloc(fixed, n, sizeof(int));
  _csl.nSpan = 0;
  cslSpanState st = {0, -1, -1};
  for (int i = 0; i < n; ++i) {
    int touched = cslSpanLine(&st, i);
    if (st.depth > 0 || touched) {
      _csl.span[i] = st.cur;
      _csl.spanLast[st.cur] = i;
    } else {
      _csl.span[i] = -1;
      // only an `else` directly after the chain may join it
      if (sbPm.lType[i] != TLOGIC) st.prev = -1;
    }
  }
  if (st.depth > 0 && st.cur >= 0) _csl.spanLast[st.cur] = n - 1;
}

static inline int cslSliceType(int t) {
  return t == TASSIGN || t == TINI || t == TDDT || t == TJAC || t == TMTIME;
}

// Where the variable line s assigns starts (-1 when none) and its length
static inline int cslLineDef(const char *s, int t, int *start) {
  *start = -1;
  if (!cslSliceType(t)) return 0;
  int len = pkTimeLhs(s, start);
  if (len <= 0) *start = -1;
  return len;
}

// 1 when the assignment at s[start] is a d/dt() value, 2 a Jacobian element
static inline int cslLineOut(const char *s, int start, int len) {
  if (start < 0) return 0;
  if (len > 13 && !strncmp(s + start, "__DDtStateVar", 13)) return 1;
  if (len > 12 && !strncmp(s + start, "__PDStateVar", 12)) return 2;
  return 0;
}

// May line i (type t) be dropped?  pinned: it has a side effect; single: it
// holds one statement
static inline int cslLineSlice(int i, int t, int pinned, int single) {
  if (pinned) return 0;
  // braces and conditions go with their chain
  if (t == TLOGIC) return _csl.span[i] >= 0;
  return cslSliceType(t) && _csl.def[i] >= 0 && single;
}

// Record the reads of line i and whether it may be dropped
static inline void cslBuildLine(int i, cslVec *rd) {
  const char *s = sbPmDt.line[i];
  int t = sbPm.lType[i];
  int start;
  int len = cslLineDef(s, t, &start);
  _csl.rdStart[i] = rd->n;
  int impure = 0, nSemi = 0, nBrace = 0;
  cslScanLine(s, start, rd, &impure, &nSemi, &nBrace);
  // reads of the dydt form too (the union is conservative)
  if (sbPm.line[i] != NULL && strcmp(sbPm.line[i], s)) {
    int start2, impure2 = 0, nSemi2 = 0, nBrace2 = 0;
    cslLineDef(sbPm.line[i], t, &start2);
    cslScanLine(sbPm.line[i], start2, rd, &impure2, &nSemi2, &nBrace2);
    impure = impure || impure2;
    nSemi = nSemi2 > nSemi ? nSemi2 : nSemi;
    nBrace = nBrace2 > nBrace ? nBrace2 : nBrace;
  }
  _csl.def[i] = (start >= 0) ? cslId(&_csl.h, s + start, len, 1) : -1;
  // a TDDT/TJAC line may also be an ordinary assignment that reads one
  _csl.out[i] = cslLineOut(s, start, len);
  _csl.slice[i] = cslLineSlice(i, t, impure || (start >= 0 &&
                                               cslIndField(s + start, len)),
                               nSemi <= 1 && nBrace == 0);
}

// Index the lines assigning each identifier
static inline void cslBuildDefLines(int n) {
  int nId = _csl.h.n;
  _csl.dlStart = (int*)rxMemRalloc(fixed, nId + 1, sizeof(int));
  for (int v = 0; v <= nId; ++v) _csl.dlStart[v] = 0;
  for (int i = 0; i < n; ++i) {
    if (_csl.def[i] >= 0) _csl.dlStart[_csl.def[i] + 1]++;
  }
  for (int v = 0; v < nId; ++v) _csl.dlStart[v + 1] += _csl.dlStart[v];
  int nDl = _csl.dlStart[nId];
  _csl.dl = (int*)rxMemRalloc(fixed, nDl > 0 ? nDl : 1, sizeof(int));
  int *fill = (int*)rxMemRalloc(fixed, nId > 0 ? nId : 1, sizeof(int));
  for (int v = 0; v < nId; ++v) fill[v] = _csl.dlStart[v];
  for (int i = 0; i < n; ++i) {
    if (_csl.def[i] >= 0) _csl.dl[fill[_csl.def[i]]++] = i;
  }
}

// Analyze the model once; _csl.ok says whether slicing may be used
static inline void cslBuild(void) {
  _csl.ok = 0;
  int n = sbPm.n;
  if (n <= 0 || sbPmDt.n != n) return;
  _csl.n = n;
  cslHashInit(&_csl.h, 1024);
  _csl.def = (int*)rxMemRalloc(fixed, n, sizeof(int));
  _csl.slice = (int*)rxMemRalloc(fixed, n, sizeof(int));
  _csl.out = (int*)rxMemRalloc(fixed, n, sizeof(int));
  _csl.rdStart = (int*)rxMemRalloc(fixed, n + 1, sizeof(int));
  cslFindSpans(n);
  cslVec rd = {NULL, 0, 0};
  for (int i = 0; i < n; ++i) cslBuildLine(i, &rd);
  _csl.rdStart[n] = rd.n;
  _csl.rd = rd.v;
  cslBuildDefLines(n);
  _csl.ok = 1;
}

static inline int cslIsMe(int show_ode) {
  return show_ode == ode_mexp || show_ode == ode_indLinVec;
}

static inline int cslIsDose(int show_ode) {
  return show_ode == ode_fbio || show_ode == ode_lag || show_ode == ode_rate ||
    show_ode == ode_dur || show_ode == ode_past;
}

// The one function emitting lines of type t; -1 when not a one-function type
static inline int cslOwner(int t) {
  switch (t) {
  case TF0: return ode_ini;
  case FBIO: return ode_fbio;
  case ALAG: return ode_lag;
  case RATE: return ode_rate;
  case DUR: return ode_dur;
  case PAST: return ode_past;
  case TMAT0: return ode_mexp;
  case TMATF: return ode_indLinVec;
  }
  return -1;
}

static inline int cslEmittedAssign(int show_ode, int i) {
  if (!(tb.isMexp || !cslIsMe(show_ode))) return 0;
  return !(show_ode == ode_past && strstr(sbPmDt.line[i], "_rxDelay") != NULL);
}

static inline int cslEmittedIni(int show_ode, int i) {
  if (cslIsMe(show_ode) || sbPm.lProp[i] < 0) return 0;
  int lh = tb.lh[sbPm.lProp[i]];
  return lh == isLHS || lh == isLHSstr || lh == isLHSparam;
}

static inline int cslEmittedDdt(int show_ode) {
  return !(show_ode == ode_ini || cslIsDose(show_ode) ||
           show_ode == ode_mtime || cslIsMe(show_ode));
}

// Lines that are not model statements; -1 for the other types
static inline int cslEmittedSpecial(int show_ode, int t) {
  switch (t) {
  case TEVID: return show_ode == ode_dydt || show_ode == ode_lhs;
  case TJAC: return show_ode == ode_lhs || show_ode == ode_jac;
  case TDDT: return cslEmittedDdt(show_ode);
  case PPRN: return fullPrint ? !cslIsMe(show_ode) : show_ode == ode_dydt;
  case TNONE: return 0;
  }
  return -1;
}

// Would codegen() emit line i into function show_ode?  Mirrors the switch in
// codegen(); erring towards 1 only keeps more lines.
static inline int cslEmitted(int show_ode, int i) {
  int t = sbPm.lType[i];
  int own = cslOwner(t);
  if (own >= 0) return show_ode == own;
  int sp = cslEmittedSpecial(show_ode, t);
  if (sp >= 0) return sp;
  int me = cslIsMe(show_ode);
  switch (t) {
  case TLIN: return (tb.isMexp || !me) && !cslIsDose(show_ode);
  case TMTIME: return !me;
  case TASSIGN: return cslEmittedAssign(show_ode, i);
  case TINI: return cslEmittedIni(show_ode, i);
  case TLOGIC: return tb.isMexp || !me;
  }
  return 1;
}

// Lines that are outputs of the function, whatever reads them
static inline int cslRootLine(int show_ode, int i) {
  return (show_ode == ode_dydt && _csl.out[i] == 1) ||
    (show_ode == ode_jac && _csl.out[i] == 2);
}

typedef struct cslWork {
  int *keep;
  int *emit;
  int *needed;
  int *spanDone;
  cslVec lines;
  cslVec vars;
} cslWork;

static inline void cslNeedVar(cslWork *w, int v) {
  if (v < 0 || w->needed[v]) return;
  w->needed[v] = 1;
  cslVecPush(&w->vars, v);
}

static inline void cslKeepLine(cslWork *w, int i) {
  if (w->keep[i] || !w->emit[i]) return;
  w->keep[i] = 1;
  cslVecPush(&w->lines, i);
}

static inline void cslNeedName(cslWork *w, const char *name) {
  cslNeedVar(w, cslIdStr(&_csl.h, name));
}

// The C spelling of variable i (as doDot() writes it), for the roots below
static inline void cslNeedVarIdx(cslWork *w, sbuf *buf, int i) {
  sClear(buf);
  doDot(buf, tb.ss.line[i]);
  cslNeedName(w, buf->s);
}

// Does variable i reach the epilogue of show_ode?
static inline int cslEpilogueVar(int show_ode, int i) {
  switch (show_ode) {
  case ode_lhs:
    return tb.lh[i] == isLHS || tb.lh[i] == isLhsStateExtra ||
      tb.lh[i] == isLHSparam || tb.lh[i] == isLHSstr;
  case ode_mtime:
    return tb.mtime[i] == 1;
  case ode_mexp: {
    char cmt1[100];
    char cmt2[100];
    return parse_micro_constant(tb.ss.line[i], cmt1, cmt2);
  }
  case ode_indLinVec:
    return strncmp(tb.ss.line[i], "rx_indLin_", 10) == 0;
  }
  return 0;
}

// Variables the function's epilogue reads
static inline void cslEpilogueRoots(cslWork *w, int show_ode) {
  sbuf buf;
  sNull(&buf);
  sIni(&buf);
  if (show_ode == ode_ini) {
    for (int i = 0; i < tb.de.n; i++) cslNeedVarIdx(w, &buf, tb.di[i]);
  } else {
    for (int i = 0; i < NV; i++) {
      if (cslEpilogueVar(show_ode, i)) cslNeedVarIdx(w, &buf, i);
    }
  }
  sFree(&buf);
}

static inline int *cslZeros(int n) {
  int *v = (int*)rxMemRalloc(fixed, n > 0 ? n : 1, sizeof(int));
  for (int i = 0; i < n; ++i) v[i] = 0;
  return v;
}

// A kept line needs what it reads and the rest of its if/else/while chain
static inline void cslKeptLine(cslWork *w, int i) {
  for (int k = _csl.rdStart[i]; k < _csl.rdStart[i + 1]; ++k) {
    cslNeedVar(w, _csl.rd[k]);
  }
  int s = _csl.span[i];
  if (s < 0 || w->spanDone[s]) return;
  w->spanDone[s] = 1;
  for (int j = _csl.spanFirst[s]; j <= _csl.spanLast[s]; ++j) cslKeepLine(w, j);
}

// Backward closure from the roots already queued
static inline void cslClosure(cslWork *w) {
  while (w->lines.n > 0 || w->vars.n > 0) {
    if (w->lines.n > 0) {
      cslKeptLine(w, w->lines.v[--w->lines.n]);
    } else {
      int v = w->vars.v[--w->vars.n];
      for (int k = _csl.dlStart[v]; k < _csl.dlStart[v + 1]; ++k) {
        cslKeepLine(w, _csl.dl[k]);
      }
    }
  }
}

// Fill _cslKeep for function show_ode (NULL when slicing is off)
static inline void cslForFunction(int show_ode) {
  _cslKeep = NULL;
  if (_cslLevel < 1 || !_csl.ok) return;
  if (ode_is_es_dcode(show_ode) || show_ode == ode_printaux) return;
  int n = _csl.n;
  cslWork w;
  w.keep = cslZeros(n);
  w.emit = (int*)rxMemRalloc(fixed, n, sizeof(int));
  w.needed = cslZeros(_csl.h.n);
  w.spanDone = cslZeros(_csl.nSpan);
  w.lines = (cslVec){NULL, 0, 0};
  w.vars = (cslVec){NULL, 0, 0};
  for (int i = 0; i < n; ++i) w.emit[i] = cslEmitted(show_ode, i);
  for (int i = 0; i < n; ++i) {
    if (w.emit[i] && (!_csl.slice[i] || cslRootLine(show_ode, i))) {
      cslKeepLine(&w, i);
    }
  }
  cslEpilogueRoots(&w, show_ode);
  cslClosure(&w);
  // lines this function does not emit are left to the codegen() switch
  for (int i = 0; i < n; ++i) {
    if (!w.emit[i]) w.keep[i] = 1;
  }
  _cslKeep = w.keep;
}

// Forget everything from the previous model (its R_alloc memory is gone)
// codegenHoist.h
static inline void chReset(void);
static inline void chBuildModel(int on);

static inline void cslReset(void) {
  chReset();
  _cslKeep = NULL;
  _cslLevel = 0;
  _csl.ok = 0;
  _cslImpure.cap = 0;
  _cslImpure.n = 0;
}

SEXP getRxode2FullOpt(void);

// Read the level and the impure call names, then analyze the parsed model
static inline void cslSetup(void) {
  cslReset();
  rxProtectGuard;
  SEXP info = rxP(getRxode2FullOpt());
  if (TYPEOF(info) != VECSXP || Rf_length(info) < 2) {
    rxUPAll();
    return;
  }
  int level = Rf_asInteger(VECTOR_ELT(info, 0));
  SEXP imp = VECTOR_ELT(info, 1);
  // parameter-only hoisting (codegenHoist.h) uses the same analysis
  int hoist = Rf_length(info) >= 3 && Rf_asLogical(VECTOR_ELT(info, 2)) == TRUE;
  if (level == NA_INTEGER) level = 0;
  if ((level < 1 && !hoist) || TYPEOF(imp) != STRSXP) {
    rxUPAll();
    return;
  }
  int ni = Rf_length(imp);
  int cap = 64;
  while (cap < 4 * (ni + 1)) cap *= 2;
  cslHashInit(&_cslImpure, cap);
  for (int i = 0; i < ni; ++i) {
    const char *c = CHAR(STRING_ELT(imp, i));
    cslId(&_cslImpure, c, (int)strlen(c), 1);
  }
  rxUPAll();
  cslBuild();
  if (_csl.ok) _cslLevel = level;
  chBuildModel(hoist);
}

#endif // __CODEGENSLICE_H__
