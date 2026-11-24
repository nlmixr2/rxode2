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
  h->key = (char**)R_alloc(cap, sizeof(char*));
  h->id = (int*)R_alloc(cap, sizeof(int));
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
  char *c = R_alloc(len + 1, sizeof(char));
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
    int *v = (int*)R_alloc(cap, sizeof(int));
    for (int i = 0; i < x->n; ++i) v[i] = x->v[i];
    x->v = v;
    x->cap = cap;
  }
  x->v[x->n++] = val;
}

// Impure call names (from .rxFullOptInfo()); a line calling one is kept
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

static inline int cslIsKeyword(const char *s, int len) {
  return (len == 2 && !strncmp(s, "if", 2)) ||
    (len == 4 && !strncmp(s, "else", 4)) ||
    (len == 5 && !strncmp(s, "while", 5)) ||
    (len == 3 && !strncmp(s, "for", 3)) ||
    (len == 6 && !strncmp(s, "return", 6)) ||
    (len == 6 && !strncmp(s, "sizeof", 6)) ||
    (len == 5 && !strncmp(s, "break", 5)) ||
    (len == 6 && !strncmp(s, "double", 6)) ||
    (len == 3 && !strncmp(s, "int", 3));
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
      continue;
    }
    char c = s[k];
    if (pkTimeIdStart(c)) {
      j = k;
      while (pkTimeIdChar(s[j])) j++;
      int len = j - k;
      // struct members are not model variables
      int member = (k > 0 && s[k-1] == '.') ||
        (k > 1 && s[k-1] == '>' && s[k-2] == '-');
      int m = j;
      while (s[m] == ' ' || s[m] == '\t') m++;
      if (s[m] == '(' && !cslIsKeyword(s + k, len)) {
        if (cslIsImpureCall(s + k, len)) *impure = 1;
      } else if (!member && k != defStart && !cslIsKeyword(s + k, len)) {
        cslVecPush(rd, cslId(&_csl.h, s + k, len, 1));
      }
      k = j;
      continue;
    }
    if (c == ';') (*nSemi)++;
    else if (c == '{' || c == '}') (*nBrace)++;
    k++;
  }
}

// Top-level if/else/while chains: an `else` opening at depth 0 joins the chain
// that just closed.
static inline void cslFindSpans(int n) {
  _csl.span = (int*)R_alloc(n, sizeof(int));
  _csl.spanFirst = (int*)R_alloc(n, sizeof(int));
  _csl.spanLast = (int*)R_alloc(n, sizeof(int));
  _csl.nSpan = 0;
  int depth = 0, cur = -1, prev = -1;
  for (int i = 0; i < n; ++i) {
    int touched = 0;
    if (sbPm.lType[i] == TLOGIC) {
      const char *s = sbPm.line[i];
      int k = 0;
      while (s[k] != '\0') {
        int j = pkTimeSkipLiteral(s, k);
        if (j != k) {
          k = j;
          continue;
        }
        if (s[k] == '{') {
          if (depth == 0) {
            if (prev >= 0 && pkTimeHasElse(s, k)) {
              cur = prev;
            } else {
              cur = _csl.nSpan++;
              _csl.spanFirst[cur] = i;
            }
          }
          depth++;
          touched = 1;
        } else if (s[k] == '}' && depth > 0) {
          depth--;
          touched = 1;
          if (depth == 0) {
            _csl.spanLast[cur] = i;
            prev = cur;
          }
        }
        k++;
      }
    }
    if (depth > 0 || touched) {
      _csl.span[i] = cur;
      _csl.spanLast[cur] = i;
    } else {
      _csl.span[i] = -1;
      // only an `else` directly after the chain may join it
      if (sbPm.lType[i] != TLOGIC) prev = -1;
    }
  }
  if (depth > 0 && cur >= 0) _csl.spanLast[cur] = n - 1;
}

static inline int cslSliceType(int t) {
  return t == TASSIGN || t == TINI || t == TDDT || t == TJAC || t == TMTIME;
}

// Analyze the model once; _csl.ok says whether slicing may be used
static inline void cslBuild(void) {
  _csl.ok = 0;
  int n = sbPm.n;
  if (n <= 0 || sbPmDt.n != n) return;
  _csl.n = n;
  cslHashInit(&_csl.h, 1024);
  _csl.def = (int*)R_alloc(n, sizeof(int));
  _csl.slice = (int*)R_alloc(n, sizeof(int));
  _csl.out = (int*)R_alloc(n, sizeof(int));
  _csl.rdStart = (int*)R_alloc(n + 1, sizeof(int));
  cslFindSpans(n);
  cslVec rd = {NULL, 0, 0};
  for (int i = 0; i < n; ++i) {
    const char *s = sbPmDt.line[i];
    int t = sbPm.lType[i];
    int start = -1, len = 0;
    if (cslSliceType(t)) {
      len = pkTimeLhs(s, &start);
      if (len <= 0) start = -1;
    }
    _csl.rdStart[i] = rd.n;
    int impure = 0, nSemi = 0, nBrace = 0;
    cslScanLine(s, start, &rd, &impure, &nSemi, &nBrace);
    // reads of the dydt form too (the union is conservative)
    if (sbPm.line[i] != NULL && strcmp(sbPm.line[i], s)) {
      int start2 = -1, impure2 = 0, nSemi2 = 0, nBrace2 = 0;
      if (cslSliceType(t) && pkTimeLhs(sbPm.line[i], &start2) <= 0) start2 = -1;
      cslScanLine(sbPm.line[i], start2, &rd, &impure2, &nSemi2, &nBrace2);
      impure = impure || impure2;
      if (nSemi2 > nSemi) nSemi = nSemi2;
      if (nBrace2 > nBrace) nBrace = nBrace2;
    }
    _csl.def[i] = (start >= 0) ? cslId(&_csl.h, s + start, len, 1) : -1;
    // a TDDT/TJAC line may also be an ordinary assignment that reads one
    _csl.out[i] = 0;
    if (start >= 0 && len > 13 && !strncmp(s + start, "__DDtStateVar", 13)) {
      _csl.out[i] = 1;
    } else if (start >= 0 && len > 12 && !strncmp(s + start, "__PDStateVar", 12)) {
      _csl.out[i] = 2;
    }
    if (impure || (start >= 0 && cslIndField(s + start, len))) {
      _csl.slice[i] = 0;
    } else if (t == TLOGIC) {
      // braces and conditions go with their chain
      _csl.slice[i] = (_csl.span[i] >= 0);
    } else {
      _csl.slice[i] = cslSliceType(t) && _csl.def[i] >= 0 && nSemi <= 1 &&
        nBrace == 0;
    }
  }
  _csl.rdStart[n] = rd.n;
  _csl.rd = rd.v;
  // lines assigning each identifier
  int nId = _csl.h.n;
  _csl.dlStart = (int*)R_alloc(nId + 1, sizeof(int));
  for (int v = 0; v <= nId; ++v) _csl.dlStart[v] = 0;
  for (int i = 0; i < n; ++i) {
    if (_csl.def[i] >= 0) _csl.dlStart[_csl.def[i] + 1]++;
  }
  for (int v = 0; v < nId; ++v) _csl.dlStart[v + 1] += _csl.dlStart[v];
  int nDl = _csl.dlStart[nId];
  _csl.dl = (int*)R_alloc(nDl > 0 ? nDl : 1, sizeof(int));
  int *fill = (int*)R_alloc(nId > 0 ? nId : 1, sizeof(int));
  for (int v = 0; v < nId; ++v) fill[v] = _csl.dlStart[v];
  for (int i = 0; i < n; ++i) {
    if (_csl.def[i] >= 0) _csl.dl[fill[_csl.def[i]]++] = i;
  }
  _csl.ok = 1;
}

// Would codegen() emit line i into function show_ode?  Mirrors the switch in
// codegen(); erring towards 1 only keeps more lines.
static inline int cslEmitted(int show_ode, int i) {
  int t = sbPm.lType[i];
  int me = (show_ode == ode_mexp || show_ode == ode_indLinVec);
  int dose = (show_ode == ode_fbio || show_ode == ode_lag ||
              show_ode == ode_rate || show_ode == ode_dur || show_ode == ode_past);
  switch (t) {
  case TLIN:
    return (tb.isMexp || !me) && !dose;
  case TMTIME:
    return !me;
  case TEVID:
    return show_ode == ode_dydt || show_ode == ode_lhs;
  case TASSIGN:
    if (!(tb.isMexp || !me)) return 0;
    if (show_ode == ode_past && strstr(sbPmDt.line[i], "_rxDelay") != NULL) return 0;
    return 1;
  case TINI:
    if (me || sbPm.lProp[i] < 0) return 0;
    return tb.lh[sbPm.lProp[i]] == isLHS || tb.lh[sbPm.lProp[i]] == isLHSstr ||
      tb.lh[sbPm.lProp[i]] == isLHSparam;
  case TF0: return show_ode == ode_ini;
  case FBIO: return show_ode == ode_fbio;
  case ALAG: return show_ode == ode_lag;
  case RATE: return show_ode == ode_rate;
  case DUR: return show_ode == ode_dur;
  case PAST: return show_ode == ode_past;
  case TJAC: return show_ode == ode_lhs || show_ode == ode_jac;
  case TDDT:
    return !(show_ode == ode_ini || dose || show_ode == ode_mtime || me);
  case PPRN:
    return (fullPrint && !me) || (!fullPrint && show_ode == ode_dydt);
  case TLOGIC:
    return tb.isMexp || !me;
  case TMAT0: return show_ode == ode_mexp;
  case TMATF: return show_ode == ode_indLinVec;
  case TNONE: return 0;
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

// Variables the function's epilogue reads
static inline void cslEpilogueRoots(cslWork *w, int show_ode) {
  sbuf buf;
  sNull(&buf);
  sIni(&buf);
  if (show_ode == ode_lhs) {
    for (int i = 0; i < NV; i++) {
      if (tb.lh[i] == isLHS || tb.lh[i] == isLhsStateExtra ||
          tb.lh[i] == isLHSparam || tb.lh[i] == isLHSstr) {
        cslNeedVarIdx(w, &buf, i);
      }
    }
  } else if (show_ode == ode_mtime) {
    for (int i = 0; i < NV; i++) {
      if (tb.mtime[i] == 1) cslNeedVarIdx(w, &buf, i);
    }
  } else if (show_ode == ode_ini) {
    for (int i = 0; i < tb.de.n; i++) cslNeedVarIdx(w, &buf, tb.di[i]);
  } else if (show_ode == ode_mexp) {
    for (int j = 0; j < NV; j++) {
      char cmt1[100];
      char cmt2[100];
      if (parse_micro_constant(tb.ss.line[j], cmt1, cmt2)) {
        cslNeedVarIdx(w, &buf, j);
      }
    }
  } else if (show_ode == ode_indLinVec) {
    for (int j = 0; j < NV; j++) {
      if (strncmp(tb.ss.line[j], "rx_indLin_", 10) == 0) {
        cslNeedVarIdx(w, &buf, j);
      }
    }
  }
  sFree(&buf);
}

// Fill _cslKeep for function show_ode (NULL when slicing is off)
static inline void cslForFunction(int show_ode) {
  _cslKeep = NULL;
  if (_cslLevel < 1 || !_csl.ok) return;
  if (ode_is_es_dcode(show_ode) || show_ode == ode_printaux) return;
  int n = _csl.n;
  int nId = _csl.h.n;
  cslWork w;
  w.keep = (int*)R_alloc(n, sizeof(int));
  w.emit = (int*)R_alloc(n, sizeof(int));
  w.needed = (int*)R_alloc(nId > 0 ? nId : 1, sizeof(int));
  w.spanDone = (int*)R_alloc(_csl.nSpan > 0 ? _csl.nSpan : 1, sizeof(int));
  w.lines.v = NULL; w.lines.n = 0; w.lines.cap = 0;
  w.vars.v = NULL; w.vars.n = 0; w.vars.cap = 0;
  for (int v = 0; v < nId; ++v) w.needed[v] = 0;
  for (int s = 0; s < _csl.nSpan; ++s) w.spanDone[s] = 0;
  for (int i = 0; i < n; ++i) {
    w.keep[i] = 0;
    w.emit[i] = cslEmitted(show_ode, i);
  }
  for (int i = 0; i < n; ++i) {
    if (w.emit[i] && (!_csl.slice[i] || cslRootLine(show_ode, i))) {
      cslKeepLine(&w, i);
    }
  }
  cslEpilogueRoots(&w, show_ode);
  while (w.lines.n > 0 || w.vars.n > 0) {
    if (w.lines.n > 0) {
      int i = w.lines.v[--w.lines.n];
      for (int k = _csl.rdStart[i]; k < _csl.rdStart[i + 1]; ++k) {
        cslNeedVar(&w, _csl.rd[k]);
      }
      int s = _csl.span[i];
      if (s >= 0 && !w.spanDone[s]) {
        w.spanDone[s] = 1;
        for (int j = _csl.spanFirst[s]; j <= _csl.spanLast[s]; ++j) {
          cslKeepLine(&w, j);
        }
      }
    } else {
      int v = w.vars.v[--w.vars.n];
      for (int k = _csl.dlStart[v]; k < _csl.dlStart[v + 1]; ++k) {
        cslKeepLine(&w, _csl.dl[k]);
      }
    }
  }
  // lines this function does not emit are left to the codegen() switch
  for (int i = 0; i < n; ++i) {
    if (!w.emit[i]) w.keep[i] = 1;
  }
  _cslKeep = w.keep;
}

// Forget everything from the previous model (its R_alloc memory is gone)
static inline void cslReset(void) {
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
  if (level == NA_INTEGER || level < 1 || TYPEOF(imp) != STRSXP) {
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
}

#endif // __CODEGENSLICE_H__
