#ifndef __CODEGENHOIST_H__
#define __CODEGENHOIST_H__
// Parameter-only hoisting (options(rxode2.hoistPar), default TRUE).
//
// dydt(), calc_jac() and calc_lhs() used to recompute at every call what
// reads only parameters: exp(theta + eta), (WT/70)^theta, ...  Here those
// items are evaluated in a block before the model lines and cached per thread
// and function: each call compares (bitwise) the parameters the items read
// with the previous call on this thread and recomputes only the items reading
// a changed one.  A new subject, new thetas/etas or a new interpolated
// covariate value recompute what depends on them.  The items are the same C
// expressions on the same inputs, and a call is replaced only by the variable
// holding its value, so the results are bitwise identical where the compiler
// does not contract FMA across statements (x86-64 without -mfma/-march=native,
// clang's default); otherwise they are deterministic per build.
//
// Items: (1) a top-level, single-statement pure assignment `v = expr;` whose v
// is assigned once and never read before it; (2) a maximal call f(args) inside
// any other emitted line.  expr/args read only parameters (set from _PP and
// never assigned), earlier hoisted variables and C constants, and call only
// the pure functions below.  Key slot 0 holds the solve options the
// Rx_pow()/_safe_log()/_div0() helpers read.
//
// Uses the line analysis of codegenSlice.h (_csl).

static int _chOn = 0;
static int *_chPar = NULL;     // csl id -> 1 for a never-assigned parameter
static int *_chFirstRd = NULL; // csl id -> first line reading it
static int *_chWrites = NULL;  // csl id -> number of lines writing it
static cslHash _chVarNames;    // C names of the model variables
static const char **_chName = NULL; // csl id -> C name

// A hoisted call also runs when the branch (if/else, ?:) that held it is not
// taken, so only functions that never call into R qualify: no Rf_warning /
// Rf_error / longjmp (MATHLIB_WARNING from an OpenMP worker crashes R), no
// global writes.  Left out on purpose: gammafn, lgammafn, lgamma1p, beta,
// lbeta, digamma, trigamma, pnorm, qnorm, probit/probitInv (R nmath, may warn)
// and lgamma (writes signgam).
static const char *_chPure[] = {
  "exp", "log", "log1p", "expm1", "log10", "log2", "sqrt", "cbrt", "pow",
  "R_pow", "R_pow_di", "Rx_pow", "Rx_pow_di", "_safe_log", "_div0", "fabs",
  "hypot", "sin", "cos", "tan", "asin", "acos", "atan", "atan2", "sinh", "cosh",
  "tanh", "asinh", "acosh", "atanh", "erf", "erfc", "tgamma",
  "floor", "ceil", "trunc", "fmin", "fmax", "expit", "logit", "_expit1",
  "_expit2", "_logit1", "_logit2", "_invLogit1", "_invLogit2", "_logitInv1",
  "_logitInv2"};

// cheap enough that a call of only these is not worth a cache slot
static const char *_chCheap[] = {"_div0", "fabs", "floor", "ceil", "trunc", "fmin", "fmax"};

static const char *_chConst[] = {
  "M_PI", "M_E", "M_LN2", "M_LN10", "M_SQRT2", "M_1_PI", "M_2_PI", "M_2PI",
  "M_PI_2", "M_PI_4", "M_SQRT_PI", "M_1_SQRT_2PI", "M_SQRT_2dPI", "M_LN_SQRT_2PI",
  "M_LN_SQRT_PI", "M_LN_SQRT_PId2", "M_LOG10_2", "DBL_EPSILON", "DBL_MAX",
  "DBL_MIN", "NA_REAL", "R_NaN", "R_PosInf", "R_NegInf"};

static inline int chInList(const char **l, int n, const char *s, int len) {
  for (int i = 0; i < n; ++i) {
    if ((int)strlen(l[i]) == len && !strncmp(l[i], s, len)) return 1;
  }
  return 0;
}

#define chN(x) ((int)(sizeof(x) / sizeof((x)[0])))

typedef struct chItem {
  int line;
  int def;          // csl id a hoisted line assigns; -1 for a call
  const char *text; // the line, or the call (own copy)
  unsigned long long mask;
} chItem;

typedef struct chOcc {
  int line, start, end, item; // call text s[start..end) of line
} chOcc;

typedef struct chFun {
  int on;
  int n, cap;
  chItem *it;
  int nOcc, capOcc;
  chOcc *occ;
  int *occFirst;      // line -> first occurrence, -1 when none
  int *lineItem;      // line -> item of a hoisted line, -1 otherwise
  const char **text;  // line -> the text analyzed
  int *defItem;       // csl id -> item assigning it, -1 otherwise
  int *slotOf;        // csl id -> key slot, -1 otherwise
  cslVec slotId;      // key slot k+1 -> csl id
} chFun;

static chFun _ch;
static sbuf _chBuf;
static int _chBufIni = 0;

static inline unsigned long long chBit(int slot) {
  return 1ULL << (slot < 63 ? slot : 63);
}

// The read s[0..len): 1 when a parameter, hoisted variable or constant
static inline int chRead(const char *s, int len, cslVec *pv,
                         unsigned long long *hm) {
  int id = cslId(&_csl.h, s, len, 0);
  if (id >= 0 && _chPar[id]) {
    cslVecPush(pv, id);
    return 1;
  }
  if (id >= 0 && _ch.defItem[id] >= 0) {
    *hm |= _ch.it[_ch.defItem[id]].mask;
    return 1;
  }
  return chInList(_chConst, chN(_chConst), s, len) &&
    cslId(&_chVarNames, s, len, 0) < 0;
}

// The operator at s[k]: 0 when it writes or indexes
static inline int chOpOk(const char *s, int k, int a) {
  char c = s[k];
  if (c == '=') {
    int cmp = (s[k+1] == '=') ||
      (k > a && (s[k-1] == '=' || s[k-1] == '<' || s[k-1] == '>' || s[k-1] == '!'));
    return cmp;
  }
  if ((c == '+' && s[k+1] == '+') || (c == '-' && s[k+1] == '-')) return 0;
  if (c == '&') return s[k+1] == '&' || (k > a && s[k-1] == '&');
  return !(c == ';' || c == '{' || c == '}' || c == '[' || c == ']' ||
           c == '.');
}

// s[a..b) reads only parameters, hoisted variables and constants and calls
// only pure functions; the parameters it reads go on *pv, the masks of the
// hoisted variables into *hm
static inline int chScan(const char *s, int a, int b, cslVec *pv,
                         unsigned long long *hm) {
  int k = a;
  while (k < b) {
    int j = pkTimeSkipLiteral(s, k);
    if (j != k) {
      if (s[k] == '"' || s[k] == '\'') return 0;
      k = j;
      continue;
    }
    if (!pkTimeIdStart(s[k])) {
      if (!chOpOk(s, k, a)) return 0;
      k++;
      continue;
    }
    while (pkTimeIdChar(s[j])) j++;
    int len = j - k;
    int m = j;
    while (s[m] == ' ' || s[m] == '\t') m++;
    if (cslIsMember(s, k)) return 0;
    if (s[m] == '(') {
      if (!chInList(_chPure, chN(_chPure), s + k, len)) {
        // a cast is fine, any other keyword or call is not
        if (!((len == 6 && !strncmp(s + k, "double", 6)) ||
              (len == 3 && !strncmp(s + k, "int", 3)))) return 0;
      }
    } else if (cslIsKeyword(s + k, len)) {
      if (!((len == 6 && !strncmp(s + k, "double", 6)) ||
            (len == 3 && !strncmp(s + k, "int", 3)))) return 0;
    } else if (!chRead(s + k, len, pv, hm)) {
      return 0;
    }
    k = j;
  }
  return 1;
}

// Index of the `)` closing the `(` at s[m]; -1 when none
static inline int chParenEnd(const char *s, int m) {
  int depth = 0;
  int k = m;
  while (s[k] != '\0') {
    int j = pkTimeSkipLiteral(s, k);
    if (j != k) {
      k = j;
      continue;
    }
    if (s[k] == '(') depth++;
    else if (s[k] == ')' && --depth == 0) return k;
    k++;
  }
  return -1;
}

// Mask of an accepted item reading parameters pv and hoisted masks hm
static inline unsigned long long chMask(cslVec *pv, unsigned long long hm) {
  unsigned long long mask = hm | chBit(0);
  for (int k = 0; k < pv->n; ++k) {
    int id = pv->v[k];
    if (_ch.slotOf[id] < 0) {
      cslVecPush(&_ch.slotId, id);
      _ch.slotOf[id] = _ch.slotId.n; // slot 0 is the options
    }
    mask |= chBit(_ch.slotOf[id]);
  }
  return mask;
}

static inline int chAddItem(int line, int def, const char *text,
                            unsigned long long mask) {
  if (_ch.n + 1 > _ch.cap) {
    int cap = _ch.cap == 0 ? 64 : 2 * _ch.cap;
    chItem *it = (chItem*)R_alloc(cap, sizeof(chItem));
    for (int k = 0; k < _ch.n; ++k) it[k] = _ch.it[k];
    _ch.it = it;
    _ch.cap = cap;
  }
  _ch.it[_ch.n] = (chItem){line, def, text, mask};
  return _ch.n++;
}

static inline void chAddOcc(int line, int start, int end, int item) {
  if (_ch.nOcc + 1 > _ch.capOcc) {
    int cap = _ch.capOcc == 0 ? 64 : 2 * _ch.capOcc;
    chOcc *o = (chOcc*)R_alloc(cap, sizeof(chOcc));
    for (int k = 0; k < _ch.nOcc; ++k) o[k] = _ch.occ[k];
    _ch.occ = o;
    _ch.capOcc = cap;
  }
  if (_ch.occFirst[line] < 0) _ch.occFirst[line] = _ch.nOcc;
  _ch.occ[_ch.nOcc++] = (chOcc){line, start, end, item};
}

// Line i (text s) as a hoisted line
static inline int chLineItem(int i, const char *s) {
  if (sbPm.lType[i] != TASSIGN || !_csl.slice[i] || _csl.span[i] >= 0 ||
      _csl.out[i] != 0) return 0;
  int v = _csl.def[i];
  if (v < 0 || _chWrites[v] != 1 || _chFirstRd[v] <= i) return 0;
  if (_pkLine != NULL && _pkLine[i]) return 0;
  int start;
  int len = pkTimeLhs(s, &start);
  if (len <= 0 || cslId(&_csl.h, s + start, len, 0) != v) return 0;
  int a = start + len;
  while (s[a] != '=') a++;
  a++;
  int b = (int)strlen(s);
  while (b > a && (s[b-1] == '\n' || s[b-1] == ' ' || s[b-1] == '\t')) b--;
  if (b <= a || s[b-1] != ';') return 0;
  b--;
  cslVec pv = {NULL, 0, 0};
  unsigned long long hm = 0;
  if (!chScan(s, a, b, &pv, &hm)) return 0;
  int it = chAddItem(i, v, s, chMask(&pv, hm));
  _ch.lineItem[i] = it;
  _ch.defItem[v] = it;
  return 1;
}

// Does s[a..b) call anything not in _chCheap?
static inline int chCostly(const char *s, int a, int b) {
  for (int k = a; k < b; ++k) {
    int j = pkTimeSkipLiteral(s, k);
    if (j != k) {
      k = j - 1;
      continue;
    }
    if (!pkTimeIdStart(s[k])) continue;
    while (pkTimeIdChar(s[j])) j++;
    int m = j;
    while (s[m] == ' ' || s[m] == '\t') m++;
    if (s[m] == '(' && !chInList(_chCheap, chN(_chCheap), s + k, j - k)) return 1;
    k = j - 1;
  }
  return 0;
}

// The item computing call text c[0..len), added when new
static inline int chCallItem(int i, const char *c, int len,
                             unsigned long long mask) {
  for (int k = 0; k < _ch.n; ++k) {
    if (_ch.it[k].def < 0 && (int)strlen(_ch.it[k].text) == len &&
        !strncmp(_ch.it[k].text, c, len)) return k;
  }
  char *t = R_alloc(len + 1, sizeof(char));
  memcpy(t, c, len);
  t[len] = '\0';
  return chAddItem(i, -1, t, mask);
}

// The maximal pure, parameter-only calls of line i (text s)
static inline void chLineCalls(int i, const char *s) {
  int k = 0;
  while (s[k] != '\0') {
    int j = pkTimeSkipLiteral(s, k);
    if (j != k) {
      k = j;
      continue;
    }
    if (!pkTimeIdStart(s[k])) {
      k++;
      continue;
    }
    while (pkTimeIdChar(s[j])) j++;
    int m = j;
    while (s[m] == ' ' || s[m] == '\t') m++;
    if (s[m] == '(' && !cslIsMember(s, k) &&
        chInList(_chPure, chN(_chPure), s + k, j - k)) {
      int e = chParenEnd(s, m);
      cslVec pv = {NULL, 0, 0};
      unsigned long long hm = 0;
      if (e > 0 && chScan(s, m + 1, e, &pv, &hm) && chCostly(s, k, e)) {
        int it = chCallItem(i, s + k, e + 1 - k, chMask(&pv, hm));
        chAddOcc(i, k, e + 1, it);
        k = e + 1;
        continue;
      }
    }
    k = j;
  }
}

// The text codegen() emits for line i into show_ode; NULL for the types not
// rewritten (mirrors the switch in codegen())
static inline const char *chText(int show_ode, int i) {
  int dy = show_ode == ode_dydt;
  switch (sbPm.lType[i]) {
  case TASSIGN:
  case TLOGIC:
  case TINI:
  case TDDT:
    return dy ? sbPm.line[i] : sbPmDt.line[i];
  case TJAC:
    return show_ode == ode_jac ? sbPm.line[i] : sbPmDt.line[i];
  }
  return NULL;
}

// Find the items of function show_ode (after cslForFunction())
static inline void chForFunction(int show_ode) {
  _ch.on = 0;
  if (!_chOn || _rxgDev || !_csl.ok) return;
  if (show_ode != ode_dydt && show_ode != ode_jac && show_ode != ode_lhs) return;
  int n = _csl.n;
  int nId = _csl.h.n;
  _ch.n = _ch.cap = _ch.nOcc = _ch.capOcc = 0;
  _ch.it = NULL;
  _ch.occ = NULL;
  _ch.occFirst = (int*)R_alloc(n, sizeof(int));
  _ch.lineItem = (int*)R_alloc(n, sizeof(int));
  _ch.text = (const char**)R_alloc(n, sizeof(char*));
  _ch.defItem = (int*)R_alloc(nId > 0 ? nId : 1, sizeof(int));
  _ch.slotOf = (int*)R_alloc(nId > 0 ? nId : 1, sizeof(int));
  _ch.slotId = (cslVec){NULL, 0, 0};
  for (int v = 0; v < nId; ++v) _ch.defItem[v] = _ch.slotOf[v] = -1;
  for (int i = 0; i < n; ++i) {
    _ch.occFirst[i] = _ch.lineItem[i] = -1;
    _ch.text[i] = NULL;
    if (!cslEmitted(show_ode, i) || (_cslKeep != NULL && !_cslKeep[i])) continue;
    const char *s = chText(show_ode, i);
    if (s == NULL) continue;
    _ch.text[i] = s;
    if (!chLineItem(i, s)) chLineCalls(i, s);
  }
  _ch.on = _ch.n > 0;
}

// The hoisted block, written before the model lines
static inline void chWriteBlock(void) {
  if (!_ch.on) return;
  int nSlot = _ch.slotId.n + 1;
  sAppend(&sbOut, "  // parameter-only items, cached per thread (options(rxode2.hoistPar))\n");
  // one thread-local struct, its address looked up once per call (pinned:
  // at -O3 gcc otherwise redoes __tls_get_addr at every use)
  sAppend(&sbOut, "  static _rxHoistTLS struct _rxHtS { unsigned long long k[%d]; double v[%d]; int ok; } _rxHt;\n",
          nSlot, _ch.n);
  sAppend(&sbOut, "  struct _rxHtS *_rxHp = &_rxHt;\n  _rxHoistPin(_rxHp);\n");
  sAppend(&sbOut, "  unsigned long long *_rxHk = _rxHp->k;\n  double *_rxHv = _rxHp->v;\n  int *_rxHok = &_rxHp->ok;\n");
  for (int k = 0; k < _ch.n; ++k) {
    if (_ch.it[k].def < 0) sAppend(&sbOut, "  double _rxHc%d;\n", k);
  }
  sAppend(&sbOut, "  unsigned long long _rxHm = *_rxHok ? 0ULL : ~0ULL;\n");
  sAppend(&sbOut, "  _rxHoistKey(0, _rxHoistOpt);\n");
  for (int k = 1; k < nSlot; ++k) {
    sAppend(&sbOut, "  _rxHoistKey(%d, %s);\n", k, _chName[_ch.slotId.v[k - 1]]);
  }
  sAppend(&sbOut, "  if (_rxHm) *_rxHok = 0;\n");
  for (int k = 0; k < _ch.n; ++k) {
    chItem *it = &_ch.it[k];
    if (it->def >= 0) {
      const char *s = it->text;
      int b = (int)strlen(s);
      while (b > 0 && (s[b-1] == '\n' || s[b-1] == ' ' || s[b-1] == '\t')) b--;
      const char *v = _chName[it->def];
      sAppend(&sbOut, "  if (_rxHm & 0x%llxULL) { %.*s _rxHv[%d] = %s; } else %s = _rxHv[%d];\n",
              it->mask, b, s, k, v, v, k);
    } else {
      sAppend(&sbOut, "  if (_rxHm & 0x%llxULL) { _rxHc%d = %s; _rxHv[%d] = _rxHc%d; } else _rxHc%d = _rxHv[%d];\n",
              it->mask, k, it->text, k, k, k, k);
    }
  }
  sAppend(&sbOut, "  *_rxHok = 1;\n\n");
}

// Line i as written into the function: NULL for a hoisted line, the text
// with its hoisted calls replaced, or s itself
static inline const char *chLine(int i, const char *s) {
  if (!_ch.on || s == NULL || _ch.text[i] != s) return s;
  if (_ch.lineItem[i] >= 0) return NULL;
  int o = _ch.occFirst[i];
  if (o < 0) return s;
  if (!_chBufIni) {
    sIni(&_chBuf);
    _chBufIni = 1;
  }
  sClear(&_chBuf);
  int k = 0;
  for (; o < _ch.nOcc && _ch.occ[o].line == i; ++o) {
    chOcc *c = &_ch.occ[o];
    sAppend(&_chBuf, "%.*s_rxHc%d", c->start - k, s + k, c->item);
    k = c->end;
  }
  sAppend(&_chBuf, "%s", s + k);
  return _chBuf.s;
}

// Helpers the blocks use, in the file header
static inline void chWriteHeader(void) {
  if (!_chOn) return;
  sAppend(&sbOut, "#include <string.h>\n#ifndef _rxHoistTLS\n#if defined(_MSC_VER)\n#define _rxHoistTLS __declspec(thread)\n#else\n#define _rxHoistTLS __thread\n#endif\n#endif\n");
  sAppend(&sbOut, "#if defined(__GNUC__) || defined(__clang__)\n#define _rxHoistPin(p) __asm__(\"\" : \"+r\"(p))\n#else\n#define _rxHoistPin(p) (void)(p)\n#endif\n");
  sAppend(&sbOut, "// the solve options Rx_pow(), _safe_log() and _div0() read\n#define _rxHoistOpt ((double)(_solveData->safeZero + 4*_solveData->safeLog + 16*_solveData->safePow + 64*_solveData->useStdPow))\n");
  sAppend(&sbOut, "// key slot k: x bitwise as on this thread's last call, else mark it changed\n#define _rxHoistKey(k, x) { double _rxHx = (x); unsigned long long _rxHu; memcpy(&_rxHu, &_rxHx, sizeof(double)); if (_rxHu != _rxHk[k]) { _rxHk[k] = _rxHu; _rxHm |= 1ULL << ((k) < 63 ? (k) : 63); } }\n");
}

// Count the identifiers line i (text s) assigns: `x =`, `x op=`
static inline void chLineWrites(const char *s, int i, int *seen) {
  if (s == NULL) return;
  int k = 0;
  while (s[k] != '\0') {
    int j = pkTimeSkipLiteral(s, k);
    if (j != k) {
      k = j;
      continue;
    }
    if (!pkTimeIdStart(s[k])) {
      k++;
      continue;
    }
    while (pkTimeIdChar(s[j])) j++;
    int m = j;
    while (s[m] == ' ' || s[m] == '\t') m++;
    if (strchr("+-*/%&|^", s[m]) != NULL && s[m] != '\0' && s[m+1] == '=') m++;
    int w = s[m] == '=' && s[m+1] != '=';
    // ++x / x++ / x--
    w = w || (s[j] == '+' && s[j+1] == '+') || (s[j] == '-' && s[j+1] == '-') ||
      (k >= 2 && ((s[k-1] == '+' && s[k-2] == '+') || (s[k-1] == '-' && s[k-2] == '-')));
    if (w && !cslIsMember(s, k)) {
      int id = cslId(&_csl.h, s + k, j - k, 0);
      if (id >= 0 && seen[id] != i) {
        seen[id] = i;
        _chWrites[id]++;
      }
    }
    k = j;
  }
}

static inline void chReset(void) {
  _chOn = 0;
  _ch.on = 0;
}

// Per-model setup after cslBuild(): parameters, first reads, variable names
static inline void chBuildModel(int on) {
  _chOn = 0;
  _ch.on = 0;
  if (!on || !_csl.ok) return;
  int n = _csl.n;
  // simeta()/simeps() rewrite parameter locals in the middle of the body
  for (int i = 0; i < n; ++i) {
    if ((sbPm.line[i] != NULL && (strstr(sbPm.line[i], "simeta") || strstr(sbPm.line[i], "simeps"))) ||
        (sbPmDt.line[i] != NULL && (strstr(sbPmDt.line[i], "simeta") || strstr(sbPmDt.line[i], "simeps")))) return;
  }
  int nId = _csl.h.n;
  _chPar = cslZeros(nId);
  _chFirstRd = (int*)R_alloc(nId > 0 ? nId : 1, sizeof(int));
  _chName = (const char**)R_alloc(nId > 0 ? nId : 1, sizeof(char*));
  for (int v = 0; v < nId; ++v) _chFirstRd[v] = n;
  for (int h = 0; h < _csl.h.cap; ++h) {
    if (_csl.h.key[h] != NULL) _chName[_csl.h.id[h]] = _csl.h.key[h];
  }
  for (int i = n; i--;) {
    for (int k = _csl.rdStart[i]; k < _csl.rdStart[i + 1]; ++k) _chFirstRd[_csl.rd[k]] = i;
  }
  // writes in any line of any type (_csl.dl only has the single-statement kinds)
  _chWrites = cslZeros(nId);
  int *seen = (int*)R_alloc(nId > 0 ? nId : 1, sizeof(int));
  for (int v = 0; v < nId; ++v) seen[v] = -1;
  for (int i = 0; i < n; ++i) {
    chLineWrites(sbPm.line[i], i, seen);
    chLineWrites(sbPmDt.line[i], i, seen);
  }
  cslHashInit(&_chVarNames, 1024);
  sbuf buf;
  sNull(&buf);
  sIni(&buf);
  for (int i = 0; i < NV; i++) {
    sClear(&buf);
    doDot(&buf, tb.ss.line[i]);
    cslId(&_chVarNames, buf.s, (int)strlen(buf.s), 1);
    // set from _PP[] (printPopulateParameters) and never assigned
    if (shouldSkipPrintLhsI(print_populateParameters, 1, i)) continue;
    if (tb.lh[i] == isLHS || (tb.lh[i] == isLHSparam && tb.ini[i] == 1)) continue;
    int id = cslIdStr(&_csl.h, buf.s);
    if (id >= 0 && _chWrites[id] == 0 &&
        !cslIndField(buf.s, (int)strlen(buf.s))) _chPar[id] = 1;
  }
  sFree(&buf);
  _chOn = 1;
}

#endif // __CODEGENHOIST_H__
