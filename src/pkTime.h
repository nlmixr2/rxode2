#ifndef __PKTIME_H__
#define __PKTIME_H__
// PK-type time with rxControl(nonmem = TRUE) (rxode2#1429).
//
// A statement that does not depend on a state reads `time` as the time of the
// record that ends the integration interval (NONMEM's $PK TIME); a statement
// that depends on a state, and d/dt() itself, keep the integrator's time
// ($DES T).  The classification runs on the generated C lines (sbPm): a line
// is state dependent when it reads a state or a state-dependent variable, or
// sits in an if/else/while chain that contains a state-dependent line.
// Iterated to a fixed point, so it is conservative in the direction of
// keeping the continuous time.

static inline int pkTimeIdStart(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static inline int pkTimeIdChar(char c) {
  return pkTimeIdStart(c) || (c >= '0' && c <= '9');
}

// Advance past a string/char literal or a number starting at s[k]; returns the
// new index, or k when s[k] starts none of them.
static inline int pkTimeSkipLiteral(const char *s, int k) {
  char c = s[k];
  if (c == '"' || c == '\'') {
    int j = k + 1;
    while (s[j] != '\0' && s[j] != c) {
      if (s[j] == '\\' && s[j+1] != '\0') j++;
      j++;
    }
    return (s[j] == '\0') ? j : j + 1;
  }
  if ((c >= '0' && c <= '9') || (c == '.' && s[k+1] >= '0' && s[k+1] <= '9')) {
    int j = k;
    while (s[j] != '\0') {
      if (pkTimeIdChar(s[j]) || s[j] == '.') {
        j++;
      } else if ((s[j] == '+' || s[j] == '-') && (s[j-1] == 'e' || s[j-1] == 'E')) {
        j++;
      } else {
        break;
      }
    }
    return j;
  }
  return k;
}

// Is there an `else` token before s[k] (the `{` it opens)?
static inline int pkTimeHasElse(const char *s, int k) {
  for (int j = 0; j + 4 <= k; ++j) {
    if (!strncmp(s + j, "else", 4) && (j == 0 || !pkTimeIdChar(s[j-1])) &&
        !pkTimeIdChar(s[j+4])) return 1;
  }
  return 0;
}

typedef struct pkTimeVars {
  char **v;
  int n;
  int nAlloc;
} pkTimeVars;

static inline int pkTimeHasVar(pkTimeVars *d, const char *s, int len) {
  for (int i = d->n; i--;) {
    if ((int)strlen(d->v[i]) == len && !strncmp(d->v[i], s, len)) return 1;
  }
  return 0;
}

static inline void pkTimeAddVar(pkTimeVars *d, const char *s, int len) {
  if (pkTimeHasVar(d, s, len)) return;
  if (d->n + 1 > d->nAlloc) {
    int nAlloc = d->nAlloc + 64;
    char **v = (char**)R_alloc(nAlloc, sizeof(char*));
    for (int i = 0; i < d->n; ++i) v[i] = d->v[i];
    d->v = v;
    d->nAlloc = nAlloc;
  }
  char *c = R_alloc(len + 1, sizeof(char));
  memcpy(c, s, len);
  c[len] = '\0';
  d->v[d->n++] = c;
}

// Does the line read a state-dependent symbol?  Also reports (via *hasT)
// whether it reads the identifier `t`.
static inline int pkTimeLineReadsDep(const char *s, pkTimeVars *d, int *hasT) {
  int ret = 0;
  int k = 0;
  *hasT = 0;
  while (s[k] != '\0') {
    int j = pkTimeSkipLiteral(s, k);
    if (j != k) {
      k = j;
    } else if (pkTimeIdStart(s[k])) {
      j = k;
      while (pkTimeIdChar(s[j])) j++;
      if (j - k == 1 && s[k] == 't') {
        *hasT = 1;
      } else if (pkTimeHasVar(d, s + k, j - k)) {
        ret = 1;
      }
      k = j;
    } else {
      k++;
    }
  }
  return ret;
}

// The variable a `x = ...` line assigns, or 0 when the line is not one.
static inline int pkTimeLhs(const char *s, int *start) {
  int k = 0;
  while (s[k] == ' ' || s[k] == '\t') k++;
  if (!pkTimeIdStart(s[k])) return 0;
  int j = k;
  while (pkTimeIdChar(s[j])) j++;
  int len = j - k;
  while (s[j] == ' ' || s[j] == '\t') j++;
  if (s[j] != '=' || s[j+1] == '=') return 0;
  *start = k;
  return len;
}

// Fill isPk[i] (sbPm.n entries) with 1 when line i is PK-type and reads `t`;
// returns the number of such lines.
static inline int pkTimeClassify(int *isPk) {
  int n = sbPm.n;
  if (n <= 0) return 0;
  // nothing to classify unless a candidate line reads `t`
  pkTimeVars d0;
  d0.v = NULL;
  d0.n = 0;
  d0.nAlloc = 0;
  int anyT = 0;
  for (int i = 0; i < n && !anyT; ++i) {
    int t = sbPm.lType[i];
    if (t != TASSIGN && t != TLOGIC && t != TINI) continue;
    pkTimeLineReadsDep(sbPm.line[i], &d0, &anyT);
  }
  if (!anyT) {
    for (int i = 0; i < n; ++i) isPk[i] = 0;
    return 0;
  }
  int *dep = (int*)R_alloc(n, sizeof(int));
  int *hasT = (int*)R_alloc(n, sizeof(int));
  // if/else/while chains: span[g] = [first opener line, last closing line]
  int *grpFirst = (int*)R_alloc(n, sizeof(int));
  int *grpLast = (int*)R_alloc(n, sizeof(int));
  int *stack = (int*)R_alloc(n + 1, sizeof(int)); // group of each open brace
  int nGrp = 0, nStack = 0;
  for (int i = 0; i < n; ++i) {
    dep[i] = 0;
    isPk[i] = 0;
    hasT[i] = 0;
    if (sbPm.lType[i] != TLOGIC) continue;
    const char *s = sbPm.line[i];
    int lastClosed = -1;
    int k = 0;
    while (s[k] != '\0') {
      int j = pkTimeSkipLiteral(s, k);
      if (j != k) {
        k = j;
        continue;
      }
      if (s[k] == '}') {
        if (nStack > 0) {
          lastClosed = stack[--nStack];
          grpLast[lastClosed] = i;
        }
      } else if (s[k] == '{') {
        int g;
        if (lastClosed >= 0 && pkTimeHasElse(s, k)) {
          g = lastClosed; // else branch: same chain as the if it follows
        } else {
          g = nGrp++;
          grpFirst[g] = i;
        }
        grpLast[g] = i;
        stack[nStack++] = g;
      }
      k++;
    }
  }
  while (nStack > 0) grpLast[stack[--nStack]] = n - 1;
  pkTimeVars d;
  d.v = NULL;
  d.n = 0;
  d.nAlloc = 0;
  sbuf buf;
  buf.s = NULL;
  buf.sN = 0;
  buf.o = 0;
  sIni(&buf);
  for (int i = 0; i < tb.de.n; ++i) {
    sClear(&buf);
    doDot(&buf, tb.ss.line[tb.di[i]]);
    pkTimeAddVar(&d, buf.s, (int)strlen(buf.s));
  }
  sFree(&buf);
  int changed = 1;
  while (changed) {
    changed = 0;
    for (int i = 0; i < n; ++i) {
      int t = sbPm.lType[i];
      int cur = pkTimeLineReadsDep(sbPm.line[i], &d, &hasT[i]);
      if (t == TDDT || t == TLIN || t == TJAC || t == TMTIME ||
          t == TMAT0 || t == TMATF || t == TEVID) cur = 1;
      // an indLin(state) <- forcing is part of the ODE right-hand side
      if (t == TASSIGN && !cur) {
        int start = 0;
        int len = pkTimeLhs(sbPm.line[i], &start);
        if (len > 10 && !strncmp(sbPm.line[i] + start, "rx_indLin_", 10)) cur = 1;
      }
      if (cur && !dep[i]) {
        dep[i] = 1;
        changed = 1;
      }
      if (dep[i] && (t == TASSIGN || t == TLIN || t == TINI)) {
        int start = 0;
        int len = pkTimeLhs(sbPm.line[i], &start);
        if (len > 0 && !pkTimeHasVar(&d, sbPm.line[i] + start, len)) {
          pkTimeAddVar(&d, sbPm.line[i] + start, len);
          changed = 1;
        }
      }
    }
    for (int g = 0; g < nGrp; ++g) {
      int any = 0;
      for (int i = grpFirst[g]; i <= grpLast[g]; ++i) {
        if (dep[i]) {
          any = 1;
          break;
        }
      }
      if (!any) continue;
      for (int i = grpFirst[g]; i <= grpLast[g]; ++i) {
        if (!dep[i]) {
          dep[i] = 1;
          changed = 1;
        }
      }
    }
  }
  int ret = 0;
  for (int i = 0; i < n; ++i) {
    int t = sbPm.lType[i];
    if (!dep[i] && hasT[i] && (t == TASSIGN || t == TLOGIC || t == TINI)) {
      isPk[i] = 1;
      ret++;
    }
  }
  return ret;
}

// Append `line`, reading the identifier `t` as `_tPK`.
static inline void pkTimeAppendLine(sbuf *out, const char *line) {
  int k = 0;
  while (line[k] != '\0') {
    int j = pkTimeSkipLiteral(line, k);
    if (j != k) {
      sAppendN(out, line + k, j - k);
      k = j;
    } else if (pkTimeIdStart(line[k])) {
      j = k;
      while (pkTimeIdChar(line[j])) j++;
      if (j - k == 1 && line[k] == 't') {
        sAppendN(out, "_tPK", 4);
      } else {
        sAppendN(out, line + k, j - k);
      }
      k = j;
    } else {
      sPut(out, line[k]);
      k++;
    }
  }
}

#endif // __PKTIME_H__
