#ifndef __parseParamMerge_H__
#define __parseParamMerge_H__
#pragma once

// nlmixr2/rxode2#1279: a model may carry more than one `param()` statement -- a
// generated model often appends one when it splices extra parameters into an
// already-built model text.  The parser merges every declaration into the one
// parameter vector reported by `rxModelVars()$params`, but the normalized model
// text kept each statement, so a consumer reading or editing "the" `param()`
// statement of that text saw only the first one and silently missed the later
// declarations.  Rewrite the normalized text to a single merged `param()`
// statement so the text agrees with `$params`.

#define rxIsNormParamLine(l) (!strncmp((l), "param(", 6))

// Mark the parameters named by one normalized `param(a,b);` line, tracking the
// highest position any of them takes in the final parameter vector.
static inline void markNormParamLine(const char *line, SEXP params, int np,
                                     int *hi) {
  const char *p = line + 6; // past "param("
  while (*p != '\0' && *p != ')') {
    const char *start = p;
    while (*p != '\0' && *p != ')' && *p != ',') p++;
    size_t n = (size_t)(p - start);
    if (n > 0) {
      for (int j = 0; j < np; j++) {
        const char *cur = CHAR(STRING_ELT(params, j));
        if (strlen(cur) == n && !strncmp(start, cur, n)) {
          if (j > *hi) *hi = j;
          break;
        }
      }
    }
    if (*p == ',') p++;
  }
}

static inline void mergeNormParamStatements(SEXP params) {
  int nParam = 0;
  for (int i = 0; i < sbNrmL.n; i++) {
    if (rxIsNormParamLine(sbNrmL.line[i])) nParam++;
  }
  if (nParam < 2) return; // nothing to merge
  int np = Rf_length(params);
  int hi = -1;
  for (int i = 0; i < sbNrmL.n; i++) {
    if (rxIsNormParamLine(sbNrmL.line[i])) {
      markNormParamLine(sbNrmL.line[i], params, np, &hi);
    }
  }
  // The merged statement takes the place of the first `param()` statement and
  // spans the whole parameter vector up to the last declared parameter, not
  // just the declared names.  Everything registered before that first statement
  // is a prefix of the parameter vector, and any declared name that survived as
  // a parameter is registered at or after it, so declaring `params[0..hi]`
  // there re-registers exactly the names the earlier lines already introduced,
  // in the same order, and re-parsing the normalized text gives back the same
  // parameter vector.  Spanning only the declared names would not: with
  // "param(b);y=c;param(d);d/dt(b)=-b;" `b` becomes a state and the merged
  // statement would move `d` ahead of `c`.  When no declared name survived as a
  // parameter the statements declare nothing and are dropped.
  //
  // sbt and sbNrmL2 are scratch globals freed by parseFree(); everything the
  // model needed from sbt has already been emitted by the time model variables
  // are generated, and using globals here means an R error raised by addLine()
  // or sAppend() cannot leak a buffer.
  sClear(&sbt);
  if (hi >= 0) {
    sAppendN(&sbt, "param(", 6);
    for (int j = 0; j <= hi; j++) {
      if (j != 0) sAppendN(&sbt, ",", 1);
      sAppend(&sbt, "%s", CHAR(STRING_ELT(params, j)));
    }
    sAppendN(&sbt, ");\n", 3);
  }
  lineIni(&sbNrmL2);
  int seen = 0;
  for (int i = 0; i < sbNrmL.n; i++) {
    const char *cur = sbNrmL.line[i];
    if (rxIsNormParamLine(cur)) {
      if (seen++ || sbt.o == 0) continue;
      cur = sbt.s;
    }
    curLineProp(&sbNrmL2, sbNrmL.lProp[i]);
    curLineType(&sbNrmL2, sbNrmL.lType[i]);
    addLine(&sbNrmL2, "%s", cur);
  }
  vLines swap = sbNrmL;
  sbNrmL = sbNrmL2;
  sbNrmL2 = swap;
  // sbNrm is the concatenation of the normalized lines; rebuild it so the two
  // cannot disagree.
  sClear(&sbNrm);
  for (int i = 0; i < sbNrmL.n; i++) {
    sAppend(&sbNrm, "%s", sbNrmL.line[i]);
  }
}

#undef rxIsNormParamLine

#endif // __parseParamMerge_H__
