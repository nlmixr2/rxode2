#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#ifndef STRICT_R_HEADERS
#define STRICT_R_HEADERS
#endif

#include "rxomp.h"
#include "solveDiag.h"
#include "solveWarn.h"
#include "../inst/include/rxMemAlloc.h"

#include <atomic>
#include <cmath>
#include <cstdio>
#include <map>
#include <string>
#include <vector>

#include <R.h>
#include <Rinternals.h>

// states named in one diagnostics line before "... (N more)"
static const int DIAG_MAX_NAMES = 5;
// orders of magnitude the finite non-zero states must span for the atol hint
static const double DIAG_SPAN_ORDERS = 8.0;

struct DiagRec {
  bool hasDydt, hasCapture, named;
  double tDydt, tAcc;
  std::vector<int> dydtBad;
  std::vector<double> dydtY;
  std::vector<double> yAcc;
  std::vector<int> failBad;
  DiagRec() : hasDydt(false), hasCapture(false), named(true),
              tDydt(NA_REAL), tAcc(NA_REAL) {}
};

static std::vector<std::string> g_stateNames;
static std::map<int, DiagRec> g_diag;
// records held, so the success path skips the lock
static std::atomic<int> g_diagN(0);

extern "C" void rxSolveDiagSetup(SEXP stateNames) {
  std::vector<std::string> names;
  if (TYPEOF(stateNames) == STRSXP) {
    int n = Rf_length(stateNames);
    names.reserve(n);
    for (int i = 0; i < n; i++) {
      names.push_back(std::string(CHAR(STRING_ELT(stateNames, i))));
    }
  }
#ifdef _OPENMP
#pragma omp critical(rxSolveDiag)
#endif
  {
    g_stateNames.swap(names);
    g_diag.clear();
    g_diagN.store(0);
  }
}

extern "C" void rxSolveDiagClear(int id) {
  if (g_diagN.load() == 0) return;
#ifdef _OPENMP
#pragma omp critical(rxSolveDiag)
#endif
  {
    if (g_diag.erase(id) > 0) g_diagN.store((int)g_diag.size());
  }
}

extern "C" void rxSolveDiagDydt(int id, int n, double t, const double *y,
                                const double *f) {
  if (id < 0 || n <= 0 || y == NULL || f == NULL) return;
  std::vector<int> bad;
  std::vector<double> vals;
  for (int i = 0; i < n; i++) {
    if (!std::isfinite(f[i])) {
      bad.push_back(i);
      vals.push_back(y[i]);
    }
  }
  if (bad.empty()) return;
#ifdef _OPENMP
#pragma omp critical(rxSolveDiag)
#endif
  {
    DiagRec &r = g_diag[id];
    if (!r.hasDydt) {
      r.hasDydt = true;
      r.tDydt = t;
      r.dydtBad.swap(bad);
      r.dydtY.swap(vals);
    }
    g_diagN.store((int)g_diag.size());
  }
}

extern "C" void rxSolveDiagCapture(int id, int n, int named, double tAcc,
                                   const double *yAcc, const double *yFail) {
  if (n <= 0) return;
  std::vector<double> acc;
  if (yAcc != NULL) rxMemAssign(fixed, acc, yAcc, yAcc + n);
  std::vector<int> failBad;
  if (yFail != NULL) {
    for (int i = 0; i < n; i++) {
      if (!std::isfinite(yFail[i])) failBad.push_back(i);
    }
  }
#ifdef _OPENMP
#pragma omp critical(rxSolveDiag)
#endif
  {
    DiagRec &r = g_diag[id];
    if (!r.hasCapture) {
      r.hasCapture = true;
      r.named = named != 0;
      r.tAcc = tAcc;
      r.yAcc.swap(acc);
      r.failBad.swap(failBad);
    }
    g_diagN.store((int)g_diag.size());
  }
}

static std::string diagName(const DiagRec &r, int i) {
  if (r.named && i >= 0 && i < (int)g_stateNames.size()) {
    return g_stateNames[i];
  }
  char buf[32];
  snprintf(buf, sizeof(buf), "state[%d]", i + 1);
  return std::string(buf);
}

// "a, b, c" naming at most DIAG_MAX_NAMES of `idx`; with `withValue`, each as
// "a = %v" so the value is summarized by the aggregator
static std::string diagNames(const DiagRec &r, const std::vector<int> &idx,
                             bool withValue) {
  std::string out;
  int nShow = (int)idx.size() < DIAG_MAX_NAMES ? (int)idx.size() : DIAG_MAX_NAMES;
  for (int k = 0; k < nShow; k++) {
    if (k > 0) out += ", ";
    out += diagName(r, idx[k]);
    if (withValue) out += " = %v";
  }
  if ((int)idx.size() > nShow) {
    char buf[48];
    snprintf(buf, sizeof(buf), ", ... (%d more)", (int)idx.size() - nShow);
    out += buf;
  }
  return out;
}

// the non-finite derivative evaluation, with the values of those states
static void diagPushDydt(int id, const DiagRec &r) {
  std::string msg = "solve error: last derivative evaluation (t = %v) was non-finite for: " +
    diagNames(r, r.dydtBad, false) + " (state values: " +
    diagNames(r, r.dydtBad, true) + ")";
  int nShow = (int)r.dydtBad.size() < DIAG_MAX_NAMES ? (int)r.dydtBad.size() : DIAG_MAX_NAMES;
  std::vector<double> stat(1 + nShow);
  stat[0] = r.tDydt;
  for (int k = 0; k < nShow; k++) stat[1 + k] = r.dydtY[k];
  rxSolveWarnPushStat(id, (int)stat.size(), stat.data(), "%s", msg.c_str());
}

// the atol hint when the finite non-zero accepted states span many orders
static void diagPushSpan(int id, const DiagRec &r) {
  int iMax = -1, iMin = -1;
  for (int i = 0; i < (int)r.yAcc.size(); i++) {
    double a = std::fabs(r.yAcc[i]);
    if (!std::isfinite(a) || a == 0.0) continue;
    if (iMax < 0 || a > std::fabs(r.yAcc[iMax])) iMax = i;
    if (iMin < 0 || a < std::fabs(r.yAcc[iMin])) iMin = i;
  }
  if (iMax < 0 || iMax == iMin ||
      std::log10(std::fabs(r.yAcc[iMax]) / std::fabs(r.yAcc[iMin])) < DIAG_SPAN_ORDERS) {
    return;
  }
  char buf[32];
  snprintf(buf, sizeof(buf), "%g", DIAG_SPAN_ORDERS);
  std::string msg = std::string("solve error: states span >= ") + buf +
    " orders of magnitude (" + diagName(r, iMax) + " = %v, " +
    diagName(r, iMin) + " = %v); consider rescaling the states or setting atol per state";
  double stat[2] = {r.yAcc[iMax], r.yAcc[iMin]};
  rxSolveWarnPushStat(id, 2, stat, "%s", msg.c_str());
}

static void diagPushRec(int id, const DiagRec &r) {
  if (r.hasDydt) diagPushDydt(id, r);
  if (!r.hasCapture) return;
  std::vector<int> accBad;
  for (int i = 0; i < (int)r.yAcc.size(); i++) {
    if (!std::isfinite(r.yAcc[i])) accBad.push_back(i);
  }
  if (!accBad.empty()) {
    std::string msg = "solve error: non-finite values in the last accepted state (t = %v): " +
      diagNames(r, accBad, false);
    rxSolveWarnPushStat(id, 1, &r.tAcc, "%s", msg.c_str());
  }
  if (!r.failBad.empty()) {
    std::string msg = "solve error: non-finite state values after the failed step: " +
      diagNames(r, r.failBad, false);
    rxSolveWarnPush(id, "%s", msg.c_str());
  }
  diagPushSpan(id, r);
}

extern "C" void rxSolveDiagPush(int id) {
  if (g_diagN.load() == 0) return;
  DiagRec r;
  bool found = false;
#ifdef _OPENMP
#pragma omp critical(rxSolveDiag)
#endif
  {
    std::map<int, DiagRec>::iterator it = g_diag.find(id);
    if (it != g_diag.end()) {
      r = it->second;
      found = true;
      g_diag.erase(it);
      g_diagN.store((int)g_diag.size());
    }
  }
  if (found) diagPushRec(id, r);
}
