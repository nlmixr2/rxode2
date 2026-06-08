#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS

#include "rxomp.h"
#include "solveWarn.h"

#include <set>
#include <vector>

#define _(String) (String)

#include <R.h>
#include <Rinternals.h>

extern "C" void RSprintf(const char *format, ...);
extern "C" const char *rxGetId(int id);
extern "C" int getSilentErr(void);

/* Aggregator state — kept tiny and global. Mutation is gated by an OpenMP
   critical section named `rxSolveWarn` so the hot Push path in intdy.c can
   be called from multiple parallel solves without corrupting the buffers.
   The Push call rate is bounded by how often the integrator misses its
   interpolation window — not per-step — so contention on this critical
   section is far cheaper than the equivalent flood of RSprintf calls it
   replaces.

   Capping at MAX_IDS distinct IDs per family bounds memory; further IDs
   still increment `count` but are not added to the set. The flush message
   reports overflow as "... (N more)". */
static const std::size_t MAX_IDS = 256;

struct WarnState {
  int count;
  std::set<int> ids;
  WarnState() : count(0) {}
};

static WarnState g_warn[RX_WARN_N];

extern "C" void rxSolveWarnReset(void) {
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    for (int f = 0; f < RX_WARN_N; f++) {
      g_warn[f].count = 0;
      g_warn[f].ids.clear();
    }
  }
}

extern "C" void rxSolveWarnPush(int family, int id) {
  if (family < 0 || family >= RX_WARN_N) return;
  if (getSilentErr() != 0) return;
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    g_warn[family].count++;
    if (g_warn[family].ids.size() < MAX_IDS) {
      g_warn[family].ids.insert(id);
    }
  }
}

static const char *familyLabel(int family) {
  switch (family) {
  case RX_WARN_INTDY:
    return "integrator window-miss (intdy)";
  default:
    return "unknown";
  }
}

extern "C" void rxSolveWarnFlush(int maxIds) {
  if (maxIds < 1) maxIds = 1;
  /* Snapshot under the lock, print outside it. R's console writer is not
     thread-safe but flush should normally be called from serial code; we
     hold the lock briefly anyway to be safe against a stray concurrent
     Push. */
  int snapCount[RX_WARN_N];
  std::vector<int> snapIds[RX_WARN_N];
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    for (int f = 0; f < RX_WARN_N; f++) {
      snapCount[f] = g_warn[f].count;
      if (snapCount[f] > 0) {
        snapIds[f].assign(g_warn[f].ids.begin(), g_warn[f].ids.end());
      }
      g_warn[f].count = 0;
      g_warn[f].ids.clear();
    }
  }
  for (int f = 0; f < RX_WARN_N; f++) {
    if (snapCount[f] <= 0) continue;
    int nIds = (int)snapIds[f].size();
    int nShow = (nIds < maxIds) ? nIds : maxIds;
    RSprintf(_("%s: %d warning(s)"), familyLabel(f), snapCount[f]);
    if (nIds > 0) {
      RSprintf(_(" for subject(s): "));
      for (int k = 0; k < nShow; k++) {
        if (k > 0) RSprintf(", ");
        RSprintf("%s", rxGetId(snapIds[f][k]));
      }
      if (nIds > nShow) {
        RSprintf(_(", ... (%d more)"), nIds - nShow);
      }
    }
    RSprintf("\n");
  }
}
