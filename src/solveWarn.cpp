#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS

#include "rxomp.h"
#include "solveWarn.h"

#include <cmath>
#include <map>
#include <set>
#include <string>
#include <vector>

#define _(String) (String)

#include <R.h>
#include <Rinternals.h>

#include "rxPrintfFormat.h"
extern "C" void RSprintf(const char *format, ...) R_PRINTF_FORMAT(1, 2);
extern "C" const char *rxGetId(int id);
extern "C" const char *rxGetIdSim(int id, int *sim);
extern "C" int getSilentErr(void);

/* Aggregator state. Keyed by exact message string so any caller can plug
   in their own template and have its repeats coalesced. Mutation is gated
   by an OpenMP critical section named `rxSolveWarn` so the hot Push path
   inside parallel solves can be called concurrently from many threads
   without corrupting the buffers. The critical-region overhead is far
   cheaper than the formatted RSprintf calls it replaces -- the warning
   rate may be high during sticky-recalc but never as high as the per-step
   solver hot path.

   MAX_MESSAGES caps the number of distinct message strings tracked at
   once (overflow drops new messages but doesn't crash); MAX_IDS caps the
   distinct-subject set per message (overflow drops ids but keeps
   counting). Both bound memory; both also bound the per-flush print
   length. The caps are deliberately generous -- overflow under normal
   workloads is a sign something pathological is happening, not a routine
   case to optimise. */
static const std::size_t MAX_MESSAGES = 64;
static const std::size_t MAX_IDS = 256;

// Running mean/variance (Welford's method) with the range of one value a
// message summarizes rather than keys on
struct WarnStat {
  int n;
  double mean, m2, mn, mx;
  WarnStat() : n(0), mean(0.0), m2(0.0), mn(0.0), mx(0.0) {}
  void add(double x) {
    n++;
    if (n == 1) {
      mn = mx = x;
    } else {
      if (x < mn) mn = x;
      if (x > mx) mx = x;
    }
    double d = x - mean;
    mean += d / n;
    m2 += d * (x - mean);
  }
};

struct WarnEntry {
  int count;
  std::set<int> ids;
  std::vector<WarnStat> stats;
  WarnEntry() : count(0) {}
};

static std::map<std::string, WarnEntry> g_warn;

// marker left in a message's key by "%%v" in its format; replaced at flush by
// the summary of the matching value
#define WARN_STAT_MARK "%v"

extern "C" void rxSolveWarnReset(void) {
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    g_warn.clear();
  }
}

static void rxSolveWarnPushV(int id, int nStat, const double *stat,
                             const char *msg0, va_list args) {
  char msg[1024];
  vsnprintf(msg, 1023, msg0, args);
  std::string key(msg);
  std::map<std::string, WarnEntry>::iterator it = g_warn.find(key);
  if (it == g_warn.end()) {
    if (g_warn.size() >= MAX_MESSAGES) {
      /* Silently drop new messages once the dedup table is full. The
         existing entries continue to aggregate their own occurrences. */
      return;
    }
    it = g_warn.insert(std::make_pair(key, WarnEntry())).first;
    it->second.stats.resize(nStat);
  }
  WarnEntry &e = it->second;
  e.count++;
  if (id >= 0 && e.ids.size() < MAX_IDS) {
    e.ids.insert(id);
  }
  int nUse = (int)e.stats.size() < nStat ? (int)e.stats.size() : nStat;
  for (int k = 0; k < nUse; k++) e.stats[k].add(stat[k]);
}

extern "C" void rxSolveWarnPush(int id, const char *msg0, ...) {
  if (msg0 == NULL) return;
  if (getSilentErr() != 0) return;
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    va_list args;
    va_start(args, msg0);
    rxSolveWarnPushV(id, 0, NULL, msg0, args);
    va_end(args);
  }
}

extern "C" void rxSolveWarnPushStat(int id, int nStat, const double *stat,
                                    const char *msg0, ...) {
  if (msg0 == NULL) return;
  if (getSilentErr() != 0) return;
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    va_list args;
    va_start(args, msg0);
    rxSolveWarnPushV(id, nStat, stat, msg0, args);
    va_end(args);
  }
}

// one summarized value: itself when seen once, else mean +/- sd (min/max)
static std::string rxSolveWarnStatStr(const WarnStat &st) {
  char buf[128];
  if (st.n <= 1) {
    snprintf(buf, sizeof(buf), "%g", st.mean);
  } else {
    snprintf(buf, sizeof(buf), "%g +/- %g (%g/%g)", st.mean,
             std::sqrt(st.m2 / (st.n - 1)), st.mn, st.mx);
  }
  return std::string(buf);
}

// the message with each marker replaced by its value's summary, in order
static std::string rxSolveWarnMsg(const std::string &key, const WarnEntry &e) {
  if (e.stats.empty()) return key;
  std::string out;
  std::size_t pos = 0;
  std::size_t k = 0;
  const std::size_t markLen = sizeof(WARN_STAT_MARK) - 1;
  while (k < e.stats.size()) {
    std::size_t f = key.find(WARN_STAT_MARK, pos);
    if (f == std::string::npos) break;
    out.append(key, pos, f - pos);
    out.append(rxSolveWarnStatStr(e.stats[k++]));
    pos = f + markLen;
  }
  out.append(key, pos, std::string::npos);
  return out;
}

extern "C" void rxSolveWarnFlush(int maxIds) {
  if (maxIds < 1) maxIds = 1;
  /* Snapshot under the lock so the print loop runs without holding it.
     R's console writer isn't thread-safe; we expect flush to be called
     from serial code, but the critical region is cheap and bounds the
     race against a stray concurrent Push. */
  std::map<std::string, WarnEntry> snap;
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    snap.swap(g_warn);
  }
  for (std::map<std::string, WarnEntry>::const_iterator it = snap.begin();
       it != snap.end(); ++it) {
    const WarnEntry &e = it->second;
    const std::string msg = rxSolveWarnMsg(it->first, e);
    int nIds = (int)e.ids.size();
    int nShow = (nIds < maxIds) ? nIds : maxIds;
    RSprintf("[%s]: %d warning(s)", msg.c_str(), e.count);
    if (nIds > 0) {
      RSprintf(_(" for subject(s): "));
      int k = 0;
      for (std::set<int>::const_iterator iit = e.ids.begin();
           iit != e.ids.end() && k < nShow; ++iit, ++k) {
        if (k > 0) RSprintf(", ");
        /* rxGetIdSim resolves the internal solve index to the user's subject
           id via the global factor table, and to the simulation that solve
           belongs to when there is more than one.  When the table isn't
           populated (e.g. nlmixr2est estimation that didn't call
           rxSetIdLvlFactors, or an older nlmixr2est) no label exists; fall
           back to the 1-based internal index so the message stays honest and
           non-misleading rather than printing a bare "Unknown".  Ask for the
           label rather than string-comparing rxGetId()'s result: "Unknown" is
           a legal subject id, and a dataset that uses it must still print
           it. */
        int sim = 1;
        const char *idStr = rxGetIdSim(*iit, &sim);
        if (idStr == NULL) {
          RSprintf(_("internal #%d"), *iit + 1);
        } else if (sim > 1) {
          RSprintf(_("%s (sim %d)"), idStr, sim);
        } else {
          RSprintf("%s", idStr);
        }
      }
      if (nIds > nShow) {
        RSprintf(_(", ... (%d more)"), nIds - nShow);
      }
    }
    RSprintf("\n");
  }
}
