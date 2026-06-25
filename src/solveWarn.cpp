#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS

#include "rxomp.h"
#include "solveWarn.h"

#include <map>
#include <set>
#include <string>
#include <cstring>

#define _(String) (String)

#include <R.h>
#include <Rinternals.h>

extern "C" void RSprintf(const char *format, ...);
extern "C" const char *rxGetId(int id);
extern "C" int getSilentErr(void);

/* Aggregator state. Keyed by exact message string so any caller can plug
   in their own template and have its repeats coalesced. Mutation is gated
   by an OpenMP critical section named `rxSolveWarn` so the hot Push path
   inside parallel solves can be called concurrently from many threads
   without corrupting the buffers. The critical-region overhead is far
   cheaper than the formatted RSprintf calls it replaces — the warning
   rate may be high during sticky-recalc but never as high as the per-step
   solver hot path.

   MAX_MESSAGES caps the number of distinct message strings tracked at
   once (overflow drops new messages but doesn't crash); MAX_IDS caps the
   distinct-subject set per message (overflow drops ids but keeps
   counting). Both bound memory; both also bound the per-flush print
   length. The caps are deliberately generous — overflow under normal
   workloads is a sign something pathological is happening, not a routine
   case to optimise. */
static const std::size_t MAX_MESSAGES = 64;
static const std::size_t MAX_IDS = 256;

struct WarnEntry {
  int count;
  std::set<int> ids;
  WarnEntry() : count(0) {}
};

static std::map<std::string, WarnEntry> g_warn;

extern "C" void rxSolveWarnReset(void) {
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    g_warn.clear();
  }
}

extern "C" void rxSolveWarnPush(int id, const char *msg) {
  if (msg == NULL) return;
  if (getSilentErr() != 0) return;
#ifdef _OPENMP
#pragma omp critical(rxSolveWarn)
#endif
  {
    std::string key(msg);
    std::map<std::string, WarnEntry>::iterator it = g_warn.find(key);
    if (it == g_warn.end()) {
      if (g_warn.size() < MAX_MESSAGES) {
        WarnEntry e;
        e.count = 1;
        if (id >= 0) e.ids.insert(id);
        g_warn[key] = e;
      }
      /* Silently drop new messages once the dedup table is full. The
         existing entries continue to aggregate their own occurrences. */
    } else {
      it->second.count++;
      if (id >= 0 && it->second.ids.size() < MAX_IDS) {
        it->second.ids.insert(id);
      }
    }
  }
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
    const std::string &msg = it->first;
    const WarnEntry &e = it->second;
    int nIds = (int)e.ids.size();
    int nShow = (nIds < maxIds) ? nIds : maxIds;
    RSprintf("[%s]: %d warning(s)", msg.c_str(), e.count);
    if (nIds > 0) {
      RSprintf(_(" for subject(s): "));
      int k = 0;
      for (std::set<int>::const_iterator iit = e.ids.begin();
           iit != e.ids.end() && k < nShow; ++iit, ++k) {
        if (k > 0) RSprintf(", ");
        /* rxGetId resolves the internal solve index to the user's subject id
           via the global factor table.  When that table isn't populated
           (e.g. nlmixr2est estimation that didn't call rxSetIdLvlFactors, or
           an older nlmixr2est), it returns the literal "Unknown"; fall back
           to the 1-based internal index so the message stays honest and
           non-misleading rather than printing a bare "Unknown". */
        const char *idStr = rxGetId(*iit);
        if (strcmp(idStr, "Unknown") == 0) {
          RSprintf(_("internal #%d"), *iit + 1);
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
