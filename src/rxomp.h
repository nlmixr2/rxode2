#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifdef _OPENMP
#include <pthread.h>
#include <omp.h>

#ifdef __cplusplus
extern "C" {
#endif
// Cross-DLL OpenMP thread-id override (defined in rxData.cpp).  Returns the
// id supplied by an external OpenMP driver (e.g. nlmixr2est FOCEi) or -1 when
// unset.  See the long comment in rxData.cpp.  Needed because rxode2's static
// libgomp reports omp_get_thread_num()==0 for threads created by another DLL's
// libgomp, which would collapse all per-thread buffers onto slot 0.
int getRxThreadId(void);
#ifdef __cplusplus
}
#endif

// `mx` is `op->cores`, and `op->cores` is the CONTRACT: it is the number of
// threads that may drive rxode2's solve at once, every per-thread pool is sized
// to it (`rxOptionsIniEnsure`), and a caller -- rxode2's own `par_*()` loops or
// an external driver such as nlmixr2est's FOCEi -- must not run a team wider
// than the count it set.  So `tn >= mx` cannot happen in a correct caller.
//
// The clamp below is a backstop for a caller that broke the contract anyway,
// and it is NOT a supported mode: two threads on one slot share the whole of
// `_setIndPointersByThread`'s per-thread state (infusion rates, the pending and
// ignored dose arrays, `gon`, `solveLast`), which is a data race on the solve
// itself.  It is still the better of the two failures -- indexing past the pool
// hands out a garbage pointer instead -- so a broken caller degrades rather
// than corrupting memory outright.  Do not read the clamp as permission to run
// a wider team; fix `op->cores` at the caller.
static inline int rx_get_thread(int mx) {
  int tn = getRxThreadId();
  if (tn < 0) tn = omp_get_thread_num();
  if (tn < 0) return 0;
  if (tn < mx) return tn;
  return (mx > 0) ? mx - 1 : 0;
}

#else

static inline int omp_get_num_procs(void){
  return 1;
}

static inline int omp_get_thread_limit(void){
  return 1;
}

static inline int omp_get_max_threads(void){
  return 1;
}

static inline int omp_get_thread_num(void) {
  return 0;
}

static inline int omp_in_parallel(void) {
  return 0;
}

static inline int rx_get_thread(int mx) {
  return 0;
}

#endif
