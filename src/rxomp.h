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

static inline int rx_get_thread(int mx) {
  int tn = getRxThreadId();
  if (tn < 0) tn = omp_get_thread_num();
  if (tn < 0) return 0;
  if (tn < mx) return tn;
  // Clamp to last valid index rather than collapsing to 0,
  // which would cause two threads to share the same slot
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
