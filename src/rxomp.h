#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#ifdef _OPENMP
#include <pthread.h>
#include <omp.h>

static inline int rx_get_thread(int mx) {
  int tn = omp_get_thread_num();
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
