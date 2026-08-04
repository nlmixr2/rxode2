#ifndef RXODE2_SUNDIALS_STAN_COMPAT_H
#define RXODE2_SUNDIALS_STAN_COMPAT_H

#include <sundials/sundials_config.h>
#include <sundials/sundials_types.h>
#include <sundials/sundials_context.h>

/*
 * StanHeaders still expects legacy SUNDIALS symbols/types (sundials::Context,
 * realtype, and RCONST). Newer SUNDIALS headers from sundialr removed them.
 * Provide a narrow compatibility shim for Stan AD compilation.
 */
#if defined(SUNDIALS_VERSION_MAJOR) && (SUNDIALS_VERSION_MAJOR >= 7)
namespace sundials {
class Context {
public:
#ifdef SUN_COMM_NULL
  explicit Context(SUNComm comm = SUN_COMM_NULL) {
#else
  explicit Context(void *comm = NULL) {
#endif
    SUNContext_Create(comm, &sunctx_);
  }

  operator SUNContext() { return sunctx_; }

  ~Context() { SUNContext_Free(&sunctx_); }

private:
  SUNContext sunctx_;
};
} // namespace sundials

#ifndef realtype
typedef sunrealtype realtype;
#endif

#ifndef RCONST
#define RCONST(x) SUN_RCONST(x)
#endif
#endif

/*
 * RcppParallel 6.2.0 statically links TBB on Windows and no longer exports
 * the oneTBB runtime symbols (tbb::detail::r1::observe) that stan-math's
 * ad_tape_observer (stan/math/rev/core/init_chainablestack.hpp) needs, so
 * configure strips -DSTAN_THREADS and no TBB library is linked (see
 * inst/tools/workaround.R).  Pre-define that header's include guard so the
 * TBB task_scheduler_observer never enters the build.  The main-thread AD
 * tape that observer would have created is created in linCmt.cpp instead,
 * keyed on RXODE2_NO_STAN_TBB_OBSERVER.
 */
#if defined(_WIN32) && !defined(STAN_THREADS) && defined(__cplusplus)
#define STAN_MATH_REV_CORE_INIT_CHAINABLESTACK_HPP
#define RXODE2_NO_STAN_TBB_OBSERVER
#endif

#endif
