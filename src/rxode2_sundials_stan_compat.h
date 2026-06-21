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

#endif
