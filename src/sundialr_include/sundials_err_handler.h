#ifndef SUNDIALS_ERR_HANDLER_H
#define SUNDIALS_ERR_HANDLER_H

// CRAN fix: CRAN policy requires that compiled code must not call abort() or
// write to stdout/stderr directly (see "Writing R Extensions" §1.6.4).
// SUNDIALS' default error handler calls abort() on fatal errors, which would
// terminate the R session. This header provides a replacement handler that
// routes fatal SUNDIALS errors through R's error mechanism (Rf_error) instead.
//
// Usage: after SUNContext_Create(), call:
//   SUNContext_PushErrHandler(sunctx, sundials_r_err_handler, NULL);

#include <Rinternals.h>
#include <sundials/sundials_types.h>
#include <sundials/sundials_context.h>

static void sundials_r_err_handler(int line, const char* func, const char* file,
                                    const char* msg, SUNErrCode err_code,
                                    void* err_user_data, SUNContext sunctx) {
  Rf_error("SUNDIALS error in %s (%s:%d): %s",
           func ? func : "unknown",
           file ? file : "unknown",
           line,
           msg  ? msg  : "no message");
}

#endif /* SUNDIALS_ERR_HANDLER_H */
