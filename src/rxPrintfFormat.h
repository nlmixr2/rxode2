#ifndef __RXODE2_PRINTF_FORMAT_H__
#define __RXODE2_PRINTF_FORMAT_H__
// R_PRINTF_FORMAT(m, n) is the printf format attribute R's headers give
// Rprintf() and Rf_error() (gnu_printf with MinGW), so -Wformat checks the
// calls of rxode2's own varargs wrappers too; empty where R lacks it
#include <R_ext/Print.h>
#ifndef R_PRINTF_FORMAT
#define R_PRINTF_FORMAT(M, N)
#endif
#endif
