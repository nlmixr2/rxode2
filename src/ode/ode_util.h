#ifndef ODE_UTIL_H_
#define ODE_UTIL_H_

//! \file ode_util.h

#include <cmath>

namespace ode {

//!Simple maximum of two doubles
double ode_max2 (double a, double b);

//!Simple minimum of two doubles
double ode_min2 (double a, double b);

//!Checks if two numbers are very close to each other
bool ode_is_close (double a, double b, double thresh);

} // namespace ode

#endif
