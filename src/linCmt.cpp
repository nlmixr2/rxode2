#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include "linCmt.h"

bool _linCmtGlobalB = false;

Eigen::Matrix<stan::math::var, Eigen::Dynamic, 1> _linCmtGlobalMicroV;

Eigen::Matrix<double, Eigen::Dynamic, 1> _linCmtGlobalMicroD;


double linCmtA(int)
