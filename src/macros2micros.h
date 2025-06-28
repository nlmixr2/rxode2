#ifndef __MICROS2MACROS_H__
#define __MICROS2MACROS_H__
#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stan/math.hpp>
#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define R_NO_REMAP
#include <Rinternals.h>
#include <RcppEigen.h>

#include "../inst/include/rxode2parse.h"

namespace stan {
  namespace math {

    using std::exp;
    using stan::math::exp;
    using std::sqrt;
    using stan::math::sqrt;
    using std::pow;
    using stan::math::pow;
    using std::acos;
    using stan::math::acos;
    using std::cos;
    using stan::math::cos;

    template<class T>
    Eigen::Matrix<T, Eigen::Dynamic, 2>
    macros2micros(const Eigen::Matrix<T, Eigen::Dynamic, 1>& p,
                  const int& ncmt,
                  const int& trans){
      Eigen::Matrix<T, Eigen::Dynamic, 2> g(ncmt, 2);
      T btemp, ctemp, dtemp;
#define p1    p[0]
#define v1    p[1]
#define p2    p[2]
#define p3    p[3]
#define p4    p[4]
#define p5    p[5]
#define v     g(0, 0)
#define k     g(0, 1)

#define k12   g(1, 0)
#define k23   g(1, 0)

#define k21   g(1, 1)
#define k32   g(1, 1)

#define k13   g(2, 0)
#define k24   g(2, 0)

#define k31   g(2, 1)
#define k42   g(2, 1)
      switch (ncmt) {
      case 3: { // 3 compartment model
        switch (trans) {
        case 1: // cl v q vp
          k = p1/v1; // k = CL/V
          v = v1;
          k12 = p2/v1; // k12 = Q/V
          k21 = p2/p3; // k21 = Q/Vp
          k13 = p4/v1; // k31 = Q2/V
          k31 = p4/p5; // k31 = Q2/Vp2
          break;
        case 2: // k=(*p1) v=(*v1) k12=(*p2) k21=(*p3) k13=(*p4) k31=(*p5)
          k = p1;
          v = v1;
          k12 = p2;
          k21 = p3;
          k13 = p4;
          k31 = p5;
          break;
        case 11:
#undef beta
#define A (1/v1)
#define B (p3)
#define C (p5)
#define alpha (p1)
#define beta (p2)
#define gamma (p4)
          v=1/(A+B+C);
          btemp = -(alpha*C + alpha*B + gamma*A + gamma*B + beta*A + beta*C)*v;
          ctemp = (alpha*beta*C + alpha*gamma*B + beta*gamma*A)*v;
          dtemp = sqrt(btemp*btemp-4*ctemp);
          k21 = 0.5*(-btemp+dtemp);
          k31 = 0.5*(-btemp-dtemp);
          k   = alpha*beta*gamma/k21/k31;
          k12 = ((beta*gamma + alpha*beta + alpha*gamma) -
                 k21*(alpha+beta+gamma) - k * k31 + k21*k21)/(k31 - k21);
          k13 = alpha + beta + gamma - (k + k12 + k21 + k31);
          break;
        case 10:
#undef A
#define A v1
          v=1/(A+B+C);
          btemp = -(alpha*C + alpha*B + gamma*A + gamma*B + beta*A + beta*C)*v;
          ctemp = (alpha*beta*C + alpha*gamma*B + beta*gamma*A)*v;
          dtemp = sqrt(btemp*btemp-4*ctemp);
          k21 = 0.5*(-btemp+dtemp);
          k31 = 0.5*(-btemp-dtemp);
          k   = alpha*beta*gamma/k21/k31;
          k12 = ((beta*gamma + alpha*beta + alpha*gamma) -
                 k21*(alpha+beta+gamma) - k * k31 + k21*k21)/(k31 - k21);
          k13 = alpha + beta + gamma - (k + k12 + k21 + k31);
#undef A
#undef B
#undef C
#undef alpha
#undef beta
#undef gamma
#define beta Rf_beta
          break;
        }
      } break;
      case 2: { // 2 compartment model
        switch (trans){
        case 1: // cl=(*p1) v=(*v1) q=(*p2) vp=(*p3)
          k = p1/v1; // k = CL/V
          v = v1;
          k12 = p2/v1; // k12 = Q/V
          k21 = p2/p3; // k21 = Q/Vp
          break;
        case 2: // k=(*p1), (*v1)=v k12=(*p2) k21=(*p3)
          k = p1;
          v = v1;
          k12 = p2;
          k21 = p3;
          break;
        case 3: // cl=(*p1) v=(*v1) q=(*p2) vss=(*p3)
          k = p1/v1; // k = CL/V
          v = v1;
          k12 = p2/v1; // k12 = Q/V
          k21 = p2/(p3-v1); // k21 = Q/(Vss-V)
          break;
        case 4: // alpha=(*p1) beta=(*p2) k21=(*p3)
          v = v1;
          k21 = p3;
          k = p1*p2/k21; // (*p1) = alpha (*p2) = beta
          k12 = p1 + p2 - k21 - k;
          break;
        case 5: // alpha=(*p1) beta=(*p2) aob=(*p3)
          v=v1;
          k21 = (p3*p2+p1)/(p3+1);
          k = (p1*p2)/k21;
          k12 = p1+ p2 - k21 - k;
          break;
        case 11: // A2 V, alpha=(*p1), beta=(*p2), k21
#undef beta
#define A (1/v1)
#define B (p3)
#define alpha (p1)
#define beta (p2)
          v   = 1/(A+B);
          k21 = (A*beta + B*alpha)*v;
          k   = alpha*beta/k21;
          k12 = alpha+beta-k21-k;
          break;
        case 10: // A=(*v1), alpha=(*p1), beta=(*p2), B=(*p3)
          // Convert to A (right now A=(*v1) or A=1/(*v1))
#undef A
#define A (v1)
          v   = 1/(A + B);
          k21 = (A*beta + B*alpha)*v;
          k   = alpha*beta/k21;
          k12 = alpha + beta - k21 - k;
#undef A
#undef B
#undef alpha
#undef beta
#define beta Rf_beta
          break;
        default:
          //RSprintf(_("invalid trans (2 cmt trans %d)\n"), trans);
          return g;
        }
      } break;
      case 1:{ // One compartment model
        switch(trans){
        case 1: // cl v
          k = p1/v1; // k = CL/V
          v = v1;
          break;
        case 2: // k V
          k = p1;
          v = v1;
          break;
        case 11: // alpha V
          k = p1;
          v = v1;
          break;
        case 10: // alpha A
          k = p1;
          v = 1/v1;
          break;
        default:
          return g;
        }
      } break;
      }
#undef p1
#undef v1
#undef p2
#undef p3
#undef p4
#undef p5

#undef k
#undef v
#undef k12
#undef k21
#undef k13
#undef k31
      return g;
    }

  }
}

#endif
