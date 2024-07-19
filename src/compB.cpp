#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#define R_NO_REMAP
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stan/math.hpp>
#include <RcppEigen.h>
#include "../inst/include/rxode2.h"
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

extern "C" void RSprintf(const char *format, ...);
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
    parTransB(const Eigen::Matrix<T, Eigen::Dynamic, 1>& p,
                  const int& ncmt,
                  const int& trans) {
      Eigen::Matrix<T, Eigen::Dynamic, 2> g(ncmt,3);
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
        switch (trans){
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
      case 2:{ // 2 compartment model
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
          RSprintf(_("invalid trans (2 cmt trans %d)\n"), trans);
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
#undef p4

#undef k
#undef v
#undef k12
#undef k21
#undef k13
#undef k31
      return g;
    }
  }

#define v     g(0, 0)
#define k20   g(0, 1)
#define kel   g(0, 1)
#define k23   g(1, 0)
#define k32   g(1, 1)
#define k24   g(2, 0)
#define k42   g(2, 1)
#define k10   g(0, 1)
#define k12   g(1, 0)
#define k21   g(1, 1)
#define k13   g(2, 0)
#define k31   g(2, 1)

  template<class T>
  int
  solComp2Cpp(const  Eigen::Matrix<T, Eigen::Dynamic, 2> g,
              Eigen::Matrix<T, 2, 1>& L,
              Eigen::Matrix<T, 2, 2>& C1,
              Eigen::Matrix<T, 2, 2>& C2) {

    Eigen::Matrix<T, 2, 1> div;

    T sum = (k10) + (k12) + (k21);
    T disc= sqrt(sum*sum - 4.0* (k10)*(k21));
    T tmp;
    L(0, 0) = 0.5*(sum + disc);
    L(1, 0) = 0.5*(sum - disc);
    div(0, 0) = L(1, 0) - L(0, 0);
    div(1, 0) = L(0, 0) - L(1, 0);
    if (div(0, 0)*div(1, 0) == 0) return 0;
    // c[0] = (0, 0)
    // c[1] = (1, 0)
    // c[2] = (0, 1)
    // c[3] = (1, 1)

    C1(0, 0) = (k21) - L(0, 0);
    C1(0, 1) = (k21) - L(1, 0);
    C2(0, 0) = C2(0, 1) = (k21);
    C1(1, 0) = C1(1, 1) = (k12);
    tmp = (k10) + (k12);
    C2(1, 0) = tmp - L(0, 0);
    C2(1, 1) = tmp - L(1, 0);
    C1(0, 0) = C1(0, 0)/div(0, 0);
    C1(1, 0) = C1(1, 0)/div(0, 0);
    C2(0, 0) = C2(0, 0)/div(0, 0);
    C2(1, 0) = C2(1, 0)/div(0, 0);
    C1(0, 1) = C1(0, 1)/div(1, 0);
    C1(1, 1) = C1(1, 1)/div(1, 0);
    C2(0, 1) = C2(0, 1)/div(1, 0);
    C2(1, 1) = C2(1, 1)/div(1, 0);
    return 1;
  }

  template<class T>
  int
  solComp3Cpp(const  Eigen::Matrix<T, Eigen::Dynamic, 2> g,
              Eigen::Matrix<T, 3, 1>& L,
              Eigen::Matrix<T, 3, 3>& C1,
              Eigen::Matrix<T, 3, 3>& C2,
              Eigen::Matrix<T, 3, 3>& C3) {

    T A1 = k10 + k12 + k13 + k21 + k31;
    T A2 = k10*k21 + k10*k31 + k12*k31 +
      k13*k21 + k21*k31;
    T A3 = k21*k31*k10;
    T Q  = (A1*A1 - 3.0*A2)/9.0;
    T RQ = 2.0*sqrt(Q);
    T R  = (2.0*A1*A1*A1 - 9.0*A1*A2 + 27.0*A3)/54.0;
    T M  = Q*Q*Q - R*R;
    if (M < 0) return 0;//stop("Error: Not real roots.")
    T Th = acos(8.0*R/(RQ*RQ*RQ));
    L(0, 0) = RQ*cos(Th/3.0) + A1/3.0;
    L(1, 0) = RQ*cos((Th + M_2PI)/3.0) + A1/3.0;
    L(2, 0) = RQ*cos((Th + 2*M_2PI)/3.0) + A1/3.0;

    Eigen::Matrix<T, 3, 1> D;
    D(0, 0) = (L(1, 0) - L(0, 0))*(L(2, 0) - L(0, 0));
    D(1, 0) = (L(0, 0) - L(1, 0))*(L(2, 0) - L(1, 0));
    D(2, 0) = (L(0, 0) - L(2, 0))*(L(1, 0) - L(2, 0));
    if (D(0, 0)*D(1, 0)*D(2, 0) == 0.0) return 0;

    C1(0, 0) = (k21 - L(0, 0))*(k31 - L(0, 0));
    C1(0, 1) = (k21 - L(1, 0))*(k31 - L(1, 0));
    C1(0, 2) = (k21 - L(2, 0))*(k31 - L(2, 0));

    C2(0, 0) = (k21)*(k31 - L(0, 0));
    C2(0, 1) = (k21)*(k31 - L(1, 0));
    C2(0, 2) = (k21)*(k31 - L(2, 0));

    C3(0, 0) = (k31)*(k21 - L(0, 0));
    C3(0, 1) = (k31)*(k21 - L(1, 0));
    C3(0, 2) = (k31)*(k21 - L(2, 0));

    C1(1, 0) = (k12)*(k31 - L(0, 0));
    C1(1, 1) = (k12)*(k31 - L(1, 0));
    C1(1, 2) = (k12)*(k31 - L(2, 0));

    C2(1, 0) = (k10 + k12 + k13 - L(0, 0))*(k31 - L(0, 0)) - (k31)*(k13);
    C2(1, 1) = (k10 + k12 + k13 - L(1, 0))*(k31 - L(1, 0)) - (k31)*(k13);
    C2(1, 2) = (k10 + k12 + k13 - L(2, 0))*(k31 - L(2, 0)) - (k31)*(k13);

    C3(1, 0) = C3(1, 1) = C3(1, 2) = (k12)*(k31);

    C1(2, 0) = (k13)*(k21 - L(0, 0));
    C1(2, 1) = (k13)*(k21 - L(1, 0));
    C1(2, 2) = (k13)*(k21 - L(2, 0));

    C2(2, 0) = C2(2, 1) = C2(2, 2) = (k21)*(k13);

    C3(2, 0) = (k10 + k12 + k13 - L(0, 0))*(k21 - L(0, 0)) - (k21)*(k12);
    C3(2, 1) = (k10 + k12 + k13 - L(1, 0))*(k21 - L(1, 0)) - (k21)*(k12);
    C3(2, 2) = (k10 + k12 + k13 - L(2, 0))*(k21 - L(2, 0)) - (k21)*(k12);

    C1(0, 0) = C1(0, 0)/D(0, 0);
    C1(1, 0) = C1(1, 0)/D(0, 0);
    C1(2, 0) = C1(2, 0)/D(0, 0);

    C2(0, 0) = C2(0, 0)/D(0, 0);
    C2(1, 0) = C2(1, 0)/D(0, 0);
    C2(2, 0) = C2(2, 0)/D(0, 0);

    C3(0, 0) = C3(0, 0)/D(0, 0);
    C3(1, 0) = C3(1, 0)/D(0, 0);
    C3(2, 0) = C3(2, 0)/D(0, 0);

    C1(0, 1) = C1(0, 1)/D(1, 0);
    C1(1, 1) = C1(1, 1)/D(1, 0);
    C1(2, 1) = C1(2, 1)/D(1, 0);

    C2(0, 1) = C2(0, 1)/D(1, 0);
    C2(1, 1) = C2(1, 1)/D(1, 0);
    C2(2, 1) = C2(2, 1)/D(1, 0);

    C3(0, 1) = C3(0, 1)/D(1, 0);
    C3(1, 1) = C3(1, 1)/D(1, 0);
    C3(2, 1) = C3(2, 1)/D(1, 0);

    C1(0, 2) = C1(0, 2)/D(2, 0);
    C1(1, 2) = C1(1, 2)/D(2, 0);
    C1(2, 2) = C1(2, 2)/D(2, 0);

    C2(0, 2) = C2(0, 2)/D(2, 0);
    C2(1, 2) = C2(1, 2)/D(2, 0);
    C2(2, 2) = C2(2, 2)/D(2, 0);

    C3(0, 2) = C3(0, 2)/D(2, 0);
    C3(1, 2) = C3(1, 2)/D(2, 0);
    C3(2, 2) = C3(2, 2)/D(2, 0);

    return 1;
  }

  // now write R interfaces to check the solutions
}

using namespace Rcpp;
extern "C" SEXP _rxode2_solComp2cpp(SEXP k10s, SEXP k12s, SEXP k21s) {
  double k10d = as<double>(k10s);
  double k12d = as<double>(k12s);
  double k21d = as<double>(k21s);
  Eigen::Matrix<double, Eigen::Dynamic, 2> g(2, 2);
  v = 1.0;
  k10 = k10d;
  k12 = k12d;
  k21 = k21d;
  Eigen::Matrix<double, 2, 1> L;
  Eigen::Matrix<double, 2, 2> C1;
  Eigen::Matrix<double, 2, 2> C2;
  if (stan::solComp2Cpp(g, L, C1, C2) == 1) {
    return Rcpp::List::create(Rcpp::_["L"]=Rcpp::wrap(L),
                              Rcpp::_["C1"]=Rcpp::wrap(C1),
                              Rcpp::_["C2"]=Rcpp::wrap(C2));
  }
  return R_NilValue;
}

using namespace Rcpp;
extern "C" SEXP _rxode2_solComp3cpp(SEXP sK10, SEXP sK12, SEXP sK21,
                                    SEXP sK13, SEXP sK31) {
  double k10d = as<double>(sK10);
  double k12d = as<double>(sK12);
  double k21d = as<double>(sK21);
  double k13d = as<double>(sK13);
  double k31d = as<double>(sK31);
  Eigen::Matrix<double, Eigen::Dynamic, 2> g(3, 2);
  v = 1.0;
  k10 = k10d;
  k12 = k12d;
  k21 = k21d;
  k13 = k13d;
  k31 = k31d;
  Eigen::Matrix<double, 3, 1> L;
  Eigen::Matrix<double, 3, 3> C1;
  Eigen::Matrix<double, 3, 3> C2;
  Eigen::Matrix<double, 3, 3> C3;
  if (stan::solComp3Cpp(g, L, C1, C2, C3) == 1) {
    return Rcpp::List::create(Rcpp::_["L"]=Rcpp::wrap(L),
                              Rcpp::_["C1"]=Rcpp::wrap(C1),
                              Rcpp::_["C2"]=Rcpp::wrap(C2),
                              Rcpp::_["C3"]=Rcpp::wrap(C3));
  }
  return R_NilValue;
}
