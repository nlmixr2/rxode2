//#undef NDEBUG
#define NDEBUG // just in case
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stan/math.hpp>
#define NDEBUG // just in case
#include <Rcpp.h>
#include <RcppEigen.h>
#include "../inst/include/rxode2.h"
#include "handle_evid.h"
#include "getTime.h"
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

extern "C" int _locateTimeIndex(double obs_time,  rx_solving_options_ind *ind);
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
    micros2macros(const Eigen::Matrix<T, Eigen::Dynamic, 1>& p,
		  const int& ncmt,
		  const int& trans){
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

#define A1    A(0, 0)
#define A2    A(1, 0)
#define A3    A(2, 0)
#define A4    A(3, 0)
#define r1    rate(0, 0)
#define Doserate    rate(0, 0)
#define r2    rate(1, 0)
#define b1    bolus(0, 0)
#define b2    bolus(1, 0)
#define A1last Alast(0, 0)
#define A2last Alast(1, 0)
#define A3last Alast(2, 0)
#define A4last Alast(3, 0)
    // one compartment ka translations ncmt=1
#define tlag  pard(0, 0)
#define F     pard(1, 0)
#define rate1 pard(2, 0)
#define dur1  pard(3, 0)
#define tlag2 pard(4, 0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)
#define ka params(2, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtKaRateSSr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		     const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		     Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		     Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      A1 = r1/ka;
      A2 = r1/k20;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtKaRateSSr2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		     const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		     Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		     Eigen::Matrix<double, Eigen::Dynamic, 1>& rate){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      A1 = 0;
      A2 = r2/k20;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtKaRateSStr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		      const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		      Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		      Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
		      double tinf, double tau){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      T eKa = exp(-ka*(tau-tinf))/(1.0-exp(-tau*ka));
      T eiKa = exp(-ka*tinf);
      T eiK = exp(-k20*tinf);
      T eK = exp(-k20*(tau-tinf))/(1.0-exp(-tau*k20));
      A1=eKa*(r1/ka - eiKa*r1/ka);
      A2=eK*(r1/k20 + eiKa*r1/(-k20 + ka) - eiK*r1*ka/(ka*k20 - k20*k20)) +
	ka*(eK - eKa)*(r1/ka - eiKa*r1/ka)/(-k20 + ka);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtKaRateSStr2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		      const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		      Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		      Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
		      double tinf, double tau){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      T eiK = exp(-k20*tinf);
      T eK = exp(-k20*(tau-tinf))/(1.0-exp(-k20*tau));
      A1=0.0;
      A2=eK*(r2/k20 - eiK*r2*(-k20 + ka)/(ka*k20 - k20*k20));
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtKaRate(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
		 Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      T eKa = exp(-ka*t);
      T e20 = exp(-k20*t);
      A1 = r1/ka-((r1-A1last*ka)*eKa)/ka + b1;
      T A21 = ((r1-A1last*ka)*eKa)/(ka-k20);
      T A22=(((ka-k20)*r2+ka*r1+(-A2last-A1last)*k20*ka+A2last*k20*k20)*e20)/(k20*ka-k20*k20);
      T A23 =(r2+r1)/k20;
      A2 = A21 - A22 + A23 + b2;
      return A;
    }
    // Undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
    // two compartment ka translations ncmt=1
#define tlag  pard(0,  0)
#define F     pard(1,  0)
#define rate1 pard(2,  0)
#define dur1  pard(3,  0)
#define ka    params(4,  0)
#define tlag2 pard(4,  0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtKaRateSSr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		     const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		     Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		     Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T s = k23+k32+k20;
      //#Calculate roots
      T beta  = 0.5*(s - sqrt(s*s - 4*k32*k20));
      T alpha = k32*k20/beta;
      A1=r1/ka;
      A2=r1*k32/(beta*alpha);
      A3=r1*k23/(beta*alpha);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtKaRateSSr2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		     const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		     Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		     Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T s = k23+k32+k20;
      //#Calculate roots
      T beta  = 0.5*(s - sqrt(s*s - 4*k32*k20));
      T alpha = k32*k20/beta;
      A1=0;
      A2=r2*k32/(beta*alpha);
      A3=r2*k23/(beta*alpha);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtKaRateSStr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		      const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		      Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		      Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
		      double tinf, double tau){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T s = k23+k32+k20;
      //#Calculate roots
      T beta  = 0.5*(s - sqrt(s*s - 4*k32*k20));
      T alpha = k32*k20/beta;

      T eA = exp(-alpha*(tau-tinf))/(1.0-exp(-alpha*tau));
      T eB = exp(-beta*(tau-tinf))/(1.0-exp(-beta*tau));

      T eiA = exp(-alpha*tinf);
      T eiB = exp(-beta*tinf);

      T alpha2 = alpha*alpha;
      T alpha3 = alpha2*alpha;

      T beta2 = beta*beta;
      T beta3 = beta2*beta;

      T ka2 = ka*ka;

      T eKa = exp(-ka*(tau-tinf))/(1.0-exp(-ka*tau));
      T eiKa = exp(-ka*tinf);

      A1=eKa*(r1/ka - eiKa*r1/ka);
      A2=(eA*(-alpha*(r1*k32/(beta*alpha) + eiKa*r1*(-k32 + ka)/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*(-alpha + k32)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*(-beta + k32)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k32*(r1*k23/(beta*alpha) - eiKa*r1*k23/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*k23/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*k23/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k32*(r1*k32/(beta*alpha) + eiKa*r1*(-k32 + ka)/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*(-alpha + k32)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*(-beta + k32)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3))) - eB*(-beta*(r1*k32/(beta*alpha) + eiKa*r1*(-k32 + ka)/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*(-alpha + k32)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*(-beta + k32)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k32*(r1*k23/(beta*alpha) - eiKa*r1*k23/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*k23/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*k23/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k32*(r1*k32/(beta*alpha) + eiKa*r1*(-k32 + ka)/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*(-alpha + k32)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*(-beta + k32)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3))))/(-alpha + beta) + ka*(eA*(-alpha + k32)/((-alpha + beta)*(-alpha + ka)) + eB*(-beta + k32)/((-beta + ka)*(alpha - beta)) + eKa*(k32 - ka)/((beta - ka)*(alpha - ka)))*(r1/ka - eiKa*r1/ka);
      A3=(eA*(-alpha*(r1*k23/(beta*alpha) - eiKa*r1*k23/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*k23/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*k23/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k23*(r1*k32/(beta*alpha) + eiKa*r1*(-k32 + ka)/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*(-alpha + k32)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*(-beta + k32)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + (k20 + k23)*(r1*k23/(beta*alpha) - eiKa*r1*k23/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*k23/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*k23/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3))) - eB*(-beta*(r1*k23/(beta*alpha) - eiKa*r1*k23/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*k23/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*k23/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k23*(r1*k32/(beta*alpha) + eiKa*r1*(-k32 + ka)/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*(-alpha + k32)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*(-beta + k32)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + (k20 + k23)*(r1*k23/(beta*alpha) - eiKa*r1*k23/(beta*alpha + ka*(-alpha - beta) + ka2) - eiA*r1*ka*k23/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r1*ka*k23/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3))))/(-alpha + beta) + ka*k23*(eA/((-alpha + beta)*(-alpha + ka)) + eB/((-beta + ka)*(alpha - beta)) + eKa/((beta - ka)*(alpha - ka)))*(r1/ka - eiKa*r1/ka);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtKaRateSStr2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		      const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		      Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		      Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
		      double tinf, double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E2 = k20+k23;
      T E3 = k32;
      T s = k23+k32+k20;
      //#Calculate roots
      T beta  = 0.5*(s - sqrt(s*s - 4*k32*k20));
      T alpha = k32*k20/beta;

      T eA = exp(-alpha*(tau-tinf))/(1.0-exp(-alpha*tau));
      T eB = exp(-beta*(tau-tinf))/(1.0-exp(-beta*tau));

      T eiA = exp(-alpha*tinf);
      T eiB = exp(-beta*tinf);

      T alpha2 = alpha*alpha;
      T alpha3 = alpha2*alpha;

      T beta2 = beta*beta;
      T beta3 = beta2*beta;
      A1=0.0;
      A2=(eA*(E3*(r2*k32/(beta*alpha) - eiA*r2*(-k32*alpha + ka*(-alpha + k32) + alpha2)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k32*beta + ka*(-beta + k32) + beta2)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) - alpha*(r2*k32/(beta*alpha) - eiA*r2*(-k32*alpha + ka*(-alpha + k32) + alpha2)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k32*beta + ka*(-beta + k32) + beta2)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k32*(r2*k23/(beta*alpha) - eiA*r2*(-k23*alpha + ka*k23)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k23*beta + ka*k23)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3))) - eB*(E3*(r2*k32/(beta*alpha) - eiA*r2*(-k32*alpha + ka*(-alpha + k32) + alpha2)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k32*beta + ka*(-beta + k32) + beta2)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) - beta*(r2*k32/(beta*alpha) - eiA*r2*(-k32*alpha + ka*(-alpha + k32) + alpha2)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k32*beta + ka*(-beta + k32) + beta2)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k32*(r2*k23/(beta*alpha) - eiA*r2*(-k23*alpha + ka*k23)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k23*beta + ka*k23)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3))))/(-alpha + beta);
      A3=(eA*(E2*(r2*k23/(beta*alpha) - eiA*r2*(-k23*alpha + ka*k23)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k23*beta + ka*k23)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) - alpha*(r2*k23/(beta*alpha) - eiA*r2*(-k23*alpha + ka*k23)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k23*beta + ka*k23)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k23*(r2*k32/(beta*alpha) - eiA*r2*(-k32*alpha + ka*(-alpha + k32) + alpha2)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k32*beta + ka*(-beta + k32) + beta2)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3))) - eB*(E2*(r2*k23/(beta*alpha) - eiA*r2*(-k23*alpha + ka*k23)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k23*beta + ka*k23)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) - beta*(r2*k23/(beta*alpha) - eiA*r2*(-k23*alpha + ka*k23)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k23*beta + ka*k23)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3)) + k23*(r2*k32/(beta*alpha) - eiA*r2*(-k32*alpha + ka*(-alpha + k32) + alpha2)/(-beta*alpha2 + ka*(beta*alpha - alpha2) + alpha3) + eiB*r2*(-k32*beta + ka*(-beta + k32) + beta2)/(beta2*alpha + ka*(-beta*alpha + beta2) - beta3))))/(-alpha + beta);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtKaRate(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
		 Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E2 =  k20+ k23;
      T s = k23+k32+k20;
      //#Calculate roots
      T beta  = 0.5*(s - sqrt(s*s - 4*k32*k20));
      T alpha = k32*k20/beta;

      T ka2 = ka*ka;

      T alpha2 = alpha*alpha;
      T alpha3 = alpha2*alpha;

      T beta2 = beta*beta;
      T beta3 = beta2*beta;

      T eKa0 = exp(-ka*t);
      T eKa = eKa0/(ka2+(-beta-alpha)*ka+alpha*beta);
      T eA = exp(-alpha*t)/((alpha*beta-alpha2)*ka-alpha2*beta+alpha3);
      T eB = exp(-beta*t)/((beta2-alpha*beta)*ka-beta3+alpha*beta2);

      A1 = b1+r1/ka*(1-eKa0)+A1last*eKa0;
      A2 = b2+(((ka-k32)*r1-A1last*ka2+A1last*k32*ka)*eKa)+((((k32-beta)*ka-beta*k32+beta2)*r2+(k32-beta)*ka*r1+((-A3last-A2last-A1last)*beta*k32+(A2last+A1last)*beta2)*ka+(A3last+A2last)*beta2*k32-A2last*beta3)*eB)-((((k32-alpha)*ka-alpha*k32+alpha2)*r2+(k32-alpha)*ka*r1+((-A3last-A2last-A1last)*alpha*k32+(A2last+A1last)*alpha2)*ka+(A3last+A2last)*alpha2*k32-A2last*alpha3)*eA)+(k32*r2+k32*r1)/(alpha*beta);
      A3 = -((k23*r1-A1last*k23*ka)*eKa)+(((k23*ka-beta*k23)*r2+k23*ka*r1+((-A2last-A1last)*beta*k23+A3last*beta2-A3last*E2*beta)*ka+A2last*beta2*k23-A3last*beta3+A3last*E2*beta2)*eB)-(((k23*ka-alpha*k23)*r2+k23*ka*r1+((-A2last-A1last)*alpha*k23+A3last*alpha2-A3last*E2*alpha)*ka+A2last*alpha2*k23-A3last*alpha3+A3last*E2*alpha2)*eA)+(k23*r2+k23*r1)/(alpha*beta);
      return A;
    }

        // undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
        // three compartment ka translations ncmt=1
#define tlag  pard(0,  0)
#define F     pard(1,  0)
#define rate1 pard(2,  0)
#define dur1  pard(3,  0)
#define ka    params(6, 0)
#define tlag2 pard(4, 0)
#define f2    pard(5, 0)
#define dur2  pard(6, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtKaRateSSr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		       const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		       Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		       Eigen::Matrix<double, Eigen::Dynamic, 1>& rate){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(4, 1);
      //##Calculate roots - see Upton, 2004
      T j = k23+k20+k32+k42+k24;
      T k = k23*k42+k20*k32+k20*k42+k32*k42+k24*k32;
      T l = k20*k32*k42;

      T m = 0.3333333333333333*(3.0*k - j*j);
      T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
      T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T rho=sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T ct3 = cos(0.3333333333333333*theta);
      T rho3 = pow(rho,0.3333333333333333);
      T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
      T j3 = 0.3333333333333333*j;
      T lam1 = j3  + rho3*(ct3 + st3);
      T lam2 = j3 + rho3*(ct3 - st3);
      T lam3 = j3 -(2.0*rho3*ct3);
      T l123 = 1.0/(lam1*lam2*lam3);
      A1=r1/ka;
      A2=r1*k42*k32*l123;
      A3=r1*k42*k23*l123;
      A4=r1*k24*k32*l123;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtKaRateSSr2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		       const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		       Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		       Eigen::Matrix<double, Eigen::Dynamic, 1>& rate){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(4, 1);
      T j = k23+k20+k32+k42+k24;
      T k = k23*k42+k20*k32+k20*k42+k32*k42+k24*k32;
      T l = k20*k32*k42;

      T m = 0.3333333333333333*(3.0*k - j*j);
      T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
      T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T rho=sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T ct3 = cos(0.3333333333333333*theta);
      T rho3 = pow(rho,0.3333333333333333);
      T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
      T j3 = 0.3333333333333333*j;
      T lam1 = j3  + rho3*(ct3 + st3);
      T lam2 = j3 + rho3*(ct3 - st3);
      T lam3 = j3 -(2.0*rho3*ct3);
      T l123 = 1.0/(lam1*lam2*lam3);
      A1=0;
      A2=r2*k42*k32*l123;
      A3=r2*k42*k23*l123;
      A4=r2*k24*k32*l123;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtKaRateSStr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
			const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
			Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
			Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
			double tinf, double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(4, 1);
      T E2 =  k20+ k23 + k24;
      T E3 = k32;
      T E4 = k42;
      //##Calculate roots - see Upton, 2004
      T j = k23+k20+k32+k42+k24;
      T k = k23*k42+k20*k32+k20*k42+k32*k42+k24*k32;
      T l = k20*k32*k42;

      T m = 0.3333333333333333*(3.0*k - j*j);
      T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
      T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T rho=sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T ct3 = cos(0.3333333333333333*theta);
      T rho3 = pow(rho,0.3333333333333333);
      T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
      T j3 = 0.3333333333333333*j;
      T lam1 = j3  + rho3*(ct3 + st3);
      T lam2 = j3 + rho3*(ct3 - st3);
      T lam3 = j3 -(2.0*rho3*ct3);

      T eKa = exp(-ka*(tau-tinf))/(1.0-exp(-ka*tau));
      T eiKa = exp(-ka*tinf);

      T eL1 = exp(-lam1*(tau-tinf))/(1.0-exp(-lam1*tau));
      T eiL1 = exp(-lam1*tinf);

      T eL2 = exp(-lam2*(tau-tinf))/(1.0-exp(-lam2*tau));
      T eiL2 = exp(-lam2*tinf);

      T eL3 = exp(-lam3*(tau-tinf))/(1.0-exp(-lam3*tau));
      T eiL3 = exp(-lam3*tinf);

      T ka2 = ka*ka;
      T ka3 = ka2*ka;

      T lam12 = lam1*lam1;
      T lam13 = lam12*lam1;
      T lam14 = lam13*lam1;

      T lam22 = lam2*lam2;
      T lam23 = lam22*lam2;
      T lam24 = lam23*lam2;

      T lam32 = lam3*lam3;
      T lam33 = lam32*lam3;
      T lam34 = lam33*lam3;
      A1=eKa*(r1/ka - eiKa*r1/ka);
      A2=(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) + ka*(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*(ka - lam1)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*(ka - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*(ka - lam3)) + eKa*(E3 - ka)*(E4 - ka)/((-ka + lam1)*(-ka + lam2)*(-ka + lam3)))*(r1/ka - eiKa*r1/ka) + eL1*(-lam1*(k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3)) + k42*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3))) + E3*k42*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) + E4*k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*(k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3)) + k42*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3))) - (E3*k42*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) + E4*k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*(k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3)) + k42*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3))) - (E3*k42*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) + E4*k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
      A3=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3)) + ka*k23*(eL1*(E4 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*(ka - lam1)) + eL2*(E4 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*(ka - lam2)) + eL3*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*(ka - lam3)) + eKa*(E4 - ka)/((-ka + lam1)*(-ka + lam2)*(-ka + lam3)))*(r1/ka - eiKa*r1/ka) + eL1*(E4*k23*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) - k23*lam1*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) + k42*k23*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) - k42*k24*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(k23*lam3*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) - (E4*k23*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) + k42*k23*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) - k42*k24*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(k23*lam2*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) - (E4*k23*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) + k42*k23*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) - k42*k24*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
      A4=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) + ka*k24*(eL1*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*(ka - lam1)) + eL2*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*(ka - lam2)) + eL3*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)*(ka - lam3)) + eKa*(E3 - ka)/((-ka + lam1)*(-ka + lam2)*(-ka + lam3)))*(r1/ka - eiKa*r1/ka) + eL1*(E3*k24*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) - k23*k32*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) + k24*k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3)) - k24*lam1*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(k24*lam3*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) - (E3*k24*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) - k23*k32*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) + k24*k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(k24*lam2*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) - (E3*k24*(-eiKa*r1*(k42*k32 + (-k32 - k42)*ka + ka2)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) - eiL1*r1*(-ka*lam12 - ka*k42*k32 + (k32 + k42)*ka*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r1*(-ka*lam22 - ka*k42*k32 + (k32 + k42)*ka*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r1*(-ka*lam32 - ka*k42*k32 + (k32 + k42)*ka*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k32/(lam2*lam1*lam3)) - k23*k32*(eiKa*r1*(-k24*k32 + ka*k24)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(ka*k24*k32 - ka*k24*lam1)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(ka*k24*k32 - ka*k24*lam2)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(ka*k24*k32 - ka*k24*lam3)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k24*k32/(lam2*lam1*lam3)) + k24*k32*(eiKa*r1*(-k42*k23 + ka*k23)/(ka2*lam1 + lam2*(-ka*lam1 + ka2) + lam3*(-ka*lam1 + lam2*(-ka + lam1) + ka2) - ka3) + eiL1*r1*(-ka*k23*lam1 + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r1*(-ka*k23*lam2 + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r1*(-ka*k23*lam3 + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r1*k42*k23/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtKaRateSStr2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
			const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
			Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
			Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
			double tinf, double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(4, 1);
      T E2 =  k20+ k23 + k24;
      T E3 = k32;
      T E4 = k42;
      //##Calculate roots - see Upton, 2004
      T j = k23+k20+k32+k42+k24;
      T k = k23*k42+k20*k32+k20*k42+k32*k42+k24*k32;
      T l = k20*k32*k42;

      T m = 0.3333333333333333*(3.0*k - j*j);
      T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
      T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T rho=sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T ct3 = cos(0.3333333333333333*theta);
      T rho3 = pow(rho,0.3333333333333333);
      T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
      T j3 = 0.3333333333333333*j;
      T lam1 = j3  + rho3*(ct3 + st3);
      T lam2 = j3 + rho3*(ct3 - st3);
      T lam3 = j3 -(2.0*rho3*ct3);

      /* T eKa = 1.0/(1.0-exp(-ka*tau)); */
      /* T eiKa = exp(-ka*tinf); */

      T eL1 = exp(-lam1*(tau-tinf))/(1.0-exp(-lam1*tau));
      T eiL1 = exp(-lam1*tinf);

      T eL2 = exp(-lam2*(tau-tinf))/(1.0-exp(-lam2*tau));
      T eiL2 = exp(-lam2*tinf);

      T eL3 = exp(-lam3*(tau-tinf))/(1.0-exp(-lam3*tau));
      T eiL3 = exp(-lam3*tinf);

      /* T ka2 = ka*ka; */
      /* T ka3 = ka2*ka; */

      T lam12 = lam1*lam1;
      T lam13 = lam12*lam1;
      T lam14 = lam13*lam1;

      T lam22 = lam2*lam2;
      T lam23 = lam22*lam2;
      T lam24 = lam23*lam2;

      T lam32 = lam3*lam3;
      T lam33 = lam32*lam3;
      T lam34 = lam33*lam3;
      A1=0.0;
      A2=(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) + eL1*(-lam1*(k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3)) + k42*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3))) + E3*k42*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) + E4*k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*(k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3)) + k42*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3))) - (E3*k42*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) + E4*k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*(k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3)) + k42*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3))) - (E3*k42*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) + E4*k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
      A3=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3)) + eL1*(E4*k23*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) - k23*lam1*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) + k42*k23*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) - k42*k24*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(k23*lam3*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) - (E4*k23*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) + k42*k23*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) - k42*k24*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(k23*lam2*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) - (E4*k23*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) + k42*k23*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) - k42*k24*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
      A4=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) + eL1*(E3*k24*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) - k23*k32*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) + k24*k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3)) - k24*lam1*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(k24*lam3*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) - (E3*k24*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) - k23*k32*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) + k24*k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(k24*lam2*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) - (E3*k24*(-eiL1*r2*(lam1*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam12 - ka*k42*k32 + lam13)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) + eiL2*r2*(lam2*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam22 - ka*k42*k32 + lam23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) - eiL3*r2*(lam3*(k42*k32 + (k32 + k42)*ka) + (-k32 - k42 - ka)*lam32 - ka*k42*k32 + lam33)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k32/(lam2*lam1*lam3)) - k23*k32*(eiL1*r2*(k24*lam12 + lam1*(-k24*k32 - ka*k24) + ka*k24*k32)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k24*lam22 + lam2*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k24*lam32 + lam3*(-k24*k32 - ka*k24) + ka*k24*k32)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k24*k32/(lam2*lam1*lam3)) + k24*k32*(eiL1*r2*(k23*lam12 + lam1*(-k42*k23 - ka*k23) + ka*k42*k23)/(-ka*lam13 + lam2*(ka*lam12 - lam13) + lam3*(ka*lam12 + lam2*(-ka*lam1 + lam12) - lam13) + lam14) - eiL2*r2*(k23*lam22 + lam2*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam23*(ka + lam1) + lam3*(lam22*(-ka - lam1) + ka*lam2*lam1 + lam23) - ka*lam22*lam1 - lam24) + eiL3*r2*(k23*lam32 + lam3*(-k42*k23 - ka*k23) + ka*k42*k23)/(lam32*(ka*lam1 + lam2*(ka + lam1)) + (-ka - lam1 - lam2)*lam33 - ka*lam2*lam1*lam3 + lam34) + r2*k42*k23/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtKaRate(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
		   Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		   const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		   Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		   Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		   Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(4, 1);
      T E2 =  k20+ k23 + k24;
      //##Calculate roots - see Upton, 2004
      T j = k23+k20+k32+k42+k24;
      T k = k23*k42+k20*k32+k20*k42+k32*k42+k24*k32;
      T l = k20*k32*k42;

      T m = 0.3333333333333333*(3.0*k - j*j);
      T n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
      T Q = 0.25*n*n + 0.03703703703703703*m*m*m;

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T rho=sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T ct3 = cos(0.3333333333333333*theta);
      T rho3 = pow(rho,0.3333333333333333);
      T st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
      T j3 = 0.3333333333333333*j;
      T lam1 = j3  + rho3*(ct3 + st3);
      T lam2 = j3 + rho3*(ct3 - st3);
      T lam3 = j3 -(2.0*rho3*ct3);
      T eKa = exp(-ka*t);
      A1 = b1+ r1/ka-((r1-A1last*ka)*eKa)/ka;

      T lam12 = lam1*lam1;
      T lam13 = lam12*lam1;
      T lam14 = lam13*lam1;

      T lam22 = lam2*lam2;
      T lam23 = lam22*lam2;
      T lam24 = lam23*lam2;

      T lam32 = lam3*lam3;
      T lam33 = lam32*lam3;
      T lam34 = lam33*lam3;

      T ka2 = ka*ka;
      T ka3 = ka2*ka;

      T a21 = (((lam33+(-ka-k42-k32)*lam32+((k42+k32)*ka+k32*k42)*lam3-k32*k42*ka)*r2+(-ka*lam32+(k42+k32)*ka*lam3-k32*k42*ka)*r1-A2last*lam34+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam33+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam32+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam3)*exp(-lam3*t))/(lam34+(-lam2-lam1-ka)*lam33+((lam1+ka)*lam2+ka*lam1)*lam32-ka*lam1*lam2*lam3);
      T a22 = (((lam23+(-ka-k42-k32)*lam22+((k42+k32)*ka+k32*k42)*lam2-k32*k42*ka)*r2+(-ka*lam22+(k42+k32)*ka*lam2-k32*k42*ka)*r1-A2last*lam24+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam23+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam22+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam2)*exp(-lam2*t))/((lam23+(-lam1-ka)*lam22+ka*lam1*lam2)*lam3-lam24+(lam1+ka)*lam23-ka*lam1*lam22);
      T a23 = (((lam13+(-ka-k42-k32)*lam12+((k42+k32)*ka+k32*k42)*lam1-k32*k42*ka)*r2+(-ka*lam12+(k42+k32)*ka*lam1-k32*k42*ka)*r1-A2last*lam14+((A2last+A1last)*ka+(A4last+A2last)*k42+(A3last+A2last)*k32)*lam13+(((-A4last-A2last-A1last)*k42+(-A3last-A2last-A1last)*k32)*ka+(-A4last-A3last-A2last)*k32*k42)*lam12+(A4last+A3last+A2last+A1last)*k32*k42*ka*lam1)*exp(-lam1*t))/(((lam12-ka*lam1)*lam2-lam13+ka*lam12)*lam3+(ka*lam12-lam13)*lam2+lam14-ka*lam13);
      T a24 = (((ka2+(-k42-k32)*ka+k32*k42)*r1-A1last*ka3+(A1last*k42+A1last*k32)*ka2-A1last*k32*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka2)*lam3+(ka2-ka*lam1)*lam2+ka2*lam1-ka3);
      T a25 = (k32*k42*r2+k32*k42*r1)/(lam1*lam2*lam3);
      A2 = b2-a21+a22-a23-a24+a25;
      T a31 = (((k23*lam32+(-k23*ka-k23*k42)*lam3+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam3)*r1+A3last*lam34+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam33+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam32+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam3)*exp(-lam3*t))/(lam34+(-lam2-lam1-ka)*lam33+((lam1+ka)*lam2+ka*lam1)*lam32-ka*lam1*lam2*lam3);
      T a32 = (((k23*lam22+(-k23*ka-k23*k42)*lam2+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam2)*r1+A3last*lam24+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam23+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam22+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam2)*exp(-lam2*t))/((lam23+(-lam1-ka)*lam22+ka*lam1*lam2)*lam3-lam24+(lam1+ka)*lam23-ka*lam1*lam22);
      T a33 = (((k23*lam12+(-k23*ka-k23*k42)*lam1+k23*k42*ka)*r2+(k23*k42*ka-k23*ka*lam1)*r1+A3last*lam14+(-A3last*ka-A3last*k42-A2last*k23-A3last*E2)*lam13+((A3last*k42+(A2last+A1last)*k23+A3last*E2)*ka+(-A3last*k24+(A4last+A2last)*k23+A3last*E2)*k42)*lam12+(A3last*k24+(-A4last-A2last-A1last)*k23-A3last*E2)*k42*ka*lam1)*exp(-lam1*t))/(((lam12-ka*lam1)*lam2-lam13+ka*lam12)*lam3+(ka*lam12-lam13)*lam2+lam14-ka*lam13);
      T a34 = (((k23*ka-k23*k42)*r1-A1last*k23*ka2+A1last*k23*k42*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka2)*lam3+(ka2-ka*lam1)*lam2+ka2*lam1-ka3);
      T a35 = (k23*k42*r2+k23*k42*r1)/(lam1*lam2*lam3);
      A3=a31-a32+a33+a34+a35;
      T a41 = (((k24*lam32+(-k24*ka-k24*k32)*lam3+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam3)*r1+A4last*lam34+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam33+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam32+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam3)*exp(-lam3*t))/(lam34+(-lam2-lam1-ka)*lam33+((lam1+ka)*lam2+ka*lam1)*lam32-ka*lam1*lam2*lam3);
      T a42 = (((k24*lam22+(-k24*ka-k24*k32)*lam2+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam2)*r1+A4last*lam24+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam23+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam22+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam2)*exp(-lam2*t))/((lam23+(-lam1-ka)*lam22+ka*lam1*lam2)*lam3-lam24+(lam1+ka)*lam23-ka*lam1*lam22);
      T a43 = (((k24*lam12+(-k24*ka-k24*k32)*lam1+k24*k32*ka)*r2+(k24*k32*ka-k24*ka*lam1)*r1+A4last*lam14+(-A4last*ka-A4last*k32-A2last*k24-A4last*E2)*lam13+((A4last*k32+(A2last+A1last)*k24+A4last*E2)*ka+((A3last+A2last)*k24-A4last*k23+A4last*E2)*k32)*lam12+((-A3last-A2last-A1last)*k24+A4last*k23-A4last*E2)*k32*ka*lam1)*exp(-lam1*t))/(((lam12-ka*lam1)*lam2-lam13+ka*lam12)*lam3+(ka*lam12-lam13)*lam2+lam14-ka*lam13);
      T a44 = (((k24*ka-k24*k32)*r1-A1last*k24*ka2+A1last*k24*k32*ka)*exp(-ka*t))/(((lam1-ka)*lam2-ka*lam1+ka2)*lam3+(ka2-ka*lam1)*lam2+ka2*lam1-ka3);
      T a45 = (k24*k32*r2+k24*k32*r1)/(lam1*lam2*lam3);
      A4=a41-a42+a43+a44+a45;
      return A;
    }

        // undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2


        // one compartment ka translations ncmt=1
#define tlag  pard(0, 0)
#define F     pard(1, 0)
#define rate1 pard(2, 0)
#define dur1  pard(3, 0)
#define ka    params(2, 0)
#define tlag2 pard(4, 0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtKaSSb1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		 double tau) {

      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      T eKa = 1.0/(1.0-exp(-tau*ka));
      T eK =  1.0/(1.0-exp(-tau*k20));
      A1=eKa*b1;
      A2=ka*b1*(eK - eKa)/(-k20 + ka);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtKaSSb2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		 double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      T eK =  1.0/(1.0-exp(-tau*k20));
      A1=0.0;
      A2=eK*b2;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtKa(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
	     Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	     const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	     Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	     Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      T rx_expr_0=exp(-t*ka);
      A1=A1last*rx_expr_0+b1;
      T rx_expr_1=exp(-t*k20);
      A2=A1last*ka/(ka-k20)*(rx_expr_1-rx_expr_0)+A2last*rx_expr_1+b2;
      return A;
    }

    // Undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
    // two compartment ka translations ncmt=1
#define tlag  pard(0,  0)
#define F     pard(1,  0)
#define rate1 pard(2,  0)
#define dur1  pard(3,  0)
#define ka    params(4,  0)
#define tlag2 pard(4,  0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtKaSSb1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		 double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E2 = k20+k23;
      T E3 = k32;
      T e2e3 = E2+E3;
      T s = sqrt(e2e3*e2e3-4*(E2*E3-k23*k32));

      //calculate hybrid rate constants
      T lambda1 = 0.5*(e2e3+s);
      T lambda2 = 0.5*(e2e3-s);
      T eKa=1.0/(1.0-exp(-tau*ka));
      T eL1=1.0/(1.0-exp(-tau*lambda1));
      T eL2=1.0/(1.0-exp(-tau*lambda2));
      A1=eKa*b1;
      A2=ka*b1*(eL1*(E3 - lambda1)/((-lambda1 + lambda2)*(ka - lambda1)) + eL2*(E3 - lambda2)/((lambda1 - lambda2)*(ka - lambda2)) + eKa*(E3 - ka)/((-ka + lambda2)*(-ka + lambda1)));
      A3=ka*b1*k23*(eL1/((-lambda1 + lambda2)*(ka - lambda1)) + eL2/((lambda1 - lambda2)*(ka - lambda2)) + eKa/((-ka + lambda2)*(-ka + lambda1)));
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtKaSSb2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		 double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E2 = k20+k23;
      T E3 = k32;
      T e2e3 = E2+E3;
      T s = sqrt(e2e3*e2e3-4*(E2*E3-k23*k32));

      //calculate hybrid rate constants
      T lambda1 = 0.5*(e2e3+s);
      T lambda2 = 0.5*(e2e3-s);
      /* T eKa=1.0/(1.0+exp(-tau*ka)); */
      T eL1=1.0/(1.0-exp(-tau*lambda1));
      T eL2=1.0/(1.0-exp(-tau*lambda2));

      A1=0.0;
      A2=(eL1*(b2*E3 - b2*lambda1) - eL2*(b2*E3 - b2*lambda2))/(-lambda1 + lambda2);
      A3=(eL1*b2*k23 - eL2*b2*k23)/(-lambda1 + lambda2);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtKa(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
	     Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	     const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	     Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	     Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
#define E2 (k20+ k23)
#define s  (k23+k32+k20)
      //#Calculate roots
      T beta  = 0.5*(s - sqrt(s*s - 4*k32*k20));
      T alpha = k32*k20/beta;


#define ka2 (ka*ka)

      T alpha2 = (alpha*alpha);
      T alpha3 =  alpha2*alpha;

      T beta2 = (beta*beta);
      T beta3 = beta2*beta;

      T eA0 = exp(-alpha*t)/(alpha3 - beta*alpha2 + ka*(-alpha2 + beta*alpha));
      T eB0 = exp(-beta*t)/(beta2*alpha + ka*(beta2 - beta*alpha) - beta3);

      T Al23  = (A2last + A3last)*k32;
      T Al123 = (A1last + A2last + A3last)*k32;
      T Al12 =  (A1last + A2last);
      T eKa0 = exp(-ka*t);
      A1 = b1 + eKa0*A1last;
      eKa0 = eKa0*A1last/(ka2 + beta*alpha - ka*(alpha + beta));
      A2 = b2 - eA0*(ka*(Al12*alpha2 - alpha*Al123) + Al23*alpha2 - alpha3*A2last) +
	eB0*(ka*(Al12*beta2 - beta*Al123) + Al23*beta2 - beta3*A2last) +
	eKa0*(ka*k32 - ka2);
      Al12 =  Al12*k23;
      A3 = eB0*(A3last*(ka*beta2-ka*E2*beta+ E2*beta2 - beta3) - ka*Al12*beta  + k23*beta2*A2last) 
	-eA0*(ka*alpha2*A3last - ka*E2*alpha*A3last - ka*Al12*alpha + E2*alpha2*A3last + k23*alpha2*A2last-alpha3*A3last)+
	eKa0*ka*k23;
#undef E2
#undef s
#undef ka2
#undef beta2
#undef alpha2
#undef Al12
      return A;
    }

    // undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
    // three compartment ka translations ncmt=1
#define tlag  pard(0,  0)
#define F     pard(1,  0)
#define rate1 pard(2,  0)
#define dur1  pard(3,  0)
#define ka    params(6, 0)
#define tlag2 pard(4, 0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtKaSSb1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		   const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		   Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		   Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		   double tau){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(4, 1);
      T E2 = k20+k23+k24;
      T E3 = k32;
      T E4 = k42;

      //calculate hybrid rate constants
      T a = E2+E3+E4;
      T b = E2*E3+E4*(E2+E3)-k23*k32-k24*k42;
      T c = E2*E3*E4-E4*k23*k32-E3*k24*k42;

      T a2 = a*a;
      T m = 0.333333333333333*(3.0*b - a2);
      T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
      T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T gamma = sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T theta3 = 0.333333333333333*theta;
      T ctheta3 = cos(theta3);
      T stheta3 = 1.7320508075688771932*sin(theta3);
      T gamma3 = pow(gamma,0.333333333333333);

      T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
      T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
      T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

      T eKa = 1.0/(1.0-exp(-tau*ka));
      T eL1 = 1.0/(1.0-exp(-tau*lambda1));
      T eL2 = 1.0/(1.0-exp(-tau*lambda2));
      T eL3 = 1.0/(1.0-exp(-tau*lambda3));

      A1=eKa*b1;
      A2=ka*b1*(eL1*(E3 - lambda1)*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*(ka - lambda1)) + eL2*(E3 - lambda2)*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*(ka - lambda2)) + eL3*(E3 - lambda3)*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*(ka - lambda3)) + eKa*(E3 - ka)*(E4 - ka)/((-ka + lambda1)*(-ka + lambda3)*(-ka + lambda2)));
      A3=ka*b1*k23*(eL1*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*(ka - lambda1)) + eL2*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*(ka - lambda2)) + eL3*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*(ka - lambda3)) + eKa*(E4 - ka)/((-ka + lambda1)*(-ka + lambda3)*(-ka + lambda2)));
      A4=ka*b1*k24*(eL1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*(ka - lambda1)) + eL2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*(ka - lambda2)) + eL3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*(ka - lambda3)) + eKa*(E3 - ka)/((-ka + lambda1)*(-ka + lambda3)*(-ka + lambda2)));
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtKaSSb2(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		   const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		   Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		   Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		   double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(4, 1);
      T E2 = k20+k23+k24;
      T E3 = k32;
      T E4 = k42;

      //calculate hybrid rate constants
      T a = E2+E3+E4;
      T b = E2*E3+E4*(E2+E3)-k23*k32-k24*k42;
      T c = E2*E3*E4-E4*k23*k32-E3*k24*k42;

      T a2 = a*a;
      T m = 0.333333333333333*(3.0*b - a2);
      T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
      T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T gamma = sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T theta3 = 0.333333333333333*theta;
      T ctheta3 = cos(theta3);
      T stheta3 = 1.7320508075688771932*sin(theta3);
      T gamma3 = pow(gamma,0.333333333333333);

      T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
      T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
      T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

      /* T eKa = 1.0/(1.0-exp(-tau*KA)); */
      T eL1 = 1.0/(1.0-exp(-tau*lambda1));
      T eL2 = 1.0/(1.0-exp(-tau*lambda2));
      T eL3 = 1.0/(1.0-exp(-tau*lambda3));
      A1=0.0;
      A2=b2*(eL1*(E3 - lambda1)*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eL2*(E3 - lambda2)*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eL3*(E3 - lambda3)*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)));
      A3=eL2*(-b2*E4*k23 + b2*k23*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*(b2*E4*k23 - b2*k23*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-b2*E4*k23 + b2*k23*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
      A4=eL2*(-b2*E3*k24 + b2*k24*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*(b2*E3*k24 - b2*k24*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-b2*E3*k24 + b2*k24*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtKa(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
	       Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	       const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	       Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	       Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(4, 1);
      T E2 = k20+k23+k24;
      T E3 = k32;
      T E4 = k42;

      //calculate hybrid rate constants
      T a = E2+E3+E4;
      T b = E2*E3+E4*(E2+E3)-k23*k32-k24*k42;
      T c = E2*E3*E4-E4*k23*k32-E3*k24*k42;

      T a2 = a*a;
      T m = 0.333333333333333*(3.0*b - a2);
      T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
      T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T gamma = sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T theta3 = 0.333333333333333*theta;
      T ctheta3 = cos(theta3);
      T stheta3 = 1.7320508075688771932*sin(theta3);
      T gamma3 = pow(gamma,0.333333333333333);

      T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
      T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
      T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

      T B = A3last*k32+A4last*k42;
      T C = E4*A3last*k32+E3*A4last*k42;
      T I = A2last*k23*E4-A3last*k24*k42+A4last*k23*k42;
      T J = A2last*k24*E3+A3last*k24*k32-A4last*k23*k32;

      T eL1 = exp(-t*lambda1);
      T eL2 = exp(-t*lambda2);
      T eL3 = exp(-t*lambda3);
      T eKA = exp(-t*ka);

      T l12 = (lambda1-lambda2);
      T l13 = (lambda1-lambda3);
      T l21 = (lambda2-lambda1);
      T l23 = (lambda2-lambda3);
      T l31 = (lambda3-lambda1);
      T l32 = (lambda3-lambda2);

      T e2l1 = (E2-lambda1);
      T e2l2 = (E2-lambda2);
      T e3l1 = (E3-lambda1);
      T e3l2 = (E3-lambda2);
      T e3l3 = (E3-lambda3);
      T e4l1 = (E4-lambda1);
      T e4l2 = (E4-lambda2);
      T e4l3 = (E4-lambda3);

      T A2term1 = A2last*(eL1*e3l1*e4l1/(l21*l31)+eL2*e3l2*e4l2/(l12*l32)+eL3*e3l3*e4l3/(l13*l23));

      T A2term2 = eL1*(C-B*lambda1)/(l12*l13)+eL2*(B*lambda2-C)/(l12*l23)+eL3*(B*lambda3-C)/(l13*l32);

      T A2term3 = A1last*ka*(eL1*e3l1*e4l1/(l21*l31*(ka-lambda1))+eL2*e3l2*e4l2/(l12*l32*(ka-lambda2))+eL3*e3l3*e4l3/(l13*l23*(ka-lambda3))+eKA*(E3-ka)*(E4-ka)/((lambda1-ka)*(lambda2-ka)*(lambda3-ka)));

      A2 = A2term1+A2term2+A2term3 + b2;

      T A3term1 = A3last*(eL1*e2l1*e4l1/(l21*l31)+eL2*e2l2*e4l2/(l12*l32)+eL3*(E2-lambda3)*e4l3/(l13*l23));

      T A3term2 = eL1*(I-A2last*k23*lambda1)/(l12*l13)+eL2*(A2last*k23*lambda2-I)/(l12*l23)+eL3*(A2last*k23*lambda3-I)/(l13*l32);

      T A3term3 = A1last*ka*k23*(eL1*e4l1/(l21*l31*(ka-lambda1))+eL2*e4l2/(l12*l32*(ka-lambda2))+eL3*e4l3/(l13*l23*(ka-lambda3))+eKA*(E4-ka)/((lambda1-ka)*(lambda2-ka)*(lambda3-ka)));

      A3 = A3term1+A3term2+A3term3;// Amount in the first-peripheral compartment

      T A4term1 = A4last*(eL1*e2l1*e3l1/(l21*l31)+eL2*e2l2*e3l2/(l12*l32)+eL3*(E2-lambda3)*e3l3/(l13*l23));

      T A4term2 = eL1*(J-A2last*k24*lambda1)/(l12*l13)+eL2*(A2last*k24*lambda2-J)/(l12*l23)+eL3*(A2last*k24*lambda3-J)/(l13*l32);

      T A4term3 = A1last*ka*k24*(eL1*e3l1/(l21*l31*(ka-lambda1))+eL2*e3l2/(l12*l32*(ka-lambda2))+eL3*e3l3/(l13*l23*(ka-lambda3))+eKA*(E3-ka)/((lambda1-ka)*(lambda2-ka)*(lambda3-ka)));
      A4 = A4term1+A4term2+A4term3;

      A1 = A1last*eKA + b1;
      return A;
    }

            // undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2


        // one compartment ka translations ncmt=1
#define tlag  pard(0, 0)
#define F     pard(1, 0)
#define rate1 pard(2, 0)
#define dur1  pard(3, 0)
#define ka    params(2, 0)
#define tlag2 pard(4, 0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtRateSSr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		   const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		   Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		   Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(1, 1);
      A1 = r1/k10;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtRateSS(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
		 double tinf, double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(1, 1);
      T eiK = exp(-k10*tinf);
      T eK = exp(-k10*(tau-tinf))/(1.0-exp(-k10*tau));
      A1=r1*(1-eiK)*eK/(k10);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtRate(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
	       Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	       const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	       Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	       Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
	       Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(1, 1);
      T eT = exp(-k10*t);
      A1 = r1/k10*(1-eT)+A1last*eT + b1;
      return A;
    }
    // Undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
    // two compartment ka translations ncmt=1
#define tlag  pard(0,  0)
#define F     pard(1,  0)
#define rate1 pard(2,  0)
#define dur1  pard(3,  0)
#define ka    params(4,  0)
#define tlag2 pard(4,  0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtRateSSr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		   const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		   Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		   Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      T E1 = k10+k12;
      T E2 = k21;
      //#calculate hybrid rate constants
      T s = E1+E2;
      T sqr = sqrt(s*s-4*(E1*E2-k12*k21));
      T lambda1 = 0.5*(s+sqr);
      T lambda2 = 0.5*(s-sqr);
      T l12 = 1.0/(lambda1*lambda2);
      A1=r1*E2*l12;
      A2=r1*k12*l12;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtRateSS(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
		 double tinf, double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      T E1 = k10+k12;
      T E2 = k21;

      //#calculate hybrid rate constants
      T s = E1+E2;
      T sqr = sqrt(s*s-4*(E1*E2-k12*k21));
      T lambda1 = 0.5*(s+sqr);
      T lambda2 = 0.5*(s-sqr);

      T eTi1 = exp(-tinf*lambda1);
      T eTi2 = exp(-tinf*lambda2);
      T eT1 =exp(-lambda1*(tau-tinf))/(1.0-exp(-tau*lambda1));
      T eT2 =exp(-lambda2*(tau-tinf))/(1.0-exp(-tau*lambda2));
      A1=(eT1*(E2*((eTi1*r1 - eTi2*r1)/(-lambda1 + lambda2) + r1*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) - lambda1*((eTi1*r1 - eTi2*r1)/(-lambda1 + lambda2) + r1*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) + r1*k12*k21*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) - eT2*(E2*((eTi1*r1 - eTi2*r1)/(-lambda1 + lambda2) + r1*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) - lambda2*((eTi1*r1 - eTi2*r1)/(-lambda1 + lambda2) + r1*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) + r1*k12*k21*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))))/(-lambda1 + lambda2);
      A2=(eT1*(k12*((eTi1*r1 - eTi2*r1)/(-lambda1 + lambda2) + r1*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) + r1*E1*k12*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2)) - r1*k12*lambda1*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) - eT2*(k12*((eTi1*r1 - eTi2*r1)/(-lambda1 + lambda2) + r1*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) + r1*E1*k12*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2)) - r1*k12*lambda2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))))/(-lambda1 + lambda2);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtRate(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
	       Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	       const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	       Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	       Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
	       Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      //#calculate hybrid rate constants
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
#define E1 (k10+k12)
#define E2 k21
      //#calculate hybrid rate constants
#define s  (E1+E2)
      T sqr = sqrt(s*s-4*(E1*E2-k12*k21));
      T lambda1 = 0.5*(s+sqr);
      T lambda2 = 0.5*(s-sqr);

      T eT1 = exp(-t*lambda1);
      T eT2 = exp(-t*lambda2);

#define l12 (lambda1-lambda2)
#define l21 (lambda2-lambda1);

      T c10 = (A1last*E2+Doserate+A2last*k21);
      T c11 = (c10-A1last*lambda1)/l21;
      T c12 = (c10-A1last*lambda2)/l21;
      A1 = c11*eT1 - c12*eT2 + Doserate*E2*(1/(lambda1*lambda2)+eT1/(lambda1*l12)-eT2/(lambda2*l12)) + b1;//Amount in the central compartment

      T c20 = (A2last*E1+A1last*k12);
      T c21 = (c20-A2last*lambda1)/l21;
      T c22 = (c20-A2last*lambda2)/l21;
      A2 = c21*eT1-c22*eT2 + Doserate*k12*(1/(lambda1*lambda2)+eT1/(lambda1*l12)-eT2/(lambda2*(lambda1-lambda2)));//Amount in the peripheral compartment
#undef s
#undef E1
#undef E2
#undef l12
#undef l21
      return A;
    }

    // undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
    // three compartment ka translations ncmt=1
#define tlag  pard(0,  0)
#define F     pard(1,  0)
#define rate1 pard(2,  0)
#define dur1  pard(3,  0)
#define ka    params(7, 0)
#define tlag2 pard(4, 0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtRateSSr1(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		     const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		     Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		     Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E1 = k10+k12+k13;
      T E2 = k21;
      T E3 = k31;

      //#calculate hybrid rate constants
      T a = E1+E2+E3;
      T b = E1*E2+E3*(E1+E2)-k12*k21-k13*k31;
      T c = E1*E2*E3-E3*k12*k21-E2*k13*k31;

      T a2 = a*a;
      T m = 0.333333333333333*(3.0*b - a2);
      T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
      T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T gamma = sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T theta3 = 0.333333333333333*theta;
      T ctheta3 = cos(theta3);
      T stheta3 = 1.7320508075688771932*sin(theta3);
      T gamma3 = pow(gamma,0.333333333333333);

      T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
      T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
      T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);
      T l123 = 1.0/(lambda1*lambda2*lambda3);
      A1=r1*E2*E3*l123;
      A2=r1*E3*k12*l123;
      A3=r1*E2*k13*l123;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtRateSS(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		   const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		   Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		   Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
		   double tinf, double tau){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E1 = k10+k12+k13;
      T E2 = k21;
      T E3 = k31;

      //#calculate hybrid rate constants
      T a = E1+E2+E3;
      T b = E1*E2+E3*(E1+E2)-k12*k21-k13*k31;
      T c = E1*E2*E3-E3*k12*k21-E2*k13*k31;

      T a2 = a*a;
      T m = 0.333333333333333*(3.0*b - a2);
      T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
      T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T gamma = sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T theta3 = 0.333333333333333*theta;
      T ctheta3 = cos(theta3);
      T stheta3 = 1.7320508075688771932*sin(theta3);
      T gamma3 = pow(gamma,0.333333333333333);

      T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
      T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
      T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);
      T eTi1 = exp(-tinf*lambda1);
      T eTi2 = exp(-tinf*lambda2);
      T eTi3 = exp(-tinf*lambda3);
      T eT1 = exp(-lambda1*(tau-tinf))/(1.0-exp(-tau*lambda1));
      T eT2 = exp(-lambda2*(tau-tinf))/(1.0-exp(-tau*lambda2));
      T eT3 = exp(-lambda3*(tau-tinf))/(1.0-exp(-tau*lambda3));
      A1=r1*(eT1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eT2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eT3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)))*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + eT2*(lambda2*(r1*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*k13*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))) - (r1*E2*k13*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*E3*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda2)*(lambda2 - lambda3)) + eT1*(-lambda1*(r1*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*k13*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))) + r1*E2*k13*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*E3*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)))/((lambda1 - lambda3)*(lambda1 - lambda2)) + eT3*(lambda3*(r1*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*k13*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))) - (r1*E2*k13*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*E3*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda3)*(-lambda2 + lambda3));
      A2=r1*k12*(eT1*(E3 - lambda1)*(E1 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eT2*(E1 - lambda2)*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eT3*(E1 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)))*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + eT2*(r1*k12*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda2 - (r1*E3*k12*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*k13*k12*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - r1*k13*k12*k31*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda2)*(lambda2 - lambda3)) + eT1*(r1*E3*k12*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - r1*k12*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda1 + r1*k13*k12*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - r1*k13*k12*k31*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)))/((lambda1 - lambda3)*(lambda1 - lambda2)) + eT3*(r1*k12*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda3 - (r1*E3*k12*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*k13*k12*k31*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - r1*k13*k12*k31*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda3)*(-lambda2 + lambda3));
      A3=r1*k13*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*(eT1*(E2 - lambda1)*(E1 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eT2*(E1 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eT3*(E1 - lambda3)*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3))) + eT2*(r1*k13*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda2 - (r1*E2*k13*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - r1*k13*k12*k21*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*k13*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda2)*(lambda2 - lambda3)) + eT1*(r1*E2*k13*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - r1*k13*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda1 - r1*k13*k12*k21*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*k13*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)))/((lambda1 - lambda3)*(lambda1 - lambda2)) + eT3*(r1*k13*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda3 - (r1*E2*k13*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - r1*k13*k12*k21*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + r1*k13*k12*k21*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda3)*(-lambda2 + lambda3));
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtRate(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
		 Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		 const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		 Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		 Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E1 = k10+k12+k13;
      T E2 = k21;
      T E3 = k31;

      //#calculate hybrid rate constants
      T a = E1+E2+E3;
      T b = E1*E2+E3*(E1+E2)-k12*k21-k13*k31;
      T c = E1*E2*E3-E3*k12*k21-E2*k13*k31;

      T a2 = a*a;
      T m = 0.333333333333333*(3.0*b - a2);
      T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
      T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T gamma = sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);
      T theta3 = 0.333333333333333*theta;
      T ctheta3 = cos(theta3);
      T stheta3 = 1.7320508075688771932*sin(theta3);
      T gamma3 = pow(gamma,0.333333333333333);

      T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
      T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
      T lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

      T B = A2last*k21+A3last*k31;
      T C = E3*A2last*k21+E2*A3last*k31;
      T I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31;
      T J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21;

      T eL1 = exp(-t*lambda1);
      T eL2 = exp(-t*lambda2);
      T eL3 = exp(-t*lambda3);

      T l12 = (lambda1-lambda2);
      T l13 = (lambda1-lambda3);
      T l21 = (lambda2-lambda1);
      T l23 = (lambda2-lambda3);
      T l31 = (lambda3-lambda1);
      T l32 = (lambda3-lambda2);

      T e1l1 = (E1-lambda1);
      T e1l2 = (E1-lambda2);
      T e1l3 = (E1-lambda3);
      T e2l1 = (E2-lambda1);
      T e2l2 = (E2-lambda2);
      T e2l3 = (E2-lambda3);
      T e3l1 = (E3-lambda1);
      T e3l2 = (E3-lambda2);
      T e3l3 = (E3-lambda3);

      T A1term1 = A1last*(eL1*e2l1*e3l1/(l21*l31)+eL2*e2l2*e3l2/(l12*l32)+eL3*e2l3*e3l3/(l13*l23));
      T A1term2 = eL1*(C-B*lambda1)/(l12*l13)+eL2*(B*lambda2-C)/(l12*l23)+eL3*(B*lambda3-C)/(l13*l32);
      T A1term3 = Doserate*((E2*E3)/(lambda1*lambda2*lambda3)-eL1*e2l1*e3l1/(lambda1*l21*l31)-eL2*e2l2*e3l2/(lambda2*l12*l32)-eL3*e2l3*e3l3/(lambda3*l13*l23));

      A1 = A1term1+A1term2+A1term3 + b1;//Amount in the central compartment

      T A2term1 = A2last*(eL1*e1l1*e3l1/(l21*l31)+eL2*e1l2*e3l2/(l12*l32)+eL3*e1l3*e3l3/(l13*l23));
      T A2term2 = eL1*(I-A1last*k12*lambda1)/(l12*l13)+eL2*(A1last*k12*lambda2-I)/(l12*l23)+eL3*(A1last*k12*lambda3-I)/(l13*l32);
      T A2term3 = Doserate*k12*(E3/(lambda1*lambda2*lambda3)-eL1*e3l1/(lambda1*l21*l31)-eL2*e3l2/(lambda2*l12*l32)-eL3*e3l3/(lambda3*l13*l23));

      A2 = A2term1+A2term2+A2term3;// Amount in the first-peripheral compartment

      T A3term1 = A3last*(eL1*e1l1*e2l1/(l21*l31)+eL2*e1l2*e2l2/(l12*l32)+eL3*e1l3*e2l3/(l13*l23));
      T A3term2 = eL1*(J-A1last*k13*lambda1)/(l12*l13)+eL2*(A1last*k13*lambda2-J)/(l12*l23)+eL3*(A1last*k13*lambda3-J)/(l13*l32);
      T A3term3 = Doserate*k13*(E2/(lambda1*lambda2*lambda3)-eL1*e2l1/(lambda1*l21*l31)-eL2*e2l2/(lambda2*l12*l32)-eL3*e2l3/(lambda3*l13*l23));

      A3 = A3term1+A3term2+A3term3;//Amount in the second-peripheral compartment
      return A;
    }

    // undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2

    // one compartment ka translations ncmt=1
#define tlag  pard(0, 0)
#define F     pard(1, 0)
#define rate1 pard(2, 0)
#define dur1  pard(3, 0)
#define ka    params(4, 0)
#define tlag2 pard(4, 0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtBolusSS(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		  const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		  Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		  Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		  double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(1, 1);
      T eT = 1.0/(1.0-exp(-k10*tau));
      A1 = b1*eT;
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    oneCmtBolus(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
		Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(1, 1);
      A1 = A1last*exp(-k10*t) + b1;
      return A;
    }

    // Undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
    // two compartment ka translations ncmt=1
#define tlag  pard(0,  0)
#define F     pard(1,  0)
#define rate1 pard(2,  0)
#define dur1  pard(3,  0)
#define ka    params(4,  0)
#define tlag2 pard(4,  0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtBolusSS(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		  const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		  Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		  Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		  double tau) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
      /* T E1 = k10+k12; */
      T E2 = k21;

      T s = k12+k21+k10;
      T sqr = sqrt(s*s-4*k21*k10);
      //calculate hybrid rate constants
      T lambda1 = 0.5*(s+sqr);
      T lambda2 = 0.5*(s-sqr);

      T eL1 = 1.0/(1.0-exp(-tau*lambda1));
      T eL2 = 1.0/(1.0-exp(-tau*lambda2));

      A1=(eL1*(b1*E2 - b1*lambda1) - eL2*(b1*E2 - b1*lambda2))/(-lambda1 + lambda2);
      A2=(eL1*b1*k12 - eL2*b1*k12)/(-lambda1 + lambda2);
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    twoCmtBolus(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
		Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus) {
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(2, 1);
#define E1 (k10+k12)
#define E2 k21
#define s  (k12+k21+k10)
      T sqr = sqrt(s*s-4*k21*k10);
      //calculate hybrid rate constants
      T lambda1 = 0.5*(s+sqr);
      T lambda2 = 0.5*(s-sqr);

      T eT1= exp(-t*lambda1);
      T eT2= exp(-t*lambda2);
      T c10 = (A1last*E2+A2last*k21);
      T c20 = (A2last*E1+A1last*k12);

      A1 = ((c10-A1last*lambda1)*eT1-(c10-A1last*lambda2)*eT2)/(lambda2-lambda1) + b1; //Amount in the central compartment
      A2 = ((c20-A2last*lambda1)*eT1-(c20-A2last*lambda2)*eT2)/(lambda2-lambda1);//            #Amount in the peripheral compartment
#undef s
#undef E1
#undef E2
      return A;
    }

    // undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
    // three compartment ka translations ncmt=1
#define tlag  pard(0,  0)
#define F     pard(1,  0)
#define rate1 pard(2,  0)
#define dur1  pard(3,  0)
#define ka    params(6, 0)
#define tlag2 pard(4, 0)
#define f2    pard(5, 0)
#define rate2 pard(6, 0)
#define dur2  pard(7, 0)

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtBolusSS(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		    const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		    Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		    Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
		    double tau){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E1 = k10+k12+k13;
      T E2 = k21;
      T E3 = k31;

      //calculate hybrid rate constants
      T a = E1+E2+E3;
      T b = E1*E2+E3*(E1+E2)-k12*k21-k13*k31;
      T c = E1*E2*E3-E3*k12*k21-E2*k13*k31;

      T a2 = a*a;
      T m = 0.333333333333333*(3.0*b - a2);
      T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
      T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T gamma = sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);

      T theta3 = 0.333333333333333*theta;
      T ctheta3 = cos(theta3);
      T stheta3 = 1.7320508075688771932*sin(theta3);
      T gamma3 = pow(gamma,0.333333333333333);
      T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
      T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
      T lambda3 = 0.333333333333333*a -(2*gamma3*ctheta3);

      T eL1 = 1.0/(1.0-exp(-tau*lambda1));
      T eL2 = 1.0/(1.0-exp(-tau*lambda2));
      T eL3 = 1.0/(1.0-exp(-tau*lambda3));

      A1=b1*(eL1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eL2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eL3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)));
      A2=eL2*(-b1*E3*k12 + b1*k12*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*(b1*E3*k12 - b1*k12*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-b1*E3*k12 + b1*k12*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
      A3=eL2*(-b1*E2*k13 + b1*k13*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*(b1*E2*k13 - b1*k13*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-b1*E2*k13 + b1*k13*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    threeCmtBolus(double t, Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
		  Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
		  const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		  Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
		  Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus){
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(3, 1);
      T E1 = k10+k12+k13;
      T E2 = k21;
      T E3 = k31;

      //calculate hybrid rate constants
      T a = E1+E2+E3;
      T b = E1*E2+E3*(E1+E2)-k12*k21-k13*k31;
      T c = E1*E2*E3-E3*k12*k21-E2*k13*k31;

      T a2 = a*a;
      T m = 0.333333333333333*(3.0*b - a2);
      T n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
      T Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

      T alpha = sqrt(-Q);
      T beta = -0.5*n;
      T gamma = sqrt(beta*beta+alpha*alpha);
      T theta = atan2(alpha,beta);

      T theta3 = 0.333333333333333*theta;
      T ctheta3 = cos(theta3);
      T stheta3 = 1.7320508075688771932*sin(theta3);
      T gamma3 = pow(gamma,0.333333333333333);
      T lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
      T lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
      T lambda3 = 0.333333333333333*a -(2*gamma3*ctheta3);

      T B = A2last*k21+A3last*k31;
      T C = E3*A2last*k21+E2*A3last*k31;
      T I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31;
      T J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21;

      T eL1 = exp(-t*lambda1);
      T eL2 = exp(-t*lambda2);
      T eL3 = exp(-t*lambda3);

      T l12 = (lambda1-lambda2);
      T l13 = (lambda1-lambda3);
      T l21 = (lambda2-lambda1);
      T l23 = (lambda2-lambda3);
      T l31 = (lambda3-lambda1);
      T l32 = (lambda3-lambda2);

      T e1l1 = (E1-lambda1);
      T e1l2 = (E1-lambda2);
      T e1l3 = (E1-lambda3);
      T e2l1 = (E2-lambda1);
      T e2l2 = (E2-lambda2);
      T e2l3 = (E2-lambda3);
      T e3l1 = (E3-lambda1);
      T e3l2 = (E3-lambda2);
      T e3l3 = (E3-lambda3);

      T A1term1 = A1last*(eL1*e2l1*e3l1/(l21*l31)+eL2*e2l2*e3l2/(l12*l32)+eL3*e2l3*e3l3/(l13*l23));
      T A1term2 = eL1*(C-B*lambda1)/(l12*l13)+eL2*(B*lambda2-C)/(l12*l23)+eL3*(B*lambda3-C)/(l13*l32);

      A1 = b1+(A1term1+A1term2);

      T A2term1 = A2last*(eL1*e1l1*e3l1/(l21*l31)+eL2*e1l2*e3l2/(l12*l32)+eL3*e1l3*e3l3/(l13*l23));
      T A2term2 = eL1*(I-A1last*k12*lambda1)/(l12*l13)+eL2*(A1last*k12*lambda2-I)/(l12*l23)+eL3*(A1last*k12*lambda3-I)/(l13*l32);

      A2 = A2term1+A2term2;

      T A3term1 = A3last*(eL1*e1l1*e2l1/(l21*l31)+eL2*e1l2*e2l2/(l12*l32)+eL3*e1l3*e2l3/(l13*l23));
      T A3term2 = eL1*(J-A1last*k13*lambda1)/(l12*l13)+eL2*(A1last*k13*lambda2-J)/(l12*l23)+eL3*(A1last*k13*lambda3-J)/(l13*l32);
      A3 = A3term1+A3term2;

      return A;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    ssRateTau(int ncmt, int oral0,
	      Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	      const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	      Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	      Eigen::Matrix<double, Eigen::Dynamic, 1>& rate,
	      double tinf, double tau) {
      if (oral0){
	if (r1 > 0 ){
	  switch (ncmt){
	  case 1: {
	    return oneCmtKaRateSStr1(params, pard, g,rate, tinf, tau);
	  } break;
	  case 2: {
	    return twoCmtKaRateSStr1(params, pard, g,rate, tinf, tau);
	  } break;
	  case 3: {
	    return threeCmtKaRateSStr1(params, pard, g,rate, tinf, tau);
	  } break;
	  }
	} else {
	  switch (ncmt){
	  case 1: {
	    return oneCmtKaRateSStr2(params, pard, g,rate, tinf, tau);
	  } break;
	  case 2: {
	    return twoCmtKaRateSStr2(params, pard, g,rate, tinf, tau);
	  } break;
	  case 3: {
	    return threeCmtKaRateSStr2(params, pard, g,rate, tinf, tau);
	  } break;
	  }
	}
      } else {
	switch (ncmt){
	case 1: {
	  return oneCmtRateSS(params, pard, g, rate, tinf, tau);
	} break;
	case 2: {
	  return twoCmtRateSS(params, pard, g, rate, tinf, tau);
	} break;
	case 3: {
	  return threeCmtRateSS(params, pard, g, rate, tinf, tau);
	} break;
	}
      }
      Rcpp::stop("bad ssRateTau; ncmt: %d  oral0: %d\n",
		 ncmt, oral0);
      return params;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    ssTau(int ncmt, int oral0,
	  Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	  const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	  Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	  Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
	  double tau){
      if (oral0){
	if (b1 > 0 ){
	  switch (ncmt){
	  case 1: {
	    return oneCmtKaSSb1(params, pard, g, bolus, tau);
	  } break;
	  case 2: {
	    return twoCmtKaSSb1(params, pard, g, bolus, tau);
	  } break;
	  case 3: {
	    return threeCmtKaSSb1(params, pard, g, bolus, tau);
	  } break;
	  }
	} else {
	  switch (ncmt){
	  case 1: {
	    return oneCmtKaSSb2(params, pard, g, bolus, tau);
	  } break;
	  case 2: {
	    return twoCmtKaSSb2(params, pard, g, bolus, tau);
	  } break;
	  case 3: {
	    return threeCmtKaSSb2(params, pard, g, bolus, tau);
	  } break;
	  }
	}
      } else {
	switch (ncmt){
	case 1: {
	  return oneCmtBolusSS(params, pard, g, bolus, tau);
	} break;
	case 2: {
	  return twoCmtBolusSS(params, pard, g, bolus, tau);
	} break;
	case 3: {
	  return threeCmtBolusSS(params, pard, g, bolus, tau);
	} break;
	}
      }
      Rcpp::stop("shouldn't get here");
      return params;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    ssRate(int ncmt, int oral0,
	   Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	   const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	   Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	   Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      if (oral0){
	if (r1 > 0){
	  switch (ncmt){
	  case 1: {
	    return oneCmtKaRateSSr1(params, pard, g, rate);
	  } break;
	  case 2: {
	    return twoCmtKaRateSSr1(params, pard, g, rate);
	  } break;
	  case 3: {
	    return threeCmtKaRateSSr1(params, pard, g, rate);
	  } break;
	  }
	} else {
	  switch (ncmt){
	  case 1: {
	    return oneCmtKaRateSSr2(params, pard, g, rate);
	  } break;
	  case 2: {
	    return twoCmtKaRateSSr2(params, pard, g, rate);
	  } break;
	  case 3: {
	    return threeCmtKaRateSSr2(params, pard, g, rate);
	  } break;
	  }
	}
      } else {
	switch (ncmt){
	case 1: {
	  return oneCmtRateSSr1(params, pard, g, rate);
	} break;
	case 2: {
	  return twoCmtRateSSr1(params, pard, g, rate);
	} break;
	case 3: {
	  return threeCmtRateSSr1(params, pard, g, rate);
	} break;
	}
      }
      Rcpp::stop("problem");
      return params;
    }

    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, 1>
    doAdvan(int ncmt, int oral0,
	    double tlast, double ct,
	    Eigen::Matrix<T, Eigen::Dynamic, 1>& Alast,
	    Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
	    const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
	    Eigen::Matrix<T, Eigen::Dynamic, 2>& g,
	    Eigen::Matrix<double, Eigen::Dynamic, 1>& bolus,
	    Eigen::Matrix<double, Eigen::Dynamic, 1>& rate) {
      double t = ct - tlast;
      if (r1 > DBL_EPSILON  || (oral0 && r2 > DBL_EPSILON)){
	if (oral0){
	  switch (ncmt){
	  case 1: {
	    return oneCmtKaRate(t, Alast, params, pard, g, bolus, rate);
	  } break;
	  case 2: {
	    return twoCmtKaRate(t, Alast, params, pard, g, bolus, rate);
	  } break;
	  case 3: {
	    return threeCmtKaRate(t, Alast, params, pard, g, bolus, rate);
	  } break;
	  }
	} else {
	  switch (ncmt){
	  case 1: {
	    return oneCmtRate(t, Alast, params, pard, g, bolus, rate);
	  } break;
	  case 2: {
	    return twoCmtRate(t, Alast, params, pard, g, bolus, rate);
	  } break;
	  case 3: {
	    return threeCmtRate(t, Alast, params, pard, g, bolus, rate);
	  } break;
	  }
	}
      } else {
	// Bolus doses only
	if (oral0){
	  switch (ncmt){
	  case 1: {
	    return oneCmtKa(t, Alast, params, pard, g, bolus);
	  } break;
	  case 2: {
	    return twoCmtKa(t, Alast, params, pard, g, bolus);
	  } break;
	  case 3: {
	    return threeCmtKa(t, Alast, params, pard, g, bolus);
	  } break;
	  }
	} else {
	  // Bolus
	  switch (ncmt){
	  case 1: {
	    return oneCmtBolus(t, Alast, params, pard, g, bolus);
	  } break;
	  case 2: {
	    return twoCmtBolus(t, Alast, params, pard, g, bolus);
	  } break;
	  case 3: {
	    return threeCmtBolus(t, Alast, params, pard, g, bolus);
	  } break;
	  }
	}
      }
      Rcpp::stop("doAdvan error; ncmt: %d, oral0: %d", ncmt, oral0);
      return params;
    }

#undef v
#undef k20
#undef kel
#undef k23
#undef k32
#undef k24
#undef k42
#undef k10
#undef k12
#undef k21
#undef k13
#undef k31
#undef A1
#undef A2
#undef A3
#undef A3
#undef r1
#undef Doserate
#undef r2
#undef b1
#undef b2
      // undefine extras
#undef tlag
#undef F
#undef rate1
#undef dur1
#undef ka
#undef tlag2
#undef f2
#undef dur2
#define d_tlag  pard(0, 0)
#define d_F     pard(1, 0)
#define d_rate1 pard(2, 0)
#define d_dur1  pard(3, 0)
#define d_ka    params(ncmt*2, 0)
#define d_tlag2 pard(4, 0)
#define d_F2    pard(5, 0)
#define d_rate2 pard(6, 0)
#define d_dur2  pard(7, 0)
#define v       g(0, 0)
    template <class T>
    Eigen::Matrix<T, Eigen::Dynamic, Eigen::Dynamic>
    genericCmtInterface(Eigen::Matrix<T, Eigen::Dynamic, 1>& params,
			const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
			const double t,
			const int oral0,
			const int trans,
			const int ncmt,
			const int linCmt,
			const int idx,
			const int sameTime,
			rx_solving_options_ind *ind,
			rx_solve *rx,
			const Eigen::Matrix<double, -1, -1>& AlastA,
			const Eigen::Matrix<double, -1, -1>& AlastG){
      rx_solving_options *op = rx->op;
      Eigen::Matrix<T, Eigen::Dynamic, 2> g(ncmt, 3);
      g = micros2macros(params, ncmt, trans);
      Eigen::Matrix<double, Eigen::Dynamic, 1> rate(oral0+1, 1);
      Eigen::Matrix<double, Eigen::Dynamic, 1> bolus(oral0+1, 1);
      double *rateD = ind->linCmtRate;
      for (int i = oral0+1; i--; ){
	bolus(i, 0) = 0.0;
	rate(i, 0) = rateD[i];
      }
      Eigen::Matrix<T, Eigen::Dynamic, 1> Alast(oral0+ncmt, 1);
      Eigen::Matrix<T, Eigen::Dynamic, 1> A(oral0+ncmt, 1);
      for (int i = oral0+ncmt; i--;){
	Alast(i, 0) = AlastA(i, 0) + params(0, 0)*AlastG(i, 0) + params(1, 0)*AlastG(i, 1);
	if (ncmt >= 2) {
	  Alast(i, 0) += params(2, 0)*AlastG(i, 2) + params(3, 0)*AlastG(i, 3);
	  if (ncmt == 3) {
	    Alast(i, 0) += params(4, 0)*AlastG(i, 4) + params(5, 0)*AlastG(i, 5);
	  }
	}
	if (oral0) {
	  Alast(i, 0) += params(2*ncmt, 0)*AlastG(i,2*ncmt);
	}
	A(i, 0) = 0.0;
      }
      double tlast;
      double curTime=0.0;
      double r0;
      double amt;
      double rateAdjust = 0.0;
      double tinf;
      int evid, wh, cmt, wh100, whI, wh0, cmtOff;
      curTime = getTime_(ind->ix[idx], ind);
      if (idx == 0) {
	tlast = curTime;
      } else {
	tlast = getTime_(ind->ix[idx-1], ind);
      }
      int extraAdvan = 1, doRate=0, doMultiply = 0, doReplace=0,
	doInf=0;
	evid = ind->evid[ind->ix[idx]];
	if (isObs(evid)){
	} else if (evid == 3){
	  // Reset event
	  for (int i = oral0+ncmt; i--;){
	    Alast(i, 0) = 0.0;
	  }
	} else {
	  getWh(evid, &wh, &cmt, &wh100, &whI, &wh0);
	  if (!oral0 && cmt > linCmt) {
	    int foundBad = 0;
	    for (int j = 0; j < ind->nBadDose; j++){
	      if (ind->BadDose[j] == cmt+1){
		foundBad=1;
		break;
	      }
	    }
	    if (!foundBad){
	      ind->BadDose[ind->nBadDose]=cmt+1;
	      ind->nBadDose++;
	    }
	  } else if (oral0 && cmt > linCmt+1) {
	    int foundBad = 0;
	    for (int j = 0; j < ind->nBadDose; j++){
	      if (ind->BadDose[j] == cmt+1){
		foundBad=1;
		break;
	      }
	    }
	    if (!foundBad){
	      ind->BadDose[ind->nBadDose]=cmt+1;
	      ind->nBadDose++;
	    }
	  }
	  cmtOff = cmt-linCmt;
	  if ((oral0 && cmtOff > 1) ||
	      (!oral0 && cmtOff != 0)) {
	  } else {
	    syncIdx(ind);
	    amt = getDoseNumber(ind, ind->ixds);
	    if (!ISNA(getDoseNumber(ind, ind->ixds)) && (amt > 0) && (wh0 == 10 || wh0 == 20)) {
	      // dosing to cmt
	      // Steady state doses; wh0 == 20 is equivalent to SS=2 in NONMEM
	      double tau = getIiNumber(ind, ind->ixds);
	      Eigen::Matrix<T, Eigen::Dynamic, 1> aSave(oral0+ncmt, 1);
	      if (wh0 == 20) {
		aSave = doAdvan(ncmt, oral0, tlast, curTime,
				Alast, params, pard, g, bolus, rate);
	      } else {
		for (int i = oral0+ncmt; i--;){
		  aSave(i, 0) = 0;
		}
	      }
	      // Reset rate
	      rate(0, 0) = 0;
	      if (oral0) {
		rate(1, 0) = 0;
	      }
	      for (int i = oral0+ncmt; i--;){
		Alast(i, 0) = 0.0;
	      }
	      doInf=0;
	      switch (whI){
	      case 0: {
		// Get bolus dose
		if (cmtOff == 0){
		  bolus(0,0) = amt*d_F;
		} else {
		  bolus(1, 0) = amt*d_F2;
		}
		A = ssTau(ncmt, oral0, params, pard, g, bolus, tau);
	      } break;
	      case 8: // Duration is modeled
	      case 9: { // Rate is modeled
		amt = -getDoseNumber(ind, ind->ixds+1);
		if (whI == 9) {
		  // cmtOff = 0
		  if (cmtOff == 0)  {
		    // Infusion to depot compartment with oral dosing or
		    // infusion to central compartment
		    r0 = d_rate1;
		    tinf = amt/r0*d_F;
		  } else {
		    // cmtOff = 1
		    // Infusion to central compartment
		    r0 = d_rate2;
		    tinf = amt/r0*d_F2;
		  }
		} else {
		  // duration is modeled
		  if (cmtOff == 0) {
		    // With oral dosing infusion to central compartment
		    tinf = d_dur1;
		    r0 = amt/tinf*d_F;
		  } else {
		    // Infusion to compartment #1 or depot
		    tinf = d_dur2;
		    r0 = amt/tinf*d_F2;
		  }
		}
		doInf=1;
	      } break;
	      case 1:
	      case 2: {
		if (ISNA(getDoseNumber(ind, ind->ixds))){
		} else if (amt > 0) {
		  doInf=1;
		  unsigned int p;
		  r0 = amt;
		  tinf = _getDur(ind->ixds, ind, 1, &p);
		  if (whI == 1){
		    // Duration changes
		    if (cmtOff == 0){
		      tinf *= d_F;
		    } else {
		      tinf *= d_F2;
		    }
		  } else {
		    // Rate changes
		    if (cmtOff == 0){
		      r0 *= d_F;
		    } else {
		      r0 *= d_F2;
		    }
		  }
		}
	      }
	      }
	      if (doInf){
		// Infusion steady state
		if (tinf >= tau){
		  ind->wrongSSDur=1;
		  double tmp = R_NaN;
		  for (int i = oral0+ncmt; i--;){
		    A(i, 0) = tmp;
		  }
		  extraAdvan=0;
		} else {
		  if (cmtOff == 0){
		    rate(0, 0) = r0;
		    if (oral0) rate(1, 0) = 0;
		  } else {
		    rate(0, 0) = 0; rate(1, 0) = r0;
		  }
		  A = ssRateTau(ncmt, oral0,params, pard, g, rate, tinf, tau);
		  rate(0,0) = 0;
		  if (oral0){
		    rate(1,0) = 0;
		  }
		}
	      }
	      if (wh0 == 20) {
		A = A + aSave;
	      }
	      extraAdvan=0;
	    } else if (wh0 == 30){
	      // Turning off a compartment; Not supported put everything to NaN
	      double tmp = R_NaN;
	      for (int i = oral0+ncmt; i--;){
		A(i, 0) = tmp;
	      }
	      extraAdvan=0;
	    }
	    // dosing to cmt
	    // use handle_evid here
	    amt = getDoseNumber(ind, ind->ixds);
	    switch (whI){
	    case 0: { // Bolus dose
	      // base dose
	      if (cmtOff == 0){
		bolus(0, 0) = amt*d_F;
	      } else {
		bolus(1, 0) = amt*d_F2;
	      }
	    } break;
	    case 4: {
	      doReplace = cmtOff+1;
	    } break;
	    case 5: {
	      doMultiply= cmtOff+1;
	    } break;
	    case 9:
	    case 8: { // modeled duration.
	      //InfusionRate[cmt] -= dose[ind->ixds+1];
	      rateAdjust = -getDoseNumber(ind, ind->ixds+1);
	      doRate = cmtOff+1;
	    } break;
	    case 6: // end modeled duration
	    case 7:{ // End modeled rate
	      // Infusion to central compartment with oral dosing
	      rateAdjust = amt;
	      doRate = cmtOff+1;
	    } break;
	    case 1: { // Begin infusion
	      rateAdjust = amt; // Amt is negative when turning off
	      doRate = cmtOff+1;
	    } break;

	    case 2: {
	      // In this case bio-availability changes the rate, but the duration remains constant.
	      // rate = amt/dur
	      if (cmtOff == 0){
		rateAdjust = amt*d_F;
	      } else {
		rateAdjust = amt*d_F2;
	      }
	      doRate = cmtOff+1;
	    } break;
	    }
	    if (wh0 == 40){
	      // Steady state infusion
	      // Now advance to steady state dosing
	      // These are easy to solve
	      for (int i = oral0+1; i--;){
		rate(i, 0) = 0.0;
	      }
	      if (doRate == 1){
		rate(0, 0) += rateAdjust;
	      } else {
		rate(1, 0) += rateAdjust;
	      }
	      doRate=0;
	      A = ssRate(ncmt, oral0, params, pard, g, rate);
	      extraAdvan=0;
	      rate(0, 0) = 0;
	      if (oral0){
		rate(1, 0) = 0;
	      }
	    }
	  }
	}
	//
	if (extraAdvan){
	  A = doAdvan(ncmt, oral0, tlast, curTime, Alast,
		      params, pard, g, bolus, rate);
	}
	if (doReplace){
	  A(doReplace-1, 0) = amt;
	} else if (doMultiply){
	  A(doMultiply-1, 0) *= amt;
	} else if (doRate){
	  rate(doRate-1, 0) += rateAdjust;
	}
	rateAdjust=0;
	doRate = doMultiply=doReplace=doInf=0;
	extraAdvan=1;
	for (int i = oral0+1; i--;){
	  bolus(i, 0)= 0.0;
	}
	Alast = A;
 	tlast = curTime;
      // Save A and rate
      double *Ad = getAdvan(idx);
      for (int i = ncmt+oral0; i--;){
	Ad[i] = A(i, 0).val();
      }
      if (!sameTime){
      	// Compute the advan solution of a t outside of the mesh.
      	Alast = A;
      	tlast = curTime;
      	curTime = t;
      	A = doAdvan(ncmt, oral0, tlast, curTime, Alast,
      		    params, pard, g, bolus, rate);
      }
      // Alast(oral0+ncmt, 1)
      Eigen::Matrix<T, Eigen::Dynamic, 1> ret(oral0+ncmt+1,1);
      ret(0, 0) = A(oral0, 0)/v;
      for (int i = ncmt+oral0; i--;){
	ret(1+i, 0) = A(i, 0);
      }
      for (int i = oral0+1; i--; ){
	rateD[i] = rate(i, 0);
      }
      return ret;
    }
    struct linCmtFun {
      const double t_;
      const int ncmt_, linCmt_, oral0_, trans_, idx_, sameTime_;
      rx_solving_options_ind *ind_;
      rx_solve *rx_;
      const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard_;
      const Eigen::Matrix<double, -1, -1>& AlastA_;
      const Eigen::Matrix<double, -1, -1>& AlastG_;
      linCmtFun(const double t,
		const int ncmt,
		const int oral0,
		const int trans,
		const int linCmt,
		const int idx,
		const int sameTime,
		rx_solving_options_ind *ind,
		rx_solve *rx,
		const Eigen::Matrix<double, Eigen::Dynamic, 1>& pard,
		const Eigen::Matrix<double, -1, -1>& AlastA,
		const Eigen::Matrix<double, -1, -1>& AlastG) :
	t_(t),
	ncmt_(ncmt),
	linCmt_(linCmt),
	oral0_(oral0),
	trans_(trans),
	idx_(idx),
	sameTime_(sameTime),
	ind_(ind),
	rx_(rx),
	pard_(pard),
	AlastA_(AlastA),
	AlastG_(AlastG)
      { }
      template <typename T>
      Eigen::Matrix<T, Eigen::Dynamic, 1> operator()(Eigen::Matrix<T, Eigen::Dynamic, 1>& params) const {
	return genericCmtInterface(params, pard_, t_, oral0_, trans_, ncmt_, linCmt_, idx_, sameTime_, ind_, rx_,
				   AlastA_, AlastG_);
      }
    };
    }
}

#undef d_tlag
#undef d_F
#undef d_rate1
#undef d_dur1
#undef d_ka
#undef d_tlag2
#undef d_F2
#undef d_rate2
#undef d_dur2
#undef v

#undef p5

typedef Eigen::Matrix<double, Eigen::Dynamic, 1> MatrixPd;

extern "C" double linCmtC(rx_solve *rx, unsigned int id, double _t, int linCmt,
			  int i_cmt, int trans,
			  double p1, double v1,
			  double p2, double p3,
			  double p4, double p5,
			  double d_tlag, double d_F, double d_rate1, double d_dur1,
			  // Oral parameters
			  double d_ka, double d_tlag2, double d_F2,  double d_rate2, double d_dur2);

double updateDiff(rx_solve *rx, unsigned int id, double t, int value, int linCmt,
		  int ncmt, int trans,
		  double p1, double v1,
		  double p2, double p3,
		  double p4, double p5,
		  double d_tlag, double d_F, double d_rate1, double d_dur1,
		  // Oral parameters
		  double d_ka, double d_tlag2, double d_F2,  double d_rate2, double d_dur2,
		  int central, double h, double &v0) {
  double p00[15] = {p1, v1, p2, p3, p4, p5, d_tlag, d_F,  d_rate1, d_dur1, d_ka, d_tlag2, d_F2, d_rate2, d_dur2};
  if (central) {
    double p01[15] = {p1, v1, p2, p3, p4, p5, d_tlag, d_F,  d_rate1, d_dur1, d_ka, d_tlag2, d_F2, d_rate2, d_dur2};
    p00[value-1] += 0.5*h;
    p01[value-1] -= 0.5*h;
    return (linCmtC(rx, id, t, linCmt, ncmt, trans, p00[0], p00[1],
		    p00[2], p00[3], p00[4], p00[5], p00[6], p00[7], p00[8], p00[9],
		    p00[10], p00[11], p00[12],  p00[13], p00[14]) -
	    linCmtC(rx, id, t, linCmt, ncmt, trans, p00[0], p00[1],
		    p01[2], p01[3], p01[4], p01[5], p01[6], p01[7], p01[8], p01[9],
		    p01[10], p01[11], p01[12],  p01[13], p01[14]))/h;
  } else {
    p00[value-1] +=h;
    return (linCmtC(rx, id, t, linCmt, ncmt, trans,p00[0], p00[1],
		    p00[2], p00[3], p00[4], p00[5], p00[6], p00[7], p00[8], p00[9],
		    p00[10], p00[11], p00[12],  p00[13], p00[14]) - v0)/h;
  }
}

static inline double linCmtBg(double *A, double &t, int& val, int& trans, int& ncmt,
			      int& oral0, double& dd_v1, double& dd_p3,
			      double& dd_p5, bool interpolate, int id){
  if (val == 0){
    if (trans == 10) {
      return(A[oral0]*(dd_v1 + dd_p3 + dd_p5));
    } else {
      return(A[oral0]/dd_v1);
    }
  } else if (val == 11) {
      if (oral0) return A[ncmt + oral0 + 2*ncmt];
      return 0.0;
  } else if (val <= 7) {
    return A[ncmt+oral0+val-1];
  } else {
    rx_solve* rx = getRxSolve_();
    rx_solving_options *op = rx->op;
    int cur = op->nlin2;
    rx_solving_options_ind *ind = &(rx->subjects[id]);
    if (op->linBflag & 64) { // tlag
      if (val == 7) {
	if (interpolate) {
	  double tlast  = getTime_(ind->ix[ind->idx-1], ind);
	  double *Alast = getAdvan(ind->idx-1);
	  double tcur  = getTime_(ind->ix[ind->idx], ind);
	  return (A[cur] - Alast[cur])/(tcur-tlast)*(t-tlast)+Alast[cur];
	} else {
	  return A[cur];
	}
      }
      cur++;
    }
    if (op->linBflag & 128) { // f 8
      if (interpolate && ind->idx > 0) {
	double tlast  = getTime_(ind->ix[ind->idx-1], ind);
	double *Alast = getAdvan(ind->idx-1);
	double tcur  = getTime_(ind->ix[ind->idx], ind);
	double diff = (A[cur] - Alast[cur]);
	return diff/(tcur-tlast)*(t-tlast)+Alast[cur];
	return A[cur];
      } else {
	if (val == 8) {
	  return A[cur];
	}
      }
      cur++;
    }
    if (op->linBflag & 256) { // rate 9
      if (val == 9) {
	if (interpolate) {
	  double tlast  = getTime_(ind->ix[ind->idx-1], ind);
	  double *Alast = getAdvan(ind->idx-1);
	  double tcur  = getTime_(ind->ix[ind->idx], ind);
	  return (A[cur] - Alast[cur])/(tcur-tlast)*(t-tlast)+Alast[cur];
	} else {
	  return A[cur];
	}
      }
      cur++;
    }
    if (op->linBflag & 512) { // dur 10
      if (val == 10) {
	if (interpolate) {
	  double tlast  = getTime_(ind->ix[ind->idx-1], ind);
	  double *Alast = getAdvan(ind->idx-1);
	  double tcur  = getTime_(ind->ix[ind->idx], ind);
	  return (A[cur] - Alast[cur])/(tcur-tlast)*(t-tlast)+Alast[cur];
	} else {
	  return A[cur];
	}
      }
      cur++;
    }
    if (op->linBflag & 2048) { // tlag2 12
      if (val == 12) {
	if (interpolate) {
	  double tlast  = getTime_(ind->ix[ind->idx-1], ind);
	  double *Alast = getAdvan(ind->idx-1);
	  double tcur  = getTime_(ind->ix[ind->idx], ind);
	  return (A[cur] - Alast[cur])/(tcur-tlast)*(t-tlast)+Alast[cur];
	} else {
	  return A[cur];
	}
      }
      cur++;
    }
    if (op->linBflag & 4096) { // f2 13
      if (val == 13) {
	if (interpolate) {
	  double tlast  = getTime_(ind->ix[ind->idx-1], ind);
	  double *Alast = getAdvan(ind->idx-1);
	  double tcur  = getTime_(ind->ix[ind->idx], ind);
	  return (A[cur] - Alast[cur])/(tcur-tlast)*(t-tlast)+Alast[cur];
	} else {
	  return A[cur];
	}
      }
      cur++;
    }
    if (op->linBflag & 8192) { // rate2 14
      if (val == 14) {
	if (interpolate) {
	  double tlast  = getTime_(ind->ix[ind->idx-1], ind);
	  double *Alast = getAdvan(ind->idx-1);
	  double tcur  = getTime_(ind->ix[ind->idx], ind);
	  return (A[cur] - Alast[cur])/(tcur-tlast)*(t-tlast)+Alast[cur];
	} else {
	  return A[cur];
	}
      }
      cur++;
    }
    if (op->linBflag & 16384) { // dur2 15
      if (val == 15) {
	if (interpolate) {
	  double tlast  = getTime_(ind->ix[ind->idx-1], ind);
	  double *Alast = getAdvan(ind->idx-1);
	  double tcur  = getTime_(ind->ix[ind->idx], ind);
	  return (A[cur] - Alast[cur])/(tcur-tlast)*(t-tlast)+Alast[cur];
	} else {
	  return A[cur];
	}
      }
      cur++;
    }
  }
  return NA_REAL;
}


extern "C" double linCmtD(rx_solve *rx, unsigned int id,
			  double t, int linCmt,
			  int ncmt, int trans, int val,
			  double dd_p1, double dd_v1,
			  double dd_p2, double dd_p3,
			  double dd_p4, double dd_p5,
			  double dd_tlag, double dd_F,
			  double dd_rate, double dd_dur,
			  // oral extra parameters
			  double dd_ka, double dd_tlag2,
			  double dd_F2, double dd_rate2, double dd_dur2);
extern "C" double linCmtE(rx_solve *rx, unsigned int id,
			  double t, int linCmt,
			  int ncmt, int trans, int val,
			  double dd_p1, double dd_v1,
			  double dd_p2, double dd_p3,
			  double dd_p4, double dd_p5,
			  double dd_tlag, double dd_F,
			  double dd_rate, double dd_dur,
			  // oral extra parameters
			  double dd_ka, double dd_tlag2,
			  double dd_F2, double dd_rate2, double dd_dur2);
extern "C" double linCmtF(rx_solve *rx, unsigned int id,
			  double t, int linCmt,
			  int ncmt, int trans, int val,
			  double dd_p1, double dd_v1,
			  double dd_p2, double dd_p3,
			  double dd_p4, double dd_p5,
			  double dd_tlag, double dd_F,
			  double dd_rate, double dd_dur,
			  // oral extra parameters
			  double dd_ka, double dd_tlag2,
			  double dd_F2, double dd_rate2, double dd_dur2);
extern "C" double linCmtB(rx_solve *rx, unsigned int id,
			  double _t, int linCmt,
			  int ncmt, int trans, int val,
			  double dd_p1, double dd_v1,
			  double dd_p2, double dd_p3,
			  double dd_p4, double dd_p5,
			  double dd_tlag, double dd_F,
			  double dd_rate, double dd_dur,
			  // oral extra parameters
			  double dd_ka, double dd_tlag2,
			  double dd_F2, double dd_rate2, double dd_dur2){
  switch (rx->sensType){
  case 1: {// sensitivity
    // Get  Alast
    stanad:
    rx_solving_options_ind *ind = &(rx->subjects[id]);
    double t = _t - ind->curShift;
    int idx = ind->idx;
    rx_solving_options *op = rx->op;
    int oral0 = (dd_ka != 0) ? 1 : 0;
    double it = getTime_(ind->ix[idx], ind);

    if (t != it) {
      // Try to get another idx by bisection
      idx = _locateTimeIndex(t, ind);
      it = getTime_(ind->ix[idx], ind);
    }
    int sameTime = fabs(t-it) < sqrt(DBL_EPSILON);
    if (idx <= ind->solved && sameTime){
      // Pull from last solved value (cached)
      double *A = getAdvan(idx);
      return linCmtBg(A, t, val, trans, ncmt, oral0, dd_v1, dd_p3, dd_p5, true, id);
    }
    MatrixPd params(2*ncmt + oral0, 1);
    params(0, 0) = dd_p1;
    params(1, 0) = dd_v1;
    if (ncmt >=2 ){
      params(2,0) = dd_p2;
      params(3,0) = dd_p3;
      if (ncmt >= 3){
	params(4,0) = dd_p4;
	params(5,0) = dd_p5;
      }
    }
    if (oral0) {
      params(2*ncmt, 0) = dd_ka;
    }

    MatrixPd pard(4 + 4*oral0, 1);

    pard(0, 0) = dd_tlag;
    pard(1, 0) = dd_F;
    pard(2, 0) = dd_rate;
    pard(3, 0) = dd_dur;
    if (oral0) {
      pard(4, 0) = dd_tlag2;
      pard(5, 0) = dd_F2;
      pard(6, 0) = dd_rate2;
      pard(7, 0) = dd_dur2;
    }
    // Restore last values from gradient into a matrix
    Eigen::Matrix<double, -1, -1> AlastG(ncmt+oral0, ncmt*2+oral0);
    Eigen::Matrix<double, -1, -1> AlastA(ncmt+oral0,1);
    double *A = NULL;
    if (idx != 0){
      A = getAdvan(idx-1);
      for (int i = 0; i < ncmt+oral0; i++) {
	AlastG(i, 0) = A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 0];
	AlastG(i, 1) = A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 1];
	// Alast Adjusted
	AlastA(i, 0) = A[i];
	AlastA(i, 0) -= AlastG(i, 0)*dd_p1;
	AlastA(i, 0) -= AlastG(i, 1)*dd_v1;
	if (ncmt >=2){
	  AlastG(i, 2) = A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 2];
	  AlastG(i, 3) = A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 3];
	  // Adjust alast
	  AlastA(i, 0) -= AlastG(i, 2)*dd_p2;
	  AlastA(i, 0) -= AlastG(i, 3)*dd_p3;
	  if (ncmt >= 3){
	    AlastG(i, 4) = A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 4];
	    AlastG(i, 5) = A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 5];
	    // Adjust Alast
	    AlastA(i, 0) -= AlastG(i, 4)*dd_p4;
	    AlastA(i, 0) -= AlastG(i, 5)*dd_p5;
	  }
	}
	if (oral0) {
	  AlastG(i, 2*ncmt) = A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 2*ncmt];
	  AlastA(i, 0) -= AlastG(i, 2*ncmt)*dd_ka;
	}
      }
      // Rcpp::print(Rcpp::wrap(AlastG));
    } else {
      AlastG.setZero(ncmt+oral0, ncmt*2+oral0);
      AlastA.setZero(ncmt+oral0, 1);
    }
    stan::math::linCmtFun f(t, ncmt, oral0, trans, linCmt, idx, sameTime, ind, rx, pard, AlastA, AlastG);
    Eigen::VectorXd fx;
    Eigen::Matrix<double, -1, -1> J;
    stan::math::jacobian(f, params, fx, J);
    if (sameTime) {
      // Rcpp::print(Rcpp::wrap(J));
      A = getAdvan(idx);
      A[ncmt + oral0 + 0] = J(0, 0);
      A[ncmt + oral0 + 1] = J(0, 1);
      if (ncmt >=2){
	A[ncmt + oral0 + 2] = J(0, 2);
	A[ncmt + oral0 + 3] = J(0, 3);
	if (ncmt == 3){
	  A[ncmt + oral0 + 4] = J(0, 4);
	  A[ncmt + oral0 + 5] = J(0, 5);
	}
      }
      if (oral0) {
	A[ncmt + oral0 + 2*ncmt] = J(0, 2*ncmt);
      }
      // Save A1-A4
      for (int i = 0; i < ncmt+oral0; i++) {
	//(3*ncmt+2*oral0)+0
	A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 0] = J(i+1, 0);
	A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 1] = J(i+1, 1);
	if (ncmt >=2){
	  A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 2] = J(i+1, 2);
	  A[ncmt + oral0 + (2*ncmt+oral0)*(i+1) + 3] = J(i+1, 3);
	  if (ncmt == 3){
	    A[ncmt + oral0 + (2*ncmt+oral0)*(i+1)+ 4] = J(i+1, 4);
	    A[ncmt + oral0 + (2*ncmt+oral0)*(i+1)+ 5] = J(i+1, 5);
	  }
	}
	if (oral0) {
	  //(3*ncmt+oral0)+2*ncmt
	  A[ncmt + oral0 + (2*ncmt+oral0)*(i+1)+ 2*ncmt] = J(i+1, 2*ncmt);
	}
      }
      // (4+(ncmt+oral0))*ncmt + (2+ncmt+oral0)*oral0+1
      int cur = op->nlin2;
      double v0=NA_REAL;
      if (trans == 10) {
	v0 =  A[oral0]*(dd_v1 + dd_p3 + dd_p5);
      } else {
	v0 = A[oral0]/dd_v1;
      }
      if (op->linBflag & 64) { // tlag
	A[cur++] = updateDiff(rx, id, _t, 7, linCmt, ncmt, trans,
			      dd_p1, dd_v1, dd_p2, dd_p3, dd_p4, dd_p5,
			      dd_tlag, dd_F, dd_rate, dd_dur, 
			      dd_ka, dd_tlag2, dd_F2,  dd_rate2, dd_dur2,
			      op->cTlag, op->hTlag, v0);
      }
      if (op->linBflag & 128) { // f 8
	A[cur++] = updateDiff(rx, id, _t, 8, linCmt, ncmt, trans,
			      dd_p1, dd_v1, dd_p2, dd_p3, dd_p4, dd_p5,
			      dd_tlag, dd_F, dd_rate, dd_dur, 
			      dd_ka, dd_tlag2, dd_F2,  dd_rate2, dd_dur2,
			      op->cF, op->hF, v0);
      }
      if (op->linBflag & 256) { // rate 9
	A[cur++] = updateDiff(rx, id, _t, 9, linCmt, ncmt, trans,
			      dd_p1, dd_v1, dd_p2, dd_p3, dd_p4, dd_p5,
			      dd_tlag, dd_F, dd_rate, dd_dur, 
			      dd_ka, dd_tlag2, dd_F2,  dd_rate2, dd_dur2,
			      op->cRate, op->hRate, v0);
      }
      if (op->linBflag & 512) { // dur 10
	A[cur++] = updateDiff(rx, id, _t, 10, linCmt, ncmt, trans,
			      dd_p1, dd_v1, dd_p2, dd_p3, dd_p4, dd_p5,
			      dd_tlag, dd_F, dd_rate, dd_dur, 
			      dd_ka, dd_tlag2, dd_F2,  dd_rate2, dd_dur2,
			      op->cDur, op->hDur, v0);
      }
      if (op->linBflag & 2048) { // tlag2 12
	A[cur++] = updateDiff(rx, id, _t, 12, linCmt, ncmt, trans,
			      dd_p1, dd_v1, dd_p2, dd_p3, dd_p4, dd_p5,
			      dd_tlag, dd_F, dd_rate, dd_dur, 
			      dd_ka, dd_tlag2, dd_F2,  dd_rate2, dd_dur2,
			      op->cTlag2, op->hTlag2, v0);
      }
      if (op->linBflag & 4096) { // f2 13
	A[cur++] = updateDiff(rx, id, _t, 13, linCmt, ncmt, trans,
			      dd_p1, dd_v1, dd_p2, dd_p3, dd_p4, dd_p5,
			      dd_tlag, dd_F, dd_rate, dd_dur, 
			      dd_ka, dd_tlag2, dd_F2,  dd_rate2, dd_dur2,
			      op->cF2, op->hF2, v0);
      }
      if (op->linBflag & 8192) { // rate2 14
	A[cur++] = updateDiff(rx, id, _t, 14, linCmt, ncmt, trans,
			      dd_p1, dd_v1, dd_p2, dd_p3, dd_p4, dd_p5,
			      dd_tlag, dd_F, dd_rate, dd_dur, 
			      dd_ka, dd_tlag2, dd_F2,  dd_rate2, dd_dur2,
			      op->cRate2, op->hRate2, v0);
      }
      if (op->linBflag & 16384) { // dur2 15
	A[cur++] = updateDiff(rx, id, _t, 15, linCmt, ncmt, trans,
			      dd_p1, dd_v1, dd_p2, dd_p3, dd_p4, dd_p5,
			      dd_tlag, dd_F, dd_rate, dd_dur, 
			      dd_ka, dd_tlag2, dd_F2,  dd_rate2, dd_dur2,
			      op->cDur2, op->hDur2, v0);
      }
      ind->solved = idx;
    }
    return linCmtBg(A, t, val, trans, ncmt, oral0, dd_v1, dd_p3, dd_p5, false, id);
  }
    break;
  case 2: // forward difference
    return linCmtD(rx, id, _t, linCmt, ncmt, trans, val,
		    dd_p1, dd_v1, dd_p2, dd_p3,
		    dd_p4, dd_p5, dd_tlag, dd_F,
		    dd_rate, dd_dur, dd_ka, dd_tlag2, dd_F2,
		    dd_rate2, dd_dur2);
    break;
  case 3: //central difference
    return linCmtE(rx, id, _t, linCmt, ncmt, trans, val,
		   dd_p1, dd_v1, dd_p2, dd_p3,
		   dd_p4, dd_p5, dd_tlag, dd_F,
		   dd_rate, dd_dur, dd_ka, dd_tlag2, dd_F2,
		   dd_rate2, dd_dur2);
  case 4: // symbolic advan
    if (ncmt == 2 || ncmt == 3) goto stanad;
    return linCmtF(rx, id, _t, linCmt, ncmt, trans, val,
		   dd_p1, dd_v1, dd_p2, dd_p3,
		   dd_p4, dd_p5, dd_tlag, dd_F,
		   dd_rate, dd_dur, dd_ka, dd_tlag2, dd_F2,
		   dd_rate2, dd_dur2);
  default:
    Rf_errorcall(R_NilValue, "unsupported sensitivity");
  }
}
