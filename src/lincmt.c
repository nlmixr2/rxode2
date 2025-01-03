#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <stdio.h>
#include <stdarg.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/Rdynload.h>

void _rxode2parse_unprotect(void);

#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parse.h"

extern rx_solve rx_global;
extern rx_solving_options op_global;
extern t_F AMT;
extern t_LAG LAG;
extern t_RATE RATE;
extern t_DUR DUR;
extern t_calc_mtime calc_mtime;

extern t_ME ME;
extern t_IndF IndF;
extern void RSprintf(const char *format, ...);

extern int _locateTimeIndex(double obs_time,  rx_solving_options_ind *ind);

#include "../inst/include/rxode2parse.h"
#include "../inst/include/rxode2parseHandleEvid.h"
#include "../inst/include/rxode2parseGetTime.h"


#define safe_zero(a) ((a) == 0 ? DBL_EPSILON : (a))
#define _as_zero(a) (fabs(a) < sqrt(DBL_EPSILON) ? 0.0 : a)
#define _as_dbleps(a) (fabs(a) < sqrt(DBL_EPSILON) ? ((a) < 0 ? -sqrt(DBL_EPSILON)  : sqrt(DBL_EPSILON)) : a)

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2parse", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#include "lincmtB1.h"
////////////////////////////////////////////////////////////////////////////////
// 1-3 oral absorption with rates
// From Richard Upton with rxode2 Expression optimization (and some manual edits)
////////////////////////////////////////////////////////////////////////////////

#define A1 A[0]
#define A2 A[1]
#define A3 A[2]
#define A4 A[3]
#define A1last Alast[0]
#define A2last Alast[1]
#define A3last Alast[2]
#define A4last Alast[3]

static inline void oneCmtKaRateSSr1(double *A, double *r1,
                                    double *ka, double *k20) {
  A1 = (*r1)/(*ka);
  A2 = (*r1)/(*k20);
}

static inline void oneCmtKaRateSSr2(double *A, double *r2,
                                    double *ka, double *k20) {
  A1 = 0;
  A2 = (*r2)/(*k20);
}

static inline void oneCmtKaRateSStr1(double *A,
                                     double *tinf, double *tau, double *r1,
                                     double *ka, double *k20) {
  double eKa = exp(-(*ka)*((*tau)-(*tinf)))/(1.0-exp(-(*tau)*(*ka)));
  double eiKa = exp(-(*ka)*(*tinf));
  double eiK = exp(-(*k20)*(*tinf));
  double eK = exp(-(*k20)*((*tau)-(*tinf)))/(1.0-exp(-(*tau)*(*k20)));
  A1=eKa*((*r1)/(*ka) - eiKa*(*r1)/(*ka));
  A2=eK*((*r1)/(*k20) + eiKa*(*r1)/(-(*k20) + (*ka)) - eiK*(*r1)*(*ka)/((*ka)*(*k20) - (*k20)*(*k20))) + (*ka)*(eK - eKa)*((*r1)/(*ka) - eiKa*(*r1)/(*ka))/(-(*k20) + (*ka));
}

static inline void oneCmtKaRateSStr2(double *A,
                                     double *tinf, double *tau, double *r2,
                                     double *ka, double *k20){
  double eiK = exp(-(*k20)*(*tinf));
  double eK = exp(-(*k20)*((*tau)-(*tinf)))/(1.0-exp(-(*k20)*(*tau)));
  A1=0.0;
  A2=eK*((*r2)/(*k20) - eiK*(*r2)*(-(*k20) + (*ka))/((*ka)*(*k20) - (*k20)*(*k20)));
}

static inline void oneCmtKaRate(double *A, double *Alast, double *t,
                                double *b1, double *b2,
                                double *r1, double *r2,
                                double *ka, double *k20) {
  double eKa = exp(-(*ka)*(*t));
  double e20 = exp(-(*k20)*(*t));
  A1 = (*r1)/(*ka)-(((*r1)-A1last*(*ka))*eKa)/(*ka);
  A2 = (((*r1)-A1last*(*ka))*eKa)/((*ka)-(*k20)) - ((((*ka)-(*k20))*(*r2)+(*ka)*(*r1)+(-A2last-A1last)*(*k20)*(*ka)+A2last*(*k20)*(*k20))*e20)/((*k20)*(*ka)-(*k20)*(*k20))+ ((*r2)+(*r1))/(*k20);
}

/*
  Two compartment with rates in each
*/

static inline void twoCmtKaRateSSr1(double *A, double *r1,
                                    double *ka,  double *k20,
                                    double *k23, double *k32) {
  double s = (*k23)+(*k32)+(*k20);
  double beta  = 0.5*(s - sqrt(s*s - 4*(*k32)*(*k20)));
  double alpha = (*k32)*(*k20)/beta;
  A1=(*r1)/(*ka);
  A2=(*r1)*(*k32)/(beta*alpha);
  A3=(*r1)*(*k23)/(beta*alpha);
}

static inline void twoCmtKaRateSSr2(double *A, double *r2,
                                    double *ka,  double *k20,
                                    double *k23, double *k32) {
  double s = (*k23)+(*k32)+(*k20);
  double beta  = 0.5*(s - sqrt(s*s - 4*(*k32)*(*k20)));
  double alpha = (*k32)*(*k20)/beta;
  A1=0;
  A2=(*r2)*(*k32)/(beta*alpha);
  A3=(*r2)*(*k23)/(beta*alpha);
}

static inline void twoCmtKaRateSStr1(double *A, double *tinf, double *tau, double *r1,
                                     double *ka,  double *k20,
                                     double *k23, double *k32){
  double s = (*k23)+(*k32)+(*k20);
  double beta  = 0.5*(s - sqrt(s*s - 4*(*k32)*(*k20)));
  double alpha = (*k32)*(*k20)/beta;

  double eA = exp(-alpha*((*tau)-(*tinf)))/(1.0-exp(-alpha*(*tau)));
  double eB = exp(-beta*((*tau)-(*tinf)))/(1.0-exp(-beta*(*tau)));

  double eiA = exp(-alpha*(*tinf));
  double eiB = exp(-beta*(*tinf));

  double alpha2 = alpha*alpha;
  double alpha3 = alpha2*alpha;

  double beta2 = beta*beta;
  double beta3 = beta2*beta;

  double ka2 = (*ka)*(*ka);

  double eKa = exp(-(*ka)*((*tau)-(*tinf)))/(1.0-exp(-(*ka)*(*tau)));
  double eiKa = exp(-(*ka)*(*tinf));

  A1=eKa*((*r1)/(*ka) - eiKa*(*r1)/(*ka));
  A2=(eA*(-alpha*((*r1)*(*k32)/(beta*alpha) + eiKa*(*r1)*(-(*k32) + (*ka))/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(-alpha + (*k32))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(-beta + (*k32))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k32)*((*r1)*(*k23)/(beta*alpha) - eiKa*(*r1)*(*k23)/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(*k23)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(*k23)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k32)*((*r1)*(*k32)/(beta*alpha) + eiKa*(*r1)*(-(*k32) + (*ka))/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(-alpha + (*k32))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(-beta + (*k32))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3))) - eB*(-beta*((*r1)*(*k32)/(beta*alpha) + eiKa*(*r1)*(-(*k32) + (*ka))/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(-alpha + (*k32))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(-beta + (*k32))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k32)*((*r1)*(*k23)/(beta*alpha) - eiKa*(*r1)*(*k23)/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(*k23)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(*k23)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k32)*((*r1)*(*k32)/(beta*alpha) + eiKa*(*r1)*(-(*k32) + (*ka))/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(-alpha + (*k32))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(-beta + (*k32))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta) + (*ka)*(eA*(-alpha + (*k32))/((-alpha + beta)*(-alpha + (*ka))) + eB*(-beta + (*k32))/((-beta + (*ka))*(alpha - beta)) + eKa*((*k32) - (*ka))/((beta - (*ka))*(alpha - (*ka))))*((*r1)/(*ka) - eiKa*(*r1)/(*ka));
  A3=(eA*(-alpha*((*r1)*(*k23)/(beta*alpha) - eiKa*(*r1)*(*k23)/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(*k23)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(*k23)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k23)*((*r1)*(*k32)/(beta*alpha) + eiKa*(*r1)*(-(*k32) + (*ka))/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(-alpha + (*k32))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(-beta + (*k32))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + ((*k20) + (*k23))*((*r1)*(*k23)/(beta*alpha) - eiKa*(*r1)*(*k23)/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(*k23)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(*k23)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3))) - eB*(-beta*((*r1)*(*k23)/(beta*alpha) - eiKa*(*r1)*(*k23)/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(*k23)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(*k23)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k23)*((*r1)*(*k32)/(beta*alpha) + eiKa*(*r1)*(-(*k32) + (*ka))/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(-alpha + (*k32))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(-beta + (*k32))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + ((*k20) + (*k23))*((*r1)*(*k23)/(beta*alpha) - eiKa*(*r1)*(*k23)/(beta*alpha + (*ka)*(-alpha - beta) + ka2) - eiA*(*r1)*(*ka)*(*k23)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r1)*(*ka)*(*k23)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta) + (*ka)*(*k23)*(eA/((-alpha + beta)*(-alpha + (*ka))) + eB/((-beta + (*ka))*(alpha - beta)) + eKa/((beta - (*ka))*(alpha - (*ka))))*((*r1)/(*ka) - eiKa*(*r1)/(*ka));
}

static inline void twoCmtKaRateSStr2(double *A, double *tinf, double *tau, double *r2,
                                     double *ka,  double *k20,
                                     double *k23, double *k32) {
  double E2 = (*k20)+(*k23);
  double E3 = (*k32);
  double s = (*k23)+(*k32)+(*k20);
  double beta  = 0.5*(s - sqrt(s*s - 4*(*k32)*(*k20)));
  double alpha = (*k32)*(*k20)/beta;

  double eA = exp(-alpha*((*tau)-(*tinf)))/(1.0-exp(-alpha*(*tau)));
  double eB = exp(-beta*((*tau)-(*tinf)))/(1.0-exp(-beta*(*tau)));

  double eiA = exp(-alpha*(*tinf));
  double eiB = exp(-beta*(*tinf));

  double alpha2 = alpha*alpha;
  double alpha3 = alpha2*alpha;

  double beta2 = beta*beta;
  double beta3 = beta2*beta;
  A1=0.0;
  A2=(eA*(E3*((*r2)*(*k32)/(beta*alpha) - eiA*(*r2)*(-(*k32)*alpha + (*ka)*(-alpha + (*k32)) + alpha2)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k32)*beta + (*ka)*(-beta + (*k32)) + beta2)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) - alpha*((*r2)*(*k32)/(beta*alpha) - eiA*(*r2)*(-(*k32)*alpha + (*ka)*(-alpha + (*k32)) + alpha2)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k32)*beta + (*ka)*(-beta + (*k32)) + beta2)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k32)*((*r2)*(*k23)/(beta*alpha) - eiA*(*r2)*(-(*k23)*alpha + (*ka)*(*k23))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k23)*beta + (*ka)*(*k23))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3))) - eB*(E3*((*r2)*(*k32)/(beta*alpha) - eiA*(*r2)*(-(*k32)*alpha + (*ka)*(-alpha + (*k32)) + alpha2)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k32)*beta + (*ka)*(-beta + (*k32)) + beta2)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) - beta*((*r2)*(*k32)/(beta*alpha) - eiA*(*r2)*(-(*k32)*alpha + (*ka)*(-alpha + (*k32)) + alpha2)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k32)*beta + (*ka)*(-beta + (*k32)) + beta2)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k32)*((*r2)*(*k23)/(beta*alpha) - eiA*(*r2)*(-(*k23)*alpha + (*ka)*(*k23))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k23)*beta + (*ka)*(*k23))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta);
  A3=(eA*(E2*((*r2)*(*k23)/(beta*alpha) - eiA*(*r2)*(-(*k23)*alpha + (*ka)*(*k23))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k23)*beta + (*ka)*(*k23))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) - alpha*((*r2)*(*k23)/(beta*alpha) - eiA*(*r2)*(-(*k23)*alpha + (*ka)*(*k23))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k23)*beta + (*ka)*(*k23))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k23)*((*r2)*(*k32)/(beta*alpha) - eiA*(*r2)*(-(*k32)*alpha + (*ka)*(-alpha + (*k32)) + alpha2)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k32)*beta + (*ka)*(-beta + (*k32)) + beta2)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3))) - eB*(E2*((*r2)*(*k23)/(beta*alpha) - eiA*(*r2)*(-(*k23)*alpha + (*ka)*(*k23))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k23)*beta + (*ka)*(*k23))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) - beta*((*r2)*(*k23)/(beta*alpha) - eiA*(*r2)*(-(*k23)*alpha + (*ka)*(*k23))/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k23)*beta + (*ka)*(*k23))/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3)) + (*k23)*((*r2)*(*k32)/(beta*alpha) - eiA*(*r2)*(-(*k32)*alpha + (*ka)*(-alpha + (*k32)) + alpha2)/(-beta*alpha2 + (*ka)*(beta*alpha - alpha2) + alpha3) + eiB*(*r2)*(-(*k32)*beta + (*ka)*(-beta + (*k32)) + beta2)/(beta2*alpha + (*ka)*(-beta*alpha + beta2) - beta3))))/(-alpha + beta);
}

static inline void twoCmtKaRate(double *A, double *Alast,
                                double *t, double *b1, double *b2,
                                double *r1, double *r2,
                                double *ka,  double *k20,
                                double *k23, double *k32) {
  double E2 =  (*k20)+ (*k23);
  double s = (*k23)+(*k32)+(*k20);
  double beta  = 0.5*(s - sqrt(s*s - 4*(*k32)*(*k20)));
  double alpha = (*k32)*(*k20)/beta;

  double eKa = exp(-(*ka)*(*t));
  double eA = exp(-alpha*(*t));
  double eB = exp(-beta*(*t));

  double ka2 = (*ka)*(*ka);

  double alpha2 = alpha*alpha;
  double alpha3 = alpha2*alpha;

  double beta2 = beta*beta;
  double beta3 = beta2*beta;

  A1 = (*r1)/(*ka)-(((*r1)-A1last*(*ka))*eKa)/(*ka);
  A2 = ((((*ka)-(*k32))*(*r1)-A1last*ka2+A1last*(*k32)*(*ka))*eKa)/(ka2+(-beta-alpha)*(*ka)+alpha*beta)+(((((*k32)-beta)*(*ka)-beta*(*k32)+beta2)*(*r2)+((*k32)-beta)*(*ka)*(*r1)+((-A3last-A2last-A1last)*beta*(*k32)+(A2last+A1last)*beta2)*(*ka)+(A3last+A2last)*beta2*(*k32)-A2last*beta3)*eB)/((beta2-alpha*beta)*(*ka)-beta3+alpha*beta2)-(((((*k32)-alpha)*(*ka)-alpha*(*k32)+alpha2)*(*r2)+((*k32)-alpha)*(*ka)*(*r1)+((-A3last-A2last-A1last)*alpha*(*k32)+(A2last+A1last)*alpha2)*(*ka)+(A3last+A2last)*alpha2*(*k32)-A2last*alpha3)*eA)/((alpha*beta-alpha2)*(*ka)-alpha2*beta+alpha3)+((*k32)*(*r2)+(*k32)*(*r1))/(alpha*beta);
  A3 = -(((*k23)*(*r1)-A1last*(*k23)*(*ka))*eKa)/(ka2+(-beta-alpha)*(*ka)+alpha*beta)+((((*k23)*(*ka)-beta*(*k23))*(*r2)+(*k23)*(*ka)*(*r1)+((-A2last-A1last)*beta*(*k23)+A3last*beta2-A3last*E2*beta)*(*ka)+A2last*beta2*(*k23)-A3last*beta3+A3last*E2*beta2)*eB)/((beta2-alpha*beta)*(*ka)-beta3+alpha*beta2)-((((*k23)*(*ka)-alpha*(*k23))*(*r2)+(*k23)*(*ka)*(*r1)+((-A2last-A1last)*alpha*(*k23)+A3last*alpha2-A3last*E2*alpha)*(*ka)+A2last*alpha2*(*k23)-A3last*alpha3+A3last*E2*alpha2)*eA)/((alpha*beta-alpha2)*(*ka)-alpha2*beta+alpha3)+((*k23)*(*r2)+(*k23)*(*r1))/(alpha*beta);
}

static inline void threeCmtKaRateSSr1(double *A, double *r1,
                                      double *ka, double *k20,
                                      double *k23, double *k32,
                                      double *k24, double *k42){
  double j = (*k23)+(*k20)+(*k32)+(*k42)+(*k24);
  double k = (*k23)*(*k42)+(*k20)*(*k32)+(*k20)*(*k42)+(*k32)*(*k42)+(*k24)*(*k32);
  double l = (*k20)*(*k32)*(*k42);

  double m = 0.3333333333333333*(3.0*k- j*j);
  double n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
  double Q = 0.25*n*n + 0.03703703703703703*m*m*m;

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double rho=sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double ct3 = cos(0.3333333333333333*theta);
  double rho3 = R_pow(rho,0.3333333333333333);
  double st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
  double j3 = 0.3333333333333333*j;
  double lam1 = j3  + rho3*(ct3 + st3);
  double lam2 = j3 + rho3*(ct3 - st3);
  double lam3 = j3 -(2.0*rho3*ct3);
  double l123 = 1.0/(lam1*lam2*lam3);
  A1=(*r1)/(*ka);
  A2=(*r1)*(*k42)*(*k32)*l123;
  A3=(*r1)*(*k42)*(*k23)*l123;
  A4=(*r1)*(*k24)*(*k32)*l123;
}

static inline void threeCmtKaRateSSr2(double *A, double *r2,
                                      double *ka, double *k20,
                                      double *k23, double *k32,
                                      double *k24, double *k42){
  double j = (*k23)+(*k20)+(*k32)+(*k42)+(*k24);
  double k = (*k23)*(*k42)+(*k20)*(*k32)+(*k20)*(*k42)+(*k32)*(*k42)+(*k24)*(*k32);
  double l = (*k20)*(*k32)*(*k42);

  double m = 0.3333333333333333*(3.0*k - j*j);
  double n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
  double Q = 0.25*n*n + 0.03703703703703703*m*m*m;

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double rho=sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double ct3 = cos(0.3333333333333333*theta);
  double rho3 = R_pow(rho,0.3333333333333333);
  double st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
  double j3 = 0.3333333333333333*j;
  double lam1 = j3  + rho3*(ct3 + st3);
  double lam2 = j3 + rho3*(ct3 - st3);
  double lam3 = j3 -(2.0*rho3*ct3);
  double l123 = 1.0/(lam1*lam2*lam3);
  A1=0;
  A2=(*r2)*(*k42)*(*k32)*l123;
  A3=(*r2)*(*k42)*(*k23)*l123;
  A4=(*r2)*(*k24)*(*k32)*l123;
}

static inline void threeCmtKaRateSStr1(double *A, double *tinf, double *tau, double *r1,
                                       double *ka, double *k20,
                                       double *k23, double *k32,
                                       double *k24, double *k42){
  double E2 =  (*k20)+ (*k23) + (*k24);
  double E3 = (*k32);
  double E4 = (*k42);
  double j = (*k23)+(*k20)+(*k32)+(*k42)+(*k24);
  double k = (*k23)*(*k42)+(*k20)*(*k32)+(*k20)*(*k42)+(*k32)*(*k42)+(*k24)*(*k32);
  double l = (*k20)*(*k32)*(*k42);

  double m = 0.3333333333333333*(3.0*k - j*j);
  double n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
  double Q = 0.25*n*n + 0.03703703703703703*m*m*m;

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double rho=sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double ct3 = cos(0.3333333333333333*theta);
  double rho3 = R_pow(rho,0.3333333333333333);
  double st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
  double j3 = 0.3333333333333333*j;
  double lam1 = j3  + rho3*(ct3 + st3);
  double lam2 = j3 + rho3*(ct3 - st3);
  double lam3 = j3 -(2.0*rho3*ct3);

  double eKa = exp(-(*ka)*((*tau)-(*tinf)))/(1.0-exp(-(*ka)*(*tau)));
  double eiKa = exp(-(*ka)*(*tinf));

  double eL1 = exp(-lam1*((*tau)-(*tinf)))/(1.0-exp(-lam1*(*tau)));
  double eiL1 = exp(-lam1*(*tinf));

  double eL2 = exp(-lam2*((*tau)-(*tinf)))/(1.0-exp(-lam2*(*tau)));
  double eiL2 = exp(-lam2*(*tinf));

  double eL3 = exp(-lam3*((*tau)-(*tinf)))/(1.0-exp(-lam3*(*tau)));
  double eiL3 = exp(-lam3*(*tinf));

  double ka2 = (*ka)*(*ka);
  double ka3 = ka2*(*ka);

  double lam12 = lam1*lam1;
  double lam13 = lam12*lam1;
  double lam14 = lam13*lam1;

  double lam22 = lam2*lam2;
  double lam23 = lam22*lam2;
  double lam24 = lam23*lam2;

  double lam32 = lam3*lam3;
  double lam33 = lam32*lam3;
  double lam34 = lam33*lam3;
  A1=eKa*((*r1)/(*ka) - eiKa*(*r1)/(*ka));
  A2=(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) + (*ka)*(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((*ka) - lam1)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((*ka) - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((*ka) - lam3)) + eKa*(E3 - (*ka))*(E4 - (*ka))/((-(*ka) + lam1)*(-(*ka) + lam2)*(-(*ka) + lam3)))*((*r1)/(*ka) - eiKa*(*r1)/(*ka)) + eL1*(-lam1*((*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3)) + (*k42)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3))) + E3*(*k42)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) + E4*(*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3)) + (*k42)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3))) - (E3*(*k42)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) + E4*(*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3)) + (*k42)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3))) - (E3*(*k42)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) + E4*(*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
  A3=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3)) + (*ka)*(*k23)*(eL1*(E4 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((*ka) - lam1)) + eL2*(E4 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((*ka) - lam2)) + eL3*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((*ka) - lam3)) + eKa*(E4 - (*ka))/((-(*ka) + lam1)*(-(*ka) + lam2)*(-(*ka) + lam3)))*((*r1)/(*ka) - eiKa*(*r1)/(*ka)) + eL1*(E4*(*k23)*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (*k23)*lam1*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) + (*k42)*(*k23)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) - (*k42)*(*k24)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((*k23)*lam3*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (E4*(*k23)*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) + (*k42)*(*k23)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) - (*k42)*(*k24)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((*k23)*lam2*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (E4*(*k23)*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) + (*k42)*(*k23)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) - (*k42)*(*k24)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
  A4=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) + (*ka)*(*k24)*(eL1*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)*((*ka) - lam1)) + eL2*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)*((*ka) - lam2)) + eL3*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)*((*ka) - lam3)) + eKa*(E3 - (*ka))/((-(*ka) + lam1)*(-(*ka) + lam2)*(-(*ka) + lam3)))*((*r1)/(*ka) - eiKa*(*r1)/(*ka)) + eL1*(E3*(*k24)*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (*k23)*(*k32)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) + (*k24)*(*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3)) - (*k24)*lam1*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((*k24)*lam3*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (E3*(*k24)*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (*k23)*(*k32)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) + (*k24)*(*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((*k24)*lam2*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (E3*(*k24)*(-eiKa*(*r1)*((*k42)*(*k32) + (-(*k32) - (*k42))*(*ka) + ka2)/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) - eiL1*(*r1)*(-(*ka)*lam12 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r1)*(-(*ka)*lam22 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r1)*(-(*ka)*lam32 - (*ka)*(*k42)*(*k32) + ((*k32) + (*k42))*(*ka)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (*k23)*(*k32)*(eiKa*(*r1)*(-(*k24)*(*k32) + (*ka)*(*k24))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam1)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam2)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*((*ka)*(*k24)*(*k32) - (*ka)*(*k24)*lam3)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k24)*(*k32)/(lam2*lam1*lam3)) + (*k24)*(*k32)*(eiKa*(*r1)*(-(*k42)*(*k23) + (*ka)*(*k23))/(ka2*lam1 + lam2*(-(*ka)*lam1 + ka2) + lam3*(-(*ka)*lam1 + lam2*(-(*ka) + lam1) + ka2) - ka3) + eiL1*(*r1)*(-(*ka)*(*k23)*lam1 + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r1)*(-(*ka)*(*k23)*lam2 + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r1)*(-(*ka)*(*k23)*lam3 + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r1)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
}

static inline void threeCmtKaRateSStr2(double *A, double *tinf, double *tau, double *r2,
                                       double *ka, double *k20,
                                       double *k23, double *k32,
                                       double *k24, double *k42) {
  double E2 =  (*k20)+ (*k23) + (*k24);
  double E3 = (*k32);
  double E4 = (*k42);
  double j = (*k23)+(*k20)+(*k32)+(*k42)+(*k24);
  double k = (*k23)*(*k42)+(*k20)*(*k32)+(*k20)*(*k42)+(*k32)*(*k42)+(*k24)*(*k32);
  double l = (*k20)*(*k32)*(*k42);

  double m = 0.3333333333333333*(3.0*k - j*j);
  double n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
  double Q = 0.25*n*n + 0.03703703703703703*m*m*m;

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double rho=sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double ct3 = cos(0.3333333333333333*theta);
  double rho3 = R_pow(rho,0.3333333333333333);
  double st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
  double j3 = 0.3333333333333333*j;
  double lam1 = j3  + rho3*(ct3 + st3);
  double lam2 = j3 + rho3*(ct3 - st3);
  double lam3 = j3 -(2.0*rho3*ct3);

  double eL1 = exp(-lam1*((*tau)-(*tinf)))/(1.0-exp(-lam1*(*tau)));
  double eiL1 = exp(-lam1*(*tinf));

  double eL2 = exp(-lam2*((*tau)-(*tinf)))/(1.0-exp(-lam2*(*tau)));
  double eiL2 = exp(-lam2*(*tinf));

  double eL3 = exp(-lam3*((*tau)-(*tinf)))/(1.0-exp(-lam3*(*tau)));
  double eiL3 = exp(-lam3*(*tinf));

  double lam12 = lam1*lam1;
  double lam13 = lam12*lam1;
  double lam14 = lam13*lam1;

  double lam22 = lam2*lam2;
  double lam23 = lam22*lam2;
  double lam24 = lam23*lam2;

  double lam32 = lam3*lam3;
  double lam33 = lam32*lam3;
  double lam34 = lam33*lam3;
  A1=0.0;
  A2=(eL1*(E4 - lam1)*(E3 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E3 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) + eL1*(-lam1*((*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3)) + (*k42)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3))) + E3*(*k42)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) + E4*(*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*(lam3*((*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3)) + (*k42)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3))) - (E3*(*k42)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) + E4*(*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*(lam2*((*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3)) + (*k42)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3))) - (E3*(*k42)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) + E4*(*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
  A3=(eL1*(E4 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E4 - lam2)*(E2 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E4 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3)) + eL1*(E4*(*k23)*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (*k23)*lam1*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) + (*k42)*(*k23)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) - (*k42)*(*k24)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((*k23)*lam3*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (E4*(*k23)*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) + (*k42)*(*k23)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) - (*k42)*(*k24)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((*k23)*lam2*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (E4*(*k23)*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) + (*k42)*(*k23)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) - (*k42)*(*k24)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
  A4=(eL1*(E3 - lam1)*(E2 - lam1)/((-lam1 + lam3)*(-lam1 + lam2)) + eL2*(E2 - lam2)*(E3 - lam2)/((-lam2 + lam3)*(lam1 - lam2)) + eL3*(E2 - lam3)*(E3 - lam3)/((lam2 - lam3)*(lam1 - lam3)))*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) + eL1*(E3*(*k24)*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (*k23)*(*k32)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) + (*k24)*(*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3)) - (*k24)*lam1*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)))/((lam1 - lam3)*(lam1 - lam2)) + eL3*((*k24)*lam3*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (E3*(*k24)*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (*k23)*(*k32)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) + (*k24)*(*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((-lam2 + lam3)*(lam1 - lam3)) + eL2*((*k24)*lam2*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (E3*(*k24)*(-eiL1*(*r2)*(lam1*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam12 - (*ka)*(*k42)*(*k32) + lam13)/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) + eiL2*(*r2)*(lam2*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam22 - (*ka)*(*k42)*(*k32) + lam23)/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) - eiL3*(*r2)*(lam3*((*k42)*(*k32) + ((*k32) + (*k42))*(*ka)) + (-(*k32) - (*k42) - (*ka))*lam32 - (*ka)*(*k42)*(*k32) + lam33)/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k32)/(lam2*lam1*lam3)) - (*k23)*(*k32)*(eiL1*(*r2)*((*k24)*lam12 + lam1*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k24)*lam22 + lam2*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k24)*lam32 + lam3*(-(*k24)*(*k32) - (*ka)*(*k24)) + (*ka)*(*k24)*(*k32))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k24)*(*k32)/(lam2*lam1*lam3)) + (*k24)*(*k32)*(eiL1*(*r2)*((*k23)*lam12 + lam1*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(-(*ka)*lam13 + lam2*((*ka)*lam12 - lam13) + lam3*((*ka)*lam12 + lam2*(-(*ka)*lam1 + lam12) - lam13) + lam14) - eiL2*(*r2)*((*k23)*lam22 + lam2*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam23*((*ka) + lam1) + lam3*(lam22*(-(*ka) - lam1) + (*ka)*lam2*lam1 + lam23) - (*ka)*lam22*lam1 - lam24) + eiL3*(*r2)*((*k23)*lam32 + lam3*(-(*k42)*(*k23) - (*ka)*(*k23)) + (*ka)*(*k42)*(*k23))/(lam32*((*ka)*lam1 + lam2*((*ka) + lam1)) + (-(*ka) - lam1 - lam2)*lam33 - (*ka)*lam2*lam1*lam3 + lam34) + (*r2)*(*k42)*(*k23)/(lam2*lam1*lam3))))/((lam2 - lam3)*(lam1 - lam2));
}

static inline void threeCmtKaRate(double *A, double *Alast,
                                  double *t,
                                  double *b1, double *b2,
                                  double *r1, double *r2,
                                  double *ka, double *k20,
                                  double *k23, double *k32,
                                  double *k24, double *k42) {
  double E2 =  (*k20)+ (*k23) + (*k24);
  double j = (*k23)+(*k20)+(*k32)+(*k42)+(*k24);
  double k = (*k23)*(*k42)+(*k20)*(*k32)+(*k20)*(*k42)+(*k32)*(*k42)+(*k24)*(*k32);
  double l = (*k20)*(*k32)*(*k42);

  double m = 0.3333333333333333*(3.0*k - j*j);
  double n = 0.03703703703703703*(2.0*j*j*j - 9.0*j*k + 27.0*l);
  double Q = 0.25*n*n + 0.03703703703703703*m*m*m;

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double rho=sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double ct3 = cos(0.3333333333333333*theta);
  double rho3 = R_pow(rho,0.3333333333333333);
  double st3 = 1.732050807568877193177*sin(0.3333333333333333*theta);
  double j3 = 0.3333333333333333*j;
  double lam1 = j3  + rho3*(ct3 + st3);
  double lam2 = j3 + rho3*(ct3 - st3);
  double lam3 = j3 -(2.0*rho3*ct3);
  double eKa = exp(-(*ka)*(*t));
  A1 = (*r1)/(*ka)-(((*r1)-A1last*(*ka))*eKa)/(*ka);

  double lam12 = lam1*lam1;
  double lam13 = lam12*lam1;
  double lam14 = lam13*lam1;

  double lam22 = lam2*lam2;
  double lam23 = lam22*lam2;
  double lam24 = lam23*lam2;

  double lam32 = lam3*lam3;
  double lam33 = lam32*lam3;
  double lam34 = lam33*lam3;

  double ka2 = (*ka)*(*ka);
  double ka3 = ka2*(*ka);

  double a21 = (((lam33+(-(*ka)-(*k42)-(*k32))*lam32+(((*k42)+(*k32))*(*ka)+(*k32)*(*k42))*lam3-(*k32)*(*k42)*(*ka))*(*r2)+(-(*ka)*lam32+((*k42)+(*k32))*(*ka)*lam3-(*k32)*(*k42)*(*ka))*(*r1)-A2last*lam34+((A2last+A1last)*(*ka)+(A4last+A2last)*(*k42)+(A3last+A2last)*(*k32))*lam33+(((-A4last-A2last-A1last)*(*k42)+(-A3last-A2last-A1last)*(*k32))*(*ka)+(-A4last-A3last-A2last)*(*k32)*(*k42))*lam32+(A4last+A3last+A2last+A1last)*(*k32)*(*k42)*(*ka)*lam3)*exp(-lam3*(*t)))/(lam34+(-lam2-lam1-(*ka))*lam33+((lam1+(*ka))*lam2+(*ka)*lam1)*lam32-(*ka)*lam1*lam2*lam3);
  double a22 = (((lam23+(-(*ka)-(*k42)-(*k32))*lam22+(((*k42)+(*k32))*(*ka)+(*k32)*(*k42))*lam2-(*k32)*(*k42)*(*ka))*(*r2)+(-(*ka)*lam22+((*k42)+(*k32))*(*ka)*lam2-(*k32)*(*k42)*(*ka))*(*r1)-A2last*lam24+((A2last+A1last)*(*ka)+(A4last+A2last)*(*k42)+(A3last+A2last)*(*k32))*lam23+(((-A4last-A2last-A1last)*(*k42)+(-A3last-A2last-A1last)*(*k32))*(*ka)+(-A4last-A3last-A2last)*(*k32)*(*k42))*lam22+(A4last+A3last+A2last+A1last)*(*k32)*(*k42)*(*ka)*lam2)*exp(-lam2*(*t)))/((lam23+(-lam1-(*ka))*lam22+(*ka)*lam1*lam2)*lam3-lam24+(lam1+(*ka))*lam23-(*ka)*lam1*lam22);
  double a23 = (((lam13+(-(*ka)-(*k42)-(*k32))*lam12+(((*k42)+(*k32))*(*ka)+(*k32)*(*k42))*lam1-(*k32)*(*k42)*(*ka))*(*r2)+(-(*ka)*lam12+((*k42)+(*k32))*(*ka)*lam1-(*k32)*(*k42)*(*ka))*(*r1)-A2last*lam14+((A2last+A1last)*(*ka)+(A4last+A2last)*(*k42)+(A3last+A2last)*(*k32))*lam13+(((-A4last-A2last-A1last)*(*k42)+(-A3last-A2last-A1last)*(*k32))*(*ka)+(-A4last-A3last-A2last)*(*k32)*(*k42))*lam12+(A4last+A3last+A2last+A1last)*(*k32)*(*k42)*(*ka)*lam1)*exp(-lam1*(*t)))/(((lam12-(*ka)*lam1)*lam2-lam13+(*ka)*lam12)*lam3+((*ka)*lam12-lam13)*lam2+lam14-(*ka)*lam13);
  double a24 = (((ka2+(-(*k42)-(*k32))*(*ka)+(*k32)*(*k42))*(*r1)-A1last*ka3+(A1last*(*k42)+A1last*(*k32))*ka2-A1last*(*k32)*(*k42)*(*ka))*exp(-(*ka)*(*t)))/(((lam1-(*ka))*lam2-(*ka)*lam1+ka2)*lam3+(ka2-(*ka)*lam1)*lam2+ka2*lam1-ka3);
  double a25 = ((*k32)*(*k42)*(*r2)+(*k32)*(*k42)*(*r1))/(lam1*lam2*lam3);
  A2 = -a21+a22-a23-a24+a25;
  double a31 = ((((*k23)*lam32+(-(*k23)*(*ka)-(*k23)*(*k42))*lam3+(*k23)*(*k42)*(*ka))*(*r2)+((*k23)*(*k42)*(*ka)-(*k23)*(*ka)*lam3)*(*r1)+A3last*lam34+(-A3last*(*ka)-A3last*(*k42)-A2last*(*k23)-A3last*E2)*lam33+((A3last*(*k42)+(A2last+A1last)*(*k23)+A3last*E2)*(*ka)+(-A3last*(*k24)+(A4last+A2last)*(*k23)+A3last*E2)*(*k42))*lam32+(A3last*(*k24)+(-A4last-A2last-A1last)*(*k23)-A3last*E2)*(*k42)*(*ka)*lam3)*exp(-lam3*(*t)))/(lam34+(-lam2-lam1-(*ka))*lam33+((lam1+(*ka))*lam2+(*ka)*lam1)*lam32-(*ka)*lam1*lam2*lam3);
  double a32 = ((((*k23)*lam22+(-(*k23)*(*ka)-(*k23)*(*k42))*lam2+(*k23)*(*k42)*(*ka))*(*r2)+((*k23)*(*k42)*(*ka)-(*k23)*(*ka)*lam2)*(*r1)+A3last*lam24+(-A3last*(*ka)-A3last*(*k42)-A2last*(*k23)-A3last*E2)*lam23+((A3last*(*k42)+(A2last+A1last)*(*k23)+A3last*E2)*(*ka)+(-A3last*(*k24)+(A4last+A2last)*(*k23)+A3last*E2)*(*k42))*lam22+(A3last*(*k24)+(-A4last-A2last-A1last)*(*k23)-A3last*E2)*(*k42)*(*ka)*lam2)*exp(-lam2*(*t)))/((lam23+(-lam1-(*ka))*lam22+(*ka)*lam1*lam2)*lam3-lam24+(lam1+(*ka))*lam23-(*ka)*lam1*lam22);
  double a33 = ((((*k23)*lam12+(-(*k23)*(*ka)-(*k23)*(*k42))*lam1+(*k23)*(*k42)*(*ka))*(*r2)+((*k23)*(*k42)*(*ka)-(*k23)*(*ka)*lam1)*(*r1)+A3last*lam14+(-A3last*(*ka)-A3last*(*k42)-A2last*(*k23)-A3last*E2)*lam13+((A3last*(*k42)+(A2last+A1last)*(*k23)+A3last*E2)*(*ka)+(-A3last*(*k24)+(A4last+A2last)*(*k23)+A3last*E2)*(*k42))*lam12+(A3last*(*k24)+(-A4last-A2last-A1last)*(*k23)-A3last*E2)*(*k42)*(*ka)*lam1)*exp(-lam1*(*t)))/(((lam12-(*ka)*lam1)*lam2-lam13+(*ka)*lam12)*lam3+((*ka)*lam12-lam13)*lam2+lam14-(*ka)*lam13);
  double a34 = ((((*k23)*(*ka)-(*k23)*(*k42))*(*r1)-A1last*(*k23)*ka2+A1last*(*k23)*(*k42)*(*ka))*exp(-(*ka)*(*t)))/(((lam1-(*ka))*lam2-(*ka)*lam1+ka2)*lam3+(ka2-(*ka)*lam1)*lam2+ka2*lam1-ka3);
  double a35 = ((*k23)*(*k42)*(*r2)+(*k23)*(*k42)*(*r1))/(lam1*lam2*lam3);
  A3=a31-a32+a33+a34+a35;
  double a41 = ((((*k24)*lam32+(-(*k24)*(*ka)-(*k24)*(*k32))*lam3+(*k24)*(*k32)*(*ka))*(*r2)+((*k24)*(*k32)*(*ka)-(*k24)*(*ka)*lam3)*(*r1)+A4last*lam34+(-A4last*(*ka)-A4last*(*k32)-A2last*(*k24)-A4last*E2)*lam33+((A4last*(*k32)+(A2last+A1last)*(*k24)+A4last*E2)*(*ka)+((A3last+A2last)*(*k24)-A4last*(*k23)+A4last*E2)*(*k32))*lam32+((-A3last-A2last-A1last)*(*k24)+A4last*(*k23)-A4last*E2)*(*k32)*(*ka)*lam3)*exp(-lam3*(*t)))/(lam34+(-lam2-lam1-(*ka))*lam33+((lam1+(*ka))*lam2+(*ka)*lam1)*lam32-(*ka)*lam1*lam2*lam3);
  double a42 = ((((*k24)*lam22+(-(*k24)*(*ka)-(*k24)*(*k32))*lam2+(*k24)*(*k32)*(*ka))*(*r2)+((*k24)*(*k32)*(*ka)-(*k24)*(*ka)*lam2)*(*r1)+A4last*lam24+(-A4last*(*ka)-A4last*(*k32)-A2last*(*k24)-A4last*E2)*lam23+((A4last*(*k32)+(A2last+A1last)*(*k24)+A4last*E2)*(*ka)+((A3last+A2last)*(*k24)-A4last*(*k23)+A4last*E2)*(*k32))*lam22+((-A3last-A2last-A1last)*(*k24)+A4last*(*k23)-A4last*E2)*(*k32)*(*ka)*lam2)*exp(-lam2*(*t)))/((lam23+(-lam1-(*ka))*lam22+(*ka)*lam1*lam2)*lam3-lam24+(lam1+(*ka))*lam23-(*ka)*lam1*lam22);
  double a43 = ((((*k24)*lam12+(-(*k24)*(*ka)-(*k24)*(*k32))*lam1+(*k24)*(*k32)*(*ka))*(*r2)+((*k24)*(*k32)*(*ka)-(*k24)*(*ka)*lam1)*(*r1)+A4last*lam14+(-A4last*(*ka)-A4last*(*k32)-A2last*(*k24)-A4last*E2)*lam13+((A4last*(*k32)+(A2last+A1last)*(*k24)+A4last*E2)*(*ka)+((A3last+A2last)*(*k24)-A4last*(*k23)+A4last*E2)*(*k32))*lam12+((-A3last-A2last-A1last)*(*k24)+A4last*(*k23)-A4last*E2)*(*k32)*(*ka)*lam1)*exp(-lam1*(*t)))/(((lam12-(*ka)*lam1)*lam2-lam13+(*ka)*lam12)*lam3+((*ka)*lam12-lam13)*lam2+lam14-(*ka)*lam13);
  double a44 = ((((*k24)*(*ka)-(*k24)*(*k32))*(*r1)-A1last*(*k24)*ka2+A1last*(*k24)*(*k32)*(*ka))*exp(-(*ka)*(*t)))/(((lam1-(*ka))*lam2-(*ka)*lam1+ka2)*lam3+(ka2-(*ka)*lam1)*lam2+ka2*lam1-ka3);
  double a45 = ((*k24)*(*k32)*(*r2)+(*k24)*(*k32)*(*r1))/(lam1*lam2*lam3);
  A4=a41-a42+a43+a44+a45;
}


////////////////////////////////////////////////////////////////////////////////
// 1-3 oral absorption with rates.
// Adapted from Richard Upton's maxima notebooks and supplementary material from
// Abuhelwa2015
////////////////////////////////////////////////////////////////////////////////

static inline void oneCmtKaSSb1(double *A, double *tau,
                                double *b1, double *ka, double *k20) {
  double eKa = 1.0/(1.0-exp(-(*tau)*(*ka)));
  double eK =  1.0/(1.0-exp(-(*tau)*(*k20)));
  A1=eKa*(*b1);
  A2=(*ka)*(*b1)*(eK - eKa)/(-(*k20) + (*ka));
}

static inline void oneCmtKaSSb2(double *A, double *tau,
                                double *b2, double *ka, double *k20) {
  double eK =  1.0/(1.0-exp(-(*tau)*(*k20)));
  A1=0.0;
  A2=eK*(*b2);
}

static inline void oneCmtKa(double *A, double *Alast,
                            double *t, double *b1, double *b2,
                            double *ka, double *k20) {
  double rx_expr_0=exp(-(*t)*(*ka));
  A1=A1last*rx_expr_0;
  double rx_expr_1=exp(-(*t)*(*k20));
  A2=A1last*(*ka)/((*ka)-(*k20))*(rx_expr_1-rx_expr_0)+A2last*rx_expr_1;
}

static inline void twoCmtKaSSb1(double *A, double *tau, double *b1,
                                double *ka, double *k20,
                                double *k23, double *k32) {
  double E2 = (*k20)+(*k23);
  double E3 = (*k32);
  double e2e3 = E2+E3;
  double s = sqrt(e2e3*e2e3-4*(E2*E3-(*k23)*(*k32)));

  double lambda1 = 0.5*(e2e3+s);
  double lambda2 = 0.5*(e2e3-s);
  double eKa=1.0/(1.0-exp(-(*tau)*(*ka)));
  double eL1=1.0/(1.0-exp(-(*tau)*lambda1));
  double eL2=1.0/(1.0-exp(-(*tau)*lambda2));
  A1=eKa*(*b1);
  A2=(*ka)*(*b1)*(eL1*(E3 - lambda1)/((-lambda1 + lambda2)*((*ka) - lambda1)) + eL2*(E3 - lambda2)/((lambda1 - lambda2)*((*ka) - lambda2)) + eKa*(E3 - (*ka))/((-(*ka) + lambda2)*(-(*ka) + lambda1)));
  A3=(*ka)*(*b1)*(*k23)*(eL1/((-lambda1 + lambda2)*((*ka) - lambda1)) + eL2/((lambda1 - lambda2)*((*ka) - lambda2)) + eKa/((-(*ka) + lambda2)*(-(*ka) + lambda1)));
}

static inline void twoCmtKaSSb2(double *A, double *tau, double *b2,
                                double *ka, double *k20,
                                double *k23, double *k32) {
  double E2 = (*k20)+(*k23);
  double E3 = (*k32);
  double e2e3 = E2+E3;
  double s = sqrt(e2e3*e2e3-4*(E2*E3-(*k23)*(*k32)));

  double lambda1 = 0.5*(e2e3+s);
  double lambda2 = 0.5*(e2e3-s);
  double eL1=1.0/(1.0-exp(-(*tau)*lambda1));
  double eL2=1.0/(1.0-exp(-(*tau)*lambda2));

  A1=0.0;
  A2=(eL1*((*b2)*E3 - (*b2)*lambda1) - eL2*((*b2)*E3 - (*b2)*lambda2))/(-lambda1 + lambda2);
  A3=(eL1*(*b2)*(*k23) - eL2*(*b2)*(*k23))/(-lambda1 + lambda2);
}

static inline void twoCmtKa(double *A, double *Alast, double *t,
                            double *b1, double *b2,
                            double *ka, double *k20,
                            double *k23, double *k32) {
  double rxe2=exp(-(*t)*(*ka));
  A1=rxe2*A1last;
  double rxe0=(*k23)+(*k32);
  double rxe1=(*k23)+(*k20);
  double rxe3=(*k32)*A2last;
  double rxe4=(*k32)*A3last;
  double rxe6=rxe0+(*k20);
  double rxe7=(rxe1)*(*k32);
  double rxe8=rxe6*rxe6;
  double rxe10=sqrt(-4*(-(*k23)*(*k32)+rxe7)+rxe8);
  A2=(-exp(-0.5*(*t)*(rxe6-rxe10))*(-0.5*A2last*(rxe6-rxe10)+rxe3+rxe4)+exp(-0.5*(*t)*(rxe6+rxe10))*(-0.5*A2last*(rxe6+rxe10)+rxe3+rxe4))/(0.5*(rxe6-rxe10)-0.5*(rxe6+rxe10))+(*ka)*(rxe2*((*k32)-(*ka))/((-(*ka)+0.5*(rxe6-rxe10))*(-(*ka)+0.5*(rxe6+rxe10)))+exp(-0.5*(*t)*(rxe6-rxe10))*((*k32)-0.5*(rxe6-rxe10))/((-0.5*(rxe6-rxe10)+0.5*(rxe6+rxe10))*((*ka)-0.5*(rxe6-rxe10)))+exp(-0.5*(*t)*(rxe6+rxe10))*((*k32)-0.5*(rxe6+rxe10))/((0.5*(rxe6-rxe10)-0.5*(rxe6+rxe10))*((*ka)-0.5*(rxe6+rxe10))))*A1last;
  double rxe5=(*k23)*A2last;
  double rxe9=(rxe1)*A3last;
  A3=(-exp(-0.5*(*t)*(rxe6-rxe10))*(-0.5*A3last*(rxe6-rxe10)+rxe5+rxe9)+exp(-0.5*(*t)*(rxe6+rxe10))*(-0.5*A3last*(rxe6+rxe10)+rxe5+rxe9))/(0.5*(rxe6-rxe10)-0.5*(rxe6+rxe10))+(*ka)*(*k23)*A1last*(rxe2/((-(*ka)+0.5*(rxe6-rxe10))*(-(*ka)+0.5*(rxe6+rxe10)))+exp(-0.5*(*t)*(rxe6-rxe10))/((-0.5*(rxe6-rxe10)+0.5*(rxe6+rxe10))*((*ka)-0.5*(rxe6-rxe10)))+exp(-0.5*(*t)*(rxe6+rxe10))/((0.5*(rxe6-rxe10)-0.5*(rxe6+rxe10))*((*ka)-0.5*(rxe6+rxe10))));
}


static inline void threeCmtKaSSb1(double *A, double *tau, double *b1,
                                  double *ka, double *k20,
                                  double *k23, double *k32,
                                  double *k24, double *k42){
  double E2 = (*k20)+(*k23)+(*k24);
  double E3 = (*k32);
  double E4 = (*k42);

  double a = E2+E3+E4;
  double b = E2*E3+E4*(E2+E3)-(*k23)*(*k32)-(*k24)*(*k42);
  double c = E2*E3*E4-E4*(*k23)*(*k32)-E3*(*k24)*(*k42);

  double a2 = a*a;
  double m = 0.333333333333333*(3.0*b - a2);
  double n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
  double Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double gamma = sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double theta3 = 0.333333333333333*theta;
  double ctheta3 = cos(theta3);
  double stheta3 = 1.7320508075688771932*sin(theta3);
  double gamma3 = R_pow(gamma,0.333333333333333);

  double lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
  double lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
  double lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

  double eKa = 1.0/(1.0-exp(-(*tau)*(*ka)));
  double eL1 = 1.0/(1.0-exp(-(*tau)*lambda1));
  double eL2 = 1.0/(1.0-exp(-(*tau)*lambda2));
  double eL3 = 1.0/(1.0-exp(-(*tau)*lambda3));

  A1=eKa*(*b1);
  A2=(*ka)*(*b1)*(eL1*(E3 - lambda1)*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((*ka) - lambda1)) + eL2*(E3 - lambda2)*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((*ka) - lambda2)) + eL3*(E3 - lambda3)*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((*ka) - lambda3)) + eKa*(E3 - (*ka))*(E4 - (*ka))/((-(*ka) + lambda1)*(-(*ka) + lambda3)*(-(*ka) + lambda2)));
  A3=(*ka)*(*b1)*(*k23)*(eL1*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((*ka) - lambda1)) + eL2*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((*ka) - lambda2)) + eL3*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((*ka) - lambda3)) + eKa*(E4 - (*ka))/((-(*ka) + lambda1)*(-(*ka) + lambda3)*(-(*ka) + lambda2)));
  A4=(*ka)*(*b1)*(*k24)*(eL1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*((*ka) - lambda1)) + eL2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*((*ka) - lambda2)) + eL3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*((*ka) - lambda3)) + eKa*(E3 - (*ka))/((-(*ka) + lambda1)*(-(*ka) + lambda3)*(-(*ka) + lambda2)));
}

static inline void threeCmtKaSSb2(double *A, double *tau, double *b2,
                                  double *ka, double *k20,
                                  double *k23, double *k32,
                                  double *k24, double *k42) {
  double E2 = (*k20)+(*k23)+(*k24);
  double E3 = (*k32);
  double E4 = (*k42);

  double a = E2+E3+E4;
  double b = E2*E3+E4*(E2+E3)-(*k23)*(*k32)-(*k24)*(*k42);
  double c = E2*E3*E4-E4*(*k23)*(*k32)-E3*(*k24)*(*k42);

  double a2 = a*a;
  double m = 0.333333333333333*(3.0*b - a2);
  double n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
  double Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double gamma = sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double theta3 = 0.333333333333333*theta;
  double ctheta3 = cos(theta3);
  double stheta3 = 1.7320508075688771932*sin(theta3);
  double gamma3 = R_pow(gamma,0.333333333333333);

  double lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
  double lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
  double lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

  double eL1 = 1.0/(1.0-exp(-(*tau)*lambda1));
  double eL2 = 1.0/(1.0-exp(-(*tau)*lambda2));
  double eL3 = 1.0/(1.0-exp(-(*tau)*lambda3));
  A1=0.0;
  A2=(*b2)*(eL1*(E3 - lambda1)*(E4 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eL2*(E3 - lambda2)*(E4 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eL3*(E3 - lambda3)*(E4 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)));
  A3=eL2*(-(*b2)*E4*(*k23) + (*b2)*(*k23)*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*((*b2)*E4*(*k23) - (*b2)*(*k23)*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-(*b2)*E4*(*k23) + (*b2)*(*k23)*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
  A4=eL2*(-(*b2)*E3*(*k24) + (*b2)*(*k24)*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*((*b2)*E3*(*k24) - (*b2)*(*k24)*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-(*b2)*E3*(*k24) + (*b2)*(*k24)*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
}

static inline void threeCmtKa(double *A, double *Alast, double *t,
                              double *b1, double *b2,
                              double *ka, double *k20,
                              double *k23, double *k32,
                              double *k24, double *k42) {
  double E2 = (*k20)+(*k23)+(*k24);
  double E3 = (*k32);
  double E4 = (*k42);

  double a = E2+E3+E4;
  double b = E2*E3+E4*(E2+E3)-(*k23)*(*k32)-(*k24)*(*k42);
  double c = E2*E3*E4-E4*(*k23)*(*k32)-E3*(*k24)*(*k42);

  double a2 = a*a;
  double m = 0.333333333333333*(3.0*b - a2);
  double n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
  double Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double gamma = sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double theta3 = 0.333333333333333*theta;
  double ctheta3 = cos(theta3);
  double stheta3 = 1.7320508075688771932*sin(theta3);
  double gamma3 = R_pow(gamma,0.333333333333333);

  double lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
  double lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
  double lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

  double B = A3last*(*k32)+A4last*(*k42);
  double C = E4*A3last*(*k32)+E3*A4last*(*k42);
  double I = A2last*(*k23)*E4-A3last*(*k24)*(*k42)+A4last*(*k23)*(*k42);
  double J = A2last*(*k24)*E3+A3last*(*k24)*(*k32)-A4last*(*k23)*(*k32);

  double eL1 = exp(-(*t)*lambda1);
  double eL2 = exp(-(*t)*lambda2);
  double eL3 = exp(-(*t)*lambda3);
  double eka = exp(-(*t)*(*ka));

  double l12 = (lambda1-lambda2);
  double l13 = (lambda1-lambda3);
  double l21 = (lambda2-lambda1);
  double l23 = (lambda2-lambda3);
  double l31 = (lambda3-lambda1);
  double l32 = (lambda3-lambda2);

  double e2l1 = (E2-lambda1);
  double e2l2 = (E2-lambda2);
  double e3l1 = (E3-lambda1);
  double e3l2 = (E3-lambda2);
  double e3l3 = (E3-lambda3);
  double e4l1 = (E4-lambda1);
  double e4l2 = (E4-lambda2);
  double e4l3 = (E4-lambda3);

  double A2term1 = A2last*(eL1*e3l1*e4l1/(l21*l31)+eL2*e3l2*e4l2/(l12*l32)+eL3*e3l3*e4l3/(l13*l23));

  double A2term2 = eL1*(C-B*lambda1)/(l12*l13)+eL2*(B*lambda2-C)/(l12*l23)+eL3*(B*lambda3-C)/(l13*l32);

  double A2term3 = A1last*(*ka)*(eL1*e3l1*e4l1/(l21*l31*((*ka)-lambda1))+eL2*e3l2*e4l2/(l12*l32*((*ka)-lambda2))+eL3*e3l3*e4l3/(l13*l23*((*ka)-lambda3))+eka*(E3-(*ka))*(E4-(*ka))/((lambda1-(*ka))*(lambda2-(*ka))*(lambda3-(*ka))));

  A2 = A2term1+A2term2+A2term3;

  double A3term1 = A3last*(eL1*e2l1*e4l1/(l21*l31)+eL2*e2l2*e4l2/(l12*l32)+eL3*(E2-lambda3)*e4l3/(l13*l23));

  double A3term2 = eL1*(I-A2last*(*k23)*lambda1)/(l12*l13)+eL2*(A2last*(*k23)*lambda2-I)/(l12*l23)+eL3*(A2last*(*k23)*lambda3-I)/(l13*l32);

  double A3term3 = A1last*(*ka)*(*k23)*(eL1*e4l1/(l21*l31*((*ka)-lambda1))+eL2*e4l2/(l12*l32*((*ka)-lambda2))+eL3*e4l3/(l13*l23*((*ka)-lambda3))+eka*(E4-(*ka))/((lambda1-(*ka))*(lambda2-(*ka))*(lambda3-(*ka))));

  A3 = A3term1+A3term2+A3term3;

  double A4term1 = A4last*(eL1*e2l1*e3l1/(l21*l31)+eL2*e2l2*e3l2/(l12*l32)+eL3*(E2-lambda3)*e3l3/(l13*l23));

  double A4term2 = eL1*(J-A2last*(*k24)*lambda1)/(l12*l13)+eL2*(A2last*(*k24)*lambda2-J)/(l12*l23)+eL3*(A2last*(*k24)*lambda3-J)/(l13*l32);

  double A4term3 = A1last*(*ka)*(*k24)*(eL1*e3l1/(l21*l31*((*ka)-lambda1))+eL2*e3l2/(l12*l32*((*ka)-lambda2))+eL3*e3l3/(l13*l23*((*ka)-lambda3))+eka*(E3-(*ka))/((lambda1-(*ka))*(lambda2-(*ka))*(lambda3-(*ka))));
  A4 = A4term1+A4term2+A4term3;

  A1 = A1last*eka;
}

////////////////////////////////////////////////////////////////////////////////
// 1-3 compartment bolus during infusion
////////////////////////////////////////////////////////////////////////////////
static inline void oneCmtRateSSr1(double *A, double *r1, double *k10) {
  A1 = (*r1)/(*k10);
}

static inline void oneCmtRateSS(double *A, double *tinf, double *tau, double *r1, double *k10) {
  double eiK = exp(-(*k10)*(*tinf));
  double eK = exp(-(*k10)*((*tau)-(*tinf)))/(1.0-exp(-(*k10)*(*tau)));
  A1=(*r1)*(1-eiK)*eK/((*k10));
}

static inline void oneCmtRate(double *A, double *Alast,
                              double *t,
                              double *b1, double *r1,
                              double *k10) {
  double eT = exp(-(*k10)*(*t));
  A1 = (*r1)/(*k10)*(1-eT)+A1last*eT;
}

static inline void twoCmtRateSSr1(double *A, double *r1, double *k10, double *k12, double *k21) {
  double E1 = (*k10)+(*k12);
  double s = E1+(*k21);
  double sqr = sqrt(s*s-4*(E1*(*k21)-(*k12)*(*k21)));
  double lambda1 = 0.5*(s+sqr);
  double lambda2 = 0.5*(s-sqr);
  double l12 = 1.0/(lambda1*lambda2);
  A1=(*r1)*(*k21)*l12;
  A2=(*r1)*(*k12)*l12;
}

static inline void twoCmtRateSS(double *A, double *tinf, double *tau, double *r1,
                                double *k10, double *k12, double *k21) {
  double E1 = (*k10)+(*k12);
  double E2 = (*k21);

  double s = E1+E2;
  double sqr = sqrt(s*s-4*(E1*E2-(*k12)*(*k21)));
  double lambda1 = 0.5*(s+sqr);
  double lambda2 = 0.5*(s-sqr);

  double eTi1 = exp(-(*tinf)*lambda1);
  double eTi2 = exp(-(*tinf)*lambda2);
  double eT1 =exp(-lambda1*((*tau)-(*tinf)))/(1.0-exp(-(*tau)*lambda1));
  double eT2 =exp(-lambda2*((*tau)-(*tinf)))/(1.0-exp(-(*tau)*lambda2));
  A1=(eT1*(E2*((eTi1*(*r1) - eTi2*(*r1))/(-lambda1 + lambda2) + (*r1)*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) - lambda1*((eTi1*(*r1) - eTi2*(*r1))/(-lambda1 + lambda2) + (*r1)*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) + (*r1)*(*k12)*(*k21)*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) - eT2*(E2*((eTi1*(*r1) - eTi2*(*r1))/(-lambda1 + lambda2) + (*r1)*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) - lambda2*((eTi1*(*r1) - eTi2*(*r1))/(-lambda1 + lambda2) + (*r1)*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) + (*r1)*(*k12)*(*k21)*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))))/(-lambda1 + lambda2);
  A2=(eT1*((*k12)*((eTi1*(*r1) - eTi2*(*r1))/(-lambda1 + lambda2) + (*r1)*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) + (*r1)*E1*(*k12)*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2)) - (*r1)*(*k12)*lambda1*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) - eT2*((*k12)*((eTi1*(*r1) - eTi2*(*r1))/(-lambda1 + lambda2) + (*r1)*E2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))) + (*r1)*E1*(*k12)*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2)) - (*r1)*(*k12)*lambda2*(1.0/(lambda1*lambda2) + eTi1/((lambda1 - lambda2)*lambda1) - eTi2/((lambda1 - lambda2)*lambda2))))/(-lambda1 + lambda2);
}

static inline void twoCmtRate(double *A, double *Alast, double *t,
                              double *b1, double *r1,
                              double *k10, double *k12, double *k21) {
  double E1 = (*k10)+(*k12);
  double E2 = (*k21);

  double s = E1+E2;
  double sqr = sqrt(s*s-4*(E1*E2-(*k12)*(*k21)));
  double lambda1 = 0.5*(s+sqr);
  double lambda2 = 0.5*(s-sqr);

  double eT1 = exp(-(*t)*lambda1);
  double eT2 = exp(-(*t)*lambda2);

  double l12 = (lambda1-lambda2);
  double l21 = (lambda2-lambda1);

  double c10 = (A1last*E2+(*r1)+A2last*(*k21));
  double c11 = (c10-A1last*lambda1)/l21;
  double c12 = (c10-A1last*lambda2)/l21;
  double A1term1 = c11*eT1 - c12*eT2;
  double A1term2 = (*r1)*E2*(1/(lambda1*lambda2)+eT1/(lambda1*l12)-eT2/(lambda2*l12));
  A1 = A1term1+A1term2;

  double c20 = (A2last*E1+A1last*(*k12));
  double c21 = (c20-A2last*lambda1)/l21;
  double c22 = (c20-A2last*lambda2)/l21;
  double A2term1 = c21*eT1-c22*eT2;
  double A2term2 = (*r1)*(*k12)*(1/(lambda1*lambda2)+eT1/(lambda1*l12)-eT2/(lambda2*(lambda1-lambda2)));
  A2 = A2term1+A2term2;
}

static inline void threeCmtRateSSr1(double *A, double *r1,
                                    double *k10, double *k12, double *k21,
                                    double *k13, double *k31) {
  double E1 = (*k10)+(*k12)+(*k13);
  double E2 = (*k21);
  double E3 = (*k31);

  double a = E1+E2+E3;
  double b = E1*E2+E3*(E1+E2)-(*k12)*(*k21)-(*k13)*(*k31);
  double c = E1*E2*E3-E3*(*k12)*(*k21)-E2*(*k13)*(*k31);

  double a2 = a*a;
  double m = 0.333333333333333*(3.0*b - a2);
  double n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
  double Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double gamma = sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double theta3 = 0.333333333333333*theta;
  double ctheta3 = cos(theta3);
  double stheta3 = 1.7320508075688771932*sin(theta3);
  double gamma3 = R_pow(gamma,0.333333333333333);

  double lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
  double lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
  double lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);
  double l123 = 1.0/(lambda1*lambda2*lambda3);
  A1=(*r1)*E2*E3*l123;
  A2=(*r1)*E3*(*k12)*l123;
  A3=(*r1)*E2*(*k13)*l123;
}

static inline void threeCmtRateSS(double *A,
                                  double *tinf, double *tau, double *r1,
                                  double *k10, double *k12, double *k21,
                                  double *k13, double *k31){
  double E1 = (*k10)+(*k12)+(*k13);
  double E2 = (*k21);
  double E3 = (*k31);

  double a = E1+E2+E3;
  double b = E1*E2+E3*(E1+E2)-(*k12)*(*k21)-(*k13)*(*k31);
  double c = E1*E2*E3-E3*(*k12)*(*k21)-E2*(*k13)*(*k31);

  double a2 = a*a;
  double m = 0.333333333333333*(3.0*b - a2);
  double n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
  double Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double gamma = sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double theta3 = 0.333333333333333*theta;
  double ctheta3 = cos(theta3);
  double stheta3 = 1.7320508075688771932*sin(theta3);
  double gamma3 = R_pow(gamma,0.333333333333333);

  double lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
  double lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
  double lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);
  double eTi1 = exp(-(*tinf)*lambda1);
  double eTi2 = exp(-(*tinf)*lambda2);
  double eTi3 = exp(-(*tinf)*lambda3);
  double eT1 = exp(-lambda1*((*tau)-(*tinf)))/(1.0-exp(-(*tau)*lambda1));
  double eT2 = exp(-lambda2*((*tau)-(*tinf)))/(1.0-exp(-(*tau)*lambda2));
  double eT3 = exp(-lambda3*((*tau)-(*tinf)))/(1.0-exp(-(*tau)*lambda3));
  A1=(*r1)*(eT1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eT2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eT3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)))*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + eT2*(lambda2*((*r1)*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*(*k13)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))) - ((*r1)*E2*(*k13)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*E3*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda2)*(lambda2 - lambda3)) + eT1*(-lambda1*((*r1)*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*(*k13)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))) + (*r1)*E2*(*k13)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*E3*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)))/((lambda1 - lambda3)*(lambda1 - lambda2)) + eT3*(lambda3*((*r1)*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*(*k13)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))) - ((*r1)*E2*(*k13)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*E3*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda3)*(-lambda2 + lambda3));
  A2=(*r1)*(*k12)*(eT1*(E3 - lambda1)*(E1 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eT2*(E1 - lambda2)*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eT3*(E1 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)))*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + eT2*((*r1)*(*k12)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda2 - ((*r1)*E3*(*k12)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*(*k13)*(*k12)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - (*r1)*(*k13)*(*k12)*(*k31)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda2)*(lambda2 - lambda3)) + eT1*((*r1)*E3*(*k12)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - (*r1)*(*k12)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda1 + (*r1)*(*k13)*(*k12)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - (*r1)*(*k13)*(*k12)*(*k31)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)))/((lambda1 - lambda3)*(lambda1 - lambda2)) + eT3*((*r1)*(*k12)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda3 - ((*r1)*E3*(*k12)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*(*k13)*(*k12)*(*k31)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - (*r1)*(*k13)*(*k12)*(*k31)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda3)*(-lambda2 + lambda3));
  A3=(*r1)*(*k13)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*(eT1*(E2 - lambda1)*(E1 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eT2*(E1 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eT3*(E1 - lambda3)*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3))) + eT2*((*r1)*(*k13)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda2 - ((*r1)*E2*(*k13)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - (*r1)*(*k13)*(*k12)*(*k21)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*(*k13)*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda2)*(lambda2 - lambda3)) + eT1*((*r1)*E2*(*k13)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - (*r1)*(*k13)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda1 - (*r1)*(*k13)*(*k12)*(*k21)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*(*k13)*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)))/((lambda1 - lambda3)*(lambda1 - lambda2)) + eT3*((*r1)*(*k13)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))*lambda3 - ((*r1)*E2*(*k13)*(E2*E3/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) - (*r1)*(*k13)*(*k12)*(*k21)*(E2/(lambda1*lambda2*lambda3) - eTi1*(E2 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E2 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3)) + (*r1)*(*k13)*(*k12)*(*k21)*(E3/(lambda1*lambda2*lambda3) - eTi1*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)*lambda1) - eTi2*(E3 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)*lambda2) - eTi3*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)*lambda3))))/((lambda1 - lambda3)*(-lambda2 + lambda3));
}

static inline void threeCmtRate(double *A, double *Alast,
                                double *t, double *b1, double *r1,
                                double *k10, double *k12, double *k21,
                                double *k13, double *k31) {
  double E1 = (*k10)+(*k12)+(*k13);
  double E2 = (*k21);
  double E3 = (*k31);

  double a = E1+E2+E3;
  double b = E1*E2+E3*(E1+E2)-(*k12)*(*k21)-(*k13)*(*k31);
  double c = E1*E2*E3-E3*(*k12)*(*k21)-E2*(*k13)*(*k31);

  double a2 = a*a;
  double m = 0.333333333333333*(3.0*b - a2);
  double n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
  double Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double gamma = sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);
  double theta3 = 0.333333333333333*theta;
  double ctheta3 = cos(theta3);
  double stheta3 = 1.7320508075688771932*sin(theta3);
  double gamma3 = R_pow(gamma,0.333333333333333);

  double lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
  double lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
  double lambda3 = 0.333333333333333*a -(2.0*gamma3*ctheta3);

  double B = A2last*(*k21)+A3last*(*k31);
  double C = E3*A2last*(*k21)+E2*A3last*(*k31);
  double I = A1last*(*k12)*E3-A2last*(*k13)*(*k31)+A3last*(*k12)*(*k31);
  double J = A1last*(*k13)*E2+A2last*(*k13)*(*k21)-A3last*(*k12)*(*k21);

  double eL1 = exp(-(*t)*lambda1);
  double eL2 = exp(-(*t)*lambda2);
  double eL3 = exp(-(*t)*lambda3);

  double l12 = (lambda1-lambda2);
  double l13 = (lambda1-lambda3);
  double l21 = (lambda2-lambda1);
  double l23 = (lambda2-lambda3);
  double l31 = (lambda3-lambda1);
  double l32 = (lambda3-lambda2);

  double e1l1 = (E1-lambda1);
  double e1l2 = (E1-lambda2);
  double e1l3 = (E1-lambda3);
  double e2l1 = (E2-lambda1);
  double e2l2 = (E2-lambda2);
  double e2l3 = (E2-lambda3);
  double e3l1 = (E3-lambda1);
  double e3l2 = (E3-lambda2);
  double e3l3 = (E3-lambda3);

  double A1term1 = A1last*(eL1*e2l1*e3l1/(l21*l31)+eL2*e2l2*e3l2/(l12*l32)+eL3*e2l3*e3l3/(l13*l23));
  double A1term2 = eL1*(C-B*lambda1)/(l12*l13)+eL2*(B*lambda2-C)/(l12*l23)+eL3*(B*lambda3-C)/(l13*l32);
  double A1term3 = (*r1)*((E2*E3)/(lambda1*lambda2*lambda3)-eL1*e2l1*e3l1/(lambda1*l21*l31)-eL2*e2l2*e3l2/(lambda2*l12*l32)-eL3*e2l3*e3l3/(lambda3*l13*l23));

  A1 = A1term1+A1term2+A1term3;

  double A2term1 = A2last*(eL1*e1l1*e3l1/(l21*l31)+eL2*e1l2*e3l2/(l12*l32)+eL3*e1l3*e3l3/(l13*l23));
  double A2term2 = eL1*(I-A1last*(*k12)*lambda1)/(l12*l13)+eL2*(A1last*(*k12)*lambda2-I)/(l12*l23)+eL3*(A1last*(*k12)*lambda3-I)/(l13*l32);
  double A2term3 = (*r1)*(*k12)*(E3/(lambda1*lambda2*lambda3)-eL1*e3l1/(lambda1*l21*l31)-eL2*e3l2/(lambda2*l12*l32)-eL3*e3l3/(lambda3*l13*l23));

  A2 = A2term1+A2term2+A2term3;

  double A3term1 = A3last*(eL1*e1l1*e2l1/(l21*l31)+eL2*e1l2*e2l2/(l12*l32)+eL3*e1l3*e2l3/(l13*l23));
  double A3term2 = eL1*(J-A1last*(*k13)*lambda1)/(l12*l13)+eL2*(A1last*(*k13)*lambda2-J)/(l12*l23)+eL3*(A1last*(*k13)*lambda3-J)/(l13*l32);
  double A3term3 = (*r1)*(*k13)*(E2/(lambda1*lambda2*lambda3)-eL1*e2l1/(lambda1*l21*l31)-eL2*e2l2/(lambda2*l12*l32)-eL3*e2l3/(lambda3*l13*l23));

  A3 = A3term1+A3term2+A3term3;
}

////////////////////////////////////////////////////////////////////////////////
// 1-3 compartment bolus only
//
static inline void oneCmtBolusSS(double *A, double *tau,
                                 double *b1, double *k10) {
  double eT = 1.0/(1.0-exp(-(*k10)*(*tau)));
  A1 = (*b1)*eT;
}

static inline void oneCmtBolus(double *A, double *Alast,
                               double *t, double *b1, double *k10) {
  A1 = A1last*exp(-(*k10)*(*t));
}

static inline void twoCmtBolusSS(double *A,
                                 double *tau, double *b1, double *k10,
                                 double *k12, double *k21) {
  double E2 = (*k21);

  double s = (*k12)+(*k21)+(*k10);
  double sqr = sqrt(s*s-4*(*k21)*(*k10));
  double lambda1 = 0.5*(s+sqr);
  double lambda2 = 0.5*(s-sqr);

  double eL1 = 1.0/(1.0-exp(-(*tau)*lambda1));
  double eL2 = 1.0/(1.0-exp(-(*tau)*lambda2));

  A1=(eL1*((*b1)*E2 - (*b1)*lambda1) - eL2*((*b1)*E2 - (*b1)*lambda2))/(-lambda1 + lambda2);
  A2=(eL1*(*b1)*(*k12) - eL2*(*b1)*(*k12))/(-lambda1 + lambda2);
}

static inline void twoCmtBolus(double *A, double *Alast,
                               double *t, double *b1, double *k10,
                               double *k12, double *k21) {
  double E1 = (*k10)+(*k12);
  double E2 = (*k21);

  double s = (*k12)+(*k21)+(*k10);
  double sqr = sqrt(s*s-4*(*k21)*(*k10));
  double lambda1 = 0.5*(s+sqr);
  double lambda2 = 0.5*(s-sqr);

  double eT1= exp(-(*t)*lambda1);
  double eT2= exp(-(*t)*lambda2);

  A1 = (((A1last*E2+A2last*(*k21))-A1last*lambda1)*eT1-((A1last*E2+A2last*(*k21))-A1last*lambda2)*eT2)/(lambda2-lambda1);
  A2 = (((A2last*E1+A1last*(*k12))-A2last*lambda1)*eT1-((A2last*E1+A1last*(*k12))-A2last*lambda2)*eT2)/(lambda2-lambda1);
}

static inline void threeCmtBolusSS(double *A, double *tau, double *b1, double *k10,
                                   double *k12, double *k21,
                                   double *k13, double *k31){
  double E1 = (*k10)+(*k12)+(*k13);
  double E2 = (*k21);
  double E3 = (*k31);

  double a = E1+E2+E3;
  double b = E1*E2+E3*(E1+E2)-(*k12)*(*k21)-(*k13)*(*k31);
  double c = E1*E2*E3-E3*(*k12)*(*k21)-E2*(*k13)*(*k31);

  double a2 = a*a;
  double m = 0.333333333333333*(3.0*b - a2);
  double n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
  double Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double gamma = sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);

  double theta3 = 0.333333333333333*theta;
  double ctheta3 = cos(theta3);
  double stheta3 = 1.7320508075688771932*sin(theta3);
  double gamma3 = R_pow(gamma,0.333333333333333);
  double lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
  double lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
  double lambda3 = 0.333333333333333*a -(2*gamma3*ctheta3);

  double eL1 = 1.0/(1.0-exp(-(*tau)*lambda1));
  double eL2 = 1.0/(1.0-exp(-(*tau)*lambda2));
  double eL3 = 1.0/(1.0-exp(-(*tau)*lambda3));

  A1=(*b1)*(eL1*(E2 - lambda1)*(E3 - lambda1)/((-lambda1 + lambda3)*(-lambda1 + lambda2)) + eL2*(E3 - lambda2)*(E2 - lambda2)/((lambda1 - lambda2)*(-lambda2 + lambda3)) + eL3*(E2 - lambda3)*(E3 - lambda3)/((lambda1 - lambda3)*(lambda2 - lambda3)));
  A2=eL2*(-(*b1)*E3*(*k12) + (*b1)*(*k12)*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*((*b1)*E3*(*k12) - (*b1)*(*k12)*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-(*b1)*E3*(*k12) + (*b1)*(*k12)*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
  A3=eL2*(-(*b1)*E2*(*k13) + (*b1)*(*k13)*lambda2)/((lambda1 - lambda2)*(lambda2 - lambda3)) + eL1*((*b1)*E2*(*k13) - (*b1)*(*k13)*lambda1)/((lambda1 - lambda3)*(lambda1 - lambda2)) + eL3*(-(*b1)*E2*(*k13) + (*b1)*(*k13)*lambda3)/((lambda1 - lambda3)*(-lambda2 + lambda3));
}

static inline void threeCmtBolus(double *A, double *Alast,
                                 double *t, double *b1, double *k10,
                                 double *k12, double *k21,
                                 double *k13, double *k31){
  double E1 = (*k10)+(*k12)+(*k13);
  double E2 = (*k21);
  double E3 = (*k31);

  double a = E1+E2+E3;
  double b = E1*E2+E3*(E1+E2)-(*k12)*(*k21)-(*k13)*(*k31);
  double c = E1*E2*E3-E3*(*k12)*(*k21)-E2*(*k13)*(*k31);

  double a2 = a*a;
  double m = 0.333333333333333*(3.0*b - a2);
  double n = 0.03703703703703703*(2.0*a2*a - 9.0*a*b + 27.0*c);
  double Q = 0.25*(n*n) + 0.03703703703703703*(m*m*m);

  double alpha = sqrt(-Q);
  double beta = -0.5*n;
  double gamma = sqrt(beta*beta+alpha*alpha);
  double theta = atan2(alpha,beta);

  double theta3 = 0.333333333333333*theta;
  double ctheta3 = cos(theta3);
  double stheta3 = 1.7320508075688771932*sin(theta3);
  double gamma3 = R_pow(gamma,0.333333333333333);
  double lambda1 = 0.333333333333333*a + gamma3*(ctheta3 + stheta3);
  double lambda2 = 0.333333333333333*a + gamma3*(ctheta3 -stheta3);
  double lambda3 = 0.333333333333333*a -(2*gamma3*ctheta3);

  double B = A2last*(*k21)+A3last*(*k31);
  double C = E3*A2last*(*k21)+E2*A3last*(*k31);
  double I = A1last*(*k12)*E3-A2last*(*k13)*(*k31)+A3last*(*k12)*(*k31);
  double J = A1last*(*k13)*E2+A2last*(*k13)*(*k21)-A3last*(*k12)*(*k21);

  double eL1 = exp(-(*t)*lambda1);
  double eL2 = exp(-(*t)*lambda2);
  double eL3 = exp(-(*t)*lambda3);

  double l12 = (lambda1-lambda2);
  double l13 = (lambda1-lambda3);
  double l21 = (lambda2-lambda1);
  double l23 = (lambda2-lambda3);
  double l31 = (lambda3-lambda1);
  double l32 = (lambda3-lambda2);

  double e1l1 = (E1-lambda1);
  double e1l2 = (E1-lambda2);
  double e1l3 = (E1-lambda3);
  double e2l1 = (E2-lambda1);
  double e2l2 = (E2-lambda2);
  double e2l3 = (E2-lambda3);
  double e3l1 = (E3-lambda1);
  double e3l2 = (E3-lambda2);
  double e3l3 = (E3-lambda3);

  double A1term1 = A1last*(eL1*e2l1*e3l1/(l21*l31)+eL2*e2l2*e3l2/(l12*l32)+eL3*e2l3*e3l3/(l13*l23));
  double A1term2 = eL1*(C-B*lambda1)/(l12*l13)+eL2*(B*lambda2-C)/(l12*l23)+eL3*(B*lambda3-C)/(l13*l32);

  A1 = (A1term1+A1term2);

  double A2term1 = A2last*(eL1*e1l1*e3l1/(l21*l31)+eL2*e1l2*e3l2/(l12*l32)+eL3*e1l3*e3l3/(l13*l23));
  double A2term2 = eL1*(I-A1last*(*k12)*lambda1)/(l12*l13)+eL2*(A1last*(*k12)*lambda2-I)/(l12*l23)+eL3*(A1last*(*k12)*lambda3-I)/(l13*l32);

  A2 = A2term1+A2term2;

  double A3term1 = A3last*(eL1*e1l1*e2l1/(l21*l31)+eL2*e1l2*e2l2/(l12*l32)+eL3*e1l3*e2l3/(l13*l23));
  double A3term2 = eL1*(J-A1last*(*k13)*lambda1)/(l12*l13)+eL2*(A1last*(*k13)*lambda2-J)/(l12*l23)+eL3*(A1last*(*k13)*lambda3-J)/(l13*l32);
  A3 = A3term1+A3term2;
}

static inline void ssRateTau(double *A,
                             int ncmt,
                             int oral0,
                             double *tinf,
                             double *tau,
                             double *r1,
                             double *r2,
                             double *ka,
                             double *kel,
                             double *k12, double *k21,
                             double *k13, double *k31){
  if (oral0){
    if ((*r1) > 0 ){
      switch (ncmt){
      case 1: {
        oneCmtKaRateSStr1(A, tinf, tau, r1, ka, kel);
        return;
      } break;
      case 2: {
        twoCmtKaRateSStr1(A, tinf, tau, r1, ka, kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtKaRateSStr1(A, tinf, tau, r1, ka, kel, k12, k21, k13, k31);
        return;
      } break;
      }
    } else {
      switch (ncmt){
      case 1: {
        oneCmtKaRateSStr2(A, tinf, tau, r2, ka, kel);
        return;
      } break;
      case 2: {
        twoCmtKaRateSStr2(A, tinf, tau, r2, ka, kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtKaRateSStr2(A, tinf, tau, r2, ka, kel, k12,  k21, k13, k31);
        return;
      } break;
      }
    }
  } else {
    switch (ncmt){
    case 1: {
      oneCmtRateSS(A, tinf, tau, r1, kel);
      return;
    } break;
    case 2: {
      twoCmtRateSS(A, tinf, tau, r1, kel, k12, k21);
      return;
    } break;
    case 3: {
      threeCmtRateSS(A, tinf, tau, r1, kel, k12,  k21, k13,  k31);
      return;
    } break;
    }
  }
}

static inline void ssTau(double *A,
                         int ncmt,
                         int oral0,
                         double *tau,
                         double *b1,
                         double *b2,
                         double *ka, // ka (for oral doses)
                         double *kel,  //double rx_v,
                         double *k12, double *k21,
                         double *k13, double *k31){
  if (oral0){
    if ((*b1) > 0 ){
      switch (ncmt){
      case 1: {
        oneCmtKaSSb1(A, tau, b1, ka, kel);
        return;
      } break;
      case 2: {
        twoCmtKaSSb1(A, tau, b1, ka, kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtKaSSb1(A, tau, b1, ka, kel, k12,  k21, k13, k31);
        return;
      } break;
      }
    } else {
      switch (ncmt){
      case 1: {
        oneCmtKaSSb2(A, tau, b2, ka, kel);
        return;
      } break;
      case 2: {
        twoCmtKaSSb2(A, tau, b2, ka, kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtKaSSb2(A, tau, b2, ka, kel, k12,  k21, k13, k31);
        return;
      } break;
      }
    }
  } else {
    switch (ncmt){
    case 1: {
      oneCmtBolusSS(A, tau, b1, kel);
      return;
    } break;
    case 2: {
      twoCmtBolusSS(A, tau, b1, kel, k12, k21);
      return;
    } break;
    case 3: {
      threeCmtBolusSS(A, tau, b1, kel, k12,  k21, k13,  k31);
      return;
    } break;
    }
  }
}

static inline void ssRate(double *A,
                          int ncmt, // Number of compartments
                          int oral0, // Indicator of if this is an oral system)
                          double *r1, // Rate in Compartment #1
                          double *r2, // Rate in Compartment #2
                          double *ka, // ka (for oral doses)
                          double *kel,  //double rx_v,
                          double *k12, double *k21,
                          double *k13, double *k31) {
  if (oral0){
    if ((*r1) > 0){
      switch (ncmt){
      case 1: {
        oneCmtKaRateSSr1(A, r1, ka, kel);
        return;
      } break;
      case 2: {
        twoCmtKaRateSSr1(A, r1, ka, kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtKaRateSSr1(A, r1, ka, kel, k12,  k21, k13,  k31);
        return;
      } break;
      }
    } else {
      switch (ncmt){
      case 1: {
        oneCmtKaRateSSr2(A, r2, ka, kel);
        return;
      } break;
      case 2: {
        twoCmtKaRateSSr2(A, r2, ka, kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtKaRateSSr2(A, r2, ka, kel, k12,  k21, k13,  k31);
        return;
      } break;
      }
    }
  } else {
    switch (ncmt){
    case 1: {
      oneCmtRateSSr1(A, r1, kel);
      return;
    } break;
    case 2: {
      twoCmtRateSSr1(A, r1, kel, k12, k21);
      return;
    } break;
    case 3: {
      threeCmtRateSSr1(A, r1, kel, k12,  k21, k13,  k31);
      return;
    } break;
    }
  }
}

static inline void doAdvan(double *A,// Amounts
                           double *Alast, // Last amounts
                           double tlast, // Time of last amounts
                           double ct, // Time of the dose
                           int ncmt, // Number of compartments
                           int oral0, // Indicator of if this is an oral system
                           double *b1, // Amount of the dose in compartment #1
                           double *b2, // Amount of the dose in compartment #2
                           double *r1, // Rate in Compartment #1
                           double *r2, // Rate in Compartment #2
                           double *ka, // ka (for oral doses)
                           double *kel,  //double rx_v,
                           double *k12, double *k21,
                           double *k13, double *k31){
  double t = ct - tlast;
  if (isSameTime(ct, tlast)){
    for (int i = ncmt+oral0; i--;) {
      A[i] = Alast[i];
    }
    return;
  }
  if ((*r1) > DBL_EPSILON  || (*r2) > DBL_EPSILON){
    if (oral0){
      switch (ncmt){
      case 1: {
        oneCmtKaRate(A, Alast, &t, b1, b2, r1, r2, ka, kel);
        return;
      } break;
      case 2: {
        twoCmtKaRate(A, Alast, &t, b1, b2, r1, r2,
                     ka,  kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtKaRate(A, Alast, &t, b1, b2, r1, r2, ka, kel, k12,  k21, k13,  k31);
        return;
      } break;
      }
    } else {
      switch (ncmt){
      case 1: {
        oneCmtRate(A, Alast, &t, b1, r1, kel);
        return;
      } break;
      case 2: {
        twoCmtRate(A, Alast, &t, b1, r1,
                   kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtRate(A, Alast, &t, b1, r1, kel,
                     k12, k21, k13, k31);
        return;
      } break;

      }
    }
  } else {
    // Bolus doses only
    if (oral0){
      switch (ncmt){
      case 1: {
        oneCmtKa(A, Alast,
                 &t, b1, b2, ka, kel);
        return;
      } break;
      case 2: {
        twoCmtKa(A, Alast, &t, b1, b2, ka,  kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtKa(A, Alast,
                   &t, b1, b2, ka, kel,
                   k12,  k21, k13,  k31);
        return;
      } break;
      }
    } else {
      // Bolus
      switch (ncmt){
      case 1: {
        oneCmtBolus(A, Alast, &t, b1, kel);
        return;
      } break;
      case 2: {
        twoCmtBolus(A, Alast, &t, b1,
                    kel, k12, k21);
        return;
      } break;
      case 3: {
        threeCmtBolus(A, Alast, &t, b1, kel, k12, k21, k13, k31);
      } break;
      }
    }
  }
}

#include "parTrans.h"

void linCmtPar1(double *v, double *k,
                double *vss,
                double *cl,
                double *A,
                double *Af,
                double *alpha,
                double *t12alpha) {
  *vss = *v;
  *cl = (*v)*(*k);
  *A = 1/(*v);
  *alpha = (*k);
  *t12alpha = M_LN2/(*k);
  *Af = (*A)*(*v); // Always 1.
}

void linCmtPar2(double *v, double *k,
                double *k12, double *k21,
                double *vp, double *vss,
                double *cl, double *q,
                double *A, double *B,
                double *Af, double *Bf,
                double *alpha, double *beta,
                double *t12alpha, double *t12beta){
  *vp = (*v)*(*k12)/(*k21);
  *vss = (*v)+(*vp);
  *cl = (*v)*(*k);
  *q  = (*v)*(*k12);
  double a0 = (*k) * (*k21);
  double a1 = -((*k) + (*k12) + (*k21));
  double sq = sqrt(a1*a1-4*a0);
  *alpha = 0.5*(-a1+sq);
  *beta = 0.5*(-a1-sq);
  *A = ((*k21)-(*alpha))/((*beta)-(*alpha))/(*v);
  *B = ((*k21)-(*beta))/((*alpha)-(*beta))/(*v);
  *Af = (*A)*(*v);
  *Bf = (*B)*(*v);
  *t12alpha = M_LN2/(*alpha);
  *t12beta = M_LN2/(*beta);
}

void linCmtPar3(double *v, double *k10,
                double *k12, double *k21, double *k13, double *k31,
                double *vp, double *vp2, double *vss,
                double *cl, double *q, double *q2,
                double *A, double *B, double *C,
                double *Af, double *Bf, double *Cf,
                double *alpha, double *beta, double *gamma,
                double *t12alpha, double *t12beta, double *t12gamma) {
  double a0 = (*k10) * (*k21) * (*k31);
  double a1 = ((*k10) * (*k31)) + ((*k21) * (*k31)) + ((*k21) * (*k13)) + ((*k10) * (*k21)) + ((*k31) * (*k12));
  double a2 = (*k10) + (*k12) + (*k13) + (*k21) + (*k31);
  double p   = a1 - (a2 * a2 / 3.0);
  double qq   = (2.0 * a2 * a2 * a2 / 27.0) - (a1 * a2 / 3.0) + a0;
  double r1  = sqrt(-(p * p * p)/27.0);
  double phi = acos((-qq/2)/r1)/3.0;
  double r2  = 2.0 * exp(log(r1)/3.0);
  *alpha = -(cos(phi) * r2 - a2/3.0);
  *beta = -(cos(phi + 2.0 * M_PI/3.0) * r2 - a2/3.0);
  *gamma = -(cos(phi + 4.0 * M_PI/3.0) * r2 - a2/3.0);
  double a;
  if ((*alpha) < (*beta)) {
    a      = *beta;
    *beta  = *alpha;
    *alpha = a;
  } // now alpha >= beta
  if ((*beta) < (*gamma)) {
    a      = *beta;
    *beta  = *gamma;
    *gamma = a;
  } // now beta >= gamma
  if ((*alpha) < (*beta)) {
    a      = *alpha;
    *alpha = *beta;
    *beta  = a;
  } // now alpha >= beta >= gamma
  *A = ((*k21) - (*alpha)) * ((*k31) - (*alpha)) / ((*alpha) - (*beta)) / ((*alpha) - (*gamma))/(*v);
  *B = ((*k21) - (*beta)) * ((*k31) - (*beta)) / ((*beta) - (*alpha)) / ((*beta) - (*gamma))/(*v);
  *C = ((*k21) - (*gamma)) * ((*k31) - (*gamma)) / ((*gamma) - (*beta)) / ((*gamma) - (*alpha))/(*v);
  *vp  = (*v) * (*k12)/(*k21);
  *vp2 = (*v) * (*k13)/(*k31);
  *vss = (*v) + (*vp) + (*vp2);
  *cl  = (*v) * (*k10);
  *q   = (*v) * (*k12);
  *q2  = (*v) * (*k13);
  *Af  = (*A) * (*v);
  *Bf  = (*B) * (*v);
  *Cf  = (*C) * (*v);
  *t12alpha = M_LN2/(*alpha);
  *t12beta  = M_LN2/(*beta);
  *t12gamma = M_LN2/(*gamma);
}

SEXP toReal(SEXP in){
  int type = TYPEOF(in);
  if (type == REALSXP) return in;
  if (type == INTSXP) {
    SEXP ret = PROTECT(Rf_allocVector(REALSXP, Rf_length(in)));
    int *inI = INTEGER(in);
    double *retR = REAL(ret);
    for (int i = Rf_length(in); i--;){
      retR[i] = (double)(inI[i]);
    }
    UNPROTECT(1);
    return ret;
  }
  Rf_errorcall(R_NilValue, _("not an integer/real"));
  return R_NilValue;
}

SEXP derived1(int trans, SEXP inp, double dig) {
  double zer = 0;
  int lenP = Rf_length(VECTOR_ELT(inp, 0));
  int pro=0;
  SEXP tmp = PROTECT(toReal(VECTOR_ELT(inp, 0))); pro++;
  double *p1 = REAL(tmp);
  int lenV = Rf_length(VECTOR_ELT(inp, 1));
  tmp = PROTECT(toReal(VECTOR_ELT(inp, 1))); pro++;
  double *v1 = REAL(tmp);
  int lenOut = lenP;
  if (lenV != lenP){
    if (lenP == 1){
      lenOut = lenV;
    } else if (lenV != 1){
      Rf_errorcall(R_NilValue, _("The dimensions of the parameters must match"));
    }
  }
  // vc, kel, vss, cl, thalf, alpha, A, fracA
  SEXP ret  = PROTECT(Rf_allocVector(VECSXP, 8)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 8)); pro++;

  SET_STRING_ELT(retN,0,mkChar("vc"));
  SEXP vcS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vc = REAL(vcS);
  SET_VECTOR_ELT(ret, 0, vcS);

  SET_STRING_ELT(retN,1,mkChar("kel"));
  SEXP kelS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *kel = REAL(kelS);
  SET_VECTOR_ELT(ret, 1, kelS);

  SET_STRING_ELT(retN,2,mkChar("vss"));
  SEXP vssS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vss = REAL(vssS);
  SET_VECTOR_ELT(ret, 2, vssS);

  SET_STRING_ELT(retN,3,mkChar("cl"));
  SEXP clS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *cl = REAL(clS);
  SET_VECTOR_ELT(ret, 3, clS);

  SET_STRING_ELT(retN,4,mkChar("t12alpha"));
  SEXP thalfS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalf = REAL(thalfS);
  SET_VECTOR_ELT(ret, 4, thalfS);

  SET_STRING_ELT(retN,5,mkChar("alpha"));
  SEXP alphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *alpha = REAL(alphaS);
  SET_VECTOR_ELT(ret, 5, alphaS);

  SET_STRING_ELT(retN,6,mkChar("A"));
  SEXP AS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *A = REAL(AS);
  SET_VECTOR_ELT(ret, 6, AS);

  SET_STRING_ELT(retN,7,mkChar("fracA"));
  SEXP fracAS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracA = REAL(fracAS);
  SET_VECTOR_ELT(ret, 7, fracAS);

  SEXP sexp_class = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,mkChar("data.frame"));
  Rf_setAttrib(ret, R_ClassSymbol, sexp_class);

  SEXP sexp_rownames = PROTECT(Rf_allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -lenOut;
  Rf_setAttrib(ret, R_RowNamesSymbol, sexp_rownames);

  Rf_setAttrib(ret, R_NamesSymbol, retN);

  unsigned int ncmta=0;

  for (int i = lenOut; i--;){
    parTrans(&trans, ((lenP == 1) ? p1 : p1++),
             ((lenV == 1) ? v1 : v1++), &zer, &zer, &zer, &zer,
             &ncmta, kel, vc, &zer, &zer, &zer, &zer);
    linCmtPar1(vc, kel, vss, cl, A, fracA, alpha, thalf);
    if (dig > 0){
      (*vc) = fprec((*vc), dig);
      (*kel) = fprec((*kel), dig);
      (*vss) = fprec((*vss), dig);
      (*cl) = fprec((*cl), dig);
      (*A) = fprec((*A), dig);
      (*alpha) = fprec((*alpha), dig);
      (*thalf) = fprec((*thalf), dig);
    }
    vc++; kel++; vss++; cl++; A++; fracA++; alpha++; thalf++;

  }
  UNPROTECT(pro);
  return ret;
}

SEXP derived2(int trans, SEXP inp, double dig) {
  double zer = 0;
  int pro=0;

  SEXP tmp = PROTECT(toReal(VECTOR_ELT(inp, 0))); pro++;
  int lenP1 = Rf_length(tmp);
  double *p1 = REAL(tmp);

  tmp = PROTECT(toReal(VECTOR_ELT(inp, 1))); pro++;
  int lenV = Rf_length(tmp);
  double *v1 = REAL(tmp);

  tmp = PROTECT(toReal(VECTOR_ELT(inp, 2))); pro++;
  int lenP2 = Rf_length(tmp);
  double *p2 = REAL(tmp);

  tmp = PROTECT(toReal(VECTOR_ELT(inp, 3))); pro++;
  int lenP3 = Rf_length(tmp);
  double *p3 = REAL(tmp);

  int lenOut = max2(lenV, lenP1);
  lenOut = max2(lenOut, lenP2);
  lenOut = max2(lenOut, lenP3);
  lenOut = max2(lenOut, lenV);
  if (lenOut != 1) {
    if ((lenP1 != 1 && lenP1 != lenOut) ||
        (lenP2 != 1 && lenP2 != lenOut) ||
        (lenP3 != 1 && lenP3 != lenOut) ||
        (lenV != 1  && lenV != lenOut)) {
      Rf_errorcall(R_NilValue, _("The dimensions of the parameters must match"));
    }
  }
  // vc, kel, k12, k21, vp, vss, cl, q, thalfAlpha, thalfBeta,
  // alpha, beta, A, B, fracA, fracB
  SEXP ret  = PROTECT(Rf_allocVector(VECSXP, 16)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 16)); pro++;

  SET_STRING_ELT(retN,0,mkChar("vc"));
  SEXP vcS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vc = REAL(vcS);
  SET_VECTOR_ELT(ret, 0, vcS);

  SET_STRING_ELT(retN,1,mkChar("kel"));
  SEXP kelS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *kel = REAL(kelS);
  SET_VECTOR_ELT(ret, 1, kelS);

  SET_STRING_ELT(retN,2,mkChar("k12"));
  SEXP k12S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k12 = REAL(k12S);
  SET_VECTOR_ELT(ret, 2, k12S);

  SET_STRING_ELT(retN,3,mkChar("k21"));
  SEXP k21S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k21 = REAL(k21S);
  SET_VECTOR_ELT(ret, 3, k21S);

  SET_STRING_ELT(retN,4,mkChar("vp"));
  SEXP vpS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vp = REAL(vpS);
  SET_VECTOR_ELT(ret, 4, vpS);

  SET_STRING_ELT(retN,5,mkChar("vss"));
  SEXP vssS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vss = REAL(vssS);
  SET_VECTOR_ELT(ret, 5, vssS);

  SET_STRING_ELT(retN,6,mkChar("cl"));
  SEXP clS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *cl = REAL(clS);
  SET_VECTOR_ELT(ret, 6, clS);

  SET_STRING_ELT(retN,7,mkChar("q"));
  SEXP qS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *q = REAL(qS);
  SET_VECTOR_ELT(ret, 7, qS);

  SET_STRING_ELT(retN,8,mkChar("t12alpha"));
  SEXP thalfAlphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfAlpha = REAL(thalfAlphaS);
  SET_VECTOR_ELT(ret, 8, thalfAlphaS);

  SET_STRING_ELT(retN,9,mkChar("t12beta"));
  SEXP thalfBetaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfBeta = REAL(thalfBetaS);
  SET_VECTOR_ELT(ret, 9, thalfBetaS);

  SET_STRING_ELT(retN,10,mkChar("alpha"));
  SEXP alphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *alpha = REAL(alphaS);
  SET_VECTOR_ELT(ret, 10, alphaS);

  SET_STRING_ELT(retN,11,mkChar("beta"));
  SEXP betaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *beta = REAL(betaS);
  SET_VECTOR_ELT(ret, 11, betaS);

  SET_STRING_ELT(retN,12,mkChar("A"));
  SEXP AS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *A = REAL(AS);
  SET_VECTOR_ELT(ret, 12, AS);

  SET_STRING_ELT(retN,13,mkChar("B"));
  SEXP BS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *B = REAL(BS);
  SET_VECTOR_ELT(ret, 13, BS);

  SET_STRING_ELT(retN,14,mkChar("fracA"));
  SEXP fracAS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracA = REAL(fracAS);
  SET_VECTOR_ELT(ret, 14, fracAS);

  SET_STRING_ELT(retN,15,mkChar("fracB"));
  SEXP fracBS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracB = REAL(fracBS);
  SET_VECTOR_ELT(ret, 15, fracBS);

  SEXP sexp_class = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,mkChar("data.frame"));
  Rf_setAttrib(ret, R_ClassSymbol, sexp_class);

  SEXP sexp_rownames = PROTECT(Rf_allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -lenOut;
  Rf_setAttrib(ret, R_RowNamesSymbol, sexp_rownames);

  Rf_setAttrib(ret, R_NamesSymbol, retN);

  unsigned int ncmta=0;

  for (int i = lenOut; i--;){
    parTrans(&trans, ((lenP1 == 1) ? p1 : p1++), ((lenV == 1) ? v1 : v1++),
             ((lenP2 == 1) ? p2 : p2++), ((lenP3 == 1) ? p3 : p3++), &zer, &zer,
             &ncmta, kel, vc, k12, k21, &zer, &zer);
    linCmtPar2(vc, kel, k12, k21, vp, vss, cl, q, A, B, fracA, fracB,
               alpha, beta, thalfAlpha, thalfBeta);
    if (dig > 0){
      (*vc) = fprec((*vc), dig);
      (*kel) = fprec((*kel), dig);
      (*k12) = fprec((*k12), dig);
      (*k21) = fprec((*k21), dig);
      (*vp) = fprec((*vp), dig);
      (*vss) = fprec((*vss), dig);
      (*cl) = fprec((*cl), dig);
      (*q) = fprec((*q), dig);
      (*A) = fprec((*A), dig);
      (*B) = fprec((*B), dig);
      (*fracA) = fprec((*fracA), dig);
      (*fracB) = fprec((*fracB), dig);
      (*alpha) = fprec((*alpha), dig);
      (*beta) = fprec((*beta), dig);
      (*thalfAlpha) = fprec((*thalfAlpha), dig);
      (*thalfBeta) = fprec((*thalfBeta), dig);
    }
    vc++; kel++; k12++; k21++; vp++; vss++; cl++; q++;
    A++; B++; fracA++; fracB++; alpha++; beta++;
    thalfAlpha++; thalfBeta++;
  }
  UNPROTECT(pro);
  return ret;
}

SEXP derived3(int trans, SEXP inp, double dig) {
  int pro = 0;
  SEXP tmp = PROTECT(toReal(VECTOR_ELT(inp, 0))); pro++;
  int lenP1 = Rf_length(tmp);
  double *p1 = REAL(tmp);

  tmp = PROTECT(toReal(VECTOR_ELT(inp, 1))); pro++;
  int lenV = Rf_length(tmp);
  double *v1 = REAL(tmp);

  tmp = PROTECT(toReal(VECTOR_ELT(inp, 2))); pro++;
  int lenP2 = Rf_length(tmp);
  double *p2 = REAL(tmp);

  tmp = PROTECT(toReal(VECTOR_ELT(inp, 3))); pro++;
  int lenP3 = Rf_length(tmp);
  double *p3 = REAL(tmp);

  tmp = PROTECT(toReal(VECTOR_ELT(inp, 4))); pro++;
  int lenP4 = Rf_length(tmp);
  double *p4 = REAL(tmp);

  tmp = PROTECT(toReal(VECTOR_ELT(inp, 5))); pro++;
  int lenP5 = Rf_length(tmp);
  double *p5 = REAL(tmp);

  int lenOut = max2(lenV, lenP1);
  lenOut = max2(lenOut, lenP2);
  lenOut = max2(lenOut, lenP3);
  lenOut = max2(lenOut, lenV);
  lenOut = max2(lenOut, lenP4);
  lenOut = max2(lenOut, lenP5);
  if (lenOut != 1) {
    if ((lenP1 != 1 && lenP1 != lenOut) ||
        (lenP2 != 1 && lenP2 != lenOut) ||
        (lenP3 != 1 && lenP3 != lenOut) ||
        (lenP4 != 1 && lenP4 != lenOut) ||
        (lenP5 != 1 && lenP5 != lenOut) ||
        (lenV != 1  && lenV != lenOut)) {
      Rf_errorcall(R_NilValue, _("The dimensions of the parameters must match"));
    }
  }
  // vc, kel, k12, k21, vp, vss, cl, q, thalfAlpha, thalfBeta,
  // alpha, beta, A, B, fracA, fracB
  SEXP ret  = PROTECT(Rf_allocVector(VECSXP, 24)); pro++;
  SEXP retN = PROTECT(Rf_allocVector(STRSXP, 24)); pro++;

  SET_STRING_ELT(retN,0,mkChar("vc"));
  SEXP vcS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vc = REAL(vcS);
  SET_VECTOR_ELT(ret, 0, vcS);

  SET_STRING_ELT(retN,1,mkChar("kel"));
  SEXP kelS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *kel = REAL(kelS);
  SET_VECTOR_ELT(ret, 1, kelS);

  SET_STRING_ELT(retN,2,mkChar("k12"));
  SEXP k12S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k12 = REAL(k12S);
  SET_VECTOR_ELT(ret, 2, k12S);

  SET_STRING_ELT(retN,3,mkChar("k21"));
  SEXP k21S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k21 = REAL(k21S);
  SET_VECTOR_ELT(ret, 3, k21S);

  SET_STRING_ELT(retN,4,mkChar("k13"));
  SEXP k13S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k13 = REAL(k13S);
  SET_VECTOR_ELT(ret, 4, k13S);

  SET_STRING_ELT(retN,5,mkChar("k31"));
  SEXP k31S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k31 = REAL(k31S);
  SET_VECTOR_ELT(ret, 5, k31S);

  SET_STRING_ELT(retN,6,mkChar("vp"));
  SEXP vpS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vp = REAL(vpS);
  SET_VECTOR_ELT(ret, 6, vpS);

  SET_STRING_ELT(retN,7,mkChar("vp2"));
  SEXP vp2S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vp2 = REAL(vp2S);
  SET_VECTOR_ELT(ret, 7, vp2S);

  SET_STRING_ELT(retN,8,mkChar("vss"));
  SEXP vssS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vss = REAL(vssS);
  SET_VECTOR_ELT(ret, 8, vssS);

  SET_STRING_ELT(retN,9,mkChar("cl"));
  SEXP clS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *cl = REAL(clS);
  SET_VECTOR_ELT(ret, 9, clS);

  SET_STRING_ELT(retN,10,mkChar("q"));
  SEXP qS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *q = REAL(qS);
  SET_VECTOR_ELT(ret, 10, qS);

  SET_STRING_ELT(retN,11,mkChar("q2"));
  SEXP q2S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *q2 = REAL(q2S);
  SET_VECTOR_ELT(ret, 11, q2S);

  SET_STRING_ELT(retN,12,mkChar("t12alpha"));
  SEXP thalfAlphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfAlpha = REAL(thalfAlphaS);
  SET_VECTOR_ELT(ret, 12, thalfAlphaS);

  SET_STRING_ELT(retN,13,mkChar("t12beta"));
  SEXP thalfBetaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfBeta = REAL(thalfBetaS);
  SET_VECTOR_ELT(ret, 13, thalfBetaS);

  SET_STRING_ELT(retN,14,mkChar("t12gamma"));
  SEXP thalfGammaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfGamma = REAL(thalfGammaS);
  SET_VECTOR_ELT(ret, 14, thalfGammaS);

  SET_STRING_ELT(retN,15,mkChar("alpha"));
  SEXP alphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *alpha = REAL(alphaS);
  SET_VECTOR_ELT(ret, 15, alphaS);

  SET_STRING_ELT(retN,16,mkChar("beta"));
  SEXP betaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *beta = REAL(betaS);
  SET_VECTOR_ELT(ret, 16, betaS);

  SET_STRING_ELT(retN,17,mkChar("gamma"));
  SEXP gammaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *gamma = REAL(gammaS);
  SET_VECTOR_ELT(ret, 17, gammaS);

  SET_STRING_ELT(retN,18,mkChar("A"));
  SEXP AS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *A = REAL(AS);
  SET_VECTOR_ELT(ret, 18, AS);


  SET_STRING_ELT(retN,19,mkChar("B"));
  SEXP BS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *B = REAL(BS);
  SET_VECTOR_ELT(ret, 19, BS);

  SET_STRING_ELT(retN,20,mkChar("C"));
  SEXP CS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *C = REAL(CS);
  SET_VECTOR_ELT(ret, 20, CS);

  SET_STRING_ELT(retN,21,mkChar("fracA"));
  SEXP fracAS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracA = REAL(fracAS);
  SET_VECTOR_ELT(ret, 21, fracAS);

  SET_STRING_ELT(retN,22,mkChar("fracB"));
  SEXP fracBS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracB = REAL(fracBS);
  SET_VECTOR_ELT(ret, 22, fracBS);

  SET_STRING_ELT(retN,23,mkChar("fracC"));
  SEXP fracCS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracC = REAL(fracCS);
  SET_VECTOR_ELT(ret, 23, fracCS);

  SEXP sexp_class = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,mkChar("data.frame"));
  Rf_setAttrib(ret, R_ClassSymbol, sexp_class);

  SEXP sexp_rownames = PROTECT(Rf_allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -lenOut;
  Rf_setAttrib(ret, R_RowNamesSymbol, sexp_rownames);

  Rf_setAttrib(ret, R_NamesSymbol, retN);

  unsigned int ncmta=0;

  for (int i = lenOut; i--;){
    parTrans(&trans, ((lenP1 == 1) ? p1 : p1++), ((lenV == 1) ? v1 : v1++),
             ((lenP2 == 1) ? p2 : p2++), ((lenP3 == 1) ? p3 : p3++),
             ((lenP4 == 1) ? p4 : p4++), ((lenP5 == 1) ? p5 : p5++),
             &ncmta, kel, vc, k12, k21, k13, k31);
    linCmtPar3(vc, kel, k12, k21, k13, k31,
               vp, vp2, vss, cl, q, q2,
               A, B, C, fracA, fracB, fracC, alpha, beta, gamma,
               thalfAlpha, thalfBeta, thalfGamma);
    if (dig > 0) {
      (*vc)  = fprec((*vc), dig);
      (*kel) = fprec((*kel), dig);
      (*k12) = fprec((*k12), dig);
      (*k21) = fprec((*k21), dig);
      (*k13) = fprec((*k13), dig);
      (*k31) = fprec((*k31), dig);
      (*vp)  = fprec((*vp), dig);
      (*vss) = fprec((*vss), dig);
      (*vp2) = fprec((*vp2), dig);
      (*cl)  = fprec((*cl), dig);
      (*q)   = fprec((*q), dig);
      (*q2)  = fprec((*q2), dig);
      (*A)   = fprec((*A), dig);
      (*B)   = fprec((*B), dig);
      (*C)   = fprec((*C), dig);
      (*fracA)=fprec((*fracA), dig);
      (*fracB)=fprec((*fracB), dig);
      (*fracC)=fprec((*fracC), dig);
      (*alpha)=fprec((*alpha), dig);
      (*beta) =fprec((*beta), dig);
      (*gamma)=fprec((*gamma), dig);
      (*thalfAlpha)=fprec((*thalfAlpha), dig);
      (*thalfBeta)=fprec((*thalfBeta), dig);
      (*thalfGamma)=fprec((*thalfGamma), dig);
    }
    vc++; kel++; k12++; k21++; k13++; k31++;
    vp++; vp2++; vss++; cl++; q++; q2++;
    A++; B++; C++; fracA++; fracB++; fracC++; alpha++; beta++; gamma++;
    thalfAlpha++; thalfBeta++; thalfGamma++;
  }
  UNPROTECT(pro);
  return ret;
}

SEXP _calcDerived(SEXP ncmtSXP, SEXP transSXP, SEXP inp, SEXP sigdigSXP) {
  int tInp = TYPEOF(inp);
  int trans=-1;
  if (TYPEOF(transSXP) == REALSXP){
    trans = (int)(REAL(transSXP)[0]);
  }
  int ncmt=-1;
  if (TYPEOF(ncmtSXP) == REALSXP) {
    ncmt = (int)(REAL(ncmtSXP)[0]);
  }
  double dig=0.0;
  int tDig = TYPEOF(sigdigSXP);
  if (tDig == INTSXP) {
    dig = (double)(INTEGER(sigdigSXP)[0]);
  } else if (tDig == REALSXP) {
    dig = REAL(sigdigSXP)[0];
  }
  if (tInp == VECSXP){
    switch (ncmt){
    case 1:
      return derived1(trans, inp, dig);
      break;
    case 2:
      return derived2(trans, inp, dig);
      break;
    case 3:
      return derived3(trans, inp, dig);
      break;
    default:
      _rxode2parse_unprotect();
      Rf_errorcall(R_NilValue, _("'ncmt' needs to be 1-3"));
    }
  } else {
    _rxode2parse_unprotect();
    Rf_errorcall(R_NilValue, _("'inp' needs to be list/data frame"));
  }
  return R_NilValue;
}

static inline void ssRateTauD(double *A, int ncmt, int oral0, double *tinf,
                              double *tau, double *r1, double *r2, double *ka,
                              double *kel, double *k12, double *k21, double *k13, double *k31) {
  if (oral0){
    if ((*r1) > 0 ){
      switch (ncmt){
      case 1: {
        oneCmtKaRateSStr1(A, tinf, tau, r1, ka, kel);
        return;
      } break;
        /* case 2: { */
        /*  twoCmtKaRateSStr1D(A, tinf, tau, r1, ka, kel, k12, k21); */
        /*  return; */
        /* } break; */
        /* case 3: { */
        /*  threeCmtKaRateSStr1D(A, tinf, tau, r1, ka, kel, k12, k21, k13, k31); */
        /*  return; */
        /* } break; */
      }
    } else {
      switch (ncmt){
      case 1: {
        oneCmtKaRateSStr2D(A, tinf, tau, r2, ka, kel);
        return;
      } break;
        /* case 2: { */
        /*  twoCmtKaRateSStr2D(A, tinf, tau, r2, ka, kel, k12, k21); */
        /*  return; */
        /* } break; */
        /* case 3: { */
        /*  threeCmtKaRateSStr2D(A, tinf, tau, r2, ka, kel, k12, k21, k13, k31); */
        /*  return; */
        /* } break; */
      }
    }
  } else {
    switch (ncmt){
    case 1: {
      oneCmtRateSSD(A, tinf, tau, r1, kel);
      return;
    } break;
      /* case 2: { */
      /*   twoCmtRateSSD(A, tinf, tau, r1, kel, k12, k21); */
      /*   return; */
      /* } break; */
      /* case 3: { */
      /*   threeCmtRateSSD(A, tinf, tau, r1, kel, k12, k21, k13, k31); */
      /*   return; */
      /* } break; */
    }
  }
}

static inline void ssTauD(double *A,
                          int ncmt,
                          int oral0,
                          double *tau,
                          double *b1,
                          double *b2,
                          double *ka, // ka (for oral doses)
                          double *kel,  //double rx_v,
                          double *k12, double *k21, double *k13, double *k31){
  if (oral0){
    if ((*b1) > 0 ){
      switch (ncmt){
      case 1: {
        oneCmtKaSSb1D(A, tau, b1, ka, kel);
        return;
      } break;
        /* case 2: { */
        /*  twoCmtKaSSb1D(A, tau, b1, ka, kel, k12, k21); */
        /*  return; */
        /* } break; */
        /* case 3: { */
        /*  threeCmtKaSSb1D(A, tau, b1, ka, kel, k12, k21, k13, k31); */
        /*  return; */
        /* } break; */
      }
    } else {
      switch (ncmt){
      case 1: {
        oneCmtKaSSb2D(A, tau, b2, ka, kel);
        return;
      } break;
        /* case 2: { */
        /*  twoCmtKaSSb2D(A, tau, b2, ka, kel, k12, k21); */
        /*  return; */
        /* } break; */
        /* case 3: { */
        /*  threeCmtKaSSb2D(A, tau, b2, ka, kel, k12, k21, k13, k31); */
        /*  return; */
        /* } break; */
      }
    }
  } else {
    switch (ncmt){
    case 1: {
      oneCmtBolusSSD(A, tau, b1, kel);
      return;
    } break;
      /* case 2: { */
      /*   twoCmtBolusSSD(A, tau, b1, kel, k12, k21); */
      /*   return; */
      /* } break; */
      /* case 3: { */
      /*   threeCmtBolusSSD(A, tau, b1, kel, k12, k21, k13, k31); */
      /*   return; */
      /* } break; */
    }
  }
}

static inline void ssRateD(double *A,
                           int ncmt, // Number of compartments
                           int oral0, // Indicator of if this is an oral system)
                           double *r1, // Rate in Compartment #1
                           double *r2, // Rate in Compartment #2
                           double *ka, // ka (for oral doses)
                           double *kel,  //double rx_v,
                           double *k12, double *k21,
                           double *k13, double *k31) {
  if (oral0){
    if ((*r1) > 0){
      switch (ncmt){
      case 1: {
        oneCmtKaRateSSr1D(A, r1, ka, kel);
        return;
      } break;
        /* case 2: { */
        /*  twoCmtKaRateSSr1D(A, r1, ka, kel, k12, k21); */
        /*  return; */
        /* } break; */
        /* case 3: { */
        /*  threeCmtKaRateSSr1D(A, r1, ka, kel, k12, k21, k13, k31); */
        /*  return; */
        /* } */
      }
    } else {
      switch (ncmt){
      case 1: {
        oneCmtKaRateSSr2D(A, r2, ka, kel);
        return;
      } break;
        /* case 2: { */
        /*  twoCmtKaRateSSr2D(A, r2, ka, kel, k12, k21); */
        /*  return; */
        /* } break; */
        /* case 3: { */
        /*  threeCmtKaRateSSr2D(A, r2, ka, kel, k12, k21, k13, k31); */
        /*  return; */
        /* } break; */
      }
    }
  } else {
    switch (ncmt){
    case 1: {
      oneCmtRateSSr1D(A, r1, kel);
      return;
    } break;
      /* case 2: { */
      /*   twoCmtRateSSr1D(A, r1, kel, k12, k21); */
      /*   return; */
      /* } break; */
      /* case 3: { */
      /*   threeCmtRateSSr1D(A, r1, kel, k12, k21, k13, k31); */
      /*   return; */
      /* } break; */
    }
  }
}

static inline void handleSSL(double *A,// Amounts
                             double *Alast, // Last amounts
                             double tlast, // Time of last amounts
                             double ct, // Time of the dose
                             int ncmt, // Number of compartments
                             int oral0, // Indicator of if this is an oral system
                             double *b1, // Amount of the dose in compartment #1
                             double *b2, // Amount of the dose in compartment #2
                             double *r1, // Rate in Compartment #1
                             double *r2, // Rate in Compartment #2
                             double *ka, // ka (for oral doses)
                             double *kel,  //double rx_v,
                             double *k12, double *k21,
                             double *k13, double *k31,
                             int *linCmt,
                             double *d_F, double *d_F2,

                             double *d_rate1, double *d_rate2,
                             double *d_dur1, double *d_dur2,
                             double *aSave, int *nSave,
                             bool doDiff,
                             rx_solving_options_ind* ind) {
  // handle_evid has been called, so ind->wh0 and like have already been called
  double *rate = ind->linCmtRate;
  // note ind->ixds has already advanced
  double amt = getDoseNumber(ind, ind->ixds-1);
  switch(ind->wh0){
  case 40: { // Steady state constant infusion
    // Already advanced ind->ixds
    rate[0] =*r1  = *r2 = 0;
    if (oral0){
      rate[1] = 0;
    }
    int cmtOff = ind->cmt-*linCmt;
    if (cmtOff == 0){
      // Infusion to central compartment with oral dosing
      *r1 = amt;
    } else {
      // Infusion to central compartment or depot
      *r2 = amt;
    }
    if (doDiff){
      ssRateD(A, ncmt, oral0, r1, r2,
              ka, kel, k12, k21, k13, k31);
    } else {
      ssRate(A, ncmt, oral0, r1, r2,
             ka, kel, k12, k21, k13, k31);
    }
    //
  } break;
  case 20: // Steady state + last observed event
  case 10: { // Steady state
    double tau = getIiNumber(ind, ind->ixds-1);
    rate[0] =*r1  = *r2 = 0;
    if (oral0){
      rate[1] = 0;
    }
    int cmtOff = ind->cmt-*linCmt;
    //*tlast=0; // is this necessary
    switch (ind->whI) {
    case 0: { // bolus dose
      if (cmtOff == 0){
        *b1 = amt*(*d_F);
        *b2 = 0;
      } else {
        *b1 = 0;
        *b2 = amt*(*d_F2);
      }
      if (doDiff){
        ssTauD(A, ncmt, oral0, &tau, b1, b2, ka,
               kel, k12, k21, k13, k31);
      } else {
        ssTau(A, ncmt, oral0, &tau, b1, b2, ka,
              kel, k12, k21, k13, k31);
      }
    } break;
    case 8: // Duration is modeled
    case 9: { // Rate is modeled
      double tinf;
      if (ind->whI == 9) {
        if (cmtOff == 0){
          // Infusion to central compartment with oral dosing
          *r1 = *d_rate1;
          tinf = amt*(*r1)/(*d_F);
          rate[0] = *r1;
        } else {
          // Infusion to central compartment or depot
          *r2 = *d_rate2;
          tinf = amt*(*r2)/(*d_F2);
          rate[1] = *r2;
        }
      } else {
        // duration is modeled
        if (cmtOff == 0) {
          // With oral dosing infusion to central compartment
          tinf = *d_dur1;
          *r1 = amt/tinf*(*d_F);
          rate[0] = *r1;
        } else {
          // Infusion to compartment #1 or depot
          tinf = *d_dur2;
          *r2 = amt/tinf*(*d_F2);
          rate[1] = *r2;
        }
      }
      if (tinf >= tau){
        ind->wrongSSDur=1;
        for (int i = ncmt + oral0; i--;){
          A[i] += R_NaN;
        }
      } else {
        if (doDiff) {
          ssRateTauD(A, ncmt, oral0, &tinf, &tau,
                     r1, r2, ka, kel, k12, k21, k13, k31);
        } else {
          ssRateTau(A, ncmt, oral0, &tinf, &tau,
                    r1, r2, ka, kel, k12, k21, k13, k31);
        }
      }
    } break;
    case 1: // Infusion rate is fixed
    case 2: { // Infusion, duration is fixed
      double tinf;
      if (ISNA(amt)){
      } else if (amt > 0) {
        unsigned int p;
        tinf = _getDur(ind->ixds-1, ind, 1, &p);
        if (ind->whI == 1){
          // Duration changes with F
          if (cmtOff == 0){
            tinf *= (*d_F);
            *r1 = amt;
            rate[0] = *r1;
          } else {
            tinf *= (*d_F2);
            *r2 = amt;
            rate[1] = *r2;
          }
        } else {
          // rate changes with F; tinf remains constant
          if (cmtOff == 0){
            *r1 = amt*(*d_F);
            rate[0] = *r1;
          } else {
            *r2 = amt*(*d_F2);
            rate[1] = *r2;
          }
        }
        if (tinf >= tau){
          ind->wrongSSDur=1;
          for (int i = ncmt + oral0; i--;){
            A[i] += R_NaN;
          }
        } else {
          if (doDiff){
            ssRateTauD(A, ncmt, oral0, &tinf, &tau,
                       r1, r2, ka, kel, k12, k21, k13, k31);
          } else {
            ssRateTau(A, ncmt, oral0, &tinf, &tau,
                      r1, r2, ka, kel, k12, k21, k13, k31);
          }
        }
      }
    } break;
    }
    // Add back saved values
    if (ind->wh0 == 20) {
      if (amt > 0) {
        for (int i = *nSave; i--;){
          A[i] += aSave[i];
        }
      }
    }
  } break;
  }
}

double linCmtA(rx_solve *rx, unsigned int id, double _t, int linCmt,
               int i_cmt, int trans,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_F, double d_rate1, double d_dur1,
               // Oral parameters
               double d_ka, double d_tlag2, double d_F2,  double d_rate2, double d_dur2) {
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  double t = _t - ind->curShift;
  int evid = 0;
  int idx = ind->idx;
  double Alast0[4] = {0, 0, 0, 0};
  rx_solving_options *op = rx->op;
  int oral0;
  oral0 = (d_ka > 0) ? 1 : 0;
  double *A;
  double *Alast;
  /* A = Alast0; Alast=Alast0; */
  double tlast;
  unsigned int ncmt = 1;
  double rx_k=0, rx_v=0;
  double rx_k12=0;
  double rx_k21=0;
  double rx_k13=0;
  double rx_k31=0;
  double *rate = ind->linCmtRate;
  double b1=0, b2=0, r1 = 0, r2 = 0;
  double curTime = getTime(ind->ix[idx], ind);
  int sameTime = isSameTime(t, curTime);
  if (sameTime && idx <= ind->solved){
    // Pull from last solved value (cached)
    A = getAdvan(idx);
    if (A[oral0] == 0.0) {
      ind->solved = max2(idx-1, 0);
    } else {
      if (trans == 10) {
        return(A[oral0]*(v1+p3+p5));
      } else {
        return(A[oral0]/v1);
      }
    }
  }
  while (t < curTime) {
    idx--;
    if (idx < 0) {
      return NA_REAL;
    }
    curTime = getTime(ind->ix[idx], ind);
  }
  A = getAdvan(idx);
  if (!parTrans(&trans, &p1, &v1, &p2, &p3, &p4, &p5,
                &ncmt, &rx_k, &rx_v, &rx_k12,
                &rx_k21, &rx_k13, &rx_k31)){
    return NA_REAL;
  }
  if (idx >= ind->solved){
    // Not saved, solve
    if (idx == 0) {
      Alast = Alast0;
      tlast = getTime(ind->ix[0], ind);
    } else {
      tlast = getTime(ind->ix[idx-1], ind);
      Alast = getAdvan(idx-1);
    }
    evid = getEvid(ind, ind->ix[idx]);
    if (op->nlinR == 2){
      r1 = rate[0];
      r2 = rate[1];
    } else {
      r1 = rate[0];
    }
    if (evid == 3){
      // Reset event
      Alast=Alast0;
    } else {
      doAdvan(A, Alast, tlast, // Time of last amounts
              curTime, ncmt, oral0, &b1, &b2, &r1, &r2,
              &d_ka, &rx_k, &rx_k12, &rx_k21,
              &rx_k13, &rx_k31);
      double aSave[4] = {0.0, 0.0, 0.0, 0.0};
      int nSave = ncmt + oral0;
      for (int i = nSave; i--;){
        aSave[i] = A[i];
      }
      if (handle_evidL(evid, A, curTime, id, ind)){
        handleSSL(A, Alast, tlast, curTime, ncmt, oral0,
                  &b1, &b2, &r1, &r2, &d_ka, &rx_k,
                  &rx_k12, &rx_k21, &rx_k13, &rx_k31,
                  &linCmt, &d_F, &d_F2, &d_rate1, &d_rate2,
                  &d_dur1, &d_dur2, aSave, &nSave, false, ind);
      }
    }
  }
  sameTime = isSameTime(t, curTime);
  if (!sameTime){
    // Compute the advan solution of a t outside of the mesh.
    Alast = A;
    double Acur[4] = {0, 0, 0, 0};
    tlast = curTime;
    curTime = t;
    b1 = b2 = 0;
    // FIXME adjust rates
    doAdvan(Acur, Alast, tlast, // Time of last amounts
            curTime, ncmt, oral0, &b1, &b2, &r1, &r2,
            &d_ka, &rx_k, &rx_k12, &rx_k21,
            &rx_k13, &rx_k31);
    return Acur[oral0]/rx_v;
  }
  return A[oral0]/rx_v;
}


double linCmtC(rx_solve *rx, unsigned int id, double _t, int linCmt,
               int i_cmt, int trans,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_F, double d_rate1, double d_dur1,
               // Oral parameters
               double d_ka, double d_tlag2, double d_F2,  double d_rate2, double d_dur2) {
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  double t = _t - ind->curShift;
  rx_solving_options *op = rx->op;
  double *rate = ind->linCmtRate;
  int oldIdx = ind->idx;
  int oldIxds = ind->ixds;
  double or1 = 0.0, or2=0.0;
  if (op->nlinR == 2){
    or1 = rate[0];
    or2 = rate[1];
  } else {
    or1 = rate[0];
  }
  int evid;
  int idxF = ind->idx;
  double Alast[4] = {0, 0, 0, 0};
  double A[4]     = {0, 0, 0, 0};
  int oral0;
  oral0 = (d_ka > 0) ? 1 : 0;
  double it = getTime(ind->ix[idxF], ind);
  double curTime, tlast;
  curTime = tlast = getTime(ind->ix[0], ind); // t0
  if (t != it) {
    // Try to get another idx by bisection
    idxF = _locateTimeIndex(t, ind);
    it = getTime(ind->ix[idxF], ind);
  }
  unsigned int ncmt = 1;
  double rx_k=0, rx_v=0;
  double rx_k12=0;
  double rx_k21=0;
  double rx_k13=0;
  double rx_k31=0;
  double b1=0, b2=0, r1 = 0, r2 = 0;
  if (!parTrans(&trans, &p1, &v1, &p2, &p3, &p4, &p5,
                &ncmt, &rx_k, &rx_v, &rx_k12,
                &rx_k21, &rx_k13, &rx_k31)){
    return NA_REAL;
  } else {
    ind->ixds = 0;
    int nSave = ncmt + oral0;
    double aSave[4] = {0.0, 0.0, 0.0, 0.0};
    if (op->nlinR == 2){
      rate[0] = rate[1] = 0;
    } else {
      rate[0] = 0;
    }
    for (int idx = 0; idx <= idxF; idx++) {
      ind->idx = idx;
      curTime = getTime(ind->ix[idx], ind);
      evid = getEvid(ind, ind->ix[idx]);
      if (op->nlinR == 2){
        r1 = rate[0];
        r2 = rate[1];
      } else {
        r1 = rate[0];
      }
      ind->wh0 = 0;
      if (isObs(evid)){
        if (idx != idxF){
          // Don't calculate observations (unless it is the current observation)
          continue;
        }
      } else if (evid == 3){
        // Reset event
        for (int i = nSave; i--;){
          Alast[i] = 0.0;
        }
      }
      doAdvan(A, Alast, tlast, // Time of last amounts
              curTime, ncmt, oral0, &b1, &b2, &r1, &r2,
              &d_ka, &rx_k, &rx_k12, &rx_k21,
              &rx_k13, &rx_k31);
      for (int i = nSave; i--;){
        aSave[i] = A[i];
      }
      if (handle_evidL(evid, A, curTime, id, ind)){
        handleSSL(A, Alast, tlast, curTime, ncmt, oral0,
                  &b1, &b2, &r1, &r2, &d_ka, &rx_k,
                  &rx_k12, &rx_k21, &rx_k13, &rx_k31,
                  &linCmt, &d_F, &d_F2, &d_rate1, &d_rate2,
                  &d_dur1, &d_dur2, aSave, &nSave, false, ind);
      }
      for (int i = nSave; i--;){
        Alast[i] = A[i];
      }
      b1=b2=r1=r2=0;
      tlast = curTime;
    }
  }
  ind->ixds = oldIxds;
  ind->idx = oldIdx;
  if (op->nlinR == 2){
    rate[0] = or1;
    rate[1] = or2;
  } else {
    rate[0] = or1;
  }
  return A[oral0]/rx_v;
}

double linCmtD(rx_solve *rx, unsigned int id, double t, int linCmt,
               int i_cmt, int trans, int val,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_F, double d_rate1, double d_dur1,
               // Oral parameters
               double d_ka, double d_tlag2, double d_F2,  double d_rate2, double d_dur2) {
  // Forward difference
  double v0 = linCmtA(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2,  d_rate2, d_dur2);
#define h 1.4901161193847656e-08
#define h2 67108864
  switch (val){
  case 0:
    return v0;
  case 1:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1 + h, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2)- v0);
  case 2:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1 + h,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 3:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2 + h, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 4:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3 + h, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 5:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4 + h, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 6:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5 + h, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 7:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag + h, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 8:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F + h, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 9:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1 + h, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 10:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1 + h,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 11:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka + h, d_tlag2, d_F2,  d_rate2, d_dur2) - v0);
  case 12:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2 + h, d_F2,  d_rate2, d_dur2) - v0);
  case 13:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2  + h,  d_rate2, d_dur2) - v0);
  case 14:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2 + h, d_dur2) - v0);
  case 15:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2 + h) - v0);
  default:
    _rxode2parse_unprotect();
    Rf_errorcall(R_NilValue, "undef diff");
  }
#undef h
#undef h2
}

double linCmtE(rx_solve *rx, unsigned int id, double t, int linCmt,
               int i_cmt, int trans, int val,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_F, double d_rate1, double d_dur1,
               // Oral parameters
               double d_ka, double d_tlag2, double d_F2,  double d_rate2, double d_dur2) {
  // Central difference
#define h 7.4505805969238281e-09
#define h2 134217728
  switch (val){
  case 0:
    return linCmtA(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                   p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                   d_ka, d_tlag2, d_F2,  d_rate2, d_dur2);
  case 1:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1 + h, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1 - h, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 2:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1 + h,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1 - h,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 3:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2 + h, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2 - h, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 4:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3 + h, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3 - h, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 5:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4 + h, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4 - h, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 6:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5 + h, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5 - h, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 7:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag + h, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag - h, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 8:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F + h, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F - h, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 9:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1 + h, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1 - h, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 10:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1 + h,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1 - h,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 11:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka + h, d_tlag2, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka - h, d_tlag2, d_F2,  d_rate2, d_dur2));
  case 12:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2 + h, d_F2,  d_rate2, d_dur2) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2 - h, d_F2,  d_rate2, d_dur2));
  case 13:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2  + h,  d_rate2, d_dur2) +
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2 - h,  d_rate2, d_dur2));
  case 14:
    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2 + h, d_dur2) +
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2 - h, d_dur2));
  case 15:

    return h2*(linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2 + h) -
               linCmtC(rx, id, t, linCmt, i_cmt, trans, p1, v1,
                       p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                       d_ka, d_tlag2, d_F2,  d_rate2, d_dur2 - h));
  default:
    _rxode2parse_unprotect();
    Rf_errorcall(R_NilValue, "undef diff");
  }
#undef h
#undef h2
}

static inline void doAdvanD(double *A,// Amounts
                            double *Alast, // Last amounts
                            double tlast, // Time of last amounts
                            double ct, // Time of the dose
                            int ncmt, // Number of compartments
                            int oral0, // Indicator of if this is an oral system
                            double *b1, // Amount of the dose in compartment #1
                            double *b2, // Amount of the dose in compartment #2
                            double *r1, // Rate in Compartment #1
                            double *r2, // Rate in Compartment #2
                            double *ka, // ka (for oral doses)
                            double *kel,  //double rx_v,
                            double *k12, double *k21, double *k13, double *k31){
  double t = ct - tlast;
  if (oral0){
    switch (ncmt){
    case 1: {
      oneCmtKaRateD(A, Alast, &t, b1, b2, r1, r2, ka, kel);
      return;
    } break;
      /* case 2: { */
      /*    twoCmtKaRateD(A, Alast, &t, b1, b2, r1, r2, */
      /*              ka,  kel, k12, k21); */
      /*    return; */
      /* } break; */
      /* case 3: { */
      /*    threeCmtKaRateD(A, Alast, &t, b1, b2, r1, r2, */
      /*            ka,  kel, k12, k21, k13, k31); */
      /*    return; */
      /* } break; */
    }
  } else {
    switch (ncmt){
    case 1: {
      oneCmtRateD(A, Alast, &t, b1, r1, kel);
      return;
    } break;
      /* case 2: { */
      /*    twoCmtRateD(A, Alast, &t, b1, r1, */
      /*            kel, k12, k21); */
      /*    return; */
      /* } break; */
      /* case 3: { */
      /*    threeCmtRateD(A, Alast, &t, b1, r1, */
      /*              kel, k12, k21, k13, k31); */
      /*    return; */
      /* } break; */
    }
  }
}

double derTrans(rx_solve *rx, double *A, int ncmt, int trans, int val,
                double p1, double v1,
                double p2, double p3,
                double p4, double p5,
                double d_tlag, double d_F, double d_rate1, double d_dur1,
                // Oral parameters
                double d_ka, double d_tlag2, double d_F2,  double d_rate2, double d_dur2) {
  int oral0 = (d_ka > 0) ? 1 : 0;
  if (val == 0){
    // Apply trans
    if (trans == 10) {
      return(A[oral0]*(v1+p3+p5));
    } else {
      return(A[oral0]/v1);
    }
  }
  switch (ncmt) {
  case 2: // 2 compartment model
    if (val == 11) {
      // d/dt(ka)
      if (trans == 10) {
        return A[6]*v1;
      } else {
        return A[6]/v1;
      }
    }
    switch (trans){
    case 1: // cl=(*p1) v=(*v1) q=(*p2) vp=(*p3)
      switch (val) {
      case 1: // cl
        return A[2 + oral0 * 5]/(v1 * v1);
      case 2: // v
        return -A[oral0]/(v1 * v1) +
          (-p1 * A[2 + oral0 * 5]/(v1 * v1) -
           p2 * A[3 + oral0 * 5]/(v1 *v1))/v1;
      case 3: // q
        return (A[4 + oral0 * 5]/p3 + A[3 + oral0 * 5]/v1)/v1;
      case 4: // vp
        return -p2 * A[4 + oral0 * 5]/(p3 * p3 * v1);
      }
    case 2: // Direct translation
      switch (val) {
      case 1: // k
        // A1k10 A[2]
        // A2k20 A[7]
        return A[2+oral0*5]/v1;
      case 2: // v
        return -A[oral0]/(v1*v1);
      case 3: // k12
        return A[3+oral0*5]/v1;
      case 4: // k21
        return A[4+oral0*5]/v1;
      }
    case 3: // cl=(*p1) v=(*v1) q=(*p2) vss=(*p3)
      switch (val) {
      case 1: // cl
        return A[2 + oral0 * 5]/(v1 * v1);
      case 2: // v
        return -A[oral0]/(v1 * v1) + (-p1 * A[2 + oral0 * 5]/(v1 * v1) -
                                      p2 * A[3 + oral0 * 5]/(v1 * v1) +
                                      p2 * A[4 + oral0 * 5]/((p3 - v1) * (p3 - v1)))/v1;
      case 3: // q
        return (A[3 + oral0 * 5]/v1 + A[4 + oral0 * 5]/(p3 - v1))/v1;
      case 4: // vss
        return -p2 * A[4 + oral0 * 5]/((p3 - v1) * (p3 - v1) * v1);
      }
    case 4: // alpha=(*p1) beta=(*p2) k21=(*p3)
      switch (val) {
      case 1: // alpha
        return ((1 - p2/p3) * A[3 + oral0 * 5] + p2 * A[2 + oral0 * 5]/p3)/v1;
      case 2: // v
        return -A[oral0]/(v1 * v1);
      case 3: // beta
        return ((1 - p1/p3) * A[3 + oral0 * 5] + p1 * A[2 + oral0 * 5]/p3)/v1;
      case 4: // k21
        return (A[3 + oral0 * 5] * (-1 + p2 * p1/(p3 * p3)) -
                p2 * p1 * A[2 + oral0 * 5]/(p3 * p3) +
                A[4 + oral0 * 5])/v1;
      }
    case 5: // alpha=(*p1) beta=(*p2) aob=(*p3)
      switch (val) {
      case 1: // alpha
        return (A[4 + oral0 * 5]/(1 + p3) + (p2 * (1 + p3)/(p1 + p2 * p3) -
                                             p2 * p1 * (1 + p3)/((p1 + p2 * p3) * (p1 + p2 * p3))) * A[2 + oral0 * 5] +
                A[3 + oral0 * 5] * (1 - p2 * (1 + p3)/(p1 + p2 * p3) +
                                    p2 * p1 * (1 + p3)/((p1 + p2 * p3) * (p1 + p2 * p3)) -
                                    (1/(1 + p3))))/v1;
      case 2: // v
        return -A[oral0]/(v1 * v1);
      case 3: // beta
        return ((p1 * (1 + p3)/(p1 + p2 * p3) -
                 p2 * p1 * p3 * (1 + p3)/((p1 + p2 * p3) * (p1 + p2 * p3))) * A[2 + oral0 * 5] +
                (1 - p3/(1 + p3) - p1 * (1 + p3)/(p1 + p2 * p3) +
                 p2 * p1 * p3 * (1 + p3)/((p1 + p2 * p3) * (p1 + p2 * p3))) * A[3 + oral0 * 5] +
                p3 * A[4 + oral0 * 5]/(1 + p3))/v1;
      case 4: // aob
        return ((p2/(1 + p3) - (p1 + p2 * p3)/((1 + p3) * (1 + p3))) * A[4 + oral0 * 5] +
                A[2 + oral0 * 5] * (p2 * p1/(p1 + p2 * p3) -
                                    (p2*p2) * p1 * (1 + p3)/((p1 + p2 * p3) * (p1 + p2 * p3))) +
                A[3 + oral0 * 5] * (-p2/(1 + p3) + (p1 + p2 * p3)/((1 + p3) * (1 + p3)) -
                                    p2 * p1/(p1 + p2 * p3) +
                                    (p2 * p2) * p1 * (1 + p3)/((p1 + p2 * p3) * (p1 + p2 * p3))))/v1;
      }
    case 11: // A2 V, alpha=(*p1), beta=(*p2), k21
      switch (val) {
      case 1: // alpha
        return (A[2 + oral0 * 5] * ( p2 * (p3 +  1.0/v1)/(p1 * p3 +  p2/v1) -  p2 * p1 * p3 * (p3 +  1.0/v1)/(((p1 * p3 +  p2/v1)) * ((p1 * p3 +  p2/v1)))) + A[3 + oral0 * 5] * (1 -  p3/(p3 +  1.0/v1) -  p2 * (p3 +  1.0/v1)/(p1 * p3 +  p2/v1) +  p2 * p1 * p3 * (p3 +  1.0/v1)/(((p1 * p3 +  p2/v1)) * ((p1 * p3 +  p2/v1)))) +  p3 * A[4 + oral0 * 5]/(p3 +  1.0/v1))/v1;
      case 2: // v
        return -A[oral0]/(v1*v1) + (A[2 + oral0 * 5] * (- p2 * p1/((v1*v1) * (p1 * p3 +  p2/v1)) +  (p2*p2) * p1 * (p3 +  1.0/v1)/((v1*v1) * (((p1 * p3 +  p2/v1)) * ((p1 * p3 +  p2/v1))))) + A[3 + oral0 * 5] * (  p2/((v1*v1) * (p3 +  1.0/v1)) -  (p1 * p3 +  p2/v1)/((v1*v1) * (((p3 +  1.0/v1)) * ((p3 +  1.0/v1)))) +  p2 * p1/((v1*v1) * (p1 * p3 +  p2/v1)) -  (p2*p2) * p1 * (p3 +  1.0/v1)/((v1*v1) *      (((p1 * p3 +  p2/v1)) * ((p1 * p3 +  p2/v1))))) + A[4 + oral0 * 5] * (- p2/((v1*v1) * (p3 +  1.0/v1)) +  (p1 * p3 +  p2/v1)/((v1*v1) * (((p3 +  1.0/v1)) * ((p3 +  1.0/v1))))))/v1;
      case 3: // beta
        return (A[2 + oral0 * 5] * ( p1 * (p3 +  1.0/v1)/(p1 * p3 +  p2/v1) -  p2 * p1 * (p3 +  1.0/v1)/(v1 * (((p1 * p3 +  p2/v1)) * ((p1 * p3 +  p2/v1))))) + A[3 + oral0 * 5] * (1 -  1/(v1 * (p3 +  1.0/v1)) -  p1 * (p3 +  1.0/v1)/(p1 * p3 +  p2/v1) +  p2 * p1 * (p3 +  1.0/v1)/(v1 * (((p1 * p3 +  p2/v1)) * ((p1 * p3 +  p2/v1))))) +  A[4 + oral0 * 5]/(v1 * (p3 +  1.0/v1)))/v1;
      case 4: // k21
        return ((0 -  p1/(p3 +  1.0/v1) +  (p1 * p3 +  p2/v1)/(((p3 +  1.0/v1)) * ((p3 +  1.0/v1))) -  p2 * p1/(p1 * p3 +  p2/v1) +  p2 * ((p1) * (p1)) * (p3 +  1.0/v1)/(((p1 * p3 +  p2/v1)) * ((p1 * p3 +  p2/v1)))) * A[3 + oral0 * 5] + A[2 + oral0 * 5] * (p2 * p1/(p1 * p3 +  p2/v1) -  p2 * ((p1) * (p1)) * (p3 +  1.0/v1)/(((p1 * p3 +  p2/v1)) * ((p1 * p3 +  p2/v1)))) + A[4 + oral0 * 5] * ( p1/(p3 + 1.0/v1) -  (p1 * p3 +  p2/v1)/((p3 + 1.0/v1) * (p3 +  1.0/v1))))/v1;
      }
    case 10: // A=(*v1), alpha=(*p1), beta=(*p2), B=(*p3)
      switch (val) {
      case 1: // alpha
        return (( (p3 + v1) * p2/(p1 * p3 + p2 * v1) -  (p3 + v1) * p2 * p1 * p3/(((p1 * p3 + p2 * v1)) * ((p1 * p3 + p2 * v1)))) * A[2 + oral0 * 5] + A[3 + oral0 * 5] * (1 -  p3/(p3 + v1) -  (p3 + v1) * p2/(p1 * p3 + p2 * v1) +  (p3 + v1) * p2 * p1 * p3/(((p1 * p3 + p2 * v1)) * ((p1 * p3 + p2 * v1)))) +  p3 * A[4 + oral0 * 5]/(p3 + v1))/v1;
      case 2: // A
        return -A[oral0]/(v1*v1) + (( p2 * p1/(p1 * p3 + p2 * v1) -  (p3 + v1) * (p2*p2) * p1/(((p1 * p3 + p2 * v1)) * ((p1 * p3 + p2 * v1)))) * A[2 + oral0 * 5] + A[3 + oral0 * 5] * (0 -  p2/(p3 + v1) +  (p1 * p3 + p2 * v1)/(((p3 + v1)) * ((p3 + v1))) -  p2 * p1/(p1 * p3 + p2 * v1) +  (p3 + v1) * (p2*p2) * p1/(((p1 * p3 + p2 * v1)) * ((p1 * p3 + p2 * v1)))) + A[4 + oral0 * 5] * ( p2/(p3 + v1) -  (p1 * p3 + p2 * v1)/(((p3 + v1)) * ((p3 + v1)))))/v1;
      case 3: // beta
        return (( (p3 + v1) * p1/(p1 * p3 + p2 * v1) -  (p3 + v1) * p2 * p1 * v1/(((p1 * p3 + p2 * v1)) * ((p1 * p3 + p2 * v1)))) * A[2 + oral0 * 5] + A[3 + oral0 * 5] * (1 -  v1/(p3 + v1) -  (p3 + v1) * p1/(p1 * p3 + p2 * v1) +  (p3 + v1) * p2 * p1 * v1/(((p1 * p3 + p2 * v1)) * ((p1 * p3 + p2 * v1)))) +  v1 * A[4 + oral0 * 5]/(p3 + v1))/v1;
      case 4: // B
        return (( p2 * p1/(p1 * p3 + p2 * v1) -  (p3 + v1) * p2 * ((p1) * (p1))/(((p1 * p3 + p2 * v1)) * ((p1 * p3 + p2 * v1)))) * A[2 + oral0 * 5] + A[3 + oral0 * 5] * (0 -  p1/(p3 + v1) +  (p1 * p3 + p2 * v1)/(((p3 + v1)) * ((p3 + v1))) -  p2 * p1/(p1 * p3 + p2 * v1) +  (p3 + v1) * p2 * ((p1) * (p1))/(((p1 * p3 + p2 * v1)) * ((p1 * p3 + p2 * v1)))) + A[4 + oral0 * 5] * ( p1/(p3 + v1) -  (p1 * p3 + p2 * v1)/(((p3 + v1)) * ((p3 + v1)))))/v1;
      }
    }
  case 1: // One compartment model
    if (val == 11) {
      // d/dt(ka)
      if (trans == 10) {
        return A[3]*v1;
      } else {
        return A[3]/v1;
      }
    }
    switch (trans){
    case 1: // cl v
      switch (val){
      case 1: // d/dt(p1) = d/dt(cl) f = A[oral0]/v
        //A2k20 = A[4]
        //A1k10 = A[1]
        return A[oral0*3 + 1]/(v1*v1);
      case 2: // d/dt(v)
        return -A[oral0]/(v1*v1) - p1*A[oral0*3+1]/(v1*v1*v1);
      }
    case 2: // k V
      switch (val) {
      case 1: // d/dt(p1) = d/dt(kel)
        //A2k20 = A[4]
        //A1k10 = A[1]
        return A[oral0*3 + 1]/(v1);
      case 2: // d/dt(v)
        return -A[oral0]/(v1*v1);
      }
    case 11: // alpha V
      switch (val) {
      case 1: // d/dt(p1) = d/dt(alpha)  //alpha=kel
        return A[oral0*3 + 1]/(v1);
      case 2: // d/dt(v)
        return -A[oral0]/(v1*v1);
      }
    case 10: // alpha A
      switch (val) {
      case 1: // d/dt(alpha)
        return A[oral0*3+1]*v1;
      case 2: // d/dt(A)
        return A[oral0];
      }
    } break;
  }
  // Get the extra derivatives
  rx_solving_options *op = rx->op;
  if (op->nlin != op->nlin2) {
    int cur = op->nlin2;
    if (op->linBflag & 64) {  // tlag 64= bitwShiftL(1, 7-1)
      if (val == 7) return A[cur];
      cur++;
    }
    if (op->linBflag & 128) { // f 128 = 1 << 8-1
      if (val == 8) return A[cur];
      cur++;
    }
    if (op->linBflag & 256) {  // rate 256 = 1 << 9-1
      if (val == 9) return A[cur];
      cur++;
    }
    if (op->linBflag & 512) {  // dur 512 = 1 << 10-1
      if (val == 10) return A[cur];
      cur++;
    }
    if (op->linBflag & 2048) { // tlag2 2048 = 1 << 12 - 1
      if (val == 12) return A[cur];
      cur++;
    }
    if (op->linBflag & 4096) { // f2 4096 = 1 << 13 - 1
      if (val == 13) return A[cur];
      cur++;
    }
    if (op->linBflag & 8192) { // rate2 8192 = 1 << 14 - 1
      if (val == 14) return A[cur];
      cur++;
    }
    if (op->linBflag & 16384) { // dur2 16384 = 1 << 15 - 1
      if (val == 15) return A[cur];
      cur++;
    }
  }
  return R_NaN;
}

double linCmtF(rx_solve *rx, unsigned int id, double _t, int linCmt,
               int i_cmt, int trans, int val,
               double p1, double v1,
               double p2, double p3,
               double p4, double p5,
               double d_tlag, double d_F, double d_rate1, double d_dur1,
               // Oral parameters
               double d_ka, double d_tlag2, double d_F2,  double d_rate2, double d_dur2) {
  /* if (i_cmt == 3 || i_cmt == 2) */
  /*   return linCmtBB(rx, id, t, linCmt, i_cmt, trans, val, */
  /*            p1, v1, p2, p3, p4, p5, d_tlag, d_F, */
  /*            d_rate1, d_dur1, d_ka, d_tlag2, d_F2, d_rate2, d_dur2); */
  rx_solving_options_ind *ind = &(rx->subjects[id]);
  double t = _t - ind->curShift;
  int evid;
  int idx = ind->idx;
  double Alast0[15] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
  rx_solving_options *op = rx->op;
  int oral0= (d_ka > 0) ? 1 : 0;
  double *A;
  double *Alast;
  /* A = Alast0; Alast=Alast0; */
  double tlast;
  double curTime= getTime(ind->ix[idx], ind); // t0
  while (t < curTime) {
    idx--;
    if (idx < 0) return NA_REAL;
    curTime = getTime(ind->ix[idx], ind);
  }
  int sameTime = isSameTime(t, curTime);
  if (idx <= ind->solved && sameTime){
    // Pull from last solved value (cached)
    A = getAdvan(idx);
    if (A[oral0] == 0.0) {
      ind->solved = max2(idx-1, 0);
    } else {
      return derTrans(rx, A, i_cmt, trans, val, p1, v1, p2, p3,
                      p4, p5, d_tlag,  d_F,  d_rate1,  d_dur1,
                      d_ka, d_tlag2, d_F2, d_rate2, d_dur2);
    }
  }
  unsigned int ncmt = 1;
  double rx_k=0, rx_v=0;
  double rx_k12=0;
  double rx_k21=0;
  double rx_k13=0;
  double rx_k31=0;
  double *rate = ind->linCmtRate;
  double b1=0, b2=0, r1 = 0, r2 = 0;
  A = Alast0;
  if (idx <= ind->solved){
    if (!parTrans(&trans, &p1, &v1, &p2, &p3, &p4, &p5,
                  &ncmt, &rx_k, &rx_v, &rx_k12,
                  &rx_k21, &rx_k13, &rx_k31)){
      return NA_REAL;
    }
  } else {
    A = getAdvan(idx);
    if (idx == 0) {
      Alast = Alast0;
      tlast = getTime(ind->ix[0], ind);
    } else {
      tlast = getTime(ind->ix[idx-1], ind);
      Alast = getAdvan(idx-1);
    }
    curTime = getTime(ind->ix[idx], ind);
    if (!parTrans(&trans, &p1, &v1, &p2, &p3, &p4, &p5,
                  &ncmt, &rx_k, &rx_v, &rx_k12,
                  &rx_k21, &rx_k13, &rx_k31)){
      return NA_REAL;
    }
    evid = getEvid(ind, ind->ix[idx]);
    if (op->nlinR == 2){
      r1 = rate[0];
      r2 = rate[1];
    } else {
      r1 = rate[0];
    }
    ind->wh0 = 0;
    if (evid == 3){
      // Reset event
      Alast=Alast0;
    } else {
      doAdvanD(A, Alast, tlast, // Time of last amounts
               curTime, ncmt, oral0, &b1, &b2, &r1, &r2,
               &d_ka, &rx_k, &rx_k12, &rx_k21, &rx_k13, &rx_k31);
      double aSave[15];
      int nSave = (ncmt == 1 ? (oral0 ? 5 : 2) : (oral0 ? 15 : 8));
      for (int i = nSave; i--;){
        aSave[i] = A[i];
      }
      if (handle_evidL(evid, A, curTime, id, ind)){
        handleSSL(A, Alast, tlast, curTime, ncmt, oral0,
                  &b1, &b2, &r1, &r2, &d_ka, &rx_k,
                  &rx_k12, &rx_k21, &rx_k13, &rx_k31,
                  &linCmt, &d_F, &d_F2, &d_rate1, &d_rate2,
                  &d_dur1, &d_dur2, aSave, &nSave, true, ind);
      }
    }
  }
  double *Ac = A;
  if (!sameTime){
    // Compute the advan solution of a t outside of the mesh.
    Alast = A;
    Ac = Alast0;
    tlast = curTime;
    curTime = t;
    b1 = b2 = 0;
    doAdvanD(Ac, Alast, tlast, // Time of last amounts
             curTime, ncmt, oral0, &b1, &b2, &r1, &r2,
             &d_ka, &rx_k, &rx_k12, &rx_k21, &rx_k13, &rx_k31);
  }
  if (op->nlin2 == op->nlin) {
    return derTrans(rx, Ac, ncmt, trans, val, p1, v1, p2, p3,
                    p4, p5, d_tlag,  d_F,  d_rate1,  d_dur1,
                    d_ka, d_tlag2, d_F2, d_rate2, d_dur2);
  }
  double v0 = derTrans(rx, Ac, ncmt, trans, 0, p1, v1, p2, p3,
                       p4, p5, d_tlag,  d_F,  d_rate1,  d_dur1,
                       d_ka, d_tlag2, d_F2, d_rate2, d_dur2);
  int cur = op->nlin2;
  double curD;
  if ((op->linBflag & 64) && (sameTime || (!sameTime && val == 7))) { // tlag
    if (op->cTlag) {
      curD =(linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                     p2, p3, p4, p5, d_tlag + 0.5*op->hTlag,
                     d_F, d_rate1, d_dur1,
                     d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
             linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                     p2, p3, p4, p5, d_tlag - 0.5*op->hTlag,
                     d_F, d_rate1, d_dur1,
                     d_ka, d_tlag2, d_F2,  d_rate2, d_dur2))/op->hTlag;
    } else {
      curD =(linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                     p2, p3, p4, p5, d_tlag + op->hTlag, d_F,
                     d_rate1, d_dur1, d_ka, d_tlag2, d_F2,
                     d_rate2, d_dur2) - v0)/op->hTlag;
    }
    if (sameTime) A[cur++] = curD;
    else return curD;
  }
  if ((op->linBflag & 128) && (sameTime || (!sameTime && val == 8))) { // f
    if (op->cF) {
      double c1 = linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                          p2, p3, p4, p5, d_tlag, d_F + 0.5*op->hF,
                          d_rate1, d_dur1, d_ka, d_tlag2, d_F2,
                          d_rate2, d_dur2);
      double c2 = linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                          p2, p3, p4, p5, d_tlag, d_F - 0.5*op->hF,
                          d_rate1, d_dur1, d_ka, d_tlag2, d_F2,
                          d_rate2, d_dur2);
      curD =(c1 - c2)/op->hF;
    } else {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F + op->hF,
                      d_rate1, d_dur1, d_ka, d_tlag2, d_F2,
                      d_rate2, d_dur2) - v0)/op->hF;
    }
    if (sameTime) A[cur++] = curD;
    else return curD;
  }
  if ((op->linBflag & 256) && (sameTime || (!sameTime && val == 9))) { // rate
    if (op->cRate) {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F,
                      d_rate1 + 0.5*op->hRate, d_dur1,
                      d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
              linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F,
                      d_rate1 - 0.5*op->hRate, d_dur1,
                      d_ka, d_tlag2, d_F2,  d_rate2, d_dur2))/op->hRate;
    } else {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F,
                      d_rate1 + op->hRate, d_dur1,
                      d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0)/op->hRate;
    }
    if (sameTime) A[cur++] = curD;
    else return curD;
  }
  if ((op->linBflag & 512) && (sameTime || (!sameTime && val == 10))) { // dur
    if (op->cDur) {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1,
                      d_dur1 + 0.5*op->hDur,
                      d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) -
              linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1,
                      d_dur1 - 0.5*op->hDur,
                      d_ka, d_tlag2, d_F2,  d_rate2, d_dur2))/op->hDur;
    } else {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1,
                      d_dur1 + op->hDur,
                      d_ka, d_tlag2, d_F2,  d_rate2, d_dur2) - v0)/op->hDur;
    }
    if (sameTime) A[cur++] = curD;
    return curD;
  }
  if ((op->linBflag & 2048) && (sameTime && (!sameTime && val == 12))) { // tlag2
    if (op->cTlag2) {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2 + 0.5*op->hTlag2, d_F2,
                      d_rate2, d_dur2) -
              linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2 - 0.5*op->hTlag2, d_F2,
                      d_rate2, d_dur2))/op->hTlag2;
    } else {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2 + op->hTlag2, d_F2, d_rate2,
                      d_dur2) - v0)/op->hTlag2;
    }
    if (sameTime) A[cur++] = curD;
    return curD;
  }
  if ((op->linBflag & 4096) && (sameTime || (!sameTime && val == 13))) { // f2
    if (op->cF2) {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2 + 0.5*op->hF2,
                      d_rate2, d_dur2) -
              linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2 - 0.5*op->hF2,
                      d_rate2, d_dur2))/op->hF2;
    } else {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2 + op->hF2,
                      d_rate2, d_dur2) - v0)/op->hF2;
    }
    if (sameTime) A[cur++] = curD;
    return curD;
  }
  if ((op->linBflag & 8192) && (sameTime || (!sameTime && val == 14))) { // rate2
    if (op->cRate2){
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2,
                      d_rate2 + 0.5*op->hRate2, d_dur2) -
              linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2,
                      d_rate2 - 0.5*op->hRate2, d_dur2))/op->hRate2;
    } else {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2,
                      d_rate2 + op->hRate2, d_dur2) - v0)/op->hRate2;
    }
    if (sameTime) A[cur++] = curD;
    else return curD;
  }
  if ((op->linBflag & 16384) && (sameTime || (!sameTime && val == 15))) { // dur2
    if (op->cDur2){
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2,  d_rate2,
                      d_dur2 + 0.5*op->hDur2) -
              linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2,  d_rate2,
                      d_dur2 - 0.5*op->hDur2))/op->hDur2;
    } else {
      curD = (linCmtC(rx, id, t, linCmt, ncmt, trans, p1, v1,
                      p2, p3, p4, p5, d_tlag, d_F, d_rate1, d_dur1,
                      d_ka, d_tlag2, d_F2,  d_rate2,
                      d_dur2 + op->hDur2) - v0)/op->hDur2;
    }
    if (sameTime) A[cur++] = curD;
    return curD;
  }
  return derTrans(rx, Ac, ncmt, trans, val, p1, v1, p2, p3,
                  p4, p5, d_tlag,  d_F,  d_rate1,  d_dur1,
                  d_ka, d_tlag2, d_F2, d_rate2, d_dur2);
  return R_NaN;
}
