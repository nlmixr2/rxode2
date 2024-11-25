#ifndef __COMP_H__
#define __COMP_H__

#include "../inst/include/rxode2.h"
#include "solComp.h"

typedef struct lin_context_c_t {
  double *rate; // rate (depot + central or depot only)
  int ncmt;
  int oral0; // does the system have an oral dose? (0:central or 1:depot+central)
  // typical PK constants and v for volume of distribution
  double ka;
  double k10;
  double k12;
  double k21;
  double k13;
  double k31;
  double v;
  // dt -- delta t
  double dt;
} lin_context_c_t;

static inline void printSqMat(double *in, int d) {
  // debugging function to print a square matrix in R.
  int pro=0;
  SEXP mat = PROTECT(Rf_allocVector(REALSXP, d*d)); pro++;
  SEXP dm  = PROTECT(Rf_allocVector(INTSXP, 2)); pro++;
  int *dmi = INTEGER(dm);
  dmi[0] =dmi[1] = d;
  double *matd = REAL(mat);
  for (int i = d*d; i--;)  {
    matd[i] = in[i];
  }
  Rf_setAttrib(mat, R_DimSymbol, dm);
  Rf_PrintValue(mat);
  UNPROTECT(pro);
}

static inline void printVec(double *in, int d) {
  int pro=0;
  SEXP vec = PROTECT(Rf_allocVector(REALSXP, d)); pro++;
  double *vecd =REAL(vec);
  for (int i = d; i--;) {
    vecd[i] = in[i];
  }
  Rf_PrintValue(vec);
  UNPROTECT(pro);
}

// Handle single point solve
static inline int comp1solve1(double *yp, lin_context_c_t *lin) {
  double E  = exp(-(lin->k10)*lin->dt);
  double Ea = E;
  double pDepot = 0.0;
  double rDepot = 0.0;
  double R = lin->rate[lin->oral0];
  // In the derivation used in https://doi.org/10.12793/tcp.2019.27.2.43
  // the expression for the Ka contribution (Eq 9, Eq 13 and Eq 33) is given by
  //
  // Ka*Xg(0)*exp(-Ka*t)
  //
  // This is true as long as there is not an infusion in the depot
  //
  // When there is an infusion (Rg) in the depot the ka would be:
  //
  // Ka*[Kg(0)*exp(-Ka*t) + Rg/ka*(1-exp(-Ka*t))]
  //
  // as implied by equation #12 (which is the eq in the bracket)
  //
  // expanding this becomes:
  //
  // (Ka*Kg(0) - Rg)*exp(-Ka*t) + Rg
  //
  // Both Ka*Kg(0) and Ka*kg(0)-Rg in general are not dependent on
  // time.  Also Rg is simply a time invariant constant
  //
  // Which means to get equations where infusions into a depot are supported
  // You need to simply change 2 items:
  //
  // - in Eq 11, 32 and 41 you change Ka*Kg(0) to (Ka*Kg(0)- Rg)
  // - in Eq 11, 32 and 41 you change R to (R+Rg)
  //
  // This was observed after solving a few systems manually
  if (lin->oral0 == 1) {
    Ea = exp(-(lin->ka)*lin->dt);
    pDepot = yp[0];
    rDepot = lin->rate[0];
    R = rDepot + R;
  }
  yp[lin->oral0] = yp[lin->oral0]*E + R*(1.0-E)/(lin->k10);
  if (isSameTime(lin->ka, lin->k10)) {
    yp[lin->oral0] += (pDepot*(lin->k10)-rDepot)*lin->dt*E;
  } else {
    yp[lin->oral0] += (pDepot*(lin->ka)-rDepot)*(E-Ea)/((lin->ka)-(lin->k10));
  }
  if (lin->oral0) {
    yp[0] = rDepot*(1.0-Ea)/(lin->ka) + pDepot*Ea;
  }
  return 1;
}

// prior solving information, will be updated with new information
// (like lsoda and the like)
static inline int comp1solve2(double *yp, lin_context_c_t *lin) {
  double L[2], C1[4], C2[4], E[2], Ea[2], Xo[2], Rm[2];
  double rDepot=0.0;
  double R=lin->rate[lin->oral0];
  //  double dT = lin->dt;
  if (solComp2C(&(lin->k10), &(lin->k12), &(lin->k21), L, C1, C2) == 0) {
    return 0;
  }
  Xo[0] = Xo[1] = 0.0;
  E[0] = Ea[0] = exp(-L[0]*lin->dt);
  E[1] = Ea[1] = exp(-L[1]*lin->dt);
  const double one = 1.0;
  const int ione = 1,
    itwo = 2;
  //Xo = Xo + pX[1 + j] * Co[, , j] %*% E # Bolus
  F77_CALL(dgemm)("N", "N", &itwo, &ione, &itwo, &(yp[lin->oral0]), C1, &itwo, E,
                  &itwo, &one, Xo, &itwo FCONE FCONE);
  F77_CALL(dgemm)("N", "N", &itwo, &ione, &itwo, &(yp[lin->oral0+1]), C2, &itwo, E,
                  &itwo, &one, Xo, &itwo FCONE FCONE);
  if (lin->oral0 == 1 && yp[0] > 0.0) {
    // Xo = Xo + Ka*pX[1]*(Co[, , 1] %*% ((E - Ea)/(Ka - L)))
    rDepot = lin->rate[0];
    R += rDepot;
    double expa = exp(-(lin->ka)*lin->dt);
    Ea[0] = (E[0]- expa)/((lin->ka)-L[0]);
    Ea[1] = (E[1]- expa)/((lin->ka)-L[1]);
    double cf = (lin->ka)*yp[0] - rDepot;
    F77_CALL(dgemm)("N", "N", &itwo, &ione, &itwo, &cf, C1, &itwo, Ea, &itwo, &one, Xo, &itwo FCONE FCONE);
    yp[0] = rDepot*(1.0-expa)/(lin->ka) + yp[0]*expa;
  }
  if (!isSameTime(R, 0.0)) {
    // Xo = Xo + ((cR*Co[, , 1]) %*% ((1 - E)/L)) # Infusion
    Rm[0] = (1.0 - E[0])/L[0];
    Rm[1] = (1.0 - E[1])/L[1];
    F77_CALL(dgemm)("N", "N", &itwo, &ione, &itwo, &(R),
                    C1, &itwo, Rm, &itwo, &one, Xo, &itwo FCONE FCONE);
  }
  yp[lin->oral0] = Xo[0];
  yp[lin->oral0+1] = Xo[1];
  return 1;
}

static inline int comp1solve3(double *yp, lin_context_c_t *lin) {
  double L[3], C1[9], C2[9], C3[9], E[3], Ea[3], Xo[3], Rm[3];
  double R = lin->rate[lin->oral0];
  double rDepot = 0.0;
  if (solComp3C(&(lin->k10), &(lin->k12), &(lin->k21),
                &(lin->k13), &(lin->k31), L, C1, C2, C3) == 0) {
    return 0;
  }
  E[0] = Ea[0] = exp(-L[0]*lin->dt);
  E[1] = Ea[1] = exp(-L[1]*lin->dt);
  E[2] = Ea[2] = exp(-L[2]*lin->dt);
  const double one = 1.0;
  const int ione = 1, ithree=3;
  //Xo = Xo + pX[1 + j] * Co[, , j] %*% E # Bolus
  Xo[0]=Xo[1]=Xo[2]=0.0;
  F77_CALL(dgemm)("N", "N", &ithree, &ione, &ithree, &(yp[lin->oral0]), C1, &ithree,
                  E, &ithree, &one, Xo, &ithree FCONE FCONE);
  F77_CALL(dgemm)("N", "N", &ithree, &ione, &ithree, &(yp[lin->oral0+1]), C2, &ithree,
                  E, &ithree, &one, Xo, &ithree FCONE FCONE);
  F77_CALL(dgemm)("N", "N", &ithree, &ione, &ithree, &(yp[lin->oral0+2]), C3, &ithree,
                  E, &ithree, &one, Xo, &ithree FCONE FCONE);
  if (lin->oral0 == 1 && yp[0] > 0.0) {
    // Xo = Xo + Ka*pX[1]*(Co[, , 1] %*% ((E - Ea)/(Ka - L)))
    rDepot=lin->rate[0];
    R += rDepot;
    double expa = exp(-(lin->ka)*lin->dt);
    Ea[0] = (E[0]- expa)/((lin->ka) - L[0]);
    Ea[1] = (E[1]- expa)/((lin->ka) - L[1]);
    Ea[2] = (E[2]- expa)/((lin->ka) - L[2]);
    double expa2 = (lin->ka)*yp[0] - rDepot;
    F77_CALL(dgemm)("N", "N", &ithree, &ione, &ithree, &expa2, C1, &ithree,
                    Ea, &ithree, &one, Xo, &ithree FCONE FCONE);
    yp[0] = rDepot*(1.0-expa)/(lin->ka) + yp[0]*expa;
  }
  if (!isSameTime(R, 0.0)) {
    // Xo = Xo + ((cR*Co[, , 1]) %*% ((1 - E)/L)) # Infusion
    Rm[0] = (1.0 - E[0])/L[0];
    Rm[1] = (1.0 - E[1])/L[1];
    Rm[2] = (1.0 - E[2])/L[2];
    F77_CALL(dgemm)("N", "N", &ithree, &ione, &ithree, &(R), C1, &ithree,
                    Rm, &ithree, &one, Xo, &ithree FCONE FCONE);
  }
  yp[lin->oral0]   = Xo[0];
  yp[lin->oral0+1] = Xo[1];
  yp[lin->oral0/+2] = Xo[2];
  return 1;
}

static inline int comp1(double *yp, lin_context_c_t *lin) {
  switch (lin->ncmt) {
  case 3:
    return comp1solve3(yp, lin);
    break;
  case 2:
    return comp1solve2(yp, lin);
    break;
  case 1:
    return comp1solve1(yp, lin);
    break;
  }
  return 0;
}
#endif
