#ifndef NDEBUG
#define NDEBUG // just in case
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#include <stan/math.hpp>
#include "macros2micros.h"

#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern "C" void _rxode2parse_unprotect(void);

static inline void parTransPtr(int *transp,
                              double *p1, double *v1,
                              double *p2, double *p3,
                              double *p4, double *p5,
                              unsigned int *ncmtp,
                              double *rx_k, double *rx_v, double *rx_k12,
                              double *rx_k21, double *rx_k13, double *rx_k31) {
  int ncmt = ncmtp[0], trans=*transp;
  Eigen::Matrix<double, Eigen::Dynamic, 1> params(2*ncmt, 1);
  params(0, 0) = p1[0];
  params(1, 0) = v1[0];
  if (ncmt >=2) {
    params(2,0) = p2[0];
    params(3,0) = p3[0];
    if (ncmt >= 3) {
      params(4,0) = p4[0];
      params(5,0) = p5[0];
    }
  }
  Eigen::Matrix<double, Eigen::Dynamic, 2> g = stan::math::macros2micros(params, ncmt, trans);
  *rx_k = g(0,1);
  *rx_v = g(0,0);
  if (ncmt >=2) {
    *rx_k12 = g(1, 0);
    *rx_k21 = g(1, 1);
    if (ncmt >= 3) {
      *rx_k13 = g(2, 0);
      *rx_k31 = g(2, 1);
    }
  }
}

void linCmtPar1(double *v,
                double *k,
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

extern "C" SEXP derived1(int trans, SEXP inp, double dig) {
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

  SET_STRING_ELT(retN,0,Rf_mkChar("vc"));
  SEXP vcS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vc = REAL(vcS);
  SET_VECTOR_ELT(ret, 0, vcS);

  SET_STRING_ELT(retN,1,Rf_mkChar("kel"));
  SEXP kelS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *kel = REAL(kelS);
  SET_VECTOR_ELT(ret, 1, kelS);

  SET_STRING_ELT(retN,2,Rf_mkChar("vss"));
  SEXP vssS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vss = REAL(vssS);
  SET_VECTOR_ELT(ret, 2, vssS);

  SET_STRING_ELT(retN,3,Rf_mkChar("cl"));
  SEXP clS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *cl = REAL(clS);
  SET_VECTOR_ELT(ret, 3, clS);

  SET_STRING_ELT(retN,4,Rf_mkChar("t12alpha"));
  SEXP thalfS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalf = REAL(thalfS);
  SET_VECTOR_ELT(ret, 4, thalfS);

  SET_STRING_ELT(retN,5,Rf_mkChar("alpha"));
  SEXP alphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *alpha = REAL(alphaS);
  SET_VECTOR_ELT(ret, 5, alphaS);

  SET_STRING_ELT(retN,6,Rf_mkChar("A"));
  SEXP AS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *A = REAL(AS);
  SET_VECTOR_ELT(ret, 6, AS);

  SET_STRING_ELT(retN,7,Rf_mkChar("fracA"));
  SEXP fracAS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracA = REAL(fracAS);
  SET_VECTOR_ELT(ret, 7, fracAS);

  SEXP sexp_class = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,Rf_mkChar("data.frame"));
  Rf_setAttrib(ret, R_ClassSymbol, sexp_class);

  SEXP sexp_rownames = PROTECT(Rf_allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -lenOut;
  Rf_setAttrib(ret, R_RowNamesSymbol, sexp_rownames);

  Rf_setAttrib(ret, R_NamesSymbol, retN);

  unsigned int ncmta=1;

  for (int i = 0; i < lenOut; ++i) {
    parTransPtr(&trans,
                p1,
                v1,
                &zer,
                &zer,
                &zer,
                &zer,
                &ncmta,
                kel,
                vc,
                &zer,
                &zer,
                &zer,
                &zer);
    linCmtPar1(vc, kel, vss, cl, A, fracA, alpha, thalf);
    if (dig > 0){
      (*vc) = Rf_fprec((*vc), dig);
      (*kel) = Rf_fprec((*kel), dig);
      (*vss) = Rf_fprec((*vss), dig);
      (*cl) = Rf_fprec((*cl), dig);
      (*A) = Rf_fprec((*A), dig);
      (*alpha) = Rf_fprec((*alpha), dig);
      (*thalf) = Rf_fprec((*thalf), dig);
    }
    vc++; kel++; vss++; cl++; A++; fracA++; alpha++; thalf++;
    p1++; v1++;
  }
  UNPROTECT(pro);
  return ret;
}


extern "C" SEXP derived2(int trans, SEXP inp, double dig) {
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

  SET_STRING_ELT(retN,0,Rf_mkChar("vc"));
  SEXP vcS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vc = REAL(vcS);
  SET_VECTOR_ELT(ret, 0, vcS);

  SET_STRING_ELT(retN,1,Rf_mkChar("kel"));
  SEXP kelS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *kel = REAL(kelS);
  SET_VECTOR_ELT(ret, 1, kelS);

  SET_STRING_ELT(retN,2,Rf_mkChar("k12"));
  SEXP k12S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k12 = REAL(k12S);
  SET_VECTOR_ELT(ret, 2, k12S);

  SET_STRING_ELT(retN,3,Rf_mkChar("k21"));
  SEXP k21S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k21 = REAL(k21S);
  SET_VECTOR_ELT(ret, 3, k21S);

  SET_STRING_ELT(retN,4,Rf_mkChar("vp"));
  SEXP vpS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vp = REAL(vpS);
  SET_VECTOR_ELT(ret, 4, vpS);

  SET_STRING_ELT(retN,5,Rf_mkChar("vss"));
  SEXP vssS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vss = REAL(vssS);
  SET_VECTOR_ELT(ret, 5, vssS);

  SET_STRING_ELT(retN,6,Rf_mkChar("cl"));
  SEXP clS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *cl = REAL(clS);
  SET_VECTOR_ELT(ret, 6, clS);

  SET_STRING_ELT(retN,7,Rf_mkChar("q"));
  SEXP qS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *q = REAL(qS);
  SET_VECTOR_ELT(ret, 7, qS);

  SET_STRING_ELT(retN,8,Rf_mkChar("t12alpha"));
  SEXP thalfAlphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfAlpha = REAL(thalfAlphaS);
  SET_VECTOR_ELT(ret, 8, thalfAlphaS);

  SET_STRING_ELT(retN,9,Rf_mkChar("t12beta"));
  SEXP thalfBetaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfBeta = REAL(thalfBetaS);
  SET_VECTOR_ELT(ret, 9, thalfBetaS);

  SET_STRING_ELT(retN,10,Rf_mkChar("alpha"));
  SEXP alphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *alpha = REAL(alphaS);
  SET_VECTOR_ELT(ret, 10, alphaS);

  SET_STRING_ELT(retN,11,Rf_mkChar("beta"));
  SEXP betaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *beta = REAL(betaS);
  SET_VECTOR_ELT(ret, 11, betaS);

  SET_STRING_ELT(retN,12,Rf_mkChar("A"));
  SEXP AS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *A = REAL(AS);
  SET_VECTOR_ELT(ret, 12, AS);

  SET_STRING_ELT(retN,13,Rf_mkChar("B"));
  SEXP BS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *B = REAL(BS);
  SET_VECTOR_ELT(ret, 13, BS);

  SET_STRING_ELT(retN,14,Rf_mkChar("fracA"));
  SEXP fracAS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracA = REAL(fracAS);
  SET_VECTOR_ELT(ret, 14, fracAS);

  SET_STRING_ELT(retN,15,Rf_mkChar("fracB"));
  SEXP fracBS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracB = REAL(fracBS);
  SET_VECTOR_ELT(ret, 15, fracBS);

  SEXP sexp_class = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,Rf_mkChar("data.frame"));
  Rf_setAttrib(ret, R_ClassSymbol, sexp_class);

  SEXP sexp_rownames = PROTECT(Rf_allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -lenOut;
  Rf_setAttrib(ret, R_RowNamesSymbol, sexp_rownames);

  Rf_setAttrib(ret, R_NamesSymbol, retN);

  unsigned int ncmta=2;

  for (int i = lenOut; i--;){
    parTransPtr(&trans, ((lenP1 == 1) ? p1 : p1++), ((lenV == 1) ? v1 : v1++),
                ((lenP2 == 1) ? p2 : p2++), ((lenP3 == 1) ? p3 : p3++), &zer, &zer,
                &ncmta, kel, vc, k12, k21, &zer, &zer);
    linCmtPar2(vc, kel, k12, k21, vp, vss, cl, q, A, B, fracA, fracB,
               alpha, beta, thalfAlpha, thalfBeta);
    if (dig > 0){
      (*vc) = Rf_fprec((*vc), dig);
      (*kel) = Rf_fprec((*kel), dig);
      (*k12) = Rf_fprec((*k12), dig);
      (*k21) = Rf_fprec((*k21), dig);
      (*vp) = Rf_fprec((*vp), dig);
      (*vss) = Rf_fprec((*vss), dig);
      (*cl) = Rf_fprec((*cl), dig);
      (*q) = Rf_fprec((*q), dig);
      (*A) = Rf_fprec((*A), dig);
      (*B) = Rf_fprec((*B), dig);
      (*fracA) = Rf_fprec((*fracA), dig);
      (*fracB) = Rf_fprec((*fracB), dig);
      (*alpha) = Rf_fprec((*alpha), dig);
      (*beta) = Rf_fprec((*beta), dig);
      (*thalfAlpha) = Rf_fprec((*thalfAlpha), dig);
      (*thalfBeta) = Rf_fprec((*thalfBeta), dig);
    }
    vc++; kel++; k12++; k21++; vp++; vss++; cl++; q++;
    A++; B++; fracA++; fracB++; alpha++; beta++;
    thalfAlpha++; thalfBeta++;
  }
  UNPROTECT(pro);
  return ret;
}

extern "C" SEXP derived3(int trans, SEXP inp, double dig) {
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

  SET_STRING_ELT(retN,0,Rf_mkChar("vc"));
  SEXP vcS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vc = REAL(vcS);
  SET_VECTOR_ELT(ret, 0, vcS);

  SET_STRING_ELT(retN,1,Rf_mkChar("kel"));
  SEXP kelS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *kel = REAL(kelS);
  SET_VECTOR_ELT(ret, 1, kelS);

  SET_STRING_ELT(retN,2,Rf_mkChar("k12"));
  SEXP k12S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k12 = REAL(k12S);
  SET_VECTOR_ELT(ret, 2, k12S);

  SET_STRING_ELT(retN,3,Rf_mkChar("k21"));
  SEXP k21S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k21 = REAL(k21S);
  SET_VECTOR_ELT(ret, 3, k21S);

  SET_STRING_ELT(retN,4,Rf_mkChar("k13"));
  SEXP k13S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k13 = REAL(k13S);
  SET_VECTOR_ELT(ret, 4, k13S);

  SET_STRING_ELT(retN,5,Rf_mkChar("k31"));
  SEXP k31S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *k31 = REAL(k31S);
  SET_VECTOR_ELT(ret, 5, k31S);

  SET_STRING_ELT(retN,6,Rf_mkChar("vp"));
  SEXP vpS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vp = REAL(vpS);
  SET_VECTOR_ELT(ret, 6, vpS);

  SET_STRING_ELT(retN,7,Rf_mkChar("vp2"));
  SEXP vp2S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vp2 = REAL(vp2S);
  SET_VECTOR_ELT(ret, 7, vp2S);

  SET_STRING_ELT(retN,8,Rf_mkChar("vss"));
  SEXP vssS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *vss = REAL(vssS);
  SET_VECTOR_ELT(ret, 8, vssS);

  SET_STRING_ELT(retN,9,Rf_mkChar("cl"));
  SEXP clS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *cl = REAL(clS);
  SET_VECTOR_ELT(ret, 9, clS);

  SET_STRING_ELT(retN,10,Rf_mkChar("q"));
  SEXP qS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *q = REAL(qS);
  SET_VECTOR_ELT(ret, 10, qS);

  SET_STRING_ELT(retN,11,Rf_mkChar("q2"));
  SEXP q2S = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *q2 = REAL(q2S);
  SET_VECTOR_ELT(ret, 11, q2S);

  SET_STRING_ELT(retN,12,Rf_mkChar("t12alpha"));
  SEXP thalfAlphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfAlpha = REAL(thalfAlphaS);
  SET_VECTOR_ELT(ret, 12, thalfAlphaS);

  SET_STRING_ELT(retN,13,Rf_mkChar("t12beta"));
  SEXP thalfBetaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfBeta = REAL(thalfBetaS);
  SET_VECTOR_ELT(ret, 13, thalfBetaS);

  SET_STRING_ELT(retN,14,Rf_mkChar("t12gamma"));
  SEXP thalfGammaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *thalfGamma = REAL(thalfGammaS);
  SET_VECTOR_ELT(ret, 14, thalfGammaS);

  SET_STRING_ELT(retN,15,Rf_mkChar("alpha"));
  SEXP alphaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *alpha = REAL(alphaS);
  SET_VECTOR_ELT(ret, 15, alphaS);

  SET_STRING_ELT(retN,16,Rf_mkChar("beta"));
  SEXP betaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *beta = REAL(betaS);
  SET_VECTOR_ELT(ret, 16, betaS);

  SET_STRING_ELT(retN,17,Rf_mkChar("gamma"));
  SEXP gammaS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *gamma = REAL(gammaS);
  SET_VECTOR_ELT(ret, 17, gammaS);

  SET_STRING_ELT(retN,18,Rf_mkChar("A"));
  SEXP AS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *A = REAL(AS);
  SET_VECTOR_ELT(ret, 18, AS);


  SET_STRING_ELT(retN,19,Rf_mkChar("B"));
  SEXP BS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *B = REAL(BS);
  SET_VECTOR_ELT(ret, 19, BS);

  SET_STRING_ELT(retN,20,Rf_mkChar("C"));
  SEXP CS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *C = REAL(CS);
  SET_VECTOR_ELT(ret, 20, CS);

  SET_STRING_ELT(retN,21,Rf_mkChar("fracA"));
  SEXP fracAS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracA = REAL(fracAS);
  SET_VECTOR_ELT(ret, 21, fracAS);

  SET_STRING_ELT(retN,22,Rf_mkChar("fracB"));
  SEXP fracBS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracB = REAL(fracBS);
  SET_VECTOR_ELT(ret, 22, fracBS);

  SET_STRING_ELT(retN,23,Rf_mkChar("fracC"));
  SEXP fracCS = PROTECT(Rf_allocVector(REALSXP, lenOut)); pro++;
  double *fracC = REAL(fracCS);
  SET_VECTOR_ELT(ret, 23, fracCS);

  SEXP sexp_class = PROTECT(Rf_allocVector(STRSXP, 1)); pro++;
  SET_STRING_ELT(sexp_class,0,Rf_mkChar("data.frame"));
  Rf_setAttrib(ret, R_ClassSymbol, sexp_class);

  SEXP sexp_rownames = PROTECT(Rf_allocVector(INTSXP,2)); pro++;
  INTEGER(sexp_rownames)[0] = NA_INTEGER;
  INTEGER(sexp_rownames)[1] = -lenOut;
  Rf_setAttrib(ret, R_RowNamesSymbol, sexp_rownames);

  Rf_setAttrib(ret, R_NamesSymbol, retN);

  unsigned int ncmta=3;

  for (int i = lenOut; i--;){
    parTransPtr(&trans, ((lenP1 == 1) ? p1 : p1++),
                ((lenV == 1) ? v1 : v1++),
                ((lenP2 == 1) ? p2 : p2++),
                ((lenP3 == 1) ? p3 : p3++),
                ((lenP4 == 1) ? p4 : p4++),
                ((lenP5 == 1) ? p5 : p5++),
                &ncmta, kel, vc, k12, k21, k13, k31);
    linCmtPar3(vc, kel, k12, k21, k13, k31,
               vp, vp2, vss, cl, q, q2,
               A, B, C, fracA, fracB, fracC, alpha, beta, gamma,
               thalfAlpha, thalfBeta, thalfGamma);
    if (dig > 0) {
      (*vc)  = Rf_fprec((*vc), dig);
      (*kel) = Rf_fprec((*kel), dig);
      (*k12) = Rf_fprec((*k12), dig);
      (*k21) = Rf_fprec((*k21), dig);
      (*k13) = Rf_fprec((*k13), dig);
      (*k31) = Rf_fprec((*k31), dig);
      (*vp)  = Rf_fprec((*vp), dig);
      (*vss) = Rf_fprec((*vss), dig);
      (*vp2) = Rf_fprec((*vp2), dig);
      (*cl)  = Rf_fprec((*cl), dig);
      (*q)   = Rf_fprec((*q), dig);
      (*q2)  = Rf_fprec((*q2), dig);
      (*A)   = Rf_fprec((*A), dig);
      (*B)   = Rf_fprec((*B), dig);
      (*C)   = Rf_fprec((*C), dig);
      (*fracA)=Rf_fprec((*fracA), dig);
      (*fracB)=Rf_fprec((*fracB), dig);
      (*fracC)=Rf_fprec((*fracC), dig);
      (*alpha)=Rf_fprec((*alpha), dig);
      (*beta) =Rf_fprec((*beta), dig);
      (*gamma)=Rf_fprec((*gamma), dig);
      (*thalfAlpha)=Rf_fprec((*thalfAlpha), dig);
      (*thalfBeta)=Rf_fprec((*thalfBeta), dig);
      (*thalfGamma)=Rf_fprec((*thalfGamma), dig);
    }
    vc++; kel++; k12++; k21++; k13++; k31++;
    vp++; vp2++; vss++; cl++; q++; q2++;
    A++; B++; C++; fracA++; fracB++; fracC++; alpha++; beta++; gamma++;
    thalfAlpha++; thalfBeta++; thalfGamma++;
  }
  UNPROTECT(pro);
  return ret;
}


extern "C" SEXP _rxode2_calcDerived(SEXP ncmtSXP,
                                    SEXP oralSXP,
                                    SEXP w2SXP,
                                    SEXP transSXP, SEXP inp, SEXP sigdigSXP) {
BEGIN_RCPP
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
END_RCPP
}
