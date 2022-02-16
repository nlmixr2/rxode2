//#undef NDEBUG
#define USE_FC_LEN_T
#define STRICT_R_HEADER
#define ARMA_DONT_USE_OPENMP // Known to cause speed problems
#include <iostream>
#include <RcppArmadillo.h>
#include <algorithm>
#include "../inst/include/rxode2.h"
#define ARMA_DONT_PRINT_ERRORS
#define ARMA_DONT_USE_OPENMP // Known to cause speed problems

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif

using namespace Rcpp;

std::string symengineRes(std::string val){
  if (val == "e" ||
      val == "E" ||
      val == "EulerGamma" ||
      val == "Catalan" ||
      val == "GoldenRatio" ||
      val == "I"){
    return "rx_SymPy_Res_" + val;
  }
  return val;
}

// Create R source for creating a Inductive linearization matrix
//
// Assume .states=the states in the model
// Assume .env= symengine environment
// @return A character string of R code for inductive linearization
//[[Rcpp::export]]
std::string rxIndLin_(CharacterVector states){
  std::string ret = "matrix(c(";
  std::string n = "c(";
  for (int i = 0; i < states.size(); i++){
    ret += ".rxIndLinLine(.env$rx__d_dt_"+as<std::string>(states[i])+
      "__" + ",.states, \""+ as<std::string>(states[i]) + "\"),";
    n += "\"" + states[i] +"\",";
  }
  ret += "NULL)," + std::to_string(states.size()) + "," + std::to_string(states.size()+2) +
    ",TRUE,list(" + n +"NULL)," + n + "\"_rxF\",\"indLin\")))";
  return ret;
}

extern "C" void F77_NAME(matexprbs)(int *ideg, int *m, double *t, double *H, int *iflag);

extern "C" void matexp_MH09(double *x, int n, const int p, double *ret);

static inline arma::mat matrixExp(arma::mat& H, double t, int& type,
				  int& order){
  switch(type){
  case 3: {
    int p = order;
    if (p > 13) p = 13;
    int n = H.n_rows;
    arma::mat Hin = H*t;
    arma::mat Hout(Hin.n_rows,Hin.n_cols);
    double *x = Hin.memptr();
    double *ret = Hout.memptr();
    matexp_MH09(x, n, p, ret);
    return Hout;
    break;
  }
  case 2: {
    int iflag=0;
    int m = H.n_rows;
    // FIXME C++ implementation for threading.
    F77_CALL(matexprbs)(&order, &m, &t, &H[0], &iflag);
    return H;
    break;
  }
  default:
    arma::mat mat2 = t*H;
    return (arma::expmat(mat2));
  }
}
// extern "C" typedef void (*matvec_t) (double *, double *, double *, int *);

// extern "C" typedef void (*DGPADM_t)(int *ideg, int *mx, double *t,
// 				    double *, int *mh, double *,
// 				    int *lfree, int *iwsp, int *iexph,
// 				    int *ns, int *iflag, int *type);

// extern "C" void F77_NAME(DSPHIV)(int *n, int *m, double *t,
// 				 double *u, double *v, double *w,
// 				 double *tol, double *anorm,
// 				 double *wsp, int *lwsp,
// 				 int *iwsp, int *liwsp, matvec_t,
// 				 int *iflag, double *A, DGPADM_t,
// 				 int *type, int *ideg, int *mxstep);

arma::vec phiv(double t, arma::mat& A, arma::vec& u,
	       arma::vec& v, rx_solving_options *op){
  int n = A.n_rows;
  int order = op->indLinMatExpOrder;
  int type = op->indLinMatExpType;
  switch(n){
  case 1: {
    // m = 0
    // I don't think we *should* run into this case, but...
    arma::vec w(1);
    double eAt = exp(t*A(0,0));
    w(0) = eAt*v(0) + (eAt-1)/A(0,0)*u(0);
    return w;
  }
  case 2: {
    // m=1
    double d= (A(0,0)*A(1,1)-A(0,1)*A(1,0));
    d = 1.0/d;
    arma::mat22 Ainv;
    Ainv(0,0) = A(1,1)*d;
    Ainv(1,1) = A(0,0)*d;
    Ainv(0,1) = -A(0,1)*d;
    Ainv(1,0) = -A(0,1)*d;
    arma::mat22 expAt = matrixExp(A, t, type, order);
    arma::vec w = expAt*v + (expAt-arma::eye(2,2))*Ainv*u;
    return w;
  }
  default: {
    double tol = op->indLinPhiTol;
    int m = op->indLinPhiM;
    if (m <= 0) m = std::min(n, 30);
    double anorm = arma::norm(A, "inf");
    int mxrej = 10;  double btol  = 1.0e-7; 
    double gamma = 0.9; double delta = 1.2; 
    int mb    = m; double t_out   = fabs(t);
    int istep = 0; double t_new   = 0;
    double t_now = 0; double s_error = 0;
    double rndoff= anorm*DBL_EPSILON;
    double sgn = (0.0 < t) - (t > 0.0);
    int k1 = 3, ireject = 0, mx=0;
    double xm = 1.0/m; 
    arma::vec w = v;
    arma::mat V, H, F, tmp;
    arma::vec p;
    double beta=0, fact=0, s=0, t_step=0, h=0, avnorm=0, err_loc=0, p1, p2;
    while (t_now < t_out){
      V = arma::mat(n, m+1, arma::fill::zeros);
      H = arma::mat(m+3, m+3, arma::fill::zeros);
      V.col(0) = A*w+u;
      beta = norm(V.col(0));
      V.col(0) /= beta;
      if (istep == 0){
	fact = R_pow_di((m+1)/M_E,m+1)*sqrt(M_2PI*(m+1));
	t_new = (1/anorm)*pow((fact*tol)/(4*beta*anorm),xm);
	s = R_pow_di(10,(std::floor(log10(t_new))-1));
	t_new = std::ceil(t_new/s)*s;
      }
      istep++;
      t_step = std::min( t_out-t_now,t_new );
      for (int j = 0; j < m; ++j){
	p = A*V.col(j);
	for (int i = 0; i < j; ++i){
	  tmp = V.col(i).t()*p;
	  H(i,j) = tmp(0,0);
	  p = p-H(i,j)*V.col(i);
	}
	s = norm(p); 
	if (s < btol){
	  k1 = 0;
	  mb = j;
	  t_step = t_out-t_now;
	  break;
	}
	H(j+1,j) = s;
	V.col(j+1) = (1/s)*p;
      }
      H(0,mb) = 1; 
      if (k1 != 0){
	H(m,m+1) = 1;
	H(m+1,m+2) = 1;
	h = H(m,m-1);
	H(m,m-1) = 0;
	avnorm = norm(A*V.col(m-1)); 
      }
      ireject = 0;
      while(ireject <= mxrej){
	mx = mb + std::max(1,k1);
	F = H(arma::span(0,mx-1),arma::span(0,mx-1));
	F = matrixExp(F, sgn*t_step, type, order);
	if (k1 == 0){
	  err_loc = btol; 
	  break;
	} else {
	  F(m,m) = h*F(m-1,m+1);
	  F(m+1,m) = h*F(m-1,m+2);
	  p1 = fabs( beta*F(m,m) );
	  p2 = fabs( beta*F(m+1,m) * avnorm );
	  if (p1 > 10*p2){
	    err_loc = p2;
	    xm = 1.0/m;
	  } else if (p1 > p2){
	    err_loc = (p1*p2)/(p1-p2);
	    xm = 1.0/m;
	  } else{
	    err_loc = p1;
	    xm = 1.0/(m-1.0);
	  }
	}
	if (err_loc <= delta * t_step*tol){
	  break;
	} else {
	  t_step = gamma * t_step * pow(t_step*tol/err_loc, xm);
	  s = R_pow_di(10,std::floor(log10(t_step))-1);
	  t_step = std::ceil(t_step/s) * s;
	  if (ireject == mxrej){
	    stop(_("requested tolerance is too high"));
	  }
	  ireject = ireject + 1;
	}
      }
      if (k1-2 > 0){
	mx = mb + k1-2;
      } else {
	mx = mb;
      }
      w = V.cols(0,mx-1)*(beta*F(arma::span(0,mx-1),arma::span(mb,mb))) + w;
  
      t_now = t_now + t_step;
      t_new = gamma * t_step * pow(t_step*tol/err_loc, xm);
      t_new = std::max(std::min(t_new, 1e300), 1.0-200);
      s = R_pow_di(10.0, std::floor(log10(t_new))-1);
      t_new = std::ceil(t_new/s) * s;
      err_loc = std::max(err_loc,rndoff);
      s_error = s_error + err_loc;
    }
    // err = s_error
    return w;
  }
  }
}

bool expm_assign=false;
SEXP expm_s;

int meOnly(int cSub, double *yc_, double *yp_, double tp, double tf, double tcov,
	   double *InfusionRate_, int *on_, t_ME ME, rx_solving_options *op){
  int neq = op->neq;
  int type = op->indLinMatExpType;
  int order = op->indLinMatExpOrder;
  arma::mat m0(neq, neq);
  ME(cSub, tcov, tf, m0.memptr(), yc_);
  const arma::vec InfusionRate(InfusionRate_, neq, false, false);
  arma::vec yp(yp_, neq, false, true);
  arma::vec yc(yc_, neq, false, true);
  // arma::mat inMat;
  // arma::mat mexp;
  // arma::mat ypout;
  unsigned int i, nInf=0;
  arma::vec ypExtra(neq);
  arma::mat m0extra(neq, neq, arma::fill::zeros);
  for (i = 0; i < (unsigned int)neq; i++){
    if (InfusionRate[i] != 0.0){
      nInf++;
      m0extra[neq*(nInf-1)+i]=1;
      ypExtra[i] = InfusionRate[i];
    }
  }
  if (nInf == 0){
    arma::mat expAT(neq, neq);
    expAT = matrixExp(m0, tf-tp, type, order);
    yc = expAT*yp;
    return 1;
  } else {
    arma::mat mout(neq+nInf, neq+nInf, arma::fill::zeros);
    arma::vec ypout(neq+nInf);
    for (int j = neq; j--;){
      std::copy(m0.colptr(j), m0.colptr(j)+neq, mout.colptr(j));
    }
    for (int j = nInf; j--;){
      std::copy(m0extra.colptr(j),m0extra.colptr(j)+neq, mout.colptr(neq+j));
    }
    std::copy(yp.begin(),yp.end(),ypout.begin());
    std::copy(ypExtra.begin(),ypExtra.end(), ypout.begin()+neq);
    arma::vec meSol(neq+nInf);
    arma::mat expAT(neq+nInf, neq+nInf);
    // Unfortunately the tf-tp may change so we can not cache this.
    expAT = matrixExp(mout, (tf-tp), type, order);
    meSol = expAT*ypout;
    std::copy(meSol.begin(), meSol.begin()+neq, yc_);
    return 1;
  }
}

//' Inductive linearization solver
//'
//' @param cSub = Current subject number
//' @param op - rxode2 solving options
//' @param tp - Prior time point/time zeor
//' @param yp - Prior state;  vector size = neq; Final state is updated here
//' @param tf - Final Time
//' @param InfusionRate = Rates of each comparment;  vector size = neq
//' @param on Indicator for if the compartment is "on"
//' @param cache
//'    0 = no Cache
//'    When doIndLin == 0, cache > 0 = nInf-1
//' @param ME the rxode2 matrix exponential function
//' @param IndF The rxode2 Inductive Linearization function F
//' 
//' @return Returns a status for solving
//' 
//'   1 = Successful solve
//' 
//'   -1 = Maximum number of iterations reached when doing
//'        inductive linearization
//' @name rxIndLin_
extern "C" int indLin(int cSub, rx_solving_options *op, double tp, double *yp_, double tf,
		      double *InfusionRate_, int *on_, 
		      t_ME ME, t_IndF  IndF){
  int neq = op->neq;
  double *rtol=op->rtol2;
  double *atol=op->atol2;
  int maxsteps=op->mxstep;
  int doIndLin=op->doIndLin;
  // int indLinPerterb=10;
  // double indLinAmt=1.0;
  // int phiM=op->indLinPhiM;
  // double phiTol=op->indLinPhiTol;
  // double phiAnorm = op->indLinPhiAnorm;
  std::ostream nullstream(0);
  arma::set_cerr_stream(nullstream);
  
  int locf=(op->is_locf!=2);
  double tcov = tf;
  if (locf) tcov = tp;
  switch(doIndLin){
  case 1: {
    return meOnly(cSub, yp_, yp_, tp, tf, tcov, InfusionRate_, on_, ME, op);
  }
  case 3: {
    // Matrix exponential  +  inductive linearzation 
    arma::vec wLast(neq);
    arma::vec w(yp_, neq);
    arma::vec y0 = w;
    // Update first value
    meOnly(cSub, w.memptr(), y0.memptr(), tp, tf, tcov, InfusionRate_, on_, ME, op);
    // Don't update rest
    wLast = w;
    meOnly(cSub, w.memptr(), y0.memptr(), tp, tf, tcov, InfusionRate_, on_, ME, op);
    bool converge = false;
    for (int i = 0; i < maxsteps; ++i){
      converge=true;
      for (int j=op->indLinN;j--;){
    	if (fabs(w[op->indLin[j]]-wLast[op->indLin[j]]) >= rtol[op->indLin[j]]*fabs(w[op->indLin[j]])+
	    atol[op->indLin[j]]){
    	  converge = false;
    	  break;
    	}
      }
      if (converge){
    	break;
      }
      wLast = w;
      meOnly(cSub, w.memptr(), y0.memptr(), tp, tf, tcov, InfusionRate_, on_, ME, op);
    }
    std::copy(w.begin(), w.begin()+neq, yp_);
    return 1;
  }
  case 2: {
    // This will not changed with IndLin
    arma::vec u(neq);
    arma::vec yp(yp_, neq, false, false);
    IndF(cSub, tcov, tf, u.memptr());
    arma::mat m0(neq, neq);
    ME(cSub, tcov, tf, m0.memptr(), yp_);
    arma::vec w = phiv((tf-tp), m0, u, yp, op);
    std::copy(w.begin(), w.begin()+neq, yp_);
    return 1;
  }
  case 4: {
    // Matrix exponential with + u and inductive linearization
    // This will not changed with IndLin
    arma::vec u(neq);
    IndF(cSub, tcov, tf, u.memptr());
    arma::mat m0(neq, neq);
    ME(cSub, tcov, tf, m0.memptr(), yp_);
    arma::vec wLast(neq);
    arma::vec w(yp_, neq);
    arma::vec yp(yp_, neq, false, false);
    // Update first value
    w = phiv((tf-tp), m0, u, yp, op);
    wLast = w;
    // Now update matrix
    ME(cSub, tcov, tf, m0.memptr(), w.memptr());
    w = phiv((tf-tp), m0, u, yp, op);
    bool converge = false;
    for (int i = 0; i < maxsteps; ++i){
      converge=true;
      for (int j=op->indLinN;j--;){
    	if (fabs(w[op->indLin[j]]-wLast[op->indLin[j]]) >= rtol[op->indLin[j]]*fabs(w[op->indLin[j]])+
	    atol[op->indLin[j]]){
    	  converge = false;
    	  break;
    	}
      }
      if (converge){
    	break;
      }
      wLast = w;
      ME(cSub, tcov, tf, m0.memptr(), w.memptr());
      w = phiv((tf-tp), m0, u, yp, op);
    }
    std::copy(w.begin(), w.begin()+neq, yp_);
    return 1;
  }
  default:
    stop(_("unsupported indLin code: %d"), doIndLin);
  }
  // if (doIndLin == 0){
  //   // Total possible enhanced matrix is (neq+neq)x(neq+neq)
  //   // Total possible initial value is (neq+neq)
  //   // expAt is (neq+neq)x(neq+neq)
  //   // Total possible output is (neq+neq)
  //   // =4*neq + 8*neq^2
  //   // These are simple linear with no f
  //   // Hence there is no need for matrix inversion
  // }
  // else {
  //   // In this case the inital matrix should not be expanded. The
  //   // infusions are put into the F function
  //   const arma::vec InfusionRate(InfusionRate_, neq, false, false);
  //   arma::vec yp(yp_, neq, false, false);
  //   arma::vec u(neq);
  //   arma::vec extra(neq,arma::fill::zeros);
  //   arma::vec w(neq);
  //   arma::vec wLast(neq);
  //   double *fptr = u.memptr();
  //   if (doIndLin==1){
  //     // For LOCF tp for NOCB tf
  //     // IndF(cSub, tcov, tf, fptr, wLast.memptr(), InfusionRate_);
  //     IndF(cSub, tcov, tf, fptr, yp_, InfusionRate_);
  //     wLast = phiv((tf-tp), m0, u, yp, op);
  //     // For inhomogenous systems we can return here.
  //     std::copy(wLast.begin(), wLast.end(), &yp_[0]);
  //     return 1;
  //   }
  //   IndF(cSub, tcov, tf, fptr, wLast.memptr(), InfusionRate_,extra.memptr());
  //   w=phiv((tf-tp), m0, u, yp, op);
  //   bool converge = false;
  //   Rprintf("tf: %f:\n",tf);
  //   for (int i = 0; i < maxsteps; ++i){
  //     converge=true;
  //     for (int j=neq;j--;){
  //   	if (fabs(w[j]-wLast[j]) >= rtol[j]*fabs(w[j])+atol[j]){
  //   	  converge = false;
  //   	  break;
  //   	}
  //     }
  //     if (converge){
  //   	break;
  //     }
  //     wLast = w+DOUBLE_EPS; // Try to break out of infinite loop.
  //     IndF(cSub, tcov, tf, fptr, wLast.memptr(), InfusionRate_,extra.memptr());
  //     w=phiv((tf-tp), m0, u, yp, op);
  //     print(wrap(w.t()));
  //   }
  //   if (!converge){
  //     Rprintf("Did not converge!");
  //     std::copy(w.begin(), w.end(), &yp_[0]);
  //     // std::fill_n(&yp_[0], neq, NA_REAL);
  //     return 1;
  //   } else {
  //     std::copy(w.begin(), w.end(), &yp_[0]);
  //     return 1;
  //   }
  // }
  return 1;
}
