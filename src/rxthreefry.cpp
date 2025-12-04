#define USE_FC_LEN_T
//#undef NDEBUG
#define STRICT_R_HEADERS
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#include <limits>
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"
#include "../inst/include/rxode2parse.h"
#include <threefry.h>
#include <R.h>
#include "checkmate.h"
#include "rxomp.h"
/* replace pkg as appropriate */
#define _(String) (String)
using namespace Rcpp;
using namespace arma;
#include "../inst/include/rxode2_as.h"
#include "threefry.h"
#include "../inst/include/rxode2random_fillVec.h"

#include <boost/random/normal_distribution.hpp>
#include <boost/random/negative_binomial_distribution.hpp>
#include <boost/random/binomial_distribution.hpp>
#include <boost/random/cauchy_distribution.hpp>
#include <boost/random/chi_squared_distribution.hpp>
#include <boost/random/exponential_distribution.hpp>
#include <boost/random/fisher_f_distribution.hpp>
#include <boost/random/gamma_distribution.hpp>
#include <boost/random/geometric_distribution.hpp>
#include <boost/random/poisson_distribution.hpp>
#include <boost/random/student_t_distribution.hpp>
#include <boost/random/weibull_distribution.hpp>

#include "rxnbinom.h"

extern "C" {
  extern rx_solve rx_global;
  extern rx_solving_options op_global;
  extern rx_solving_options_ind *inds_thread;
}

extern "C" uint32_t getRxSeed1(int ncores);
extern "C" void setSeedEng1(uint32_t seed);
extern "C" void setRxSeedFinal(uint32_t seed);

arma::vec getLowerVec(int type, rx_solve* rx);
arma::vec getUpperVec(int type, rx_solve* rx);
arma::mat getArmaMat(int type, int csim, rx_solve* rx);

//[[Rcpp::export]]
SEXP rxRmvn_(NumericMatrix A_, arma::rowvec mu, arma::mat sigma,
             int ncores=1, bool isChol=false) {
  int n = A_.nrow();
  int d = mu.n_elem;
  arma::mat ch;
  if (sigma.is_zero()){
    ch = sigma;
    for (int i = 0; i < d; ++i){
      for (int j = 0; j < n; ++j) {
        A_(j, i) = mu[i];
      }
    }
    return R_NilValue;
  } else {
    if (isChol){
      ch=arma::trimatu(sigma);
    } else {
      ch=arma::trimatu(arma::chol(sigma));
    }
  }

  if (n < 1) stop(_("n should be a positive integer"));
  if (ncores < 1) stop(_("'ncores' has to be greater than one"));
  if (d != (int)sigma.n_cols) stop("length(mu) != ncol(sigma)");
  if (d != (int)sigma.n_rows) stop("length(mu) != ncol(sigma)");
  if (d != (int)A_.ncol()) stop("length(mu) != ncol(A)");

  uint32_t seed = getRxSeed1(ncores);
#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif
    arma::mat A(A_.begin(), A_.nrow(), A_.ncol(), false, true);
    sitmo::threefry eng;
#ifdef _OPENMP
    eng.seed(seed+rx_get_thread(op_global.cores));
#else
    eng.seed(seed);
#endif

    boost::random::normal_distribution<> snorm(0.0, 1.0);

    double acc;
    arma::rowvec work(d);
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n*d; i += ncores) {
        A[i] = snorm(eng);
      }
    }
    if (d == 1){
      double sd = ch(0, 0);
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
      for (int thread = 0; thread < ncores; ++thread) {
        for (int i = thread; i < n; i+=ncores) {
          A[i] = A[i]*sd+mu(0);
        }
      }
    } else {
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
      for (int thread = 0; thread < ncores; ++thread) {
        for(int ir = thread; ir < n; ir += ncores) {
          for(int ic = d; ic--;) {
            acc = 0.0;
            for (int ii = 0; ii <= ic; ++ii) {
              acc += A.at(ir,ii) * ch.at(ii,ic);
            }
            work.at(ic) = acc;
          }
          work += mu;
          A(arma::span(ir), arma::span::all) = work;
        }
      }
#ifdef _OPENMP
    }
#endif
  }
  return R_NilValue;
}

void rxRmvn2_(arma::mat& A, arma::rowvec mu, arma::mat sigma,
              int ncores=1, bool isChol=false) {
  int n = A.n_rows;
  int d = mu.n_elem;
  arma::mat ch;
  if (sigma.is_zero()){
    ch = sigma;
  } else {
    if (isChol){
      ch=arma::trimatu(sigma);
    } else {
      ch=arma::trimatu(arma::chol(sigma));
    }
  }

  if (n < 1) stop(_("n should be a positive integer"));
  if (ncores < 1) stop(_("'ncores' has to be greater than one"));
  if (d != (int)sigma.n_cols) stop("length(mu) != ncol(sigma)");
  if (d != (int)sigma.n_rows) stop("length(mu) != ncol(sigma)");
  if (d != (int)A.n_cols) stop("length(mu) != ncol(A)");

  uint32_t seed = getRxSeed1(ncores);
#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

    sitmo::threefry eng;

#ifdef _OPENMP
    eng.seed(seed+rx_get_thread(op_global.cores));
#else
    eng.seed(seed);
#endif

    boost::random::normal_distribution<> snorm(0.0, 1.0);

    double acc;
    arma::rowvec work(d);
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n*d; i += ncores){
        A[i] = snorm(eng);
      }
    }
    if (d == 1){
      double sd = ch(0, 0);
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
      for (int i = 0; i < n; i++){
        A[i] = A[i]*sd+mu(0);
      }
    } else {
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
      for(int ir = 0; ir < n; ++ir){
        for(int ic = d; ic--;){
          acc = 0.0;
          for (int ii = 0; ii <= ic; ++ii){
            acc += A.at(ir,ii) * ch.at(ii,ic);
          }
          work.at(ic) = acc;
        }
        work += mu;
        A(arma::span(ir), arma::span::all) = work;
      }
    }
#ifdef _OPENMP
  }
#endif
}


// Adapted from https://github.com/cran/TruncatedNormal/blob/7364c5bc3f7c84d00eb4a767807b103b4232b648/R/ntail.R
double ntail(double l, double u, sitmo::threefry& eng){
  // samples a column vector of length=length(l)=length(u)
  // from the standard multivariate normal distribution,
  // truncated over the region [l,u], where l>0 and
  // l and u are column vectors;
  //  uses acceptance-rejection from Rayleigh distr;
  // method due to Marsaglia (1964);
  std::uniform_real_distribution<> unif(0.0, 1.0);
  double c=l*l/2.0;
  double f = expm1(c-u*u/2.0);
  double x =0.0;
  bool accept = true;
  while (accept){
    double tmp = unif(eng);
    x = c - log(1+unif(eng)*f); // sample using Rayleigh
    // Reject
    if (tmp*tmp*x <= c){ // accepted
      accept = false;
    }
  }
  // this Rayleigh transform can be delayed till the end
  return sqrt(2.0*x);
}

double tn(double l, double u, sitmo::threefry& eng, double tol = 2.05){
  // samples a column vector of length=length(l)=length(u)
  // from the standard multivariate normal distribution,
  // truncated over the region [l,u], where -a<l<u<a for some
  // 'a' and l and u are column vectors;
  // uses acceptance rejection and inverse-transform method;
  // tol=2.05 # controls switch between methods
  // # threshold can be tuned for maximum speed for each platform
  // case: abs(u-l)>tol, uses accept-reject from randn
  boost::random::normal_distribution<> rnorm(0.0, 1.0);
  std::uniform_real_distribution<> runif(0.0, 1.0);
  double x=0;
  if (fabs(u - l) > tol){
    x = rnorm(eng);
    while (x < l || x > u){
      x = rnorm(eng);
    }
  } else {
    double pl=R::pnorm(l, 0.0, 1.0, true, false);
    double pu=R::pnorm(u, 0.0, 1.0, true, false);
    x = R::qnorm(pl+(pu-pl)*runif(eng), 0.0, 1.0, true, false);
  }
  return x;
}

double trandn(double l, double u, sitmo::threefry& eng, double a=0.4,
              double tol = 2.05){
  // truncated normal generator
  // * efficient generator of a vector of length(l)=length(u)
  // from the standard multivariate normal distribution,
  // truncated over the region [l,u];
  // infinite values for 'u' and 'l' are accepted;
  // * Remark:
  // If you wish to simulate a random variable
  // 'Z' from the non-standard Gaussian N(m,s^2)
  // conditional on l<Z<u, then first simulate
  // X=trandn((l-m)/s,(u-m)/s) and set Z=m+s*X;
  //
  // Reference:
  // Z. I. Botev (2015),
  // "The Normal Law Under Linear Restrictions:
  //  Simulation and Estimation via Minimax Tilting", submitted to JRSS(B)
  // a=.4; # treshold for switching between methods
  // threshold can be tuned for maximum speed for each Matlab version
  // three cases to consider:
  double x=0;
  if (l > a){
    // case 1: a<l<u
    x = ntail(l, u, eng);
  } else if (u < -a){
    // case 2: l<u<-a
    x = -ntail(-u, -l, eng);
  } else {
    x = tn(l, u, eng, tol);
  }
  return x;
}

double lnNpr(double a,double b) {
  // computes ln(P(a<Z<b))
  // where Z~N(0,1) very accurately for any 'a', 'b'
  if (a > 0){
    double pa = R::pnorm(a, 0.0, 1.0, false, true);
    double pb = R::pnorm(b, 0.0, 1.0, false, true);
    return pa + log1p(-exp(pb-pa));
  }
  // case a < b < 0
  if (b < 0){
    double pa = R::pnorm(a, 0.0, 1.0, true, true);
    double pb = R::pnorm(b, 0.0, 1.0, true, true);
    return pb+log1p(-exp(pa-pb));
  }
  // case a<0<b
  double pa = R::pnorm(a, 0.0, 1.0, true, false);
  double pb = R::pnorm(b, 0.0, 1.0, false, false);
  return log1p(-pa-pb);
}

typedef struct {
  arma::mat Z;
  arma::vec p;
  arma::vec u;
} rx_mvnrnd;

rx_mvnrnd mvnrnd(int n, arma::mat& L, arma::vec& l,
                 arma::vec& u, arma::vec mu,
                 sitmo::threefry& eng,
                 double a=0.4, double tol = 2.05){
  // generates the proposals from the exponentially tilted
  // sequential importance sampling pdf;
  // output:    'logpr', log-likelihood of sample
  //              Z, random sample
  rx_mvnrnd ret;
  int d=l.n_elem; // Initialization
  mu[d-1]=0;
  arma::mat Z(d, n, arma::fill::zeros); // create array for variables
  arma::vec p(n, arma::fill::zeros);
  arma::vec uu(n, arma::fill::zeros);
  std::uniform_real_distribution<> unif(0.0, 1.0);
  for (int k = 0; k < d; ++k){
    //# compute matrix multiplication L*Z
    arma::vec col=trans(L(k,arma::span(0,k)) * Z.rows(0, k));
    //# compute limits of truncation
    arma::vec tl=l[k]-mu[k]-col;
    arma::vec tu=u[k]-mu[k]-col;
    //#simulate N(mu,1) conditional on [tl,tu]
    for (int j = n; j--;){
      Z(k,j) = mu[k] + trandn(tl[j], tu[j], eng, a, tol);
      // # update likelihood ratio
      p[j] += lnNpr(tl[j], tu[j]) + 0.5*mu[k]*mu[k] - mu[k]*Z(k,j);
      if (k == 0) uu[j] = -log(unif(eng));
    }
  }
  ret.Z = Z;
  ret.p = p;
  ret.u = uu;
  return ret;
}

//[[Rcpp::export]]
List rxMvnrnd(int n, arma::mat& L, arma::vec& l,
              arma::vec& u, arma::vec mu,
              double a=0.4, double tol = 2.05){
  uint32_t seed = getRxSeed1(1);
  sitmo::threefry eng;
  eng.seed(seed);
  rx_mvnrnd retI = mvnrnd(n, L, l, u, mu, eng,
                          a, tol);
  List ret(2);
  NumericVector po(retI.p.size());
  std::copy(retI.p.begin(), retI.p.end(), po.begin());
  ret[0] = po;
  ret[1] = wrap(retI.Z);
  ret.attr("names") = CharacterVector::create("logpr", "Z");
  return ret;
}

typedef struct {
  // return(list(L=L,l=l,u=u,perm=perm))
  arma::mat L;
  arma::vec l;
  arma::vec u;
  arma::uvec perm;

} rx_cholperms;

rx_cholperms cholperm(arma::mat Sig, arma::vec& cl, arma::vec& cu,
                      double eps=1e-10){
  // #  Computes permuted lower Cholesky factor L for Sig
  // #  by permuting integration limit vectors l and u.
  // #  Outputs perm, such that Sig(perm,perm)=L%*%t(L).
  // #
  // # Reference:
  // #  Gibson G. J., Glasbey C. A., Elston D. A. (1994),
  // #  "Monte Carlo evaluation of multivariate normal integrals and
  // #  sensitivity to variate ordering",
  // #  In: Advances in Numerical Methods and Applications, pages 120--126
  arma::vec l=cl;
  arma::vec u=cu;
  int d = l.n_elem;
  arma::uvec perm(d);
  std::iota(perm.begin(),perm.end(),0);
  arma::mat L(d,d, arma::fill::zeros);
  arma::vec z(d, arma::fill::zeros);
  for (int j = 0; j < d; ++j){
    arma::vec pr(d);
    std::fill_n(pr.begin(), d, R_PosInf);
    arma::span I = span(j,d-1);
    arma::vec D = Sig.diag();
    arma::vec sv(1);
    if (j > 1){
      arma::vec r(j);
      std::fill_n(r.begin(), j, 1.0);
      sv = D(I) - (L(I, span(0, j-1)) % L(I, span(0, j-1))) * r;
    } else if (j == 1){
      sv =D(I)-L(I,0) % L(I,0);
    } else {
      sv = D(I);
    }
    for(int kk=sv.size(); kk--;){
      if (sv[kk] < 0) sv[kk] = sqrt(eps);
      else sv[kk] = sqrt(sv[kk]);
    }
    arma::vec colsV;
    if (j >1){
      colsV=L(I,span(0, j-1)) * z(span(0,j-1));
    } else if (j==1){
      colsV=L(I,0)*z(0);
    }
    arma::vec tl;
    arma::vec tu;
    if (j == 0){
      tl = l(I)/sv;
      tu = u(I)/sv;
    } else {
      tl = (l(I) - colsV)/sv;
      tu = (u(I) - colsV)/sv;
    }
    double minPr=R_PosInf;
    int k=0;
    for (int kk = j; kk < d; ++kk){
      pr[kk] = lnNpr(tl[kk-j], tu[kk-j]);
      if (pr[kk] < minPr) {
        minPr = pr[kk];
        k = kk;
      }
    }
    // find smallest marginal dimension
    // flip dimensions k-->j
    arma::uvec jk(2); jk(0) = j; jk(1) = k;
    arma::uvec kj(2); kj(0) = k; kj(1) = j;
    // update rows and cols of Sig
    Sig.rows(jk)=Sig.rows(kj);
    Sig.cols(jk)=Sig.cols(kj);
    // Update only rows of L.
    L.rows(jk) = L.rows(kj);
    // keep track of permutation
    perm(jk) = perm(kj);
    // update integration limits
    l(jk) = l(kj); u(jk) = u(kj);
    //construct L sequentially via Cholesky computation
    if (j == 0){
      sv=Sig(j,j)-L(j,0) * L(j,0);
    } else {
      sv=Sig(j,j)-accu(L(j,span(0,j-1)) % L(j,span(0,j-1)));
    }
    if (sv(0) < -0.001){
      stop("'Sigma' is not positive semi-definite");
    }
    if (sv(0) < 0) sv(0) =eps;
    L(j,j)= sqrt(sv(0));
    if (j < d-1 ){
      if (j > 1){
        L(span(j+1,d-1), j) = (Sig(span(j+1,d-1),j)-
                               L(span(j+1,d-1),span(0,j-1)) *
                               trans(L(j,span(0,(j-1))))) / L(j,j);
      } else if (j == 1){
        L(span(j+1,d-1),j) = (Sig(span(j+1,d-1),j)-
                              L(span(j+1,d-1),0) *
                              L(j,0)) / L(j,j);
      } else if (j == 0){
        L(span(j+1,d-1),j)=Sig(span((j+1),d-1),j)/L(j,j);
      }
    }
    // find mean value, z(j), of truncated normal:
    tl = (l(j) - L(j, span(0, j)) * z(span(0, j))) / L(j,j);
    tu = (u(j) - L(j, span(0, j)) * z(span(0, j))) / L(j, j);
    double w = lnNpr(tl(0), tu(0));
    // 1/sqrt(2*pi) = M_1_SQRT_2PI
    z[j] = (exp(-0.5 * tl(0)*tl(0) - w) - exp(-0.5* tu(0) * tu(0) - w)) * M_1_SQRT_2PI;
    // Rprintf("tl: %f, tu: %f, w: %f, z: %f\n", tl(0), tu(0), w, z[j]);
  }
  rx_cholperms ret;
  ret.L = L;
  ret.l = l;
  ret.u = u;
  ret.perm=perm;
  return ret;
}

// Exported for testing
//[[Rcpp::export]]
List rxCholperm(arma::mat Sig, arma::vec l, arma::vec u,
                double eps=1e-10){
  rx_cholperms retI = cholperm(Sig, l, u, eps);
  List ret(4);
  NumericVector lI(retI.l.size());
  std::copy(retI.l.begin(),retI.l.end(), lI.begin());
  NumericVector uI(retI.u.size());
  std::copy(retI.u.begin(),retI.u.end(), uI.begin());
  IntegerVector permI(retI.perm.size());
  std::copy(retI.perm.begin(),retI.perm.end(), permI.begin());
  ret[0] = wrap(retI.L);
  ret[1] = lI;
  ret[2] = uI;
  ret[3] = permI;
  ret.attr("names") = CharacterVector::create("L", "l", "u", "perm");
  return ret;
}

typedef struct {
  arma::vec grad;
  arma::mat Jac;
} rx_gradpsi;

rx_gradpsi gradpsi(arma::vec y, arma::mat L, arma::vec l, arma::vec u,
                   int ncores=1){
  rx_gradpsi ret;
  //# implements grad_psi(x) to find optimal exponential twisting;
  //  # assume scaled 'L' with zero diagonal;
  int d = u.n_elem;
  arma::vec c(d,arma::fill::zeros);
  arma::vec x, mu;
  x = c; mu = x;
  if (d <= 1){
    stop(_("dimension wrong in 'gradpsi' (d=%d)"), d);
  }
  x(span(0, d-2)) = y(span(0, d-2));
  mu(span(0, d-2)) = y(span(d-1, 2*d-3));
  //
  // compute now ~l and ~u
  c(span(1, d-1)) = L(span(1,d-1), span(0, d-1)) * x;
  arma::vec lt = l - mu - c;
  arma::vec ut = u - mu - c;
  // compute gradients avoiding catastrophic cancellation
  arma::vec w(d);
  arma::vec pl(d);
  arma::vec pu(d);
  arma::vec P(d);
#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int j = thread; j < d; j += ncores) {
        w[j] = lnNpr(lt[j], ut[j]);
        pl[j] = exp(-0.5*lt[j]*lt[j] - w[j])*M_1_SQRT_2PI;
        pu[j] = exp(-0.5*ut[j]*ut[j] - w[j])*M_1_SQRT_2PI;
        P[j] = pl[j] - pu[j];
      }
    }
    arma::vec dfdx = -mu(span(0, d-2)) + trans(trans(P) * L(span(0, d-1), span(0, d-2)));
    arma::vec dfdm = mu - x + P;
    arma::vec grad(dfdx.size()+dfdm.size()-1);
    std::copy(dfdx.begin(),dfdx.end(), grad.begin());
    std::copy(dfdm.begin(),dfdm.end()-1, grad.begin()+dfdx.size());
    // here compute Jacobian matrix
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int j = thread; j < d; j += ncores){
        if (!R_FINITE(lt[j])){
          lt[j] = 0;
        }
        if (!R_FINITE(ut[j])){
          ut[j] = 0;
        }
      }
    }
    arma::vec dP = -(P % P) + lt % pl - ut % pu; // dPdm
    arma::mat dPm(d, d);
    for (int j = d; j--;){
      std::copy(dP.begin(),dP.end(),dPm.begin()+d*j);
    }
    arma::mat DL = dPm % L;
    arma::mat mx = -eye(d, d) + DL;
    arma::mat xx = trans(L) * DL;
    arma::mat Jac;
    if (d > 2){
      mx = mx(span(0, d-2), span(0, d-2));
      xx = xx(span(0, d-2), span(0, d-2));
      Jac = join_cols(join_rows(xx,trans(mx)),
                      join_rows(mx, diagmat(1.0+dP(span(0, d-2)))));
    } else {
      mx = mx(0, 0);
      xx = xx(0, 0);
      Jac  = mat(2,2);
      Jac(0,0) = xx(0,0);
      Jac(1,0) = mx(0,0);
      Jac(0,1) = mx(0,0);
      Jac(1,1) = 1+dP(0);
    }
    ret.grad = grad;
    ret.Jac = Jac;
#ifdef _OPENMP
  }
#endif
  return ret;
}


// Exported for testing
//[[Rcpp::export]]
List rxGradpsi(arma::vec y, arma::mat L, arma::vec l, arma::vec u){
  rx_gradpsi retI = gradpsi(y, L, l, u);
  List ret(2);
  NumericVector lgrad(retI.grad.size());
  std::copy(retI.grad.begin(),retI.grad.end(), lgrad.begin());
  ret[1] = wrap(retI.Jac);
  ret[0] = lgrad;
  ret.attr("names") = CharacterVector::create("grad", "Jac");
  return ret;
}


arma::vec nleq(arma::vec l, arma::vec u, arma::mat L, double tol= 1e-10, int maxiter=100){
  int d=l.n_elem;
  arma::vec x(2*d-2, arma::fill::zeros); // initial point for Newton iteration
  double err=R_PosInf;
  int iter=0;
  while (err > tol){
    rx_gradpsi f=gradpsi(x,L,l,u);
    // Jac=f$Jac
    // grad=f$grad
    arma::vec del=solve(f.Jac,-f.grad, solve_opts::likely_sympd); // Newton correction
    x=x+del;
    err=accu(f.grad%f.grad);
    iter++;
    if (iter> maxiter){
      stop(_("covariance matrix is ill-conditioned and method failed (truncated mvn)"));
    }
  }
  return x;
}

// Exported for testing
//[[Rcpp::export]]
NumericVector rxNleq(arma::vec l, arma::vec u, arma::mat L){
  arma::vec retA = nleq(l, u, L, 1e-10, 100);
  NumericVector ret(retA.n_elem);
  std::copy(retA.begin(),retA.end(),ret.begin());
  return ret;
}

double psy(arma::vec x,arma::mat L,arma::vec l, arma::mat u, arma::vec mu,
           int ncores = 1){
  double p=0;
  // implements psi(x,mu); assume scaled 'L' without diagonal;
  int d=u.n_elem;
  x.resize(d);
  x(d-1)=0;
  mu.resize(d);
  mu(d-1)=0;
  // compute now ~l and ~u
  arma::vec c = L*x;
  l=l-mu-c;
  u=u-mu-c;
#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int j = thread; j < d; j += ncores){
        p+= lnNpr(l[j], u[j]) + 0.5*mu[j]*mu[j]-x[j]*mu[j];
      }
    }
#ifdef _OPENMP
  }
#endif
  return p;
}
arma::mat mvrandn(arma::vec lin, arma::vec uin, arma::mat Sig, int n,
                  sitmo::threefry& eng, double a=0.4, double tol = 2.05,
                  double nlTol=1e-10, int nlMaxiter=100, int ncores=1){
  if (ncores < 1) stop(_("'ncores' has to be greater than one"));
  int d = lin.n_elem;
  if ((int)uin.n_elem != d) stop(_("'lower' and 'upper' must have the same number of elements."));
  if ((int)Sig.n_rows != d || (int)Sig.n_cols != d) stop(_("'sigma' must be a square matrix with the same dimension as 'upper' and 'lower'"));
  if (any(lin>uin)){
    stop(_("'lower' is bigger than 'upper' for at least one item"));
  }
  // Cholesky decomposition of matrix
  rx_cholperms outC=cholperm(Sig,lin,uin);
  arma::mat Lfull=outC.L;
  arma::vec l=outC.l;
  arma::vec u=outC.u;
  arma::vec D=Lfull.diag();
  arma::uvec perm=outC.perm;
  if (any(D < 1e-10)){
    Rf_warningcall(R_NilValue, "%s", _("truncated multivariate normal may fail as covariance matrix is singular"));
  }
  // rescale
  arma::mat L=Lfull.each_col()/D;
  u=u/D;
  l=l/D;
  L=L-eye(d, d); // remove diagonal
  // find optimal tilting parameter via non-linear equation solver
  arma::vec xmu = nleq(l,u,L,nlTol, nlMaxiter); // nonlinear equation solver
  // assign saddlepoint x* and mu*
  arma::vec x = xmu(span(0, d-2));
  arma::vec mu = xmu(span(d-1, 2*d-3));
  // compute psi star
  double psistar=psy(x,L,l,u,mu, ncores);
  //  start acceptance rejection sampling
  int iter=0;
  // rv=c();
  int accepted=0;

  arma::mat ret(d, n);
  while (accepted < n){
    // rx_mvnrnd out=mvnrnd(n,L,l,u,mu);
    rx_mvnrnd out = mvnrnd(n, L, l, u, mu, eng, a, tol);
    arma::vec logpr = out.p;
    arma::mat curZ  = out.Z;
    // idx=-log(runif(n))>(psistar-logpr); # acceptance tests
    for (int i = n; i--;){
      if (out.u[i] > (psistar-logpr[i]) &&
          all(outC.l < curZ.col(i)) &&
          all(outC.u > curZ.col(i))) {
        ret.col(accepted) =curZ.col(i);
        accepted++;
        if (accepted == n) break;
      }
    }
    iter++;
    if (iter == 1e3){
      Rf_warningcall(R_NilValue, "%s", _("acceptance probability smaller than 0.001"));
    } else if (iter> 1e4){
      if (accepted == 0) {
        stop(_("could not sample from truncated normal"));
      } else if (accepted > 1) {
        Rf_warningcall(R_NilValue, _("sample of size %d which is smaller than requested 'n' returned"), accepted);
        ret = ret.rows(0, accepted);
        break;
      }
    }
  }
  ret = trans(ret);
  ret = ret.cols(outC.perm);
  return ret;
}

void rxMvrandn__(arma::mat& A,
                 arma::rowvec mu, arma::mat sigma, arma::vec lower,
                 arma::vec upper, int ncores=1,
                 double a=0.4, double tol = 2.05, double nlTol=1e-10, int nlMaxiter=100){
  int n = A.n_rows;
  int d = mu.n_elem;
  arma::mat ch;
  if (n < 1) stop(_("n should be a positive integer"));
  if (ncores < 1) stop(_("'ncores' has to be greater than one"));
  if (d != (int)sigma.n_cols) stop("length(mu) != ncol(sigma)");
  if (d != (int)sigma.n_rows) stop("length(mu) != ncol(sigma)");
  if (d != (int)A.n_cols) stop("length(mu) != ncol(A)");

  if (sigma.is_zero()){
    if (d == 1){
      for (int i = 0; i < n; ++i) {
        A[i] = mu(0);
      }
    } else {
      A.zeros();
      A.each_row() += mu;
    }
  } else {
    uint32_t seed = getRxSeed1(ncores);

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
    {
#endif

      sitmo::threefry eng;
      eng.seed(seed+rx_get_thread(op_global.cores));

      arma::vec low = lower-trans(mu);
      arma::vec up = upper-trans(mu);

      if (d == 1){
        double sd = sqrt(sigma(0,0));
        double l=low(0)/sd;
        double u=up(0)/sd;
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
        for (int i = 0; i < n; ++i) {
          A[i] = sd*trandn(l, u, eng, a, tol)+mu(0);
        }
      } else {
        arma::mat ret = mvrandn(low, up, sigma, n, eng, a, tol,
                                nlTol, nlMaxiter, ncores);
        ret.each_row() += mu;
        std::copy(ret.begin(), ret.end(), A.begin());
      }
    }
#ifdef _OPENMP
  }
#endif
}

//[[Rcpp::export]]
arma::mat rxMvrandn_(NumericMatrix A_,
                     arma::rowvec mu, arma::mat sigma, arma::vec lower,
                     arma::vec upper, int ncores=1,
                     double a=0.4, double tol = 2.05, double nlTol=1e-10, int nlMaxiter=100){
  arma::mat A(A_.begin(), A_.nrow(), A_.ncol(), false, true);
  rxMvrandn__(A, mu, sigma, lower, upper, ncores, a, tol, nlTol, nlMaxiter);
  return A;
}

std::vector<sitmo::threefry> _eng;

extern "C" void seedEng(int ncores) {
  uint32_t seed = getRxSeed1(ncores);
  _eng.clear();
  for (int i= 0; i < ncores; i++) {
    sitmo::threefry eng0;
    eng0.seed(seed + i);
    _eng.push_back(eng0);
  }
  seed = getRxSeed1(ncores);
}

extern "C" void setSeedEng1(uint32_t seed) {
  (_eng[rx_get_thread(op_global.cores)]).seed(seed);
}

//' This seeds the engine based on the number of cores used in random number generation
//'
//' @param ncores is the number of cores to use.
//' @keywords internal
//' @return Nothing, called for side effects
//' @export
//' @examples
//' rxSeedEng()
//[[Rcpp::export]]
RObject rxSeedEng(int ncores = 1){
  seedEng(ncores);
  return R_NilValue;
}

static inline double _rxnbinom__(double size, double p) {
  boost::random::negative_binomial_distribution_r<> dist(size, p);
  return (double) dist(_eng[rx_get_thread(op_global.cores)]);
}

static inline double _rxnbinom_mu_(double size, double mu) {
  boost::random::negative_binomial_distribution_mu<> dist(size, mu);
  return (double) dist(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double rxnbinomMu(int size, double mu) {
  if (ISNA(size) || ISNA(mu)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0;
  return _rxnbinom_mu_(size, mu);
}

extern "C" double rinbinomMu(int id, int size, double mu){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni == 1) {
    if (ISNA(size) || ISNA(mu)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    ind->simIni[id] = _rxnbinom_mu_(size, mu);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
IntegerVector rxnbinomMu_(int size, double mu, int n, int ncores){
  IntegerVector ret(n);
  int n2 = ret.size();
  int *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; thread++) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = (int)_rxnbinom_mu_(size, mu);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxnbinom(int size, double prob) {
  if (ISNA(size) || ISNA(prob)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0;
  return _rxnbinom__(size, prob);
}

extern "C" double rinbinom(int id, int size, double prob){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni == 1) {
    if (ISNA(size) || ISNA(prob)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    ind->simIni[id] =  _rxnbinom__(size, prob);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
IntegerVector rxnbinom_(double size, double prob, int n, int ncores){
  IntegerVector ret(n);
  int n2 = ret.size();
  int *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; thread++) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = (int)_rxnbinom__(size, prob);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}


extern "C" double rxbinom(int n, double prob){
  if (ISNA(n) || ISNA(prob)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0;
  boost::random::binomial_distribution<int> d(n, prob);
  return (double)d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double ribinom(int id, int n, double prob){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni == 1) {
    if (ISNA(n) || ISNA(prob)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::binomial_distribution<int> d(n, prob);
    ind->simIni[id] = (double) d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
IntegerVector rxbinom_(int n0, double prob, int n, int ncores){
  IntegerVector ret(n);
  int n2 = ret.size();
  boost::random::binomial_distribution<int> d(n0, prob);
  int *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; thread++) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxcauchy(double location, double scale){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ISNA(location) || ISNA(scale)) return NA_REAL;
  if (!ind->inLhs) return 0;
  boost::random::cauchy_distribution<double> d(location, scale);
  return d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double ricauchy(int id, double location, double scale){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni == 1) {
    if (ISNA(location) || ISNA(scale)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::cauchy_distribution<double> d(location, scale);
    ind->simIni[id] = d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxcauchy_(double location, double scale, int n, int ncores) {
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::cauchy_distribution<double> d(location, scale);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i+= ncores){
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxchisq(double df){
  if (ISNA(df)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0;
  // Non central not supported in C++11
  boost::random::chi_squared_distribution<double> d(df);
  return d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double richisq(int id, double df){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni == 1) {
    if (ISNA(df)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    // Non central not supported in C++11
    boost::random::chi_squared_distribution<double> d(df);
    ind->simIni[id] = d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxchisq_(double df, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::chi_squared_distribution<double> d(df);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxexp(double rate){
  if (ISNA(rate)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0;
  boost::random::exponential_distribution<double> d(rate);
  return d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double riexp(int id, double rate){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni) {
    if (ISNA(rate)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::exponential_distribution<double> d(rate);
    ind->simIni[id] = d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxexp_(double rate, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::exponential_distribution<double> d(rate);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxf(double df1, double df2) {
  if (ISNA(df1) || ISNA(df2)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0;
  boost::random::fisher_f_distribution<double> d(df1, df2);
  return d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double rif(int id, double df1, double df2){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni) {
    if (ISNA(df1) || ISNA(df2)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::fisher_f_distribution<double> d(df1, df2);
    ind->simIni[id] =  d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxf_(double df1, double df2, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::fisher_f_distribution<double> d(df1, df2);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

double rxgamma(rx_solving_options_ind* ind, double shape, double rate){
  if (ISNA(shape) || ISNA(rate)) return NA_REAL;
  // R uses rate; C++ uses scale
  if (!ind->inLhs) return 0;
  boost::random::gamma_distribution<double> d(shape, 1.0/rate);
  return d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double rxgamma(double shape, double rate){
  // R uses rate; C++ uses scale
  return rxgamma(&inds_thread[rx_get_thread(op_global.cores)], shape, rate);
}

extern "C" double rigamma(int id, double shape, double rate) {
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni) {
    if (ISNA(shape) || ISNA(rate)) {
      ind->simIni[id] = NA_REAL;
    } else {
      boost::random::gamma_distribution<double> d(shape, 1.0/rate);
      ind->simIni[id] =  d(_eng[rx_get_thread(op_global.cores)]);
    }
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxgamma_(double shape, double rate, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::gamma_distribution<double> d(shape, 1.0/rate);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}


extern "C" void _setThreadInd(int cid) {
  inds_thread[rx_get_thread(op_global.cores)] = rx_global.subjects[cid];
}

double rxbeta(rx_solving_options_ind* ind, double shape1, double shape2) {
  if (!ind->inLhs) return 0;
  // Efficient simulation when shape1 and shape2 are "large"
  // (p 658) Intro Prob Stats 8th Ed by Sheldon Ross
  double x = rxgamma(ind, shape1,1.0);
  return x/(x+rxgamma(ind, shape2, 1.0));
}

extern "C" double rxbeta(double shape1, double shape2) {
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  return rxbeta(ind, shape1, shape2);
}

extern "C" double ribeta(int id, double shape1, double shape2) {
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni == 1) {
    if (ISNA(shape1) || ISNA(shape2)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    int inLhs = ind->inLhs;
    ind->inLhs = 1;
    ind->simIni[id] = rxbeta(ind, shape1, shape2);
    ind->inLhs = inLhs;
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxbeta_(double shape1, double shape2, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::gamma_distribution<double> d1(shape1, 1.0);
  boost::random::gamma_distribution<double> d2(shape2, 1.0);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores){
        double x = d1(_eng[rx_get_thread(op_global.cores)]);
        retD[i] =  x/(x+d2(_eng[rx_get_thread(op_global.cores)]));
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxgeom(double prob){
  if (ISNA(prob)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0;
  boost::random::geometric_distribution<int> d(prob);
  return (double) d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double rigeom(int id, double prob){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni) {
    if (ISNA(prob)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::geometric_distribution<int> d(prob);
    ind->simIni[id] = (double)d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
IntegerVector rxgeom_(double prob, int n, int ncores){
  IntegerVector ret(n);
  int n2 = ret.size();
  boost::random::geometric_distribution<int> d(prob);
  int *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxnorm(double mean, double sd){
  if (ISNA(mean) || ISNA(sd)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0.0;
  boost::random::normal_distribution<double> d(mean, sd);
  return d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double rinorm(int id, double mean, double sd) {
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni) {
    if (ISNA(mean) || ISNA(sd)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::normal_distribution<double> d(mean, sd);
    ind->simIni[id] = d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxnorm_(double mean, double sd, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::normal_distribution<double> d(mean, sd);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores) {
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxpois(double lambda){
  if (ISNA(lambda)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0.0;
  boost::random::poisson_distribution<int> d(lambda);
  return (double)d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double ripois(int id, double lambda){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni == 1){
    if (ISNA(lambda)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::poisson_distribution<int> d(lambda);
    ind->simIni[id] = (double)d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
IntegerVector rxpois_(double lambda, int n, int ncores){
  IntegerVector ret(n);
  int n2 = ret.size();
  boost::random::poisson_distribution<int> d(lambda);
  int *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores) {
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxt_(double df){
  if (ISNA(df)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0.0;
  boost::random::student_t_distribution<double> d(df);
  return d(_eng[rx_get_thread(op_global.cores)]);
}

extern "C" double rit_(int id, double df){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind != NULL && ind->isIni == 1){
    if (ISNA(df)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::student_t_distribution<double> d(df);
    ind->simIni[id] =  d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxt__(double df, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::student_t_distribution<double> d(df);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores) {
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxunifmix(rx_solving_options_ind* ind) {
  std::uniform_real_distribution<double> d(0.0, 1.0);
  return d(_eng[rx_get_thread(op_global.cores)]);
}

double rxunif(rx_solving_options_ind* ind, double low, double hi) {
  if (ISNA(low) || ISNA(hi)) return NA_REAL;
  if (!ind->inLhs) return 0.0;
  if (low >= hi) return std::numeric_limits<double>::quiet_NaN();
  std::uniform_real_distribution<double> d(low, hi);
  return d(_eng[rx_get_thread(op_global.cores)]);
}
extern "C" double rxunif(double low, double hi) {
  return rxunif(&inds_thread[rx_get_thread(op_global.cores)], low, hi);
}

extern "C" double riunif(int id, double low, double hi) {
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind != NULL && ind->isIni == 1) {
    if (ISNA(low) || ISNA(hi)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    if (low >= hi) {
      ind->simIni[id] = std::numeric_limits<double>::quiet_NaN();
    } else {
      std::uniform_real_distribution<double> d(low, hi);
      ind->simIni[id] = d(_eng[rx_get_thread(op_global.cores)]);
    }
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxunif_(double low, double hi, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  std::uniform_real_distribution<double> d(low, hi);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores) {
        if (low >= hi) {
          retD[i] = std::numeric_limits<double>::quiet_NaN();
        } else {
          retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
        }
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}

extern "C" double rxweibull(double shape, double scale) {
  if (ISNA(shape) || ISNA(scale)) return NA_REAL;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (!ind->inLhs) return 0.0;
  boost::random::weibull_distribution<double> d(shape, scale);
  return d(_eng[rx_get_thread(op_global.cores)]);
}


extern "C" double riweibull(int id, double shape, double scale){
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->isIni) {
    if (ISNA(shape) || ISNA(scale)) {
      ind->simIni[id] = NA_REAL;
      return ind->simIni[id];
    }
    boost::random::weibull_distribution<double> d(shape, scale);
    ind->simIni[id] = d(_eng[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
NumericVector rxweibull_(double shape, double scale, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  boost::random::weibull_distribution<double> d(shape, scale);
  double *retD = ret.begin();

#ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
#endif

#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores){
        retD[i] = d(_eng[rx_get_thread(op_global.cores)]);
      }
    }
#ifdef _OPENMP
  }
#endif
  return ret;
}


bool anyFinite(arma::vec v){
  for (int i = v.size(); i--;){
    if (R_FINITE(v[i])) return true;
  }
  return false;
}

//[[Rcpp::export]]
SEXP rxRmvn0(NumericMatrix& A_, arma::rowvec mu, arma::mat sigma,
             arma::vec lower, arma::vec upper, int ncores=1, bool isChol=false,
             double a=0.4, double tol = 2.05, double nlTol=1e-10, int nlMaxiter=100){
  bool trunc = false;
  if (anyFinite(lower)){
    trunc = true;
  } else if (anyFinite(upper)){
    trunc = true;
  }
  if (trunc){
    arma::mat sigma0 = sigma;
    if (isChol){
      sigma0 = sigma * sigma.t();
    }
    IntegerVector dm = as<IntegerVector>(A_.attr("dim"));
    // int n = dm[0];
    arma::vec lower0 = fillVec(lower, dm[1]);
    arma::vec upper0 = fillVec(upper, dm[1]);
    arma::mat ret = rxMvrandn_(A_, mu, sigma0, lower0,
                               upper0, ncores, a, tol, nlTol,nlMaxiter);
    return R_NilValue;
  } else {
    return rxRmvn_(A_, mu, sigma, ncores, isChol);
  }
}

// Armadillo-only simulation (for simeta and simeps)
void rxRmvnA(arma::mat & A_, arma::rowvec & mu, arma::mat &sigma,
             arma::vec & lower, arma::vec & upper, int ncores=1, bool isChol=false,
             double a=0.4, double tol = 2.05, double nlTol=1e-10, int nlMaxiter=100){
  bool trunc = false;
  if (anyFinite(lower)){
    trunc = true;
  } else if (anyFinite(upper)){
    trunc = true;
  }
  if (trunc){
    arma::mat sigma0 = sigma;
    if (isChol){
      sigma0 = sigma * sigma.t();
    }
    arma::vec lower0 = fillVec(lower, A_.n_cols);
    arma::vec upper0 = fillVec(upper, A_.n_cols);
    rxMvrandn__(A_, mu, sigma0, lower0,
                upper0, ncores, a, tol, nlTol,nlMaxiter);
  } else {
    rxRmvn2_(A_, mu, sigma, ncores, isChol);
  }
}

void simvar(double *out, int type, int csim, rx_solve* rx) {
  int n = 0;
  if (type == 0) { // eps
    n = rx->neps;
  } else {
    n = rx->neta;
  }
  arma::mat A(&out[0], 1, n, false, true);
  arma::vec lower = getLowerVec(type, rx);
  arma::vec upper = getUpperVec(type, rx);
  arma::rowvec mu(n, arma::fill::zeros);
  arma::mat sigma = getArmaMat(type, csim, rx);

  // FIXME? allow changing of a, tol nlTol and nlMaxiter?
  rxRmvnA(A, mu, sigma, lower, upper, 1, false, 0.4, 2.05, 1e-10, 100);
}
extern "C" void simeps(void) {
  rx_solve* rx = &rx_global;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind->inLhs == 1) { // only change while calculating the lhs
    // In this case the par_ptr will be updated with the new values, but they are out of order
    arma::mat out(1, rx->neps);
    // ind->id  = csub+csim*nsub;
    int csim = std::floor(((double)ind->id)/((double)rx->nsub));
    simvar(&out[0], 0, csim, rx);
    int *svar = rx->svar;
    double *par_ptr = ind->par_ptr;
    for (int j=0; j < rx->neps; j++){
      // The error pointer is updated if needed
      par_ptr[svar[j]] = out[j];
    }
  }
}


extern "C" void simeta(void) {
  rx_solve* rx = &rx_global;
  rx_solving_options_ind* ind = &inds_thread[rx_get_thread(op_global.cores)];
  if (ind != NULL && ind->isIni == 1) { // only initialize at beginning
    // In this case the par_ptr will be updated with the new values, but they are out of order
    arma::mat out(1, rx->neta);
    // ind->id  = csub+csim*nsub;
    int csim = std::floor((double)(ind->id)/((double)rx->nsub));
    simvar(&out[0], 1, csim, rx);
    double *par_ptr = ind->par_ptr;
    for (int j=0; j < rx->neta; j++){
      // The error pointer is updated if needed
      // REprintf("j: %d\n", j );
      // REprintf("ovar[j]: %d\n", op->ovar[j]);
      par_ptr[rx->ovar[j]] = out[j];
      // REprintf("out[j]: %d\n", out[j]);
    }
  }
}

//[[Rcpp::export]]
SEXP rxRmvnSEXP(SEXP nS,
                SEXP muS, SEXP sigmaS,
                SEXP lowerS, SEXP upperS, SEXP ncoresS, SEXP isCholS,
                SEXP keepNamesS,
                SEXP aS, SEXP tolS, SEXP nlTolS, SEXP nlMaxiterS){
  BEGIN_RCPP
    int type = TYPEOF(sigmaS);
  bool isSigmaList = (type == VECSXP);
  int d=0;
  NumericMatrix sigma0;
  NumericVector muNV;
  List sigmaList;
  CharacterVector dimnames;
  arma::mat sigma;
  NumericMatrix A;
  int n=0;
  qassertS(aS, "R1[0,)", "a");
  double a = as<double>(aS);
  qassertS(tolS, "R1[0,)", "tol");
  double tol = as<double>(tolS);
  qassertS(nlTolS, "R1[0,)", "nlTol");
  double nlTol=as<double>(nlTolS);
  qassertS(nlMaxiterS, "X1[0,)", "nlMaxiter");
  int nlMaxiter=as<int>(nlMaxiterS);
  qassertS(lowerS, "R+", "lower");
  NumericVector lowerIn = as<NumericVector>(lowerS);
  NumericVector lowerNV;
  qassertS(ncoresS, "X1[1,)", "ncores");
  int ncores = as<int>(ncoresS);
  // arma::vec lower, arma::vec upper, int ncores=1, bool isChol=false,
  qassertS(isCholS, "B1", "isChol");
  bool isChol = as<bool>(isCholS);
  qassertS(keepNamesS, "B1", "keepNames");
  bool keepNames = as<bool>(keepNamesS);

  // get sigma0
  if (isSigmaList){
    sigmaList = as<List>(sigmaS);
    if (sigmaList.size() == 0) {
      stop(_("when 'sigma' is a list, it has to have at least 1 element"));
    }
    if (!Rf_isMatrix(sigmaList[0])){
      stop(_("'sigma' must be a list of square symmetric matrices"));
    }
    qassertS(sigmaList[0], "M", "sigma[[0]]");
    sigma0 = as<NumericMatrix>(sigmaList[0]);
    d  = sigma0.nrow();
    if (d != sigma0.ncol()) {
      stop("'sigma[[0]]' must a square matrix");
    }
    for (int i = sigmaList.size()-1; i--;){
      NumericMatrix sigmaTmp = as<NumericMatrix>(sigmaList[i]);
      if (d != sigmaTmp.nrow() || d != sigmaTmp.ncol()) {
        stop(_("'sigma' list element %d does not match dimension of first matrix"), i+1);
      }
    }
  } else if (Rf_isMatrix(sigmaS)) {
    qassertS(sigma0, "M", "sigma");
    sigma0 = as<NumericMatrix>(sigmaS);
    d = sigma0.nrow();
  } else {
    qassertS(sigma0, "M", "sigma");
  }
  if (Rf_isMatrix(nS)) {
    A = as<NumericMatrix>(nS);
    n = A.nrow();
  } else {
    qassertS(nS, "X1[1,)", "n");
    n = as<int>(nS);
    A = NumericMatrix(n, d);
  }
  if (sigma0.hasAttribute("dimnames")){
    List curL = as<List>(sigma0.attr("dimnames"));
    if (!Rf_isNull(curL[0])){
      dimnames = as<CharacterVector>(curL[0]);
    } else {
      dimnames = as<CharacterVector>(curL[1]);
    }
  }
  // get mu
  if (Rf_isNull(muS)) {
    muNV = NumericVector(d);
    std::fill(muNV.begin(),muNV.end(), 0);
  } else if (qtest(muS, "I+")) {
    muNV = as<NumericVector>(muS);
  } else {
    qassertS(muS, "R+", "mu");
    muNV = as<NumericVector>(muS);
  }
  arma::rowvec mu = as<arma::rowvec>(muNV);
  if (muNV.size() != d) {
    stop(_("'mu' length must match 'sigma' dimensions"));
  }
  if (muNV.hasAttribute("names")) {
    if (dimnames.size() == 0) {
      dimnames = as<CharacterVector>(muNV.attr("names"));
    } else {
      CharacterVector dimnames2 = as<CharacterVector>(muNV.attr("names"));
      if (dimnames2.size() != dimnames.size()) stop("malformed 'muNV' 'sigma'");
      for (int i = dimnames2.size(); i--;) {
        if (as<std::string>(dimnames[i]) != as<std::string>(dimnames2[i])) {
          stop("'mu' and 'sigma' names have to have the same order");
        }
      }
    }
  }
  if (lowerIn.size() == 1) {
    lowerNV = NumericVector(d);
    std::fill(lowerNV.begin(), lowerNV.end(), lowerIn[0]);
  } else {
    if (lowerIn.size() != d)
      stop("'lower' needs to match the dimension of 'mu' and 'sigma' or be 1");
    lowerNV = lowerIn;
  }
  NumericVector upperIn = as<NumericVector>(upperS);
  NumericVector upperNV;
  if (upperIn.size() == 1) {
    upperNV = NumericVector(d);
    std::fill(upperNV.begin(), upperNV.end(), upperIn[0]);
  } else {
    if (upperIn.size() != d)
      stop("'upper' needs to match the dimension of 'mu' and 'sigma' or be 1");
    upperNV = upperIn;
  }
  for (int i = 0; i < d; ++i) {
    if (upperNV[i] <= lowerNV[i]) {
      stop("'upper[%d]' <= 'lower[%d]'", i+1, i+1);
    }
  }
  arma::vec lower = as<arma::vec>(lowerNV);
  arma::vec upper = as<arma::vec>(upperNV);
  SEXP retS;
  if (isSigmaList) {
    int sListN = sigmaList.size();
    NumericMatrix retNM(n*sListN, d);
    for (int i = 0; i < sListN; ++i) {
      NumericMatrix Acur(n, d);
      sigma=as<arma::mat>(sigmaList[i]);
      rxRmvn0(Acur, mu, sigma, lower, upper, ncores, isChol,
              a, tol, nlTol, nlMaxiter);
      for (int j = 0; j < d; ++j) {
        std::copy(Acur.begin()  + n*j,
                  Acur.begin()  + n*(j+1),
                  retNM.begin() + n*sListN*j + i*n);
      }
    }
    if (keepNames && dimnames.size() > 0) {
      retNM.attr("dimnames") = List::create(R_NilValue, dimnames);
    }
    retS = as<SEXP>(retNM);
  } else {
    sigma=as<arma::mat>(sigma0);
    rxRmvn0(A, mu, sigma, lower, upper, ncores, isChol,
            a, tol, nlTol, nlMaxiter);
    if (keepNames && dimnames.size() > 0) {
      A.attr("dimnames") = List::create(R_NilValue, dimnames);
    }
    retS = as<SEXP>(A);
  }
  return retS;
  END_RCPP
    }

static inline NumericVector rpp_h(int n, double& lambda, double& t0i, double &tmax){
  boost::random::exponential_distribution<double> d(lambda);
  double t0 = t0i;
  NumericVector ret(n);
  for (int i = 0; i < n; ++i){
    t0  += d(_eng[rx_get_thread(op_global.cores)]);
    if (t0 >= tmax){
      while (i < n){
        ret[i++] = tmax;
      }
    } else {
      ret[i] = t0;
    }
  }
  return ret;
}

static inline SEXP wrapRandom(NumericVector ret0, bool randomOrder){
  if (randomOrder){
    IntegerVector r0 = sample(ret0.size(), ret0.size(), false, R_NilValue, false);
    NumericVector ret(ret0.size());
    for (int i = r0.size(); i--;){
      ret[r0[i]] = ret0[i];
    }
    return wrap(ret);
  } else {
    return wrap(ret0);
  }
}

//[[Rcpp::export]]
NumericVector rpp_(SEXP nS, SEXP lambdaS, SEXP gammaS, SEXP probS, SEXP t0S, SEXP tmaxS,
                   SEXP randomOrderS) {
  double t0 = asDouble(t0S, "t0");
  double lambda = asDouble(lambdaS, "lambda");
  int n = asInt(nS, "n");
  bool randomOrder = asBool(randomOrderS, "randomOrder");
  double tmax = asDouble(tmaxS, "tmax");
  seedEng(1);
  if (Rf_isNull(probS)) {
    if (Rf_isNull(gammaS)){
      // Homogenous case
      return wrapRandom(rpp_h(n, lambda, t0, tmax), randomOrder);
    } else {
      double gamma = asDouble(gammaS, "gamma");
      if (gamma == 1.0){
        // Constant dropout
        return wrapRandom(rpp_h(n, lambda, t0, tmax), randomOrder);
      } else {
        int cur = 0;
        NumericVector ret(n);
        std::uniform_real_distribution<double> runif(0.0, 1.0);
        boost::random::exponential_distribution<double> rexp(lambda);
        double ttest;
        while (cur != n){
          ttest = t0 + rexp(_eng[rx_get_thread(op_global.cores)]);
          if (runif(_eng[rx_get_thread(op_global.cores)]) < gamma*pow(ttest/tmax , gamma-1.0)){
            if (ttest >= tmax){
              // Reached the maxumim event, fill with max.
              while (cur != n){
                ret[cur++] = tmax;
              }
            } else {
              ret[cur++] = ttest;
              t0 = ttest;
            }
          }
        }
        return wrapRandom(ret, randomOrder);
      }
    }
  } else {
    // Non homogenous case
    NumericVector ret(n);
    Function prob = as<Function>(probS);
    NumericVector s0;
    NumericVector p0;
    NumericVector u0;// = rxunif_(0.0, 1.0, 2*n, 1);
    if (p0.size() != s0.size()){
      stop(_("'prob' function should return a vector the same length as the input"));
    }
    int cur = 0;
    double tmaxf = t0;
    while (cur != n){
      s0 = rpp_h(2*n, lambda, tmaxf, tmax);
      p0 = prob(s0);
      u0 = rxunif_(0.0, 1.0, 2*n, 1);
      for (int i = 0; i < 2*n; i++){
        if (p0[i] < 0) stop("'prob' function should return values between 0 and 1");
        if (p0[i] > 1) stop("'prob' function should return values between 0 and 1");
        if (u0[i] < p0[i]) {
          double val = s0[i];
          if (val >= tmax){
            // Already reached the maximum event horizon fill with max.
            while (cur != n){
              ret[cur++] = s0[i];
            }
          } else {
            ret[cur++] = s0[i];
            tmaxf = s0[i];
            if (cur == n) break;
          }
        }
      }
    }
    return wrapRandom(ret, randomOrder);
  }
  return R_NilValue;
}


//[[Rcpp::export]]
double rxordSelect(double u, NumericVector cs) {
  unsigned int n = (unsigned int)(cs.size());
  double ret=0.0;
  for (unsigned int i = 0; i < n; i++) {
    if (ret < 1e-6) {
      if (u < cs[i]) {
        ret = (double)(i+1);
      }
    }
  }
  if (ret < 1e-6) ret = (double)(n+1);
  return ret;
}
