#define USE_FC_LEN_T
#define STRICT_R_HEADERS
// [[Rcpp::interfaces(r, cpp)]]
//#undef NDEBUG
#include "rxomp.h"
#define min2( a , b )  ( (a) < (b) ? (a) : (b) )
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"
#include <vandercorput.h>
#include "seed.h"
#include <R.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("rxode2", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
using namespace Rcpp;
using namespace arma;

std::vector<sitmo::vandercorput> _engV;
void seedEngV(uint32_t seed, int ncores){
  _engV.clear();  
  for (int i= 0; i < ncores; i++) {
    sitmo::vandercorput eng0;
    eng0.seed(seed + i);
    _engV.push_back(eng0);
  }
}

extern "C" void setSeedEng1V(uint32_t seed) {
  (_engV[rx_get_thread(op_global.cores)]).seed(seed);
}


extern "C" double rxnormV(rx_solving_options_ind* ind, double mean, double sd){
  if (!ind->inLhs) return 0;
  std::normal_distribution<double> d(mean, sd);
  return d(_engV[rx_get_thread(op_global.cores)]);
}

extern "C" double rinormV(rx_solving_options_ind* ind, int id, double mean, double sd){
  if (ind->isIni == 1) {
    std::normal_distribution<double> d(mean, sd);
    ind->simIni[id] = d(_engV[rx_get_thread(op_global.cores)]);
  }
  return ind->simIni[id];
}

//[[Rcpp::export]]
arma::mat rxrandnV(unsigned int nrow, unsigned int ncol){
  arma::mat ret(nrow, ncol);
  std::normal_distribution<double> d(0.0, 1.0);
  for (int j = nrow; j--;) {
    for (int i = ncol; i--;) {
      ret(j,i) = d(_engV[rx_get_thread(op_global.cores)]);
    }
  }
  return ret;
}


//[[Rcpp::export]]
NumericVector rxnormV_(double mean, double sd, int n, int ncores){
  NumericVector ret(n);
  int n2 = ret.size();
  uint32_t seed = getRxSeed1(ncores);
  double *A  = ret.begin();
  std::normal_distribution<double> d(mean, sd);
  #ifdef _OPENMP
#pragma omp parallel num_threads(ncores) if(ncores > 1)
  {
    seed += rx_get_thread(op_global.cores);
#endif
    sitmo::vandercorput eng;
    eng.seed(seed);
#ifdef _OPENMP
#pragma omp for schedule(static)
#endif
    for (int thread = 0; thread < ncores; ++thread) {
      for (int i = thread; i < n2; i += ncores){
	A[i] = d(eng);
      }
    }
  #ifdef _OPENMP
  }
  #endif
  return ret;
}
