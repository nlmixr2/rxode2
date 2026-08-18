#ifndef __RXODE2PRIOR_H__
#define __RXODE2PRIOR_H__

// Plain-C data layout for the shared prior log-density kernel
// (nlmixr2/nlmixr2est#929).  No R types appear anywhere in this header --
// `rxPriorLogDensityEval()` (src/priorDensity.cpp) is meant to be called
// from inside an OpenMP-parallel objective/gradient evaluation, where
// touching the R API (even indirectly, through Rf_error()/R_alloc()) is not
// thread safe.

#if defined(__cplusplus)
extern "C" {
#endif

// One prior term: a single (possibly truncated) normal/Cauchy penalty on
// one member, a joint multivariate-normal block (spanning population
// parameters and/or omega diagonal elements), or an inverse-Wishart
// degrees-of-freedom penalty on an omega block -- either the textbook
// inverse-Wishart density (type 3, the "general" method) or NONMEM's own
// $PRIOR NWPRI parameterization (type 4, the "nwpri" method), which are
// NOT the same density: NONMEM7 Technical Guide eq. 1.157/1.159/1.170
// (Appendix on prior information) uses degrees of freedom
// d_W = rho + n + 1 (the "modal"/non-BAYES convention) and scale rho*Psi,
// not rho/Psi directly -- deriving type 4 by plugging those into the
// type-3 formula gives the wrong answer (rederiving eq. 1.157 directly and
// simplifying eq. 1.159 with d_W-n-1=rho both agree on the closed form
// type 4 actually implements). NONMEM's own prior machinery has no Cauchy
// analogue at all -- type 1 exists only for the "general" method.
//
// `thetaIdx[k]`/`etaIdx[k]` are 1-based positions in the caller's own
// `theta`/`omega` arrays for member `k` (exactly one of the pair is
// nonzero) -- the same `ntheta`/`neta1` numbering rxode2's `iniDf` already
// assigns, which is also the numbering nlmixr2est's own op_focei uses for
// its theta vector and omega matrix, so the evaluator never needs a name
// lookup.
typedef struct rx_prior_term_t {
  int type;      // 0=normal, 1=cauchy, 2=multiNormal, 3=invWishart (general),
                 // 4=invWishart (NONMEM NWPRI)
  int n;         // number of members (1 for normal/cauchy)
  int *thetaIdx; // length n; 0 when member k is an omega element
  int *etaIdx;   // length n; 0 when member k is a population parameter
  double *mu;    // length n; unused (NULL) for invWishart
  double *scale; // normal/cauchy: length-1 sd/scale. multiNormal: n*n
                 // row-major covariance Sigma. invWishart (3 or 4): n*n
                 // row-major scale matrix Psi (the block's own ini() values).
  double lower;  // truncation bounds (normal/cauchy only; +-Inf otherwise)
  double upper;
  double nu;     // invWishart (3): classical degrees of freedom. invWishart
                 // (4): NONMEM's "rho" -- the invWishart(rho) argument as
                 // written, i.e. NOT d_W.
} rx_prior_term_t;

typedef struct rx_prior_spec_t {
  int nTerms;
  rx_prior_term_t *terms;
} rx_prior_spec_t;

#if defined(__cplusplus)
}
#endif

// Its address is exported to downstream packages through the rxode2
// function-pointer table (see _rxode2_rxode2Ptr in src/init.c and
// rxode2ptr.h); these direct declarations are used only when building
// rxode2 itself (guarded off once rxode2ptr.h has redeclared the names as
// function pointers), the same way rxode2AdjointSweep is declared below.
#ifndef __RXODE2PTR_H__
#if defined(__cplusplus)
extern "C" {
#endif

// Value and gradient of a model's prior log density, on the natural scale.
// Sums every term's contribution, ADDING into gradTheta/gradOmega (the
// caller zeroes them first). `theta` has length `thetaLen`; `omega` is
// `omegaDim` x `omegaDim`, row-major. No R/Rcpp call of any kind -- safe
// to call from any OpenMP thread. A term whose live covariance (omega, or
// an omega submatrix) is not positive definite contributes -INFINITY to
// the value rather than touching an R error path; its gradient
// contribution is left at 0 for that term.
double rxPriorLogDensityEval(const rx_prior_spec_t *spec,
                              const double *theta, int thetaLen,
                              const double *omega, int omegaDim,
                              double *gradTheta, double *gradOmega);

// Free a spec built by rxPriorBuildSpec() (R/priorDensity.R's
// .Call("_rxode2_rxPriorBuildSpec", ...)). Also R/Rcpp free, safe to call
// from a non-R-owning thread once the caller is done with the fit.
void rxPriorFreeSpec(void *spec);

#if defined(__cplusplus)
}
#endif
#endif

#endif
