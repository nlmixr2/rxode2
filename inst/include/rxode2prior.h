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
// parameters and/or omega diagonal elements), an inverse-Wishart
// degrees-of-freedom penalty on an omega block -- either the textbook
// inverse-Wishart density (type 3, the "general" method) or NONMEM's own
// $PRIOR NWPRI parameterization (type 4, the "nwpri" method), which are
// NOT the same density: NONMEM7 Technical Guide eq. 1.157/1.159/1.170
// (Appendix on prior information) uses degrees of freedom
// d_W = rho + n + 1 (the "modal"/non-BAYES convention) and scale rho*Psi,
// not rho/Psi directly -- deriving type 4 by plugging those into the
// type-3 formula gives the wrong answer (rederiving eq. 1.157 directly and
// simplifying eq. 1.159 with d_W-n-1=rho both agree on the closed form
// type 4 actually implements) -- or a joint multivariate-normal block on
// the DIAGONAL entries of chol(Omega^-1) rather than on the raw omega
// values (type 5, the "tnpri" method, matching how nlmixr2est's own FOCEI
// already parameterizes the omega optimization -- op_focei.cholOmegaInv,
// src/inner.cpp -- and the analogous assumption Monolix's Bayesian
// estimation makes). Type 5's members address a POSITION IN A CHOLESKY
// FACTOR, not a raw omega value, so it carries its own block bookkeeping
// (nBlocks/blockDim/blockEtaIdx below); there is no direct way to name an
// off-diagonal Cholesky entry -- the correlation between two omega
// elements' diagonal Cholesky entries only ever enters through the joint
// term's own Sigma (eg `om.eta1 + om.eta2 ~ c(1, 0.1, 1)`), the same way
// type 2 already lets multiple members covary. Neither type 4 nor type 5
// has a Cauchy analogue -- type 1 exists only for the "general" method.
//
// `thetaIdx[k]`/`etaIdx[k]` are 1-based positions in the caller's own
// `theta`/`omega` arrays for member `k` (exactly one of the pair is
// nonzero) -- the same `ntheta`/`neta1` numbering rxode2's `iniDf` already
// assigns, which is also the numbering nlmixr2est's own op_focei uses for
// its theta vector and omega matrix, so the evaluator never needs a name
// lookup. For type 5, `etaIdx[k]` still means "the diagonal position for
// this eta", just interpreted as addressing chol(Omega_block^-1)[i,i]
// rather than Omega[i,i] directly -- which block, and that block's other
// (possibly unreferenced) members, come from `blockDim`/`blockEtaIdx`.
typedef struct rx_prior_term_t {
  int type;      // 0=normal, 1=cauchy, 2=multiNormal, 3=invWishart (general),
                 // 4=invWishart (NONMEM NWPRI), 5=multiNormal on chol(Omega^-1)
                 // diagonal (NONMEM/Monolix TNPRI)
  int n;         // number of members (1 for normal/cauchy)
  int *thetaIdx; // length n; 0 when member k is an omega element
  int *etaIdx;   // length n; 0 when member k is a population parameter
  double *mu;    // length n; unused (NULL) for invWishart
  double *scale; // normal/cauchy: length-1 sd/scale. multiNormal/tnpri: n*n
                 // row-major covariance Sigma. invWishart (3 or 4): n*n
                 // row-major scale matrix Psi (the block's own ini() values).
  double lower;  // truncation bounds (normal/cauchy only; +-Inf otherwise)
  double upper;
  double nu;     // invWishart (3): classical degrees of freedom. invWishart
                 // (4): NONMEM's "rho" -- the invWishart(rho) argument as
                 // written, i.e. NOT d_W.
  // type 5 (tnpri) only: the distinct omega blocks this term's members
  // span. A block must be handled in full (Omega_block^-1 and its
  // Cholesky depend on every entry, referenced or not), so each block
  // carries its own full eta index list, in local (Cholesky row/col)
  // order. Concatenated across blocks, length sum(blockDim); block b's
  // slice starts at sum(blockDim[0..b-1]).
  int nBlocks;
  int *blockDim;     // length nBlocks
  int *blockEtaIdx;  // length sum(blockDim)
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
//
// `spec` comes from R/priorDensity.R's rxPriorBuildSpec(), an R external
// pointer whose finalizer is the ONLY thing that frees it -- there is
// deliberately no exported "free this spec now" entry point. A raw void*
// obtained via R_ExternalPtrAddr() and a same-process finalizer both firing
// on the same allocation is a double free; the caller must instead keep
// the R external pointer object itself alive (referenced from wherever it
// stores its own fit state) for as long as it calls this function, and let
// R's garbage collector reclaim it in the ordinary way once that reference
// is dropped.
double rxPriorLogDensityEval(const rx_prior_spec_t *spec,
                              const double *theta, int thetaLen,
                              const double *omega, int omegaDim,
                              double *gradTheta, double *gradOmega);

#if defined(__cplusplus)
}
#endif
#endif

#endif
