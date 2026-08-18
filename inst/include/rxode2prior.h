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
// type 4 actually implements). Neither has a Cauchy analogue -- type 1
// exists only for the "general" method.
//
// A "tnpri" prior (Monolix's Bayesian-estimation assumption that ALL
// estimated parameters are jointly normal, and NONMEM's own estimation
// applies the same assumption to omega) needs NO term type of its own:
// the prior is specified, and evaluated, on the RAW omega value, exactly
// like type 0/2 already do for a "general" om.<eta> member -- there is no
// Cholesky decomposition anywhere in the TERM EVALUATION. R/priorDensity.R's
// "tnpri" method therefore builds the exact same type 0/2 terms "general"
// would for an om.<eta> member; the only difference is which distributions
// it refuses (Cauchy, like "nwpri", and invWishart(), which is "nwpri"'s
// own mechanism).
//
// A caller whose optimizer varies cholOmegaInv internally instead of Omega
// itself (FOCEI, op_focei.cholOmegaInv in nlmixr2est/src/inner.cpp) still
// needs the natural-scale gradient this kernel returns chain-ruled into its
// own parameterization -- that conversion (NOT part of evaluating the
// prior itself) is rxPriorOmegaToCholOmegaInvGrad() below. A method that
// estimates Omega directly (SAEM) skips that step and uses the returned
// gradOmega as-is.
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

// Chain-rules a natural (raw omega-scale) gradient for ONE omega block --
// from rxPriorLogDensityEval()'s gradOmega, or from anywhere else -- into
// the equivalent gradient w.r.t. that block's own
// U = chol(Omega_block^-1) (upper triangular, Omega_block^-1 = U^T U,
// nlmixr2est's own op_focei.cholOmegaInv convention -- src/rxInv.cpp's
// rxToCholOmega() treats it as trimatu()). Not specific to any prior term
// type: ANY omega-space gradient a caller has needs this same conversion
// to be usable by an optimizer that varies cholOmegaInv directly (FOCEI).
//
// Derived via the standard matrix-inverse VJP (Abar = -Omega Gsym Omega,
// since Omega = (Omega^-1)^-1) composed with the VJP of A = U^T U
// (Ubar = upperTriangle(2 U Abar)); verified against central-difference
// gradients of the full U -> Omega(U) -> objective pipeline before use
// here. Gsym is gradOmegaBlock symmetrized ((G + G^T)/2) internally before
// use: Omega(U) is symmetric by construction, so only gradOmegaBlock's
// symmetric part has any effect on a directional derivative along the U
// manifold -- gradOmegaBlock need not already be symmetric on entry (the
// entrywise convention rxPriorLogDensityEval's own gradOmega uses is
// accepted as-is; its antisymmetric part, if any, is correctly dropped
// here rather than silently corrupting the result).
//
// omegaBlock/gradOmegaBlock are the block's own p x p row-major
// submatrices; gradCholOmegaInv (output, p x p row-major) is populated in
// full, with its strict lower triangle (which addresses no free parameter
// of U) set to exactly 0. Returns false (gradCholOmegaInv untouched) if
// omegaBlock is not positive definite. No R/Rcpp call of any kind -- safe
// to call from any OpenMP thread.
bool rxPriorOmegaToCholOmegaInvGrad(const double *omegaBlock, int p,
                                    const double *gradOmegaBlock,
                                    double *gradCholOmegaInv);

#if defined(__cplusplus)
}
#endif
#endif

#endif
