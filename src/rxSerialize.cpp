// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: nil; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
// [[Rcpp::interfaces(r,cpp)]]
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"
#include "../inst/include/rxMemoryCalc.h"
#include "../inst/include/rxode2parse_control.h"
#include "rxGlobals.h"
#include <stdio.h>
#include <string.h>
#include <stdint.h>

using namespace Rcpp;

extern "C" rx_solve *getRxSolve_(void);
extern rx_globals _globals;

static const char rxSerializeMagic[8] = {'R','X','O','D','E','2','S','Z'};
static const uint32_t rxSerializeFormatVer = 1u;

// ---------------------------------------------------------------------------
// Low-level write helpers — all abort via Rf_error on failure
// ---------------------------------------------------------------------------

static void sWrite(FILE *f, const void *buf, size_t n, const char *what) {
  if (fwrite(buf, 1, n, f) != n)
    (Rf_error)("rxSaveState: write error writing %s", what);
}

static void sWriteU32(FILE *f, uint32_t v, const char *what) {
  sWrite(f, &v, sizeof(v), what);
}

static void sWriteU64(FILE *f, uint64_t v, const char *what) {
  sWrite(f, &v, sizeof(v), what);
}

static void sWriteI32(FILE *f, int32_t v, const char *what) {
  sWrite(f, &v, sizeof(v), what);
}

// Write a size-prefixed blob: uint64_t count then count elements of width w.
static void sWriteBlob(FILE *f, const void *data, uint64_t count, size_t w,
                       const char *what) {
  sWriteU64(f, count, what);
  if (count > 0 && data != NULL)
    sWrite(f, data, count * w, what);
}

static void sWriteDoubleBlob(FILE *f, const double *data, uint64_t n,
                             const char *what) {
  sWriteBlob(f, data, n, sizeof(double), what);
}

static void sWriteIntBlob(FILE *f, const int *data, uint64_t n,
                          const char *what) {
  sWriteBlob(f, data, n, sizeof(int), what);
}

// ---------------------------------------------------------------------------
// vLines serialization
// ---------------------------------------------------------------------------

static void sWriteVLines(FILE *f, const vLines *vl, const char *what) {
  uint32_t n = (uint32_t)(vl->n > 0 ? vl->n : 0);
  sWriteU32(f, n, what);
  for (uint32_t i = 0; i < n; i++) {
    const char *s = (vl->line && vl->line[i]) ? vl->line[i] : "";
    uint32_t len = (uint32_t)strlen(s) + 1u;
    sWriteU32(f, len, what);
    sWrite(f, s, len, what);
  }
  if (n > 0) {
    if (vl->lProp) sWrite(f, vl->lProp, n * sizeof(int), what);
    else { for (uint32_t i = 0; i < n; i++) sWriteI32(f, 0, what); }
    if (vl->lType) sWrite(f, vl->lType, n * sizeof(int), what);
    else { for (uint32_t i = 0; i < n; i++) sWriteI32(f, 0, what); }
  }
}

// ---------------------------------------------------------------------------
// rxSaveState_: write live pre-integration rx_solve state to file
// ---------------------------------------------------------------------------

//' Save the pre-integration rxode2 solver state to a binary file
//'
//' Called internally after event-table and parameter setup but before ODE
//' integration.  The file can later be restored with rxRestoreState_().
//'
//' @param path File path to write (created/overwritten).
//' @return Invisibly TRUE.
//' @noRd
// [[Rcpp::export]]
SEXP rxSaveState_(SEXP pathSexp) {
  const char *path = CHAR(STRING_ELT(pathSexp, 0));

  FILE *f = fopen(path, "wb");
  if (!f) (Rf_error)("rxSaveState: cannot open '%s' for writing", path);

  rx_solve *rx = getRxSolve_();
  rx_solving_options *op = rx->op;

  // ── Section 1: Header ────────────────────────────────────────────────────
  sWrite(f, rxSerializeMagic, 8, "magic");
  sWriteU32(f, rxSerializeFormatVer, "format_ver");

  // Package version via R
  std::string pkgVer;
  try {
    Function fmtFn("format");
    Function pkgVerFn("packageVersion");
    pkgVer = as<std::string>(as<CharacterVector>(fmtFn(pkgVerFn("rxode2")))[0]);
  } catch (...) {
    pkgVer = "unknown";
  }
  uint32_t vlen = (uint32_t)pkgVer.size() + 1u;
  sWriteU32(f, vlen, "pkg_ver_len");
  sWrite(f, pkgVer.c_str(), vlen, "pkg_ver");

  sWriteU32(f, (uint32_t)sizeof(rx_solving_options),     "sizeof_opts");
  sWriteU32(f, (uint32_t)sizeof(rx_solving_options_ind), "sizeof_ind");
  sWriteU32(f, (uint32_t)sizeof(rx_solve),               "sizeof_solve");

  // ── Section 2: rx_solving_options scalar POD fields ──────────────────────
  // Write each field individually (skip pointer fields).
#define W_I32(field) sWriteI32(f, (int32_t)(op->field), #field)
#define W_U32(field) sWriteU32(f, (uint32_t)(op->field), #field)
#define W_DBL(field) sWrite(f, &(op->field), sizeof(double), #field)
#define W_BOOL(field) { uint8_t _b = (uint8_t)(op->field); sWrite(f, &_b, 1, #field); }

  W_I32(badSolve); W_I32(naTime); W_I32(naTimeInput); W_I32(naTimeInputWarn);
  W_DBL(ATOL); W_DBL(RTOL); W_DBL(H0); W_DBL(HMIN);
  W_I32(mxstep); W_I32(MXORDN); W_I32(MXORDS);
  W_I32(nlhs); W_I32(neq); W_I32(stiff); W_I32(ncov);
  sWrite(f, op->modNamePtr, sizeof(op->modNamePtr), "modNamePtr");
  W_BOOL(do_par_cov);
  W_I32(is_locf); W_I32(instant_backward); W_I32(keep_interp);
  W_I32(cores); W_I32(doesRandom); W_I32(extraCmt);
  W_DBL(hmax2);
  W_I32(indLinN); W_DBL(indLinPhiTol); W_I32(indLinPhiM);
  W_I32(indLinMatExpType); W_I32(indLinMatExpOrder);
  W_I32(nDisplayProgress); W_I32(ncoresRV); W_I32(isChol);
  W_I32(nsvar); W_I32(abort); W_I32(minSS); W_I32(maxSS);
  W_I32(doIndLin); W_I32(strictSS);
  W_DBL(infSSstep); W_I32(mxhnil); W_DBL(hmxi);
  W_I32(nLlik); W_I32(numLinSens); W_I32(numLin);
  W_I32(depotLin); W_I32(linOffset); W_I32(ssSolved);
  W_I32(indOwnAlloc);

#undef W_I32
#undef W_U32
#undef W_DBL
#undef W_BOOL

  // ── Section 3: rx_solve scalar POD fields ────────────────────────────────
#define W_RX_U32(field) sWriteU32(f, (uint32_t)(rx->field), #field)
#define W_RX_I32(field) sWriteI32(f, (int32_t)(rx->field), #field)
#define W_RX_I64(field) sWrite(f, &(rx->field), sizeof(int64_t), #field)
#define W_RX_DBL(field) sWrite(f, &(rx->field), sizeof(double), #field)
#define W_RX_BOOL(field) { uint8_t _b = (uint8_t)(rx->field); sWrite(f, &_b, 1, #field); }

  W_RX_U32(nsub); W_RX_U32(nsim);
  W_RX_I32(neta); W_RX_I32(neps); W_RX_I32(nIndSim); W_RX_I32(simflg);
  W_RX_U32(nall); W_RX_I32(nevid9); W_RX_I32(nobs); W_RX_I32(nobs2);
  W_RX_I64(nr);
  W_RX_I32(add_cov); W_RX_I32(matrix); W_RX_I32(needSort);
  W_RX_I32(nMtime);
  W_RX_DBL(stateTrimU); W_RX_DBL(stateTrimL);
  W_RX_I32(nCov0); W_RX_I32(nKeepF); W_RX_I32(istateReset);
  W_RX_I32(cens); W_RX_I32(limit);
  W_RX_I32(safeZero); W_RX_I32(safeLog); W_RX_I32(safePow);
  W_RX_I32(sumType); W_RX_I32(prodType);
  sWrite(f, rx->factorNs, sizeof(rx->factorNs), "factorNs");
  W_RX_I32(hasFactors); W_RX_I32(maxAllTimes);
  W_RX_I32(hasEvid2); W_RX_I32(useStdPow);
  W_RX_BOOL(ss2cancelAllPending);
  W_RX_I32(npars); W_RX_I32(ndiff); W_RX_I32(sensType);
  W_RX_DBL(sensH);
  W_RX_I32(linB); W_RX_I32(linCmtOral0); W_RX_I32(linCmtNcmt);
  W_RX_DBL(linCmtGillFtol); W_RX_I32(linCmtGillK);
  W_RX_DBL(linCmtGillStep); W_RX_DBL(linCmtGillRtol);
  W_RX_DBL(linCmtShiErr); W_RX_I32(linCmtShiMax);
  W_RX_I32(linCmtHcmt); W_RX_I32(linCmtHmeanI); W_RX_I32(linCmtHmeanO);
  W_RX_DBL(linCmtSuspect); W_RX_I32(linCmtForwardMax);
  W_RX_I32(mixnum); W_RX_I32(input_mixnum);
  W_RX_I32(maxExtra); W_RX_I32(extraPushAbort);
  W_RX_I32(splitBolusN);
  W_RX_BOOL(sample);

#undef W_RX_U32
#undef W_RX_I32
#undef W_RX_I64
#undef W_RX_DBL
#undef W_RX_BOOL

  // ── Section 4: vLines ────────────────────────────────────────────────────
  sWriteVLines(f, &rx->factors,     "factors");
  sWriteVLines(f, &rx->factorNames, "factorNames");

  // ── Section 5: rx_solve auxiliary arrays ─────────────────────────────────
  sWriteIntBlob(f, rx->cov0,       (uint64_t)(rx->nCov0 > 0 ? rx->nCov0 : 0),
                "cov0");
  sWriteIntBlob(f, rx->splitBolus, (uint64_t)(rx->splitBolusN > 0 ? rx->splitBolusN : 0),
                "splitBolus");
  // par_sample: size = npars (from how it's allocated in rxData.cpp)
  {
    uint64_t psn = (rx->par_sample != NULL) ? (uint64_t)rx->npars : 0u;
    sWriteIntBlob(f, rx->par_sample, psn, "par_sample");
  }
  // linCmtScale: linCmtNcmt doubles
  {
    uint64_t lcsn = (rx->linCmtScale != NULL) ? (uint64_t)rx->linCmtNcmt : 0u;
    sWriteDoubleBlob(f, rx->linCmtScale, lcsn, "linCmtScale");
  }

  // ── Section 6: gsolve meaningful subsections ─────────────────────────────
  // state_size: needed to compute n0 layout on restore.
  // At pre-integration time, ind->solve = &gsolve[n0_offset] where n0 = nall*state_size*nsim.
  // We recover state_size as: if gsolve != NULL and nall > 0 and nsim > 0, compute from
  // ind[0]->solve offset. Simpler: store it directly.
  // state_size = (op->neq > 0) ? op->neq : 0  for ODE models; for linCmt-only, 0.
  // Since gLin = gsolve + n0, and gLin is set = gsolve + nall*state_size*nsim,
  // state_size = (int)((_globals.gLin - _globals.gsolve) / ((int64_t)rx->nall * rx->nsim))
  // Use this formula when nall > 0 and nsim > 0, else default to op->neq.
  int32_t state_size_saved = op->neq; // default
  if (_globals.gsolve && _globals.gLin && rx->nall > 0 && rx->nsim > 0) {
    int64_t n0 = _globals.gLin - _globals.gsolve;
    state_size_saved = (int32_t)(n0 / ((int64_t)rx->nall * rx->nsim));
  }
  sWriteI32(f, state_size_saved, "state_size");

  // gLin (linH values): linB * 7 * nsub * nsim doubles
  sWriteDoubleBlob(f, _globals.gLin,
                   (uint64_t)((int64_t)rx->linB * 7 * rx->nsub * rx->nsim),
                   "gLin");

  // gmtime: nMtime * nsub * nsim doubles
  sWriteDoubleBlob(f, _globals.gmtime,
                   (uint64_t)((int64_t)rx->nMtime * rx->nsub * rx->nsim),
                   "gmtime");

  // ginits: n4 = initsC.size() — stored in op->neq when pure ODE; use pointer arithmetic
  // ginits pointer in gsolve: gsolve + n0 + nlin + 3*nsave + n2
  // Easiest: compute size as scaleC offset - ginits
  // But we don't have scaleC.size() directly. Use _globals.gscale - _globals.ginits.
  {
    uint64_t n4 = 0, n6 = 0;
    if (_globals.ginits && _globals.gscale && _globals.gscale > _globals.ginits)
      n4 = (uint64_t)(_globals.gscale - _globals.ginits);
    sWriteDoubleBlob(f, _globals.ginits, n4, "ginits");

    // gscale: n6 = scaleC.size()
    // gscale is followed by gIndSim in the slab; use gIndSim - gscale
    if (_globals.gscale && _globals.gIndSim && _globals.gIndSim > _globals.gscale)
      n6 = (uint64_t)(_globals.gIndSim - _globals.gscale);
    sWriteDoubleBlob(f, _globals.gscale, n6, "gscale");
  }

  // gIndSim: n7 = nIndSim * nsub * nsim doubles
  sWriteDoubleBlob(f, _globals.gIndSim,
                   (uint64_t)((int64_t)rx->nIndSim * rx->nsub * rx->nsim),
                   "gIndSim");

  // ypNA + gatol2 + grtol2 + gssRtol + gssAtol block: 5*neq doubles
  // ypNA is stored at a known offset after gIndSim in gsolve.
  // op->atol2 points to the global (non-thread) portion; ypNA = rx->ypNA.
  sWriteDoubleBlob(f, rx->ypNA, (uint64_t)(op->neq > 0 ? op->neq : 0), "ypNA");
  sWriteDoubleBlob(f, op->atol2,  (uint64_t)(op->neq > 0 ? op->neq : 0), "gatol2");
  sWriteDoubleBlob(f, op->rtol2,  (uint64_t)(op->neq > 0 ? op->neq : 0), "grtol2");
  sWriteDoubleBlob(f, op->ssAtol, (uint64_t)(op->neq > 0 ? op->neq : 0), "gssAtol");
  sWriteDoubleBlob(f, op->ssRtol, (uint64_t)(op->neq > 0 ? op->neq : 0), "gssRtol");

  // ── Section 7: Global event-indexed bulk arrays ───────────────────────────
  uint64_t nall = (uint64_t)rx->nall;
  sWriteU64(f, nall, "nall");
  sWriteU32(f, (uint32_t)op->ncov, "ncov");

  // gall_times slab: 5*nall doubles (all_times, dv, amt, ii, limit)
  sWriteDoubleBlob(f, _globals.gall_times, 5u * nall, "gall_times_slab");

  // gevid slab: allocated as 3*nall + dfN*2 + strLhs.size() ints.
  // We save from gevid start to gcens end (3*nall ints covering evid+gidose+gcens).
  // The remainder (gpar_cov, glhs_str etc.) is reconstructed from R on restore.
  sWriteIntBlob(f, _globals.gevid, 3u * nall, "gevid_slab");

  // gcov: ncov * nall doubles
  sWriteDoubleBlob(f, _globals.gcov, (uint64_t)op->ncov * nall, "gcov");

  // gix: nall * nsim ints (in gon slab, pointed to by _globals.gix)
  sWriteIntBlob(f, _globals.gix, nall * (uint64_t)rx->nsim, "gix");

  // gpars: npars * nsub doubles
  sWriteDoubleBlob(f, _globals.gpars,
                   (uint64_t)rx->npars * rx->nsub, "gpars");

  // ordId: nall ints (subject ordering)
  sWriteIntBlob(f, rx->ordId, nall, "ordId");

  // gParPos: npars ints
  {
    uint64_t gpp_n = (rx->npars > 0 && _globals.gParPos) ? (uint64_t)rx->npars : 0u;
    sWriteIntBlob(f, _globals.gParPos, gpp_n, "gParPos");
  }

  // gParPos2: npars + sigmaN + omegaN ints; covers gsvar and govar.
  // Size = govar end - gParPos2 start. Use gsvar and govar pointers.
  {
    uint64_t gpp2_n = 0;
    if (_globals.gParPos2 && _globals.gsvar && _globals.govar) {
      // gsvar is at gParPos2 + npars; govar is at gsvar + sigmaN.
      // We need govar end = govar + omegaN.
      // Safest: write through the last known pointer govar.
      // omegaN is not directly stored; use rx->neta as approximation.
      // Actually: gParPos2 contains npars + sigmaN + omegaN elements.
      // sigmaN = gsvar - gParPos2 - npars -- wait, gsvar = gParPos2 + npars.
      // govar = gsvar + sigmaN. We don't have sigmaN and omegaN directly.
      // Use end of gpar_cov/gpar_covInterp as boundary? Too fragile.
      // Simpler: save gsvar and govar as separate blobs.
      gpp2_n = 0; // handled below separately
    }
    sWriteIntBlob(f, _globals.gParPos2, gpp2_n, "gParPos2");
  }
  // gsvar and govar: sizes stored in op->nsvar and rx->neta/neps
  {
    // sigmaN is not directly available; govar size = rx->neta.
    // gsvar size = op->nsvar (number of sigma vars).
    uint64_t svar_n = (uint64_t)(op->nsvar > 0 ? op->nsvar : 0);
    uint64_t ovar_n = (uint64_t)(rx->neta > 0 ? rx->neta : 0);
    sWriteIntBlob(f, _globals.gsvar, svar_n, "gsvar");
    sWriteIntBlob(f, _globals.govar, ovar_n, "govar");
  }

  // gall_timesS: 2*(nsim-1)*nall doubles (if nsim > 1)
  {
    uint64_t gasn = (rx->nsim > 1) ? 2u * (uint64_t)(rx->nsim - 1) * nall : 0u;
    sWriteDoubleBlob(f, _globals.gall_timesS, gasn, "gall_timesS");
  }

  // gamtS: (nsim-1)*nall doubles (if nsim > 1)
  {
    uint64_t gamsn = (rx->nsim > 1) ? (uint64_t)(rx->nsim - 1) * nall : 0u;
    sWriteDoubleBlob(f, _globals.gamtS, gamsn, "gamtS");
  }

  // ── Section 8: Per-subject blocks ────────────────────────────────────────
  uint32_t nsub = rx->nsub;
  for (uint32_t si = 0; si < nsub; si++) {
    rx_solving_options_ind *ind = &rx->subjects[si];

    // 8a: scalar POD fields
#define W_IND_I32(field) sWriteI32(f, (int32_t)(ind->field), #field)
#define W_IND_U32(field) sWriteU32(f, (uint32_t)(ind->field), #field)
#define W_IND_DBL(field) sWrite(f, &(ind->field), sizeof(double), #field)
#define W_IND_BOOL(field) { uint8_t _b = (uint8_t)(ind->field); sWrite(f, &_b, 1, #field); }

    W_IND_DBL(bT);
    W_IND_I32(nBadDose);
    W_IND_DBL(HMAX);
    W_IND_DBL(tlast); W_IND_DBL(curDose); W_IND_I32(dosenum);
    W_IND_DBL(tfirst); W_IND_DBL(podo);
    W_IND_I32(n_all_times); W_IND_I32(n_all_times_orig);
    W_IND_I32(nevid2); W_IND_I32(ixds);
    W_IND_I32(ndoses); W_IND_I32(idosen);
    W_IND_I32(id); W_IND_I32(idReal); W_IND_I32(sim);
    W_IND_I32(idx); W_IND_I32(solvedIdx);
    W_IND_DBL(ylow); W_IND_DBL(yhigh);
    W_IND_DBL(logitHi); W_IND_DBL(logitLow);
    W_IND_DBL(lambda); W_IND_DBL(yj);
    W_IND_I32(wh); W_IND_I32(wh100); W_IND_I32(cmt);
    W_IND_I32(whI); W_IND_I32(wh0); W_IND_I32(doSS);
    W_IND_I32(allCovWarn); W_IND_I32(wrongSSDur);
    W_IND_I32(_newind); W_IND_I32(_rxFlag); W_IND_I32(err);
    W_IND_I32(solved); W_IND_I32(linCmt); W_IND_I32(cacheME);
    W_IND_I32(inLhs);
    W_IND_DBL(solveTime); W_IND_DBL(curShift);
    W_IND_I32(isIni); W_IND_I32(_update_par_ptr_in); W_IND_I32(badIni);
    W_IND_I32(linSS); W_IND_I32(linSScmt); W_IND_I32(linSSbolusCmt);
    W_IND_DBL(linSStau); W_IND_DBL(linSSvar); W_IND_DBL(ssTime);
    W_IND_I32(linCmtHparIndex); W_IND_DBL(linCmtH); W_IND_DBL(linCmtHV);
    W_IND_I32(mixest); W_IND_DBL(mixunif);
    W_IND_DBL(tolFactor); W_IND_I32(neqOverride);
    W_IND_I32(indOwnAlloc); W_IND_I32(indOwnAllocN);
    W_IND_I32(solveAllocN); W_IND_I32(idoseOwnAllocN);
    W_IND_I32(mainSorted); W_IND_I32(extraSorted);
    W_IND_I32(idxExtra); W_IND_I32(nPushedExtra);
    W_IND_I32(_atEventTime);
    W_IND_BOOL(lastIsSs2);
    W_IND_I32(idxLow); W_IND_I32(idxHi);
    W_IND_DBL(tprior); W_IND_DBL(tout);

#undef W_IND_I32
#undef W_IND_U32
#undef W_IND_DBL
#undef W_IND_BOOL

    // 8b: array data — always read from wherever ind-> points (own or global slab)
    int nat = ind->n_all_times;
    int nd  = ind->ndoses;
    int neq = op->neq;

    // Event-indexed arrays (nat elements each)
    sWriteDoubleBlob(f, ind->dose,      (uint64_t)(nat > 0 ? nat : 0), "dose");
    sWriteDoubleBlob(f, ind->ii,        (uint64_t)(nat > 0 ? nat : 0), "ii");
    sWriteDoubleBlob(f, ind->all_times, (uint64_t)(nat > 0 ? nat : 0), "all_times");
    sWriteIntBlob(f,    ind->evid,      (uint64_t)(nat > 0 ? nat : 0), "evid");
    sWriteIntBlob(f,    ind->ix,        (uint64_t)(nat > 0 ? nat : 0), "ix");

    // idose
    sWriteIntBlob(f, ind->idose, (uint64_t)(nd > 0 ? nd : 0), "idose");

    // observation arrays (dv, cens, limit) — nat elements each
    sWriteDoubleBlob(f, ind->dv,    (uint64_t)(nat > 0 ? nat : 0), "dv");
    sWriteIntBlob(f,    ind->cens,  (uint64_t)(nat > 0 ? nat : 0), "cens");
    sWriteDoubleBlob(f, ind->limit, (uint64_t)(nat > 0 ? nat : 0), "limit");

    // cov_ptr: ncov * nat doubles
    sWriteDoubleBlob(f, ind->cov_ptr,
                     (uint64_t)(nat > 0 ? (int64_t)op->ncov * nat : 0),
                     "cov_ptr");

    // solve: initial ODE state — neq doubles from ind->solve[0..neq-1]
    sWriteDoubleBlob(f, ind->solve, (uint64_t)(neq > 0 ? neq : 0), "solve_init");

    // par_ptr: npars doubles (the original parameter values for this subject)
    sWriteDoubleBlob(f, ind->par_ptr, (uint64_t)(rx->npars > 0 ? rx->npars : 0),
                     "par_ptr");

    // mtime / mtime0: nMtime doubles each
    {
      uint64_t nm = (uint64_t)(rx->nMtime > 0 ? rx->nMtime : 0);
      sWriteDoubleBlob(f, ind->mtime,  nm, "mtime");
      sWriteDoubleBlob(f, ind->mtime0, nm, "mtime0");
    }

    // linH: 7 doubles per subject (if linB)
    sWriteDoubleBlob(f, ind->linH, (uint64_t)(rx->linB ? 7 : 0), "linH");
  }

  fclose(f);
  return Rf_ScalarLogical(1);
}

// ---------------------------------------------------------------------------
// rxIsSerializeFile: check magic bytes — used for rxSolve dispatch
// ---------------------------------------------------------------------------

//' Check whether a file was written by rxSaveState_
//' @noRd
// [[Rcpp::export]]
bool rxIsSerializeFile_(SEXP pathSexp) {
  const char *path = CHAR(STRING_ELT(pathSexp, 0));
  FILE *f = fopen(path, "rb");
  if (!f) return false;
  char magic[8];
  bool ok = (fread(magic, 1, 8, f) == 8) &&
             (memcmp(magic, rxSerializeMagic, 8) == 0);
  fclose(f);
  return ok;
}
