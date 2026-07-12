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
extern "C" void rxOptionsIniEnsure(int mx, int cores);
extern rx_globals _globals;

static const char rxSerializeMagic[8] = {'R','X','O','D','E','2','S','Z'};
static const uint32_t rxSerializeFormatVer = 2u;

// ---------------------------------------------------------------------------
// Low-level write helpers -- all abort via Rf_error on failure
// ---------------------------------------------------------------------------

static void sWrite(std::vector<uint8_t> *f, const void *buf, size_t n, const char *what) {
  const uint8_t *ptr = (const uint8_t *)buf;
  f->insert(f->end(), ptr, ptr + n);
}

static void sWriteU32(std::vector<uint8_t> *f, uint32_t v, const char *what) {
  sWrite(f, &v, sizeof(v), what);
}

static void sWriteU64(std::vector<uint8_t> *f, uint64_t v, const char *what) {
  sWrite(f, &v, sizeof(v), what);
}

static void sWriteI32(std::vector<uint8_t> *f, int32_t v, const char *what) {
  sWrite(f, &v, sizeof(v), what);
}

// Write a size-prefixed blob: uint64_t count then count elements of width w.
static void sWriteBlob(std::vector<uint8_t> *f, const void *data, uint64_t count, size_t w,
                       const char *what) {
  sWriteU64(f, count, what);
  if (count > 0 && data != NULL)
    sWrite(f, data, count * w, what);
}

static void sWriteDoubleBlob(std::vector<uint8_t> *f, const double *data, uint64_t n,
                             const char *what) {
  sWriteBlob(f, data, n, sizeof(double), what);
}

static void sWriteIntBlob(std::vector<uint8_t> *f, const int *data, uint64_t n,
                          const char *what) {
  sWriteBlob(f, data, n, sizeof(int), what);
}

// ---------------------------------------------------------------------------
// vLines serialization
// ---------------------------------------------------------------------------

static void sWriteVLines(std::vector<uint8_t> *f, const vLines *vl, const char *what) {
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
SEXP rxSaveState_() {
  std::vector<uint8_t> vec;
  std::vector<uint8_t> *f = &vec;

  rx_solve *rx = getRxSolve_();
  rx_solving_options *op = rx->op;

  // -- Section 1: Header ----------------------------------------------------
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

  // -- Section 2: rx_solving_options scalar POD fields ----------------------
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

  // -- Section 3: rx_solve scalar POD fields --------------------------------
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

  // -- Section 4: vLines ----------------------------------------------------
  sWriteVLines(f, &rx->factors,     "factors");
  sWriteVLines(f, &rx->factorNames, "factorNames");

  // -- Section 5: rx_solve auxiliary arrays ---------------------------------
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

  // -- Section 6: gsolve meaningful subsections -----------------------------
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

  // ginits: n4 = initsC.size() -- stored in op->neq when pure ODE; use pointer arithmetic
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

  // -- Section 7: Global event-indexed bulk arrays ---------------------------
  // nall: expanded total events (rx->nall = representative * nsub for homGroups)
  // nall_rep: representative event count = actual allocation size of gall_times/gevid/gcov
  // For non-homGroups solves, nall_rep == nall.
  uint64_t nall = (uint64_t)rx->nall;
  uint64_t nall_rep = (_globals.gall_times_n > 0) ? (uint64_t)_globals.gall_times_n : nall;
  sWriteU64(f, nall, "nall");
  sWriteU64(f, nall_rep, "nall_rep");
  sWriteU32(f, (uint32_t)op->ncov, "ncov");

  // gall_times slab: 5*nall_rep doubles (all_times, dv, amt, ii, limit)
  // Allocated for the representative events only, not the expanded subject count.
  sWriteDoubleBlob(f, _globals.gall_times, 5u * nall_rep, "gall_times_slab");

  // gevid slab: allocated as 3*nall_rep + dfN*2 + strLhs.size() ints.
  uint64_t dfN = (rx->npars <= 0) ? 0 : rx->npars;
  uint64_t gevid_n = 3u * nall_rep + dfN * 2 + (uint64_t)op->nlhs;
  sWriteIntBlob(f, _globals.gevid, gevid_n, "gevid_slab");

  // gcov: ncov * nall_rep doubles (representative events only)
  sWriteDoubleBlob(f, _globals.gcov, (uint64_t)op->ncov * nall_rep, "gcov");

  // gix: nall * nsim ints -- correctly allocated for the full expanded count
  sWriteIntBlob(f, _globals.gix, nall * (uint64_t)rx->nsim, "gix");

  // gpars: npars * nsub doubles
  sWriteDoubleBlob(f, _globals.gpars,
                   (uint64_t)rx->npars * rx->nsub, "gpars");

  // ordId: nsub * nsim ints (subject ordering index, not an event-indexed array)
  sWriteIntBlob(f, rx->ordId, (uint64_t)rx->nsub * rx->nsim, "ordId");

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

  // -- Section 8: Per-subject blocks ----------------------------------------
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

    // 8b: array data -- always read from wherever ind-> points (own or global slab)
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

    // observation arrays (dv, cens, limit) -- nat elements each
    sWriteDoubleBlob(f, ind->dv,    (uint64_t)(nat > 0 ? nat : 0), "dv");
    sWriteIntBlob(f,    ind->cens,  (uint64_t)(nat > 0 ? nat : 0), "cens");
    sWriteDoubleBlob(f, ind->limit, (uint64_t)(nat > 0 ? nat : 0), "limit");

    // cov_ptr: ncov * nat doubles
    sWriteDoubleBlob(f, ind->cov_ptr,
                     (uint64_t)(nat > 0 ? (int64_t)op->ncov * nat : 0),
                     "cov_ptr");

    // solve: initial ODE state -- neq doubles from ind->solve[0..neq-1]
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

  RawVector ret(vec.size());
  std::copy(vec.begin(), vec.end(), ret.begin());
  return ret;
}

// ---------------------------------------------------------------------------
// rxIsSerializeFile: check magic bytes -- used for rxSolve dispatch
// ---------------------------------------------------------------------------

//' Check whether a file was written by rxSaveState_
//' @noRd
// [[Rcpp::export]]
bool rxIsSerializeFile_(SEXP rawSexp) {
  if (TYPEOF(rawSexp) != RAWSXP || Rf_length(rawSexp) < 8) return false;
  uint8_t *data = RAW(rawSexp);
  return memcmp(data, rxSerializeMagic, 8) == 0;
}

// ---------------------------------------------------------------------------
// Low-level read helpers
// ---------------------------------------------------------------------------
struct MemReader {
  const uint8_t *data;
  size_t size;
  size_t pos;
};

static void sRead(MemReader *f, void *buf, size_t n, const char *what) {
  if (f->pos + n > f->size)
    (Rf_error)("rxRestoreState: read error reading %s (EOF)", what);
  memcpy(buf, f->data + f->pos, n);
  f->pos += n;
}

static uint32_t sReadU32(MemReader *f, const char *what) {
  uint32_t v; sRead(f, &v, sizeof(v), what); return v;
}

static uint64_t sReadU64(MemReader *f, const char *what) {
  uint64_t v; sRead(f, &v, sizeof(v), what); return v;
}

static int32_t sReadI32(MemReader *f, const char *what) {
  int32_t v; sRead(f, &v, sizeof(v), what); return v;
}

static double sReadDbl(MemReader *f, const char *what) {
  double v; sRead(f, &v, sizeof(v), what); return v;
}

static uint8_t sReadU8(MemReader *f, const char *what) {
  uint8_t v; sRead(f, &v, sizeof(v), what); return v;
}

// Read a size-prefixed blob: allocates with calloc, returns count via *n_out.
// Caller must free(). Returns NULL when count == 0.
static void *sReadBlob(MemReader *f, uint64_t *n_out, size_t w, const char *what) {
  *n_out = sReadU64(f, what);
  if (*n_out == 0) return NULL;
  void *buf = calloc((size_t)(*n_out), w);
  if (!buf) (Rf_error)("rxRestoreState: out of memory reading %s", what);
  sRead(f, buf, (size_t)(*n_out) * w, what);
  return buf;
}

static double *sReadDoubleBlob(MemReader *f, uint64_t *n_out, const char *what) {
  return (double *)sReadBlob(f, n_out, sizeof(double), what);
}

static int *sReadIntBlob(MemReader *f, uint64_t *n_out, const char *what) {
  return (int *)sReadBlob(f, n_out, sizeof(int), what);
}

// Helper to read dynamically allocated double blob with exact size check
static double *sReadDoubleBlobExpected(MemReader *f, uint64_t expected_n, const char *what) {
  uint64_t n_out;
  double *buf = sReadDoubleBlob(f, &n_out, what);
  if (n_out != expected_n) {
    if (buf) free(buf);
    (Rf_error)("rxRestoreState: %s size mismatch (file: %llu, expected: %llu)", what, (unsigned long long)n_out, (unsigned long long)expected_n);
  }
  return buf;
}

// Helper to read dynamically allocated int blob with exact size check
static int *sReadIntBlobExpected(MemReader *f, uint64_t expected_n, const char *what) {
  uint64_t n_out;
  int *buf = sReadIntBlob(f, &n_out, what);
  if (n_out != expected_n) {
    if (buf) free(buf);
    (Rf_error)("rxRestoreState: %s size mismatch (file: %llu, expected: %llu)", what, (unsigned long long)n_out, (unsigned long long)expected_n);
  }
  return buf;
}

// Helper to read directly into a pre-allocated double buffer
static void sReadDoubleDirect(MemReader *f, double *dest, uint64_t expected_n, const char *what) {
  uint64_t n = sReadU64(f, what);
  if (n > expected_n) {
    (Rf_error)("rxRestoreState: buffer overflow protection, %s size mismatch (file: %llu, expected: %llu)", what, (unsigned long long)n, (unsigned long long)expected_n);
  }
  if (n > 0 && dest != NULL) {
    sRead(f, dest, (size_t)n * sizeof(double), what);
  }
}

// Helper to read directly into a pre-allocated int buffer
static void sReadIntDirect(MemReader *f, int *dest, uint64_t expected_n, const char *what) {
  uint64_t n = sReadU64(f, what);
  if (n > expected_n) {
    (Rf_error)("rxRestoreState: buffer overflow protection, %s size mismatch (file: %llu, expected: %llu)", what, (unsigned long long)n, (unsigned long long)expected_n);
  }
  if (n > 0 && dest != NULL) {
    sRead(f, dest, (size_t)n * sizeof(int), what);
  }
}

// ---------------------------------------------------------------------------
// vLines restore
// ---------------------------------------------------------------------------

static void sReadVLines(MemReader *f, vLines *vl, const char *what) {
  lineNull(vl);
  uint32_t n = sReadU32(f, what);
  for (uint32_t i = 0; i < n; i++) {
    uint32_t len = sReadU32(f, what);
    if (len == 0) {
      addLine(vl, "%s", "");
      continue;
    }
    char *buf = (char *)R_alloc(len, 1);
    sRead(f, buf, len, what);
    buf[len - 1] = '\0';
    addLine(vl, "%s", buf);
  }
  if (n > 0) {
    // restore lProp and lType
    for (uint32_t i = 0; i < n; i++)
      vl->lProp[i] = sReadI32(f, what);
    for (uint32_t i = 0; i < n; i++)
      vl->lType[i] = sReadI32(f, what);
  }
}

// ---------------------------------------------------------------------------
// rxRestoreState_: read binary and reconstruct live rx_solve state
// ---------------------------------------------------------------------------

extern "C" void rxSolveFreeC(void);

//' Restore a pre-integration rxode2 solver state from binary file
//'
//' @param path File path written by rxSaveState_().
//' @return Invisibly TRUE.
//' @noRd
// [[Rcpp::export]]
SEXP rxRestoreState_(SEXP rawSexp) {
  if (TYPEOF(rawSexp) != RAWSXP) {
    (Rf_error)("rxRestoreState_: expected a raw vector");
  }
  MemReader _reader;
  _reader.data = RAW(rawSexp);
  _reader.size = Rf_length(rawSexp);
  _reader.pos = 0;
  MemReader *f = &_reader;

  // -- Section 1: Header validation -----------------------------------------
  char magic[8];
  sRead(f, magic, 8, "magic");
  if (memcmp(magic, rxSerializeMagic, 8) != 0) {
    
    (Rf_error)("rxRestoreState: object is not a valid rxode2 state file");
  }
  uint32_t fmt = sReadU32(f, "format_ver");
  if (fmt > rxSerializeFormatVer) {
    
    (Rf_error)("rxRestoreState: file format version %u > supported %u",
               fmt, rxSerializeFormatVer);
  }

  uint32_t vlen = sReadU32(f, "pkg_ver_len");
  std::string filePkgVer(vlen, '\0');
  sRead(f, &filePkgVer[0], vlen, "pkg_ver");
  filePkgVer.resize(strlen(filePkgVer.c_str()));  // trim null terminator

  std::string curPkgVer;
  try {
    Function fmtFn("format");
    Function pkgVerFn("packageVersion");
    curPkgVer = as<std::string>(
      as<CharacterVector>(fmtFn(pkgVerFn("rxode2")))[0]);
  } catch (...) { curPkgVer = "unknown"; }
  if (filePkgVer != curPkgVer) {
    Rf_warningcall(R_NilValue,
      "rxRestoreState: file was written by rxode2 %s, current is %s",
      filePkgVer.c_str(), curPkgVer.c_str());
  }

  uint32_t sz_opts  = sReadU32(f, "sizeof_opts");
  uint32_t sz_ind   = sReadU32(f, "sizeof_ind");
  uint32_t sz_solve = sReadU32(f, "sizeof_solve");
  if (sz_opts  != (uint32_t)sizeof(rx_solving_options) ||
      sz_ind   != (uint32_t)sizeof(rx_solving_options_ind) ||
      sz_solve != (uint32_t)sizeof(rx_solve)) {
    
    (Rf_error)("rxRestoreState: struct size mismatch -- file was built against a "
               "different rxode2 ABI (opts %u vs %u, ind %u vs %u, solve %u vs %u)",
               sz_opts,  (uint32_t)sizeof(rx_solving_options),
               sz_ind,   (uint32_t)sizeof(rx_solving_options_ind),
               sz_solve, (uint32_t)sizeof(rx_solve));
  }

  // Start fresh
  rxSolveFreeC();

  rx_solve *rx = getRxSolve_();
  rx_solving_options *op = rx->op;

  // -- Section 2: rx_solving_options scalar fields ---------------------------
#define R_I32(field) op->field = (decltype(op->field))sReadI32(f, #field)
#define R_DBL(field) op->field = sReadDbl(f, #field)
#define R_BOOL(field) op->field = (decltype(op->field))sReadU8(f, #field)

  R_I32(badSolve); R_I32(naTime); R_I32(naTimeInput); R_I32(naTimeInputWarn);
  R_DBL(ATOL); R_DBL(RTOL); R_DBL(H0); R_DBL(HMIN);
  R_I32(mxstep); R_I32(MXORDN); R_I32(MXORDS);
  R_I32(nlhs); R_I32(neq); R_I32(stiff); R_I32(ncov);
  sRead(f, op->modNamePtr, sizeof(op->modNamePtr), "modNamePtr");
  R_BOOL(do_par_cov);
  R_I32(is_locf); R_I32(instant_backward); R_I32(keep_interp);
  R_I32(cores); R_I32(doesRandom); R_I32(extraCmt);
  R_DBL(hmax2);
  R_I32(indLinN); R_DBL(indLinPhiTol); R_I32(indLinPhiM);
  R_I32(indLinMatExpType); R_I32(indLinMatExpOrder);
  R_I32(nDisplayProgress); R_I32(ncoresRV); R_I32(isChol);
  R_I32(nsvar); R_I32(abort); R_I32(minSS); R_I32(maxSS);
  R_I32(doIndLin); R_I32(strictSS);
  R_DBL(infSSstep); R_I32(mxhnil); R_DBL(hmxi);
  R_I32(nLlik); R_I32(numLinSens); R_I32(numLin);
  R_I32(depotLin); R_I32(linOffset); R_I32(ssSolved);
  R_I32(indOwnAlloc);

#undef R_I32
#undef R_DBL
#undef R_BOOL

  // -- Section 3: rx_solve scalar fields -------------------------------------
#define R_RX_U32(field) rx->field = (decltype(rx->field))sReadU32(f, #field)
#define R_RX_I32(field) rx->field = (decltype(rx->field))sReadI32(f, #field)
#define R_RX_I64(field) sRead(f, &(rx->field), sizeof(int64_t), #field)
#define R_RX_DBL(field) rx->field = sReadDbl(f, #field)
#define R_RX_BOOL(field) rx->field = (decltype(rx->field))sReadU8(f, #field)

  R_RX_U32(nsub); R_RX_U32(nsim);
  R_RX_I32(neta); R_RX_I32(neps); R_RX_I32(nIndSim); R_RX_I32(simflg);
  R_RX_U32(nall); R_RX_I32(nevid9); R_RX_I32(nobs); R_RX_I32(nobs2);
  R_RX_I64(nr);
  R_RX_I32(add_cov); R_RX_I32(matrix); R_RX_I32(needSort);
  R_RX_I32(nMtime);
  R_RX_DBL(stateTrimU); R_RX_DBL(stateTrimL);
  R_RX_I32(nCov0); R_RX_I32(nKeepF); R_RX_I32(istateReset);
  R_RX_I32(cens); R_RX_I32(limit);
  R_RX_I32(safeZero); R_RX_I32(safeLog); R_RX_I32(safePow);
  R_RX_I32(sumType); R_RX_I32(prodType);
  sRead(f, rx->factorNs, sizeof(rx->factorNs), "factorNs");
  R_RX_I32(hasFactors); R_RX_I32(maxAllTimes);
  R_RX_I32(hasEvid2); R_RX_I32(useStdPow);
  R_RX_BOOL(ss2cancelAllPending);
  R_RX_I32(npars); R_RX_I32(ndiff); R_RX_I32(sensType);
  R_RX_DBL(sensH);
  R_RX_I32(linB); R_RX_I32(linCmtOral0); R_RX_I32(linCmtNcmt);
  R_RX_DBL(linCmtGillFtol); R_RX_I32(linCmtGillK);
  R_RX_DBL(linCmtGillStep); R_RX_DBL(linCmtGillRtol);
  R_RX_DBL(linCmtShiErr); R_RX_I32(linCmtShiMax);
  R_RX_I32(linCmtHcmt); R_RX_I32(linCmtHmeanI); R_RX_I32(linCmtHmeanO);
  R_RX_DBL(linCmtSuspect); R_RX_I32(linCmtForwardMax);
  R_RX_I32(mixnum); R_RX_I32(input_mixnum);
  R_RX_I32(maxExtra); R_RX_I32(extraPushAbort);
  R_RX_I32(splitBolusN);
  R_RX_BOOL(sample);

#undef R_RX_U32
#undef R_RX_I32
#undef R_RX_I64
#undef R_RX_DBL
#undef R_RX_BOOL

  // -- Section 4: vLines -----------------------------------------------------
  sReadVLines(f, &rx->factors,     "factors");
  sReadVLines(f, &rx->factorNames, "factorNames");

  // -- Section 5: rx_solve auxiliary arrays ----------------------------------
  {
    uint64_t n;
    int *tmp;

    tmp = sReadIntBlob(f, &n, "cov0");
    rx->cov0 = tmp;   // may be NULL

    tmp = sReadIntBlob(f, &n, "splitBolus");
    if (rx->splitBolus) free(rx->splitBolus);
    rx->splitBolus  = tmp;
    rx->splitBolusN = (int)n;

    tmp = sReadIntBlob(f, &n, "par_sample");
    if (rx->par_sample) free(rx->par_sample);
    rx->par_sample = tmp;

    double *dtmp = sReadDoubleBlob(f, &n, "linCmtScale");
    if (rx->linCmtScale) free(rx->linCmtScale);
    rx->linCmtScale = dtmp;
  }

  // -- Section 6: gsolve meaningful subsections ------------------------------
  int32_t state_size_saved = sReadI32(f, "state_size");

  // Allocate gsolve using rxFillMemLayout with saved dimensions
  rx_mem_layout _mem;
  rxFillMemLayout(
    op->neq, state_size_saved, op->nlhs,
    (int)rx->nsim, op->cores, rx->nMtime, op->extraCmt,
    rx->linB, op->nLlik, rx->nIndSim, (int)rx->nsub,
    (int64_t)rx->nall, rx->maxAllTimes,
    op->numLinSens, op->numLin,
    (int64_t)op->neq,  // n4_actual: use neq as proxy (actual restored below)
    (int64_t)op->neq,  // n6_actual: use neq as proxy
    &_mem);

  if (_globals.gsolve != NULL) free(_globals.gsolve);
  _globals.gsolve = (double *)calloc(_mem.gsolve_total, sizeof(double));
  if (!_globals.gsolve) {
    
    (Rf_error)("rxRestoreState: out of memory for gsolve");
  }

  // Re-establish gsolve pointer layout (mirrors rxData.cpp exactly)
  _globals.gLin        = _globals.gsolve     + _mem.n0;
  _globals.gLlikSave   = _globals.gLin        + _mem.nlin;
  _globals.gSolveSave  = _globals.gLlikSave   + _mem.nllik_c;
  _globals.gSolveLast  = _globals.gSolveSave  + _mem.nsave;
  _globals.gSolveLast2 = _globals.gSolveLast  + _mem.nsave;
  _globals.gEsPendingJump = _globals.gSolveLast2 + _mem.nsave;
  _globals.gmtime      = _globals.gEsPendingJump + _mem.nsave;
  _globals.ginits      = _globals.gmtime      + _mem.n2;
  op->inits            = &_globals.ginits[0];
  _globals.glhs        = _globals.ginits      + _mem.n4;
  _globals.gscale      = _globals.glhs        + _mem.n5_c;
  op->scale            = &_globals.gscale[0];
  _globals.gatol2      = _globals.gscale      + _mem.n6;
  _globals.grtol2      = _globals.gatol2      + op->neq;
  _globals.gssRtol     = _globals.grtol2      + op->neq;
  _globals.gssAtol     = _globals.gssRtol     + op->neq;
  rx->ypNA             = _globals.gssAtol     + op->neq;
  _globals.gTlastS     = rx->ypNA             + op->neq;
  _globals.gTfirstS    = _globals.gTlastS     + _mem.n3a_c;
  _globals.gCurDoseS   = _globals.gTfirstS    + _mem.n3a_c;
  _globals.gIndSim     = _globals.gCurDoseS   + _mem.n3a_c;
  _globals.gLinSave    = _globals.gIndSim     + _mem.n7;
  _globals.gLinDummy   = _globals.gLinSave    + _mem.n9;
  _globals.timeThread  = _globals.gLinDummy   + _mem.n10;
  _globals.gatol2Thread  = _globals.timeThread  + _mem.n8;
  _globals.grtol2Thread  = _globals.gatol2Thread  + op->neq * op->cores;
  _globals.gssAtolThread = _globals.grtol2Thread  + op->neq * op->cores;
  _globals.gssRtolThread = _globals.gssAtolThread + op->neq * op->cores;
  _globals.gmtime0       = _globals.gssRtolThread + op->neq * op->cores;

  op->atol2 = &_globals.gatol2[0];
  op->rtol2 = &_globals.grtol2[0];
  op->ssAtol = _globals.gssAtol;
  op->ssRtol = _globals.gssRtol;

  // gLin (linH): read and copy into position
  {
    uint64_t expected = (uint64_t)((int64_t)rx->linB * 7 * rx->nsub * rx->nsim);
    sReadDoubleDirect(f, _globals.gLin, expected, "gLin");
  }

  // gmtime
  {
    uint64_t expected = (uint64_t)((int64_t)rx->nMtime * rx->nsub * rx->nsim);
    sReadDoubleDirect(f, _globals.gmtime, expected, "gmtime");
  }

  // ginits (n4)
  {
    uint64_t expected = (uint64_t)_mem.n4;
    sReadDoubleDirect(f, _globals.ginits, expected, "ginits");
  }

  // gscale (n6)
  {
    uint64_t expected = (uint64_t)_mem.n6;
    sReadDoubleDirect(f, _globals.gscale, expected, "gscale");
  }

  // gIndSim
  {
    uint64_t expected = (uint64_t)((int64_t)rx->nIndSim * rx->nsub * rx->nsim);
    sReadDoubleDirect(f, _globals.gIndSim, expected, "gIndSim");
  }

  // ypNA + tolerance arrays (5 * neq doubles)
  {
    uint64_t expected = (uint64_t)(op->neq > 0 ? op->neq : 0);
    sReadDoubleDirect(f, rx->ypNA, expected, "ypNA");
    sReadDoubleDirect(f, _globals.gatol2, expected, "gatol2");
    sReadDoubleDirect(f, _globals.grtol2, expected, "grtol2");
    sReadDoubleDirect(f, _globals.gssAtol, expected, "gssAtol");
    sReadDoubleDirect(f, _globals.gssRtol, expected, "gssRtol");
  }

  // Propagate global tolerances to per-thread slices
  for (int _c = 0; _c < op->cores; _c++) {
    if (op->neq > 0) {
      memcpy(_globals.gatol2Thread  + _c * op->neq, _globals.gatol2,  op->neq * sizeof(double));
      memcpy(_globals.grtol2Thread  + _c * op->neq, _globals.grtol2,  op->neq * sizeof(double));
      memcpy(_globals.gssAtolThread + _c * op->neq, _globals.gssAtol, op->neq * sizeof(double));
      memcpy(_globals.gssRtolThread + _c * op->neq, _globals.gssRtol, op->neq * sizeof(double));
    }
  }

  // -- Section 7: gon slab (zero-initialized) + gix data --------------------
  if (_globals.gon != NULL) free(_globals.gon);
  _globals.gon = (int *)calloc(_mem.gon_total, sizeof(int));
  if (!_globals.gon) {
    
    (Rf_error)("rxRestoreState: out of memory for gon");
  }
  int64_t nSize = (int64_t)rx->nsim * rx->nsub;
  _globals.gBadDose     = _globals.gon          + _mem.n3a_c;
  _globals.grc          = _globals.gBadDose     + _mem.n3;
  _globals.slvr_counter = _globals.grc          + nSize;
  _globals.dadt_counter = _globals.slvr_counter + nSize;
  _globals.jac_counter  = _globals.dadt_counter + nSize;
  _globals.gix          = _globals.jac_counter  + nSize;

  // InfusionRate: allocate per-thread buffers (zero = no active infusions)
  {
    if (_globals.gInfusionRate != NULL) {
      for (int _t = 0; _t < _globals.nInfusionRateThreads; _t++)
        if (_globals.gInfusionRate[_t] != NULL) free(_globals.gInfusionRate[_t]);
      free(_globals.gInfusionRate);
      free(_globals.gInfusionRateN);
    }
    int _ncmt = op->neq + op->extraCmt;
    _globals.gInfusionRate  = (double **)calloc(op->cores, sizeof(double *));
    _globals.gInfusionRateN = (int *)calloc(op->cores, sizeof(int));
    _globals.nInfusionRateThreads = op->cores;
    for (int _t = 0; _t < op->cores; _t++) {
      if (_ncmt > 0) {
        _globals.gInfusionRate[_t] = (double *)calloc(_ncmt, sizeof(double));
        if (!_globals.gInfusionRate[_t]) {
          
          (Rf_error)("rxRestoreState: out of memory for InfusionRate thread %d", _t);
        }
      }
      _globals.gInfusionRateN[_t] = _ncmt;
    }
  }

  // -- Section 8: Global event-indexed bulk arrays ---------------------------
  // nall_saved: expanded total (rx->nall, already restored in Section 3)
  // nall_rep: representative event count = actual allocation of gall_times/gevid/gcov
  uint64_t nall_saved = sReadU64(f, "nall");
  uint64_t nall_rep   = sReadU64(f, "nall_rep");
  _globals.gall_times_n = (int64_t)nall_rep;
  uint32_t ncov_saved = sReadU32(f, "ncov");

  // gall_times slab (5*nall_rep doubles -- representative events only)
  {
    uint64_t expected = 5u * nall_rep;
    double *buf = sReadDoubleBlobExpected(f, expected, "gall_times_slab");
    if (_globals.gall_times != NULL) free(_globals.gall_times);
    _globals.gall_times = buf;
    _globals.gdv    = _globals.gall_times + nall_rep;
    _globals.gamt   = _globals.gdv        + nall_rep;
    _globals.gii    = _globals.gamt       + nall_rep;
    _globals.glimit = _globals.gii        + nall_rep;
  }

  // gevid slab (3*nall_rep + dfN*2 + nlhs ints)
  {
    uint64_t dfN = (rx->npars <= 0) ? 0 : rx->npars;
    uint64_t expected = 3u * nall_rep + dfN * 2 + (uint64_t)op->nlhs;
    int *buf = sReadIntBlobExpected(f, expected, "gevid_slab");
    if (_globals.gevid != NULL) free(_globals.gevid);
    _globals.gevid  = buf;
    _globals.gidose = _globals.gevid + nall_rep;
    _globals.gcens  = _globals.gidose + nall_rep;
    _globals.gpar_cov = _globals.gcens;
    _globals.gpar_covInterp = _globals.gpar_cov + dfN;
    _globals.glhs_str = _globals.gpar_covInterp + dfN;
  }

  // gcov (ncov * nall_rep doubles)
  {
    uint64_t expected = (uint64_t)op->ncov * nall_rep;
    double *buf = sReadDoubleBlobExpected(f, expected, "gcov");
    if (_globals.gcov != NULL) free(_globals.gcov);
    _globals.gcov = buf;
  }

  // gix: nall_saved * nsim ints (correctly allocated for the full expanded count)
  {
    uint64_t expected = nall_saved * (uint64_t)rx->nsim;
    sReadIntDirect(f, _globals.gix, expected, "gix");
  }

  // gpars
  {
    uint64_t expected = (uint64_t)rx->npars * rx->nsub;
    double *buf = sReadDoubleBlobExpected(f, expected, "gpars");
    if (_globals.gpars != NULL) free(_globals.gpars);
    _globals.gpars = buf;
  }

  // ordId: nsub * nsim ints (subject ordering, not event count)
  {
    uint64_t expected = (uint64_t)rx->nsub * rx->nsim;
    int *buf = sReadIntBlobExpected(f, expected, "ordId");
    if (rx->ordId != NULL) free(rx->ordId);
    rx->ordId = _globals.ordId = buf;
  }

  // gParPos, gParPos2 (gsvar, govar covered by gParPos2)
  {
    uint64_t expected_gpp = (rx->npars > 0 && _globals.gParPos) ? (uint64_t)rx->npars : 0u;
    int *buf;

    buf = sReadIntBlobExpected(f, expected_gpp, "gParPos");
    if (_globals.gParPos != NULL) free(_globals.gParPos);
    _globals.gParPos = buf;
    
    uint64_t n_gpp2;
    buf = sReadIntBlob(f, &n_gpp2, "gParPos2"); // Could be 0
    if (_globals.gParPos2 != NULL) free(_globals.gParPos2);
    _globals.gParPos2 = buf;

    // gsvar and govar separately
    uint64_t expected_svar = (uint64_t)(op->nsvar > 0 ? op->nsvar : 0);
    uint64_t expected_ovar = (uint64_t)(rx->neta > 0 ? rx->neta : 0);
    int *svar_buf = sReadIntBlobExpected(f, expected_svar, "gsvar");
    int *ovar_buf = sReadIntBlobExpected(f, expected_ovar, "govar");
    _globals.gsvar = svar_buf;
    _globals.govar = ovar_buf;
  }

  // gall_timesS and gamtS
  {
    uint64_t expected_ts = (rx->nsim > 1) ? 2u * (uint64_t)(rx->nsim - 1) * nall_saved : 0u;
    double *buf = sReadDoubleBlobExpected(f, expected_ts, "gall_timesS");
    if (_globals.gall_timesS != NULL) free(_globals.gall_timesS);
    _globals.gall_timesS = buf;

    uint64_t expected_amts = (rx->nsim > 1) ? (uint64_t)(rx->nsim - 1) * nall_saved : 0u;
    buf = sReadDoubleBlobExpected(f, expected_amts, "gamtS");
    if (_globals.gamtS != NULL) free(_globals.gamtS);
    _globals.gamtS = buf;
  }

  // -- Section 9: Per-subject blocks -----------------------------------------
  uint32_t nsub = rx->nsub;
  // rxOptionsIniEnsure resets rx->ordId = NULL; save the restored pointer first.
  int *savedOrdId = rx->ordId;
  // Allocate via rxOptionsIniEnsure so inds_global is properly set up.
  // All solve functions in par_solve.cpp use inds_global directly.
  rxOptionsIniEnsure((int)(nsub * rx->nsim), op->cores);
  // Restore ordId that rxOptionsIniEnsure nulled out.
  rx->ordId = _globals.ordId = savedOrdId;
  rx_solving_options_ind *inds = rx->subjects; // == inds_global

  for (uint32_t si = 0; si < nsub; si++) {
    rx_solving_options_ind *ind = &inds[si];
    memset(ind, 0, sizeof(*ind));
    ind->op = op;
    ind->rx = rx;
    ind->fns = &rx->fns;

    // 9a: scalar POD fields
#define R_IND_I32(field) ind->field = (decltype(ind->field))sReadI32(f, #field)
#define R_IND_DBL(field) ind->field = sReadDbl(f, #field)
#define R_IND_BOOL(field) ind->field = (decltype(ind->field))sReadU8(f, #field)

    R_IND_DBL(bT);
    R_IND_I32(nBadDose);
    R_IND_DBL(HMAX);
    R_IND_DBL(tlast); R_IND_DBL(curDose); R_IND_I32(dosenum);
    R_IND_DBL(tfirst); R_IND_DBL(podo);
    R_IND_I32(n_all_times); R_IND_I32(n_all_times_orig);
    R_IND_I32(nevid2); R_IND_I32(ixds);
    R_IND_I32(ndoses); R_IND_I32(idosen);
    R_IND_I32(id); R_IND_I32(idReal); R_IND_I32(sim);
    R_IND_I32(idx); R_IND_I32(solvedIdx);
    R_IND_DBL(ylow); R_IND_DBL(yhigh);
    R_IND_DBL(logitHi); R_IND_DBL(logitLow);
    R_IND_DBL(lambda); R_IND_DBL(yj);
    R_IND_I32(wh); R_IND_I32(wh100); R_IND_I32(cmt);
    R_IND_I32(whI); R_IND_I32(wh0); R_IND_I32(doSS);
    R_IND_I32(allCovWarn); R_IND_I32(wrongSSDur);
    R_IND_I32(_newind); R_IND_I32(_rxFlag); R_IND_I32(err);
    R_IND_I32(solved); R_IND_I32(linCmt); R_IND_I32(cacheME);
    R_IND_I32(inLhs);
    R_IND_DBL(solveTime); R_IND_DBL(curShift);
    R_IND_I32(isIni); R_IND_I32(_update_par_ptr_in); R_IND_I32(badIni);
    R_IND_I32(linSS); R_IND_I32(linSScmt); R_IND_I32(linSSbolusCmt);
    R_IND_DBL(linSStau); R_IND_DBL(linSSvar); R_IND_DBL(ssTime);
    R_IND_I32(linCmtHparIndex); R_IND_DBL(linCmtH); R_IND_DBL(linCmtHV);
    R_IND_I32(mixest); R_IND_DBL(mixunif);
    R_IND_DBL(tolFactor); R_IND_I32(neqOverride);
    R_IND_I32(indOwnAlloc); R_IND_I32(indOwnAllocN);
    R_IND_I32(solveAllocN); R_IND_I32(idoseOwnAllocN);
    R_IND_I32(mainSorted); R_IND_I32(extraSorted);
    R_IND_I32(idxExtra); R_IND_I32(nPushedExtra);
    R_IND_I32(_atEventTime);
    R_IND_BOOL(lastIsSs2);
    R_IND_I32(idxLow); R_IND_I32(idxHi);
    R_IND_DBL(tprior); R_IND_DBL(tout);

#undef R_IND_I32
#undef R_IND_DBL
#undef R_IND_BOOL

    int nat = ind->n_all_times;
    int nd  = ind->ndoses;
    int neq = op->neq;

    // 9b: per-subject arrays -- always restore as indOwnAlloc
    {
      uint64_t exp_nat = (uint64_t)(nat > 0 ? nat : 0);
      uint64_t exp_nd  = (uint64_t)(nd > 0 ? nd : 0);
      ind->dose      = sReadDoubleBlobExpected(f, exp_nat, "dose");
      ind->ii        = sReadDoubleBlobExpected(f, exp_nat, "ii");
      ind->all_times = sReadDoubleBlobExpected(f, exp_nat, "all_times");
      ind->evid      = sReadIntBlobExpected(f, exp_nat, "evid");
      ind->ix        = sReadIntBlobExpected(f, exp_nat, "ix");
      ind->idose     = sReadIntBlobExpected(f, exp_nd, "idose");
      ind->dv        = sReadDoubleBlobExpected(f, exp_nat, "dv");
      ind->cens      = sReadIntBlobExpected(f, exp_nat, "cens");
      ind->limit     = sReadDoubleBlobExpected(f, exp_nat, "limit");
      
      uint64_t exp_cov = (uint64_t)(nat > 0 ? (int64_t)op->ncov * nat : 0);
      ind->cov_ptr   = sReadDoubleBlobExpected(f, exp_cov, "cov_ptr");

      // solve: restore initial conditions into a fresh buffer
      int solveN = nat + EVID_EXTRA_SIZE;
      ind->solve = (double *)calloc((int64_t)neq * solveN, sizeof(double));
      if (!ind->solve) {
        
        (Rf_error)("rxRestoreState: out of memory for ind->solve subject %u", si);
      }
      uint64_t exp_neq = (uint64_t)(neq > 0 ? neq : 0);
      sReadDoubleDirect(f, ind->solve, exp_neq, "solve_init");

      uint64_t exp_npars = (uint64_t)(rx->npars > 0 ? rx->npars : 0);
      ind->par_ptr = sReadDoubleBlobExpected(f, exp_npars, "par_ptr");

      // mtime / mtime0
      uint64_t exp_mtime = (uint64_t)(rx->nMtime > 0 ? rx->nMtime : 0);
      ind->mtime  = sReadDoubleBlobExpected(f, exp_mtime, "mtime");
      ind->mtime0 = sReadDoubleBlobExpected(f, exp_mtime, "mtime0");

      // linH: 7 doubles per linB subject
      uint64_t exp_linH = (uint64_t)(rx->linB ? 7 : 0);
      if (rx->linB) {
        ind->linH = _globals.gLin + si * 7;
        sReadDoubleDirect(f, ind->linH, exp_linH, "linH");
      } else {
        uint64_t n;
        double *linH_buf = sReadDoubleBlob(f, &n, "linH");
        free(linH_buf);
        ind->linH = NULL;
      }

      // Mark as own-alloc so rxFreeInd frees the arrays we just allocated
      ind->indOwnAlloc    = 1;
      ind->indOwnAllocN   = nat;
      ind->solveAllocN    = solveN;
      ind->idoseOwnAllocN = nd;
    }

    // timeThread: not restored (recomputed from ix by solver)
    // Allocate a private timeThread buffer for this subject
    if (nat > 0) {
      ind->timeThread = (double *)calloc(nat + 1, sizeof(double));
    }

    // Allocate per-thread scratch slots that setupRxInd would normally set.
    // These point to global per-thread slabs; use thread 0 as placeholder --
    // setupRxInd() in the solve loop will re-point them before use.
    ind->on           = _globals.gon;    // thread 0 slice placeholder
    ind->InfusionRate = (_globals.gInfusionRate && _globals.gInfusionRate[0]) ? _globals.gInfusionRate[0] : NULL;
    ind->solveSave = _globals.gSolveSave;
    ind->solveLast = _globals.gSolveLast;
    ind->solveLast2= _globals.gSolveLast2;
    ind->esPendingJump = _globals.gEsPendingJump;
    ind->lhs       = _globals.glhs;
    ind->llikSave  = _globals.gLlikSave;
    ind->linCmtSave= _globals.gLinSave;
    ind->linCmtDummy=_globals.gLinDummy;

    ind->slvr_counter = _globals.slvr_counter + si;
    ind->dadt_counter = _globals.dadt_counter + si;
    ind->jac_counter  = _globals.jac_counter  + si;
    ind->BadDose      = _globals.gBadDose + (int64_t)neq * si;
    ind->rc           = _globals.grc + si;
  }

  _globals.alloc = true;
  return R_NilValue;
}
