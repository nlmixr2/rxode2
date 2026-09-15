// rxode2's side of the tables shared with rxode2lincmt.
//
// C++ (not C) on purpose: rx_get_thread() is only real under _OPENMP, and
// only the C++ sources are compiled with the OpenMP flags.
//
// Nothing here validates anything.  The _p_* pointers start at harmless
// stubs, so a solve before `.linkAll()` gets NA instead of a crash, and the
// reader only overwrites the slots the list provides.
#define STRICT_R_HEADERS
#define USE_FC_LEN_T
#define R_NO_REMAP
#include <cstddef>
#include <cstring>
#include <type_traits>
#include "rxomp.h"
#include "../inst/include/rxode2.h"
#include "rxProtect.h"
#include "rxode2lincmtLink.h"
#include <rxode2lincmtHost.h>

extern "C" double getTime(int idx, rx_solving_options_ind *ind);

// Every field rxode2lincmt reads through the offset table must still exist
// with the type it was built for; a mismatch fails this compile.
RXLC_HOST_STATIC_CHECKS

extern "C" {

static double rxLcStubA(rx_solve *, int, double, int, int, int, int, int,
                        double, double, double, double, double, double,
                        double) {
  return NA_REAL;
}

static double rxLcStubB(rx_solve *, int, double, int, int, int, int, int,
                        int, double, double, double, double, double, double,
                        double) {
  return NA_REAL;
}

static void rxLcStubEnsureA(int) {}
static void rxLcStubEnsureB(int) {}
static void rxLcStubBindFree(rx_solving_options_ind *) {}
static double rxLcStubScaleInitPar(int) { return NA_REAL; }
static double rxLcStubScaleInitN(void) { return 0.0; }
static int rxLcStubZeroJac(int) { return 1; }
static void rxLcStubFreeInd(rx_solving_options_ind *) {}

rxLcLinCmtA_t _p_linCmtA = &rxLcStubA;
rxLcLinCmtB_t _p_linCmtB = &rxLcStubB;
rxLcEnsure_t _p_ensureLinCmtA = &rxLcStubEnsureA;
rxLcEnsure_t _p_ensureLinCmtB = &rxLcStubEnsureB;
rxLcIndFree_t _p_linCmtBindFree = &rxLcStubBindFree;
rxLcScaleInitPar_t _p_linCmtScaleInitPar = &rxLcStubScaleInitPar;
rxLcScaleInitN_t _p_linCmtScaleInitN = &rxLcStubScaleInitN;
rxLcZeroJac_t _p_linCmtZeroJac = &rxLcStubZeroJac;
rxLcIndFree_t _p_linCmtFreeInd = &rxLcStubFreeInd;

SEXP _rxode2_iniRxode2lincmtPtrs(SEXP p) {
  iniRxode2lincmtPtrs0(p);
  return R_NilValue;
}

// rxode2's thread slot (thread_local override + this DLL's OpenMP numbering),
// handed to rxode2lincmt so both packages index the same per-thread slot
int rxode2LinCmtThread(int mx) {
  return rx_get_thread(mx);
}

// Compiled models bind linCmtA/linCmtB by name through R_GetCCallable
double rxode2LinCmtAFwd(rx_solve *rx, int id, double _t, int linCmt, int ncmt,
                        int oral0, int which, int trans, double p1, double v1,
                        double p2, double p3, double p4, double p5, double ka) {
  return _p_linCmtA(rx, id, _t, linCmt, ncmt, oral0, which, trans,
                    p1, v1, p2, p3, p4, p5, ka);
}

double rxode2LinCmtBFwd(rx_solve *rx, int id, double _t, int linCmt, int ncmt,
                        int oral0, int which1, int which2, int trans, double p1,
                        double v1, double p2, double p3, double p4, double p5,
                        double ka) {
  return _p_linCmtB(rx, id, _t, linCmt, ncmt, oral0, which1, which2, trans,
                    p1, v1, p2, p3, p4, p5, ka);
}

// struct size, table constants, then offsetof() of every RXLC_HOST_FIELDS row,
// all compile-time constants
#define RXLC_OFFSET_INIT_(s, f, t, d) (int) offsetof(RXLC_STRUCT_##s, f),
static const int rxLcOffsetTable[RXLC_HOST_NOFF] = {
  (int) sizeof(rx_solving_options_ind),
  RX_LINCMT_CARRY_MAXPAIRS,
  RX_LINCMT_ORIGIN_MAX,
  RXLC_HOST_FIELDS(RXLC_OFFSET_INIT_)
};

static SEXP rxLcHostOffsets(void) {
  SEXP off = Rf_allocVector(INTSXP, RXLC_HOST_NOFF);
  std::memcpy(INTEGER(off), rxLcOffsetTable, sizeof(rxLcOffsetTable));
  return off;
}

static SEXP rxLcFnPtr(DL_FUNC fn) {
  return R_MakeExternalPtrFn(fn, R_NilValue, R_NilValue);
}

// Host table in the wire format of rxode2lincmtHost.h: offsets, then the
// host functions in RXLC_HOST_FNS order (getTime, rxThreadSlot, getRxSolve)
SEXP _rxode2_rxode2lincmtHost(void) {
  rxProtect rx_protect;
  SEXP ret = rx_protect.protect(Rf_allocVector(VECSXP, 1 + RXLC_HOST_NFNS));
  SET_VECTOR_ELT(ret, 0, rxLcHostOffsets());
  SET_VECTOR_ELT(ret, 1, rxLcFnPtr((DL_FUNC)&getTime));
  SET_VECTOR_ELT(ret, 2, rxLcFnPtr((DL_FUNC)&rxode2LinCmtThread));
  SET_VECTOR_ELT(ret, 3, rxLcFnPtr((DL_FUNC)&getRxSolve_));
  return ret;
}

// Test hook: which lincmt slots are bound to something other than their stub
SEXP _rxode2_rxode2lincmtLinked(void) {
  rxProtect rx_protect;
  SEXP ret = rx_protect.protect(Rf_allocVector(LGLSXP, 9));
  int *p = LOGICAL(ret);
  p[0] = _p_linCmtA != &rxLcStubA;
  p[1] = _p_linCmtB != &rxLcStubB;
  p[2] = _p_ensureLinCmtA != &rxLcStubEnsureA;
  p[3] = _p_ensureLinCmtB != &rxLcStubEnsureB;
  p[4] = _p_linCmtBindFree != &rxLcStubBindFree;
  p[5] = _p_linCmtScaleInitPar != &rxLcStubScaleInitPar;
  p[6] = _p_linCmtScaleInitN != &rxLcStubScaleInitN;
  p[7] = _p_linCmtZeroJac != &rxLcStubZeroJac;
  p[8] = _p_linCmtFreeInd != &rxLcStubFreeInd;
  return ret;
}

}
