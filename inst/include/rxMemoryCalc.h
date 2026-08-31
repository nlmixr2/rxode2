#pragma once
#ifndef RXODE2_MEMORY_CALC_H
#define RXODE2_MEMORY_CALC_H

#include <stdint.h>

/* rxLlikSaveSize is defined in rxode2.h; provide a fallback so this header
   can be included in translation units that do not pull in the full rxode2
   header chain (e.g. standalone tests). */
#ifndef rxLlikSaveSize
#define rxLlikSaveSize 9
#endif

/* ---------------------------------------------------------------------------
 * rx_mem_layout
 *
 * Holds every intermediate element count produced by rxFillMemLayout().
 * Fields use the same names as the local variables in rxData.cpp so that
 * the two call-sites (rxData.cpp allocation and rxMemoryComponents_ estimate)
 * are directly comparable.
 *
 * All counts are in ELEMENTS, not bytes.  Multiply by sizeof(double) or
 * sizeof(int) as appropriate.
 * --------------------------------------------------------------------------- */
typedef struct {
  /* --- gsolve components (all double) --- */
  int64_t n0;          /* nall * state_size * nsim  -- ODE output matrix      */
  int64_t nlin;        /* linB * 7 * nsub * nsim    -- linCmt scratch          */
  int64_t nsave;       /* neq * cores               -- used x4 (Save/Last/Last2/EsPendingJump) */
  int64_t n2;          /* nMtime * nsub * nsim      -- mtime arrays            */
  int64_t n3a_c;       /* (neq + extraCmt) * cores  -- gTlastS/firstS/CurDose  */
  int64_t n4;          /* initsC.size()  (~ neq for estimates)                */
  int64_t n5_c;        /* nlhs * cores              -- per-thread LHS buffer   */
  int64_t n6;          /* scaleC.size()  (~ neq for estimates)                */
  int64_t n7;          /* nIndSim * nsub * nsim     -- per-individual sim info  */
  int64_t n8;          /* maxAllTimes * cores       -- per-thread time buffer   */
  int64_t n9;          /* (numLinSens+numLin)*cores -- linSens (FOCEi)          */
  int64_t n10;         /* neq * cores               -- linDummy                */
  int64_t n11;         /* 4 * neq * cores           -- per-thread tol arrays   */
  int64_t nmtime0_c;   /* nMtime * cores            -- per-thread mtime copy   */
  int64_t nllik_c;     /* rxLlikSaveSize * nLlik * cores -- llik save buffer   */
  int64_t gsolve_total;/* sum of all above -- passed directly to calloc        */

  /* --- gon components (all int) --- */
  int64_t nSize;       /* nsim * nsub                                          */
  int64_t n3;          /* neq * nSize               -- BadDose array            */
  int64_t gon_total;   /* n3a_c + n3 + 4*nSize + nall*nsim                    */
} rx_mem_layout;

/* ---------------------------------------------------------------------------
 * rxFillMemLayout
 *
 * Computes every element count and stores them in *out.
 *
 * Parameters that differ between the estimate path and the actual-alloc path:
 *   state_size  -- use state.size() in rxData.cpp; use neq for estimates
 *   n4_actual   -- use initsC.size() in rxData.cpp; use neq for estimates
 *   n6_actual   -- use scaleC.size() in rxData.cpp; use neq for estimates
 * --------------------------------------------------------------------------- */
static inline void rxFillMemLayout(
  int     neq,
  int     state_size,   /* state.size() from modVars -- equals neq for pure ODE */
  int     nlhs,
  int     nsim,
  int     cores,
  int     nMtime,
  int     extraCmt,
  int     linB,
  int     nLlik,
  int     nIndSim,
  int     nsub,
  int64_t nall,
  int     maxAllTimes,
  int     numLinSens,
  int     numLin,
  int64_t n4_actual,    /* initsC.size(); pass neq for estimates               */
  int64_t n6_actual,    /* scaleC.size(); pass neq for estimates               */
  rx_mem_layout *out)
{
  out->nSize     = (int64_t)nsim * nsub;
  out->n0        = nall * (int64_t)state_size * nsim;
  out->nlin      = (int64_t)linB * 7 * nsub * nsim;
  out->nsave     = (int64_t)neq * cores;
  out->n2        = (int64_t)nMtime * nsub * nsim;
  out->n3a_c     = ((int64_t)neq + extraCmt) * cores;
  out->n4        = n4_actual;
  out->n5_c      = (int64_t)nlhs * cores;
  out->nllik_c   = (int64_t)rxLlikSaveSize * nLlik * cores;
  out->n6        = n6_actual;
  out->n7        = (int64_t)nIndSim * nsub * nsim;
  out->n8        = (int64_t)maxAllTimes * cores;
  out->n9        = ((int64_t)numLinSens + numLin) * cores;
  out->n10       = (int64_t)neq * cores;
  out->n11       = (int64_t)4 * neq * cores;
  out->nmtime0_c = (int64_t)nMtime * cores;

  /* gsolve_total mirrors the calloc expression in rxData.cpp exactly */
  out->gsolve_total =
    out->n0        +
    out->nlin      +
    4 * out->nsave +  /* gSolveSave, gSolveLast, gSolveLast2, gEsPendingJump */
    out->n2        +
    out->n4        +
    out->n5_c      +
    out->n6        +
    out->n7        +
    out->n8        +
    out->n9        +
    out->n10       +
    out->n11       +
    out->nmtime0_c +
    (int64_t)5 * neq    +  /* ypNA + gatol2 + grtol2 + gssRtol + gssAtol    */
    3 * out->n3a_c      +  /* gTlastS, gTfirstS, gCurDoseS (each n3a_c); gIndSim counted via n7 */
    out->nllik_c;

  /* gon_total mirrors the calloc expression for _globals.gon in rxData.cpp */
  out->n3        = (int64_t)neq * out->nSize;
  out->gon_total = out->n3a_c + out->n3 + (int64_t)4 * out->nSize + nall * nsim;
}

/* ---------------------------------------------------------------------------
 * rx_ind_alloc / rxFillIndAllocTotal
 *
 * When op->indOwnAlloc is set, rxAllocInd() (src/rxData.cpp) gives every
 * individual its OWN dose/ii/all_times/timeThread/evid/ix/idose/solve arrays
 * instead of pointing them into the global buffers.  gsolve is still calloc'd
 * at gsolve_total, so its n0 region simply goes unused -- the per-individual
 * solve arrays are memory ON TOP of it, not instead of it.  An estimate that
 * ignores them undercounts, which is the direction that makes an
 * out-of-memory guard useless.
 *
 * op->indOwnAlloc is NOT the same as method="indLin"; it defaults to the
 * model's evid_ parser flag (dose-pushing models) and can be forced with
 * rxSolve(..., indOwnAlloc = TRUE).
 * --------------------------------------------------------------------------- */
#ifndef EVID_EXTRA_SIZE
#define EVID_EXTRA_SIZE 16
#endif

typedef struct {
  int64_t dbl;  /* doubles: solve + dose + ii + all_times + timeThread */
  int64_t i32;  /* ints:    evid + ix + idose                          */
} rx_ind_alloc;

/* Summed over every individual (nsub * nsim of them).  `nall` and `ndoses` are
   totals across all subjects for ONE simulation; the per-individual guard
   elements (+1) and the solve slack (EVID_EXTRA_SIZE) are charged per
   individual, matching rxAllocInd() exactly. */
static inline void rxFillIndAllocTotal(
  int     neq,
  int64_t nall,
  int64_t ndoses,
  int     nsub,
  int     nsim,
  rx_ind_alloc *out)
{
  int64_t nind = (int64_t)nsub * nsim;   /* individuals rxAllocInd() runs for */
  int64_t nat  = nall * nsim;            /* events across all of them         */
  int64_t nd   = ndoses * nsim;
  /* newSolve is neq * (nat + EVID_EXTRA_SIZE) per individual */
  out->dbl = (int64_t)neq * (nat + (int64_t)EVID_EXTRA_SIZE * nind) +
    /* newDose, newIi, newAT, newTT: each nat + 1 per individual */
    4 * (nat + nind);
  /* newEvid, newIx: each nat + 1 per individual; newIdose: nd + 1 */
  out->i32 = 2 * (nat + nind) + (nd + nind);
}

#endif /* RXODE2_MEMORY_CALC_H */
