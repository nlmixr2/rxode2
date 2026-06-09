// CVODE solver implementation using vendored SUNDIALS sources (from sundialr).
// Uses CVODE for plain ODE integration (no sensitivity analysis).
// This file has NO dependency on Rcpp, R, or Stan math headers.

#include "cvode_solver.h"

#include <stdlib.h>
#include <cvode/cvode.h>
#include <cvode/cvode_ls.h>
#include <nvector/nvector_serial.h>
#include <sunmatrix/sunmatrix_dense.h>
#include <sunlinsol/sunlinsol_dense.h>
#include <sunlinsol/sunlinsol_spgmr.h>
#include <sunlinsol/sunlinsol_spbcgs.h>
#include <sunlinsol/sunlinsol_sptfqmr.h>

struct cvode_ctx_t {
  void           *mem;
  SUNContext      sunctx;
  N_Vector        y;
  N_Vector        atol_v;
  N_Vector        dky;    // scratch for CVodeGetDky (dense output)
  SUNMatrix       A;
  SUNLinearSolver LS;
  sunindextype    neq;
  cvode_rhs_fn_t  rhs;
  void           *rhs_data;
  double          tret;   // time at end of last CV_ONE_STEP (dense output)
};

static int cvode_rhs_wrapper(sunrealtype t, N_Vector y, N_Vector ydot,
                               void *user_data) {
  cvode_ctx_t *ctx = (cvode_ctx_t *)user_data;
  return ctx->rhs((double)t,
                  N_VGetArrayPointer(y),
                  N_VGetArrayPointer(ydot),
                  ctx->rhs_data);
}

static void cvode_ctx_free_internals(cvode_ctx_t *ctx) {
  if (ctx->LS)     { SUNLinSolFree(ctx->LS);      ctx->LS     = NULL; }
  if (ctx->A)      { SUNMatDestroy(ctx->A);        ctx->A      = NULL; }
  if (ctx->dky)    { N_VDestroy(ctx->dky);          ctx->dky    = NULL; }
  if (ctx->atol_v) { N_VDestroy(ctx->atol_v);      ctx->atol_v = NULL; }
  if (ctx->y)      { N_VDestroy(ctx->y);            ctx->y      = NULL; }
  if (ctx->mem)    { CVodeFree(&ctx->mem);          ctx->mem    = NULL; }
  if (ctx->sunctx) { SUNContext_Free(&ctx->sunctx); ctx->sunctx = NULL; }
}

extern "C"
cvode_ctx_t *cvode_ctx_create(int neq, double *yp, double *atol, double rtol,
                               double t0, double hmin, double hmax, int mxstep,
                               cvode_rhs_fn_t rhs, void *rhs_data, int lin_type) {
  cvode_ctx_t *ctx = (cvode_ctx_t *)calloc(1, sizeof(cvode_ctx_t));
  if (!ctx) return NULL;

  ctx->rhs      = rhs;
  ctx->rhs_data = rhs_data;
  ctx->neq      = (sunindextype)neq;

  // SUNComm is int (non-MPI serial build); pass 0 not NULL to avoid -Wconversion-null
  if (SUNContext_Create(0, &ctx->sunctx) != 0) {
    free(ctx); return NULL;
  }

  ctx->y = N_VMake_Serial(ctx->neq, yp, ctx->sunctx);
  if (!ctx->y) { cvode_ctx_free_internals(ctx); free(ctx); return NULL; }

  ctx->mem = CVodeCreate(CV_BDF, ctx->sunctx);
  if (!ctx->mem) { cvode_ctx_free_internals(ctx); free(ctx); return NULL; }

  if (CVodeInit(ctx->mem, cvode_rhs_wrapper, (sunrealtype)t0, ctx->y) < 0) {
    cvode_ctx_free_internals(ctx); free(ctx); return NULL;
  }

  // Use ctx as user_data so cvode_rhs_wrapper can reach rhs and rhs_data.
  if (CVodeSetUserData(ctx->mem, (void *)ctx) < 0) {
    cvode_ctx_free_internals(ctx); free(ctx); return NULL;
  }

  ctx->atol_v = N_VNew_Serial(ctx->neq, ctx->sunctx);
  if (!ctx->atol_v) { cvode_ctx_free_internals(ctx); free(ctx); return NULL; }
  {
    double *av = N_VGetArrayPointer(ctx->atol_v);
    for (sunindextype k = 0; k < ctx->neq; k++) av[k] = atol[k];
  }

  if (CVodeSVtolerances(ctx->mem, (sunrealtype)rtol, ctx->atol_v) < 0) {
    cvode_ctx_free_internals(ctx); free(ctx); return NULL;
  }

  ctx->dky = N_VNew_Serial(ctx->neq, ctx->sunctx);
  if (!ctx->dky) { cvode_ctx_free_internals(ctx); free(ctx); return NULL; }

  ctx->tret = t0;

  if (hmin > 0.0) CVodeSetMinStep(ctx->mem, (sunrealtype)hmin);
  if (hmax > 0.0) CVodeSetMaxStep(ctx->mem, (sunrealtype)hmax);
  CVodeSetMaxNumSteps(ctx->mem, (long int)(mxstep > 0 ? mxstep : 5000));

  switch (lin_type) {
  case 3:
    ctx->LS = SUNLinSol_SPGMR(ctx->y, SUN_PREC_NONE, 0, ctx->sunctx);
    break;
  case 4:
    ctx->LS = SUNLinSol_SPBCGS(ctx->y, SUN_PREC_NONE, 0, ctx->sunctx);
    break;
  case 5:
    ctx->LS = SUNLinSol_SPTFQMR(ctx->y, SUN_PREC_NONE, 0, ctx->sunctx);
    break;
  default: /* 1: dense, 2: band aliases to dense until real bandwidth is known */
    ctx->A  = SUNDenseMatrix(ctx->neq, ctx->neq, ctx->sunctx);
    if (!ctx->A) { cvode_ctx_free_internals(ctx); free(ctx); return NULL; }
    ctx->LS = SUNLinSol_Dense(ctx->y, ctx->A, ctx->sunctx);
    break;
  }
  if (!ctx->LS) { cvode_ctx_free_internals(ctx); free(ctx); return NULL; }

  if (CVodeSetLinearSolver(ctx->mem, ctx->LS, ctx->A) < 0) {
    cvode_ctx_free_internals(ctx); free(ctx); return NULL;
  }

  return ctx;
}

extern "C"
void cvode_ctx_destroy(cvode_ctx_t *ctx) {
  if (!ctx) return;
  cvode_ctx_free_internals(ctx);
  free(ctx);
}

extern "C"
int cvode_ctx_integrate(cvode_ctx_t *ctx, double *yp, double t0, double tout) {
  N_VSetArrayPointer(yp, ctx->y);
  if (CVodeReInit(ctx->mem, (sunrealtype)t0, ctx->y) < 0) return -1;
  sunrealtype tret;
  int ret = CVode(ctx->mem, (sunrealtype)tout, ctx->y, &tret, CV_NORMAL);
  ctx->tret = (double)tret;
  return (ret >= 0) ? 1 : -1;
}

extern "C"
void cvode_ctx_dense_reinit(cvode_ctx_t *ctx, double *yp, double t0) {
  N_VSetArrayPointer(yp, ctx->y);
  CVodeReInit(ctx->mem, (sunrealtype)t0, ctx->y);
  ctx->tret = t0;
}

extern "C"
double cvode_ctx_dense_current_time(const cvode_ctx_t *ctx) {
  return ctx->tret;
}

extern "C"
int cvode_ctx_dense_do_step(cvode_ctx_t *ctx, double *yp, double tout_dir) {
  N_VSetArrayPointer(yp, ctx->y);
  sunrealtype tret;
  int ret = CVode(ctx->mem, (sunrealtype)tout_dir, ctx->y, &tret, CV_ONE_STEP);
  if (ret < 0) return -1;
  ctx->tret = (double)tret;
  return 1;
}

extern "C"
int cvode_ctx_dense_calc_state(cvode_ctx_t *ctx, double t, double *yp) {
  if (CVodeGetDky(ctx->mem, (sunrealtype)t, 0, ctx->dky) < 0) return -1;
  double *d = N_VGetArrayPointer(ctx->dky);
  for (sunindextype i = 0; i < ctx->neq; i++) yp[i] = d[i];
  return 1;
}
