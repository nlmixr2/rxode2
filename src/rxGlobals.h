#pragma once
// rx_globals: shared definition for rxData.cpp and rxSerialize.cpp
struct rx_globals {
  double *gLin;
  double *gLlikSave;
  double *gSolveLast2;
  double *gSolveLast;
  double *gSolveSave;
  int *gon;
  double *gIndSim;
  double *gsolve;
  double **gInfusionRate;
  int    *gInfusionRateN;
  int     nInfusionRateThreads;
  double *gTlastS;
  double *gTfirstS;
  double *gCurDoseS;
  double *gall_times;
  double *gall_timesS;
  int *gix;
  double *gdv;
  double *glimit;
  int *gcens;
  double *gamt;
  double *gamtS;
  double *gii;
  double *glhs;
  double *gcov;
  double *ginits;
  double *gscale;
  double *gatol2;
  double *grtol2;
  double *gssAtol;
  double *gssRtol;
  double *gpars;
  double *gLinSave;
  double *gLinDummy;
  int *gevid;
  int *gBadDose;
  int *grc;
  int *gidose;
  int *gpar_cov;
  int *gpar_covInterp;
  int *glhs_str;
  int *gParPos;
  int *gParPos2;
  int *gsvar;
  int *govar;
  int *gsiV;
  int *gsi;
  int *slvr_counter;
  int *dadt_counter;
  int *jac_counter;
  int *gSampleCov;
  double *gmtime;
  double *gmtime0;
  double *gsigma = NULL;
  int nSigma = 0;
  double *gomega = NULL;
  int nOmega = 0;
  int *ordId = NULL;
  bool zeroTheta = false;
  bool zeroOmega = false;
  bool zeroSigma = false;
  int *gindLin = NULL;
  int *gdelayState = NULL; /* delay() history column -> ODE state index */
  int *gdelayCol = NULL;   /* ODE state index -> delay() history column (-1 if none) */
  int **pendingDoses = NULL;
  int *  nPendingDoses = NULL;
  int *  nAllocPendingDoses = NULL;
  int **ignoredDoses = NULL;
  int *  nIgnoredDoses = NULL;
  int *  nAllocIgnoredDoses = NULL;
  int    **extraDoseTimeIdx = NULL;
  double **extraDoseTime    = NULL;
  int    **extraDoseEvid    = NULL;
  double **extraDoseDose    = NULL;
  int     *extraDoseN       = NULL;
  int     *extraDoseAllocN  = NULL;
  int extraDoseCores = 0;
  double *timeThread = NULL;
  double *gatol2Thread = NULL;
  double *grtol2Thread = NULL;
  double *gssAtolThread = NULL;
  double *gssRtolThread = NULL;
  double *geta_pre = NULL;      // active pointer (NULL when deactivated)
  double *geta_pre_alloc = NULL; // actual allocated buffer (survives deactivate)
  int geta_pre_n = 0;            // capacity of geta_pre_alloc
  bool alloc=false;
  int64_t gall_times_n = 0; // actual allocation count of gall_times (representative, not expanded)
};
