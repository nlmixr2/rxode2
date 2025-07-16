#ifndef __EXTRA_DOSING_H__
#define __EXTRA_DOSING_H__

static inline void freeExtraDosing() {
  int i = 0;

  if (_globals.pendingDoses != NULL) {
    for (i = 0; i < _globals.extraDoseCores; i++) {
      if (_globals.pendingDoses[i] != NULL) {
        free(_globals.pendingDoses[i]);
        _globals.pendingDoses[i] = NULL;
      }
    }
    free(_globals.pendingDoses);
    _globals.pendingDoses=NULL;
  }

  if (_globals.ignoredDoses != NULL) {
    for (i = 0; i < _globals.extraDoseCores; i++) {
      if (_globals.ignoredDoses[i] != NULL){
        free(_globals.ignoredDoses[i]);
        _globals.ignoredDoses[i] = NULL;
      }
    }
    free(_globals.ignoredDoses);
    _globals.ignoredDoses=NULL;
  }

  if (_globals.extraDoseTimeIdx != NULL) {
    for (i = 0; i < _globals.extraDoseCores; i++) {
      if (_globals.extraDoseTimeIdx[i] != NULL){
        free(_globals.extraDoseTimeIdx[i]);
        _globals.extraDoseTimeIdx[i] = NULL;
      }
    }
    free(_globals.extraDoseTimeIdx);
    _globals.extraDoseTimeIdx=NULL;
  }

  if (_globals.extraDoseTime != NULL) {
    for (i = 0; i < _globals.extraDoseCores; i++) {
      if (_globals.extraDoseTime[i] != NULL) {
        free(_globals.extraDoseTime[i]);
        _globals.extraDoseTime[i] = NULL;
      }
    }
    _globals.extraDoseTime=NULL;
  }

  if (_globals.extraDoseEvid != NULL) {
    for (i = 0; i < _globals.extraDoseCores; i++) {
      if (_globals.extraDoseEvid[i] != NULL) {
        free(_globals.extraDoseEvid[i]);
        _globals.extraDoseEvid[i] = NULL;
      }
    }
    free(_globals.extraDoseEvid);
    _globals.extraDoseEvid=NULL;
  }

  if (_globals.extraDoseDose != NULL) {
    for (i = 0; i < _globals.extraDoseCores; i++) {
      if (_globals.extraDoseDose[i] != NULL) {
        free(_globals.extraDoseDose[i]);
        _globals.extraDoseDose[i] = NULL;
      }
    }
    _globals.extraDoseDose=NULL;
  }

  if (_globals.extraDoseN != NULL) {
    free(_globals.extraDoseN);
  }
  _globals.extraDoseN = NULL;

  if (_globals.extraDoseAllocN != NULL) {
    free(_globals.extraDoseAllocN);
  }
  _globals.extraDoseAllocN = NULL;

  if (_globals.nPendingDoses != NULL) {
    free(_globals.nPendingDoses);
  }
  _globals.nPendingDoses = NULL;

  if (_globals.nAllocPendingDoses != NULL) {
    free(_globals.nAllocPendingDoses);
  }
  _globals.nAllocPendingDoses = NULL;

  if (_globals.nIgnoredDoses != NULL) {
    free(_globals.nIgnoredDoses);
  }
  _globals.nIgnoredDoses = NULL;

  _globals.extraDoseCores = 0;
}

static inline void allocExtraDosing(int ncores) {

  freeExtraDosing();

  int *tmpI = (int*)malloc((ncores) * sizeof(int));
  if (tmpI == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.extraDoseN = tmpI;

  tmpI = (int*)malloc((ncores) * sizeof(int));
  if (tmpI == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.extraDoseAllocN = tmpI;

  tmpI = (int*)malloc((ncores) * sizeof(int));
  if (tmpI == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.nPendingDoses = tmpI;

  tmpI = (int*)malloc((ncores) * sizeof(int));
  if (tmpI == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.nAllocPendingDoses = tmpI;


  tmpI = (int*)malloc((ncores) * sizeof(int));
  if (tmpI == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.nIgnoredDoses = tmpI;


  tmpI = (int*)malloc((ncores) * sizeof(int));
  if (tmpI == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.nAllocIgnoredDoses = tmpI;


  int **tmpII = (int**)malloc((ncores+1) * sizeof(int*));
  if (tmpII == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.pendingDoses = tmpII;

  tmpII = (int**)malloc((ncores+1) * sizeof(int*));
  if (tmpII == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.ignoredDoses = tmpII;

  tmpII = (int**)malloc((ncores+1) * sizeof(int*));
  if (tmpII == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.extraDoseTimeIdx = tmpII;

  tmpII = (int**)malloc((ncores+1) * sizeof(int*));
  if (tmpII == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.extraDoseEvid = tmpII;

  double **tmpDD = (double**)malloc((ncores+1) * sizeof(double*));
  if (tmpDD == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.extraDoseTime = tmpDD;

  tmpDD = (double**)malloc((ncores+1) * sizeof(double*));
  if (tmpDD == NULL) {
    rxSolveFree();
    stop(_("ran out of memory"));
  }
  _globals.extraDoseDose = tmpDD;

  double *tmpD;
  for (int i = 0; i < ncores; i++) {
    tmpI = (int*)malloc(EVID_EXTRA_SIZE * sizeof(int));
    if (tmpI == NULL) {
      rxSolveFree();
      stop(_("ran out of memory"));
    }
    _globals.pendingDoses[i] = tmpI;

    _globals.nPendingDoses[i] = 0;
    _globals.nAllocPendingDoses[i] = EVID_EXTRA_SIZE;

    tmpI = (int*)malloc(EVID_EXTRA_SIZE * sizeof(int));
    if (tmpI == NULL) {
      rxSolveFree();
      stop(_("ran out of memory"));
    }
    _globals.ignoredDoses[i] =  tmpI;

    _globals.nIgnoredDoses[i] = 0;
    _globals.nAllocIgnoredDoses[i] = EVID_EXTRA_SIZE;
    tmpI = (int*)malloc(EVID_EXTRA_SIZE * sizeof(int));

    if (tmpI == NULL) {
      rxSolveFree();
      stop(_("ran out of memory"));
    }
    _globals.extraDoseTimeIdx[i] = tmpI;

    tmpI = (int*)malloc(EVID_EXTRA_SIZE * sizeof(int));
    if (tmpI == NULL) {
      rxSolveFree();
      stop(_("ran out of memory"));
    }
    _globals.extraDoseEvid[i] = tmpI;

    tmpD = (double*)malloc(EVID_EXTRA_SIZE * sizeof(double));
    if (tmpD == NULL) {
      rxSolveFree();
      stop(_("ran out of memory"));
    }
    _globals.extraDoseTime[i] = tmpD;

    tmpD = (double*)malloc(EVID_EXTRA_SIZE * sizeof(double));
    if (tmpD == NULL) {
      rxSolveFree();
      stop(_("ran out of memory"));
    }
    _globals.extraDoseDose[i] = tmpD;
    _globals.extraDoseAllocN[i] = EVID_EXTRA_SIZE;
    _globals.extraDoseN[i] = 0;
  }
  _globals.pendingDoses[ncores]     = NULL;
  _globals.ignoredDoses[ncores]     = NULL;
  _globals.extraDoseTimeIdx[ncores] = NULL;
  _globals.extraDoseEvid[ncores]    = NULL;
  _globals.extraDoseTime[ncores]    = NULL;
  _globals.extraDoseDose[ncores]    = NULL;
  _globals.extraDoseCores            = ncores;
}

static inline void resetExtraDosing() {
  for (int i = 0; i < _globals.extraDoseCores; i++) {
    _globals.nPendingDoses[i] = 0;
    _globals.nIgnoredDoses[i] = 0;
    _globals.extraDoseN[i] = 0;
  }
}

#endif
