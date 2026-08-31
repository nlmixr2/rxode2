// Extra event slots pre-allocated in ind->solve / indOwnAllocN to absorb
// _rxPushDose growth without requiring a solve-buffer realloc mid-integration.
#ifndef EVID_EXTRA_SIZE
#define EVID_EXTRA_SIZE 16
#endif

typedef struct sbuf {
  char *s;        /* curr print buffer */
  int sN;
  int o;                        /* offset of print buffer */
} sbuf;

typedef struct vLines {
  char *s;
  int sN;
  int o;
  int n;
  int nL;
  char **line;
  int *lProp;
  int *lType;
  int *os;
} vLines;

#define rxode2naTimeInputIgnore 1
#define rxode2naTimeInputWarn   2
#define rxode2naTimeInputError  3

/* stateProp bit marking an ODE state referenced by delay(state, T).  Must match
   propDelay in src/tran.h. */
#define rxDelayStateProp 262144

struct rx_solving_options_ind_s;
typedef struct rx_solving_options_ind_s rx_solving_options_ind;

struct rx_solve_s;
typedef struct rx_solve_s rx_solve;

typedef double (*t_F)(int _cSub,  int _cmt, double _amt, double t, double *y);
typedef double (*t_LAG)(int _cSub,  int _cmt, double t, double *y);
typedef double (*t_RATE)(int _cSub,  int _cmt, double _amt, double t, double *y);
typedef double (*t_DUR)(int _cSub,  int _cmt, double _amt, double t, double *y);
// Event ("jump") sensitivities: total derivatives of the modeled alag / F
// fractions wrt each first-order sensitivity parameter, written into a flat
// per-subject buffer indexed (cmt0 * nSensParam + paramIdx).  Fill-all-states in
// one call (like Lag/F fill all _alag/_f), evaluated at the supplied state y.
typedef void (*t_dLag)(int _cSub, double t, double *y, double *_dLagSave);
typedef void (*t_dF)(int _cSub, double t, double *y, double *_dFSave);
typedef void (*t_dRate)(int _cSub, double t, double *y, double *_dRateSave);
typedef void (*t_dDur)(int _cSub, double t, double *y, double *_dDurSave);

typedef void (*t_calc_mtime)(int cSub, double *mtime, double *y);

typedef void (*t_ME)(int _cSub, double _t, double t, double *_mat, const double *__zzStateVar__);
typedef void (*t_IndF)(int _cSub, double _t, double t, double *_mat, const double *__zzStateVar__);

typedef double (*t_getTime)(int idx, rx_solving_options_ind *ind);
typedef int (*t_locateTimeIndex)(double obs_time,  rx_solving_options_ind *ind);
typedef int (*t_handle_evidL)(int evid, double *yp, double xout, int id, rx_solving_options_ind *ind) ;
typedef double (*t_getDur)(int l, rx_solving_options_ind *ind, int backward, unsigned int *p);

typedef struct {
  t_F f;
  t_LAG lag;
  t_RATE rate;
  t_DUR dur;
  t_calc_mtime mtime;
  t_ME me;
  t_IndF indf;
  t_getTime gettime;
  t_locateTimeIndex timeindex;
  t_handle_evidL handleEvid;
  t_getDur getdur;
} rx_fn_pointers;

typedef struct {
  // These options should not change based on an individual solve
  int badSolve;
  int naTime;
  int naTimeInput;
  int naTimeInputWarn;
  double ATOL; //absolute error
  double RTOL; //relative error
  double H0;
  double HMIN;
  int mxstep;
  int MXORDN;
  int MXORDS;
  //
  int nlhs;
  int neq;
  int stiff;
  int ncov;
  char modNamePtr[1000];
  int *par_cov;
  int *par_cov_interp;
  int *lhs_str;

  double *inits;
  double *scale;
  bool do_par_cov;
  // approx fun options
  int is_locf;
  int instant_backward;
  int keep_interp;
  int cores;
  int doesRandom;
  int extraCmt;
  double hmax2; // Determined by diff
  double *rtol2;
  double *atol2;
  double *ssRtol;
  double *ssAtol;
  int *indLin;
  int indLinN;
  double indLinPhiTol;
  int indLinPhiM;
  int indLinMatExpType;
  int indLinMatExpOrder;
  int indLinStepSearch; // 0 = none, 1 = secant, 2 = exact
  int indLinMaxIter;
  int indLinRichardson; // 1 = Richardson-extrapolate to third order
  int nDisplayProgress;
  int ncoresRV;
  int isChol;
  int nsvar;
  int abort;
  int minSS;
  int maxSS;
  int doIndLin;
  int strictSS;
  double infSSstep;
  int mxhnil;
  double hmxi;
  int nLlik;
  int numLinSens;
  int numLin;
  int depotLin;
  int linOffset;
  int ssSolved;
  int useDense;
  int hasDelay; /* model uses delay() -- record dense history for DDE lookups */
  int nDelayState; /* number of ODE states delay() looks back on (history columns) */
  int *delayState; /* [nDelayState] their ODE state indices (history column -> state) */
  int *delayCol;   /* [neq] ODE state index -> history column, -1 if not delayed */
  int indOwnAlloc;
  int cvodeLinSolver;
  int    stiff2;                /* secondary method code (stiff); 0 = no autoswitch */
  int    autoSwitchMaxStiff;    /* consecutive stiff detections before switching (default 10) */
  int    autoSwitchMaxNonstiff; /* consecutive nonstiff detections before switching back (default 3) */
  int    autoSwitchStiffFirst;  /* start with stiff algorithm (default 0 = FALSE) */
  double autoSwitchNonstifftol; /* stiffness ratio threshold in non-stiff mode (default 0.9) */
  double autoSwitchStifftol;    /* non-stiffness ratio threshold in stiff mode (default 0.9) */
  double autoSwitchDtfac;       /* dt multiplier on switch-to-stiff; divides on switch-back (default 2.0) */
  int    autoSwitchSwitchMax;   /* min intervals after a switch before switch-back allowed (default 5) */
  /* --- discrete-adjoint (rk4s and other `<base>s` methods) layout --- */
  int    adjoint;               /* 0 = off; 1 = in-engine discrete-adjoint mode */
  int    adjNbase;              /* number of base ODE states (stepped forward) */
  int    adjNp;                 /* number of adjoint parameters (calcSens count) */
  int    adjFxOff;              /* lhs index where F_X block starts (row-major i*ns+j) */
  int    adjFpOff;              /* lhs index where F_p block starts (i*np+p) */
  int    adjDfOff;              /* lhs index where dF/dtheta block starts (k*np+p); -1 if none */
  int    adjJpOff;              /* lhs index where dJ/dtheta block starts ((i*ns+j)*np+p); -1 if none (Rosenbrock) */
  int    adjJyOff;              /* lhs index where dJ/dy=f'' block starts ((i*ns+j)*ns+c); -1 if none (Rosenbrock nonlinear) */
  int    adjFxdOff;             /* lhs index where DDE delayed-Jacobian F_Xd block starts (i*ns+j); -1 if no delay() */
  int    adjTauOff;             /* lhs index where DDE delay-duration tau block starts (i*ns+j); -1 if no delay() */
  int    adjDtauOff;            /* lhs index where DDE dtau/dp block starts ((i*ns+j)*np+p); -1 if no delay() (dose-jump) */
  int    adjDlagOff;           /* lhs index where dlag/dtheta block starts (k*np+p); -1 if no modeled alag() (transversality) */
  int    adjDrateOff;          /* lhs index where drate/dtheta block starts (k*np+p); -1 if no modeled rate() (infusion dual) */
  int    adjSensOff;            /* solve-vector index where rx__sens_* output slots begin */
  int    cmtCov;               /* covariate index (into par_cov/cov_ptr) of the CMT covariate, cached at setup; -1 if the model has no CMT covariate (single endpoint) */
  int    indLinIteration;      /* method="indLin" substep scheme: 0 picard, 1 newton, 2 exprb, 3 auto (stiffness-gated) */
  int    indLinJac;            /* forcing Jacobian source: 0 auto, 1 symbolic (calc_jac - A), 2 finite difference */
  int    indLinForcing;        /* forcing over a substep: 0 constant, 1 linear ramp between its endpoint values */
  /* Bitmask over the linCmt() block's physical compartments (bit c = block
     index c, 0 = depot when oral) of those carrying a modeled alag().  Set at
     solve setup from the model's stateProp; read by linCmtBdoseTime()
     (which1 = -3) to tell whether every dose reaching the linear system
     really does share one delay (nlmixr2/rxode2#1119, #1237).  0 when the
     model declares no modeled alag() on a linCmt() compartment. */
  int    linCmtLagMask;
} rx_solving_options;


struct rx_solving_options_ind_s {
  double bT;
  int *slvr_counter;
  int *dadt_counter;
  int *jac_counter;
  double *InfusionRate;
  int *BadDose;
  int nBadDose;
  double HMAX; // Determined by diff
  double tlast;
  double curDose;
  int dosenum;
  double tfirst;
  double *tlastS;
  double *curDoseS;
  double *tfirstS;
  double podo;
  double *podoS;
  double *par_ptr; // both time changing and time invariant
  double *dose;
  double *ii;
  double *solve;
  double *mtime;
  double *mtime0;    // initial sort-time mtime values; sentinel R_NegInf = already fired
  double *solveSave;
  double *solveLast;
  double *solveLast2;
  double *lhs;
  int  *evid;
  int *rc;
  double *cov_ptr;
  int *cov_sample;
  // a b
  // 1 4
  // 2 5
  // 3 6
  int n_all_times;
  int n_all_times_orig;
  int nevid2;
  int ixds;
  int ndoses;
  double *all_times;
  int *ix;
  double *dv;
  double *limit;
  int *cens;
  int *idose;
  int *on;
  int idosen;
  int id;
  int idReal;
  int sim;
  int idx;
  int solvedIdx;
  double ylow;
  double yhigh;
  double logitHi;
  double logitLow;
  double lambda;
  double yj;
  // Saved info
  int wh;
  int wh100;
  int cmt;
  int whI;
  int wh0;
  int doSS;
  int allCovWarn;
  int wrongSSDur;
  int _newind;
  int _rxFlag;
  int err;
  int solved;
  double *linCmtSave;
  double *linCmtAlast;
  double *linCmtDummy;
  int linCmt;
  int cacheME;
  int inLhs;
  double solveTime;
  double curShift;
  double *simIni;
  int isIni;
  int _update_par_ptr_in;
  int badIni;
  double *llikSave;
  // Add pointers for drifting atol/rtol values during optimization
  double *rtol2;
  double *atol2;
  double *ssRtol;
  double *ssAtol;
  // ignored and pending doses
  int *ignoredDoses;
  int *ignoredDosesN;
  int *ignoredDosesAllocN;
  int *pendingDoses;
  int *pendingDosesN;
  int *pendingDosesAllocN;
  // extra doses
  int *extraDoseTimeIdx;
  int *extraDoseN;
  int *extraDoseAllocN;
  double *extraDoseTime;
  int *extraDoseEvid;
  double *extraDoseDose;
  double extraDoseNewXout;
  int idxExtra; // extra idx
  int extraSorted; // extra sorted?
  int mainSorted;  // 0 = main ix[] needs re-sort after runtime rate/dur/mtime update
  //double *extraDoseIi; // ii doses unsupported
  bool lastIsSs2;
  double *timeThread;
  int idxLow;
  int idxHi;
  double tprior;
  double tout;
  // This *DDtStateVar pointer is for the dydt solution
  // This is made so that `linCmtA` and `linCmtB` can
  // insert the solution when needed.
  // This is the state values for the current solve. These can also be
  // updated by the `linCmtA` and `linCmtB` functions.
  int linSS;
  int linSScmt;
  int linSSbolusCmt;
  double linSStau;
  double linSSvar;
  double ssTime;
  double* linH;
  // This indicates what the linear compartment is being calculated when calculating
  // the optimized H value.
  //
  // -1: Indicates this is the function value, not a parameter to optimize H for
  // -2: Indicates there is no optimization of the H value
  //
  // Otherwise this represents the index of the sensitivity parameter
  // that is being optimized for; This is related to the number of
  // parameters that request derivatives.
  //
  //
  int linCmtHparIndex;
  // When optimizing the H value, this is the value of H that is being optimized.
  double linCmtH;
  // When optimizing the H value, the function value of the linear
  // compartment volume is stored in the `linCmtHV` variable:
  double linCmtHV;

  // Add mixture estimate flag
  int mixest;

  // Add mixture uniform variable
  double mixunif;

  // Per-individual sticky tolerance factor; initialized to 1.0 by
  // setupRxInd() at first allocation and intentionally NOT reset on
  // subsequent calls so that any loosening applied via atolRtolFactor_()
  // or setIndTolFactor() persists across re-solves for stiff individuals.
  double tolFactor;

  // Per-individual neq override; -1 means use op->neq (default).  Set
  // via setIndNeqOverride()/getIndNeqOverride() so that downstream
  // packages (e.g. nlmixr2est's predOde / shi21EtaGeneral) can solve
  // with a different neq for a single individual without mutating the
  // shared op->neq from a parallel worker thread.  Cleared by
  // restoring -1.
  int neqOverride;

  // When 1, this individual owns its dose/ii/all_times/solve arrays
  // (independently malloc'd, not pointers into the global buffer)
  int indOwnAlloc;
  int indOwnAllocN;     // allocated capacity for event arrays (>= n_all_times)
  int solveAllocN;      // allocated capacity for ind->solve in units of events (neq doubles each)
  int idoseOwnAllocN;   // allocated capacity for idose (>= ndoses)
  int _atEventTime;     // set at an evid_() firing record; consumed once in calc_lhs
  int nPushedExtra;      // count of events pushed via evid_() for this individual this solve
  int    autoMethod;             /* 0 = using primary (non-stiff), 1 = using secondary (stiff) */
  int    autoCount;              /* positive = consecutive stiff detections; negative = nonstiff */
  double autoHcur;               /* current suggested step size for autoswitch (tracks dtfac scaling) */
  int    autoLastSwitchIntervals; /* intervals elapsed since last permanent method switch */
  // Delay differential equation (DDE) dense history.  Each record stores the
  // dop853 dense-output coefficients for one accepted step so that delay()
  // can interpolate any past state.  Layout per record (stride doubles):
  //   rcont1[0..neq-1], rcont2[..], ..., rcont8[..], xold, h
  double *delayHist;       /* flat ring/grow buffer, or NULL when unused */
  int     delayHistN;      /* number of step records currently stored */
  int     delayHistCap;    /* capacity in records */
  int     delayHistStride; /* doubles per record (8*neq+2 dop853, 4*neq+2 ros4) */
  int     delayHistNeq;    /* neq used when building records */
  int     delayHistType;   /* 0 = dop853 dense (rcont), 1 = ros4 cubic samples */
  int     delayHistOn;     /* 1 while recording is active for this subject */
  double  delayT0;         /* initial time; history before this is the IC */
  double  delayMinT;       /* smallest delay duration seen; caps the step size */
  int     delayWarmed;     /* 1 once the RHS has been evaluated to learn delays */
  // linCmtB(which1 = -3) (the dose-time/moving-boundary sensitivity, see
  // src/linCmt.cpp) needs the infusion rate at output time, but
  // ind->InfusionRate is only maintained while solving -- rxode2_df.cpp's
  // output pass clears it (nlmixr2/rxode2#1236).  linCmtB() records the live
  // rate here, once per output index, while it is actually being solved
  // (mirroring how the amounts themselves are cached via getAdvan()/Alast);
  // linCmtBdoseTime() reads it back on a re-query (the output pass, or any
  // backward re-query of an already-solved index) instead of the
  // (by-then-cleared) live rate.  Flat idx-major layout: idx*linCmtRateHistW
  // + j, j in [0, linCmtRateHistW).  Kept after the solve (freed with the
  // subject) so the output pass can still read it, like delayHist above.
  double *linCmtRateHist;   /* flat idx-indexed rate history, or NULL when unused */
  int     linCmtRateHistCap; /* capacity in idx slots */
  int     linCmtRateHistW;   /* doubles per idx slot (op->numLin); realloc if changed */
  // Cumulative sensitivity carry state for the linCmt()-time-varying-covariate
  // fix (project_lincmt_timevarying_covariate_bug): d(Alast_i)/d(eta), one
  // column per carried (linCmt-parameter, eta) pair, one row per linCmt()
  // compartment (up to 4, ncmt+oral0 <= 3+1). Up to RX_LINCMT_CARRY_MAXPAIRS
  // simultaneous pairs (8 covers an eta on every one of the 7 linCmt()
  // parameters plus one spare; ~256 bytes/subject, still tiny). Fixed flat,
  // row-major, stride RX_LINCMT_CARRY_MAXPAIRS regardless of the model's
  // actual m so iniSubject() can reset it without needing to know m --
  // linCmtB(which1=-5/-6/-7, src/linCmt.cpp) only ever reads/writes the top
  // m rows. The model-build layer (nlmixr2est's carry codegen) must enforce
  // the pair cap BEFORE emitting code; linCmtB itself guards an out-of-range
  // pair index by returning NA (visible poison, never an out-of-bounds
  // write). Reset to all-zero each subject (Alast starts at 0, so
  // d(Alast_0)/d(eta) = 0), mirroring how linCmtSave is reset above.
  // Opt-in: unused unless a model actually requests which1=-5/-6/-7.
#define RX_LINCMT_CARRY_MAXPAIRS 8
  double linCmtCarryT[4*RX_LINCMT_CARRY_MAXPAIRS];
  // Time of this subject's previous which1=-5 carry advance.  calc_lhs fires
  // exactly once per event row in solve order (dose rows included, output
  // filtering notwithstanding), but in the post-solve lhs pass ind->tprior is
  // stale (frozen at the solve's final interval), so the -5 advance derives
  // its interval dt from its OWN previous invocation time instead.  NAN =
  // no prior advance (first row -> dt 0, harmless: the carry state is 0).
  double linCmtCarryTlast;
  // Runtime per-subject fast path for the which1=-5 carry advance: while this
  // subject's theta vector (p1..ka as passed to -5) has been IDENTICAL on
  // every row of the CURRENT solve pass, the carried recurrence telescopes to
  // G*J (the production constant-theta Jacobian is exact there), so the
  // advance's m forward-mode transition-matrix passes can be skipped and the
  // carry/tracker columns left un-multiplied -- the emitted -7 adds then
  // accumulate G*(J_i - J_{i-1}), which sums to exactly G*J_n.  Within-pass
  // only by construction: both fields reset at iniSubject(), which fires
  // before every lhs walk, so etas changing BETWEEN inner iterations never
  // enter the comparison.  0 = no row seen this pass, 1 = constant so far,
  // 2 = variation seen (slow path for the rest of the pass).
  double linCmtCarryPrevTheta[7];
  int linCmtCarryVarying;
  // Event ("jump") sensitivities: deferred moving-boundary jump for non-dosed
  // compartments.  At a modeled-lag dose the sensitivity of a compartment that
  // does NOT receive the bolus has a genuine jump discontinuity at the arrival
  // time tau; its reported value at an output coincident with tau must be the
  // pre-jump left limit (matching the reported left-continuous state), while
  // t > tau must see the post-jump value.  handle_evid accumulates the non-dosed
  // jump here instead of into yp; preSolve flushes it into yp at the first step
  // that advances past tau.  Per-thread slice of _globals.gEsPendingJump.
  double *esPendingJump;   /* per-thread neq buffer, or NULL when unused */
  double  esPendingTau;    /* event time the pending jump belongs to */
  int     esHasPending;    /* 1 while a deferred jump is waiting to be flushed */
  rx_fn_pointers *fns;
  rx_solving_options *op;
  rx_solve *rx;
  // AutoSwitch: dop853's stiffness-probe state, carried across the per-interval
  // dop853() calls of one subject solve (dop853_stiff_t, src/dop853.h).  The
  // probe is per interval, so without this the accepted-step count and the
  // verdict counters restart every few steps and the detector can never reach
  // its limit.  Reset per subject solve in iniSubject().
  //
  // APPENDED AT THE END ON PURPOSE: everything above keeps its offset, so a
  // downstream object file built against an older copy of this header still
  // finds every field it knows about where it expects it.
  double   autoStiffHlamb;  /* h*rho(J) estimate carried between intervals */
  long int autoStiffIasti;  /* consecutive stiff verdicts */
  long int autoStiffNonsti; /* clean verdicts since the last stiff one */
  long int autoStiffNaccpt; /* accepted steps across the subject's intervals */
  int      autoBackoff;     /* re-probe wait multiplier while on the secondary */
};

typedef struct rx_solve_s {
  rx_solving_options_ind *subjects;
  rx_solving_options *op;
  uint32_t nsub;
  uint32_t nsim;
  int neta;
  int neps;
  int nIndSim;
  int simflg;
  uint32_t nall;
  int nevid9;
  int nobs; // isObs() observations
  int nobs2; // evid=0 observations
  int64_t nr; // int64_t because nobs*nsim can exceed INT_MAX in VPC with many subjects/timepoints/sims
  int add_cov;
  int matrix;
  int needSort;
  int nMtime;
  double stateTrimU;
  double stateTrimL;
  int nCov0;
  int *cov0;
  int nKeepF;
  int istateReset;
  int cens;
  int limit;
  int safeZero;
  int safeLog;
  int safePow;
  int sumType;
  int prodType;
  vLines factors;
  vLines factorNames;
  int factorNs[500];
  int hasFactors;
  int maxAllTimes;
  int *ordId;
  double *ypNA;
  bool sample;
  int *par_sample;
  double maxShift;
  int maxwhile;
  int whileexit;
  int *svar;
  int *ovar;
  int hasEvid2;
  int useStdPow;
  bool ss2cancelAllPending;
  int npars;
  int ndiff;
  int sensType;
  double sensH;
  // Transition-matrix propagation of the sequential row Jacobian: 1 = auto
  // (engage where the row gap repeats, see linCmtSeqTailJac), 0 = off (the
  // tail's own operation order).  Both evaluate the same exact closed form.
  int linCmtSensPhi;
  // Whether the model reads raw amount-Jacobian rows
  // (linCmtB(which1 >= 0, which2 >= 0)); parse-time metadata -- the
  // sequential evaluator writes the full Jacobian block either way.
  int linCmtBraw;
  int linB;

  // flag to determine if the linear compartment model has first order
  // absorption as in oral models
  int linCmtOral0;

  // number of linear compartments in model
  int linCmtNcmt;

  double linCmtGillFtol;
  int linCmtGillK;
  double linCmtGillStep;
  double linCmtGillRtol;

  double linCmtShiErr;
  int linCmtShiMax;
  double *linCmtScale;

  int linCmtHcmt; // linear compartments used for H optimization
  int linCmtHmeanI; // Type of sum for each individual time point
  int linCmtHmeanO; // Type of sum for overall H optimization

  double linCmtSuspect; // What value is close enough to zero to request more der accuracy.
  int linCmtForwardMax; // Maximum number of forward steps to take with forward differences

  // Add mixture number flag
  int mixnum;
  int input_mixnum;

  // Maximum number of events that evid_() may push per individual per solve.
  // 0 = unlimited.  Exceeding the limit aborts the individual with NA output.
  int maxExtra;
  // Set to 1 (atomically) when any individual exceeds maxExtra; checked after
  // the solve completes to emit an error.
  int extraPushAbort;
  int *splitBolus;
  int splitBolusN;
  rx_fn_pointers fns;
} rx_solve;

typedef void (*rxode2_assignFuns2_t)(rx_solve, rx_solving_options, t_F, t_LAG, t_RATE, t_DUR,t_calc_mtime, t_ME, t_IndF, t_getTime, t_locateTimeIndex, t_handle_evidL,t_getDur);

static inline void sNull(sbuf *sbb) {
  sbb->s = NULL;
  sbb->sN=0;
  sbb->o=0;
}

static inline void lineNull(vLines *sbb) {
  sbb->s = NULL;
  sbb->lProp = NULL;
  sbb->lType = NULL;
  sbb->line = NULL;
  sbb->os = NULL;
  sbb->sN = 0;
  sbb->nL = 0;
  sbb->n  = 0;
  sbb->o  = 0;
}
