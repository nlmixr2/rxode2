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
} rx_solving_options;


typedef struct {
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
} rx_solving_options_ind;

typedef struct {
  rx_solving_options_ind *subjects;
  rx_solving_options *op;
  int nsub;
  int nsim;
  int neta;
  int neps;
  int nIndSim;
  int simflg;
  int nall;
  int nevid9;
  int nobs; // isObs() observations
  int nobs2; // evid=0 observations
  int nr;
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
} rx_solve;


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

typedef double (*t_F)(int _cSub,  int _cmt, double _amt, double t, double *y);
typedef double (*t_LAG)(int _cSub,  int _cmt, double t);
typedef double (*t_RATE)(int _cSub,  int _cmt, double _amt, double t);
typedef double (*t_DUR)(int _cSub,  int _cmt, double _amt, double t);

typedef void (*t_calc_mtime)(int cSub, double *mtime);

typedef void (*t_ME)(int _cSub, double _t, double t, double *_mat, const double *__zzStateVar__);
typedef void (*t_IndF)(int _cSub, double _t, double t, double *_mat);

typedef double (*t_getTime)(int idx, rx_solving_options_ind *ind);
typedef int (*t_locateTimeIndex)(double obs_time,  rx_solving_options_ind *ind);
typedef int (*t_handle_evidL)(int evid, double *yp, double xout, int id, rx_solving_options_ind *ind) ;
typedef double (*t_getDur)(int l, rx_solving_options_ind *ind, int backward, unsigned int *p);
