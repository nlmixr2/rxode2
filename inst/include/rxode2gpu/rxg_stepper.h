#ifndef RXG_STEPPER_H
#define RXG_STEPPER_H
// Per-trajectory adaptive integrators for device model code: one work-item
// integrates one subject over [t0, t1] with fixed-size private storage
// (after Utkarsh et al. 2024, arXiv:2304.06835).  Included after the model's
// device code, so RXG_NEQ/RXG_NEQ1/RXG_HAS_JAC and rxg_dydt()/rxg_jac() are
// defined.  Compiles as OpenCL C 1.2 and as host C.
//
//   RXG_RKT54   rkt54, Tsitouras 5(4) FSAL (tableau of src/ode_rkt54.cpp)
//   RXG_VERN76  vern76, Verner's efficient 7(6) (tableau of src/ode_impl.cpp)
//   RXG_ROS54P  ros54p, Steinebach's 8-stage L-stable Rosenbrock 5(4)
//               (Rodas5P, BIT 63:27, 2023) with the model's analytic
//               Jacobian (df()/dy() lines; without them it is unavailable,
//               as for every Rosenbrock method in rxode2)
//
// Define RXG_METHOD to one of these to build only that method (a kernel).
// (the method codes RXG_RKT54 ... are defined in rxg_model.h)


#ifndef RXG_SOLVE_ROS
#if defined(RXG_METHOD) && (RXG_METHOD == RXG_RKT54 || RXG_METHOD == RXG_VERN76)
#define RXG_SOLVE_ROS 0
#else
#define RXG_SOLVE_ROS 1
#endif
#endif

// rxg_integrate() return codes
#define RXG_SUCCESS 2
#define RXG_ERR_MXSTEP (-1)
#define RXG_ERR_HMIN (-2)
#define RXG_ERR_METHOD (-3)
#define RXG_ERR_STIFF (-4) // stiffness detected (stiffness probe only)
#define RXG_ERR_HIST (-5)  // the delay() history is full

// counters (cnt[])
#define RXG_CNT_STEP 0
#define RXG_CNT_ACC 1
#define RXG_CNT_REJ 2
#define RXG_CNT_FEV 3
#define RXG_CNT_JAC 4
#define RXG_CNT_LU 5
#define RXG_NCNT 6

// stage vectors kept: vern76 has the most, 16 with its interpolant
#if RXG_HAS_DELAY && RXG_HDEG >= 7
#define RXG_NK 16
#else
#define RXG_NK 10
#endif

#ifndef RXG_MIN
#ifdef RXG_FLOAT
#define RXG_MIN FLT_MIN
#else
#define RXG_MIN DBL_MIN
#endif
#endif

typedef struct rxg_opt {
  double atol[RXG_NEQ1]; // absolute tolerance per state
  double rtol[RXG_NEQ1]; // relative tolerance per state
  double hmin;           // smallest step allowed (0: none)
  double hmax;           // largest step (0: the interval)
  int mxstep;            // most steps (accepted and rejected) per interval
  // AutoSwitch composites, as rxControl(autoSwitch*=)
  double nonstifftol;    // stiffness ratio called stiff
  double stifftol;       // the same once the subject has been persistently stiff
  int maxStiff;          // stiff intervals before staying on the secondary
  int maxNonstiff;       // intervals on the secondary before probing again
  int switchMax;         // least intervals between switches
} rxg_opt;

// Stiffness probe of an explicit method (Hairer & Wanner IV.2): h times the
// dominant eigenvalue estimated from two stages at the same time, against
// the method's stability size.  15 verdicts are a stiff report and 6
// non-stiff steps clear the count, as dop853 does.
typedef struct rxg_stiff {
  double tol;    // ratio (autoSwitchNonstifftol or autoSwitchStifftol)
  int iasti;     // stiff verdicts
  int nonsti;    // non-stiff steps since the last verdict
  double tStop;  // time reached when stiffness was reported
} rxg_stiff;

// AutoSwitch state kept per subject across intervals (the host keeps the
// same in rx_solving_options_ind: autoMethod, autoCount, ...)
typedef struct rxg_auto {
  int mode;      // 0 probe the primary, 1 stay on the stiff secondary
  int count;     // consecutive intervals the primary failed
  int since;     // intervals on the secondary since the last switch
  int backoff;   // probe back-off multiplier
  int switched;  // has switched at least once
  double h0, h1; // step sizes of the primary and the secondary
  rxg_stiff st;
} rxg_auto;

// Mixed error weight for state i
RXG_FN double _rxg_sc(const rxg_opt *o, int i, double a, double b) {
  double fa = fabs(a), fb = fabs(b);
  return o->atol[i] + o->rtol[i]*(fa > fb ? fa : fb);
}

// Root mean square of e[i]/sc(y0[i], y1[i]); a non-finite candidate or
// error gives Inf
RXG_FN double _rxg_errNorm(const rxg_opt *o, const double *e,
                           const double *y0, const double *y1) {
  double s = 0.0;
  for (int i = 0; i < RXG_NEQ; i++) {
    if (!isfinite(y1[i])) return INFINITY;
    double r = e[i]/_rxg_sc(o, i, y0[i], y1[i]);
    s += r*r;
  }
  s = sqrt(s/(double)(RXG_NEQ > 0 ? RXG_NEQ : 1));
  return isfinite(s) ? s : INFINITY;
}

RXG_FN void _rxg_f(double t, double *y, double *dy, const rxg_ctx *c, int *cnt) {
  rxg_dydt(t, y, dy, c);
  cnt[RXG_CNT_FEV]++;
}

// Hairer & Wanner's starting step (Solving ODEs I, II.4); f0 = f(t, y)
RXG_FN double _rxg_hinit(double t, double dir, double hmax, int order,
                         double *y, const double *f0, double *ytmp,
                         double *f1, const rxg_ctx *c, const rxg_opt *o,
                         int *cnt) {
  double d0 = 0.0, d1 = 0.0;
  for (int i = 0; i < RXG_NEQ; i++) {
    double sc = _rxg_sc(o, i, y[i], y[i]);
    d0 += (y[i]/sc)*(y[i]/sc);
    d1 += (f0[i]/sc)*(f0[i]/sc);
  }
  d0 = sqrt(d0/RXG_NEQ);
  d1 = sqrt(d1/RXG_NEQ);
  double h0 = (d0 < 1e-5 || d1 < 1e-5) ? 1e-6 : 0.01*d0/d1;
  h0 = fmin(h0, hmax);
  for (int i = 0; i < RXG_NEQ; i++) ytmp[i] = y[i] + dir*h0*f0[i];
  _rxg_f(t + dir*h0, ytmp, f1, c, cnt);
  double d2 = 0.0;
  for (int i = 0; i < RXG_NEQ; i++) {
    double r = (f1[i] - f0[i])/_rxg_sc(o, i, y[i], y[i]);
    d2 += r*r;
  }
  d2 = sqrt(d2/RXG_NEQ)/h0;
  double dm = fmax(d1, d2);
  double h1 = (dm <= 1e-15) ? fmax(1e-6, h0*1e-3) :
    pow(0.01/dm, 1.0/(double)(order + 1));
  double h = fmin(100.0*h0, h1);
  h = fmin(h, hmax);
  return isfinite(h) && h > 0.0 ? h : fmin(1e-6, hmax);
}

//////////////////////////////////////////////////////////////////////////////
// Explicit Runge-Kutta pairs
//////////////////////////////////////////////////////////////////////////////

// Loops over states are unrolled for small systems so the vectors stay in
// registers (every index a constant)
#if RXG_NEQ <= 12
#if defined(__OPENCL_VERSION__) || defined(__clang__)
#define RXG_UNROLL _Pragma("unroll")
#elif defined(__GNUC__)
#define RXG_UNROLL _Pragma("GCC unroll 16")
#else
#define RXG_UNROLL
#endif
#else
#define RXG_UNROLL
#endif

// The explicit steps inline with their literal tableaux so the stage loops
// have constant trip counts (and unroll)
#if defined(__GNUC__) || defined(__clang__) || defined(__OPENCL_VERSION__)
#define RXG_FN_INLINE static inline __attribute__((always_inline))
#else
#define RXG_FN_INLINE RXG_FN
#endif

// One step of an explicit pair with ns stages, A packed by rows (row i has i
// entries), b the solution weights, bt = b - bhat.  With fsal the stage after
// the last is f(t + h, y1) (weight 0 in b), and k[0] holds f(t, y) on entry.
// The states of stages sa and sb (sb == ns: the solution) go to ya and yb
// for the stiffness probe.
RXG_FN_INLINE void _rxg_erkStep(int ns, int fsal, const double *A, const double *cc,
                         const double *b, const double *bt, double t,
                         double h, double *y, double *y1, double *e,
                         double *k, int sa, int sb, double *ya, double *yb,
                         const rxg_ctx *c, int *cnt) {
  int ia = 0;
  // (inlined with a literal tableau: unrolled, the coefficients fold)
  RXG_UNROLL
  for (int s = 1; s < ns; s++) {
    RXG_UNROLL
    for (int i = 0; i < RXG_NEQ; i++) {
      double a = 0.0;
      RXG_UNROLL
      for (int j = 0; j < s; j++) a += A[ia + j]*k[j*RXG_NEQ1 + i];
      y1[i] = y[i] + h*a;
      if (s == sa) ya[i] = y1[i];
      if (s == sb) yb[i] = y1[i];
    }
    ia += s;
    _rxg_f(t + cc[s]*h, y1, k + s*RXG_NEQ1, c, cnt);
  }
  RXG_UNROLL
  for (int i = 0; i < RXG_NEQ; i++) {
    double a = 0.0;
    RXG_UNROLL
    for (int j = 0; j < ns; j++) a += b[j]*k[j*RXG_NEQ1 + i];
    y1[i] = y[i] + h*a;
    if (sb == ns) yb[i] = y1[i];
  }
  int nt = ns;
  if (fsal) {
    _rxg_f(t + h, y1, k + ns*RXG_NEQ1, c, cnt);
    nt++;
  }
  RXG_UNROLL
  for (int i = 0; i < RXG_NEQ; i++) {
    double a = 0.0;
    RXG_UNROLL
    for (int j = 0; j < nt; j++) a += bt[j]*k[j*RXG_NEQ1 + i];
    e[i] = h*a;
  }
}

// stiffness probe: stage 6 and the FSAL stage, both at t + h; stability
// size 3.5068
RXG_FN_INLINE void _rxg_rkt54Step(double t, double h, double *y, double *y1,
                           double *e, double *k, double *ya, double *yb,
                           const rxg_ctx *c, int *cnt) {
  const double A[15] = {
    0.161,
    -0.0084806554923569921,
    0.3354806554923569885444268742502307746751211773934303915373692342452941929761641411569,
    2.8971530571054935,
    -6.359448489975074843148159912383825625952700647415626703305928850207288721235210244366,
    4.362295432869581411017727318190886861027813359713760212991062156752264926097707165077,
    5.3258648284392569,
    -11.74888356406282787774717033978577296188744178259862899288666928009020615663593781589,
    7.495539342889836208304604784564358155658679161518186721010132816213648793440552049753,
    -0.09249506636175524925650207933207191611349983406029535244034750452930469056411389539635,
    5.8614554429464203,
    -12.92096931784710929170611868178335939541780751955743459166312250439928519268343184452,
    8.159367898576158643180400794539253485181918321135053305748355423955009222648673734986,
    -0.07158497328140099722453054252582973869127213147363544882721139659546372402303777878835,
    -0.02826905039406838290900305721271224146717633626879770007617876201276764571291579142206
  };
  const double cc[6] = {
    0.0, 0.161, 0.327, 0.9,
    0.9800255409045096857298102862870245954942137979563024768854764293221195950761080302604,
    1.0
  };
  const double b[6] = {
    0.09646076681806522951816731316512876333711995238157997181903319145764851595234062815396,
    0.01,
    0.4798896504144995747752495322905965199130404621990332488332634944254542060153074523509,
    1.379008574103741893192274821856872770756462643091360525934940067397245698027561293331,
    -3.290069515436080679901047585711363850115683290894936158531296799594813811049925401677,
    2.324710524099773982415355918398765796109060233222962411944060046314465391054716027841
  };
  // b - bhat
  const double bt[7] = {
    0.0017800110522257773, 0.00081643445965674626, -0.0078808780102619935,
    0.14471100717326291, -0.58235716545255523, 0.45808210592918686,
    -0.015151515151515152
  };
  _rxg_erkStep(6, 1, A, cc, b, bt, t, h, y, y1, e, k, 5, 6, ya, yb, c, cnt);
}

// stiffness probe: stages 9 and 10, both at t + h; stability size 4.64
RXG_FN_INLINE void _rxg_vern76Step(double t, double h, double *y, double *y1,
                            double *e, double *k, double *ya, double *yb,
                            const rxg_ctx *c, int *cnt) {
  const double A[45] = {
    0.005,
    -1.076790123456790123456790123456790123457, 1.185679012345679012345679012345679012346,
    0.04083333333333333333333333333333333333333, 0.0, 0.1225,
    0.6389139236255726780508121615993336109954, 0.0, -2.455672638223656809662640566430653894211,
    2.272258714598084131611828404831320283215,
    -2.661577375018757131119259297861818119279, 0.0, 10.80451388645613769565396655365532838482,
    -8.353914657396199411968048547819291691541, 0.8204875949566569791420417341743839209619,
    6.067741434696770992718360183877276714679, 0.0, -24.71127363591108579734203485290746001803,
    20.42751793078889394045773111748346612697, -1.906157978816647150624096784352757010879,
    1.006172249242068014790040335899474187268,
    12.05467007625320299509109452892778311648, 0.0, -49.75478495046898932807257615331444758322,
    41.14288863860467663259698416710157354209, -4.461760149974004185641911603484815375051,
    2.042334822239174959821717077708608543738, -0.09834843665406107379530801693870224403537,
    10.13814652288180787641845141981689030769, 0.0, -42.64113603171750214622846006736635730625,
    35.76384003992257007135021178023160054034, -4.348022840392907653340370296908245943710,
    2.009862268377035895441943593011827554771, 0.3487490460338272405953822853053145879140,
    -0.2714390051048312842371587140910297407572,
    -45.03007203429867712435322405073769635151, 0.0, 187.3272437654588840752418206154201997384,
    -154.0288236935018690596728621034510402582, 18.56465306347536233859492332958439136765,
    -7.141809679295078854925420496823551192821, 1.308808578161378625114762706007696696508,
    0.0, 0.0
  };
  const double cc[10] = {
    0.0, 0.005, 0.1088888888888888888888888888888888888889,
    0.1633333333333333333333333333333333333333, 0.4555,
    0.6095094489978381317087004421486024949638, 0.884, 0.925, 1.0, 1.0
  };
  const double b[10] = {
    0.04715561848627222170431765108838175679569, 0.0, 0.0,
    0.2575056429843415189596436101037687580986,
    0.2621665397741262047713863095764527711129,
    0.1521609265673855740323133199165117535523,
    0.4939969170032484246907175893227876844296,
    -0.2943031171403250441557244744092703429139,
    0.08131747232495109999734599440136761892478, 0.0
  };
  const double bt[10] = {
    0.04715561848627222170431765108838175679569 - 0.04460860660634117628731817597479197781432,
    0.0, 0.0,
    0.2575056429843415189596436101037687580986 - 0.2671640378571372680509102260943837899738,
    0.2621665397741262047713863095764527711129 - 0.2201018300177293019979715776650753096323,
    0.1521609265673855740323133199165117535523 - 0.2188431703143156830983120833512893824578,
    0.4939969170032484246907175893227876844296 - 0.2289871705411202883378173889763552365362,
    -0.2943031171403250441557244744092703429139,
    0.08131747232495109999734599440136761892478,
    -0.02029518466335628222767054793810430358554
  };
  _rxg_erkStep(10, 0, A, cc, b, bt, t, h, y, y1, e, k, 8, 9, ya, yb, c, cnt);
}

//////////////////////////////////////////////////////////////////////////////
// ros54p
//////////////////////////////////////////////////////////////////////////////

#if RXG_SOLVE_ROS && RXG_HAS_JAC
// J[i*RXG_NEQ1 + j] = df_i/dy_j at (t, y), from the model's df()/dy() lines
RXG_FN void _rxg_jacobian(double t, double *y, double *J, const rxg_ctx *c,
                          int *cnt) {
  cnt[RXG_CNT_JAC]++;
  for (int i = 0; i < RXG_NEQ1*RXG_NEQ1; i++) J[i] = 0.0;
  rxg_jac(t, y, J, RXG_NEQ1, c);
}
#endif

// Does f depend on t over this interval?  Only through the model reading
// time (RXG_TIME, from the code generator) or covariates interpolated
// linearly between different values.
RXG_FN int _rxg_nonAutonomous(const rxg_ctx *c) {
#if RXG_TIME
  (void)c;
  return 1;
#else
  if (c->covType != 1) return 0;
  for (int i = 0; i < RXG_NPAR; i++) {
    if (c->p0[i] != c->p1[i]) return 1;
  }
  return 0;
#endif
}

// df/dt at (t, y) by forward differences (models reading time and
// interpolated covariates only; zero otherwise)
RXG_FN void _rxg_dfdt(double t, double dir, double *y, const double *f0,
                      double *dT, double *ftmp, const rxg_ctx *c, int *cnt) {
  double d = dir*sqrt(RXG_EPS)*fmax(fabs(t), 1.0);
  volatile double t1 = t + d;
  d = t1 - t;
  _rxg_f(t1, y, ftmp, c, cnt);
  for (int i = 0; i < RXG_NEQ; i++) dT[i] = (ftmp[i] - f0[i])/d;
}

// LU of W = I/(h*gamma) - J with partial pivoting; returns 0 when singular.
// Rows are swapped by selects rather than indexed by the pivot, and the
// diagonal holds the reciprocal of U's diagonal.
RXG_FN int _rxg_luW(const double *J, double hg, double *W, int *piv) {
  double ihg = 1.0/hg;
  RXG_UNROLL
  for (int i = 0; i < RXG_NEQ; i++) {
    RXG_UNROLL
    for (int j = 0; j < RXG_NEQ; j++) W[i*RXG_NEQ1 + j] = -J[i*RXG_NEQ1 + j];
    W[i*RXG_NEQ1 + i] += ihg;
  }
  int ok = 1;
  RXG_UNROLL
  for (int k = 0; k < RXG_NEQ; k++) {
    int p = k;
    double m = fabs(W[k*RXG_NEQ1 + k]);
    RXG_UNROLL
    for (int i = k + 1; i < RXG_NEQ; i++) {
      double v = fabs(W[i*RXG_NEQ1 + k]);
      int b = v > m;
      m = b ? v : m;
      p = b ? i : p;
    }
    piv[k] = p;
    ok = ok && (m > 0.0) && isfinite(m);
    RXG_UNROLL
    for (int i = k + 1; i < RXG_NEQ; i++) {
      int sw = (p == i);
      RXG_UNROLL
      for (int j = 0; j < RXG_NEQ; j++) {
        double a = W[k*RXG_NEQ1 + j], b = W[i*RXG_NEQ1 + j];
        W[k*RXG_NEQ1 + j] = sw ? b : a;
        W[i*RXG_NEQ1 + j] = sw ? a : b;
      }
    }
    double inv = ok ? 1.0/W[k*RXG_NEQ1 + k] : 0.0;
    ok = ok && isfinite(inv);
    W[k*RXG_NEQ1 + k] = inv;
    RXG_UNROLL
    for (int i = k + 1; i < RXG_NEQ; i++) {
      double l = W[i*RXG_NEQ1 + k]*inv;
      W[i*RXG_NEQ1 + k] = l;
      RXG_UNROLL
      for (int j = k + 1; j < RXG_NEQ; j++) W[i*RXG_NEQ1 + j] -= l*W[k*RXG_NEQ1 + j];
    }
  }
  return ok;
}

// Solve W x = b in place with the factors from _rxg_luW()
// (_rxg_luW() swaps whole rows, so every swap applies to b before the
// forward substitution, as LAPACK's getrs)
RXG_FN void _rxg_luSolve(const double *W, const int *piv, double *x) {
  RXG_UNROLL
  for (int k = 0; k < RXG_NEQ; k++) {
    int p = piv[k];
    RXG_UNROLL
    for (int i = k + 1; i < RXG_NEQ; i++) {
      int sw = (p == i);
      double a = x[k], b = x[i];
      x[k] = sw ? b : a;
      x[i] = sw ? a : b;
    }
  }
  RXG_UNROLL
  for (int k = 0; k < RXG_NEQ; k++) {
    RXG_UNROLL
    for (int i = k + 1; i < RXG_NEQ; i++) x[i] -= W[i*RXG_NEQ1 + k]*x[k];
  }
  RXG_UNROLL
  for (int k = RXG_NEQ - 1; k >= 0; k--) {
    double s = x[k];
    RXG_UNROLL
    for (int j = k + 1; j < RXG_NEQ; j++) s -= W[k*RXG_NEQ1 + j]*x[j];
    x[k] = s*W[k*RXG_NEQ1 + k];
  }
}

// Small systems solve with the explicit inverse of W (from its LU): each
// stage is then a matrix-vector product with no dependent chain
#ifndef RXG_WINV
#define RXG_WINV (RXG_NEQ <= 4)
#endif

#if RXG_WINV
// Wi = W^-1 from the factors of _rxg_luW() (columns solved from unit
// vectors), row major; returns 0 when an entry overflows (the stages then
// solve with the factors)
RXG_FN int _rxg_luInv(const double *W, const int *piv, double *Wi) {
  int ok = 1;
  RXG_UNROLL
  for (int j = 0; j < RXG_NEQ; j++) {
    double x[RXG_NEQ1];
    RXG_UNROLL
    for (int i = 0; i < RXG_NEQ; i++) x[i] = (i == j) ? 1.0 : 0.0;
    _rxg_luSolve(W, piv, x);
    RXG_UNROLL
    for (int i = 0; i < RXG_NEQ; i++) {
      Wi[i*RXG_NEQ1 + j] = x[i];
      ok = ok && isfinite(x[i]);
    }
  }
  return ok;
}

// x = Wi x
RXG_FN void _rxg_invSolve(const double *Wi, double *x) {
  double b[RXG_NEQ1];
  RXG_UNROLL
  for (int i = 0; i < RXG_NEQ; i++) b[i] = x[i];
  RXG_UNROLL
  for (int i = 0; i < RXG_NEQ; i++) {
    double s = 0.0;
    RXG_UNROLL
    for (int j = 0; j < RXG_NEQ; j++) s += Wi[i*RXG_NEQ1 + j]*b[j];
    x[i] = s;
  }
}
#define _rxg_wSolve(x)                              \
  do {                                              \
    if (useInv) _rxg_invSolve(Wi, x);               \
    else _rxg_luSolve(W, piv, x);                   \
  } while (0)
#else
#define _rxg_wSolve(x) _rxg_luSolve(W, piv, x)
#endif

// x[i] = <expression of i> over the states
#define RXG_VEC(i, expr)                        \
  RXG_UNROLL                                    \
  for (int i = 0; i < RXG_NEQ; i++) expr

// ros54p stage k_s solves (I/(h gamma) - J) k_s = f(t + c_s h, u_s) +
// sum_j C_sj/h k_j + h d_s df/dt with u_s = y + sum_j A_sj k_j; the solution
// is u_8 + k_8 and the error estimate k_8.  f0, J, dT are at (t, y); dT is
// zero for an autonomous interval.  Straight-line stages (rows 7 and 8 of A
// are row 6 plus ones: u_7 = u_6 + k_6, u_8 = u_7 + k_7).
RXG_FN int _rxg_ros54pStep(double t, double h, double *y, double *y1,
                            double *e, double *k, const double *f0,
                            const double *J, const double *dT, double *W,
                            int *piv, const rxg_ctx *c, int *cnt) {
  cnt[RXG_CNT_LU]++;
  if (!_rxg_luW(J, h*0.21193756319429014, W, piv)) return 0;
#if RXG_WINV
  double Wi[RXG_NEQ1*RXG_NEQ1];
  int useInv = _rxg_luInv(W, piv, Wi);
#endif
  double ih = 1.0/h;
  double *k1 = k, *k2 = k + RXG_NEQ1, *k3 = k + 2*RXG_NEQ1,
    *k4 = k + 3*RXG_NEQ1, *k5 = k + 4*RXG_NEQ1, *k6 = k + 5*RXG_NEQ1,
    *k7 = k + 6*RXG_NEQ1, *k8 = k + 7*RXG_NEQ1;
  double hd1 = h*0.21193756319429014, hd2 = h*-0.42387512638858027,
    hd3 = h*-0.3384627126235924, hd4 = h*1.8046452872882734,
    hd5 = h*2.325825639765069;
  // stage 1
  RXG_VEC(i, k1[i] = f0[i] + hd1*dT[i]);
  _rxg_wSolve(k1);
  // stage 2
  RXG_VEC(i, y1[i] = y[i] + 3.0*k1[i]);
  _rxg_f(t + 0.6358126895828704*h, y1, k2, c, cnt);
  RXG_VEC(i, k2[i] += ih*(-14.155112264123755*k1[i]) + hd2*dT[i]);
  _rxg_wSolve(k2);
  // stage 3
  RXG_VEC(i, y1[i] = y[i] + 2.849394379747939*k1[i] + 0.45842242204463923*k2[i]);
  _rxg_f(t + 0.4095798393397535*h, y1, k3, c, cnt);
  RXG_VEC(i, k3[i] += ih*(-17.97296035885952*k1[i] - 2.859693295451294*k2[i]) +
          hd3*dT[i]);
  _rxg_wSolve(k3);
  // stage 4
  RXG_VEC(i, y1[i] = y[i] - 6.954028509809101*k1[i] + 2.489845061869568*k2[i] -
          10.358996098473584*k3[i]);
  _rxg_f(t + 0.9769306725060716*h, y1, k4, c, cnt);
  RXG_VEC(i, k4[i] += ih*(147.12150275711716*k1[i] - 1.41221402718213*k2[i] +
                          71.68940251302358*k3[i]) + hd4*dT[i]);
  _rxg_wSolve(k4);
  // stage 5
  RXG_VEC(i, y1[i] = y[i] + 2.8029986275628964*k1[i] + 0.5072464736228206*k2[i] -
          0.3988312541770524*k3[i] - 0.04721187230404641*k4[i]);
  _rxg_f(t + 0.4288403609558664*h, y1, k5, c, cnt);
  RXG_VEC(i, k5[i] += ih*(165.43517024871676*k1[i] - 0.4592823456491126*k2[i] +
                          42.90938336958603*k3[i] - 5.961986721573306*k4[i]) +
          hd5*dT[i]);
  _rxg_wSolve(k5);
  // stage 6
  double t1 = t + h;
  RXG_VEC(i, y1[i] = y[i] - 7.502846399306121*k1[i] + 2.561846144803919*k2[i] -
          11.627539656261098*k3[i] - 0.18268767659942256*k4[i] +
          0.030198172008377946*k5[i]);
  _rxg_f(t1, y1, k6, c, cnt);
  RXG_VEC(i, k6[i] += ih*(24.854864614690072*k1[i] - 3.0009227002832186*k2[i] +
                          47.4931110020768*k3[i] + 5.5814197821558125*k4[i] -
                          0.6610691825249471*k5[i]));
  _rxg_wSolve(k6);
  // stage 7
  RXG_VEC(i, y1[i] += k6[i]);
  _rxg_f(t1, y1, k7, c, cnt);
  RXG_VEC(i, k7[i] += ih*(30.91273214028599*k1[i] - 3.1208243349937974*k2[i] +
                          77.79954646070892*k3[i] + 34.28646028294783*k4[i] -
                          19.097331116725623*k5[i] - 28.087943162872662*k6[i]));
  _rxg_wSolve(k7);
  // stage 8
  RXG_VEC(i, y1[i] += k7[i]);
  _rxg_f(t1, y1, k8, c, cnt);
  RXG_VEC(i, k8[i] += ih*(37.80277123390563*k1[i] - 3.2571969029072276*k2[i] +
                          112.26918849496327*k3[i] + 66.9347231244047*k4[i] -
                          40.06618937091002*k5[i] - 54.66780262877968*k6[i] -
                          9.48861652309627*k7[i]));
  _rxg_wSolve(k8);
  RXG_VEC(i, {
    e[i] = k8[i];
    y1[i] += k8[i];
  });
  return 1;
}

//////////////////////////////////////////////////////////////////////////////
// Driver
//////////////////////////////////////////////////////////////////////////////

// Order of the embedded (error) solution
RXG_FN int _rxg_errOrder(int method) {
  return (method == RXG_VERN76) ? 6 : 4;
}

// Real-axis stability size of an explicit method (0: no stiffness probe)
RXG_FN double _rxg_stabSize(int method) {
  if (method == RXG_RKT54) return 3.5068;
  if (method == RXG_VERN76) return 4.64;
  return 0.0;
}

// Stage pair of the stiffness probe (see the step functions)
RXG_FN int _rxg_probeStage(int method, int b) {
  if (method == RXG_RKT54) return b ? 6 : 5;
  return b ? 9 : 8;
}

// Step controller state
typedef struct rxg_ctl {
  double logErrOld; // log of the last accepted error (PI memory)
  int rejected;  // the previous attempt was rejected
} rxg_ctl;

// New step size from the error of a step of size h; PI on acceptance
// (Hairer & Wanner IV.2), I on rejection
RXG_FN double _rxg_newH(double h, double err, int q, rxg_ctl *ctl) {
  double k = (double)(q + 1);
  double fac;
  if (err <= 1.0) {
    double le = log(fmax(err, 1e-10));
    // 0.9 err^(-0.7/k) errOld^(0.4/k)
    fac = 0.9*exp((-0.7*le + 0.4*ctl->logErrOld)/k);
    fac = fmin(fmax(fac, 0.2), 10.0);
    if (ctl->rejected) fac = fmin(fac, 1.0);
    ctl->logErrOld = fmax(le, -9.210340371976184); // log(1e-4)
    ctl->rejected = 0;
  } else {
    fac = isfinite(err) ? fmax(0.2, 0.9*pow(err, -1.0/k)) : 0.1;
    ctl->rejected = 1;
  }
  return h*fac;
}

// Stiffness verdict after an accepted step of size h; returns 1 on a stiff
// report
RXG_FN int _rxg_stiffCheck(rxg_stiff *st, int method, double h,
                           const double *k, const double *ya,
                           const double *yb) {
  int ka = _rxg_probeStage(method, 0), kb = _rxg_probeStage(method, 1);
  double num = 0.0, den = 0.0;
  for (int i = 0; i < RXG_NEQ; i++) {
    double d = k[kb*RXG_NEQ1 + i] - k[ka*RXG_NEQ1 + i];
    num += d*d;
    d = yb[i] - ya[i];
    den += d*d;
  }
  // a step that moved nothing carries no stiffness signal
  double hl = (den > 0.0) ? fabs(h)*sqrt(num/den) : 0.0;
  // dop853's rules (dop853.c, src/ode_stiff_probe.h): a report and six
  // clean verdicts each clear both counts
  if (hl > st->tol*_rxg_stabSize(method)) {
    st->nonsti = 0;
    if (++st->iasti >= 15) {
      st->iasti = 0;
      st->nonsti = 0;
      return 1;
    }
  } else if (++st->nonsti >= 6) {
    st->iasti = 0;
    st->nonsti = 0;
  }
  return 0;
}

// Working storage of rxg_integrate()
typedef struct rxg_work {
  double k[RXG_NK*RXG_NEQ1];
  double y1[RXG_NEQ1], e[RXG_NEQ1], f0[RXG_NEQ1];
  double ya[RXG_NEQ1], yb[RXG_NEQ1];
#if RXG_SOLVE_ROS
  double J[RXG_NEQ1*RXG_NEQ1], W[RXG_NEQ1*RXG_NEQ1], dT[RXG_NEQ1];
  int piv[RXG_NEQ1];
#endif
  int jacOk;   // J and dT are at the current (t, y)
  int nonAuto; // f depends on t: dT is needed (zero otherwise)
} rxg_work;

// One attempted step of size hs from (t, y) into w->y1 and the error into
// w->e; w->f0 = f(t, y).  Returns 0 when the step cannot be taken (singular
// iteration matrix).
RXG_FN int _rxg_tryStep(int method, double t, double hs, double dir,
                        double *y, rxg_work *w, const rxg_ctx *c, int *cnt) {
  if (method == RXG_ROS54P) {
#if RXG_SOLVE_ROS && RXG_HAS_JAC
    if (!w->jacOk) {
      _rxg_jacobian(t, y, w->J, c, cnt);
      if (w->nonAuto) {
        _rxg_dfdt(t, dir, y, w->f0, w->dT, w->e, c, cnt);
      }
      w->jacOk = 1;
    }
    return _rxg_ros54pStep(t, hs, y, w->y1, w->e, w->k, w->f0, w->J, w->dT,
                           w->W, w->piv, c, cnt);
#else
    return 0;
#endif
  }
  for (int i = 0; i < RXG_NEQ; i++) w->k[i] = w->f0[i];
  if (method == RXG_RKT54) {
    _rxg_rkt54Step(t, hs, y, w->y1, w->e, w->k, w->ya, w->yb, c, cnt);
  } else {
    _rxg_vern76Step(t, hs, y, w->y1, w->e, w->k, w->ya, w->yb, c, cnt);
  }
  return 1;
}

// Take an accepted step: y = y1 and f0 = f(t + h, y1) when the method has it
RXG_FN void _rxg_accept(int method, double *y, rxg_work *w, int *fsal) {
  for (int i = 0; i < RXG_NEQ; i++) y[i] = w->y1[i];
  if (method == RXG_RKT54) {
    for (int i = 0; i < RXG_NEQ; i++) w->f0[i] = w->k[6*RXG_NEQ1 + i];
    *fsal = 1;
  } else {
    *fsal = 0;
  }
  w->jacOk = 0;
}

#if RXG_HAS_DELAY
// vern76's six extra stages for its 7th order interpolant (Verner's
// RKV76.IIa dense output, as OrdinaryDiffEq's Vern7): stage 11 is
// f(t + h, y1); k must hold 16 stage vectors
RXG_FN void _rxg_vern76Extra(double t, double h, const double *y,
                             rxg_work *w, const rxg_ctx *c, int *cnt) {
  const double cc[5] = {0.29, 0.125, 0.25, 0.53, 0.79};
  // rows: stages 12..16; columns: stages 1, 4..9, 11..15
  const double A[60] = {
    0.0523222769159969, 0.22495861826705715, 0.017443709248776376,
    -0.007669379876829393, 0.03435896044073285, -0.0410209723009395,
    0.025651133005205617, -0.0160443457, 0.0, 0.0, 0.0, 0.0,
    0.053053341257859085, 0.12195301011401886, 0.017746840737602496,
    -0.0005928372667681495, 0.008381833970853752, -0.01293369259698612,
    0.009412056815253861, -0.005353253107275676, -0.06666729992455811,
    0.0, 0.0, 0.0,
    0.03887903257436304, -0.0024403203308301317, -0.0013928917214672623,
    -0.00047446291558680135, 0.00039207932413159514, -0.00040554733285128004,
    0.00019897093147716726, -0.00010278198793179169, 0.03385661513870267,
    0.1814893063199928, 0.0, 0.0,
    0.05723681204690013, 0.22265948066761182, 0.12344864200186899,
    0.04006332526666491, -0.05269894848581452, 0.04765971214244523,
    -0.02138895885042213, 0.015193891064036402, 0.12060546716289655,
    -0.022779423016187374, 0.0, 0.0,
    0.051372038802756814, 0.5414214473439406, 0.350399806692184,
    0.14193112269692182, 0.10527377478429423, -0.031081847805874016,
    -0.007401883149519145, -0.006377932504865363, -0.17325495908361865,
    -0.18228156777622026, 0.0, 0.0
  };
  const int col[12] = {0, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14};
  _rxg_f(t + h, w->y1, w->k + 10*RXG_NEQ1, c, cnt);
  for (int s = 0; s < 5; s++) {
    for (int i = 0; i < RXG_NEQ; i++) {
      double a = 0.0;
      for (int j = 0; j < 8 + s; j++) a += A[s*12 + j]*w->k[col[j]*RXG_NEQ1 + i];
      w->e[i] = y[i] + h*a;
    }
    _rxg_f(t + cc[s]*h, w->e, w->k + (11 + s)*RXG_NEQ1, c, cnt);
  }
}

// Interpolant coefficients of state i over an accepted step of size hs
// from y to w->y1, a[j] for s^j (j = 0..RXG_HDEG):
//   rkt54   Tsitouras' free 4th order interpolant
//   ros54p  Rodas5P's dense output (Steinebach 2023)
//   vern76  Verner's 7th order interpolant (after _rxg_vern76Extra())
RXG_FN void _rxg_dense(int method, double hs, int i, const double *y,
                       const rxg_work *w, double *a) {
  // Tsitouras: b_s(theta) = sum_j R[s][j] theta^(j+1), stages 1..7
  const double R[28] = {
    1.0, -2.763706197274826, 2.9132554618219126, -1.0530884977290216,
    0.0, 0.13169999999999998, -0.2234, 0.1017,
    0.0, 3.9302962368947516, -5.941033872131505, 2.490627285651253,
    0.0, -12.411077166933676, 30.33818863028232, -16.548102889244902,
    0.0, 37.50931341651104, -88.1789048947664, 47.37952196281928,
    0.0, -27.896526289197286, 65.09189467479366, -34.87065786149661,
    0.0, 1.5, -4.0, 2.5
  };
  // Rodas5P: K_j = sum_s H[j][s] k_s
  const double H[24] = {
    25.948786856663858, -2.5579724845846235, 10.433815404888879, -2.3679251022685204,
    0.524948541321073, 1.1241088310450404, 0.4272876194431874, -0.17202221070155493,
    -9.91568850695171, -0.9689944594115154, 3.0438037242978453, -24.495224566215796,
    20.176138334709044, 15.98066361424651, -6.789040303419874, -6.710236069923372,
    11.419903575922262, 2.8879645146136994, 72.92137995996029, 80.12511834622643,
    -52.072871366152654, -59.78993625266729, -0.15582684282751913, 4.883087185713722
  };
  for (int j = 0; j <= RXG_HDEG; j++) a[j] = 0.0;
  a[0] = y[i];
  if (method == RXG_RKT54) {
    for (int j = 0; j < 4; j++) {
      double v = 0.0;
      for (int s = 0; s < 7; s++) v += R[s*4 + j]*w->k[s*RXG_NEQ1 + i];
      a[j + 1] = hs*v;
    }
  } else if (method == RXG_ROS54P) {
    double K[3];
    for (int j = 0; j < 3; j++) {
      double v = 0.0;
      for (int s = 0; s < 8; s++) v += H[j*8 + s]*w->k[s*RXG_NEQ1 + i];
      K[j] = v;
    }
    a[1] = w->y1[i] - y[i] + K[0];
    a[2] = K[1] - K[0];
    a[3] = K[2] - K[1];
    a[4] = -K[2];
  } else {
#if RXG_HDEG >= 7
    // Verner: b_s(theta) = sum_j V[s][j] theta^(j+1), stages 1, 4..9, 11..16
    const double V[91] = {
      1.0, -8.413387198332767, 33.675508884490895, -70.80159089484886,
      80.64695108301298, -47.19413969837522, 11.133813442539243,
      0.0, 8.754921980674396, -88.4596828699771, 346.9017638429916,
      -629.2580030059837, 529.6773755604193, -167.35886986514018,
      0.0, 8.913387586637922, -90.06081846893218, 353.1807459217058,
      -640.6476819744374, 539.2646279047156, -170.38809442991547,
      0.0, 5.1733120298478, -52.271115900055385, 204.9853867374073,
      -371.8306118563603, 312.9880934374529, -98.89290352172495,
      0.0, 16.79537744079696, -169.70040000059728, 665.4937727009246,
      -1207.1638892336007, 1016.1291515818546, -321.06001557237494,
      0.0, -10.005997536098665, 101.1005433052275, -396.47391512378437,
      719.1787707014183, -605.3681033918824, 191.27439892797935,
      0.0, 2.764708833638599, -27.934602637390462, 109.54779186137893,
      -198.7128113064482, 167.26633571640318, -52.85010499525706,
      0.0, -2.1696320280163506, 22.016696037569876, -86.90152427798948,
      159.22388973861476, -135.9618306534588, 43.792401183280006,
      0.0, -4.890070188793804, 22.75407737425176, -30.78034218537731,
      -2.797194317207249, 31.369456637508403, -15.655927320381801,
      0.0, 10.862170929551967, -50.542971417827104, 68.37148040407511,
      6.213326521632409, -69.68006323194157, 34.776056794509195,
      0.0, -11.37286691922923, 130.79058078246717, -488.65113677785604,
      832.2148793276441, -664.7743368554426, 201.79288044241662,
      0.0, -5.919778732715007, 63.27679965889219, -265.432682088738,
      520.1009254140611, -467.412109533902, 155.3868452824017,
      0.0, -10.492146197961823, 105.35538525188011, -409.43975011988937,
      732.831448907654, -606.3044574733512, 188.0495196316683
    };
    const int st[13] = {0, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15};
    for (int j = 0; j < 7; j++) {
      double v = 0.0;
      for (int s = 0; s < 13; s++) v += V[s*7 + j]*w->k[st[s]*RXG_NEQ1 + i];
      a[j + 1] = hs*v;
    }
#endif
  }
}

// Breakpoints t0 + sum_i k_i lag_i (sum_i k_i <= 6, the order up to which a
// discontinuity still affects the methods), increasing
RXG_FN void _rxg_bpBuild(rxg_hist *h) {
  int n = 0, lo = 0;
  h->bpDirty = 0;
  // level 1 from t0, then each level adds every lag to the previous one
  for (int lev = 0; lev < 6; lev++) {
    int hi = n;
    for (int p = (lev == 0) ? -1 : lo; p < hi; p++) {
      double base = (p < 0) ? h->t0 : h->bp[p];
      for (int k = 0; k < h->nlag; k++) {
        double v = base + h->lag[k];
        int dup = 0;
        for (int q = 0; q < n && !dup; q++) dup = fabs(h->bp[q] - v) <= 1e-12*fabs(v);
        if (!dup && n < RXG_NBP) h->bp[n++] = v;
      }
      if (lev == 0) break;
    }
    lo = hi;
  }
  // insertion sort (n is small)
  for (int i = 1; i < n; i++) {
    double v = h->bp[i];
    int j = i - 1;
    while (j >= 0 && h->bp[j] > v) {
      h->bp[j + 1] = h->bp[j];
      j--;
    }
    h->bp[j + 1] = v;
  }
  h->nbp = n;
}

// Distance from t to the next breakpoint after it (Inf: none)
RXG_FN double _rxg_bpNext(rxg_hist *h, double t) {
  if (h->bpDirty) _rxg_bpBuild(h);
  for (int i = 0; i < h->nbp; i++) {
    if (h->bp[i] > t + 4.0*RXG_EPS*fabs(t) + 4.0*RXG_MIN) return h->bp[i] - t;
  }
  return INFINITY;
}

// Record an accepted step of size hs from (t, y) in the delay() history;
// returns 0 when it is full
RXG_FN int _rxg_histPush(int method, double t, double hs, const double *y,
                         rxg_work *w, const rxg_ctx *c, int *cnt) {
  rxg_hist *hi = c->hist;
  if (hi->n >= hi->cap) return 0;
#if RXG_HDEG >= 7
  if (method == RXG_VERN76) _rxg_vern76Extra(t, hs, y, w, c, cnt);
#endif
  RXG_GLOBAL double *r = hi->rec + hi->n*RXG_HREC;
  double a[RXG_HDEG + 1];
  for (int j = 0; j < RXG_NDLY; j++) {
    _rxg_dense(method, hs, RXG_DSTATE(j), y, w, a);
    for (int d = 0; d <= RXG_HDEG; d++) r[d*RXG_NDLY1 + j] = a[d];
  }
  r[(RXG_HDEG + 1)*RXG_NDLY1] = t;
  r[(RXG_HDEG + 1)*RXG_NDLY1 + 1] = hs;
  hi->n++;
  return 1;
}
#endif

// Integrate y from t0 to t1 (either direction) with an adaptive method.
// *h is the starting step size (<= 0: chosen here) and returns the size to
// try next, for a warm start on the next interval.  cnt[] (RXG_NCNT) is
// incremented.  With st (an explicit method only) stiffness is probed and
// reported as RXG_ERR_STIFF with y at st->tStop.  Returns RXG_SUCCESS or a
// negative RXG_ERR_* code; y then holds the solution at the last accepted
// time.
RXG_FN int rxg_integrate(int method, double t0, double t1, double *y,
                         double *h, const rxg_ctx *c, const rxg_opt *o,
                         rxg_stiff *st, int *cnt) {
  if (method != RXG_RKT54 && method != RXG_VERN76 && method != RXG_ROS54P) {
    return RXG_ERR_METHOD;
  }
#if RXG_NEQ == 0
  return RXG_SUCCESS;
#endif
#if !RXG_HAS_JAC
  // Rosenbrock methods need the model's analytic Jacobian
  if (method == RXG_ROS54P) return RXG_ERR_METHOD;
#endif
  if (t1 == t0) return RXG_SUCCESS;
  rxg_work w;
  w.jacOk = 0;
  w.nonAuto = _rxg_nonAutonomous(c);
#if RXG_SOLVE_ROS
  if (!w.nonAuto) {
    for (int i = 0; i < RXG_NEQ1; i++) w.dT[i] = 0.0;
  }
#endif
  int probe = (st != 0) && (_rxg_stabSize(method) > 0.0);
  double dir = (t1 > t0) ? 1.0 : -1.0;
  double hmax = (o->hmax > 0.0) ? fmin(o->hmax, fabs(t1 - t0)) : fabs(t1 - t0);
  int q = _rxg_errOrder(method);
  rxg_ctl ctl;
  ctl.logErrOld = -9.210340371976184; // log(1e-4)
  ctl.rejected = 0;
  double t = t0;
  _rxg_f(t, y, w.f0, c, cnt);
  int fsal = 1; // w.f0 = f(t, y)
  double ha = fabs(*h);
  if (!(ha > 0.0) || !isfinite(ha)) {
    ha = _rxg_hinit(t, dir, hmax, q + 1, y, w.f0, w.y1, w.e, c, o, cnt);
  }
  int nstep = 0;
  int ret = RXG_SUCCESS;
  while (dir*(t1 - t) > 0.0) {
    double rem = fabs(t1 - t);
    ha = fmin(ha, hmax);
    // hmin is a floor: fail only when a step that small is rejected
    if (o->hmin > 0.0) ha = fmax(ha, fmin(o->hmin, rem));
#if RXG_HAS_DELAY
    // never step past the shortest delay, so delay() reads recorded steps
    ha = fmin(ha, c->hist->minT);
#endif
    if (ha < 16.0*RXG_EPS*fmax(fabs(t), RXG_MIN) && ha < rem) {
      ret = RXG_ERR_HMIN;
      break;
    }
    if (nstep++ >= o->mxstep) {
      ret = RXG_ERR_MXSTEP;
      break;
    }
    int last = (ha >= rem);
    double hh = last ? rem : ha;
    double tEnd = last ? t1 : t + dir*hh;
#if RXG_HAS_DELAY
    if (dir > 0.0) {
      // land on the next breakpoint rather than step across it
      double bp = _rxg_bpNext(c->hist, t);
      if (bp <= hh) {
        hh = bp;
        tEnd = t + bp;
        last = 1;
      }
    }
#endif
    if (!fsal) _rxg_f(t, y, w.f0, c, cnt);
    fsal = 1;
    int ok = _rxg_tryStep(method, t, dir*hh, dir, y, &w, c, cnt);
    cnt[RXG_CNT_STEP]++;
#if RXG_HAS_DELAY
    if (c->hist->minT < hh*(1.0 - 4.0*RXG_EPS)) {
      // a stage met a shorter delay than the step: it may have read past the
      // history, so retry within that delay
      cnt[RXG_CNT_REJ]++;
      ha = c->hist->minT;
      continue;
    }
#endif
    double err = ok ? _rxg_errNorm(o, w.e, y, w.y1) : INFINITY;
    double hn = _rxg_newH(hh, err, q, &ctl);
    if (err > 1.0) {
      cnt[RXG_CNT_REJ]++;
      if (o->hmin > 0.0 && hh <= o->hmin) {
        ret = RXG_ERR_HMIN;
        break;
      }
      ha = hn;
      continue;
    }
    cnt[RXG_CNT_ACC]++;
    int stiff = probe && _rxg_stiffCheck(st, method, dir*hh, w.k, w.ya, w.yb);
#if RXG_HAS_DELAY
    if (!_rxg_histPush(method, t, dir*hh, y, &w, c, cnt)) {
      ret = RXG_ERR_HIST;
      break;
    }
#endif
    t = tEnd;
    _rxg_accept(method, y, &w, &fsal);
#if RXG_HAS_DELAY
#if RXG_HDEG >= 7
    if (method == RXG_VERN76) {
      // stage 11 of the interpolant is the next step's f(t, y)
      for (int i = 0; i < RXG_NEQ; i++) w.f0[i] = w.k[10*RXG_NEQ1 + i];
      fsal = 1;
    }
#endif
#endif
    // a step cut short to land on t1 says little about the next size
    ha = last ? fmax(hn, ha) : hn;
    // reported on any accepted step, the last of an interval included, as
    // dop853 does; the composite then counts the interval as stiff
    if (stiff) {
      st->tStop = t;
      ret = RXG_ERR_STIFF;
      break;
    }
  }
  *h = ha;
  return ret;
}

//////////////////////////////////////////////////////////////////////////////
// AutoSwitch composites ("rkt54+ros54p", "vern76+ros54p")
//////////////////////////////////////////////////////////////////////////////

// Start a subject; stiffFirst is rxControl(autoSwitchStiffFirst=)
RXG_FN void rxg_autoInit(rxg_auto *a, const rxg_opt *o, int stiffFirst) {
  a->mode = stiffFirst ? 1 : 0;
  a->count = 0;
  a->since = 0;
  a->backoff = 0;
  a->switched = 0;
  a->h0 = 0.0;
  a->h1 = 0.0;
  a->st.tol = o->nonstifftol;
  a->st.iasti = 0;
  a->st.nonsti = 0;
  a->st.tStop = 0.0;
}

// Hysteresis after an interval, as rxAutoSwitchCount() in par_solve.cpp
RXG_FN void _rxg_autoCount(rxg_auto *a, const rxg_opt *o, int tried,
                           int failed) {
  if (tried) {
    if (!failed) {
      a->count = 0;
      a->backoff = 0;
      return;
    }
    a->count = (a->count > 0) ? a->count + 1 : 1;
    if (a->count >= o->maxStiff) {
      a->mode = 1;
      a->count = 0;
      a->since = 0;
      a->backoff = (a->backoff > 0) ? ((a->backoff < 64) ? a->backoff*2 : 64) : 1;
    }
    return;
  }
  a->since++;
  int back = (o->maxNonstiff > 0) ? o->maxNonstiff : 3;
  if (o->switchMax > back) back = o->switchMax;
  if (a->backoff > 1) back *= a->backoff;
  if (a->since >= back) {
    a->mode = 0;
    a->since = 0;
  }
}

// One interval of an AutoSwitch composite: probe the explicit primary and,
// when it reports stiffness or fails, finish the interval with ros54p from
// where the probe stopped (from t0 after a failure).  Once persistently
// stiff the probe is skipped until the back-off has passed.
RXG_FN int rxg_integrateAuto(int primary, double t0, double t1, double *y,
                             const rxg_ctx *c, const rxg_opt *o, rxg_auto *a,
                             int *cnt) {
  if (a->mode != 0) {
    int r = rxg_integrate(RXG_ROS54P, t0, t1, y, &(a->h1), c, o, 0, cnt);
    _rxg_autoCount(a, o, 0, 0);
    return r;
  }
  double ys[RXG_NEQ1];
  for (int i = 0; i < RXG_NEQ; i++) ys[i] = y[i];
#if RXG_HAS_DELAY
  int n0 = c->hist->n;
#endif
  // autoSwitchStifftol once the subject has been persistently stiff (the
  // back-off is set), as the host composites do
  a->st.tol = (a->backoff > 0) ? o->stifftol : o->nonstifftol;
  int r = rxg_integrate(primary, t0, t1, y, &(a->h0), c, o, &(a->st), cnt);
  int failed = (r != RXG_SUCCESS);
  if (failed) {
    double tf = t0;
    if (r == RXG_ERR_STIFF) {
      tf = a->st.tStop;
    } else {
      // restart the interval, dropping the history the probe recorded
      for (int i = 0; i < RXG_NEQ; i++) y[i] = ys[i];
#if RXG_HAS_DELAY
      c->hist->n = n0;
#endif
    }
    a->switched = 1;
    r = rxg_integrate(RXG_ROS54P, tf, t1, y, &(a->h1), c, o, 0, cnt);
  }
  _rxg_autoCount(a, o, 1, failed);
  return r;
}

// One interval with any method code, single or composite.  *h is the
// starting step size (<= 0: chosen) and returns the next one; a composite
// keeps one per method and uses *h to start a method that has none yet.
RXG_FN int rxg_solve(int method, double t0, double t1, double *y, double *h,
                     const rxg_ctx *c, const rxg_opt *o, rxg_auto *a,
                     int *cnt) {
  if (method != RXG_RKT54_ROS54P && method != RXG_VERN76_ROS54P) {
    return rxg_integrate(method, t0, t1, y, h, c, o, 0, cnt);
  }
  if (*h > 0.0) {
    if (!(a->h0 > 0.0)) a->h0 = *h;
    if (!(a->h1 > 0.0)) a->h1 = *h;
  }
  int primary = (method == RXG_RKT54_ROS54P) ? RXG_RKT54 : RXG_VERN76;
  int r = rxg_integrateAuto(primary, t0, t1, y, c, o, a, cnt);
  *h = (a->mode != 0) ? a->h1 : a->h0;
  return r;
}

#endif // RXG_STEPPER_H
