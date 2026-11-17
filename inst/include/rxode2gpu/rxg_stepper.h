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
//               (Rodas5P, BIT 63:27, 2023); analytic Jacobian when the model
//               has df()/dy() lines, otherwise forward differences
//
// Define RXG_METHOD to one of these to build only that method (a kernel).
#define RXG_RKT54 1
#define RXG_VERN76 2
#define RXG_ROS54P 3

// AutoSwitch composites (rxg_solve())
#define RXG_RKT54_ROS54P 4
#define RXG_VERN76_ROS54P 5

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

// counters (cnt[])
#define RXG_CNT_STEP 0
#define RXG_CNT_ACC 1
#define RXG_CNT_REJ 2
#define RXG_CNT_FEV 3
#define RXG_CNT_JAC 4
#define RXG_CNT_LU 5
#define RXG_NCNT 6

#define RXG_NK 10 // stage vectors kept (vern76 has the most)

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
  double stifftol;       // the same once the subject has switched
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
  return o->atol[i] + o->rtol[i]*fmax(fabs(a), fabs(b));
}

// Root mean square of e[i]/sc(y0[i], y1[i]); non-finite values give Inf
RXG_FN double _rxg_errNorm(const rxg_opt *o, const double *e,
                           const double *y0, const double *y1) {
  double s = 0.0;
  for (int i = 0; i < RXG_NEQ; i++) {
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

// One step of an explicit pair with ns stages, A packed by rows (row i has i
// entries), b the solution weights, bt = b - bhat.  With fsal the stage after
// the last is f(t + h, y1) (weight 0 in b), and k[0] holds f(t, y) on entry.
// The states of stages sa and sb (sb == ns: the solution) go to ya and yb
// for the stiffness probe.
RXG_FN void _rxg_erkStep(int ns, int fsal, const double *A, const double *cc,
                         const double *b, const double *bt, double t,
                         double h, double *y, double *y1, double *e,
                         double *k, int sa, int sb, double *ya, double *yb,
                         const rxg_ctx *c, int *cnt) {
  int ia = 0;
  for (int s = 1; s < ns; s++) {
    for (int i = 0; i < RXG_NEQ; i++) {
      double a = 0.0;
      for (int j = 0; j < s; j++) a += A[ia + j]*k[j*RXG_NEQ1 + i];
      y1[i] = y[i] + h*a;
      if (s == sa) ya[i] = y1[i];
      if (s == sb) yb[i] = y1[i];
    }
    ia += s;
    _rxg_f(t + cc[s]*h, y1, k + s*RXG_NEQ1, c, cnt);
  }
  for (int i = 0; i < RXG_NEQ; i++) {
    double a = 0.0;
    for (int j = 0; j < ns; j++) a += b[j]*k[j*RXG_NEQ1 + i];
    y1[i] = y[i] + h*a;
    if (sb == ns) yb[i] = y1[i];
  }
  int nt = ns;
  if (fsal) {
    _rxg_f(t + h, y1, k + ns*RXG_NEQ1, c, cnt);
    nt++;
  }
  for (int i = 0; i < RXG_NEQ; i++) {
    double a = 0.0;
    for (int j = 0; j < nt; j++) a += bt[j]*k[j*RXG_NEQ1 + i];
    e[i] = h*a;
  }
}

// stiffness probe: stage 6 and the FSAL stage, both at t + h; stability
// size 3.5068
RXG_FN void _rxg_rkt54Step(double t, double h, double *y, double *y1,
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
RXG_FN void _rxg_vern76Step(double t, double h, double *y, double *y1,
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

// J[i*RXG_NEQ1 + j] = df_i/dy_j at (t, y); f0 = f(t, y)
RXG_FN void _rxg_jacobian(double t, double *y, const double *f0, double *J,
                          double *ytmp, double *ftmp, const rxg_ctx *c,
                          int *cnt) {
  cnt[RXG_CNT_JAC]++;
#if RXG_HAS_JAC
  (void)f0; (void)ytmp; (void)ftmp;
  for (int i = 0; i < RXG_NEQ1*RXG_NEQ1; i++) J[i] = 0.0;
  rxg_jac(t, y, J, RXG_NEQ1, c);
#else
  for (int i = 0; i < RXG_NEQ; i++) ytmp[i] = y[i];
  for (int j = 0; j < RXG_NEQ; j++) {
    double d = sqrt(RXG_EPS)*fmax(fabs(y[j]), 1e-5);
    ytmp[j] = y[j] + d;
    d = ytmp[j] - y[j];
    _rxg_f(t, ytmp, ftmp, c, cnt);
    for (int i = 0; i < RXG_NEQ; i++) J[i*RXG_NEQ1 + j] = (ftmp[i] - f0[i])/d;
    ytmp[j] = y[j];
  }
#endif
}

// df/dt at (t, y) by forward differences (covariates and time-varying terms)
RXG_FN void _rxg_dfdt(double t, double dir, double *y, const double *f0,
                      double *dT, double *ftmp, const rxg_ctx *c, int *cnt) {
  double d = dir*sqrt(RXG_EPS)*fmax(fabs(t), 1.0);
  volatile double t1 = t + d;
  d = t1 - t;
  _rxg_f(t1, y, ftmp, c, cnt);
  for (int i = 0; i < RXG_NEQ; i++) dT[i] = (ftmp[i] - f0[i])/d;
}

// LU of W = I/(h*gamma) - J with partial pivoting; returns 0 when singular
RXG_FN int _rxg_luW(const double *J, double hg, double *W, int *piv) {
  for (int i = 0; i < RXG_NEQ; i++) {
    for (int j = 0; j < RXG_NEQ; j++) W[i*RXG_NEQ1 + j] = -J[i*RXG_NEQ1 + j];
    W[i*RXG_NEQ1 + i] += 1.0/hg;
  }
  for (int k = 0; k < RXG_NEQ; k++) {
    int p = k;
    double m = fabs(W[k*RXG_NEQ1 + k]);
    for (int i = k + 1; i < RXG_NEQ; i++) {
      double v = fabs(W[i*RXG_NEQ1 + k]);
      if (v > m) { m = v; p = i; }
    }
    piv[k] = p;
    if (!(m > 0.0) || !isfinite(m)) return 0;
    if (p != k) {
      for (int j = 0; j < RXG_NEQ; j++) {
        double tmp = W[k*RXG_NEQ1 + j];
        W[k*RXG_NEQ1 + j] = W[p*RXG_NEQ1 + j];
        W[p*RXG_NEQ1 + j] = tmp;
      }
    }
    double inv = 1.0/W[k*RXG_NEQ1 + k];
    for (int i = k + 1; i < RXG_NEQ; i++) {
      double l = W[i*RXG_NEQ1 + k]*inv;
      W[i*RXG_NEQ1 + k] = l;
      for (int j = k + 1; j < RXG_NEQ; j++) W[i*RXG_NEQ1 + j] -= l*W[k*RXG_NEQ1 + j];
    }
  }
  return 1;
}

// Solve W x = b in place with the factors from _rxg_luW()
RXG_FN void _rxg_luSolve(const double *W, const int *piv, double *x) {
  for (int k = 0; k < RXG_NEQ; k++) {
    int p = piv[k];
    if (p != k) { double tmp = x[k]; x[k] = x[p]; x[p] = tmp; }
    for (int i = k + 1; i < RXG_NEQ; i++) x[i] -= W[i*RXG_NEQ1 + k]*x[k];
  }
  for (int k = RXG_NEQ - 1; k >= 0; k--) {
    double s = x[k];
    for (int j = k + 1; j < RXG_NEQ; j++) s -= W[k*RXG_NEQ1 + j]*x[j];
    x[k] = s/W[k*RXG_NEQ1 + k];
  }
}

// ros54p stage k_s solves (I/(h gamma) - J) k_s = f(t + c_s h, u_s) +
// sum_j C_sj/h k_j + h d_s df/dt with u_s = y + sum_j A_sj k_j; the solution
// is u_8 + k_8 and the error estimate k_8.  f0, J, dT are at (t, y).
RXG_FN int _rxg_ros54pStep(double t, double h, double *y, double *y1,
                            double *e, double *k, const double *f0,
                            const double *J, const double *dT, double *W,
                            int *piv, const rxg_ctx *c, int *cnt) {
  const double gam = 0.21193756319429014;
  const double A[28] = {
    3.0,
    2.849394379747939, 0.45842242204463923,
    -6.954028509809101, 2.489845061869568, -10.358996098473584,
    2.8029986275628964, 0.5072464736228206, -0.3988312541770524, -0.04721187230404641,
    -7.502846399306121, 2.561846144803919, -11.627539656261098, -0.18268767659942256,
    0.030198172008377946,
    -7.502846399306121, 2.561846144803919, -11.627539656261098, -0.18268767659942256,
    0.030198172008377946, 1.0,
    -7.502846399306121, 2.561846144803919, -11.627539656261098, -0.18268767659942256,
    0.030198172008377946, 1.0, 1.0
  };
  const double C[28] = {
    -14.155112264123755,
    -17.97296035885952, -2.859693295451294,
    147.12150275711716, -1.41221402718213, 71.68940251302358,
    165.43517024871676, -0.4592823456491126, 42.90938336958603, -5.961986721573306,
    24.854864614690072, -3.0009227002832186, 47.4931110020768, 5.5814197821558125,
    -0.6610691825249471,
    30.91273214028599, -3.1208243349937974, 77.79954646070892, 34.28646028294783,
    -19.097331116725623, -28.087943162872662,
    37.80277123390563, -3.2571969029072276, 112.26918849496327, 66.9347231244047,
    -40.06618937091002, -54.66780262877968, -9.48861652309627
  };
  const double cc[8] = {
    0.0, 0.6358126895828704, 0.4095798393397535, 0.9769306725060716,
    0.4288403609558664, 1.0, 1.0, 1.0
  };
  const double d[8] = {
    0.21193756319429014, -0.42387512638858027, -0.3384627126235924,
    1.8046452872882734, 2.325825639765069, 0.0, 0.0, 0.0
  };
  cnt[RXG_CNT_LU]++;
  if (!_rxg_luW(J, h*gam, W, piv)) return 0;
  for (int i = 0; i < RXG_NEQ; i++) k[i] = f0[i] + h*d[0]*dT[i];
  _rxg_luSolve(W, piv, k);
  int ia = 0;
  for (int s = 1; s < 8; s++) {
    double *ks = k + s*RXG_NEQ1;
    for (int i = 0; i < RXG_NEQ; i++) {
      double a = 0.0;
      for (int j = 0; j < s; j++) a += A[ia + j]*k[j*RXG_NEQ1 + i];
      y1[i] = y[i] + a;
    }
    _rxg_f(t + cc[s]*h, y1, ks, c, cnt);
    for (int i = 0; i < RXG_NEQ; i++) {
      double a = 0.0;
      for (int j = 0; j < s; j++) a += C[ia + j]*k[j*RXG_NEQ1 + i];
      ks[i] += a/h + h*d[s]*dT[i];
    }
    ia += s;
    _rxg_luSolve(W, piv, ks);
  }
  // y1 holds u_8
  for (int i = 0; i < RXG_NEQ; i++) {
    e[i] = k[7*RXG_NEQ1 + i];
    y1[i] += e[i];
  }
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
  double errOld; // last accepted error (PI memory)
  int rejected;  // the previous attempt was rejected
} rxg_ctl;

// New step size from the error of a step of size h; PI on acceptance
// (Hairer & Wanner IV.2), I on rejection
RXG_FN double _rxg_newH(double h, double err, int q, rxg_ctl *ctl) {
  double k = (double)(q + 1);
  double fac;
  if (err <= 1.0) {
    double en = fmax(err, 1e-10);
    fac = 0.9*pow(en, -0.7/k)*pow(ctl->errOld, 0.4/k);
    fac = fmin(fmax(fac, 0.2), 10.0);
    if (ctl->rejected) fac = fmin(fac, 1.0);
    ctl->errOld = fmax(err, 1e-4);
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
  if (hl > st->tol*_rxg_stabSize(method)) {
    st->nonsti = 0;
    if (++st->iasti >= 15) {
      st->iasti = 0;
      return 1;
    }
  } else if (++st->nonsti == 6) {
    st->iasti = 0;
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
  int jacOk; // J and dT are at the current (t, y)
} rxg_work;

// One attempted step of size hs from (t, y) into w->y1 and the error into
// w->e; w->f0 = f(t, y).  Returns 0 when the step cannot be taken (singular
// iteration matrix).
RXG_FN int _rxg_tryStep(int method, double t, double hs, double dir,
                        double *y, rxg_work *w, const rxg_ctx *c, int *cnt) {
  if (method == RXG_ROS54P) {
#if RXG_SOLVE_ROS
    if (!w->jacOk) {
      _rxg_jacobian(t, y, w->f0, w->J, w->y1, w->e, c, cnt);
      _rxg_dfdt(t, dir, y, w->f0, w->dT, w->e, c, cnt);
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
  if (t1 == t0) return RXG_SUCCESS;
  rxg_work w;
  w.jacOk = 0;
  int probe = (st != 0) && (_rxg_stabSize(method) > 0.0);
  double dir = (t1 > t0) ? 1.0 : -1.0;
  double hmax = (o->hmax > 0.0) ? fmin(o->hmax, fabs(t1 - t0)) : fabs(t1 - t0);
  int q = _rxg_errOrder(method);
  rxg_ctl ctl;
  ctl.errOld = 1e-4;
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
    if (ha < fmax(o->hmin, 16.0*RXG_EPS*fmax(fabs(t), RXG_MIN)) && ha < rem) {
      ret = RXG_ERR_HMIN;
      break;
    }
    if (nstep++ >= o->mxstep) {
      ret = RXG_ERR_MXSTEP;
      break;
    }
    int last = (ha >= rem);
    double hh = last ? rem : ha;
    if (!fsal) _rxg_f(t, y, w.f0, c, cnt);
    fsal = 1;
    int ok = _rxg_tryStep(method, t, dir*hh, dir, y, &w, c, cnt);
    cnt[RXG_CNT_STEP]++;
    double err = ok ? _rxg_errNorm(o, w.e, y, w.y1) : INFINITY;
    double hn = _rxg_newH(hh, err, q, &ctl);
    if (err > 1.0) {
      cnt[RXG_CNT_REJ]++;
      ha = hn;
      continue;
    }
    cnt[RXG_CNT_ACC]++;
    int stiff = probe && _rxg_stiffCheck(st, method, dir*hh, w.k, w.ya, w.yb);
    t = last ? t1 : t + dir*hh;
    _rxg_accept(method, y, &w, &fsal);
    // a step cut short to land on t1 says little about the next size
    ha = last ? fmax(hn, ha) : hn;
    if (stiff && !last) {
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
  a->st.tol = a->switched ? o->stifftol : o->nonstifftol;
  int r = rxg_integrate(primary, t0, t1, y, &(a->h0), c, o, &(a->st), cnt);
  int failed = (r != RXG_SUCCESS);
  if (failed) {
    double tf = t0;
    if (r == RXG_ERR_STIFF) {
      tf = a->st.tStop;
    } else {
      for (int i = 0; i < RXG_NEQ; i++) y[i] = ys[i];
    }
    a->switched = 1;
    r = rxg_integrate(RXG_ROS54P, tf, t1, y, &(a->h1), c, o, 0, cnt);
  }
  _rxg_autoCount(a, o, 1, failed);
  return r;
}

// One interval with any method code, single or composite; h is the step
// size of a single method
RXG_FN int rxg_solve(int method, double t0, double t1, double *y, double *h,
                     const rxg_ctx *c, const rxg_opt *o, rxg_auto *a,
                     int *cnt) {
  if (method == RXG_RKT54_ROS54P) {
    return rxg_integrateAuto(RXG_RKT54, t0, t1, y, c, o, a, cnt);
  }
  if (method == RXG_VERN76_ROS54P) {
    return rxg_integrateAuto(RXG_VERN76, t0, t1, y, c, o, a, cnt);
  }
  return rxg_integrate(method, t0, t1, y, h, c, o, 0, cnt);
}

#endif // RXG_STEPPER_H
