#ifdef IN_PAR_SOLVE
#undef min
#undef max
#include "odeinter.h"
#include <vector>
#include <cstring>
#include <R_ext/RS.h>      // F77_CALL, FCONE
#include <R_ext/Lapack.h>  // dgetrf / dgetrs (robust LU for the Rosenbrock W)

// ===========================================================================
// rk4s and the fixed-step explicit-RK discrete-adjoint family.
//
// SEPARATE ode methods (rk4 etc. stay byte-identical).  Each is a fixed-step
// explicit Runge-Kutta method selected by a Butcher tableau (rksGetTableau):
// rk4s (206), eulers (239), midpoints (240), heuns (241).  The forward pass
// RECORDS each step's realized dt + its stage states, so the backward
// reverse-mode sweep is the EXACT (table-driven) transpose of the actual
// numerical map -- a machine-precision-consistent gradient, mirroring
// R/adjointDiscrete.R's .rxDiscreteAdjointGrad.
//
// The model is built by R's .rxAdjointExpand: base ODE states, then
// rx__sens_<state>_BY_<param>__ OUTPUT-storage compartments (d/dt=0), plus the
// state Jacobian F_X (rx__adjFX_i_j__) and parameter forcing F_p
// (rx__adjFP_i_p__) exposed as lhs.  op carries the layout (op->adjNbase, adjNp,
// adjFxOff, adjFpOff, adjSensOff), set by rxData.cpp for these method codes.
//
// Forward: step ONLY the nBase base states (the rx__sens_* slots stay 0 during
// the forward pass).  Backward: for each observation time and each base state k
// run an independent reset sweep with terminal covector e_k; the resulting
// mu (length np) = dy_k(t_obs)/dtheta is written into that obs row's
// rx__sens_* solve slots.  rk4s is the GRADIENT method (full-trajectory
// sensitivities); if no gradient is needed use plain rk4.  A scalar objective
// gradient is a downstream REDUCTION of these columns, not a mode of the solver.
// ===========================================================================

// An additive bolus recorded during the forward pass: which 0-based RK4 step
// its (F-scaled) state jump precedes (step = cumulative steps at dose time),
// the base-state compartment, and the RAW amt (bioavailability F is applied to
// the state in handle_evid; the adjoint needs mu += amt*dF/dtheta*lambda[cmt]).
// A recorded forward state-jump event, tagged with the cumulative step index it
// precedes.  type: 0 = additive bolus (amt = raw dose; F-jump quadrature), 3 =
// reset (evid 3, whole state -> inits), 5 = replace (state cmt -> value), 6 =
// multiply (state cmt *= amt).
struct rk4s_dose { size_t step; int cmt; double amt; int type; };

// A modeled-rate infusion window: active over steps [onStep, offStep) delivering
// rate R into compartment cmt (amt total).  Drives the two-term infusion dual --
// the in-window forcing quadrature and the off-boundary (offStep) transversality.
struct rk4s_infus { size_t onStep, offStep; int cmt; double R, amt, durMult; };  // durMult: 1 (rate) or amt (dur)

// Apply the transpose event jumps at backward step n.  On entry lamR = lambda_n+
// (post-RK-transpose, pre-jump); on exit lamR = lambda_n (= (dPhi/dy)^T lambda_n+).
//   additive (dPhi/dy = I): lambda unchanged; mu += amt * dF/dp * lambda[c].
//   reset    (Phi = inits) : dPhi/dy = 0        -> lambda := 0.
//   replace  (state c := v): row c is 0         -> lambda[c] := 0.
//   multiply (state c *= a): diag c is a        -> lambda[c] *= a.
// A constant replace/multiply value contributes nothing to mu; a modeled value
// would add mu += lambda_n+[c] * dvalue/dp (a later addition).
// fx0 = F_X (df/dy) at this step's stage 0 (post-dose state), row-major i*nBase+j,
// or NULL; dlag = d(lag)/dtheta block (c*np+p) or NULL.  When a bolus into cmt c has
// a modeled lag, the dose time t_d+lag(theta) shifts with theta, adding the
// transversality  mu += -amt * dlag[c][p] * (lambda^T F_X[:,c]).
static inline void rk4sApplyEventJumps(size_t n, std::vector<double> &lamR,
    std::vector<double> &muR, const std::vector<rk4s_dose> &doses,
    const std::vector<double> &dFdp, bool haveDf, int nBase, int np,
    const double *fx0, const double *dlag, size_t maxIdx = (size_t)-1) {
  for (size_t di = 0; di < doses.size(); ++di) {
    if (di >= maxIdx) break;             // only doses recorded before this point
    if (doses[di].step != n) continue;
    const rk4s_dose &d = doses[di];
    switch (d.type) {
    case 0:
      if (haveDf) for (int p = 0; p < np; ++p) muR[p] += d.amt * dFdp[d.cmt * np + p] * lamR[d.cmt];
      if (dlag && fx0) {
        double sc = 0.0;                          // lambda^T F_X[:,c]
        for (int i = 0; i < nBase; ++i) sc += lamR[i] * fx0[i * nBase + d.cmt];
        for (int p = 0; p < np; ++p) muR[p] += -d.amt * dlag[d.cmt * np + p] * sc;
      }
      break;
    case 3:
      for (int j = 0; j < nBase; ++j) lamR[j] = 0.0;
      break;
    case 5:
      lamR[d.cmt] = 0.0;
      break;
    case 6:
      lamR[d.cmt] *= d.amt;
      break;
    }
  }
}

// Butcher tableau for an explicit RK method (the discrete-adjoint transpose is
// table-driven, so adding a fixed-step OR adaptive explicit method = adding a
// tableau).  A is s x s row-major, lower-triangular (A[i*s+j], j<i).  For
// adaptive methods bhat is the embedded (error-estimate) weight row and errOrder
// is the lower order of the embedded pair; bhat/errOrder are used ONLY by the
// forward step controller -- the backward transpose uses b (and A/c) alone.
// Sized for s<=36 (the largest libode explicit-RK method, f1412, has 35 stages;
// dop853 has 12).  All step/backward loops are bounded by T.s, so only these arrays
// need the capacity; A/gam are s x s (36*36 = 1296).
struct rksTableau { int s; int adaptive; int errOrder;
                    double c[36]; double b[36]; double bhat[36]; double A[1296];
                    // Rosenbrock (linearly-implicit) extension: rosenbrock=1 uses
                    // A as the stage coefficients alpha_ij, b as the weights m_i,
                    // gam as gamma_ij, and gamma as the diagonal.  Stage i:
                    // (I/(h*gamma) - J) k_i = f(y + sum_{j<i} alpha_ij k_j)
                    //                         + (1/h) sum_{j<i} gam_ij k_j
                    // y_new = y + sum_i m_i k_i.  J = df/dy (frozen at step start).
                    int rosenbrock; double gamma; double gam[1296];
                    // implicitRK=1: fully-implicit RK (Radau IIA).  A is the FULL
                    // s x s coefficient matrix; k_i = f(y + h sum_j A_ij k_j) solved
                    // by Newton; y_new = y + h sum_i b_i k_i.  Exact adjoint needs
                    // only J(u_i) (first derivatives) via the implicit-function thm.
                    int implicitRK; };

// -- LU for the Rosenbrock stage matrix W (n x n, n = nBase, small) via LAPACK
//    dgetrf/dgetrs (robust, the same routines armadillo wraps).  W is stored
//    ROW-major, so LAPACK's column-major view is W^T: hence solving W x = b uses
//    trans 'T' and solving W^T x = b uses trans 'N'.  Factor once, reuse across
//    the many backward solves.  piv holds LAPACK's pivots. --------------------
static inline bool luFactor(double *W, int n, int *piv) {
  int info = 0; F77_CALL(dgetrf)(&n, &n, W, &n, piv, &info); return info == 0;
}
static inline void luSolve(const double *W, int n, const int *piv, double *b) {  // W x = b
  int info = 0, one = 1; char tr = 'T';
  F77_CALL(dgetrs)(&tr, &n, &one, (double*)W, &n, (int*)piv, b, &n, &info FCONE);
}
static inline void luSolveT(const double *W, int n, const int *piv, double *b) { // W^T x = b
  int info = 0, one = 1; char tr = 'N';
  F77_CALL(dgetrs)(&tr, &n, &one, (double*)W, &n, (int*)piv, b, &n, &info FCONE);
}

static void rksTableauRk4(rksTableau &T) {
  T.s = 4;
  T.c[0] = 0; T.c[1] = 0.5; T.c[2] = 0.5; T.c[3] = 1.0;
  T.b[0] = 1.0/6; T.b[1] = 1.0/3; T.b[2] = 1.0/3; T.b[3] = 1.0/6;
  T.A[1*4+0] = 0.5; T.A[2*4+1] = 0.5; T.A[3*4+2] = 1.0;
}

// Dormand-Prince RK5(4), 7 stages (the ode45 / rxode2 "dop5" tableau).
static void rksTableauDop5(rksTableau &T) {
  const int s = 7; T.s = s; T.adaptive = 1; T.errOrder = 4;
  T.c[0]=0; T.c[1]=1.0/5; T.c[2]=3.0/10; T.c[3]=4.0/5; T.c[4]=8.0/9; T.c[5]=1.0; T.c[6]=1.0;
  T.A[1*s+0]=1.0/5;
  T.A[2*s+0]=3.0/40;      T.A[2*s+1]=9.0/40;
  T.A[3*s+0]=44.0/45;     T.A[3*s+1]=-56.0/15;    T.A[3*s+2]=32.0/9;
  T.A[4*s+0]=19372.0/6561;T.A[4*s+1]=-25360.0/2187;T.A[4*s+2]=64448.0/6561; T.A[4*s+3]=-212.0/729;
  T.A[5*s+0]=9017.0/3168; T.A[5*s+1]=-355.0/33;   T.A[5*s+2]=46732.0/5247; T.A[5*s+3]=49.0/176;    T.A[5*s+4]=-5103.0/18656;
  T.A[6*s+0]=35.0/384;    T.A[6*s+1]=0;           T.A[6*s+2]=500.0/1113;   T.A[6*s+3]=125.0/192;   T.A[6*s+4]=-2187.0/6784;  T.A[6*s+5]=11.0/84;
  T.b[0]=35.0/384; T.b[1]=0; T.b[2]=500.0/1113; T.b[3]=125.0/192; T.b[4]=-2187.0/6784; T.b[5]=11.0/84; T.b[6]=0;
  T.bhat[0]=5179.0/57600; T.bhat[1]=0; T.bhat[2]=7571.0/16695; T.bhat[3]=393.0/640;
  T.bhat[4]=-92097.0/339200; T.bhat[5]=187.0/2100; T.bhat[6]=1.0/40;
}

// Dormand-Prince 8(5,3), 12 propagating stages (Hairer's DOP853, from
// src/dop853.c).  b = 8th-order weights; the embedded error estimate is
// er = (8th - 5th order) difference, so setting bhat = b - er makes the
// controller's (yhigh - ylow) equal h*sum er_j k_j (the DOP853 5th-order error).
// Nodes c are the A row sums (consistent explicit RK).
static void rksTableauDop853(rksTableau &T) {
  const int s = 12; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double *A = T.A;
  A[1*s+0]=5.26001519587677318785587544488E-2;
  A[2*s+0]=1.97250569845378994544595329183E-2; A[2*s+1]=5.91751709536136983633785987549E-2;
  A[3*s+0]=2.95875854768068491816892993775E-2; A[3*s+2]=8.87627564304205475450678981324E-2;
  A[4*s+0]=2.41365134159266685502369798665E-1; A[4*s+2]=-8.84549479328286085344864962717E-1; A[4*s+3]=9.24834003261792003115737966543E-1;
  A[5*s+0]=3.7037037037037037037037037037E-2;  A[5*s+3]=1.70828608729473871279604482173E-1; A[5*s+4]=1.25467687566822425016691814123E-1;
  A[6*s+0]=3.7109375E-2;                        A[6*s+3]=1.70252211019544039314978060272E-1; A[6*s+4]=6.02165389804559606850219397283E-2; A[6*s+5]=-1.7578125E-2;
  A[7*s+0]=3.70920001185047927108779319836E-2; A[7*s+3]=1.70383925712239993810214054705E-1; A[7*s+4]=1.07262030446373284651809199168E-1; A[7*s+5]=-1.53194377486244017527936158236E-2; A[7*s+6]=8.27378916381402288758473766002E-3;
  A[8*s+0]=6.24110958716075717114429577812E-1; A[8*s+3]=-3.36089262944694129406857109825E0; A[8*s+4]=-8.68219346841726006818189891453E-1; A[8*s+5]=2.75920996994467083049415600797E1; A[8*s+6]=2.01540675504778934086186788979E1; A[8*s+7]=-4.34898841810699588477366255144E1;
  A[9*s+0]=4.77662536438264365890433908527E-1; A[9*s+3]=-2.48811461997166764192642586468E0; A[9*s+4]=-5.90290826836842996371446475743E-1; A[9*s+5]=2.12300514481811942347288949897E1; A[9*s+6]=1.52792336328824235832596922938E1; A[9*s+7]=-3.32882109689848629194453265587E1; A[9*s+8]=-2.03312017085086261358222928593E-2;
  A[10*s+0]=-9.3714243008598732571704021658E-1;A[10*s+3]=5.18637242884406370830023853209E0; A[10*s+4]=1.09143734899672957818500254654E0; A[10*s+5]=-8.14978701074692612513997267357E0; A[10*s+6]=-1.85200656599969598641566180701E1; A[10*s+7]=2.27394870993505042818970056734E1; A[10*s+8]=2.49360555267965238987089396762E0; A[10*s+9]=-3.0467644718982195003823669022E0;
  A[11*s+0]=2.27331014751653820792359768449E0; A[11*s+3]=-1.05344954667372501984066689879E1;A[11*s+4]=-2.00087205822486249909675718444E0;A[11*s+5]=-1.79589318631187989172765950534E1;A[11*s+6]=2.79488845294199600508499808837E1; A[11*s+7]=-2.85899827713502369474065508674E0;A[11*s+8]=-8.87285693353062954433549289258E0;A[11*s+9]=1.23605671757943030647266201528E1; A[11*s+10]=6.43392746015763530355970484046E-1;
  for (int i = 0; i < s; ++i) { double cs = 0; for (int j = 0; j < i; ++j) cs += A[i*s+j]; T.c[i] = cs; }
  double b[12] = {0}, er[12] = {0};
  b[0]=5.42937341165687622380535766363E-2; b[5]=4.45031289275240888144113950566E0; b[6]=1.89151789931450038304281599044E0; b[7]=-5.8012039600105847814672114227E0; b[8]=3.1116436695781989440891606237E-1; b[9]=-1.52160949662516078556178806805E-1; b[10]=2.01365400804030348374776537501E-1; b[11]=4.47106157277725905176885569043E-2;
  er[0]=0.1312004499419488073250102996E-01; er[5]=-0.1225156446376204440720569753E+01; er[6]=-0.4957589496572501915214079952E+00; er[7]=0.1664377182454986536961530415E+01; er[8]=-0.3503288487499736816886487290E+00; er[9]=0.3341791187130174790297318841E+00; er[10]=0.8192320648511571246570742613E-01; er[11]=-0.2235530786388629525884427845E-01;
  for (int i = 0; i < s; ++i) { T.b[i] = b[i]; T.bhat[i] = b[i] - er[i]; }
}

// Cash-Karp RK5(4), 6 stages (rxode2 "ck54").  b = 5th-order weights, bhat =
// 4th-order embedded; errOrder = 4.
static void rksTableauCk54(rksTableau &T) {
  const int s = 6; T.s = s; T.adaptive = 1; T.errOrder = 4;
  T.c[0]=0; T.c[1]=1.0/5; T.c[2]=3.0/10; T.c[3]=3.0/5; T.c[4]=1.0; T.c[5]=7.0/8;
  T.A[1*s+0]=1.0/5;
  T.A[2*s+0]=3.0/40;         T.A[2*s+1]=9.0/40;
  T.A[3*s+0]=3.0/10;         T.A[3*s+1]=-9.0/10;      T.A[3*s+2]=6.0/5;
  T.A[4*s+0]=-11.0/54;       T.A[4*s+1]=5.0/2;        T.A[4*s+2]=-70.0/27;       T.A[4*s+3]=35.0/27;
  T.A[5*s+0]=1631.0/55296;   T.A[5*s+1]=175.0/512;    T.A[5*s+2]=575.0/13824;    T.A[5*s+3]=44275.0/110592; T.A[5*s+4]=253.0/4096;
  T.b[0]=37.0/378; T.b[1]=0; T.b[2]=250.0/621; T.b[3]=125.0/594; T.b[4]=0; T.b[5]=512.0/1771;
  T.bhat[0]=2825.0/27648; T.bhat[1]=0; T.bhat[2]=18575.0/48384; T.bhat[3]=13525.0/55296; T.bhat[4]=277.0/14336; T.bhat[5]=1.0/4;
}

// Bogacki-Shampine RK3(2), 4 stages (FSAL; the ode23 tableau, rxode2 "bs32").
// Runge-Kutta 4(3), 5-stage FSAL (coefficients from src/ode/ode_rk_43.h).
// b = 4th-order weights (propagated), bhat = 3rd-order embedded; errOrder = 3.
static void rksTableauRk43(rksTableau &T) {
  const int s = 5; T.s = s; T.adaptive = 1; T.errOrder = 3;
  T.c[0]=0; T.c[1]=1.0/3; T.c[2]=2.0/3; T.c[3]=1.0; T.c[4]=1.0;
  T.A[1*s+0]=1.0/3;
  T.A[2*s+0]=-1.0/3; T.A[2*s+1]=1.0;
  T.A[3*s+0]=1.0;    T.A[3*s+1]=-1.0;  T.A[3*s+2]=1.0;
  T.A[4*s+0]=1.0/8;  T.A[4*s+1]=3.0/8; T.A[4*s+2]=3.0/8; T.A[4*s+3]=1.0/8;
  T.b[0]=1.0/8; T.b[1]=3.0/8; T.b[2]=3.0/8; T.b[3]=1.0/8; T.b[4]=0.0;          // 4th order
  T.bhat[0]=1.0/12; T.bhat[1]=1.0/2; T.bhat[2]=1.0/4; T.bhat[3]=0.0; T.bhat[4]=1.0/6; // 3rd
}

// b = 3rd-order weights, bhat = 2nd-order embedded; errOrder = 2.
static void rksTableauBs32(rksTableau &T) {
  const int s = 4; T.s = s; T.adaptive = 1; T.errOrder = 2;
  T.c[0]=0; T.c[1]=1.0/2; T.c[2]=3.0/4; T.c[3]=1.0;
  T.A[1*s+0]=1.0/2;
  T.A[2*s+0]=0.0;    T.A[2*s+1]=3.0/4;
  T.A[3*s+0]=2.0/9;  T.A[3*s+1]=1.0/3;  T.A[3*s+2]=4.0/9;
  T.b[0]=2.0/9; T.b[1]=1.0/3; T.b[2]=4.0/9; T.b[3]=0;
  T.bhat[0]=7.0/24; T.bhat[1]=1.0/4; T.bhat[2]=1.0/3; T.bhat[3]=1.0/8;
}

// Jim Verner's "most efficient" 6(5) pair, 9 stages (rxode2 "vern65", from
// src/ode_impl.cpp OdeVern65).  b = 6th-order weights (= the stage-9 a-row),
// bhat = d (5th-order embedded); errOrder = 5.  Nodes c = A row sums.
static void rksTableauVern65(rksTableau &T) {
  const int s = 9; T.s = s; T.adaptive = 1; T.errOrder = 5;
  double *A = T.A;
  A[1*s+0]=0.06;
  A[2*s+0]=0.01923996296296296296296296296296296296296; A[2*s+1]=0.07669337037037037037037037037037037037037;
  A[3*s+0]=0.035975; A[3*s+2]=0.107925;
  A[4*s+0]=1.318683415233148260919747276431735612861; A[4*s+2]=-5.042058063628562225427761634715637693344; A[4*s+3]=4.220674648395413964508014358283902080483;
  A[5*s+0]=-41.87259166432751461803757780644346812905; A[5*s+2]=159.4325621631374917700365669070346830453; A[5*s+3]=-122.1192135650100309202516203389242140663; A[5*s+4]=5.531743066200053768252631238332999150076;
  A[6*s+0]=-54.43015693531650433250642051294142461271; A[6*s+2]=207.0672513650184644273657173866509835987; A[6*s+3]=-158.6108137845899991828742424365058599469; A[6*s+4]=6.991816585950242321992597280791793907096; A[6*s+5]=-0.01859723106220323397765171799549294623692;
  A[7*s+0]=-54.66374178728197680241215648050386959351; A[7*s+2]=207.9528062553893734515824816699834244238; A[7*s+3]=-159.2889574744995071508959805871426654216; A[7*s+4]=7.018743740796944434698170760964252490817; A[7*s+5]=-0.01833878590504572306472782005141738268361; A[7*s+6]=-0.0005119484997882099077875432497245168395840;
  A[8*s+0]=0.03438957868357036009278820124728322386520; A[8*s+3]=0.2582624555633503404659558098586120858767; A[8*s+4]=0.4209371189673537150642551514069801967032; A[8*s+5]=4.405396469669310170148836816197095664891; A[8*s+6]=-176.4831190242986576151740942499002125029; A[8*s+7]=172.3641334014150730294022582711902413315;
  for (int i = 0; i < s; ++i) { double cs = 0; for (int j = 0; j < i; ++j) cs += A[i*s+j]; T.c[i] = cs; }
  T.b[0]=0.03438957868357036009278820124728322386520; T.b[3]=0.2582624555633503404659558098586120858767; T.b[4]=0.4209371189673537150642551514069801967032; T.b[5]=4.405396469669310170148836816197095664891; T.b[6]=-176.4831190242986576151740942499002125029; T.b[7]=172.3641334014150730294022582711902413315;
  T.bhat[0]=0.04909967648382489730906854927971225836479; T.bhat[3]=0.2251112229516524153401395320539875329485; T.bhat[4]=0.4694682253029562039431948525047387412553; T.bhat[5]=0.8065792249988867707634161808995217981443; T.bhat[7]=-0.6071194891777959797672951465256217122488; T.bhat[8]=0.05686113944047569241147603178766138153594;
}

// Jim Verner's "most efficient" 7(6) pair, 10 stages (rxode2 "vern76", from
// src/ode_impl.cpp OdeVern76).  b = 7th-order, bhat = d (6th embedded); errOrder=6.
static void rksTableauVern76(rksTableau &T) {
  const int s = 10; T.s = s; T.adaptive = 1; T.errOrder = 6;
  double *A = T.A;
  A[1*s+0]=0.005;
  A[2*s+0]=-1.076790123456790123456790123456790123457; A[2*s+1]=1.185679012345679012345679012345679012346;
  A[3*s+0]=0.04083333333333333333333333333333333333333; A[3*s+2]=0.1225;
  A[4*s+0]=0.6389139236255726780508121615993336109954; A[4*s+2]=-2.455672638223656809662640566430653894211; A[4*s+3]=2.272258714598084131611828404831320283215;
  A[5*s+0]=-2.661577375018757131119259297861818119279; A[5*s+2]=10.80451388645613769565396655365532838482; A[5*s+3]=-8.353914657396199411968048547819291691541; A[5*s+4]=0.8204875949566569791420417341743839209619;
  A[6*s+0]=6.067741434696770992718360183877276714679; A[6*s+2]=-24.71127363591108579734203485290746001803; A[6*s+3]=20.42751793078889394045773111748346612697; A[6*s+4]=-1.906157978816647150624096784352757010879; A[6*s+5]=1.006172249242068014790040335899474187268;
  A[7*s+0]=12.05467007625320299509109452892778311648; A[7*s+2]=-49.75478495046898932807257615331444758322; A[7*s+3]=41.14288863860467663259698416710157354209; A[7*s+4]=-4.461760149974004185641911603484815375051; A[7*s+5]=2.042334822239174959821717077708608543738; A[7*s+6]=-0.09834843665406107379530801693870224403537;
  A[8*s+0]=10.13814652288180787641845141981689030769; A[8*s+2]=-42.64113603171750214622846006736635730625; A[8*s+3]=35.76384003992257007135021178023160054034; A[8*s+4]=-4.348022840392907653340370296908245943710; A[8*s+5]=2.009862268377035895441943593011827554771; A[8*s+6]=0.3487490460338272405953822853053145879140; A[8*s+7]=-0.2714390051048312842371587140910297407572;
  A[9*s+0]=-45.03007203429867712435322405073769635151; A[9*s+2]=187.3272437654588840752418206154201997384; A[9*s+3]=-154.0288236935018690596728621034510402582; A[9*s+4]=18.56465306347536233859492332958439136765; A[9*s+5]=-7.141809679295078854925420496823551192821; A[9*s+6]=1.308808578161378625114762706007696696508;
  for (int i = 0; i < s; ++i) { double cs = 0; for (int j = 0; j < i; ++j) cs += A[i*s+j]; T.c[i] = cs; }
  T.b[0]=0.04715561848627222170431765108838175679569; T.b[3]=0.2575056429843415189596436101037687580986; T.b[4]=0.2621665397741262047713863095764527711129; T.b[5]=0.1521609265673855740323133199165117535523; T.b[6]=0.4939969170032484246907175893227876844296; T.b[7]=-0.2943031171403250441557244744092703429139; T.b[8]=0.08131747232495109999734599440136761892478;
  T.bhat[0]=0.04460860660634117628731817597479197781432; T.bhat[3]=0.2671640378571372680509102260943837899738; T.bhat[4]=0.2201018300177293019979715776650753096323; T.bhat[5]=0.2188431703143156830983120833512893824578; T.bhat[6]=0.2289871705411202883378173889763552365362; T.bhat[9]=0.02029518466335628222767054793810430358554;
}

// Prince-Dormand 8(7) 13M, 13 stages (rxode2 "dop87", from src/ode_impl.cpp
// OdeDoPri87).  b = 8th-order, bhat = d (7th embedded); errOrder = 7.
static void rksTableauDop87(rksTableau &T) {
  const int s = 13; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double *A = T.A;
  A[1*s+0]=1.0/18;
  A[2*s+0]=1.0/48; A[2*s+1]=1.0/16;
  A[3*s+0]=1.0/32; A[3*s+2]=3.0/32;
  A[4*s+0]=5.0/16; A[4*s+2]=-75.0/64; A[4*s+3]=75.0/64;
  A[5*s+0]=3.0/80; A[5*s+3]=3.0/16; A[5*s+4]=3.0/20;
  A[6*s+0]=29443841.0/614563906; A[6*s+3]=77736538.0/692538347; A[6*s+4]=-28693883.0/1125000000; A[6*s+5]=23124283.0/1800000000;
  A[7*s+0]=16016141.0/946692911; A[7*s+3]=61564180.0/158732637; A[7*s+4]=22789713.0/633445777; A[7*s+5]=545815736.0/2771057229; A[7*s+6]=-180193667.0/1043307555;
  A[8*s+0]=39632708.0/573591083; A[8*s+3]=-433636366.0/683701615; A[8*s+4]=-421739975.0/2616292301; A[8*s+5]=100302831.0/723423059; A[8*s+6]=790204164.0/839813087; A[8*s+7]=800635310.0/3783071287;
  A[9*s+0]=246121993.0/1340847787; A[9*s+3]=-37695042795.0/15268766246; A[9*s+4]=-309121744.0/1061227803; A[9*s+5]=-12992083.0/490766935; A[9*s+6]=6005943493.0/2108947869; A[9*s+7]=393006217.0/1396673457; A[9*s+8]=123872331.0/1001029789;
  A[10*s+0]=-1028468189.0/846180014; A[10*s+3]=8478235783.0/508512852; A[10*s+4]=1311729495.0/1432422823; A[10*s+5]=-10304129995.0/1701304382; A[10*s+6]=-48777925059.0/3047939560; A[10*s+7]=15336726248.0/1032824649; A[10*s+8]=-45442868181.0/3398467696; A[10*s+9]=3065993473.0/597172653;
  A[11*s+0]=185892177.0/718116043; A[11*s+3]=-3185094517.0/667107341; A[11*s+4]=-477755414.0/1098053517; A[11*s+5]=-703635378.0/230739211; A[11*s+6]=5731566787.0/1027545527; A[11*s+7]=5232866602.0/850066563; A[11*s+8]=-4093664535.0/808688257; A[11*s+9]=3962137247.0/1805957418; A[11*s+10]=65686358.0/487910083;
  A[12*s+0]=403863854.0/491063109; A[12*s+3]=-5068492393.0/434740067; A[12*s+4]=-411421997.0/543043805; A[12*s+5]=652783627.0/914296604; A[12*s+6]=11173962825.0/925320556; A[12*s+7]=-13158990841.0/6184727034; A[12*s+8]=3936647629.0/1978049680; A[12*s+9]=-160528059.0/685178525; A[12*s+10]=248638103.0/1413531060;
  for (int i = 0; i < s; ++i) { double cs = 0; for (int j = 0; j < i; ++j) cs += A[i*s+j]; T.c[i] = cs; }
  T.b[0]=14005451.0/335480064; T.b[5]=-59238493.0/1068277825; T.b[6]=181606767.0/758867731; T.b[7]=561292985.0/797845732; T.b[8]=-1041891430.0/1371343529; T.b[9]=760417239.0/1151165299; T.b[10]=118820643.0/751138087; T.b[11]=-528747749.0/2220607170; T.b[12]=1.0/4;
  T.bhat[0]=13451932.0/455176632; T.bhat[5]=-808719846.0/976000145; T.bhat[6]=1757004468.0/5645159321; T.bhat[7]=656045339.0/265891186; T.bhat[8]=-3867574721.0/1518517206; T.bhat[9]=465885868.0/322736535; T.bhat[10]=53011238.0/667516719; T.bhat[11]=2.0/45;
}

// Fehlberg RK7(8), 13 stages (rxode2 "f78" / boost runge_kutta_fehlberg78,
// Fehlberg 1968).  b = 8th-order weights (propagated, local extrapolation),
// bhat = 7th-order; errOrder = 7.  Nodes c = A row sums.  (b, bhat both sum to 1.)
static void rksTableauF78(rksTableau &T) {
  const int s = 13; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double *A = T.A;
  A[1*s+0]=2.0/27;
  A[2*s+0]=1.0/36; A[2*s+1]=1.0/12;
  A[3*s+0]=1.0/24; A[3*s+2]=1.0/8;
  A[4*s+0]=5.0/12; A[4*s+2]=-25.0/16; A[4*s+3]=25.0/16;
  A[5*s+0]=1.0/20; A[5*s+3]=1.0/4; A[5*s+4]=1.0/5;
  A[6*s+0]=-25.0/108; A[6*s+3]=125.0/108; A[6*s+4]=-65.0/27; A[6*s+5]=125.0/54;
  A[7*s+0]=31.0/300; A[7*s+4]=61.0/225; A[7*s+5]=-2.0/9; A[7*s+6]=13.0/900;
  A[8*s+0]=2.0; A[8*s+3]=-53.0/6; A[8*s+4]=704.0/45; A[8*s+5]=-107.0/9; A[8*s+6]=67.0/90; A[8*s+7]=3.0;
  A[9*s+0]=-91.0/108; A[9*s+3]=23.0/108; A[9*s+4]=-976.0/135; A[9*s+5]=311.0/54; A[9*s+6]=-19.0/60; A[9*s+7]=17.0/6; A[9*s+8]=-1.0/12;
  A[10*s+0]=2383.0/4100; A[10*s+3]=-341.0/164; A[10*s+4]=4496.0/1025; A[10*s+5]=-301.0/82; A[10*s+6]=2133.0/4100; A[10*s+7]=45.0/82; A[10*s+8]=45.0/164; A[10*s+9]=18.0/41;
  A[11*s+0]=3.0/205; A[11*s+5]=-6.0/41; A[11*s+6]=-3.0/205; A[11*s+7]=-3.0/41; A[11*s+8]=3.0/41; A[11*s+9]=6.0/41;
  A[12*s+0]=-1777.0/4100; A[12*s+3]=-341.0/164; A[12*s+4]=4496.0/1025; A[12*s+5]=-289.0/82; A[12*s+6]=2193.0/4100; A[12*s+7]=51.0/82; A[12*s+8]=33.0/164; A[12*s+9]=12.0/41; A[12*s+11]=1.0;
  for (int i = 0; i < s; ++i) { double cs = 0; for (int j = 0; j < i; ++j) cs += A[i*s+j]; T.c[i] = cs; }
  T.b[5]=34.0/105; T.b[6]=9.0/35; T.b[7]=9.0/35; T.b[8]=9.0/280; T.b[9]=9.0/280; T.b[11]=41.0/840; T.b[12]=41.0/840;
  T.bhat[0]=41.0/840; T.bhat[5]=34.0/105; T.bhat[6]=9.0/35; T.bhat[7]=9.0/35; T.bhat[8]=9.0/280; T.bhat[9]=9.0/280; T.bhat[10]=41.0/840;
}

// ROS4 -- Shampine's L-stable 4th-order Rosenbrock, the boost runge_kutta
// rosenbrock4 / rxode2 "ros4" coefficients.  6 stages in the autonomous Hairer
// form (see rksTableau): the last stage is boost's error stage, included in the
// solution by local extrapolation (m6 = 1, and stage-6 argument u6 = u5 + g5).
// The non-autonomous d_i*df/dt terms are omitted (exact for autonomous RHS).
static void rksTableauRos4(rksTableau &T) {
  const int s = 6; T.s = s; T.rosenbrock = 1; T.gamma = 0.25;
  double *A = T.A, *G = T.gam;
  double a21=1.544, a31=0.9466785280815826, a32=0.2557011698983284,
         a41=3.314825187068521, a42=2.896124015972201, a43=0.9986419139977817,
         a51=1.221224509226641, a52=6.019134481288629, a53=12.53708332932087, a54=-0.6878860361058950;
  double c21=-5.6688, c31=-2.430093356833875, c32=-0.2063599157091915,
         c41=-0.1073529058151375, c42=-9.594562251023355, c43=-20.47028614809616,
         c51=7.496443313967647, c52=-10.24680431464352, c53=-33.99990352819905, c54=11.70890893206160,
         c61=8.083246795921522, c62=-7.981132988064893, c63=-31.52159432874371, c64=16.31930543123136, c65=-6.058818238834054;
  A[1*s+0]=a21;
  A[2*s+0]=a31; A[2*s+1]=a32;
  A[3*s+0]=a41; A[3*s+1]=a42; A[3*s+2]=a43;
  A[4*s+0]=a51; A[4*s+1]=a52; A[4*s+2]=a53; A[4*s+3]=a54;
  A[5*s+0]=a51; A[5*s+1]=a52; A[5*s+2]=a53; A[5*s+3]=a54; A[5*s+4]=1.0;   // u6 = u5 + g5
  G[1*s+0]=c21;
  G[2*s+0]=c31; G[2*s+1]=c32;
  G[3*s+0]=c41; G[3*s+1]=c42; G[3*s+2]=c43;
  G[4*s+0]=c51; G[4*s+1]=c52; G[4*s+2]=c53; G[4*s+3]=c54;
  G[5*s+0]=c61; G[5*s+1]=c62; G[5*s+2]=c63; G[5*s+3]=c64; G[5*s+4]=c65;
  T.b[0]=a51; T.b[1]=a52; T.b[2]=a53; T.b[3]=a54; T.b[4]=1.0; T.b[5]=1.0;   // x_new = x + sum m_i g_i
}

// Radau IIA, 3-stage, 5th order, L-stable, stiffly accurate (rxode2 "radauiia5").
// Fully-implicit: A is the full 3x3 matrix; b = last A row (stiffly accurate).
static void rksTableauRadau5(rksTableau &T) {
  const int s = 3; T.s = s; T.implicitRK = 1;
  double q = sqrt(6.0);
  T.c[0]=(4.0-q)/10.0; T.c[1]=(4.0+q)/10.0; T.c[2]=1.0;
  T.A[0*s+0]=(88.0-7.0*q)/360.0;    T.A[0*s+1]=(296.0-169.0*q)/1800.0; T.A[0*s+2]=(-2.0+3.0*q)/225.0;
  T.A[1*s+0]=(296.0+169.0*q)/1800.0;T.A[1*s+1]=(88.0+7.0*q)/360.0;     T.A[1*s+2]=(-2.0-3.0*q)/225.0;
  T.A[2*s+0]=(16.0-q)/36.0;         T.A[2*s+1]=(16.0+q)/36.0;          T.A[2*s+2]=1.0/9.0;
  T.b[0]=(16.0-q)/36.0; T.b[1]=(16.0+q)/36.0; T.b[2]=1.0/9.0;   // = A row 3 (stiffly accurate)
}

// Backward (implicit) Euler: 1-stage fully-implicit RK, order 1, L-stable.
static void rksTableauBackwardEuler(rksTableau &T) {
  T.s = 1; T.implicitRK = 1;
  T.c[0] = 1.0; T.A[0] = 1.0; T.b[0] = 1.0;
}

// Gauss-Legendre, 3-stage, order 6 (A-stable, symplectic).  Fully-implicit RK
// with the full (non-lower-triangular) A -- solved by the same coupled Newton
// as Radau; NOT stiffly accurate (b != last A row), which the framework allows.
static void rksTableauGauss6(rksTableau &T) {
  const int s = 3; T.s = s; T.implicitRK = 1;
  double q = sqrt(15.0);
  T.c[0]=0.5-q/10.0; T.c[1]=0.5; T.c[2]=0.5+q/10.0;
  T.A[0*s+0]=5.0/36.0;        T.A[0*s+1]=2.0/9.0-q/15.0; T.A[0*s+2]=5.0/36.0-q/30.0;
  T.A[1*s+0]=5.0/36.0+q/24.0; T.A[1*s+1]=2.0/9.0;        T.A[1*s+2]=5.0/36.0-q/24.0;
  T.A[2*s+0]=5.0/36.0+q/30.0; T.A[2*s+1]=2.0/9.0+q/15.0; T.A[2*s+2]=5.0/36.0;
  T.b[0]=5.0/18.0; T.b[1]=4.0/9.0; T.b[2]=5.0/18.0;
}

// SDIRK43, 5-stage, order 3, L-stable (Hairer & Wanner, Solving ODEs II).  A is
// lower-triangular with constant diagonal gamma=1/4 (singly diagonally implicit)
// and stiffly accurate (b = last A row).  Runs on the fully-implicit Radau
// coupled-Newton framework -- the lower-triangular A just makes the coupled
// Newton matrix block-lower-triangular (same converged stages, adjoint exact).
static void rksTableauSdirk43(rksTableau &T) {
  const int s = 5; T.s = s; T.implicitRK = 1;
  double g = 0.25;
  T.A[0*s+0]=g;
  T.A[1*s+0]=1.0/2;      T.A[1*s+1]=g;
  T.A[2*s+0]=17.0/50;    T.A[2*s+1]=-1.0/25;     T.A[2*s+2]=g;
  T.A[3*s+0]=371.0/1360; T.A[3*s+1]=-137.0/2720; T.A[3*s+2]=15.0/544;  T.A[3*s+3]=g;
  T.A[4*s+0]=25.0/24;    T.A[4*s+1]=-49.0/48;    T.A[4*s+2]=125.0/16;  T.A[4*s+3]=-85.0/12; T.A[4*s+4]=g;
  T.b[0]=25.0/24; T.b[1]=-49.0/48; T.b[2]=125.0/16; T.b[3]=-85.0/12; T.b[4]=g;  // = A row 5
  T.c[0]=g; T.c[1]=3.0/4; T.c[2]=11.0/20; T.c[3]=1.0/2; T.c[4]=1.0;
}

// Lobatto IIIC, 4-stage, order 6, L-stable (coefficients from
// src/ode/ode_lobatto_iiic_6.h).  Fully-implicit RK (full A) with c[0]=0 but a
// nonzero first row -> solved by the same coupled Newton as Radau; stiffly
// accurate (b = last A row).
static void rksTableauLobattoIIIC6(rksTableau &T) {
  const int s = 4; T.s = s; T.implicitRK = 1;
  double r = sqrt(5.0);
  T.A[0*s+0]=1.0/12; T.A[0*s+1]=-r/12.0;         T.A[0*s+2]=r/12.0;          T.A[0*s+3]=-1.0/12;
  T.A[1*s+0]=1.0/12; T.A[1*s+1]=1.0/4;           T.A[1*s+2]=(10.0-7.0*r)/60.0; T.A[1*s+3]=r/60.0;
  T.A[2*s+0]=1.0/12; T.A[2*s+1]=(10.0+7.0*r)/60.0; T.A[2*s+2]=1.0/4;         T.A[2*s+3]=-r/60.0;
  T.A[3*s+0]=1.0/12; T.A[3*s+1]=5.0/12;          T.A[3*s+2]=5.0/12;          T.A[3*s+3]=1.0/12;
  T.b[0]=1.0/12; T.b[1]=5.0/12; T.b[2]=5.0/12; T.b[3]=1.0/12;   // = A row 4
  T.c[0]=0.0; T.c[1]=0.5-r/10.0; T.c[2]=0.5+r/10.0; T.c[3]=1.0;
}

// Geng5, 3-stage, order 5 fully-implicit RK (coefficients from
// src/ode/ode_geng_5.h / OdeGeng5).  Same b/c as Radau IIA5 but a different A
// (not stiffly accurate); runs on the coupled-Newton framework unchanged.
static void rksTableauGeng5(rksTableau &T) {
  const int s = 3; T.s = s; T.implicitRK = 1;
  double r = sqrt(6.0);
  T.c[0]=(4.0-r)/10.0; T.c[1]=(4.0+r)/10.0; T.c[2]=1.0;
  T.A[0*s+0]=(16.0-r)/72.0;          T.A[0*s+1]=(328.0-167.0*r)/1800.0; T.A[0*s+2]=(-2.0+3.0*r)/450.0;
  T.A[1*s+0]=(328.0+167.0*r)/1800.0; T.A[1*s+1]=(16.0+r)/72.0;          T.A[1*s+2]=(-2.0-3.0*r)/450.0;
  T.A[2*s+0]=(85.0-10.0*r)/180.0;    T.A[2*s+1]=(85.0+10.0*r)/180.0;    T.A[2*s+2]=1.0/18.0;
  T.b[0]=(16.0-r)/36.0; T.b[1]=(16.0+r)/36.0; T.b[2]=1.0/9.0;
}

// GRK4A (Kaps-Rentrop), 4-stage, order 4, L-stable Rosenbrock -- rxode2's "ros43".
// The libode ROW-form coefficients (alp, gam_ij, b, gamma=0.395 from
// src/ode/ode_grk4a.h) are transformed to the Hairer-Wanner form this framework
// uses (a = alp*Ginv, c = -Ginv off-diagonal, m = Ginv^T b, with Ginv = inverse
// of the lower-triangular gamma_ij matrix).
static void rksTableauGrk4a(rksTableau &T) {
  const int s = 4; T.s = s; T.rosenbrock = 1; T.gamma = 0.395;
  double *A = T.A, *G = T.gam;
  A[1*s+0]=1.10886075949367;
  A[2*s+0]=2.37708526198196; A[2*s+1]=0.185011498889874;
  A[3*s+0]=2.37708526198196; A[3*s+1]=0.185011498889874; A[3*s+2]=0.0;
  G[1*s+0]=-4.92018840239705;
  G[2*s+0]=1.05558868604935; G[2*s+1]=3.35181726766864;
  G[3*s+0]=3.84686900704732; G[3*s+1]=3.42710924127013; G[3*s+2]=-2.16240884875501;
  T.b[0]=1.84568324040826; T.b[1]=0.136979689436354; T.b[2]=0.712909778329511; T.b[3]=0.632911392405063;
}

// ROW6A, 6-stage, order 6 Rosenbrock -- rxode2's "ros6" (src/ode/ode_row_6a.h /
// ode_impl.cpp OdeROW6A).  Its ROW form is the DIRECT one (k_i = W^{-1}(h f + sum
// c_ij k_j), no subtraction trick), so the Hairer-Wanner coefficients are just
// a_ij/gamma, c_ij/gamma, m_i/gamma (see the u=gamma*k scaling).
static void rksTableauRos6(rksTableau &T) {
  const int s = 6; T.s = s; T.rosenbrock = 1;
  double g = 0.33414236706805043; T.gamma = g; double ig = 1.0/g;
  double *A = T.A, *G = T.gam;
  A[1*s+0]=0.66828473413610087*ig;
  A[2*s+0]=0.5852480389573658*ig;   A[2*s+1]=-0.048594008221492802*ig;
  A[3*s+0]=-0.61719233202999775*ig; A[3*s+1]=-0.83995264476522158*ig;  A[3*s+2]=0.62641917900148600*ig;
  A[4*s+0]=3.5406887484552165*ig;   A[4*s+1]=0.65991497772646308*ig;   A[4*s+2]=-0.63661180895697222*ig; A[4*s+3]=-1.1945984675295562*ig;
  A[5*s+0]=0.80783664328582613*ig;  A[5*s+1]=0.10194631616818569*ig;   A[5*s+2]=-0.078396778850607012*ig; A[5*s+3]=-0.044341977375427388*ig; A[5*s+4]=0.013074732797453325*ig;
  G[1*s+0]=-5.8308828523185086*ig;
  G[2*s+0]=-4.0175939515896193*ig;  G[2*s+1]=0.43970131925236112*ig;
  G[3*s+0]=7.7228006257490299*ig;   G[3*s+1]=4.3368108251435758*ig;    G[3*s+2]=-2.8219574578033366*ig;
  G[4*s+0]=-1.0516225114542007*ig;  G[4*s+1]=-0.58853585181331353*ig;  G[4*s+2]=2.0433794587212771*ig;   G[4*s+3]=5.0098631723809151*ig;
  G[5*s+0]=-6.7357785372199458*ig;  G[5*s+1]=-0.53593889506199845*ig;  G[5*s+2]=0.38622517020810987*ig;  G[5*s+3]=0.21066472713931598*ig;  G[5*s+4]=-0.053546655670373728*ig;
  T.b[0]=11.358660043232931*ig; T.b[1]=-6.9896898855829058*ig; T.b[2]=-4.5967580421042947*ig;
  T.b[3]=-3.7220984696531517*ig; T.b[4]=0.96012685868421520*ig; T.b[5]=12.953396234292936*ig;
}

// Jim Verner's "most efficient" 9(8) pair, 16 stages (rxode2 "vern98", from
// src/ode_impl.cpp OdeVern98).  b = 9th-order weights, bhat = d (8th embedded);
// errOrder = 8.  Nodes c and the sparse A are copied verbatim from OdeVern98.
static void rksTableauVern98(rksTableau &T) {
  const int s = 16; T.s = s; T.adaptive = 1; T.errOrder = 8;
  double *A = T.A;
  A[1*s+0]=0.03462;
  A[2*s+0]=-0.0389335438857287327017042687229284478532; A[2*s+1]=0.1359578945245091786499878854939346230295;
  A[3*s+0]=0.03638413148954266723060635628912731569111; A[3*s+2]=0.1091523944686280016918190688673819470733;
  A[4*s+0]=2.025763914393969636805657604282571047511;   A[4*s+2]=-7.638023836496292020387602153091964592952; A[4*s+3]=6.173259922102322383581944548809393545442;
  A[5*s+0]=0.05112275589406060872792270881648288397197; A[5*s+3]=0.1770823794555021537929910813839068684087; A[5*s+4]=0.00080277624092225014536138698108025283759;
  A[6*s+0]=0.1316006357975216279279871693164256985334;  A[6*s+3]=-0.2957276252669636417685183174672273730699; A[6*s+4]=0.0878137803564295237421124704053886667082; A[6*s+5]=0.62130529752252747743214350056394300261;
  A[7*s+0]=0.07166666666666666666666666666666666666667;  A[7*s+5]=0.3305533578915319409260346730051472207728; A[7*s+6]=0.2427799754418013924072986603281861125606;
  A[8*s+0]=0.071806640625;                               A[8*s+5]=0.3294380283228177160744825466257672816401; A[8*s+6]=0.1165190029271822839255174533742327183599; A[8*s+7]=-0.034013671875;
  A[9*s+0]=0.04836757646340646986611287718844085773549;  A[9*s+5]=0.03928989925676163974333190042057047002852; A[9*s+6]=0.1054740945890344608263649267140088017604; A[9*s+7]=-0.02143865284648312665982642293830533996214; A[9*s+8]=-0.1041229174627194437759832813847147895623;
  A[10*s+0]=-0.02664561487201478635337289243849737340534; A[10*s+5]=0.03333333333333333333333333333333333333333; A[10*s+6]=-0.1631072244872467239162704487554706387141; A[10*s+7]=0.03396081684127761199487954930015522928244; A[10*s+8]=0.1572319413814626097110769806810024118077; A[10*s+9]=0.2152267478031879552303534778794770376960;
  A[11*s+0]=0.03689009248708622334786359863227633989718;  A[11*s+5]=-0.1465181576725542928653609891758501156785; A[11*s+6]=0.2242577768172024345345469822625833796001; A[11*s+7]=0.02294405717066072637090897902753790803034; A[11*s+8]=-0.0035850052905728761357394424889330334334; A[11*s+9]=0.08669223316444385506869203619044453906053; A[11*s+10]=0.4383840651968337846196219974168630120572;
  A[12*s+0]=-0.4866012215113340846662212357570395295088;  A[12*s+5]=-6.304602650282852990657772792012007122988; A[12*s+6]=-0.281245618289472564778284183790118418111; A[12*s+7]=-2.679019236219849057687906597489223155566; A[12*s+8]=0.518815663924157511565311164615012522024; A[12*s+9]=1.365353187603341710683633635235238678626; A[12*s+10]=5.885091088503946585721274891680604830712; A[12*s+11]=2.802808786272062889819965117517532194812;
  A[13*s+0]=0.4185367457753471441471025246471931649633;   A[13*s+5]=6.724547581906459363100870806514855026676; A[13*s+6]=-0.425444280164611790606983409697113064616; A[13*s+7]=3.343279153001265577811816947557982637749; A[13*s+8]=0.617081663117537759528421117507709784737; A[13*s+9]=-0.929966123939932833937749523988800852013; A[13*s+10]=-6.099948804751010722472962837945508844846; A[13*s+11]=-3.002206187889399044804158084895173690015; A[13*s+12]=0.2553202529443445472336424602988558373637;
  A[14*s+0]=-0.779374086122884664644623040843840506343;   A[14*s+5]=-13.93734253810777678786523664804936051203; A[14*s+6]=1.252048853379357320949735183924200895136; A[14*s+7]=-14.69150040801686878191527989293072091588; A[14*s+8]=-0.494705058533141685655191992136962873577; A[14*s+9]=2.242974909146236657906984549543692874755; A[14*s+10]=13.36789380382864375813864978592679139881; A[14*s+11]=14.39665048665068644512236935340272139005; A[14*s+12]=-0.7975813331776800379127866056663258667437; A[14*s+13]=0.4409353709534277758753793068298041158235;
  A[15*s+0]=2.058051337466886442151242368989994043993;    A[15*s+5]=22.35793772796803295519317565842520212899; A[15*s+6]=0.90949810997556332745009198137971890783; A[15*s+7]=35.89110098240264104710550686568482456493; A[15*s+8]=-3.442515027624453437985000403608480262211; A[15*s+9]=-4.865481358036368826566013387928704014496; A[15*s+10]=-18.90980381354342625688427480879773032857; A[15*s+11]=-34.26354448030451782929251177395134170515; A[15*s+12]=1.264756521695642578827783499806516664686;
  T.c[0]=0; T.c[1]=0.03462; T.c[2]=0.09702435063878044594828361677100617517633; T.c[3]=0.1455365259581706689224254251565092627645;
  T.c[4]=0.561; T.c[5]=0.2290079115904850126662751771814700052182; T.c[6]=0.5449920884095149873337248228185299947818; T.c[7]=0.645;
  T.c[8]=0.48375; T.c[9]=0.06757; T.c[10]=0.2500; T.c[11]=0.6590650618730998549405331618649220295334;
  T.c[12]=0.8206; T.c[13]=0.9012; T.c[14]=1.0; T.c[15]=1.0;
  T.b[0]=0.01461197685842315252051541915018784713459; T.b[7]=-0.3915211862331339089410228267288242030810; T.b[8]=0.2310932500289506415909675644868993669908; T.b[9]=0.1274766769992852382560589467488989175618; T.b[10]=0.2246434176204157731566981937082069688984; T.b[11]=0.5684352689748512932705226972873692126743; T.b[12]=0.05825871557215827200814768021863420902155; T.b[13]=0.1364317403482215641609022744494239843327; T.b[14]=0.03057013983082797397721005067920369646664;
  T.bhat[0]=0.01996996514886773085518508418098868756464; T.bhat[7]=2.191499304949330054530747099310837524864; T.bhat[8]=0.08857071848208438030833722031786358862953; T.bhat[9]=0.1140560234865965622484956605091432032674; T.bhat[10]=0.2533163805345107065564577734569651977347; T.bhat[11]=-2.056564386240941011158999594595981300493; T.bhat[12]=0.3408096799013119935160094894224543812830; T.bhat[15]=0.04834231373823958314376726739772871714902;
}

// ---- libode explicit-RK tableaus (auto-generated from src/ode_rk*.cpp; the
// discrete adjoint is table-driven, so each is just its Butcher tableau) --------
static void rksTableauF45(rksTableau &T) {
  const int s = 6; T.s = s; T.adaptive = 1; T.errOrder = 4;
  double c2, c3, c4, c6, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, b1, b3, b4, b5, b6, d1, d3, d4, d5, d6;
  c2 = 1.0  / 4.0;
  c3 = 3.0  / 8.0;
  c4 = 12.0 / 13.0;
  c6 = 1.0  / 2.0;
  a21 = 1.0 / 4.0;
  a31 = 3.0 / 32.0;
  a32 = 9.0 / 32.0;
  a41 = 1932.0  / 2197.0;
  a42 = -7200.0 / 2197.0;
  a43 = 7296.0  / 2197.0;
  a51 = 439.0  / 216.0;
  a52 = -8.0;
  a53 = 3680.0 / 513.0;
  a54 = -845.0 / 4104.0;
  a61 = -8.0     / 27.0;
  a62 = 2.0;
  a63 = -3544.0  / 2565.0;
  a64 = 1859.0  / 4104.0;
  a65 = -11.0    / 40.0;
  b1 = 16.0    / 135.0;
  b3 = 6656.0  / 12825.0;
  b4 = 28561.0 / 56430.0;
  b5 = -9.0    / 50.0;
  b6 = 2.0     / 55.0;
  d1 = 25.0 / 216.0;
  d3 = 1408.0 / 2565.0;
  d4 = 2197.0 / 4104.0;
  d5 = -1.0  / 5.0;
  d6 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
}

static void rksTableauT54(rksTableau &T) {
  const int s = 7; T.s = s; T.adaptive = 1; T.errOrder = 4;
  double c2, c3, c4, c5, c6, a32, a42, a43, a52, a53, a54, a62, a63, a64, a65, a21, a31, a41, a51, a61, b1, b2, b3, b4, b5, b6, d1, d2, d3, d4, d5, d6, d7, b7;
  c2 = 0.161;
  c3 = 0.327;
  c4 = 0.9;
  c5 = 0.9800255409045096857298102862870245954942137979563024768854764293221195950761080302604;
  c6 = 1.0;
  a32 = 0.3354806554923569885444268742502307746751211773934303915373692342452941929761641411569;
  a42 = -6.359448489975074843148159912383825625952700647415626703305928850207288721235210244366;
  a43 = 4.362295432869581411017727318190886861027813359713760212991062156752264926097707165077;
  a52 = -11.74888356406282787774717033978577296188744178259862899288666928009020615663593781589;
  a53 = 7.495539342889836208304604784564358155658679161518186721010132816213648793440552049753;
  a54 = -0.9249506636175524925650207933207191611349983406029535244034750452930469056411389539635e-1;
  a62 = -12.92096931784710929170611868178335939541780751955743459166312250439928519268343184452;
  a63 = 8.159367898576158643180400794539253485181918321135053305748355423955009222648673734986;
  a64 = -0.7158497328140099722453054252582973869127213147363544882721139659546372402303777878835e-1;
  a65 = -0.2826905039406838290900305721271224146717633626879770007617876201276764571291579142206e-1;
  a21 = c2;
  a31 = c3 - (a32);
  a41 = c4 - (a42+a43);
  a51 = c5 - (a52+a53+a54);
  a61 = c6 - (a62+a63+a64+a65);
  b1 = 0.9646076681806522951816731316512876333711995238157997181903319145764851595234062815396e-1;
  b2 = 0.1e-1;
  b3 = 0.4798896504144995747752495322905965199130404621990332488332634944254542060153074523509;
  b4 = 1.379008574103741893192274821856872770756462643091360525934940067397245698027561293331;
  b5 = -3.290069515436080679901047585711363850115683290894936158531296799594813811049925401677;
  b6 = 2.324710524099773982415355918398765796109060233222962411944060046314465391054716027841;
  d1 = 0.9468075576583945807478876255758922856117527357724631226139574065785592789071067303271e-1;
  d2 = 0.9183565540343253096776363936645313759813746240984095238905939532922955247253608687270e-2;
  d3 = 0.4877705284247615707855642599631228241516691959761363774365216240304071651579571959813;
  d4 = 1.234297566930478985655109673884237654035539930748192848315425833500484878378061439761;
  d5 = -2.707712349983525454881109975059321670689605166938197378763992255714444407154902012702;
  d6 = 1.866628418170587035753719399566211498666255505244122593996591602841258328965767580089;
  d7 = 1.0 / 66.0;
  b7 = 0.0;
  T.A[2*s+1]=a32;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[3*s+0]=a41;
  T.A[4*s+0]=a51;
  T.A[5*s+0]=a61;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=b2;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.bhat[0]=d1;
  T.bhat[1]=d2;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
}

static void rksTableauPp54(rksTableau &T) {
  const int s = 7; T.s = s; T.adaptive = 1; T.errOrder = 4;
  double c2, c3, c4, c5, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, b1, b3, b4, b5, b6, d1, d3, d4, d5, d6, d7, b7;
  c2 = 2.03174603174603174603174603174603174603174603174603174603174603174603174603174603175e-1;
  c3 = 3.01837270341207349081364829396325459317585301837270341207349081364829396325459317585e-1;
  c4 = 8.14973262032085561497326203208556149732620320855614973262032085561497326203208556150e-1;
  c5 = 8.92857142857142857142857142857142857142857142857142857142857142857142857142857142857e-1;
  a21 = 2.03174603174603174603174603174603174603174603174603174603174603174603174603174603175e-1;
  a31 = 7.76317438051542769752206171079008824684316035298737264141194949056564779796226259119e-2;
  a32 = 2.24205526536053072106144212288424576849153698307396614793229586459172918345836691673e-1;
  a41 = 9.97054225910649851955403847106996156261144080556828071531226332913609633807524002826e-1;
  a42 = -3.9229583098359766207115693643000730264502583412156151006542354260709699970151262085e0;
  a43 = 3.74087734595741233025349172040163301992173458151440200238504117871885768941081076180e0;
  a51 = 2.41107195213820140711520163970784555943075679078314370129890823084996830515582884769e0;
  a52 = -9.6949301103878662103154820500485383234086722205716227648448127788897312407254087039e0;
  a53 = 8.37096577602619810632388905477462929171641908259741309319801335095185921382851168188e0;
  a54 = -1.9425047491939044598075150157679367059564650995179117250925166005495342111607468280e-1;
  a61 = 2.71680576250618168562042732618844645610969561203458029063121928183564071601176708510e0;
  a62 = -1.0546836035293105493140447781471570084309037601955971025884876060757459965008137190e1;
  a63 = 8.82438454669452608734254952418112316791646628404483378649096846259826474086840654822e0;
  a64 = 3.30231934937991900184188201559235426225444020547394478857972406538849522552348656011e-1;
  a65 = -3.2458620884559418000671727045723496594256831467083753009528409021529501442438509930e-1;
  a71 = 9.15102514359618091216858762219939898816995701624253490052873825554414393852942295257e-2;
  a72 = 0;
  a73 = 4.53088687963682563226735078441923499983686103419218207034508391590507417156754767246e-1;
  a74 = 7.70212354683768742558003248195356972937967314339813992394948819785937766391608532599e-1;
  a75 = -4.6993115056469338687729550442725993708636301781314244468307254289871602785250570955e-1;
  a76 = 1.55119856481280271970871301567985474283010029891684896248327948966829404918848180181e-1;
  b1 = 9.15102514359618091216858762219939898816995701624253490052873825554414393852942295257e-2;
  b3 = 4.53088687963682563226735078441923499983686103419218207034508391590507417156754767246e-1;
  b4 = 7.70212354683768742558003248195356972937967314339813992394948819785937766391608532599e-1;
  b5 = -4.6993115056469338687729550442725993708636301781314244468307254289871602785250570955e-1;
  b6 = 1.55119856481280271970871301567985474283010029891684896248327948966829404918848180181e-1;
  d1 = 9.00540754825040114941655607289341515504490822369589853929344566160995864634775952099e-2;
  d3 = 4.58093041355081442291135932317120322249435302966675363526235643007610910215270397680e-1;
  d4 = 7.17141736822002520962978129215068133609337518199443217706780208321061003994969882396e-1;
  d5 = -3.9730095904319871050513227050199810426175540655366523944169265633781699285936171784e-1;
  d6 = 1.08202581574086926233043124431351687328723979341063863291932824583521682661834318740e-1;
  d7 = 2.38095238095238095238095238095238095238095238095238095238095238095238095238095238095e-2;
  b7 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
}

static void rksTableauPp54b(rksTableau &T) {
  const int s = 7; T.s = s; T.adaptive = 1; T.errOrder = 4;
  double c2, c3, c4, c5, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, b1, b3, b4, b5, b6, d1, d3, d4, d5, d6, d7, b7;
  c2 = 2.201257861635220125786163522012578616352201257861635220125786163522012578616352201257e-1;
  c3 = 3.206106870229007633587786259541984732824427480916030534351145038167938931297709923664e-1;
  c4 = 9.160839160839160839160839160839160839160839160839160839160839160839160839160839160839e-1;
  c5 = 9.545454545454545454545454545454545454545454545454545454545454545454545454545454545454e-1;
  a21 = 2.201257861635220125786163522012578616352201257861635220125786163522012578616352201257e-1;
  a31 = 8.712778975584173416467571819824019579278596818367228017015325447235009614824310937591e-2;
  a32 = 2.334828972670590291941029077559582774896567799079307732649612493444437969815278829905e-1;
  a41 = 1.229131120244552267075198689039393789189448076016053493121879281174531378872492304514e0;
  a42 = -5.174619722997219626464507457640199024688660926548912688647080129723360618339765067163e0;
  a43 = 4.861572518836583443305392684684721319415296766616775279441284764632745323383356678732e0;
  a51 = 1.635985585148686070813241441537203183216255340359063210526474302100593166648405715598e0;
  a52 = -6.911673476406501672554353133581688231701838702998878887272831441157207088140814883585e0;
  a53 = 6.264598153851822604079697377077162805912462110097874626859336947971237445955455470077e0;
  a54 = -3.436480804855245688404023048722321197233329291260440465843435436916897900850084754553e-2;
  a61 = 2.080199455181079060589945391266072650640389894799843423576531143602673095720452243744e0;
  a62 = -8.741042793278798222268721650787838365080091632936417117311519288907488660315135346032e0;
  a63 = 7.705947570635899833548328481096646034173724896740120966754373004394344077008935321798e0;
  a64 = 8.908088473605147947645976479787347297467362259887092033560387203210318720083482332807e-2;
  a65 = -1.341851172742321513460119863727537927086967812024181933549887311216316996150870428391e-1;
  a71 = 9.660368812495311950055679608569466802836486587849728525846301200717776508398100546410e-2;
  a72 = 0;
  a73 = 4.847050840514515173851124515232483753229089551347247274647212546244695209679216658037e-1;
  a74 = 2.985721504066570633782817413202582623066368759430782759246016403146502612390979295903e0;
  a75 = -3.882058366130615720106688908002536902372698760219285670845605164160172370352994326721e0;
  a76 = 1.315028089887640449438202247191011235955056179775280898876404494382022471910112359550e0;
  b1 = 9.660368812495311950055679608569466802836486587849728525846301200717776508398100546410e-2;
  b3 = 4.847050840514515173851124515232483753229089551347247274647212546244695209679216658037e-1;
  b4 = 2.985721504066570633782817413202582623066368759430782759246016403146502612390979295903e0;
  b5 = -3.882058366130615720106688908002536902372698760219285670845605164160172370352994326721e0;
  b6 = 1.315028089887640449438202247191011235955056179775280898876404494382022471910112359550e0;
  d1 = 9.532926220038256951865552620720509864047974971891244940167035171716827175894410248606e-2;
  d3 = 4.890503958633595206242721536021852509608016393506331933941960604717211861520491115111e-1;
  d4 = 2.782787369422591128285131424607412688536477162495912788909265685055380447336451073683e0;
  d5 = -3.544320847545574336397313295146726421831558091536665262636276708453748484122524958198e0;
  d6 = 1.168225248630669689397825619301351955122370968542635402359716039780907150303652099088e0;
  d7 = 8.928571428571428571428571428571428571428571428571428571428571428571428571428571428571e-3;
  b7 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
}

static void rksTableauBs54(rksTableau &T) {
  const int s = 8; T.s = s; T.adaptive = 1; T.errOrder = 4;
  double c2, c3, c4, c5, c6, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, a81, a83, a84, a85, a86, a87, b1, b3, b4, b5, b6, b7, d1, d2, d3, d4, d5, d6, d7, d8, b8;
  c2 = 1.0 / 6.0;
  c3 = 2.0 / 9.0;
  c4 = 3.0 / 7.0;
  c5 = 2.0 / 3.0;
  c6 = 3.0 / 4.0;
  a21 = 1.0 / 6.0;
  a31 = 2.0 / 27.0;
  a32 = 4.0 / 27.0;
  a41 = 183.0 / 1372.0;
  a42 = -162.0 / 343.0;
  a43 = 1053.0 / 1372.0;
  a51 = 68.0 / 297.0;
  a52 = -4.0 / 11.0;
  a53 = 42.0 / 143.0;
  a54 = 1960.0 / 3861.0;
  a61 = 597.0 / 22528.0;
  a62 = 81.0 / 352.0;
  a63 = 63099.0 / 585728.0;
  a64 = 58653.0 / 366080.0;
  a65 = 4617.0 / 20480.0;
  a71 = 174197.0 / 959244.0;
  a72 = -30942.0 / 79937.0;
  a73 = 8152137.0 / 19744439.0;
  a74 = 666106.0 / 1039181.0;
  a75 = -29421.0 / 29068.0;
  a76 = 482048.0 / 414219.0;
  a81 = 587.0 / 8064.0;
  a83 = 4440339.0 / 15491840.0;
  a84 = 24353.0 / 124800.0;
  a85 = 387.0 / 44800.0;
  a86 = 2152.0 / 5985.0;
  a87 = 7267.0 / 94080.0;
  b1 = 587.0 / 8064.0;
  b3 = 4440339.0 / 15491840.0;
  b4 = 24353.0 / 124800.0;
  b5 = 387.0 / 44800.0;
  b6 = 2152.0 / 5985.0;
  b7 = 7267.0 / 94080.0;
  d1 = 2479.0 / 34992.0;
  d2 = 0;
  d3 = 123.0 / 416.0;
  d4 = 612941.0 / 3411720.0;
  d5 = 43.0 / 1440.0;
  d6 = 2272.0 / 6561.0;
  d7 = 79937.0 / 1113912.0;
  d8 = 3293.0 / 556956.0;
  b8 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.bhat[0]=d1;
  T.bhat[1]=d2;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
}

static void rksTableauSs54(rksTableau &T) {
  const int s = 7; T.s = s; T.adaptive = 1; T.errOrder = 4;
  double c2, c3, c4, c5, c6, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, b1, b3, b4, b5, b6, b7, d1, d3, d4, d5, d6, d7;
  c2 = 16.0 / 105.0;
  c3 = 8.0 / 35.0;
  c4 = 9.0 / 20.0;
  c5 = 2.0 / 3.0;
  c6 = 7.0 / 9.0;
  a21 = 16.0 / 105.0;
  a31 = 2.0 / 35.0;
  a32 = 6.0 / 35.0;
  a41 = 8793.0 / 40960.0;
  a42 = -5103.0 / 8192.0;
  a43 = 17577.0 / 20480.0;
  a51 = 347.0 / 1458.0;
  a52 = -7.0 / 20.0;
  a53 = 3395.0 / 10044.0;
  a54 = 49792.0 / 112995.0;
  a61 = -1223224109959.0 / 9199771214400.0;
  a62 = 1234787701.0 / 2523942720.0;
  a63 = 568994101921.0 / 3168810084960.0;
  a64 = -105209683888.0 / 891227836395.0;
  a65 = 9.0 / 25.0;
  a71 = 2462504862877.0 / 8306031988800.0;
  a72 = -123991.0 / 287040.0;
  a73 = 106522578491.0 / 408709510560.0;
  a74 = 590616498832.0 / 804646848915.0;
  a75 = -319138726.0 / 534081275.0;
  a76 = 52758.0 / 71449.0;
  b1 = 1093.0 / 15120.0;
  b3 = 60025.0 / 190992.0;
  b4 = 3200.0 / 20709.0;
  b5 = 1611.0 / 11960.0;
  b6 = 712233.0 / 2857960.0;
  b7 = 3.0 / 40.0;
  d1 = 84018211.0 / 991368000.0;
  d3 = 92098979.0 / 357791680.0;
  d4 = 17606944.0 / 67891005.0;
  d5 = 3142101.0 / 235253200.0;
  d6 = 22004596809.0 / 70270091500.0;
  d7 = 9.0 / 125.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
}

static void rksTableauDp65(rksTableau &T) {
  const int s = 8; T.s = s; T.adaptive = 1; T.errOrder = 5;
  double c2, c3, c4, c5, c6, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, a81, a82, a83, a84, a85, a86, b1, b3, b4, b5, b6, b7, b8, d1, d3, d4, d5, d6, d7, d8;
  c2 = 1.0 / 10.0;
  c3 = 2.0 / 9.0;
  c4 = 3.0 / 7.0;
  c5 = 3.0 / 5.0;
  c6 = 4.0 / 5.0;
  a21 = 1.0 / 10.0;
  a31 = -2.0 / 81.0;
  a32 = 20.0 / 81.0;
  a41 = 615.0 / 1372.0;
  a42 = -270.0 / 343.0;
  a43 = 1053.0 / 1372.0;
  a51 = 3243.0 / 5500.0;
  a52 = -54.0 / 55.0;
  a53 = 50949.0 / 71500.0;
  a54 = 4998.0 / 17875.0;
  a61 = -26492.0 / 37125.0;
  a62 = 72.0 / 55.0;
  a63 = 2808.0 / 23375.0;
  a64 = -24206.0 / 37125.0;
  a65 = 338.0 / 459.0;
  a71 = 5561.0 / 2376.0;
  a72 = -35.0 / 11.0;
  a73 = -24117.0 / 31603.0;
  a74 = 899983.0 / 200772.0;
  a75 = -5225.0 / 1836.0;
  a76 = 3925.0 / 4056.0;
  a81 = 465467.0 / 266112.0;
  a82 = -2945.0 / 1232.0;
  a83 = -5610201.0 / 14158144.0;
  a84 = 10513573.0 / 3212352.0;
  a85 = -424325.0 / 205632.0;
  a86 = 376225.0 / 454272.0;
  b1 = 61.0 / 864.0;
  b3 = 98415.0 / 321776.0;
  b4 = 16807.0 / 146016.0;
  b5 = 1375.0 / 7344.0;
  b6 = 1375.0 / 5408.0;
  b7 = -37.0 / 1120.0;
  b8 = 1.0 / 10.0;
  d1 = 821.0 / 10800.0;
  d3 = 19683.0 / 71825.0;
  d4 = 175273.0 / 912600.0;
  d5 = 395.0 / 3672.0;
  d6 = 785.0 / 2704.0;
  d7 = 3.0 / 50.0;
  d8 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+1]=a82;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
}

static void rksTableauC65(rksTableau &T) {
  const int s = 9; T.s = s; T.adaptive = 1; T.errOrder = 5;
  double c2, c3, c4, c5, c6, c7, a21, a31, a32, a41, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, a81, a82, a83, a84, a85, a86, a87, a91, a93, a94, a95, a96, a97, a98, b1, b3, b4, b5, b6, b7, b8, d1, d3, d4, d5, d6, d7, d8, d9, b9;
  c2 = 2.0/15.0;
  c3 = 1.0/5.0;
  c4 = 3.0/10.0;
  c5 = 14.0/25.0;
  c6 = 19.0/25.0;
  c7 = 35226607.0/35688279.0;
  a21 = 2.0/15.0;
  a31 = 1.0/20.0;
  a32 = 3.0/20.0;
  a41 = 3.0/40.0;
  a43 = 9.0/40.0;
  a51 = 86727015.0/196851553.0;
  a52 = -60129073.0/52624712.0;
  a53 = 957436434.0/1378352377.0;
  a54 = 83886832.0/147842441.0;
  a61 = -86860849.0/45628967.0;
  a62 = 111022885.0/25716487.0;
  a63 = 108046682.0/101167669.0;
  a64 = -141756746.0/36005461.0;
  a65 = 73139862.0/60170633.0;
  a71 = 77759591.0/16096467.0;
  a72 = -49252809.0/6452555.0;
  a73 = -381680111.0/51572984.0;
  a74 = 879269579.0/66788831.0;
  a75 = -90453121.0/33722162.0;
  a76 = 111179552.0/157155827.0;
  a81 = 237564263.0/39280295.0;
  a82 = -100523239.0/10677940.0;
  a83 = -265574846.0/27330247.0;
  a84 = 317978411.0/18988713.0;
  a85 = -124494385.0/35453627.0;
  a86 = 86822444.0/100138635.0;
  a87 = -12873523.0/724232625.0;
  a91 = 17572349.0/289262523.0;
  a93 = 57513011.0/201864250.0;
  a94 = 15587306.0/354501571.0;
  a95 = 71783021.0/234982865.0;
  a96 = 29672000.0/180480167.0;
  a97 = 65567621.0/127060952.0;
  a98 = -79074570.0/210557597.0;
  b1 = 17572349.0/289262523.0;
  b3 = 57513011.0/201864250.0;
  b4 = 15587306.0/354501571.0;
  b5 = 71783021.0/234982865.0;
  b6 = 29672000.0/180480167.0;
  b7 = 65567621.0/127060952.0;
  b8 = -79074570.0/210557597.0;
  d1 = 15231665.0/510830334.0;
  d3 = 59452991.0/116050448.0;
  d4 = -28398517.0/122437738.0;
  d5 = 56673824.0/137010559.0;
  d6 = 68003849.0/426673583.0;
  d7 = 7097631.0/37564021.0;
  d8 = -71226429.0/583093742.0;
  d9 = 1.0/20.0;
  b9 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+1]=a82;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
}

static void rksTableauTp64(rksTableau &T) {
  const int s = 7; T.s = s; T.adaptive = 1; T.errOrder = 4;
  double a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, c2, c3, c4, c5, c6, b1, b3, b4, b5, b6, b7, d1, d3, d4, d5, d6, d7;
  a21 = 4.0 / 27.0;
  a31 = 1.0 / 18.0;
  a32 = 1.0 / 6.0;
  a41 = 66.0 / 343.0;
  a42 = -729.0 / 1372.0;
  a43 = 1053.0 / 1372.0;
  a51 = 13339.0 / 49152.0;
  a52 = -4617.0 / 16384.0;
  a53 = 5427.0 / 53248.0;
  a54 = 95207.0 / 159744.0;
  a61 = -6935.0 / 57122.0;
  a62 = 23085.0 / 48334.0;
  a63 = 33363360.0 / 273642941.0;
  a64 = 972160.0 / 118442467.0;
  a65 = 172687360.0 / 610434253.0;
  a71 = 611.0 / 1891.0;
  a72 = -4617.0 / 7564.0;
  a73 = 6041007.0 / 13176488.0;
  a74 = 12708836.0 / 22100117.0;
  a75 = -35840000.0 / 62461621.0;
  a76 = 6597591.0 / 7972456.0;
  c2 = 4.0 / 27.0;
  c3 = 2.0 / 9.0;
  c4 = 3.0 / 7.0;
  c5 = 11.0 / 16.0;
  c6 = 10.0 / 13.0;
  b1 = 131.0 / 1800.0;
  b3 = 1121931.0 / 3902080.0;
  b4 = 319333.0 / 1682928.0;
  b5 = 262144.0 / 2477325.0;
  b6 = 4084223.0 / 15177600.0;
  b7 = 1891.0 / 25200.0;
  d1 = 2694253.0 / 26100360.0;
  d3 = 83647323.0 / 535804360.0;
  d4 = 691202281.0 / 1789061040.0;
  d5 = -1275547648.0 / 10565208225.0;
  d6 = 2.0 / 5.0;
  d7 = 1891.0 / 25200.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
}

static void rksTableauV65r(rksTableau &T) {
  const int s = 9; T.s = s; T.adaptive = 1; T.errOrder = 5;
  double c2, c3, c4, c5, c6, c7, a21, a31, a32, a41, a43, a51, a53, a54, a61, a63, a64, a65, a71, a73, a74, a75, a76, a81, a83, a84, a85, a86, a87, a91, a92, a93, a94, a95, a96, a97, a98, b1, b4, b6, b7, b8, d1, d4, d5, d6, d8, d9, b5, b9, d7;
  c2 = .18;
  c3 = .1666666666666666666666666666666666666667;
  c4 = .25;
  c5 = .53;
  c6 = .6;
  c7 = .8;
  a21 = .18;
  a31 = .8950617283950617283950617283950617283951e-1;
  a32 = .7716049382716049382716049382716049382716e-1;
  a41 = .625e-1;
  a43 = .1875;
  a51 = .316516;
  a53 = -1.044948;
  a54 = 1.258432;
  a61 = .2723261273648562625722506556667430550251;
  a63 = -.8251336032388663967611336032388663967611;
  a64 = 1.048091767881241565452091767881241565452;
  a65 = .1047157079927685687367911796908817762840;
  a71 = -.1669941859971651431432960727896179733320;
  a73 = .6317085020242914979757085020242914979757;
  a74 = .1746104455277387608214675883848816179643;
  a75 = -1.066535645908606612252519473401868067778;
  a76 = 1.227210884353741496598639455782312925170;
  a81 = .3642375168690958164642375168690958164642;
  a83 = -.2040485829959514170040485829959514170040;
  a84 = -.3488373781606864313631230924464007170774;
  a85 = 3.261932303285686744333360874714258172905;
  a86 = -2.755102040816326530612244897959183673469;
  a87 = .6818181818181818181818181818181818181818;
  a91 = .7638888888888888888888888888888888888889e-1;
  a92 = 0;
  a93 = 0;
  a94 = .3694083694083694083694083694083694083694;
  a95 = 0;
  a96 = .2480158730158730158730158730158730158730;
  a97 = .2367424242424242424242424242424242424242;
  a98 = .6944444444444444444444444444444444444444e-1;
  b1 = .7638888888888888888888888888888888888889e-1;
  b4 = .3694083694083694083694083694083694083694;
  b6 = .2480158730158730158730158730158730158730;
  b7 = .2367424242424242424242424242424242424242;
  b8 = .6944444444444444444444444444444444444444e-1;
  d1 = .5870020964360587002096436058700209643606e-1;
  d4 = .4807256235827664399092970521541950113379;
  d5 = -.8534124207691908557883209486122831308356;
  d6 = 1.204648526077097505668934240362811791383;
  d8 = -.5924237307216030620285939434875605088371e-1;
  d9 = .1685804345378813463919846898570302825622;
  b5 = 0.0;
  b9 = 0.0;
  d7 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+1]=a92;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
}

static void rksTableauDverk65(rksTableau &T) {
  const int s = 8; T.s = s; T.adaptive = 1; T.errOrder = 5;
  double c2, c3, c4, c5, c7, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a81, a82, a83, a84, a85, a87, b1, b3, b4, b5, b7, b8, d1, d3, d4, d5, d6, b6, d7, d8;
  c2 = 1.0 / 6.0;
  c3 = 4.0 / 15.0;
  c4 = 2.0 / 3.0;
  c5 = 5.0 / 6.0;
  c7 = 1.0 / 15.0;
  a21 = 1.0      / 6.0;
  a31 = 4.0      / 75.0;
  a32 = 16.0     / 75.0;
  a41 = 5.0      / 6.0;
  a42 = -8.0     / 3.0;
  a43 = 5.0      / 2.0;
  a51 = -165.0   / 64.0;
  a52 = 55.0     / 6.0;
  a53 = -425.0   / 64.0;
  a54 = 85.0     / 96.0;
  a61 = 12.0     / 5.0;
  a62 = -8.0;
  a63 = 4015.0   / 612.0;
  a64 = -11.0    / 36.0;
  a65 = 88.0     / 255.0;
  a71 = -8263.0  / 15000.0;
  a72 = 124.0    / 75.0;
  a73 = -643.0   / 680.0;
  a74 = -81.0    / 250.0;
  a75 = 2484.0   / 10625.0;
  a81 = 3501.0   / 1720.0;
  a82 = -300.0   / 43.0;
  a83 = 297275.0 / 52632.0;
  a84 = -319.0   / 2322.0;
  a85 = 24068.0  / 84065.0;
  a87 = 3850.0   / 26703.0;
  b1 = 3.0   / 40.0;
  b3 = 875.0 / 2244.0;
  b4 = 23.0  / 72.0;
  b5 = 264.0 / 1955.0;
  b7 = 125.0 / 11592.0;
  b8 = 43.0  / 616.0;
  d1 = 13.0   / 160.0;
  d3 = 2375.0 / 5984.0;
  d4 = 5.0    / 16.0;
  d5 = 12.0   / 85.0;
  d6 = 3.0    / 44.0;
  b6 = 0.0;
  d7 = 0.0;
  d8 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[7*s+0]=a81;
  T.A[7*s+1]=a82;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+6]=a87;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
}

static void rksTableauTf65(rksTableau &T) {
  const int s = 9; T.s = s; T.adaptive = 1; T.errOrder = 5;
  double c2, c3, c4, c5, c6, c7, a21, a31, a32, a41, a43, a51, a53, a54, a61, a63, a64, a65, a71, a73, a74, a75, a76, a81, a83, a84, a85, a86, a87, a91, a94, a95, a96, a97, a98, b1, b4, b5, b6, b7, b8, d1, d4, d5, d6, d7, d8, d9, b9;
  c2 = 9.28961748633879781420765027322404371584699453551912568306010928961748633879781420765e-2;
  c3 = 1.44578313253012048192771084337349397590361445783132530120481927710843373493975903614e-1;
  c4 = 2.16867469879518072289156626506024096385542168674698795180722891566265060240963855422e-1;
  c5 = 5.68000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c6 = 7.11864406779661016949152542372881355932203389830508474576271186440677966101694915254e-1;
  c7 = 9.95000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  a21 = 9.28961748633879781420765027322404371584699453551912568306010928961748633879781420765e-2;
  a31 = 3.20715889781663863106572284887245651635599805316233039884555941697335052470690700435e-2;
  a32 = 1.12506724274845661882113855848624832426801465251509226132026333541109868246906833571e-1;
  a41 = 5.42168674698795180722891566265060240963855421686746987951807228915662650602409638554e-2;
  a43 = 1.62650602409638554216867469879518072289156626506024096385542168674698795180722891566e-1;
  a51 = 6.56598126617283950617283950617283950617283950617283950617283950617283950617283950617e-1;
  a53 = -2.4972770465185185185185185185185185185185185185185185185185185185185185185185185185e0;
  a54 = 2.40867891990123456790123456790123456790123456790123456790123456790123456790123456790e0;
  a61 = -1.7212338830272756350112289300338046256920059840520750991645177506321959252208613366e0;
  a63 = 7.22310751192021126636920155870658124799459070542113115974697356535677039490201029989e0;
  a64 = -5.4959191727393518000813340726320306436524484390690989489777480799761402436788850115e0;
  a65 = 7.05909950626077185672513986332135377282067107530551362971563451692243740099430963481e-1;
  a71 = 4.12867189716161527469018071566157538750756968750429124243842122794843113666860197270e0;
  a73 = -1.6914025304289438220281346004343739703895247887189472122417356767305063273262951589e1;
  a74 = 1.43289813217405868844323023574174855877382144987680899683823277400972814702596244738e1;
  a75 = -1.5533550903735309941352947451105003919180320410408681830389632181656533578398372284e0;
  a76 = 1.00472717576076705529415767637517912056749574195795909463557101742500402417456237099e0;
  a81 = 4.46860784284429599022330889710676757001193443328341367781153268526799285074027050787e0;
  a83 = -1.8345441869429650066499014099391358577583747335752433112108627235199546393109090809e1;
  a84 = 1.55237707233543199854527260736987323920859616527670939779256214402670438227794675418e1;
  a85 = -1.7228800213316940986071454499455146268823602061289992205388429896934101951354882736e0;
  a86 = 1.08151571740214517180934640319081789154027660669157008569533046194596896725782768317e0;
  a87 = -5.5723928394169823792218246594446491720651508606454087850143625880490525329866504633e-3;
  a91 = 6.42309093721083246173486105540744934898802830008152394733186013531402447815498986849e-2;
  a94 = 3.32861824699421109595374929094856793240150741775741341700388991831307262888094726546e-1;
  a95 = 2.67859229165778080498089104036336501876704241846808785499560813816788199727204331902e-1;
  a96 = 1.79863899670938711343662526258687438581919457377762276997401953795124106054682426881e-1;
  a97 = 1.51075784805762160255845148219838318965956604578826959670696217369891977986727762107e0;
  a98 = -1.3555737109658678286129266521423384168482207697893972403776325344952795933188090051e0;
  b1 = 6.42309093721083246173486105540744934898802830008152394733186013531402447815498986849e-2;
  b4 = 3.32861824699421109595374929094856793240150741775741341700388991831307262888094726546e-1;
  b5 = 2.67859229165778080498089104036336501876704241846808785499560813816788199727204331902e-1;
  b6 = 1.79863899670938711343662526258687438581919457377762276997401953795124106054682426881e-1;
  b7 = 1.51075784805762160255845148219838318965956604578826959670696217369891977986727762107e0;
  b8 = -1.3555737109658678286129266521423384168482207697893972403776325344952795933188090051e0;
  d1 = 6.22980954171238844288765383876964441563622264193747564518688416336360055072506207144e-2;
  d4 = 3.40203519635783669970356076069438053031579043631460538976870898652474922063170157204e-1;
  d5 = 2.35997541364108988248916956975923713134336326558729708330181566604979639253608698633e-1;
  d6 = 2.20640276396042514942275961685396046511990069015617783025687162561106239011510198458e-1;
  d7 = 1.15044133952868915480448599961883762022583301353216360785466101429534020477553812807e0;
  d8 = -1.0029141056750815457282448660706252103934340124906797279726028170808703439444111364e0;
  d9 = -6.6666666666666666666666666666666666666666666666666666666666666666666666666666666667e-3;
  b9 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
}

static void rksTableauTp75(rksTableau &T) {
  const int s = 9; T.s = s; T.adaptive = 1; T.errOrder = 5;
  double a21, a32, a41, a43, a51, a53, a54, a61, a63, a64, a65, a71, a73, a74, a75, a76, a81, a83, a84, a85, a86, a87, a91, a93, a94, a95, a96, a97, a98, c2, c3, c4, c5, c6, c7, c8, b1, b4, b5, b6, b7, b8, b9, d1, d4, d5, d6, d7, d8, d9;
  a21 = 1.0 / 18.0;
  a32 = 1.0 / 9.0;
  a41 = 1.0 / 24.0;
  a43 = 1.0 / 8.0;
  a51 = 2183971.0 / 4000000.0;
  a53 = -8340813.0 / 4000000.0;
  a54 = 3968421.0 / 2000000.0;
  a61 = 695768212.0 / 7463744411.0;
  a63 = -1803549175.0 / 7007942496.0;
  a64 = 3474507053.0 / 6790877290.0;
  a65 = 2188198899.0 / 15264927763.0;
  a71 = -11894934857.0 / 8390623634.0;
  a73 = 53094780276.0 / 9800512003.0;
  a74 = -8415376229.0 / 2277049503.0;
  a75 = -18647567697.0 / 10138317907.0;
  a76 = 27551494893.0 / 11905950217.0;
  a81 = 30828057951.0 / 7654644085.0;
  a83 = -4511704.0 / 324729.0;
  a84 = 16217851618.0 / 1651177175.0;
  a85 = 282768186839.0 / 40694064384.0;
  a86 = -104400780537.0 / 15869257619.0;
  a87 = 5409241639.0 / 9600177208.0;
  a91 = -133775720546.0 / 36753383835.0;
  a93 = 49608695511.0 / 4066590848.0;
  a94 = -59896475201.0 / 7901259813.0;
  a95 = -48035527651.0 / 5727379426.0;
  a96 = 86266718551.0 / 10188951048.0;
  a97 = -7751618114.0 / 23575802495.0;
  a98 = 2289274942.0 / 8464405725.0;
  c2 = 1.0 / 18.0;
  c3 = 1.0 / 9.0;
  c4 = 1.0 / 6.0;
  c5 = 89.0 / 200.0;
  c6 = 56482.0 / 115069.0;
  c7 = 74.0 / 95.0;
  c8 = 8.0 / 9.0;
  b1 = 597988726.0 / 12374436915.0;
  b4 = 3138312158.0 / 11968408119.0;
  b5 = 480882843.0 / 7850665645.0;
  b6 = 988558885.0 / 3512253271.0;
  b7 = 5302636961.0 / 26425940286.0;
  b8 = 1259489433.0 / 12163586030.0;
  b9 = 1016647712.0 / 23899101975.0;
  d1 = 1421940313.0 / 46193547077.0;
  d4 = 1943068601.0 / 5911217046.0;
  d5 = -3807140880.0 / 8205366359.0;
  d6 = 9377220888.0 / 11577671635.0;
  d7 = 586186883.0 / 5187186385.0;
  d8 = 1114095023.0 / 8014791121.0;
  d9 = 1016647712.0 / 23899101975.0;
  T.A[1*s+0]=a21;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
}

static void rksTableauTmy7s(rksTableau &T) {
  const int s = 10; T.s = s; T.adaptive = 1; T.errOrder = 6;
  double c2, c3, c4, c5, c6, c7, c8, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, a81, a82, a83, a84, a85, a86, a87, a91, a92, a93, a94, a95, a96, a97, a98, a101, a102, a103, a104, a105, a106, a107, a108, a109, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10;
  c2 = 8.86136910904712762297901616787065703474372205022359821121431028551771585827313381493e-2;
  c3 = 1.32920536635706914344685242518059855521155830753353973168214654282765737874097007224e-1;
  c4 = 1.99380804953560371517027863777089783281733746130030959752321981424148606811145510836e-1;
  c5 = 2.51937984496124031007751937984496124031007751937984496124031007751937984496124031008e-1;
  c6 = 2.26466433020936656065704702091900664344459244867723201401086840088319283348150590416e-2;
  c7 = 6.78260869565217391304347826086956521739130434782608695652173913043478260869565217391e-1;
  c8 = 7.66129032258064516129032258064516129032258064516129032258064516129032258064516129032e-1;
  a21 = 8.86136910904712762297901616787065703474372205022359821121431028551771585827313381493e-2;
  a31 = 3.32301341589267285861713106295149638802889576883384932920536635706914344685242518060e-2;
  a32 = 9.96904024767801857585139318885448916408668730650154798761609907120743034055727554180e-2;
  a41 = 4.98452012383900928792569659442724458204334365325077399380804953560371517027863777090e-2;
  a42 = 0;
  a43 = 1.49535603715170278637770897832817337461300309597523219814241486068111455108359133127e-1;
  a51 = 5.51347401967803154774903010020881505852005989293761620647741487589886184464539035017e-2;
  a52 = 0;
  a53 = 1.12885720141989326660141409395828560302843986115057948865980980005720662021597769579e-1;
  a54 = 8.39175241573543888701202275865794131429631668935503851932758789872287040280723579274e-2;
  a61 = 1.53780353659624250311801512696108169293868598553255672534718580953857479599369970368e-2;
  a62 = 0;
  a63 = 3.20168833857685908914047299444587333329182280459513826892095134179082200072969961132e-2;
  a64 = -4.2539659878681657073856959205351592650720670439267341724969304281964049361468337347e-2;
  a65 = 1.77913844290443067578425482004721088228615070247627119223966167775020097290494032391e-2;
  a71 = -1.5256873392554910892888395407304491271001202239387133944183533057047079677113113410e1;
  a72 = 0;
  a73 = -4.0883456759270366648261512467321799058186499039799239493858442836256593204975747971e-1;
  a74 = -1.3608204533866463335338410239697411205509650095546743972399822290598206098672910268e1;
  a75 = 1.01149044236967810023853975406104197029605187561685172742777704473133250229308176604e1;
  a76 = 1.98372689398825142836283710571516572858713290039459617328963432417380049457745287151e1;
  a81 = 1.88486645187560529586357925865538054589099172763864914361630595960178420222946575411e0;
  a82 = 0;
  a83 = 7.38723400736316245047913111275147442586352189973516108074150970202563979482282113515e-2;
  a84 = 8.04572070566500039801983801614661023094256190473761133022111517088062229540462605430e-1;
  a85 = 4.63664644831429482283531761249424920698038820301953813871953906173904815616778436120e-3;
  a86 = -2.2571634399585372828168228383264979022555589706647864119944165313232196821070240573e0;
  a87 = 2.55344963252550543952665407380963468836953509868134018667928934680410062297215831041e-1;
  a91 = -5.9346254587724754339983223484206523031910583834235199880213406063079393162409202809e0;
  a92 = 0;
  a93 = -7.8254437924926937330746887342046970213268979627548332452496248647606158812656995539e-2;
  a94 = -7.8190850697311404939022123036040717579732265786069442338599154407498020519983830030e0;
  a95 = 6.62838952616537145950452382452961751768395831246709307616126651109617352143730615816e0;
  a96 = 8.02308370728521564415778792242500347174153708508314015132305288664913153021463120273e0;
  a97 = -9.0428803055994289478315309000341323995405509411440606262203643609271327625942925961e-1;
  a98 = 1.08477976353789865635212288241556328190611363822218538947146933405275575165945217817e0;
  a101 = 1.62179253869109927468375569917595038904177242620112769247150828367984290149786137801e1;
  a102 = 0;
  a103 = 2.16664888484339819588806336947105292464689479004009761616228992822572035699934314119e-1;
  a104 = 1.28224265684564880927217994602681081201929120394991496951845578168120816842817479071e1;
  a105 = -8.1089672876336983196919059983906105838229524182068262549575894100211346860455654545e0;
  a106 = -2.0667128454690444724018643663830140813737619941924668651212413900366570540033662448e1;
  a107 = -3.1833603946776751327638844301283960383913589487275171654091584194061718372090669796e-1;
  a108 = 8.37414937940089897838775316258873698324382474489810241195049505895239674839838599165e-1;
  a109 = 0;
  b1 = -7.0509607576408896307075679328876855284603613449588923698038237210603268019606544968e-1;
  b2 = 0;
  b3 = 0;
  b4 = -1.3042081566303485225649874598063839519475921490266396703180477516076867108693171832e0;
  b5 = 1.48160311264599651661502935631784760949494268493436673049606673336731557372364280543e0;
  b6 = 1.06895075363674788262512061362504155168134693772218655812810426772234205050708971472e0;
  b7 = 6.13577483162844175682242729746843974632226625804493770700993518511008512922114250458e-2;
  b8 = 3.26912547604941692272822108612497316454980622616931033966225739519181388387272903080e-1;
  b9 = 7.04800701904669765545479015650816296991353756685952076379340312537795271551657846090e-2;
  b10 = 0;
  d1 = -3.6831507795991271832603276725031327096976142593866411295983627470495355912891936736e-1;
  d2 = 0;
  d3 = 0;
  d4 = -8.0950091191196753949937080390898589767029649084843059799077876945035271299089357243e-1;
  d5 = 1.08173849169813311928266834059595139857072621270843946078873270438439274085083834379e0;
  d6 = 6.13601703165021649508638707253091181092345706628708774027475501516795619531413661207e-1;
  d7 = 1.38925487324489763175454783958272954686752997010110778095994410501506567737198901272e-1;
  d8 = 2.68550307684235725858641739351983634290233000439835698038412427752611344000362033525e-1;
  d9 = 0;
  d10 = 7.50000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+1]=a82;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+1]=a92;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+1]=a102;
  T.A[9*s+2]=a103;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=b2;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.bhat[0]=d1;
  T.bhat[1]=d2;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
}

static void rksTableauV76r(rksTableau &T) {
  const int s = 10; T.s = s; T.adaptive = 1; T.errOrder = 6;
  double c2, c3, c4, c5, c6, c7, c8, a21, a31, a32, a41, a43, a51, a53, a54, a61, a63, a64, a65, a71, a73, a74, a75, a76, a81, a83, a84, a85, a86, a87, a91, a93, a94, a95, a96, a97, a98, a101, a103, a104, a105, a106, a107, b1, b4, b5, b6, b7, b8, b9, d1, d4, d5, d6, d7, d10, b10, d8, d9;
  c2 = .5e-2;
  c3 = .1088888888888888888888888888888888888889;
  c4 = .1633333333333333333333333333333333333333;
  c5 = .455;
  c6 = .6059617471462913245758145021744683294809;
  c7 = .835;
  c8 = .915;
  a21 = .5e-2;
  a31 = -1.076790123456790123456790123456790123457;
  a32 = 1.185679012345679012345679012345679012346;
  a41 = .4083333333333333333333333333333333333333e-1;
  a43 = .1225;
  a51 = .6360714285714285714285714285714285714286;
  a53 = -2.444464285714285714285714285714285714286;
  a54 = 2.263392857142857142857142857142857142857;
  a61 = -2.535121107934924522925638355466021548721;
  a63 = 10.29937465444926792043851446075602491361;
  a64 = -7.951303288599057994949321745826687653648;
  a65 = .7930114892310059220122601427111526182380;
  a71 = 1.001876581252463296196919658309499980821;
  a73 = -4.166571282442379833131393800547097145319;
  a74 = 3.834343292912864241255266521825137866520;
  a75 = -.5023333356071084754746433022861176561240;
  a76 = .6676847438841607711538509226985769541026;
  a81 = 27.25501835463076713033396381917500571735;
  a83 = -42.00461727841063835531864544390929536961;
  a84 = -10.53571312661948991792108160054652610372;
  a85 = 80.49553671141193714798365215892682663420;
  a86 = -67.34388227179051346854907596321297564093;
  a87 = 13.04865761077793746347118702956696476271;
  a91 = -3.039737805711496514694365865875576322688;
  a93 = 10.13816141032980111185794619070970015044;
  a94 = -6.429305674864721572146282562955529806444;
  a95 = -1.586437148340827658711531285379861057947;
  a96 = 1.892178184196842441086430890913135336502;
  a97 = .1969933540760886906129236016333644283801e-1;
  a98 = .5441698982793323546510272424795257297790e-2;
  a101 = -1.444951891677773513735100317935571236052;
  a103 = 8.031891385995591922411703322301956043504;
  a104 = -7.583174166340134682079888302367158860498;
  a105 = 3.581616935319007421124768544245287869686;
  a106 = -2.436972263219952941118380906569375238373;
  a107 = .8515899999232617933968976603248614217339;
  b1 = .4742583783370675608356917271757453469893e-1;
  b4 = .2562236165937056265996172745827462344816;
  b5 = .2695137683307420661947381725807595288676;
  b6 = .1268662240909278284598913836473917324788;
  b7 = .2488722594206007162204644942764749276729;
  b8 = .3074483740820063133530438847909918476864e-2;
  b9 = .4802380998949694330818906334714312332321e-1;
  d1 = .4748524769929963103753127380572796155227e-1;
  d4 = .2559941258869063329715491824590539387050;
  d5 = .2705847808106768872253089109926813573239;
  d6 = .1250561868442599291363882232374691792045;
  d7 = .2520446872374386050718404382019744256218;
  d10 = .4883497152141861455738197130309313759259e-1;
  b10 = 0.0;
  d8 = 0.0;
  d9 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+2]=a103;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
}

static void rksTableauSs76(rksTableau &T) {
  const int s = 11; T.s = s; T.adaptive = 1; T.errOrder = 6;
  double c2, c3, c4, c5, c6, c7, c8, c9, a21, a31, a32, a41, a43, a51, a53, a54, a61, a63, a64, a65, a71, a73, a74, a75, a76, a81, a83, a84, a85, a86, a87, a91, a93, a94, a95, a96, a97, a98, a101, a103, a104, a105, a106, a107, a108, a109, a111, a113, a114, a115, a116, a117, a118, a119, b1, b5, b6, b7, b8, b9, b10, d1, d5, d6, d7, d8, d9, d11, b11, d10;
  c2 = 2.00000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  c3 = 2.16000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c4 = 4.10000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c5 = 5.70000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c6 = 8.60000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c7 = 1.89734415567554714447412739459850829747319955773081476233291234325271903458210730096e-1;
  c8 = 7.20000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c9 = 8.33333333333333333333333333333333333333333333333333333333333333333333333333333333333e-1;
  a21 = 2.00000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  a31 = -9.5040000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  a32 = 1.16640000000000000000000000000000000000000000000000000000000000000000000000000000000e0;
  a41 = 2.08796296296296296296296296296296296296296296296296296296296296296296296296296296296e-2;
  a43 = 3.89120370370370370370370370370370370370370370370370370370370370370370370370370370370e-1;
  a51 = 1.18750000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  a53 = 1.16301546391752577319587628865979381443298969072164948453608247422680412371134020619e-1;
  a54 = 3.34948453608247422680412371134020618556701030927835051546391752577319587628865979381e-1;
  a61 = -1.5500000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  a63 = 1.26419952627798163356436989147106218572232900382472285320441881685984429300871726173e0;
  a64 = -1.4923539518900343642611683848797250859106529209621993127147766323024054982817869416e0;
  a65 = 1.24315442561205273069679849340866290018832391713747645951035781544256120527306967985e0;
  a71 = 5.32404818460847244880465705884559948010013316415508489398809027494008261005225044158e-2;
  a73 = 2.63190905005944241395058267679896015089060637096436681624641838124193103498139523636e-1;
  a74 = -2.2992382584611251312628854152112504080421217996536744225053579460154724416916797118e-1;
  a75 = 1.15020646834503298021503942059822317395934020502096307719511499416916110119759292822e-1;
  a76 = -1.1793792272865036330907499347198456734463853501634919800207211363690892091042619602e-2;
  a81 = 1.01833250525779410925056563242748813162631092358712093910139906641055533787858600752e-1;
  a83 = 3.67650402264021539690171893675768442020160413827130555257229267948121503927894530089e0;
  a84 = -1.3855064886371957262483408369918407532890732638371784299323099278110242610274153464e0;
  a85 = 1.10585536543242802574001937437432027003273412482516272090263172771172002666187126910e0;
  a86 = -1.0442483660597752261508675907091877183025101004713070424083201191960255004658089509e-1;
  a87 = -2.6742613133552495847033672783119939782776450815708712332119223741033637886546789293e0;
  a91 = -7.0447278725441551253173322525085182085211898876535016082680181826378260058107213334e-2;
  a93 = 2.57849022691031881978588351318784339446713208794819222430391001640703602812013523694e0;
  a94 = -1.7350648742705534137364675443051006373149953263240864527861457323256158060409724050e0;
  a95 = 1.42076925842656275743055431879937793726979032667289381452783078677285932226590451543e0;
  a96 = -2.9994002356151422690421464715731127083992859674624280646321971569589440552603013442e-2;
  a97 = -1.2738969184577295296576255258337856595600446902141816152083573928383427523639249936e0;
  a98 = -5.6523078193672326545416641274185392359344306198325340774902191286635758037098793739e-2;
  a101 = 1.12188929566497595110309434330843411185625863434326301999225595935353619019786372693e-1;
  a103 = -9.4478179712463597825861610011719624575041188851623316508747159935931144002457572402e0;
  a104 = 4.72928685969506458483541566908828462557623316160618665528745761537386487691574498692e0;
  a105 = -2.1535862973784519387089628138790863673524951190739517405566618338182156484450238408e0;
  a106 = -1.5003176034455324417891524003264793431625218068048301751161484381435733494665524146e0;
  a107 = 7.32228493516381472239660644916433114018057892752031963313864026830512965942191992469e0;
  a108 = -1.0171881348813826762245691938848731969668479025856205360601503764094927919654990399e0;
  a109 = 2.95514928252634993696651385667894218804354576106590151218235316235004803476538125126e0;
  a111 = 1.26777165953424622263709262280041924195783046360695933368685779852909039382377368327e-2;
  a113 = -1.0069960582593041001624297555860950454903273549350063548770708526802347773053466643e1;
  a114 = 4.62534242829213472690316528870584469183038900200033492988915271866480429986075212694e0;
  a115 = -2.1610283337552771303871477895705714199299596209426922872912146988024763221546690181e0;
  a116 = -1.6781218510451494897112741151843512565528970972689550849978046265492823347869148500e0;
  a117 = 8.10720558296219407284479196100506119564876298707858160850972380908658055161496706221e0;
  a118 = -9.9034046697481677826772383527914286511512256496144470729835169541686418859152250889e-1;
  a119 = 3.15422550651861313801611511995610591660252253880816949662233444183429486317261609431e0;
  b1 = 5.49699932022185175241442181883597507655769828815946565807124447154669050002194891238e-2;
  b5 = 5.27017899545901255716693676497458451526562246965950291388111745594043275344112358966e-1;
  b6 = -5.7640010460253178146313005308307447324854309728047707823605247362054750175374841292e-1;
  b7 = 2.99473766075857495911574410718252874676717226006797736321538429237884690787951256718e-1;
  b8 = -4.3511367243178106005606352407152054011086352330218025896985334826437712480556570901e-1;
  b9 = 1.06972120095994204935876677034564950812746194295206963084848042699967266056102464470e0;
  b10 = 6.03309172503935230080145014048744282630882217762450220670627753378570948660063724257e-2;
  d1 = 5.51948596604973952360558600324258025823235723557842084969195331387651217650789597054e-2;
  d5 = 5.38771472959210564343392307526371633391618262286599376534904900758161647057057668278e-1;
  d6 = -7.2002415738222743602899270061101901552733320251540477745765896611188358469552535128e-1;
  d7 = 2.98535385184885222079230265390696358266728601821898916299256942162002807672289159564e-1;
  d8 = -4.8443496217457505118337730236228616963449840154387472665426235404619128753787634184e-1;
  d9 = 1.24611378035303235082118128195796776952198421286248671471499632269996834100646561750e0;
  d11 = 6.58436213991769547325102880658436213991769547325102880658436213991769547325102880658e-2;
  b11 = 0.0;
  d10 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+2]=a103;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+2]=a113;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
}

static void rksTableauV78(rksTableau &T) {
  const int s = 13; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double b1, b6, b7, b8, b9, b10, b11, c2, c3, c4, c5, c6, c7, c8, c9, c10, c12, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a85, a86, a87, a91, a94, a95, a96, a97, a98, a101, a104, a105, a106, a107, a108, a109, a111, a114, a115, a116, a117, a118, a119, a1110, a121, a124, a125, a126, a127, a128, a129, a1210, a131, a134, a135, a136, a137, a138, a139, a1310, a1312, d1, d6, d7, d8, d9, d10, d11, d12, d13, b12, b13;
  b1 = 13.0    / 288.0;
  b6 = 32.0    / 125.0;
  b7 = 31213.0 / 144000.0;
  b8 = 2401.0  / 12375.0;
  b9 = 1701.0  / 14080.0;
  b10 = 2401.0  / 19200.0;
  b11 = 19.0    / 450.0;
  c2 = 1.0 / 4.0;
  c3 = 1.0 / 12.0;
  c4 = 1.0 / 8.0;
  c5 = 2.0 / 5.0;
  c6 = 1.0 / 2.0;
  c7 = 6.0 / 7.0;
  c8 = 1.0 / 7.0;
  c9 = 2.0 / 3.0;
  c10 = 2.0 / 7.0;
  c12 = 1.0 / 3.0;
  a31 = 5.0       / 72.0;
  a32 = 1.0       / 72.0;
  a41 = 1.0       / 32.0;
  a43 = 3.0       / 32.0;
  a51 = 106.0     / 125.0;
  a53 = -408.0     / 125.0;
  a54 = 352.0     / 125.0;
  a61 = 1.0       / 48.0;
  a64 = 8.0       / 33.0;
  a65 = 125.0     / 528.0;
  a71 = -13893.0   / 26411.0;
  a74 = 39936.0   / 26411.0;
  a75 = -64125.0   / 26411.0;
  a76 = 60720.0   / 26411.0;
  a81 = 37.0      / 392.0;
  a85 = 1625.0    / 9408.0;
  a86 = -2.0       / 15.0;
  a87 = 61.0      / 6720.0;
  a91 = 17176.0   / 25515.0;
  a94 = -47104.0   / 25515.0;
  a95 = 1325.0    / 504.0;
  a96 = -41792.0   / 25515.0;
  a97 = 20237.0   / 145800.0;
  a98 = 4312.0    / 6075.0;
  a101 = -23834.0   / 180075.0;
  a104 = -77824.0   / 1980825.0;
  a105 = -636635.0  / 633864.0;
  a106 = 254048.0  / 300125.0;
  a107 = -183.0     / 7000.0;
  a108 = 8.0       / 11.0;
  a109 = -324.0     / 3773.0;
  a111 = 12733.0   / 7600.0;
  a114 = -20032.0   / 5225.0;
  a115 = 456485.0  / 80256.0;
  a116 = -42599.0   / 7125.0;
  a117 = 339227.0  / 912000.0;
  a118 = -1029.0    / 4180.0;
  a119 = 1701.0    / 1408.0;
  a1110 = 5145.0    / 2432.0;
  a121 = -27061.0   / 204120.0;
  a124 = 40448.0   / 280665.0;
  a125 = -1353775.0 / 1197504.0;
  a126 = 17662.0   / 25515.0;
  a127 = -71687.0   / 1166400.0;
  a128 = 98.0      / 225.0;
  a129 = 1.0       / 16.0;
  a1210 = 3773.0    / 11664.0;
  a131 = 11203.0   / 8680.0;
  a134 = -38144.0   / 11935.0;
  a135 = 2354425.0 / 458304.0;
  a136 = -84046.0   / 16275.0;
  a137 = 673309.0  / 1636800.0;
  a138 = 4704.0    / 8525.0;
  a139 = 9477.0    / 10912.0;
  a1310 = -1029.0    / 992.0;
  a1312 = 729.0     / 341.0;
  d1 = b1 - (-6600.0   / 3168000.0);
  d6 = b6 - (-135168.0 / 3168000.0);
  d7 = b7 - (-14406.0  / 3168000.0);
  d8 = b8 - (57624.0  / 3168000.0);
  d9 = b9 - (54675.0  / 3168000.0);
  d10 = b10 - (-396165.0 / 3168000.0);
  d11 = b11 - (-133760.0 / 3168000.0);
  d12 = -(437400.0 / 3168000.0);
  d13 = -(136400.0 / 3168000.0);
  b12 = 0.0;
  b13 = 0.0;
  T.A[1*s+0]=c2;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[12*s+0]=a131;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+11]=a1312;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
}

static void rksTableauDverk78(rksTableau &T) {
  const int s = 13; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a84, a85, a86, a87, a91, a94, a95, a96, a97, a98, a101, a104, a105, a106, a107, a108, a109, a111, a114, a115, a116, a117, a118, a119, a1110, a121, a124, a125, a126, a127, a128, a129, a1210, a1211, a131, a134, a135, a136, a137, a138, a139, a1310, b1, b6, b7, b8, b9, b10, b11, b12, d1, d6, d7, d8, d9, d10, d13, b13, d11, d12;
  c2 = 6.25000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  c3 = 1.05164319248826291079812206572769953051643192488262910798122065727699530516431924883e-1;
  c4 = 1.57746478873239436619718309859154929577464788732394366197183098591549295774647887324e-1;
  c5 = 3.90000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c6 = 4.66666666666666666666666666666666666666666666666666666666666666666666666666666666667e-1;
  c7 = 1.56000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c8 = 9.60000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c9 = 8.92267261582876569617601634626813346131102801365312428359450025561131899210951871087e-1;
  c10 = 9.16666666666666666666666666666666666666666666666666666666666666666666666666666666667e-1;
  c11 = 9.50000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  a21 = 6.25000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  a31 = 1.66880469042738433732284158786836826908241310145694196477771165333157001476779298640e-2;
  a32 = 8.84762723445524477065837906940862703608190614736934911503449491943838303687539950186e-2;
  a41 = 3.94366197183098591549295774647887323943661971830985915492957746478873239436619718310e-2;
  a43 = 1.18309859154929577464788732394366197183098591549295774647887323943661971830985915493e-1;
  a51 = 3.76656086575255102040816326530612244897959183673469387755102040816326530612244897959e-1;
  a53 = -1.4062762954400510204081632653061224489795918367346938775510204081632653061224489796e0;
  a54 = 1.41962020886479591836734693877551020408163265306122448979591836734693877551020408163e0;
  a61 = 4.78355492244381133270022158911047799936688825577714466603355492244381133270022158911e-2;
  a64 = 2.34464677432637812666112645898374622854105368760715435467810644685518346322874319640e-1;
  a65 = 1.84366440009590740673551804877187263818892415348179784538520472756710207016790131136e-1;
  a71 = 6.11454257142857142857142857142857142857142857142857142857142857142857142857142857143e-2;
  a74 = 1.12966312083735007492797513828957773063162531957288289301243638495099728858422070298e-1;
  a75 = -3.6523126216152081630500698710681045165713080391277981385292799325019115669575763968e-2;
  a76 = 1.84113884181313598519888991674375578168362627197039777983348751156336725254394079556e-2;
  a81 = -1.1290190344812819716873002143964435191540855755186649222833121580618541079059519076e0;
  a84 = -1.4000000000000000000000000000000000000000000000000000000000000000000000000000000000e0;
  a85 = -8.8802928704648604017469555116668654703824580186941491818867912879961932084706688647e0;
  a86 = 7.13094409677757261060034674920714023333661544255001489279410558828850531341195834447e0;
  a87 = 5.23836780816856976283390897685616875619992815166279921137599785776954200296466242780e0;
  a91 = -6.1529353321310240531758577059630882789056716837277145620689461335329804789378294402e-1;
  a94 = -4.9518730627307206123980419770218646265183975456624504456109720877843836846893436386e0;
  a95 = -5.1425568821223535028317455499886772909974359615422348128021564911695375075220376057e0;
  a96 = 4.40119257638430388133537282166699561228064232903290937377378080578921781089684014377e0;
  a97 = 7.19135317060505642064565222344470796214151883455022732703393848995802890190295274383e0;
  a98 = 9.44499265969278818394988712196051711534231335963244217175392212110442651632317187457e-3;
  a101 = -8.0487754950012651630611680571217899707181954913548370272898245110488470451176167195e-1;
  a104 = -4.5929131940948324444453893585077961341346223390010271981133375952914577811961889600e0;
  a105 = -6.5344796429871976349685844664568433689437520794427587560502836549097348190068258330e0;
  a106 = 5.43653721645848061969301952101216405498569628561398967433445354186586889917076802846e0;
  a107 = 7.41428376786239108774181233597389231941925460585428819853117348548056293090948270879e0;
  a108 = 1.62969198761492907780853867652646563859111189149676578185954495159286718271369038020e-2;
  a109 = -1.8180850948197735826159946407835863974001376137309207124952108889616530525944509467e-2;
  a111 = -1.0578581056602630497010123299247635673755482083807816057707586430609366121794742310e0;
  a114 = -2.4902747870066666124482517838284554072812073092990236244911099705917862954551978618e0;
  a115 = -8.3678653641187673586734269894057729098141483833620036736491918565911809345813949287e0;
  a116 = 6.76706391372510233295677955943576702708643348087685990605145394724880397320088890883e0;
  a117 = 6.10187575754750708348558966187981017887077884162790182554024225466956680755366458148e0;
  a118 = 2.26346926591268273851441242963489920479522697011540597935824660244066631601010686288e-2;
  a119 = 2.42115288970666032926841417691368020854458954458812827759456719481984746605406648566e-2;
  a1110 = -4.9787636043105826297506384222071115619706586609988170250163869647072076359128202386e-2;
  a121 = -2.2141388211521594228089507244618610021434452510302869093059447809375117564415940198e0;
  a124 = 4.56355804724646925572212018240182661579238223094527486516681347955670181620817635015e0;
  a125 = -1.6957036205517104297839745395366915857642603514682315438023365324576286639172315989e1;
  a126 = 1.31105217928641421165649661367441373070008285256157591924114546268073486094645142906e1;
  a127 = 2.65774224749870274100467824154490008698121216459786964112414318137911711432599035412e0;
  a128 = 2.74592934104436868684638073474053713826464018949259425746847719273619536662550065244e0;
  a129 = -1.1506549223705344448192637136257715209341884980498719927919640966806759385051562199e0;
  a1210 = 2.42302296121448799919236503348925313670760208048083006656846347016299049952947649144e0;
  a1211 = -4.1789444408283726338625504954661059040264279273698536826180777484478790720345919102e0;
  a131 = -1.5511316950026916120467765404925329008269305742185819600030671406615107733844590777e0;
  a134 = 5.46092722623409081818657767842421731964760251560133825662023106815177868484348803926e0;
  a135 = -1.1979220224305761165813607594501926859816750798269770560276587319873047174682994374e1;
  a136 = 9.35213287509985895980770600293832447863564422228774591854591472707049491657923415773e0;
  a137 = -2.6805151435786875122787398112144651989157843381870119309573397987877093382768663729e-1;
  a138 = -2.9111005885695086970119200745004024552717605363363761755145270197855298352241438457e-1;
  a139 = -5.7342608212997377743524954389733665898828809950550624616346021546025114366487668036e-1;
  a1310 = 8.49879473319296398230415986100741386767477221557113401924155562629859407659708957311e-1;
  b1 = 4.46288489653070217415613140881920071523054807312072680684782090651440586079475760217e-2;
  b6 = 3.53849672808056955006078714962021325996588002624194340841646462152138105292559006658e-1;
  b7 = 2.49214137824034653157413522433245474576906451975957952284766070502834548257871008029e-1;
  b8 = -5.2283384709802500404740481515533751847798124016472832324227905218125681954020960889e0;
  b9 = 5.15116549236861223325928760078465842871662817825128618469086436536651971244098141329e0;
  b10 = -9.5097928721643673005225329855637374707813853242252723834130793535769142379902016318e0;
  b11 = 1.00596341373431488638526005247320813694813862000943570400067314269066845716409977948e1;
  b12 = -1.2036094616454238602036053988308595036261658780444717005661665860383856284805907814e-1;
  d1 = 4.46938336974745394582507450063655006984853368676139647920294768627190144403875012741e-2;
  d6 = 3.54849078534857257151115499384161010948488673442599070019549247826104344436599329295e-1;
  d7 = 2.48972827546254539183557734416986167348192069231072959065041272432884166102268412123e-1;
  d8 = 2.66563173304884358413831674493708561666064947673721173917129352743937890031432599813e0;
  d9 = 3.92326394871838972220877485800106547858756064671589732933492272245546934442704232603e0;
  d10 = -5.6804277609880002002190441358794913525295674525389817820177718907811376030065962976e0;
  d13 = -5.5698366055781944192097144586617242171380875045541328036506435623541816671402726928e-1;
  b13 = 0.0;
  d11 = 0.0;
  d12 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
}

static void rksTableauDp85(rksTableau &T) {
  const int s = 12; T.s = s; T.adaptive = 1; T.errOrder = 5;
  double c7, c8, c9, c10, c11, c6, c5, c4, c3, c2, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a84, a85, a86, a87, a91, a94, a95, a96, a97, a98, a101, a104, a105, a106, a107, a108, a109, a111, a114, a115, a116, a117, a118, a119, a1110, a121, a124, a125, a126, a127, a128, a129, a1210, a1211, b1, b6, b7, b8, b9, b10, b11, b12, d1, d6, d7, d8, d9, d10, d11, d12;
  c7 = 1.0 / 4.0;
  c8 = 4.0 / 13.0;
  c9 = 127.0 / 195.0;
  c10 = 3.0 / 5.0;
  c11 = 6.0 / 7.0;
  c6 = 4.0 / 3.0 * c7;
  c5 = (6.0 + std::sqrt(6.0)) / 10.0 * c6;
  c4 = (6.0 - std::sqrt(6.0)) / 10.0 * c6;
  c3 = 2.0 / 3.0 * c4;
  c2 = 2.0 / 3.0 * c3;
  a21 = 5.26001519587677318785587544488e-2;
  a31 = 1.97250569845378994544595329183e-2;
  a32 = 5.91751709536136983633785987549e-2;
  a41 = 2.95875854768068491816892993775e-2;
  a43 = 8.87627564304205475450678981324e-2;
  a51 = 2.41365134159266685502369798665e-1;
  a53 = -8.84549479328286085344864962717e-1;
  a54 = 9.24834003261792003115737966543e-1;
  a61 = 3.7037037037037037037037037037e-2;
  a64 = 1.70828608729473871279604482173e-1;
  a65 = 1.25467687566822425016691814123e-1;
  a71 = 3.7109375e-2;
  a74 = 1.70252211019544039314978060272e-1;
  a75 = 6.02165389804559606850219397283e-2;
  a76 = -1.7578125e-2;
  a81 = 3.70920001185047927108779319836e-2;
  a84 = 1.70383925712239993810214054705e-1;
  a85 = 1.07262030446373284651809199168e-1;
  a86 = -1.53194377486244017527936158236e-2;
  a87 = 8.27378916381402288758473766002e-3;
  a91 = 6.24110958716075717114429577812e-1;
  a94 = -3.36089262944694129406857109825;
  a95 = -8.68219346841726006818189891453e-1;
  a96 = 2.75920996994467083049415600797e1;
  a97 = 2.01540675504778934086186788979e1;
  a98 = -4.34898841810699588477366255144e1;
  a101 = 4.77662536438264365890433908527e-1;
  a104 = -2.48811461997166764192642586468e0;
  a105 = -5.90290826836842996371446475743e-1;
  a106 = 2.12300514481811942347288949897e1;
  a107 = 1.52792336328824235832596922938e1;
  a108 = -3.32882109689848629194453265587e1;
  a109 = -2.03312017085086261358222928593e-2;
  a111 = -9.3714243008598732571704021658e-1;
  a114 = 5.18637242884406370830023853209e0;
  a115 = 1.09143734899672957818500254654e0;
  a116 = -8.14978701074692612513997267357e0;
  a117 = -1.85200656599969598641566180701e1;
  a118 = 2.27394870993505042818970056734e1;
  a119 = 2.49360555267965238987089396762e0;
  a1110 = -3.0467644718982195003823669022e0;
  a121 = 2.27331014751653820792359768449e0;
  a124 = -1.05344954667372501984066689879e1;
  a125 = -2.00087205822486249909675718444e0;
  a126 = -1.79589318631187989172765950534e1;
  a127 = 2.79488845294199600508499808837e1;
  a128 = -2.85899827713502369474065508674e0;
  a129 = -8.87285693353062954433549289258e0;
  a1210 = 1.23605671757943030647266201528e1;
  a1211 = 6.43392746015763530355970484046e-1;
  b1 = 5.42937341165687622380535766363e-2;
  b6 = 4.45031289275240888144113950566;
  b7 = 1.89151789931450038304281599044;
  b8 = -5.8012039600105847814672114227;
  b9 = 3.1116436695781989440891606237e-1;
  b10 = -1.52160949662516078556178806805e-1;
  b11 = 2.01365400804030348374776537501e-1;
  b12 = 4.47106157277725905176885569043e-2;
  d1 = b1  - 0.1312004499419488073250102996e-01;
  d6 = b6  - (-0.1225156446376204440720569753e+01);
  d7 = b7  - (-0.4957589496572501915214079952);
  d8 = b8  - 0.1664377182454986536961530415e+01;
  d9 = b9  - (-0.3503288487499736816886487290);
  d10 = b10 - 0.3341791187130174790297318841;
  d11 = b11 - 0.8192320648511571246570742613e-01;
  d12 = b12 - (-0.2235530786388629525884427845e-01);
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
}

static void rksTableauTp86(rksTableau &T) {
  const int s = 12; T.s = s; T.adaptive = 1; T.errOrder = 6;
  double a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a84, a85, a86, a87, a91, a94, a95, a96, a97, a98, a101, a104, a105, a106, a107, a108, a109, a111, a114, a115, a116, a117, a118, a119, a1110, a121, a124, a125, a126, a127, a128, a129, a1210, a1211, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, b1, b6, b7, b8, b9, b10, b11, b12, d1, d6, d7, d8, d9, d10, d11, d12;
  a21 = 9.0 /142.0;
  a31 = 178422123.0 /9178574137.0;
  a32 = 685501333.0 /8224473205.0;
  a41 = 12257.0 /317988.0;
  a43 = 12257.0 /105996.0;
  a51 = 2584949729.0 /6554704252.0;
  a53 = -9163901916.0 /6184003973.0;
  a54 = 26222057794.0 /17776421907.0;
  a61 = 4418011.0 /96055225.0;
  a64 = 2947922107.0 /12687381736.0;
  a65 = 3229973413.0 /17234960414.0;
  a71 = 2875139539.0 /47877267651.0;
  a74 = 2702377211.0 /24084535832.0;
  a75 = -135707089.0 /4042230341.0;
  a76 = 299874140.0 /17933325691.0;
  a81 = -7872176137.0 /5003514694.0;
  a84 = -35136108789.0 /26684798878.0;
  a85 = -114433184681.0 /9760995895.0;
  a86 = 299204996517.0 /32851421233.0;
  a87 = 254.0 /39.0;
  a91 = -3559950777.0 /7399971898.0;
  a94 = -29299291531.0 /4405504148.0;
  a95 = -42434013379.0 /9366905709.0;
  a96 = 20642871700.0 /5300635453.0;
  a97 = 12951197050.0 /1499985011.0;
  a98 = 59527523.0 /6331620793.0;
  a101 = -8196723582.0 /10570795981.0;
  a104 = -46181454005.0 /5775132776.0;
  a105 = -196277106011.0 /29179424052.0;
  a106 = 63575135343.0 /11491868333.0;
  a107 = 120535663067.0 /11060780187.0;
  a108 = 195434294.0 /9727139945.0;
  a109 = -617468037.0 /15757346105.0;
  a111 = -6373809055.0 /5357779452.0;
  a114 = -150772749657.0 /21151088080.0;
  a115 = -58076657383.0 /6089469394.0;
  a116 = 9252721190.0 /1221566797.0;
  a117 = 132381309631.0 /11748965576.0;
  a118 = 704633904.0 /13813696331.0;
  a119 = 656417033.0 /8185349658.0;
  a1110 = -1669806516.0 /10555289849.0;
  a121 = -2726346953.0 /6954959789.0;
  a124 = 24906446731.0 /6359105161.0;
  a125 = -65277767625.0 /23298960463.0;
  a126 = 39128152317.0 /16028215273.0;
  a127 = -40638357893.0 /16804059016.0;
  a128 = -7437361171.0 /21911114743.0;
  a129 = 1040125706.0 /5334949109.0;
  a1210 = -1129865134.0 /5812907645.0;
  a1211 = 6253441118.0 /10543852725.0;
  c2 = 9.0 / 142.0;
  c3 = 24514.0 / 238491.0;
  c4 = 12257.0 / 79497.0;
  c5 = 50.0 / 129.0;
  c6 = 34.0 / 73.0;
  c7 = 23.0 / 148.0;
  c8 = 142.0 / 141.0;
  c9 = 14183175345.0 / 16188232343.0;
  c10 = 83.0 / 91.0;
  c11 = 143.0 / 149.0;
  b1 = 438853193.0 / 9881496838.0;
  b6 = 11093525429.0 / 31342013414.0;
  b7 = 481311443.0 / 1936695762.0;
  b8 = -3375294558.0 / 10145424253.0;
  b9 = 9830993862.0 / 5116981057.0;
  b10 = -138630849943.0 / 50747474617.0;
  b11 = 7152278206.0 / 5104393345.0;
  b12 = 5118195927.0 / 53798651926.0;
  d1 = 289283091.0 / 6008696510.0;
  d6 = 3034152487.0 / 7913336319.0;
  d7 = 7170564158.0 / 30263027435.0;
  d8 = 7206303747.0 / 16758195910.0;
  d9 = -1059739258.0 / 8472387467.0;
  d10 = 16534129531.0 / 11550853505.0;
  d11 = -3.0 / 2.0;
  d12 = 5118195927.0 / 53798651926.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
}

static void rksTableauV87e(rksTableau &T) {
  const int s = 13; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a84, a85, a86, a87, a91, a94, a95, a96, a97, a98, a101, a104, a105, a106, a107, a108, a109, a111, a114, a115, a116, a117, a118, a119, a1110, a121, a124, a125, a126, a127, a128, a129, a1210, a1211, a131, a134, a135, a136, a137, a138, a139, a1310, b1, b6, b7, b8, b9, b10, b11, b12, d1, d6, d7, d8, d9, d10, d13, b13, d11, d12;
  c2 = .5e-1;
  c3 = .1065625;
  c4 = .15984375;
  c5 = .39;
  c6 = .465;
  c7 = .155;
  c8 = .943;
  c9 = .9018020417358569582597079406783721499560;
  c10 = .909;
  c11 = .94;
  a21 = .5e-1;
  a31 = -.69931640625e-2;
  a32 = .1135556640625;
  a41 = .399609375e-1;
  a43 = .1198828125;
  a51 = .3613975628004575124052940721184028345129;
  a53 = -1.341524066700492771819987788202715834917;
  a54 = 1.370126503900035259414693716084313000404;
  a61 = .4904720279720279720279720279720279720280e-1;
  a64 = .2350972042214404739862988335493427143122;
  a65 = .1808555929813567288109039636534544884850;
  a71 = .6169289044289044289044289044289044289044e-1;
  a74 = .1123656831464027662262557035130015442303;
  a75 = -.3885046071451366767049048108111244567456e-1;
  a76 = .1979188712522045855379188712522045855379e-1;
  a81 = -1.767630240222326875735597119572145586714;
  a84 = -62.5;
  a85 = -6.061889377376669100821361459659331999758;
  a86 = 5.650823198222763138561298030600840174201;
  a87 = 65.62169641937623283799566054863063741227;
  a91 = -1.180945066554970799825116282628297957882;
  a94 = -41.50473441114320841606641502701994225874;
  a95 = -4.434438319103725011225169229846100211776;
  a96 = 4.260408188586133024812193710744693240761;
  a97 = 43.75364022446171584987676829438379303004;
  a98 = .7871425489912310687446475044226307550860e-2;
  a101 = -1.281405999441488405459510291182054246266;
  a104 = -45.04713996013986630220754257136007322267;
  a105 = -4.731362069449576477311464265491282810943;
  a106 = 4.514967016593807841185851584597240996214;
  a107 = 47.44909557172985134869022392235929015114;
  a108 = .1059228297111661135687393955516542875228e-1;
  a109 = -.5746842263844616254432318478286296232021e-2;
  a111 = -1.724470134262485191756709817484481861731;
  a114 = -60.92349008483054016518434619253765246063;
  a115 = -5.951518376222392455202832767061854868290;
  a116 = 5.556523730698456235979791650843592496839;
  a117 = 63.98301198033305336837536378635995939281;
  a118 = .1464202825041496159275921391759452676003e-1;
  a119 = .6460408772358203603621865144977650714892e-1;
  a1110 = -.7930323169008878984024452548693373291447e-1;
  a121 = -3.301622667747079016353994789790983625569;
  a124 = -118.0112723597525085666923303957898868510;
  a125 = -10.14142238845611248642783916034510897595;
  a126 = 9.139311332232057923544012273556827000619;
  a127 = 123.3759428284042683684847180986501894364;
  a128 = 4.623244378874580474839807625067630924792;
  a129 = -3.383277738068201923652550971536811240814;
  a1210 = 4.527592100324618189451265339351129035325;
  a1211 = -5.828495485811622963193088019162985703755;
  a131 = -3.039515033766309030040102851821200251056;
  a134 = -109.2608680894176254686444192322164623352;
  a135 = -9.290642497400293449717665542656897549158;
  a136 = 8.430504981764911142134299253836167803454;
  a137 = 114.2010010378331313557424041095523427476;
  a138 = -.9637271342145479358162375658987901652762;
  a139 = -5.034884088802189791198680336183332323118;
  a1310 = 5.958130824002923177540402165388172072794;
  b1 = .4427989419007951074716746668098518862111e-1;
  b6 = .3541049391724448744815552028733568354121;
  b7 = .2479692154956437828667629415370663023884;
  b8 = -15.69420203883808405099207034271191213468;
  b9 = 25.08406496555856261343930031237186278518;
  b10 = -31.73836778626027646833156112007297739997;
  b11 = 22.93828327398878395231483560344797018313;
  b12 = -.2361324633071542145259900641263517600737;
  d1 = .4431261522908979212486436510209029764893e-1;
  d6 = .3546095642343226447863179350895055038855;
  d7 = .2478480431366653069619986721504458660016;
  d8 = 4.448134732475784492725128317159648871312;
  d9 = 19.84688636611873369930932399297687935291;
  d10 = -23.58162337746561841969517960870394965085;
  d13 = -.3601679437289775162124536737746202409110;
  b13 = 0.0;
  d11 = 0.0;
  d12 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
}

static void rksTableauV87r(rksTableau &T) {
  const int s = 13; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a85, a86, a87, a91, a94, a95, a96, a97, a98, a101, a104, a105, a106, a107, a108, a109, a111, a114, a115, a116, a117, a118, a119, a1110, a121, a124, a125, a126, a127, a128, a129, a1210, a1211, a131, a134, a135, a136, a137, a138, a139, a1310, b1, b6, b7, b8, b9, b10, b11, b12, d1, d6, d7, d8, d9, d10, d13, b13, d11, d12;
  c2 = .25;
  c3 = .1128884514435695538057742782152230971129;
  c4 = .1693326771653543307086614173228346456693;
  c5 = .424;
  c6 = .509;
  c7 = .867;
  c8 = .15;
  c9 = .7090680365138684008060140010282474786750;
  c10 = .32;
  c11 = .45;
  a21 = .25;
  a31 = .8740084650491523205268632759487741197705e-1;
  a32 = .2548760493865432175308795062034568513581e-1;
  a41 = .4233316929133858267716535433070866141732e-1;
  a43 = .1269995078740157480314960629921259842520;
  a51 = .4260950588874226149488144523757227409094;
  a53 = -1.598795284659152326542773323065718111709;
  a54 = 1.596700225771729711593958870689995370799;
  a61 = .5071933729671392951509061813851363923933e-1;
  a64 = .2543337726460040758275471440887777803137;
  a65 = .2039468900572819946573622377727085804470;
  a71 = -.2900037471752311097038837928542589612409;
  a74 = 1.344187391026078988943868110941433700318;
  a75 = -2.864777943361442730961110382703656282947;
  a76 = 2.677594299510594851721126064616481543870;
  a81 = .9853501133799354646974040298072701428476e-1;
  a85 = .2219268063075138484202403649819738790358;
  a86 = -.1814062291180699431269033828807395245747;
  a87 = .1094441147256254823692261491803863125415e-1;
  a91 = .3871105254573114467944461816516637340565;
  a94 = -1.442445497485527757125674555307792776717;
  a95 = 2.905398189069950931769134644923384844174;
  a96 = -1.853771069630105929084333267581197802518;
  a97 = .1400364809872815426949732510977124147922;
  a98 = .5727394081149581657574677462444770648875;
  a101 = -.1612440344443930810063001619791348059544;
  a104 = -.1733960295735898408357840447396256789490;
  a105 = -1.301289281406514740601681274517249252974;
  a106 = 1.137950375173861730855879213143100347212;
  a107 = -.3174764966396688010692352113804302469898e-1;
  a108 = .9335129382493366643981106448605688485659;
  a109 = -.8378631833473385270330085562961643320150e-1;
  a111 = -.1919944488158953328151080465148357607314e-1;
  a114 = .2733085726526428490794232625401612427562;
  a115 = -.6753497320694437291969161121094238085624;
  a116 = .3415184981384601607173848997472838271198;
  a117 = -.6795006480337577247892051619852462939191e-1;
  a118 = .9659175224762387888426558649121637650975e-1;
  a119 = .1325308251118210118072103846654538995123;
  a1110 = .3685495936038611344690632995153166681295;
  a121 = .6091877403645289867688841211158881778458;
  a124 = -2.272569085898001676899980093141308839972;
  a125 = 4.757898342694029006815525588191478549755;
  a126 = -5.516106706692758482429468966784424824484;
  a127 = .2900596369680119270909581856594617437818;
  a128 = .5691423963359036822910985845480184914563;
  a129 = .7926795760332167027133991620589332757995;
  a1210 = .1547372045328882289412619077184989823205;
  a1211 = 1.614970895662181624708321510633454443497;
  a131 = .8873576220853471966321169405198102270488;
  a134 = -2.975459782108536755851363280470930158198;
  a135 = 5.600717009488163059799039254835009892383;
  a136 = -5.915607450536674468001493018994165735184;
  a137 = .2202968915613492701687914254080763833124;
  a138 = .1015509782446221666614327134090299699755;
  a139 = 1.151434564738605590978039775212585055356;
  a1310 = 1.929710166527123939613436190080584365307;
  b1 = .4472956466669571420301584042904938246647e-1;
  b6 = .1569103352770819981336869801072664540918;
  b7 = .1846097340815163774070245187352627789204;
  b8 = .2251638060208699104247941940035072197092;
  b9 = .1479461565197023468700517988544914175374;
  b10 = .7605554244495582526979836191033649101273e-1;
  b11 = .1227729023501861961082434631592143738854;
  b12 = .4181195863899163158338484280087188237679e-1;
  d1 = .4584711140049592587866473012201028209588e-1;
  d6 = .2623189140415238743744335658484580339239;
  d7 = .1916937233785261190448573863568842900803;
  d8 = .2170917232790261833097840742290644856820;
  d9 = .1273818962483370679680316945065673786790;
  d10 = .1151053038536532625824051575004319214889;
  d13 = .4056132779843756684182339143658360805005e-1;
  b13 = 0.0;
  d11 = 0.0;
  d12 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
}

static void rksTableauEv87(rksTableau &T) {
  const int s = 13; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a84, a85, a86, a87, a91, a94, a95, a96, a97, a98, a101, a104, a105, a106, a107, a108, a109, a111, a114, a115, a116, a117, a118, a119, a1110, a121, a124, a125, a126, a127, a128, a129, a1210, a1211, a131, a134, a135, a136, a137, a138, a139, a1310, b1, b6, b7, b8, b9, b10, b11, b12, d1, d6, d7, d8, d9, d10, d13, b13, d11, d12;
  c2 = 5.56000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  c3 = 1.02577772963604852686308492201039861351819757365684575389948006932409012131715771231e-1;
  c4 = 1.53866659445407279029462738301559792027729636048526863084922010398613518197573656846e-1;
  c5 = 3.84600000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c6 = 4.61500000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c7 = 1.53800000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c8 = 8.57100000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c9 = 9.50522279549898543264080746155892215372083974054663210568629277143998134232923935826e-1;
  c10 = 7.22200000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c11 = 9.37500000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  a21 = 5.56000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  a31 = 7.95367668507191489463769912616233707314310387997043291210079553116635514850736965522e-3;
  a32 = 9.46240962785329377916707930748775242786766534857141424778472114012426569832084015753e-2;
  a41 = 3.84666648613518197573656845753899480069324090121317157712305025996533795493934142114e-2;
  a43 = 1.15399994584055459272097053726169844020797227036395147313691507798960138648180242634e-1;
  a51 = 3.84391795249995742463414694148930014496173869183551650147262740706965192166135782038e-1;
  a53 = -1.4413754967533895577013957427459670216869972115118037596261793190397908645560378797e0;
  a54 = 1.44158370150339381523798104859703700719082334232825210947891657833282567238990209761e0;
  a61 = 4.61679927225245821615407392990543100173216372016370799947874024989156378037664283266e-2;
  a64 = 2.30766671478580119613626743161835287145952544968066036170902600446677938235230248901e-1;
  a65 = 1.84565335798895298224832517539110402836725817830296883834309997054406423961003322773e-1;
  a71 = 5.98340656981684945502396474355291084616704020934744687941026109734450878397200860064e-2;
  a74 = 1.11070988365806962422446377950496848082610655711386443707870631101667161815495770356e-1;
  a75 = -3.4214310915191931585007005185985812353861433634302098328491649125465037270146208540e-2;
  a76 = 1.71092568512164746123209797999598558095803758294411858265184070503527876149303521772e-2;
  a81 = -5.3795007752787302261474715462946928642562492201550692194017067742239449282090711696e-1;
  a84 = -6.9376482130983202468485892167839192961199723756828602894007094394246115740310891917e0;
  a85 = -4.6624538209733341287003258854737026552374536190626584633882834158844336356509780740e0;
  a86 = 3.99515211159952739816366225688709123778305091676102567472916353273143970250297438270e0;
  a87 = 9.0;
  a91 = -1.6324274407986590789038658936937338307876068928047009736797164524613917396815405352e0;
  a94 = -1.0827155649128676300106788103992512405570868288293796026024992785621740125352159452e1;
  a95 = -1.2412770216529556426254179980877757507654516931216971035750731127725169216761599855e1;
  a96 = 9.72736897958029723333620569705898647991679730938513196401603844136574931065998854059e0;
  a97 = 1.61993509145171614713050347911446705109706857016123854149928694302903467232127746865e1;
  a98 = -1.0384430809066835611232576348376103150240692462738613298483822870379681784453944854e-1;
  a101 = 4.37969506182387858999383959550753086479857907336724342215547180958125611435049537720e-1;
  a104 = 3.93953188040780609055484179804938065477461949555863432014746148406805874085095872275e0;
  a105 = 2.86077034685671490951716275075256969722509129259119025387241581075503457766785108234e0;
  a106 = -1.7743107088675159424540294493688191717730257147970606451633813362863675504027539094e0;
  a107 = -4.8953905177642011781075138200194429107217851579522239829952612873605231471994530162e0;
  a108 = 2.13024858819919749451668262421798635017462468974251987239988291318720261714005089224e-1;
  a109 = -5.9395365635111487961513501386239991002220291711516275316770143453048494065657506474e-2;
  a111 = -1.4741971530084051096783684495220401762953156744718612221415953506898842409410764394e0;
  a114 = -1.0994004568773831856436455451565667915965033574293371252746458409074590501516335418e1;
  a115 = -1.1347103595550558407033481028962542986773842505250429938230098582640399568841966955e1;
  a116 = 8.95698732805847648932199138433047040420587034365927857662263761085553916622223226594e0;
  a117 = 1.58937788726768491199870127047315726092390614749240868516326055420934681415123903105e1;
  a118 = -9.8752574205231410895382886685088801840992767797867610760540167194860976207728613471e-2;
  a119 = 4.88850458495220344526104177023129683434865807354905894219062956659502007705067202486e-3;
  a1110 = -4.0968137822510287105773140969344294040959548433844633187412729158670403045658227312e-3;
  a121 = -2.6300593327636131335587362170718842216114396527353516008958114998659179172525086061e0;
  a124 = -9.1742180511635168771961994716051325461520269343576437631118099519514590372371556623e0;
  a125 = -1.9181392627635586309607750914002594644548673509102310272301267608284090817942461230e1;
  a126 = 1.46425586936966487212372354845143649157868001572824958943201356999170406197491766176e1;
  a127 = 1.75293194641808432064264792329037561372134510518264860784855289743130207876560918202e1;
  a128 = -3.7191756017725564567993148040646183259986035050825855493662910931190949416983788302e-1;
  a129 = -7.0099615383151451311120261802995845194696552530300984200157512415971100577511657140e-1;
  a1210 = 5.10160166123419814594373796567007747993680583372732066087135277690995003233586244844e-2;
  a1211 = 8.35689551081652570030668604041209869059346704560318853832715091573927364648452890489e-1;
  a131 = 2.15760322561474994411124388359746470315016189993958868463227784863178137907983232935e-1;
  a134 = 8.34514732667823366560737998962040201361659061600363822667194131727201870364735969189e0;
  a135 = 2.18566238546511021703127210271410825362935054094744855941151130759498192966438607636e0;
  a136 = -1.6872364802758613307171751762566037387388156104034081739315755043398409544172643181e0;
  a137 = -8.7118979009844764699498732005474293440649791305528456441763927818947185541425600579e0;
  a138 = 2.44414593429891254272241354773491646097520831036964631654327459178700700237090814351e-2;
  a139 = 8.46378799450539085004935994965690483699123804486085782773911124365252659256135973281e-2;
  a1310 = 5.43485007267475889689554161135858132263172930458903122118464018149985401390772696043e-1;
  b1 = 4.39177036443990264767390168245612938204165409329946310979955485867750206154532516854e-2;
  b6 = 3.51024625301198059003711947067197522471081955372891881652834064609672316461737698276e-1;
  b7 = 2.46142826354923978927496217611357561294520506174049687891933955470328003987048218576e-1;
  b8 = 9.00324493052912782292485743663086167980166191074574843765667123724307486893548248673e-1;
  b9 = 4.54941872725474687205992745995487359662269870465745627778562103310520198596208865435e0;
  b10 = 4.80250151923706131251435406595354490562463095633152715752232119918349986859035476853e-3;
  b11 = -4.7410543521200460045854095885960988103202068879671787514735082136428459116707380269e0;
  b12 = -3.5457652500737177548746515059093087677430164120112009787806583305262240211772839944e-1;
  d1 = 4.33210538122143048906481448990561225500794142960488184912531716454290716526337909125e-2;
  d6 = 3.38299786239324428483566710567298525845838208179552347378195505093356569277661366407e-1;
  d7 = 2.48479842819169689968723882111796748697722834015831878169551896051150521744166052704e-1;
  d8 = 2.23788296726381915717640915256391015765072451399129220461423268948426689142758885858e-1;
  d9 = -4.0201628625024655295655283841782384327701940439930130919929624261829397741432866016e-2;
  d10 = 1.23292316936198306185965165238573766500855475391670951932862563960817353162350588140e-1;
  d13 = 6.30203320917360100491104657686662049681335571576969144866432185626491927618621819940e-2;
  b13 = 0.0;
  d11 = 0.0;
  d12 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
}

static void rksTableauK87(rksTableau &T) {
  const int s = 13; T.s = s; T.adaptive = 1; T.errOrder = 7;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a84, a85, a86, a87, a91, a94, a95, a96, a97, a98, a101, a104, a105, a106, a107, a108, a109, a111, a114, a115, a116, a117, a118, a119, a1110, a121, a124, a125, a126, a127, a128, a129, a1210, a1211, a131, a134, a135, a136, a137, a138, a139, a1310, a1311, b1, b6, b7, b8, b9, b10, b11, b12, b13, d1, d6, d7, d8, d9, d10, d11, d12, d13;
  c2 = 2.80032137795311131773988246233287895064681826798949202422973107165103409675642981593e-2;
  c3 = 1.17211542455798199481153380377682961936340701290456762390962847695113281163272045568e-1;
  c4 = 1.75817313683697299221730070566524442904511051935685143586444271542669921744908068353e-1;
  c5 = 4.12418774972264624675725499028219180367525003545123161749372304663713787609586013029e-1;
  c6 = 4.85788040125653655249646480463893555030416915791171976577551272195807115944917385569e-1;
  c7 = 1.61945436881864850373469095233292102203653467396439966236482648418795024659046870770e-1;
  c8 = 9.69999622636762726575177046252154115146102466697694310620259374331752600661643542686e-1;
  c9 = 9.67212187169379825484211792872972399851315982484116576998481473839941803252105313177e-1;
  c10 = 9.51733593009250954565256387258796094610870512261999333726263998154934269533352125669e-1;
  c11 = 9.68202771170154148019728796247765727264175032590143665416817858054805198295905065247e-1;
  a21 = 2.80032137795311131773988246233287895064681826798949202422973107165103409675642981593e-2;
  a31 = -1.2809147513766000602840952300143562083196457175706189219831636475705535103141265693e-1;
  a32 = 2.45303017593458205509562903379118613607705175036823097559603878184775233215504838718e-1;
  a41 = 4.39543284209243248054325176416311107261277629839212858966110678856674804362270170882e-2;
  a43 = 1.31862985262772974416297552924893332178383288951763857689833203657002441308681051264e-1;
  a51 = 3.37793581678770282454466907421349755434169654489904742664883921124336709641723527478e-1;
  a53 = -1.2272549349642134972638116912742760383098006898941228119302659681159830923493358553e0;
  a54 = 1.30188012825770783948507028288114558866194048988029573253542612869530190101485281039e0;
  a61 = 5.55707180742854953774795421040398153698425834244408078243992150711110988336315378908e-2;
  a64 = 2.51201707056335789089668842569664694678061738700922859182154307406956224053004373812e-1;
  a65 = 1.79015614995032370782498095790188986963634606811801882820741709651062551409519978967e-1;
  a71 = 6.68574347263499831237129311835436351646983012984884556347193166264996297617553477916e-2;
  a74 = 1.18464279145447863166879557612335382759089586154167432866512942838128687075481264338e-1;
  a75 = -4.9625776043451759740292422000972633501142622216483112027320727273129396510418976777e-2;
  a76 = 2.62494990535187638231690284383855963023506866196027758824536604264425581164307083575e-2;
  a81 = -2.8828852333227597022830828823050925772450620891223369172510088561729332090076138712e0;
  a84 = -6.2440208358504107644024024514118804244308557646280655968919670956442898657820780678e1;
  a85 = -1.8568693804204652514452785270528995249754499915069250253336433143578163151508120889e0;
  a86 = 3.63680272715359047470913994410370911642713568016445118939856192986207517504148311731e0;
  a87 = 6.45131598677305048496184230256252416917236861835199785338289493414147153759342124993e1;
  a91 = -2.8044177030062210609793681802747767950835025205736626228179150569096325843989251959e0;
  a94 = -6.0724923514423992066404703993882607779493861739697985128789442636875454172217394054e1;
  a95 = -1.8439925449308251307668788876911848442475862438554757484077098049668682602503860260e0;
  a96 = 3.58341131683436481398169260904220163311107056209428641899333714250541234007157548631e0;
  a97 = 6.27562314336350305301529075880419713175762252340352806429712952136086493920793363741e1;
  a98 = 9.03199061022739500562657637369442031043920598379602859131908802359360137694011980530e-4;
  a101 = -2.3867982748517262502656630171168908345533985055664349630981323816949508696589885131e0;
  a104 = -5.1627063109123959312754417147040225909854829131526049890819790306938208226330016825e1;
  a105 = -1.7536886231138950479955440820659955467185829948904221201507369859127252396249986529e0;
  a106 = 3.28049564858832505325605350268523413528007142886883523265902376116967563208653997102e0;
  a107 = 5.34320383180959473568498403628072891122624268061087757470806360734926014535143237753e1;
  a108 = -5.1871871749054814179692202838627648419279746926926280632135852213870053073114332763e-2;
  a109 = 5.86215051636139696546789708280127234169485458303572483088290904612000509294669736233e-2;
  a111 = -2.8321844833678190185701414338307927128819672795892717761836464805524068227918002961e0;
  a114 = -6.1331782300299804156579640111028383230359362036389968935124318342451005168121921975e1;
  a115 = -1.8486956144396106169929005145135397640092170913037834243348598217810735996138171447e0;
  a116 = 3.60245017376559155660368881029952545634233977218873589569763598469965203717427900757e0;
  a117 = 6.33778491267616171592057963772550300254984410272010439289876706968447695603368885389e1;
  a118 = -1.4415990912159637680387758372857367088383577073617671128542010599339480760810185327e-2;
  a119 = 1.79850128395124994545750132486994455015114960352855140561364324582618209513891071121e-2;
  a1110 = -3.0031531771736374212615868099161882544231529622685172138561311528231108610020573246e-3;
  a121 = -4.0538618880188790999955490264156166727875270359794492495465581102083340028760864442e0;
  a124 = -8.9573424620688719282465667174595500838253854698064448723443082397797973834679560630e1;
  a125 = -2.1741602779971559275874048256900391711850691989074830707636990301813342017567123130e0;
  a126 = 4.90277984566299792538628476237120023643679128602934227568174295156160923256501166825e0;
  a127 = 9.21308811959261498386202635372829188598935945642923099199446915685906551410074148629e1;
  a128 = -1.0615798848368367252894309217989593007445778391108496729169226136688468696047739072e2;
  a129 = 7.15244888413933960825621793700326950628446023762240029384782852503289700766263846501e2;
  a1210 = -5.8741641553458552194866804448440052315485267257872985945866582180724510668125792910e1;
  a1211 = -5.5057747263167612955576982434494393109010422446397491970269616705184666943368933333e2;
  a131 = -1.5426576117927492437318562966731433591238073288653428133646285261236434041035073113e1;
  a134 = -4.0842463342275502766697135673815027343155742825350936594898511656878424252559774704e2;
  a135 = -1.3703330210475051981990819170722577088815078067066750833460437452985770323423290388e1;
  a136 = 3.86861748380283839981840260187136546795985692447349074847554981151691738530542460349e1;
  a137 = 4.11064481162365098634151552138274804840423912301682160479008282157089539192861116480e2;
  a138 = -5.2625896422984832125592493280526052943397760170789326525937264023403370769397788590e3;
  a139 = 3.59120401468351311263204092337603786279414890192167416431431463233495380766373955611e4;
  a1310 = -2.9428565975170828789337889297586264433907248673913937095047691721267811559650558183e3;
  a1311 = -2.7717790023268800945373425815230530802580454445165684044343605511598675223912861429e4;
  b1 = 4.62710144559749507643438665696565266477463923324303941594264880671698479184226674443e-2;
  b6 = 3.70024261130579576116287766947369066170603597694346848542347561458083670963945787412e-1;
  b7 = 2.59046906438325331026392759493100306344056209570957698029069680004143772202534147226e-1;
  b8 = 4.39526151941350658964709597497950710839169490812990167038362808341439048915871691690e2;
  b9 = 2.54699783254442554735577328130608901983844079828777855737604201012040528247471881063e-3;
  b10 = 3.71160265008765610052766362243075634874924739787417318444668226976710753590848050226e1;
  b11 = -4.6957326399687634573980555517438556136604535869046208947272277883667432727498150998e2;
  b12 = -6.8840856231271330609815727620309531232087818518011356960217496959521261426131545711e0;
  b13 = 1.37281997918834547346514047866805411030176899063475546305931321540062434963579604579e-1;
  d1 = b1 - (5.85295842711462028451365622251323842688965073989360183197454435266584210000889266151e-6);
  d6 = b6 - (9.39126876188594677381173972498082533162562941166681876536458073485843968573614935432e-5);
  d7 = b7 - (-2.1858808218216614235946904150275890682710865952635557843966398644681448317233753310e-5);
  d8 = b8 - (3.82147409595844443746138596500996871329379548062006046484595468670810794523045735063e2);
  d9 = b9 - (7.57594006256575145190136681251802140063882902269863054941867158087541929651242123261e2);
  d10 = b10 - (-5.0572248451354316398326863204362055571090849938158216870898444305578120258688628657e0);
  d11 = b11 - (-1.1340723721698801541760405363985770050927554242983877738142540298190990694395498870e3);
  d12 = b12 - (-6.2562494403371433261049312272180260925647282293121510390454736461360125268267236809e-1);
  d13 = b13 - (1.37281997918834547346514047866805411030176899063475546305931321540062434963579604579e-2);
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
}

static void rksTableauV89(rksTableau &T) {
  const int s = 16; T.s = s; T.adaptive = 1; T.errOrder = 8;
  double c2, a21, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c15, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a86, a87, a91, a96, a97, a98, a101, a106, a107, a108, a109, a111, a117, a118, a119, a1110, a121, a126, a127, a128, a129, a1210, a1211, a131, a136, a137, a138, a139, a1310, a1311, a1312, a141, a146, a147, a148, a149, a1410, a1411, a1412, a1413, a151, a156, a157, a158, a159, a1510, a1511, a1512, a1513, a161, a166, a167, a168, a169, a1610, a1611, a1612, a1613, a1615, b1, b8, b9, b10, b11, b12, b13, b14, d1, d8, d9, d10, d11, d12, d13, d14, d15, d16, b15, b16;
  const double s6 = std::sqrt(6.0);
  c2 = 1.0/12.0;
  a21 = c2;
  c3 = 1.0/9.0;
  c4 = 1.0/6.0;
  c5 = 2.0*(1.0+s6)/15.0;
  c6 = (6.0+s6)/15.0;
  c7 = (6.0-s6)/15.0;
  c8 = 2.0/3.0;
  c9 = 1.0/2.0;
  c10 = 1.0/3.0;
  c11 = 1.0/4.0;
  c12 = 4.0/3.0;
  c13 = 5.0/6.0;
  c15 = 1.0/6.0;
  a31 = 1.0/27.0;
  a32 = 2.0/27.0;
  a41 = 1.0/24.0;
  a43 = 3.0/24.0;
  a51 = (4.0+94.0*s6)/375.0;
  a53 = -(282.0+252.0*s6)/375.0;
  a54 = (328.0+208.0*s6)/375.0;
  a61 = (9.0-s6)/150.0;
  a64 = (312.0+32.0*s6)/1425.0;
  a65 = (69.0+29.0*s6)/570.0;
  a71 = (927.0-347.0*s6)/1250.0;
  a74 = (-16248.0+7328.0*s6)/9375.0;
  a75 = (-489.0+179.0*s6)/3750.0;
  a76 = (14268.0-5798.0*s6)/9375.0;
  a81 = 4.0/54.0;
  a86 = (16.0-s6)/54.0;
  a87 = (16.0+s6)/54.0;
  a91 = 38.0/512.0;
  a96 = (118.0-23.0*s6)/512.0;
  a97 = (118.0+23.0*s6)/512.0;
  a98 = -18.0/512.0;
  a101 = 11.0/144.0;
  a106 = (266.0-s6)/864.0;
  a107 = (266.0+s6)/864.0;
  a108 = -1.0/16.0;
  a109 = -8.0/27.0;
  a111 = (5034.0-271.0*s6)/61440.0;
  a117 = (7859.0-1626.0*s6)/10240.0;
  a118 = (-2232.0+813.0*s6)/20480.0;
  a119 = (-594.0+271.0*s6)/960.0;
  a1110 = (657.0-813.0*s6)/5120.0;
  a121 = (5996.0-3794.0*s6)/405.0;
  a126 = (-4342.0-338.0*s6)/9.0;
  a127 = (154922.0-40458.0*s6)/135.0;
  a128 = (-4176.0+3794.0*s6)/45.0;
  a129 = (-340864.0+242816.0*s6)/405.0;
  a1210 = (26304.0-15176.0*s6)/45.0;
  a1211 = -26624.0/81.0;
  a131 = (3793.0+2168.0*s6)/103680.0;
  a136 = (4042.0+2263.0*s6)/13824.0;
  a137 = (-231278.0+40717.0*s6)/69120.0;
  a138 = (7947.0-2168.0*s6)/11520.0;
  a139 = (1048.0-542.0*s6)/405.0;
  a1310 = (-1383.0+542.0*s6)/720.0;
  a1311 = 2624.0/1053.0;
  a1312 = 3.0/1664.0;
  a141 = -137.0/1296.0;
  a146 = (5642.0-337.0*s6)/864.0;
  a147 = (5642.0+337.0*s6)/864.0;
  a148 = -299.0/48.0;
  a149 = 184.0/81.0;
  a1410 = -44.0/9.0;
  a1411 = -5120.0/1053.0;
  a1412 = -11.0/468.0;
  a1413 = 16.0/9.0;
  a151 = (33617.0-2168.0*s6)/518400.0;
  a156 = (-3846.0+31.0*s6)/13824.0;
  a157 = (155338.0-52807.0*s6)/345600.0;
  a158 = (-12537.0+2168.0*s6)/57600.0;
  a159 = (92.0+542.0*s6)/2025.0;
  a1510 = (-1797.0-542.0*s6)/3600.0;
  a1511 = 320.0/567.0;
  a1512 = -1.0/1920.0;
  a1513 = 4.0/105.0;
  a161 = (-36487.0-30352.0*s6)/279600.0;
  a166 = (-29666.0-4499.0*s6)/7456.0;
  a167 = (2779182.0-615973.0*s6)/186400.0;
  a168 = (-94329.0+91056.0*s6)/93200.0;
  a169 = (-232192.0+121408.0*s6)/17475.0;
  a1610 = (101226.0-22764.0*s6)/5825.0;
  a1611 = -169984.0/9087.0;
  a1612 = -87.0/30290.0;
  a1613 = 492.0/1165.0;
  a1615 = 1260.0/233.0;
  b1 = 103.0/1680.0;
  b8 = -27.0/140.0;
  b9 = 76.0/105.0;
  b10 = -201.0/280.0;
  b11 = 1024.0/1365.0;
  b12 = 3.0/7280.0;
  b13 = 12.0/35.0;
  b14 = 9.0/280.0;
  d1 = b1 - (-1911.0/109200.0);
  d8 = b8 - (34398.0/109200.0);
  d9 = b9 - (-61152.0/109200.0);
  d10 = b10 - (114660.0/109200.0);
  d11 = b11 - (-114688.0/109200.0);
  d12 = b12 - (-63.0/109200.0);
  d13 = b13 - (-13104.0/109200.0);
  d14 = b14 - (-3510.0/109200.0);
  d15 = -(39312.0/109200.0);
  d16 = -(6058.0/109200.0);
  b15 = 0.0;
  b16 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  T.A[12*s+11]=a1312;
  T.A[13*s+0]=a141;
  T.A[13*s+5]=a146;
  T.A[13*s+6]=a147;
  T.A[13*s+7]=a148;
  T.A[13*s+8]=a149;
  T.A[13*s+9]=a1410;
  T.A[13*s+10]=a1411;
  T.A[13*s+11]=a1412;
  T.A[13*s+12]=a1413;
  T.A[14*s+0]=a151;
  T.A[14*s+5]=a156;
  T.A[14*s+6]=a157;
  T.A[14*s+7]=a158;
  T.A[14*s+8]=a159;
  T.A[14*s+9]=a1510;
  T.A[14*s+10]=a1511;
  T.A[14*s+11]=a1512;
  T.A[14*s+12]=a1513;
  T.A[15*s+0]=a161;
  T.A[15*s+5]=a166;
  T.A[15*s+6]=a167;
  T.A[15*s+7]=a168;
  T.A[15*s+8]=a169;
  T.A[15*s+9]=a1610;
  T.A[15*s+10]=a1611;
  T.A[15*s+11]=a1612;
  T.A[15*s+12]=a1613;
  T.A[15*s+14]=a1615;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=0.0;
  T.b[6]=0.0;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.b[13]=b14;
  T.b[14]=b15;
  T.b[15]=b16;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=0.0;
  T.bhat[6]=0.0;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
  T.bhat[13]=d14;
  T.bhat[14]=d15;
  T.bhat[15]=d16;
}

static void rksTableauT98a(rksTableau &T) {
  const int s = 16; T.s = s; T.adaptive = 1; T.errOrder = 8;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a86, a87, a91, a96, a97, a98, a101, a106, a107, a108, a109, a111, a116, a117, a118, a119, a1110, a121, a126, a127, a128, a129, a1210, a1211, a131, a136, a137, a138, a139, a1310, a1311, a1312, a141, a146, a147, a148, a149, a1410, a1411, a1412, a1413, a151, a156, a157, a158, a159, a1510, a1511, a1512, a1513, a1514, a161, a166, a167, a168, a169, a1610, a1611, a1612, a1613, a1614, b1, b8, b9, b10, b11, b12, b13, b14, b15, b16, d1, d8, d9, d10, d11, d12, d13, d14, d15, d16;
  c2 = .2040816326530612244897959183673469387755102040816326530612244897959183673469387755102e-1;
  c3 = .8813293914998103008637915939241511230858121177369086646032265578682773616755710635430e-1;
  c4 = .1321994087249715451295687390886226684628718176605362996904839836802416042513356595314;
  c5 = .4285714285714285714285714285714285714285714285714285714285714285714285714285714285714;
  c6 = .5364755392243287681395100999813264375851395225813758811703296868095847858703057159708;
  c7 = .2254292226804331366223946619234354671767652393233860235915750750951771188915990459340;
  c8 = .6349206349206349206349206349206349206349206349206349206349206349206349206349206349206;
  c9 = .4761904761904761904761904761904761904761904761904761904761904761904761904761904761905;
  c10 = 1.055555555555555555555555555555555555555555555555555555555555555555555555555555555556;
  c11 = .7777777777777777777777777777777777777777777777777777777777777777777777777777777777778;
  c12 = .1474169624260994760478401587101660956180420042646161803585052367559161448324216358210;
  c13 = .9375;
  c14 = .975;
  a21 = .2040816326530612244897959183673469387755102040816326530612244897959183673469387755102e-1;
  a31 = -.1021687274487683147769723606535303770211896504959152824720881616958241197178852529434;
  a32 = .1903016665987493448633515200459454893297708622696061489324108174826518558854423592977;
  a41 = .3304985218124288628239218477215566711571795441513407492262099592006040106283391488286e-1;
  a43 = .9914955654372865884717655431646700134715386324540222476786298776018120318850174464858e-1;
  a51 = .9439263832171291883556670672811977550046744677129090342789268623214132543830715838461;
  a53 = -3.630115063093482037077114300073110289641773922761700400575782997067611129391489310214;
  a54 = 3.114760108447781420150018661363341106065670883620219937725427563317626446436989154939;
  a61 = .2056923328616261711782848556408566522300362983753802984037858427390204762957826067614e-1;
  a64 = .2604824183197402987552340723087785980849282464799395547519553603229784733524946656053;
  a65 = .2554238876184258522664475421084621742772076462638982965779957422127042648882327896893;
  a71 = .4318483705109263028835194255758669000001323804220689783671769343298616516403726355252e-1;
  a74 = .1769284839807689645520842945002498666673573450203833044058383710943493957801668363127;
  a75 = .7715414621876983794315795767271715967327448157389024021256382790156907385043923034755e-2;
  a76 = -.2399512973305442012357370901672805457932791896593202672237372222315349437648976965990e-2;
  a81 = .7054673721340388007054673721340388007054673721340388007054673721340388007054673721340e-1;
  a86 = .2389860715558522381270320269011306632810238539566724845073599194488366776462554669041;
  a87 = .3253878261513788024373418708061003772833500437505585560570139782583943629181184308032;
  a91 = .7068452380952380952380952380952380952380952380952380952380952380952380952380952380952e-1;
  a96 = .1146981694865827822571846471014964620252663838260725205469644176957364421846758824211;
  a97 = .3242899257515124558380534481365987760699717114120227175482736775423587959105622128170;
  a98 = -.3348214285714285714285714285714285714285714285714285714285714285714285714285714285714e-1;
  a101 = .3828039568865740740740740740740740740740740740740740740740740740740740740740740740741;
  a106 = -26.28104570763124221540974003468081947028606261133704083478349807677632293296095495655;
  a107 = -1.748424643449004698170506878899427443294184302243206078796748836803923980619291957032;
  a108 = 8.7107466796875;
  a109 = 19.99147527006172839506172839506172839506172839506172839506172839506172839506172839506;
  a111 = .5775788905284689042365660435243263588459243112336590023001577005230527581251090920939e-1;
  a116 = .76;
  a117 = .4018553287915560050245043972333732847203199147775749045996022855946715894896836169645;
  a118 = .6626589626062798854099077293240416432854142660801015341291381713518812647417195998478e-1;
  a119 = -.5150696954976787388652658281684056884304318728657759205657506333922693112630203285629;
  a1110 = .6968359170425632653891831427973381274755878134602740100996538387882097264431620182023e-2;
  a121 = .6994103035906395715686489597582150022600838999784034656313893569317997374351308591645e-1;
  a126 = -.1206166060565470490794750098024428160239891793653556676806869026855595723414769418541;
  a127 = .1326963017314458683010911057621835709637828919128544554925159376674464191887834137030;
  a128 = .2031532538077627922059108299487811850373691592058111305356962697560067377203999264697;
  a129 = -.7548640175023889467177231137902524280970115417595113975094208653922765875893450604889e-1;
  a1210 = .3789103686439011295643140618205140110558445127918206929994555147217682251832650190596e-2;
  a1211 = -.6605971935182620916042249241335724188598654843850115173121147228314743697169599255572e-1;
  a131 = .6878836016002817993752515099013030041019173079702440987327706356852301558017737433194;
  a136 = -3.658808527366451517196946897056976218504208192589081007775064092270089158621094491595;
  a137 = 5.845503378564669890314102776059891925747966958611333131112559317698644813283634646256;
  a138 = 5.411412752004688858844863696709300082070631013287790747126942423884017249319289413984;
  a139 = -1.582095988269673568992894109712396974043574141076643374159775089066374202615336309544;
  a1310 = .9945248865706299667788846635419585634800918858472131439118016352130452805983178532975e-1;
  a1311 = -1.329593487105234404098771549303922926244482088345956610970650644487259700977298059950;
  a1312 = -4.536254218085344054923493892951394749476260046442408298457962714965473684250800727799;
  a141 = .8075602959459265637003617465986992156750424242709811245269878174761587489984770070790;
  a146 = -7.835750139557992772198249358151231578755580407654683121798887409512496231362858547948;
  a147 = 6.174200025941229770123184423146954177664197990793732802584504944027199598185299195828;
  a148 = 7.111311052783799148723822283846058963658585479679811643355604800924488305671070483858;
  a149 = 1.126413982652184268358560776506518924191271296281999997786352511455840556312961264408;
  a1410 = .1138838501589788637237573235699770016403876336825059128989257049544545698305937427348;
  a1411 = -1.434774678607913207528871735039082294103761283429995473644501054462770211806990144762;
  a1412 = -5.064314742122892898464863809545209351959607473464534474081890709755483907888716564210;
  a1413 = -.2352964719331973643770165093268505801053566015981841162709660510739142793983643698681e-1;
  a151 = .8101674645254722523232876762637530370910583262175105775933018316393086542069855754015;
  a156 = -11.92841245677955898272791316012006359130670880301152754325896491950568584401093883276;
  a157 = 5.364312898490360554625912421385267920693980736534035810795373384314434259846321616737;
  a158 = 8.059954459759511795757228635532122933456240951117021423047033797467500489813129448587;
  a159 = 4.618063439001721483758160286714057772615332315540952842312029459375380582946251202099;
  a1510 = .1128601659181894505188796244218558774756228116520111404880300475877949274532116052613;
  a1511 = -1.315207706945002903017387913180865178001333874822374203640801449667142626811434784416;
  a1512 = -4.689892399903609125554297739238443813841792924181292032608257980483530407060762654113;
  a1513 = .6721374554329681653036240358001936392819624140428230276841754345869866338681915837036e-2;
  a1514 = -.3856723862141420733690607213568689457521916318676624500458592507392990272144509263344e-1;
  a161 = .7081867006810967019347772392865651712376321476396730568850847595024088622008058350820;
  a166 = -11.35112404359189673946143658241126665471485966274613155801176412146453899273199024599;
  a167 = 4.439836852763619566651386303128039432194840880353792439043045399385549660055953598204;
  a168 = 7.160528966855846780330688332694210230082237210593958603950542885510151860241609708315;
  a169 = 4.944752289066181389539519911976094691889352151362799884071179481037363375554630307368;
  a1610 = .9819535610867987425927035019405551701168012197193983668338466050076531884883427929320e-1;
  a1611 = -1.023835739158951766446520340031695827655784562575140398451539531509310691718294156648;
  a1612 = -3.935185699031579433419722235418354733643010565663948821548988100702690247383937415641;
  a1613 = -.2186260374551646198917238469418771662941856089051019740795631241695933765898516172461e-1;
  a1614 = -.1949207994747991139879059472346010977266916004643284521298911984273980740862674825940e-1;
  b1 = .4153556008805959168895898921867201921459132555672551671643920848892579905394945027033e-1;
  b8 = -.4252280874169805589327845543703559909471287347438636516704849004566228405918914211737;
  b9 = .4911269629417608841870622286295630148000839362387724323042417629187646342598357379221;
  b10 = .4541782417588474254373676812638040365345450854512311037987257309434313503153553995250;
  b11 = 1.006032649094428065183781872196896559026233580812897577690817173850511464781870422371;
  b12 = .2396980714287725918428584296767300637015778481901015943085981245718488414129283930348;
  b13 = -4.455491297731407466229188603532881977410859597554953958343743900169006073451667494575;
  b14 = 9.288978775706101824452250592593505318906312756833224753343846524732597445318251252011;
  b15 = -6.410061645100351588399537404906702274594586970014904598917670494111219851867862508616;
  b16 = .7692307692307692307692307692307692307692307692307692307692307692307692307692307692308;
  d1 = .3999250134104311539086473895855274325427452646000198100229842429713750574052674683348e-1;
  d8 = .4011325788584545964379727040448630014695811775770738360963521200500104385050340643944e-1;
  d9 = .3925127095625233142417946847596602750245079278580359243007567443309335209235627547386;
  d10 = -1.051197094764485752167499211132792259622314942028765288915320450078532078636064340575;
  d11 = -.2505083298864024791053208779904093804217869849340728193482139880406203405457882973484;
  d12 = .2465428288939690633785173766277520619424499467256191777438442341744800285916120244248;
  d13 = 6.839965112378725948727677787215511467384166222758213034610265848044786251996046243769;
  d14 = -16.02665175464198790087906253807353043847748558382750862377249679396395516268962930751;
  d15 = 10.0;
  d16 = .7692307692307692307692307692307692307692307692307692307692307692307692307692307692308;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  T.A[12*s+11]=a1312;
  T.A[13*s+0]=a141;
  T.A[13*s+5]=a146;
  T.A[13*s+6]=a147;
  T.A[13*s+7]=a148;
  T.A[13*s+8]=a149;
  T.A[13*s+9]=a1410;
  T.A[13*s+10]=a1411;
  T.A[13*s+11]=a1412;
  T.A[13*s+12]=a1413;
  T.A[14*s+0]=a151;
  T.A[14*s+5]=a156;
  T.A[14*s+6]=a157;
  T.A[14*s+7]=a158;
  T.A[14*s+8]=a159;
  T.A[14*s+9]=a1510;
  T.A[14*s+10]=a1511;
  T.A[14*s+11]=a1512;
  T.A[14*s+12]=a1513;
  T.A[14*s+13]=a1514;
  T.A[15*s+0]=a161;
  T.A[15*s+5]=a166;
  T.A[15*s+6]=a167;
  T.A[15*s+7]=a168;
  T.A[15*s+8]=a169;
  T.A[15*s+9]=a1610;
  T.A[15*s+10]=a1611;
  T.A[15*s+11]=a1612;
  T.A[15*s+12]=a1613;
  T.A[15*s+13]=a1614;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=0.0;
  T.b[6]=0.0;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.b[13]=b14;
  T.b[14]=b15;
  T.b[15]=b16;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=0.0;
  T.bhat[6]=0.0;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
  T.bhat[13]=d14;
  T.bhat[14]=d15;
  T.bhat[15]=d16;
}

static void rksTableauV98r(rksTableau &T) {
  const int s = 16; T.s = s; T.adaptive = 1; T.errOrder = 8;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a86, a87, a91, a96, a97, a98, a101, a106, a107, a108, a109, a111, a116, a117, a118, a119, a1110, a121, a126, a127, a128, a129, a1210, a1211, a131, a136, a137, a138, a139, a1310, a1311, a1312, a141, a146, a147, a148, a149, a1410, a1411, a1412, a1413, a151, a156, a157, a158, a159, a1510, a1511, a1512, a1513, a1514, a161, a166, a167, a168, a169, a1610, a1611, a1612, a1613, b1, b8, b9, b10, b11, b12, b13, b14, b15, d1, d8, d9, d10, d11, d12, d13, d16, b16, d14, d15;
  c2 = .4e-1;
  c3 = .9648736013787361245235039379666356743708e-1;
  c4 = .1447310402068104186785255906949953511556;
  c5 = .576;
  c6 = .2272326564618766017153738192188229509142;
  c7 = .5407673435381233982846261807811770490858;
  c8 = .64;
  c9 = .48;
  c10 = .6754e-1;
  c11 = .25;
  c12 = .6770920153543242682384311058159603931192;
  c13 = .8115;
  c14 = .906;
  a21 = .4e-1;
  a31 = -.198852731918229097650241511466089129345e-1;
  a32 = .1163726333296965222173745449432724803716;
  a41 = .3618276005170260466963139767374883778890e-1;
  a43 = .1085482801551078140088941930212465133667;
  a51 = 2.272114264290177409193144938921415409241;
  a53 = -8.526886447976398578316416192982602292786;
  a54 = 6.830772183686221169123271254061186883545;
  a61 = .5094385535389374394512668566783434123978e-1;
  a64 = .1755865049809071110203693328749561646990;
  a65 = .70229612707574674987780067603244497535e-3;
  a71 = .1424783668683284782770955365543878809824;
  a74 = -.3541799434668684104094753917518523845155;
  a75 = .7595315450295100889001534202778550159932e-1;
  a76 = .6765157656337123215269906939508560510196;
  a81 = .7111111111111111111111111111111111111111e-1;
  a86 = .3279909287605898328568406057725491803016;
  a87 = .2408979601282990560320482831163397085872;
  a91 = .7125e-1;
  a96 = .3268842451575245554847578757216915662785;
  a97 = .1156157548424754445152421242783084337215;
  a98 = -.3375e-1;
  a101 = .4822677322465810178387112087673611111111e-1;
  a106 = .3948559980495400110769549704186108167677e-1;
  a107 = .1058851161934658144373823566907778072121;
  a108 = -.2152006320474309346664428710937500000000e-1;
  a109 = -.1045374260183348238623046875000000000000;
  a111 = -.2609113435754923412210928689962011065179e-1;
  a116 = .3333333333333333333333333333333333333333e-1;
  a117 = -.1652504006638105086724681598195267241410;
  a118 = .3434664118368616658319419895678838776647e-1;
  a119 = .1595758283215209043195814910843067811951;
  a1110 = .2140857321828193385584684233447183324979;
  a121 = -.362842339625565859076509979091267105528e-1;
  a126 = -1.096167597427208807028761474420297770752;
  a127 = .1826035504321331052308236240517254331348;
  a128 = .708225444417068325613028685455625123741e-1;
  a129 = -.231364701848243126999929738482630407146e-1;
  a1210 = .2711204726320932916455631550463654973432;
  a1211 = 1.308133749422980744437146904349994472286;
  a131 = -.5074635056416974879347823927726392374259;
  a136 = -6.631342198657237090355284142048733580937;
  a137 = -.252748010090880105270020973014860316405;
  a138 = -.4952612380036095562991116175550167835424;
  a139 = .293252554525388690285739720360003594753;
  a1310 = 1.440108693768280908474851998204423941413;
  a1311 = 6.237934498647055877243623886838802127716;
  a1312 = .7270192054526987638549835199880202544289;
  a141 = .6130118256955931701496387847232542148725;
  a146 = 9.088803891640463313341034206647776279557;
  a147 = -.407378815629344868103315381138325162923;
  a148 = 1.790733389490374687043894756399015035977;
  a149 = .714927166761755073724875250629602731782;
  a1410 = -1.438580857841722850237810322456327208949;
  a1411 = -8.263329312064740580595954649844133476994;
  a1412 = -1.537570570808865115231450725068826856201;
  a1413 = .3453832827564871699090880801079644428793;
  a151 = -1.211697910343873872490625222495537087293;
  a156 = -19.05581871559595277753334676575234493500;
  a157 = 1.26306067538987510135943101851905310045;
  a158 = -6.913916969178458046793476128409110926069;
  a159 = -.676462266509498065300115641383621209887;
  a1510 = 3.367860445026607887090352785684064242560;
  a1511 = 18.00675164312590810020103216906571965203;
  a1512 = 6.838828926794279896350389904990814350968;
  a1513 = -1.031516451921950498420447675652291096155;
  a1514 = .4129106232130622755368055554332539084021;
  a161 = 2.157389007494053627033175177985666660692;
  a166 = 23.80712219809580523172312179815279712750;
  a167 = .88627792492165554903036801415266308369;
  a168 = 13.13913039759876381480201677314222971522;
  a169 = -2.604415709287714883747369630937415176632;
  a1610 = -5.193859949783872300189266203049579105962;
  a1611 = -20.41234071154150778768154893536134356354;
  a1612 = -12.30085625250572261314889445241581039623;
  a1613 = 1.521553095008539362178397458330791655267;
  b1 = .1458885278405539719101539582255752917034e-1;
  b8 = .2024197887889332650566666683195656097825e-2;
  b9 = .2178047084569716646796256135839225745895;
  b10 = .1274895340854389692868677968654808668201;
  b11 = .2244617745463131861258531547137348031621;
  b12 = .1787254491259903095100090833796054447157;
  b13 = .7594344758096557172908303416513173076283e-1;
  b14 = .1294845879197561516869001434704642286297;
  b15 = .2947744761261941714007911131590716605202e-1;
  d1 = .2034666655224434599707885098832906986649e-1;
  d8 = 1.069617650982700109541321983413338230042;
  d9 = .7680834711303187278673130261850350530338e-1;
  d10 = .1130778186885240437498706751119241126785;
  d11 = .2552587357981962194892445789565762186511;
  d12 = -.9825898086919164036191607912120918904022;
  d13 = .3981545824421514217762002137442675068982;
  d16 = .4932600711506839027871318637915324696208e-1;
  b16 = 0.0;
  d14 = 0.0;
  d15 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  T.A[12*s+11]=a1312;
  T.A[13*s+0]=a141;
  T.A[13*s+5]=a146;
  T.A[13*s+6]=a147;
  T.A[13*s+7]=a148;
  T.A[13*s+8]=a149;
  T.A[13*s+9]=a1410;
  T.A[13*s+10]=a1411;
  T.A[13*s+11]=a1412;
  T.A[13*s+12]=a1413;
  T.A[14*s+0]=a151;
  T.A[14*s+5]=a156;
  T.A[14*s+6]=a157;
  T.A[14*s+7]=a158;
  T.A[14*s+8]=a159;
  T.A[14*s+9]=a1510;
  T.A[14*s+10]=a1511;
  T.A[14*s+11]=a1512;
  T.A[14*s+12]=a1513;
  T.A[14*s+13]=a1514;
  T.A[15*s+0]=a161;
  T.A[15*s+5]=a166;
  T.A[15*s+6]=a167;
  T.A[15*s+7]=a168;
  T.A[15*s+8]=a169;
  T.A[15*s+9]=a1610;
  T.A[15*s+10]=a1611;
  T.A[15*s+11]=a1612;
  T.A[15*s+12]=a1613;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=0.0;
  T.b[6]=0.0;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.b[13]=b14;
  T.b[14]=b15;
  T.b[15]=b16;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=0.0;
  T.bhat[6]=0.0;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
  T.bhat[13]=d14;
  T.bhat[14]=d15;
  T.bhat[15]=d16;
}

static void rksTableauS98(rksTableau &T) {
  const int s = 16; T.s = s; T.adaptive = 1; T.errOrder = 8;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a74, a75, a76, a81, a86, a87, a91, a96, a97, a98, a101, a106, a107, a108, a109, a111, a116, a117, a118, a119, a1110, a121, a126, a127, a128, a129, a1210, a1211, a131, a136, a137, a138, a139, a1310, a1311, a1312, a141, a146, a147, a148, a149, a1410, a1411, a1412, a1413, a151, a156, a157, a158, a159, a1510, a1511, a1512, a1513, a1514, a161, a166, a167, a168, a169, a1610, a1611, a1612, a1613, b1, b8, b9, b10, b11, b12, b13, b14, b15, d1, d8, d9, d10, d11, d12, d13, d16, b16, d14, d15;
  c2 = 2.00000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  c3 = 9.66220283800537781651565568869531890039129854640901213159807057025513165826690659288e-2;
  c4 = 1.44933042570080667247734835330429783505869478196135181973971058553826974874003598893e-1;
  c5 = 3.11111111111111111111111111111111111111111111111111111111111111111111111111111111111e-1;
  c6 = 3.51498773299780208885007017507765081905783415195317477342800010797639951702224305104e-1;
  c7 = 1.47701226700219791114992982492234918094216584804682522657199989202360048297775694896e-1;
  c8 = 4.16000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c9 = 3.12000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c10 = 1.05000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  c11 = 5.87002096436058700209643605870020964360587002096436058700209643605870020964360587002e-1;
  c12 = 6.83818495973559057197009883791356922634313147538567178788853669325746483233925079300e-1;
  c13 = 8.79003558718861209964412811387900355871886120996441281138790035587188612099644128114e-1;
  c14 = 9.16000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  a21 = 2.00000000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  a31 = -1.3677338082684416521484257374903677512719677066274978006646616817250635870287502595e-1;
  a32 = 2.33395409206897943379999130635989964131109756126839901382446873875057675285544091877e-1;
  a41 = 3.62332606425201668119337088326074458764673695490337954934927646384567437185008997233e-2;
  a43 = 1.08699781927560500435801126497822337629402108647101386480478293915370231155502699170e-1;
  a51 = 1.93101200330205381129632393375249728863798118161035750556803447904498441444456645259e-1;
  a53 = -6.4770998560312388589569152951216097849732130945128175207869408225841451367147303820e-1;
  a54 = 7.65719896384029615877170247248022360744634302401357112633001745465027183338127504049e-1;
  a61 = 4.77439440833806232500188714529354943400951843563751259289256424427344686758530074484e-2;
  a64 = 1.96932175795354826650179476488669790168656892603887237546677770684075728220363810345e-1;
  a65 = 1.06822653421044758984808669566159797397031338235055113867196597670829754806007487311e-1;
  a71 = 5.35653520740100302121103332775188587103176832181869303040831220432325640950516524391e-2;
  a74 = 1.20625588947728919056244246899316675798038285391744744331826391017415590396850177402e-1;
  a75 = -6.7750756225149701080673518583857606163917837693866673698932599085495183512419090449e-2;
  a76 = 4.12610419036305429273119208992569897497784538886175217202230752272070773182929555031e-2;
  a81 = 4.62222222222222222222222222222222222222222222222222222222222222222222222222222222222e-2;
  a86 = 1.56583674083394386420831384025620810581726829112411811849222219222877791193826581916e-1;
  a87 = 2.13194103694383391356946393752156967196050948665365965928555558554899986583951195862e-1;
  a91 = 4.63125000000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  a96 = 7.51502406476090389349073807809004819189545346828427154623710864742465169193996381623e-2;
  a97 = 2.12474759352390961065092619219099518081045465317157284537628913525753483080600361838e-1;
  a98 = -2.1937500000000000000000000000000000000000000000000000000000000000000000000000000000e-2;
  a101 = 4.71370805167570855745159483211372150835054795000175063898322887854066734358040684850e-2;
  a106 = 2.09088767177245700619002712371830037495557938247264604576291946948488435273048771651e-1;
  a107 = 1.00170199845336072001441249674316806102355300084726572203232579503666601315305977833e-1;
  a108 = -4.1746535935132619792943174258604390602569938027379993697699660376037253597563110535e-2;
  a109 = -2.0964951160420623840201673610867966807884877980462868947165715486152445642659570743e-1;
  a111 = 9.62506254804954308501791978456184013295337530291135748904639061412584879741027523020e-2;
  a116 = -3.2889823940483436676586225555059183413399004899899513082733172247062419719368801086e0;
  a117 = 1.58120592819482939980472941802825082360990331356773185743485106618875616830910113284e-1;
  a118 = 1.38472174110756949611966846419084262312633249684929784010383708600379765979254809143e0;
  a119 = 2.38689153107685450091794555753665319888363091085120276623574076954818022830367973855e0;
  a1110 = -1.5000000000000000000000000000000000000000000000000000000000000000000000000000000000e-1;
  a121 = -4.1407181780829333826951338677923830282957788550316015757490123469403067795923041646e-1;
  a126 = 2.79362329285576366638884698134000509392713675687003189893845038096478481618920557795e0;
  a127 = -3.9420148894844370214218382106635188284591903158605020970611433089765204398968285433e0;
  a128 = -1.8050833493703232778218655623536664178736717202844850014226094729833601601906484738e0;
  a129 = -8.2411175299103457871280197945625000732690195817565658068273621079502124203745163579e-2;
  a1210 = 3.76724363839931467375143823390080271058360595326428259518770377058250406454692000855e0;
  a1211 = 3.66532796680637812441222026292597668018700554869965598789627155511871004748252089945e-1;
  a131 = 2.57118803244482153847626462807856681730780184786345438393372906485058163365316011894e-1;
  a136 = 3.22367498544415690028414563287198307551779583958713835991093894064695605463707833852e0;
  a137 = 3.95644809715768343577645732188103093651324923567481376817610794743631132119165042871e0;
  a138 = 1.90269610866406552179805731106267938194469931412329236667880503143330311252532589886e0;
  a139 = -5.2391055401717914982701824261828721839527015786103641427597273886467794920713496236e0;
  a1310 = -2.6899481313272987041207973348420457663574444684767403019558990390373975528574973750e0;
  a1311 = -1.6326510182468950685438619936368781848861272140333241607130497353632849427158674841e0;
  a1312 = 1.10077025395445846919296783742614641536163480794527995340824137263302194802498793285e0;
  a141 = 4.08583369788787674770196821641246834353608695949114599877074475305978651235285469124e0;
  a146 = -2.2271040812705511368267126642675337195508778742206509440033684681620630123740836983e1;
  a147 = 2.14948200377041255968873743962135304520531006690258805591495319234107676337073934764e1;
  a148 = -1.2574667847036709064117981533059871307009863478980024060295805266513615915043138735e1;
  a149 = 2.54025651049869277589813851770135045700861797774341329374584242615333576062269117780e1;
  a1410 = -2.4865730830721493170953431486117421078941351149011810402690627604263543458446989032e1;
  a1411 = 1.82479073749516085147180202069097519978378918397970543443181892233252604987304701845e1;
  a1412 = -9.4291787815393124480125158715016538695138573450523345127986553789149287859337960255e0;
  a1413 = 8.25492056472487433062307536805028087460591469502464576121882769983546032147130645121e-1;
  a151 = -2.0602212042704652199244807912284589395147988173289204904243925620741467390453149503e0;
  a156 = -2.3055564596876178326198929677498175792624263330011017577312032231421737767030559692e0;
  a157 = -1.8379647671170325920306936223300488315443940076512542750939032591666020672221106554e1;
  a158 = 9.80063543421329918398827905876150569685353384114341904048681595591515045410441012888e-1;
  a159 = 6.44589158658994320826489839207649833491520903216082715269469597755156737618623074351e0;
  a1510 = 1.69192749452203241565519919050371391213914448119219462829620457478758678378197682618e1;
  a1511 = -2.0988423575394962681126735718162381051185885412147351718156202906941587529583151802e0;
  a1512 = 1.32027878990329475096946854788916481157084451169381843791872814006897180501111028426e0;
  a1513 = 1.29768753187020321933985725604863066333871547241993002451338834678114639100987388885e-1;
  a1514 = 4.89900743459928848448110776111870354430304809243733908347583718104632373992549620659e-2;
  a161 = -1.2158274587431439669291103556715073078571331121887250332403319959071088711190342783e-1;
  a166 = -1.3009191687212087953937489554001604506419126739405937395616109697496506450739882872e1;
  a167 = -1.0172115280165551555631444224047088737887421449467885159610413901710577571727155577e1;
  a168 = -9.1376040597684553208500728397293269920338377345009007549988772949417238792208104990e0;
  a169 = 2.20271190656799492286306009834337099718633750373570345566819174880669811072575443296e1;
  a1610 = 6.06122452890837591335001768945892462299766158185132002339883804559430763723850436801e0;
  a1611 = 9.49756499938619826937987552815088909338119306846899551533354213035888156520270940573e0;
  a1612 = -4.7967103151651448708172582989203880579450635055043503690520505457817795045514591312e0;
  a1613 = 6.51295494211030686568681751222035336828933052420596087187186975501127983652453403424e-1;
  b1 = 2.96275053023277765257147159271391983963018401625397769109581065606146193195367573656e-2;
  b8 = 7.80838132003046860057542718319735163112389396834314055277737601781852412778315993413e-2;
  b9 = 2.07620199958252939386280504658544994598465755307530373969021587633956273026455132933e-1;
  b10 = 1.69689395533794848088161847963068489566652421567777796351282799972583366756393555465e-1;
  b11 = 1.51705614234815145844037646207281350814128482549399473556449026004053183774844489538e-1;
  b12 = 1.44245948285115701641251053349684808091393327368972824014235232832191510849137389325e-1;
  b13 = 1.66761530218905580484076634450811227110090575704792045335148871651256233134779966440e-1;
  b14 = 1.92534285621243594762130197569399694644610012365298208475543983102390200679811369880e-2;
  b15 = 3.30125647043589625485103058545564456472676564190264834875762168569205517930399726031e-2;
  d1 = 2.98603077327271866543455374899994274225585056921637520652930266673233165901602953218e-2;
  d8 = 6.57755975911182224511842125160987804765191941684313617933281553333569953681029565846e-2;
  d9 = 2.13956779658119140898029891022265645728716698807857092033794693592749028494691587115e-1;
  d10 = 1.68721578796992869389907340999574096253253203866862151079585793545930377244267574219e-1;
  d11 = 1.71768360736786458177293485795001832452979570477507671772344473084105210367066149609e-1;
  d12 = 1.25492680790662476798676554483056276508051861757938232565244001424475853044387070116e-1;
  d13 = 1.89263652417235144013217502395309419983557239873730317305944966458721798394126740078e-1;
  d16 = 3.51610422763585016173454752986945211743637253555094213844648898933374204971976269572e-2;
  b16 = 0.0;
  d14 = 0.0;
  d15 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  T.A[12*s+11]=a1312;
  T.A[13*s+0]=a141;
  T.A[13*s+5]=a146;
  T.A[13*s+6]=a147;
  T.A[13*s+7]=a148;
  T.A[13*s+8]=a149;
  T.A[13*s+9]=a1410;
  T.A[13*s+10]=a1411;
  T.A[13*s+11]=a1412;
  T.A[13*s+12]=a1413;
  T.A[14*s+0]=a151;
  T.A[14*s+5]=a156;
  T.A[14*s+6]=a157;
  T.A[14*s+7]=a158;
  T.A[14*s+8]=a159;
  T.A[14*s+9]=a1510;
  T.A[14*s+10]=a1511;
  T.A[14*s+11]=a1512;
  T.A[14*s+12]=a1513;
  T.A[14*s+13]=a1514;
  T.A[15*s+0]=a161;
  T.A[15*s+5]=a166;
  T.A[15*s+6]=a167;
  T.A[15*s+7]=a168;
  T.A[15*s+8]=a169;
  T.A[15*s+9]=a1610;
  T.A[15*s+10]=a1611;
  T.A[15*s+11]=a1612;
  T.A[15*s+12]=a1613;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=0.0;
  T.b[6]=0.0;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.b[13]=b14;
  T.b[14]=b15;
  T.b[15]=b16;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=0.0;
  T.bhat[6]=0.0;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
  T.bhat[13]=d14;
  T.bhat[14]=d15;
  T.bhat[15]=d16;
}


static void rksTableauC108(rksTableau &T) {
  const int s = 21; T.s = s; T.adaptive = 1; T.errOrder = 8;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, a81, a82, a83, a84, a85, a86, a87, a91, a92, a93, a94, a95, a96, a97, a98, a101, a102, a103, a104, a105, a106, a107, a108, a109, a111, a112, a113, a114, a115, a116, a117, a118, a119, a1110, a121, a122, a123, a124, a125, a126, a127, a128, a129, a1210, a1211, a131, a132, a133, a134, a135, a136, a137, a138, a139, a1310, a1311, a1312, a141, a142, a143, a144, a145, a146, a147, a148, a149, a1410, a1411, a1412, a1413, a151, a152, a153, a154, a155, a156, a157, a158, a159, a1510, a1511, a1512, a1513, a1514, a161, a162, a163, a164, a165, a166, a167, a168, a169, a1610, a1611, a1612, a1613, a1614, a1615, a171, a172, a173, a174, a175, a176, a177, a178, a179, a1710, a1711, a1712, a1713, a1714, a1715, a1716, a181, a182, a183, a184, a185, a186, a187, a188, a189, a1810, a1811, a1812, a1813, a1814, a1815, a1816, a1817, a191, a192, a193, a194, a195, a196, a197, a198, a199, a1910, a1911, a1912, a1913, a1914, a1915, a1916, a1917, a1918, a201, a202, a203, a204, a205, a206, a207, a208, a209, a2010, a2011, a2012, a2013, a2014, a2015, a2016, a2017, a2018, a2019, a211, a212, a213, a214, a215, a216, a217, a218, a219, a2110, a2111, a2112, a2113, a2114, a2115, a2116, a2117, a2118, a2119, a2120, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21;
  c2 = .1452518960316150517617548528770033320314511251329947060838468741983976455607179673401;
  c3 = .1452518960316150517617548528770033320314511251329947060838468741983976455607179673401;
  c4 = .2178778440474225776426322793155049980471766876994920591257703112975964683410769510101;
  c5 = .5446946101185564441065806982887624951179417192487301478144257782439911708526923775252;
  c6 = .6536335321422677329278968379465149941415300630984761773773109338927894050232308530303;
  c7 = .2746594919905254008808021630247618520892150865127407293922085868737635475402543533498;
  c8 = .7735775201106609448405825008093973718589542913426807556412662673054607938029043386501;
  c9 = .5801831400829957086304368756070480288942157185070105667309497004790955953521782539876;
  c10 = .1174723380352676535744985130203309248171321557319478803362088220814723414805867429383;
  c11 = .3573842417596774518429245029795604640404982636367873040901247917361510345429002009092;
  c12 = .6426157582403225481570754970204395359595017363632126959098752082638489654570997990908;
  c13 = .1174723380352676535744985130203309248171321557319478803362088220814723414805867429383;
  c14 = .8825276619647323464255014869796690751828678442680521196637911779185276585194132570617;
  c15 = .3573842417596774518429245029795604640404982636367873040901247917361510345429002009092;
  c16 = .6426157582403225481570754970204395359595017363632126959098752082638489654570997990908;
  c17 = .8825276619647323464255014869796690751828678442680521196637911779185276585194132570617;
  c18 = 1;
  c19 = .3510848126232741617357001972386587771203155818540433925049309664694280078895463510848;
  c20 = .6157407407407407407407407407407407407407407407407407407407407407407407407407407407407;
  c21 = 1;
  a21 = .1452518960316150517617548528770033320314511251329947060838468741983976455607179673401;
  a31 = .7262594801580752588087742643850166601572556256649735304192343709919882278035898367003e-1;
  a32 = .7262594801580752588087742643850166601572556256649735304192343709919882278035898367003e-1;
  a41 = .5446946101185564441065806982887624951179417192487301478144257782439911708526923775252e-1;
  a42 = 0;
  a43 = .1634083830355669332319742094866287485353825157746190443443277334731973512558077132576;
  a51 = .5446946101185564441065806982887624951179417192487301478144257782439911708526923775252;
  a52 = 0;
  a53 = -2.042604787944586665399677618582859356692281447182738054304096668414966890697596415720;
  a54 = 2.042604787944586665399677618582859356692281447182738054304096668414966890697596415720;
  a61 = .6536335321422677329278968379465149941415300630984761773773109338927894050232308530303e-1;
  a62 = 0;
  a63 = 0;
  a64 = .3268167660711338664639484189732574970707650315492380886886554669463947025116154265151;
  a65 = .2614534128569070931711587351786059976566120252393904709509243735571157620092923412121;
  a71 = .8233707757482716585173454344310125296066814318521742241762319051772963627695955263034e-1;
  a72 = 0;
  a73 = 0;
  a74 = .2119171963202803561687843468555305553175658807629274312902985594840086570224567152664;
  a75 = -.3997343508054218311577932550061320162379840049816347807630118786107674477850206579628e-1;
  a76 = .2037865317596006197606259822674324543477946306275935376058802473310199901934015124941e-1;
  a81 = .8595305779007343831562027786771081909543936570474230618236291858949564375587825985001e-1;
  a82 = 0;
  a83 = 0;
  a84 = 0;
  a85 = 0;
  a86 = .2911769478058850960337179621761553399856026049598393013981874594942289837064329700000;
  a87 = .3964475145147024104912442607655312127779123206780991480607158892217361663405931088001;
  a91 = .8612093485606967549983047372292119178898514571588438099912534616486575243508895957628e-1;
  a92 = 0;
  a93 = 0;
  a94 = 0;
  a95 = 0;
  a96 = .1397464826824442089036313891001189801074425314582326737716288563521183595455090268480;
  a97 = .3951098495815674599900526056001284215294125840404176924334653987770478924197803010468;
  a98 = -.4079412703708563576307759281612056453162454270752418047326990081493640904820003348350e-1;
  a101 = .7233144422337948077616348229119326315582930871089020733092900891206129381937795204778e-1;
  a102 = 0;
  a103 = 0;
  a104 = 0;
  a105 = 0;
  a106 = .2200276284689998102140972735735070061373242800181187459951219347361114857342828430157;
  a107 = .8789533425436734013369780264792573637952226487753296416823846876217040795688489371334e-1;
  a108 = -.4445383996260350863990674880611108986832860648196030000580004690002268108984238641730e-1;
  a109 = -.2183282289488754689095532966861839909872150913926337371522805434288481649401165594213;
  a111 = .8947100936731114228785441966773836169071038390882857211057269158522704971585365845223e-1;
  a112 = 0;
  a113 = 0;
  a114 = 0;
  a115 = 0;
  a116 = .3946008170285561860741397654755022300929434262701385530048127140223687993778661654316;
  a117 = .3443011367963333487713764986067104675654371857504670290688086760696354596195596354011;
  a118 = -.7946682664292661290694938113119430997053815140863772328764150866582492425892231395780e-1;
  a119 = -.3915218947895966123834967996391962853380545808840091268064277812752553499114569444180;
  a1110 = 0;
  a121 = .3210006877963209212945282736072241886741425314298532400216927262619488479186214523312e-1;
  a122 = 0;
  a123 = 0;
  a124 = 0;
  a125 = 0;
  a126 = 0;
  a127 = 0;
  a128 = -.1846375997512050141835163881753227910996323204749769226655464078048769505209525299752e-3;
  a129 = .1560894025313219860759149162557283383430181475726228517203663063649626288079337909898;
  a1210 = .1934496857654560252749984220385188727138526287670744309970093278715606577140084022992;
  a1211 = .2611612387636636496908928477536452288263163392010050661129958478089356710938164130987;
  a131 = .4423749328524996327035388417792688154433173133294892285295756457561276315648477233732e-1;
  a132 = 0;
  a133 = 0;
  a134 = 0;
  a135 = 0;
  a136 = 0;
  a137 = 0;
  a138 = .4640774434539039636406222168781981616534115643208114455689698789119941732444857047798e-2;
  a139 = .4704660282615136532130927218172390570903230981414159347904277946537920001824903276586e-1;
  a1310 = .8620749948011488160369445167416002799205317397013619044391270706339561700281526529703e-1;
  a1311 = -.2607983024682138093233254079066687623148682426317395111719299641390118652802949600035e-1;
  a1312 = -.3858020174396621532493277639159499581333235076531298977820093139813399390137768850940e-1;
  a141 = .2318046717429411567006043539613275607940758021709332569729352990777336390158311630529e-1;
  a142 = 0;
  a143 = 0;
  a144 = 0;
  a145 = 0;
  a146 = 0;
  a147 = 0;
  a148 = .3197856784116367067302124322582100058864027838197120089129330601737324659881765852593;
  a149 = .5933233331841898686063939886797828376866051205773280426848164018120869674204443797948;
  a1410 = -1.937519548878479314706815782408229952008442222624773168771865465659822020582450444783;
  a1411 = .1803950557030502357344063195737827904476240180662764468232042537858892203518134072359;
  a1412 = -.4554014298857220726863505256926549022316460712353658688873150702827663762861750674926;
  a1413 = 2.158764106255762807077594619172645539322916635447781333204724468181634037726021280742;
  a151 = .2624364325798105891527733985858552391723553030719144065844544880498188553839263944447e-1;
  a152 = 0;
  a153 = 0;
  a154 = 0;
  a155 = 0;
  a156 = 0;
  a157 = 0;
  a158 = .4863139423867266106526843913609225996253073727381961544415263239431571586043622332760e-1;
  a159 = .4274382538346478867636942429421724367591866585774144180215122660980822123988151132213e-1;
  a1510 = -.4862259869465547771298976981868643277396586803130813159599600102115609499827986711663;
  a1511 = .1326047194917652331781527125743684254490968718259563958293167893998110899691451568372;
  a1512 = -.9402962152946515651634831658142934852383791641671387741034606371378082209616938685225e-1;
  a1513 = .6993864679941022534190304512277131176659196396138275832136258135631963192299339871223;
  a1514 = -.1197020013028860976492784934312243036670658451195397948726104511062042521592125912599e-1;
  a161 = .5568066641536216461090823068917803436066365804361903532125349474551476120813558125830e-1;
  a162 = 0;
  a163 = 0;
  a164 = 0;
  a165 = 0;
  a166 = 0;
  a167 = 0;
  a168 = -.4324853319508358432896036654421685136736530810118924113940744870078036705505610668088;
  a169 = -.9979726994172038714656907882931844552238093285811791155499130927685987422432191170216;
  a1610 = 2.707893755718926115778725270396739994070337972517006747100005607751792006959604868323;
  a1611 = -1.024823023512132929313567156576969954855232272749038347671818195935585095295127839150;
  a1612 = 1.334565206642246959252239602313589265188981560552694580059808406200559397799055652161;
  a1613 = -2.587748998830690939658228913150922979184368065866213469477796089200252812362701917187;
  a1614 = .8992773696348355846430438306111181223414632598285854300924423251352733205187087732678e-1;
  a1615 = 1.497578446211167333777988534023066333042434967475357134513165331964695787890042760189;
  a171 = -.8434891199686377639125188391985671318383858641413517143104162188088468627447515172982e-3;
  a172 = 0;
  a173 = 0;
  a174 = 0;
  a175 = 0;
  a176 = 0;
  a177 = 0;
  a178 = .7602144218856081893754106886111596435015500427480120290148318740899211421773423234728;
  a179 = 1.769083927820959377467464871522349066447068428702073590698445112684989184432409492025;
  a1710 = -4.499239797622297101452915424261016593995695456495268863455643396071539024609271033574;
  a1711 = 1.490558190212043468817221563278239942209691100326719140478588601720867838040211450448;
  a1712 = -2.552203480132132516997563217309689292804518121743365818482497611667126218719069737195;
  a1713 = 4.795167551528575994217413424533259845001657006088189480440731104737960266616292993321;
  a1714 = -.9161854401769482236671414092387917470686251714192236693920061138984202381209109248553e-1;
  a1715 = -1.525735678746850818217653470352135651821164556169070505816135230784807058389577753184;
  a1716 = .7371445601564892133467497107205798584829803038168267854389817508169123996459113657504;
  a181 = .1017366974111576638766809656369828971944080018220332809259398740674738807023371082700;
  a182 = 0;
  a183 = 0;
  a184 = 0;
  a185 = 0;
  a186 = 0;
  a187 = 0;
  a188 = -1.696217553209432810711666838709742166182992092906177246174096517233561845662947862824;
  a189 = -3.825235846211624254528740857512255693551264719132875740261231165548583482101116676418;
  a1810 = 9.754768979885866648856431516333641627109105703674164986615824197909762854575668793816;
  a1811 = -2.520767789227152291196336314591227486393143379933686189126240710041836742414125694941;
  a1812 = 5.472417145227780046950992000565734793413395536531652419585004300790370984185945495978;
  a1813 = -9.781098113458736121002383874108051372067873053264954833376114258940736444388841687929;
  a1814 = .3189152692455334369024560213486753019540464785641163242047782111839399471147176681561;
  a1815 = 3.447227036527756718156475010324322155277035924051392880570525223655410460762027138915;
  a1816 = -.6051983612219277832241707671295607127814820499715293613761402732652780120810041653591;
  a1817 = .3334525350307787459202631378414806560287636505658634784117511174230383993073398823363;
  a191 = -.1012987737478284424676828232882617689682012456457322189102956361570156443805900941944;
  a192 = 0;
  a193 = 0;
  a194 = 0;
  a195 = 0;
  a196 = -.2409389328948775401304659380663043147167897928467308244359962659633933617326533285822e-1;
  a197 = -.6679880790275182076676283582867036095782150170801495251932447614617249253864579543857;
  a198 = 1.600262798493100648047998296908183265688507618079976446601985464092263571149154964705;
  a199 = 3.706958893826695766827011000213884379914407774639901049574259778345288538246990591819;
  a1910 = -8.581755560147929325446798534254342948628755672447282004336563881429983605741487870996;
  a1911 = .5607314974300953986559644699099897253584501767603091982484141468619493221310582281877e-1;
  a1912 = -4.547761497422899514520768375507009011918601407646237921467449197008085790456674001879;
  a1913 = 9.255775439941294621826928846245618922061242300726600002589630404152665447900428712156;
  a1914 = -.3450876657451631707159097079770789925142348071643902737346329921538351794816584861003;
  a1915 = 0;
  a1916 = 0;
  a1917 = 0;
  a1918 = 0;
  a201 = .3826909723812638609001259641818040193828105314579492422836388985468479567237561247336e-1;
  a202 = 0;
  a203 = 0;
  a204 = 0;
  a205 = 0;
  a206 = .7786978965202527814624406274393101840018332461648638653990700950184871893714491273096;
  a207 = .4859454140913448249612202172501868752761599132465501266008866131088163955018926230543;
  a208 = 1.814925350154666364151014269029611427420766367555858499108920245656959783343309816408;
  a209 = 4.551165245704657956889158854062833952834232753889932986749613143631480116805870313264;
  a2010 = -7.173770670344544101351160462586215092596352548535380880420409450623251883641801862305;
  a2011 = -.3943009017000923237232456850787591816773705728833192412204243696911216045268772747196;
  a2012 = -6.036544185898100312430357626685382432626027303329497026597513524312479466987506315664;
  a2013 = 7.338904299721887701527380004651998686389416058019429466200740313593568240326087171554;
  a2014 = -.4143158595971836110248598960027762194900538872022960061452263646470675916118824501965;
  a2015 = 0;
  a2016 = 0;
  a2017 = 0;
  a2018 = 0;
  a2019 = -.3732349451502749258108621577582478607301443393311959731632798508493352335121760204375;
  a211 = .2162339046022045866878628785550588026780578552494608097931198882276791962912244674840e-1;
  a212 = 0;
  a213 = 0;
  a214 = 0;
  a215 = 0;
  a216 = .4611834700744369218866370212060318930941322187829670117414118166503940620998117275429;
  a217 = .1940797759547798743610542713744618433967649025379792966207862125676964319674160574624;
  a218 = .7041001229739959807963554405302474570280838416767002383409508232534658577705201658489;
  a219 = 2.877431096792763528910415905652149398266490601780194388811216042455337979365709745445;
  a2110 = 0;
  a2111 = -.4332742088749107411735902392606181444105337491234912425673655059805456011404518143074;
  a2112 = -2.234178753588834452567105459024473991729105867012210449973082203886376638514123583334;
  a2113 = .2235678086885984010238782832657956960650576194069632574873732156942360146780276407657;
  a2114 = .1293532338308457711442786069651741293532338308457711442786069651741293532338308457711;
  a2115 = 0;
  a2116 = 0;
  a2117 = 0;
  a2118 = 0;
  a2119 = .1418136968194278394808045812385429206355105705182818920178205766092934777719870449624;
  a2120 = -1.085699633131323582531514699802817081967439754938101617737029931360398856861850276906;
  b1 = .3333333333333333333333333333333333333333333333333333333333333333333333333333333333333e-1;
  b2 = 0;
  b3 = 0;
  b4 = 0;
  b5 = 0;
  b6 = 0;
  b7 = 0;
  b8 = 0;
  b9 = 0;
  b10 = 0;
  b11 = 0;
  b12 = .1387145942588715882541801312803271702142521598590204181697361204933422401935856968980;
  b13 = .1892374781489234901583064041060123262381623469486258303271944256799821862794952728707;
  b14 = .9461873907446174507915320205300616311908117347431291516359721283999109313974763643533e-1;
  b15 = .2774291885177431765083602625606543404285043197180408363394722409866844803871713937960;
  b16 = .1387145942588715882541801312803271702142521598590204181697361204933422401935856968980;
  b17 = .9461873907446174507915320205300616311908117347431291516359721283999109313974763643533e-1;
  b18 = .3333333333333333333333333333333333333333333333333333333333333333333333333333333333333e-1;
  b19 = 0;
  b20 = 0;
  b21 = 0;
  d1 = .3339829895931337572271945815422988633728883413227543303554098429202731077409488318421e-1;
  d2 = 0;
  d3 = 0;
  d4 = 0;
  d5 = 0;
  d6 = 0;
  d7 = 0;
  d8 = 0;
  d9 = .5024509803921568627450980392156862745098039215686274509803921568627450980392156862745e-1;
  d10 = -.1423859191318858946753152353981644782061337055184060977838998119673893661279423564924;
  d11 = .2126013199429258434998789109063801828540550730541648287733608913970804891935883227446;
  d12 = .3254854965632843133622967470840062095221514741629108993207015882688341071771214986692;
  d13 = .3312629399585921325051759834368530020703933747412008281573498964803312629399585921325;
  d14 = .1887845809230650005639203350759631573314744764356665687950807917985316096487827551997;
  d15 = 0;
  d16 = 0;
  d17 = 0;
  d18 = 0;
  d19 = .6159811094287144604404508847679200761569154839698701533406779753675999506388462962070e-1;
  d20 = -.9440109660594088037957791636147830082275023098021053120999763935859315478744850552959e-1;
  d21 = .3341117040855897708234682470384970584684876341854831047975628586614323631403861184369e-1;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+1]=a82;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+1]=a92;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+1]=a102;
  T.A[9*s+2]=a103;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+1]=a112;
  T.A[10*s+2]=a113;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+1]=a122;
  T.A[11*s+2]=a123;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+1]=a132;
  T.A[12*s+2]=a133;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  T.A[12*s+11]=a1312;
  T.A[13*s+0]=a141;
  T.A[13*s+1]=a142;
  T.A[13*s+2]=a143;
  T.A[13*s+3]=a144;
  T.A[13*s+4]=a145;
  T.A[13*s+5]=a146;
  T.A[13*s+6]=a147;
  T.A[13*s+7]=a148;
  T.A[13*s+8]=a149;
  T.A[13*s+9]=a1410;
  T.A[13*s+10]=a1411;
  T.A[13*s+11]=a1412;
  T.A[13*s+12]=a1413;
  T.A[14*s+0]=a151;
  T.A[14*s+1]=a152;
  T.A[14*s+2]=a153;
  T.A[14*s+3]=a154;
  T.A[14*s+4]=a155;
  T.A[14*s+5]=a156;
  T.A[14*s+6]=a157;
  T.A[14*s+7]=a158;
  T.A[14*s+8]=a159;
  T.A[14*s+9]=a1510;
  T.A[14*s+10]=a1511;
  T.A[14*s+11]=a1512;
  T.A[14*s+12]=a1513;
  T.A[14*s+13]=a1514;
  T.A[15*s+0]=a161;
  T.A[15*s+1]=a162;
  T.A[15*s+2]=a163;
  T.A[15*s+3]=a164;
  T.A[15*s+4]=a165;
  T.A[15*s+5]=a166;
  T.A[15*s+6]=a167;
  T.A[15*s+7]=a168;
  T.A[15*s+8]=a169;
  T.A[15*s+9]=a1610;
  T.A[15*s+10]=a1611;
  T.A[15*s+11]=a1612;
  T.A[15*s+12]=a1613;
  T.A[15*s+13]=a1614;
  T.A[15*s+14]=a1615;
  T.A[16*s+0]=a171;
  T.A[16*s+1]=a172;
  T.A[16*s+2]=a173;
  T.A[16*s+3]=a174;
  T.A[16*s+4]=a175;
  T.A[16*s+5]=a176;
  T.A[16*s+6]=a177;
  T.A[16*s+7]=a178;
  T.A[16*s+8]=a179;
  T.A[16*s+9]=a1710;
  T.A[16*s+10]=a1711;
  T.A[16*s+11]=a1712;
  T.A[16*s+12]=a1713;
  T.A[16*s+13]=a1714;
  T.A[16*s+14]=a1715;
  T.A[16*s+15]=a1716;
  T.A[17*s+0]=a181;
  T.A[17*s+1]=a182;
  T.A[17*s+2]=a183;
  T.A[17*s+3]=a184;
  T.A[17*s+4]=a185;
  T.A[17*s+5]=a186;
  T.A[17*s+6]=a187;
  T.A[17*s+7]=a188;
  T.A[17*s+8]=a189;
  T.A[17*s+9]=a1810;
  T.A[17*s+10]=a1811;
  T.A[17*s+11]=a1812;
  T.A[17*s+12]=a1813;
  T.A[17*s+13]=a1814;
  T.A[17*s+14]=a1815;
  T.A[17*s+15]=a1816;
  T.A[17*s+16]=a1817;
  T.A[18*s+0]=a191;
  T.A[18*s+1]=a192;
  T.A[18*s+2]=a193;
  T.A[18*s+3]=a194;
  T.A[18*s+4]=a195;
  T.A[18*s+5]=a196;
  T.A[18*s+6]=a197;
  T.A[18*s+7]=a198;
  T.A[18*s+8]=a199;
  T.A[18*s+9]=a1910;
  T.A[18*s+10]=a1911;
  T.A[18*s+11]=a1912;
  T.A[18*s+12]=a1913;
  T.A[18*s+13]=a1914;
  T.A[18*s+14]=a1915;
  T.A[18*s+15]=a1916;
  T.A[18*s+16]=a1917;
  T.A[18*s+17]=a1918;
  T.A[19*s+0]=a201;
  T.A[19*s+1]=a202;
  T.A[19*s+2]=a203;
  T.A[19*s+3]=a204;
  T.A[19*s+4]=a205;
  T.A[19*s+5]=a206;
  T.A[19*s+6]=a207;
  T.A[19*s+7]=a208;
  T.A[19*s+8]=a209;
  T.A[19*s+9]=a2010;
  T.A[19*s+10]=a2011;
  T.A[19*s+11]=a2012;
  T.A[19*s+12]=a2013;
  T.A[19*s+13]=a2014;
  T.A[19*s+14]=a2015;
  T.A[19*s+15]=a2016;
  T.A[19*s+16]=a2017;
  T.A[19*s+17]=a2018;
  T.A[19*s+18]=a2019;
  T.A[20*s+0]=a211;
  T.A[20*s+1]=a212;
  T.A[20*s+2]=a213;
  T.A[20*s+3]=a214;
  T.A[20*s+4]=a215;
  T.A[20*s+5]=a216;
  T.A[20*s+6]=a217;
  T.A[20*s+7]=a218;
  T.A[20*s+8]=a219;
  T.A[20*s+9]=a2110;
  T.A[20*s+10]=a2111;
  T.A[20*s+11]=a2112;
  T.A[20*s+12]=a2113;
  T.A[20*s+13]=a2114;
  T.A[20*s+14]=a2115;
  T.A[20*s+15]=a2116;
  T.A[20*s+16]=a2117;
  T.A[20*s+17]=a2118;
  T.A[20*s+18]=a2119;
  T.A[20*s+19]=a2120;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=b2;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.b[13]=b14;
  T.b[14]=b15;
  T.b[15]=b16;
  T.b[16]=b17;
  T.b[17]=b18;
  T.b[18]=b19;
  T.b[19]=b20;
  T.b[20]=b21;
  T.bhat[0]=d1;
  T.bhat[1]=d2;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
  T.bhat[13]=d14;
  T.bhat[14]=d15;
  T.bhat[15]=d16;
  T.bhat[16]=d17;
  T.bhat[17]=d18;
  T.bhat[18]=d19;
  T.bhat[19]=d20;
  T.bhat[20]=d21;
}

static void rksTableauB109(rksTableau &T) {
  const int s = 21; T.s = s; T.adaptive = 1; T.errOrder = 9;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, a21, a31, a32, a41, a43, a51, a53, a54, a61, a64, a65, a71, a75, a76, a81, a85, a86, a87, a91, a96, a97, a98, a101, a106, a107, a108, a109, a111, a118, a119, a1110, a121, a129, a1210, a1211, a131, a139, a1310, a1311, a1312, a141, a149, a1410, a1411, a1412, a1413, a151, a159, a1510, a1511, a1512, a1513, a1514, a161, a169, a1610, a1611, a1612, a1613, a1614, a1615, a171, a179, a1710, a1711, a1712, a1713, a1714, a1715, a1716, a181, a189, a1810, a1811, a1812, a1813, a1814, a1815, a1816, a1817, a191, a199, a1910, a1911, a1912, a1913, a1914, a1915, a1916, a1917, a1918, a201, a209, a2010, a2011, a2012, a2013, a2014, a2015, a2016, a2017, a2018, a2019, a211, a219, a2110, a2111, a2112, a2113, a2114, a2115, a2116, a2117, a2118, a2119, a2120, b1, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, d1, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21;
  c2 = .2232129192123735665527132860096509572180113541116006004801774831897914255866976731834;
  c3 = .3348193788185603498290699290144764358270170311674009007202662247846871383800465097751;
  c4 = .5022290682278405247436048935217146537405255467511013510803993371770307075700697646627;
  c5 = .1176948756548443596524604744040430749724814058786280779261159420482531581524139111558;
  c6 = .6425923677604462086219057105219705193480713109262431864517703346402040737649802005250;
  c7 = .181826565311210497;
  c8 = .4341610334077954337462856744299909865481150413847250582144329859884319977831972426112;
  c9 = .7122335424182418496009012272374473135415214567547251841147975874613222331084545787886;
  c10 = .1894400309592476109308204841234726559856268372193596062465558450034952083309882121192;
  c11 = .4943921538691045853333333333333333333333333333333333333333333333333333333333333333333;
  c12 = .6403413702953303301268868454436798781085931758963391614454137298592697657577711636312;
  c13 = .741588230803656878;
  c14 = .382911329862520855;
  c15 = .107157755822487322;
  c16 = .875691376241245711;
  c17 = .964069299370187816;
  c18 = .281729610523717922;
  c19 = .631145492177176748;
  c20 = .973803039377034607;
  a21 = .2232129192123735665527132860096509572180113541116006004801774831897914255866976731834;
  a31 = .8370484470464008745726748225361910895675425779185022518006655619617178459501162744378e-1;
  a32 = .2511145341139202623718024467608573268702627733755506755401996685885153537850348823314;
  a41 = .1255572670569601311859012233804286634351313866877753377700998342942576768925174411657;
  a43 = .3766718011708803935577036701412859903053941600633260133102995028827730306775523234970;
  a51 = .8645012631860283043095827588443080342589988168209518203030156172654040053726457363040e-1;
  a53 = .5236243791730284147504959465381950604441622286481307774905930763200738501679225239292e-1;
  a54 = -.2111768858106131225354739613420723449783469866828018185324492731029462740164291486751e-1;
  a61 = -.2639257721366917655020719039008537814312675653905044467083967854693494521584367325145e-1;
  a64 = .3321586951309942525210016931443520748342752809336835485070926725007327816351124496012;
  a65 = .3368262498431211326511112077677038226569227865316100826155173406864062373457114241753;
  a71 = .4214481430128891924590207161639307925193144098983055869572587559196013842334308634337e-1;
  a75 = .1395091008993089985837339976891755843541033762656629420011519949721775047502000865276;
  a76 = .1726501106125791703639306944313363939651827445064993031221294358623568264568271289984e-3;
  a81 = .1913223517063317850561878098464623128757461736096094048517648270556161690028960671678;
  a85 = -.6549476653170516368338607340883454058106339891146285998978549868910881698171713079544;
  a86 = .1755876323742502725238471568246235150972597859217101866902108545773998108136024750683e-1;
  a87 = .8802275837810902582715738829894117279732768782975732345915020603661640175161122358910;
  a91 = .5819519506251173970176101222224200992345451475572200041442763494089781743712740682029e-1;
  a96 = .1647908558976784212367930935320516004070340774715814512653291320117464889726338996259;
  a97 = .2562758618556534830885936612274458691344630164445656645146446091037555911278059725788;
  a98 = .2329716296023982055737534602557078340765698480828560679203962114049223355708872997637;
  a101 = .6609808969751805186432114363691927852621669632797821827493854746683225501745446560252e-1;
  a106 = .4993798303607936315852000088060192246997357096577012552548527217200430403080630524414e-1;
  a107 = .1536216094454890707317822140262379594167730891287251273659790090180235843883503581847;
  a108 = -.5414130381994316899529893137177555939937633423733000345305400547297139497578508331128e-1;
  a109 = -.2607634739989570582850394304851094502796018496578386146679297818039354012983783360089e-1;
  a111 = .5977281413067455622264838600535046960155059547948827360548395666199015131925709118733e-1;
  a118 = .1656649460691327819663527933964608353435263909407310250007028968559622442637053740488;
  a119 = -.1270467077219322361480177163110966635379558631317440513037999854071349138024611223099e-2;
  a1110 = .2702248607465165695058123310946329950236359055444314752401844796694522868883954793203;
  a121 = .5714646346927726543617292683289117330020257083064077657640682066128476784510949443172e-1;
  a129 = .1706647333279140031893907789070497592665517669717541507696626200796144815409471923569e-1;
  a1210 = .2853767151165243146804641383356191775396044057521502467001987179873305495674085297549;
  a1211 = .2807517183767373496913107023844645513421310226163727230918419292026930001911584202089;
  a131 = .5713690770667411245888754904665701448619979494564888202366599688508571201235483609660e-1;
  a139 = .8662146075905090149738288126241361791710263955784173047391548833627524888649185757901e-1;
  a1310 = .2854329582389984323634810972884390808440109342965556282630983683305184760043391707767;
  a1311 = .2797633923959254653770985744082321055561046971559230633717557229339774187704436927395;
  a1312 = .3263351170300796630314989799425818119658193404403069586756442351414314432637044280815e-1;
  a141 = .5792154217958510846632846508298421800727675597066414244319766319648769731268367294802e-1;
  a149 = -.1168598764394854203108670771355968386139754171212251879659829443523204385801129100146;
  a1410 = .2791270024477207066381851603610170508129292534731657465038373367821947287200953472810;
  a1411 = .5523482312158897030618178268050450731680950103624425704577340542168627357243106364454e-1;
  a1412 = .3313821420230448163586175200236513161592367149984143944502756986720155621978125806284e-1;
  a1413 = .7434962435080700826430991700872593086103623514130960252814696908475018275512156807813e-1;
  a151 = .5424349131266048906033564077410202025889503282676783408313456162421294081865905389248e-1;
  a159 = .2555744036484019017045493997261865838594175905367142496400185786785191604031397243218e-1;
  a1510 = .1065318948685044129706483445621162791089602467505369099004177225283624953902959013228;
  a1511 = .1494739176822617600298548318084537294306548147599349812604239821749187453609545343658;
  a1512 = -.7541418150821686840498891896239704084250667222662001839556328840119093341810397465136e-1;
  a1513 = .1183662716974706173695161845106353658054818835708868187585164205844835807880512638150e-2;
  a1514 = -.154418469614537368;
  a161 = -.3130073355625165399871063999971367682939799979170688711523373232868473829204122305445e-1;
  a169 = -.4393041016682237597697452523147415408933247130414835042917723489831082747083637622576;
  a1610 = -.2971894957150042139937620479069507765251785957115322181603409909075380714667735160813;
  a1611 = .1262980195755398451585609791771777378208362550125140868265029793320289593624402155837;
  a1612 = -.1352235663389501393036813956309918309028161801076417057198393439731017100387729485379e-1;
  a1613 = .6695605432945227295340251006073274395173466715429726933128280272846122961086155806635;
  a1614 = .398926850454282556;
  a1615 = .462222650490275222;
  a171 = .4254608456033344945622257854593736284131499735662276010271273463216872625702497770866e-1;
  a179 = -.1821182296512653493976221650402057582117417514147759470622260585285199838506585092559;
  a1710 = .1693488568788993457692776359468516487310835997985566768493328170663073221084671579068;
  a1711 = -.2812932311380423097505532359005039285971305910469541561543126052387690343228512975388e-2;
  a1712 = .4367359500435692592480947784835035112081291953371300785838045346974343617081011595250;
  a1713 = .9649623253501190961728232014648167901724237884864786722867265364689659268793239634042e-2;
  a1714 = .1827541189026459104590716992261562936435133038172843022420870171219768684076842803782;
  a1715 = .8163992629788446963341645082372801271393454416361108721409416282456258720143539344240e-1;
  a1716 = .2263259013959999629673163223583858004580131789671757969088706528737681492423808136362;
  a181 = -.6162786199706763955052568808743247751165729542444646544961722501628952288996567177280e-1;
  a189 = .2630819816424975496840706912577088603030984130761613342651746722632362416592729338065e-1;
  a1810 = -.6563819353303824389148917307603424726525093364227772615481314402265589817962663323807;
  a1811 = -.4996969738639946601661934398570589720521936732243676171454742217015940903119194293034;
  a1812 = -.7123015625596798250364056005999154454586809433543859495632863935368185749281956104361e-1;
  a1813 = .1945887064918253295880838290230513152388881501421435771759995471289411971821731631008;
  a1814 = .7502438829684451516052952921664213906375154664124515160998662893556068266252405773932;
  a1815 = .6778525665536439867432200838066450041417881571145524785032579156045245492715999778952;
  a1816 = -.1046057647455310786652080097009039359249473747848876107492887123406055065619567484248;
  a1817 = .2627894853849749889545315434384080663867415921515384464319901932333376180798673115539e-1;
  a191 = .3796521632874261201186203436841209430559695655768644122346210733272382403437031805294e-1;
  a199 = -.2594056416058702079475722192315214454017994671360402614116801042085357278273610676615;
  a1910 = .6023088303591701879982724849459819049819908747310071139535368513067871259409896536125;
  a1911 = .6119387226913542078978281038711879095845432514371708320113689988639552031445275756169;
  a1912 = -.8557593281472216275173535332798856826091404689475696899046474643882545069592793853088;
  a1913 = 1.016014044479150879819181655935044451622873646716581194764492422092270586935018600847;
  a1914 = .4174635465318728144170599907012271233953357864579855698710555478932012900105054820193;
  a1915 = -.3142068748294011550388298801522627398568106507178091710884531834553635025761917127745e-2;
  a1916 = -.3113859536805386540643212859648902182769708893494791481799285293081168071417215791223;
  a1917 = .7475323182002611486490630088842356498085791607202230555314272159767158496743797941577e-1;
  a1918 = -.6996051078512155679294752334324570751847195000321862661699180193471489380787256603442;
  a201 = .6354247858004162057833209973904648790423789887640110847597201944718467473335513915457e-1;
  a209 = .1335709427214855386564319367816597150656631989678512452281616080142611136536750546091;
  a2010 = .3491939710685402856556370077059343902437022368395394555525545826876029022896488736825;
  a2011 = -.1284299219085288104043584513269729325317465049017238832115899338924554224999281557779;
  a2012 = .5820143454609020234416665664957204705761540086346612646427774305741185500271652747165;
  a2013 = -.7032295749061967719609757517752619736970110102217160910226632819061710474763494562046e-1;
  a2014 = .4108572358925710179476579003323188290642373757595878674150744283920916547885055706399;
  a2015 = -.2510654779239962466269627907503763505655390551209676232633905831376503530736867220512e-1;
  a2016 = .1223166254706007785807460575472930729088496060945489634909944334258869465746143128690;
  a2017 = .4712374572639742166850752217533416565767093458666637954607896698933218387654817641078e-1;
  a2018 = -.2035773172368828280351383367281992122282011746329188150975183458730713499142844139626;
  a2019 = -.3073795611150731392306884484695711542343125736903452146138998032605691134742962145162;
  a211 = .1189924137880676460852663367282125023346761041034714988771622602337896848257576311721;
  a219 = 1.567065389218647018255284948985537561396036403483411695550932448674279128530361565244;
  a2110 = 1.062742213102872753657741254837796588064400661866785937264672961506012193460062190291;
  a2111 = -.7051696022721412819771965938099389611228930171636494130149946862206926743626267591091;
  a2112 = .9907825344767892999287937933800084396119601938859814415204705439463102044422773242413;
  a2113 = .2507980962931107922266871779870423357231706420300879746540980799200371363012966217081;
  a2114 = 1.972591757153629542226038011120263810650524345125694323573613060238829589123920677616;
  a2115 = -.3262628387941532893880299499584174546880007304318604226454131079685364715701264155684;
  a2116 = -.7195961905900189101450624277119790470362703727002334732811111315403329989928142118605;
  a2117 = .5802031535291661528133435810283051023192180079596550160030993013203826047471987749923;
  a2118 = -1.503104563178041688806298917914932279967077722444117887454135595533548208509663052219;
  a2119 = -2.082917406986733532241895178393857460921252779279252368270349439324592549474879051363;
  a2120 = -.2061249557411945026346720362780411363644917364359743227780446952519376385207652951444;
  b1 = .3074440935793207675883517938984149786411995438646211366049687901929864736726360396701e-1;
  b12 = .4447886124335158463373029422601699728444313324875120673928896655863659883998986985001;
  b13 = -.7714472898672551027236427412075689933241937304249667830486089002567926592070578328099e-1;
  b14 = .1895969236666960081782143661904924178003392273539755384045555627359634132139704039829;
  b15 = .1696794742660741325357704245298007854728500376148224262050684467711119689665579207267;
  b16 = .3107725327623489988155072749206969138501610275590999188855016092183529050525521871700;
  b17 = -1.052173416444910172223599400939351222118848154980691993896969290833000795415995203779;
  b18 = .1098353169596310609348506603862869818226618225743191040640071521542528953059954349550;
  b19 = -.1334572232187388442842841059278351057902328711498735111118805829160094435254500793407;
  b20 = 1.154026328574134208065260150796827370454610532524502151460002781580501491609214462821;
  b21 = -.1466682293699578048454932174861727128676735353276311367588113332911578050533016457222;
  d1 = .3069935889151056339784198488152917237594701959064431113839566366053496143740989806580e-1;
  d12 = .4501037370765134863000400416812391552231563060566733706571606677666390164501322407860;
  d13 = -.6514757933632400121177566572810133388907129562810217527103583351082868427677317818171e-1;
  d14 = .1916476309177840038043731680068457542540807928558914659725060136615214781805707317846;
  d15 = .1699093037015861786030207592719576231809404332930270025745281206261397945871067257969;
  d16 = .2831822563446878149008449047225439184357774091336334653238641676396106848038134341285;
  d17 = -.7938273328067640993878535581971311480056976834520424653124835645605083935506842817243;
  d18 = .1085805168491408111596918257428235537727685528809234551377846603272747280143198404809;
  d19 = -.1442902422009048623094173280045594665841429401482516755746485620108546739010869727583;
  d20 = .8718101111217405681350791198631736702436128801469513492598526664004710882551915616216;
  d21 = -.102667760558970463391845252240320899007371474729348103905924;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  T.A[12*s+11]=a1312;
  T.A[13*s+0]=a141;
  T.A[13*s+8]=a149;
  T.A[13*s+9]=a1410;
  T.A[13*s+10]=a1411;
  T.A[13*s+11]=a1412;
  T.A[13*s+12]=a1413;
  T.A[14*s+0]=a151;
  T.A[14*s+8]=a159;
  T.A[14*s+9]=a1510;
  T.A[14*s+10]=a1511;
  T.A[14*s+11]=a1512;
  T.A[14*s+12]=a1513;
  T.A[14*s+13]=a1514;
  T.A[15*s+0]=a161;
  T.A[15*s+8]=a169;
  T.A[15*s+9]=a1610;
  T.A[15*s+10]=a1611;
  T.A[15*s+11]=a1612;
  T.A[15*s+12]=a1613;
  T.A[15*s+13]=a1614;
  T.A[15*s+14]=a1615;
  T.A[16*s+0]=a171;
  T.A[16*s+8]=a179;
  T.A[16*s+9]=a1710;
  T.A[16*s+10]=a1711;
  T.A[16*s+11]=a1712;
  T.A[16*s+12]=a1713;
  T.A[16*s+13]=a1714;
  T.A[16*s+14]=a1715;
  T.A[16*s+15]=a1716;
  T.A[17*s+0]=a181;
  T.A[17*s+8]=a189;
  T.A[17*s+9]=a1810;
  T.A[17*s+10]=a1811;
  T.A[17*s+11]=a1812;
  T.A[17*s+12]=a1813;
  T.A[17*s+13]=a1814;
  T.A[17*s+14]=a1815;
  T.A[17*s+15]=a1816;
  T.A[17*s+16]=a1817;
  T.A[18*s+0]=a191;
  T.A[18*s+8]=a199;
  T.A[18*s+9]=a1910;
  T.A[18*s+10]=a1911;
  T.A[18*s+11]=a1912;
  T.A[18*s+12]=a1913;
  T.A[18*s+13]=a1914;
  T.A[18*s+14]=a1915;
  T.A[18*s+15]=a1916;
  T.A[18*s+16]=a1917;
  T.A[18*s+17]=a1918;
  T.A[19*s+0]=a201;
  T.A[19*s+8]=a209;
  T.A[19*s+9]=a2010;
  T.A[19*s+10]=a2011;
  T.A[19*s+11]=a2012;
  T.A[19*s+12]=a2013;
  T.A[19*s+13]=a2014;
  T.A[19*s+14]=a2015;
  T.A[19*s+15]=a2016;
  T.A[19*s+16]=a2017;
  T.A[19*s+17]=a2018;
  T.A[19*s+18]=a2019;
  T.A[20*s+0]=a211;
  T.A[20*s+8]=a219;
  T.A[20*s+9]=a2110;
  T.A[20*s+10]=a2111;
  T.A[20*s+11]=a2112;
  T.A[20*s+12]=a2113;
  T.A[20*s+13]=a2114;
  T.A[20*s+14]=a2115;
  T.A[20*s+15]=a2116;
  T.A[20*s+16]=a2117;
  T.A[20*s+17]=a2118;
  T.A[20*s+18]=a2119;
  T.A[20*s+19]=a2120;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=0.0;
  T.b[4]=0.0;
  T.b[5]=0.0;
  T.b[6]=0.0;
  T.b[7]=0.0;
  T.b[8]=0.0;
  T.b[9]=0.0;
  T.b[10]=0.0;
  T.b[11]=b12;
  T.b[12]=b13;
  T.b[13]=b14;
  T.b[14]=b15;
  T.b[15]=b16;
  T.b[16]=b17;
  T.b[17]=b18;
  T.b[18]=b19;
  T.b[19]=b20;
  T.b[20]=b21;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=0.0;
  T.bhat[4]=0.0;
  T.bhat[5]=0.0;
  T.bhat[6]=0.0;
  T.bhat[7]=0.0;
  T.bhat[8]=0.0;
  T.bhat[9]=0.0;
  T.bhat[10]=0.0;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
  T.bhat[13]=d14;
  T.bhat[14]=d15;
  T.bhat[15]=d16;
  T.bhat[16]=d17;
  T.bhat[17]=d18;
  T.bhat[18]=d19;
  T.bhat[19]=d20;
  T.bhat[20]=d21;
}

static void rksTableauS1110a(rksTableau &T) {
  const int s = 26; T.s = s; T.adaptive = 1; T.errOrder = 10;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, a81, a82, a83, a84, a85, a86, a87, a91, a92, a93, a94, a95, a96, a97, a98, a101, a102, a103, a104, a105, a106, a107, a108, a109, a111, a112, a113, a114, a115, a116, a117, a118, a119, a1110, a121, a122, a123, a124, a125, a126, a127, a128, a129, a1210, a1211, a131, a132, a133, a134, a135, a136, a137, a138, a139, a1310, a1311, a1312, a141, a142, a143, a144, a145, a146, a147, a148, a149, a1410, a1411, a1412, a1413, a151, a152, a153, a154, a155, a156, a157, a158, a159, a1510, a1511, a1512, a1513, a1514, a161, a162, a163, a164, a165, a166, a167, a168, a169, a1610, a1611, a1612, a1613, a1614, a1615, a171, a172, a173, a174, a175, a176, a177, a178, a179, a1710, a1711, a1712, a1713, a1714, a1715, a1716, a181, a182, a183, a184, a185, a186, a187, a188, a189, a1810, a1811, a1812, a1813, a1814, a1815, a1816, a1817, a191, a192, a193, a194, a195, a196, a197, a198, a199, a1910, a1911, a1912, a1913, a1914, a1915, a1916, a1917, a1918, a201, a202, a203, a204, a205, a206, a207, a208, a209, a2010, a2011, a2012, a2013, a2014, a2015, a2016, a2017, a2018, a2019, a211, a212, a213, a214, a215, a216, a217, a218, a219, a2110, a2111, a2112, a2113, a2114, a2115, a2116, a2117, a2118, a2119, a2120, a221, a222, a223, a224, a225, a226, a227, a228, a229, a2210, a2211, a2212, a2213, a2214, a2215, a2216, a2217, a2218, a2219, a2220, a2221, a231, a232, a233, a234, a235, a236, a237, a238, a239, a2310, a2311, a2312, a2313, a2314, a2315, a2316, a2317, a2318, a2319, a2320, a2321, a2322, a241, a242, a243, a244, a245, a246, a247, a248, a249, a2410, a2411, a2412, a2413, a2414, a2415, a2416, a2417, a2418, a2419, a2420, a2421, a2422, a2423, a251, a252, a253, a254, a255, a256, a257, a258, a259, a2510, a2511, a2512, a2513, a2514, a2515, a2516, a2517, a2518, a2519, a2520, a2521, a2522, a2523, a2524, a261, a262, a263, a264, a265, a266, a267, a268, a269, a2610, a2611, a2612, a2613, a2614, a2615, a2616, a2617, a2618, a2619, a2620, a2621, a2622, a2623, a2624, a2625, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26;
  c2 = .2494510436877389344264053439636521218011835467298426368145366983339993718715373083760;
  c3 = .3741765655316084016396080159454781827017753200947639552218050475009990578073059625640;
  c4 = .5612648482974126024594120239182172740526629801421459328327075712514985867109589438460;
  c5 = .8546995299174592915742289900418092935366740866720183954100852188058051166442961011609e-1;
  c6 = .7310788517640473762691237698106576677338584467168049416410984572334277402413648068397;
  c7 = .1304049416609471516815374056280027453671928620452985586822237474262182566918325326012;
  c8 = .4712636357202847267558933149451482905659499178473869504545717970436656667382069175430;
  c9 = .7730997092193940758126470461455964941402897392485301085000777589597371419303943343132;
  c10 = .2056292271097569160894663338279693801391636310750116019089310570387586267029180604980;
  c11 = .5366419968074299811348135248875344652445218400812654186620229284574082136119576258888;
  c12 = .6950637644720240807995723705690567056506824408065743296638976932495337928615038239116;
  c13 = .8049629952111449717022202873313016978667827601218981279930343926861123204179364388333;
  c14 = .1913760139217099199372138166733346224093015957152232805822661231536077778241788913071;
  c15 = .8894442715562032655697860690613313211578199121477983480401175761840408067430713807992;
  c16 = .4680151547126001101041424878175828807838998612363955308441536427569141462651129332201;
  c17 = .9908386187455954897815362931642001409443269908386187455954897815362931642001409443270;
  c18 = .9000188288457917529655432122010920730559216720015063076633402372434569760873658444737e-1;
  c19 = .4031478884213807074957145083372292348449431198379304971170328814087579865980988000623;
  c20 = .2426620464737056665307786384019567876070118222584590297594781899714635140644109253975;
  c21 = .7208245981830887491264849755415793151642208245981830887491264849755415793151642208246;
  c22 = .1999681579366342939022448654672822798917369845565992676325425887597516319057474924375;
  c23 = .9639977912755383765875207067918277194919933738266151297625621203754831584759801214798;
  c24 = .7436137801734239512538082962268572767752519334426997890789782048277478322006093273963;
  a21 = .2494510436877389344264053439636521218011835467298426368145366983339993718715373083760;
  a31 = .9354414138290210040990200398636954567544383002369098880545126187524976445182649064100e-1;
  a32 = .2806324241487063012297060119591086370263314900710729664163537856257492933554794719230;
  a41 = .1403162120743531506148530059795543185131657450355364832081768928128746466777397359615;
  a42 = 0;
  a43 = .4209486362230594518445590179386629555394972351066094496245306784386239400332192078845;
  a51 = .7019164670454382087443786606405374274908739183454652276598025183935982136630730721870e-1;
  a52 = 0;
  a53 = .2631174841924423444834727510216642181614053183934029855724529099908799066900681707156e-1;
  a54 = -.1103344213204212616536224216203923521156051500668498178221702095786730037088451417417e-1;
  a61 = -.1566211240781183642488518551453027928209011808429926742075994450765608166883593055436;
  a62 = 0;
  a63 = 0;
  a64 = .4022037025415642768212777334976723807953788960750969348079276345083207687523340439985;
  a65 = .4854962733006014636966978914582880797593807314847006810407702678016677881773900683848;
  a71 = .3112249065189454540975938749103688775640860268015765517875901819636788841622883514715e-1;
  a72 = 0;
  a73 = 0;
  a74 = 0;
  a75 = .9925602946470755730976861333746552578010113543371556501156532289522223083686039750464e-1;
  a76 = .2642154434504896200940479950033183068312393142533849189940633462813743874329994944194e-4;
  a81 = .7097221158585919779802904970340267773691990074739280896770286766721241217196318737330;
  a82 = 0;
  a83 = 0;
  a84 = 0;
  a85 = -2.878599621086994245472819276760640913301855954929611359904818127369971660983418731969;
  a86 = .2129446575803202495593011450000675123146819436283352905644086375052608273806710598083e-1;
  a87 = 2.618846675190654969292491980171755675267138670940236691625920383990987123263926669799;
  a91 = .1971162769751622653374279030890901895102271260934752560073298666516820598947694065245e-1;
  a92 = 0;
  a93 = 0;
  a94 = 0;
  a95 = 0;
  a96 = .1314769583961512153957380944910102917010453313237904532896703042317759437588108172284;
  a97 = .2651026426320722003136425762899652531974839391071893983013696279981183473287139122567;
  a98 = .3568084804936544335695235850557119302907377562082027313083048400646746448533926641757;
  a101 = .4520001981236963034666158997158145581787527592168118014437521620068508165483327850010e-1;
  a102 = 0;
  a103 = 0;
  a104 = 0;
  a105 = 0;
  a106 = .4002799368963269424453448726036172129707286035255084033530848251945047638855433381069e-2;
  a107 = .1601500636916154116141648988130496051518236572818790559811874215346710453554656819808;
  a108 = -.6948308961135587660550768108803798325673156163094392313978348939414189769206882520960e-3;
  a109 = -.3028824867077836529758526871817473127675272547494279018764594054601128969315645111849e-2;
  a111 = .6488088874156601542550101009631282425235066317670643554509232342293350370832201888741e-1;
  a112 = 0;
  a113 = 0;
  a114 = 0;
  a115 = 0;
  a116 = 0;
  a117 = 0;
  a118 = .1798223672519538764980931982771653336226267689019203140039759418977874942481337275737;
  a119 = -.1379038853795374658459375939872891452260918957641627984236926962149885290906676389842e-2;
  a1110 = .2933177796677054638696786924539291988218053269602802970971915900988371009464085558176;
  a121 = .6203009498964512152733682602483252800544465468027723696513649660598713938403105468563e-1;
  a122 = 0;
  a123 = 0;
  a124 = 0;
  a125 = 0;
  a126 = 0;
  a127 = 0;
  a128 = 0;
  a129 = .1852494271216683106566732412801252654028709112506185661813162013190347158018146409385e-1;
  a1210 = .3097644836067189024326911104871249008925595428403487952252886015862314098920345337684;
  a1211 = .3047442431634932257738771099290867502123911521608864408553409749254117720052567713637;
  a131 = .6201972260916893080711420739558975045994481991404016270942831042621296675332534575866e-1;
  a132 = 0;
  a133 = 0;
  a134 = 0;
  a135 = 0;
  a136 = 0;
  a137 = 0;
  a138 = 0;
  a139 = .9402397126314593976910409432600376284640745739392856006491693414739377176752496830254e-1;
  a1310 = .3098255331628556417346604747446843311853795538255778076358109181983411673182996479068;
  a1311 = .3036714566645797050698936883147468540428250347499812111630779317183972047785244555068;
  a1312 = .3542231151139475432144782255027699933222589423837038641980029819576720980026202135845e-1;
  a141 = .6969089344825787554650204576987666522661239866526931243865956872566266585875574389090e-1;
  a142 = 0;
  a143 = 0;
  a144 = 0;
  a145 = 0;
  a146 = 0;
  a147 = 0;
  a148 = 0;
  a149 = -1.026068398848221434143517600646828432765332980244057389200313887686341269100904144289;
  a1410 = .1707111685325415936794748807994057363691702390665289122715815578989450621608097471178;
  a1411 = -.2276426317828345244640899307352560724213609963668371269120350778897604268488707131379;
  a1412 = .6469733264162559693641327687577763359541689058709531537823573009011020286605375682754;
  a1413 = .5577116561557104399547116527283603900460440287233664182020166612039997170938506894503;
  a151 = .5627122376293571949132309653445143949301687520262788651750408232740266180616570845510e-1;
  a152 = 0;
  a153 = 0;
  a154 = 0;
  a155 = 0;
  a156 = 0;
  a157 = 0;
  a158 = 0;
  a159 = 0;
  a1510 = 0;
  a1511 = .3678983222850209699517524982831781294240536196092600812178960084767342167600554097921;
  a1512 = -.5195587208803483299356547234278383561187847326601986146678158388835223877380213814230e-1;
  a1513 = .2204300024722190946507889136186101583051939445678464072502258435712922894837870998433;
  a1514 = .2968005951240623144694870329678754295474339460340838345212732256969638774668653008510;
  a161 = .5634846497340329026169333310752166966844307632715132381117641490389593628519826878531e-1;
  a162 = 0;
  a163 = 0;
  a164 = 0;
  a165 = 0;
  a166 = 0;
  a167 = 0;
  a168 = 0;
  a169 = 0;
  a1610 = 0;
  a1611 = .2154493134975348678816921094538633047666156318059356569827382889198219015749935439823;
  a1612 = -.1828038597798339032767249818891038294211236603774749922977117870744959009671532402296;
  a1613 = .1115035565964385314512820745510850004252820806597292903369723111122469246811072381550;
  a1614 = .2961573176911709124764789850722285989373150621850530267865487559044685065325051431373;
  a1615 = -.2863963826611358869027903247801186359263232936399877477557034100902322184153802061016e-1;
  a171 = .5768994198724289036171882589079551200329053317074015736866935607317368058263947892388e-1;
  a172 = 0;
  a173 = 0;
  a174 = 0;
  a175 = 0;
  a176 = 0;
  a177 = 0;
  a178 = 0;
  a179 = 0;
  a1710 = 0;
  a1711 = -.3339594153573746138779657254239687461940518464286089760815893952034552574434211037540e-1;
  a1712 = .4007337036063909756966778946251460958783204419363211999098235920936595305217080757252;
  a1713 = -.2804910522268063380484013221059916164179105523752052409880171087961359378301286147028;
  a1714 = .2860659865450288214940338791467983472419468134474127404785360793939950768656398860364;
  a1715 = .3300830129889967277869093004777042822760017064369869377090469066296160876867312632942;
  a1716 = .2301529673804798738783942876721443945820832328652238487255898956623302521178929654255;
  a181 = .4628349579719024427317175399131733273511297084139209516535128748975595102348466963346e-1;
  a182 = 0;
  a183 = 0;
  a184 = 0;
  a185 = 0;
  a186 = 0;
  a187 = 0;
  a188 = 0;
  a189 = 0;
  a1810 = 0;
  a1811 = 1.630450107397975249685538886816283747362802271995334921665631466207136001357816066251;
  a1812 = -1.519389027110125818833104438792078888366805294218517984315935323689939634346785541266;
  a1813 = 1.451177905612818398158204503782829478002025896171620079182131464615748573245045066933;
  a1814 = .1115858300886827251108540309774994639792980968086331408518485993004009147049002886661;
  a1815 = -.6927910549842217223999151167283689307038087310272710650839151778360002928231938996564;
  a1816 = -1.033967300639154935442351963388333197850853637180263551013370838161871418837432150763;
  a1817 = .9665192672141503474415666456096020214782059380922299431459254579911560328490208464940e-1;
  a191 = -.2030459770939010457179865043457543159890686836908949375313125798640212974640722247139e-1;
  a192 = 0;
  a193 = 0;
  a194 = 0;
  a195 = 0;
  a196 = 0;
  a197 = 0;
  a198 = 0;
  a199 = 0;
  a1910 = 0;
  a1911 = 5.403684273875123798630962329849980016426640735397844799635504992740247972317943415738;
  a1912 = -6.547667878535769638577053800139325829335808492655639368966216657633158529493394018748;
  a1913 = 7.013821350363578373165272471360941682153886129098573011050120208706394847104611280659;
  a1914 = .1870129209851220070917089872488052512865730910524666003812699685036249110639968876355;
  a1915 = -3.586221630318466310345416445340680667399109825679889726092108094877121092019568725906;
  a1916 = -2.796807192734124892412016302005787005622243784243008121795193206999340165569504484433;
  a1917 = .5350966429298067141403865717192268565615462868769074262461851475076297049847405900305;
  a1918 = .2145339995655007603736693460786443623723658483597653704106017814468824679556810775581;
  a201 = -.1742485918290044205614554854196710455479504553190752005798475964042862615568249931979e-1;
  a202 = 0;
  a203 = 0;
  a204 = 0;
  a205 = 0;
  a206 = 0;
  a207 = 0;
  a208 = 0;
  a209 = 0;
  a2010 = 0;
  a2011 = 1.624681387971290566033459737855869526767754661771120924184664522930252560057487715917;
  a2012 = -2.354176134519334554629765394537690359953484690980299665915399924759933249024433201062;
  a2013 = 2.681464673816256355689049206851920401601461184001531947851310974409875440156813229521;
  a2014 = -.3769156802887539884394023979903229235497658274041343397278286356588457999273698837568e-1;
  a2015 = -1.416558131026522083720211494413873707821785422770559705231460034487688457012423059506;
  a2016 = -.6949353599391523142082433283060914200352029386004581317085753581959007030993120101233;
  a2017 = .2176483372495038460388837827561689520877604442565239353657200502175872181787882826377;
  a2018 = .2697602441781337550421531890140860774876933975825845928799740196946913466182988994464;
  a2019 = -.3010654404469406281446127247743328561741318472966391363598843663110743566238944373807e-1;
  a211 = -.5403069633602259108646269644934256957551404599412561453767268074870783669925498016457e-2;
  a212 = 0;
  a213 = 0;
  a214 = 0;
  a215 = 0;
  a216 = 0;
  a217 = 0;
  a218 = 0;
  a219 = 0;
  a2110 = 0;
  a2111 = .4139045320778312241379049484992085439613081939853923740529945110841130228938567470608;
  a2112 = -.6225569512435124987325753043472442306739960604338314680453027342793707050930404307846;
  a2113 = .8734771329093316867790902545034150641322140869164058836843523721471009826005087793734;
  a2114 = -.4460631186034505809671866759278699513326438900110859254673757293843834232949457511945;
  a2115 = -.4770024428429503264261245457799687730095056727976098627656199278228457764648229288301;
  a2116 = .3350194037993700956622540574882038342150917200081189888351569726131444997493018243069;
  a2117 = .7504248468668828335131027827336082864172905052946156155171125652518370192017019285897e-1;
  a2118 = .2868602398412017179778096520767496699075337622961506198525486757858393614463244192166;
  a2119 = -.3105278876109550093409796945938508430591700006793679865817465482241195091701064106306;
  a2120 = .5980742748031364157936282749945094293392110393839614650861749046057502083978432774641;
  a221 = -.8040637783629852670386253408427477129839038039895222958327102384382347854387228140586e-1;
  a222 = 0;
  a223 = 0;
  a224 = 0;
  a225 = 0;
  a226 = 0;
  a227 = 0;
  a228 = 0;
  a229 = 0;
  a2210 = 0;
  a2211 = 14.09003825209192451104055534327205557907144506304998967309786817952326707840468519855;
  a2212 = -16.51095452142836096439418777253125778161824307435047496403861839615632359732680906324;
  a2213 = 17.13478919889388959512706015258229430254357775856472236246578628732774154201548556949;
  a2214 = .5162416754963992955466349330401373481541758099272782749146402071285515617224748797349;
  a2215 = -8.738239054447235555769280431947112005536670926201592356606646679961357074323383907433;
  a2216 = -7.928763291217204762997981382770542063580344552643223907022402362361688687161033776950;
  a2217 = 1.299607095883618341404631332785395816146491714221581325131902479964394904680531866743;
  a2218 = .2985974887158637819626343538772133330364013942516888589723607063393086453938726507764;
  a2219 = .2306899737777788621463004228318904282554044640133218561071121312583287783148540854557;
  a2220 = -.4162475612748372571090619229550372319146077330570330975546626972398912191287912992344;
  a2221 = .3046152792810969736488023713665193266324974471792934717484737567812431778577335699415;
  a231 = .5188944067774664913134965881842338637465167538426812594601866401917453125373780443288e-1;
  a232 = 0;
  a233 = 0;
  a234 = 0;
  a235 = 0;
  a236 = 0;
  a237 = 0;
  a238 = 0;
  a239 = 0;
  a2310 = 0;
  a2311 = -1.283277980375441090269741488559725341590001661469261097548681503281119502510781510671;
  a2312 = 3.197031115178587180279278821590157014124289302868827178494105498343258944734856975984;
  a2313 = -4.925730995528770332329418001427019544036228452951763539493235207541539355193215629707;
  a2314 = .4556842843249686735202095835169824346358162044635023939110786683727545600555997662146;
  a2315 = 2.672937738349530686900845106745665406421870114003531687061025215633257210063413548721;
  a2316 = -1.545486350471516022666399706441233218275810264466541860777249858172892030485892221263;
  a2317 = -.3721987962685219677888284678662933875128991354125828036313277053504464873414697307303;
  a2318 = .1275610106742831413187042273550361954603324559021122028077942983877428065036205767992;
  a2319 = 1.955690675673468524392185835321028843337157689840864927642297223731192314442172030656;
  a2320 = .3191081222387442241092001119049884652557628641382080948098270187047261996513219724153;
  a2321 = 1.121546003224352473639037341460033868464982074905347279402236597305066991912121393629;
  a2322 = -.8107564764218937636489023156262164031679294933798974588613267897756930246095048550015;
  a241 = -.7011708297168838093320073271824231811555516376839339889768780533772204046098056778645e-2;
  a242 = 0;
  a243 = 0;
  a244 = 0;
  a245 = 0;
  a246 = 0;
  a247 = 0;
  a248 = 0;
  a249 = 0;
  a2410 = 0;
  a2411 = 1.191430397515656948185785084406871641958062497543742234300725797836945018037271858084;
  a2412 = -1.828224802245176636695187739870698589078178771180859212263094110856159464053843046402;
  a2413 = 2.587660024998591526533741070898054580235728551249558749988014852858819613806922059203;
  a2414 = -.4609429033777800137012176300671539646289103914701490781289612317160305665226273519358;
  a2415 = -1.325931501693639442980805419646217538577342867896123447497177267595031990967256304102;
  a2416 = .5048216427266366058315103764214691413381451411460686651485736358879672359427752204445;
  a2417 = .2194255008759965600775266235750828151587685421380286198819468613425068758315070811985;
  a2418 = .2580087037350744276193737624019399564007443016182003098695556923721330317034140914297;
  a2419 = -.7820644366919761953621998768725631028114098091524728093576852041863328545044120664888;
  a2420 = .4981236112353889237817573857026156554544669576903254922783923277260844155420240361688;
  a2421 = -.2818659230370184110487250730841700637198882434735413160224991411608463306147813033701;
  a2422 = .1975630649870201406787729762339550948339749510061280462627577046361617264269398213974;
  a2423 = -.2737789055818164357320317060050411797735340939936712549180293178469667438122671145178e-1;
  a251 = .6971223157985491441889543463058968335832800922315696226973214263599946413187133757441e-1;
  a252 = 0;
  a253 = 0;
  a254 = 0;
  a255 = 0;
  a256 = 0;
  a257 = 0;
  a258 = 0;
  a259 = 0;
  a2510 = 0;
  a2511 = .8485072933915981598683071881067011723176540155630964375777672731824955304237165117739;
  a2512 = -1.504790599380062029550479292004278750516583328735111485609300227641118522265117453228;
  a2513 = 2.971661962491066226601618850699945230551598000336030549022007199066569229936095601265;
  a2514 = .2189212281135321618639152301919425667361998064642432899287016486436734890226912997813;
  a2515 = -1.363504916136495083863504916136495083863504916136495083863504916136495083863504916136;
  a2516 = 1.535256651208129648912110087693939955244136470648889182343875125782209378315005545886;
  a2517 = .2255130922481182477299374148203865138344993252214597927624562120740486384043245934395;
  a2518 = -.1252487725356194666272520523138563196763786809348751670013078003838735310461344402659;
  a2519 = -1.433947471579772638181105448843590748725989807918463347706781654253234025872206977656;
  a2520 = -.2653692486145121794045624436138677664647506121922928212398504961979636551101946127078;
  a2521 = -1.057083353895191276322695714066286715782319071838176809244435034528746563196511305239;
  a2522 = .5958992342288874679818860848371024166473432615899828093938621236679595663138268737944;
  a2523 = .5640763974002360165840684759509622393779174110889642877127279949714649497561235682228e-1;
  a2524 = .2280650291404422449145227284026716224019757875996592625955056045913295898305255848960;
  a261 = .5847724407501707800509964027663995519804001870472850379216053933603321448937320382873e-1;
  a262 = 0;
  a263 = 0;
  a264 = 0;
  a265 = 0;
  a266 = 0;
  a267 = 0;
  a268 = 0;
  a269 = 0;
  a2610 = 0;
  a2611 = .8482433234853130399472226781516394610296267520150369993929425712656780053034142790857;
  a2612 = -1.818778337229596341437999685014432821665031881560984595319320029619105771387827231293;
  a2613 = 3.360950949051261986329911895518526801628857587757616068272635548399517354842337530945;
  a2614 = .1508779656009112841743785043192199515750215073024373320382690391467484988490972888214;
  a2615 = -1.535625970104296058211732653128688607443092018291498032244748403611478465692632705036;
  a2616 = 1.689710648596447426456373308903510510459502815029443327222583037297243278873766013804;
  a2617 = .2753763916359371389555315070580325732352897337628204267122697551960198539312003364345;
  a2618 = -.8663993203603434555880694559625317699151588603507943515655135799742692170332074641456e-1;
  a2619 = -1.514109179619982965788582168222695819982704233094238060182049846398573819503074487947;
  a2620 = -.2731051919432418524342072455220752640028637692479133311886846927223922900302148881433;
  a2621 = -.7386383863141516263346011834004149434481207761507984915319537912098408949133636097415;
  a2622 = .6343578675072522696688679414505709253269883958978404947025376176286935002340724649840;
  a2623 = .2219915268009860510634252213682523269888255016876797090075141605397073674221546364928e-1;
  a2624 = -.7329654538493563887779811693040477761888079625817917741084140276508628003504291297654e-1;
  a2625 = 0;
  b1 = .2560300422448818640056973733027381517733824076443136953634798402199579680290993667785e-1;
  b2 = 0;
  b3 = 0;
  b4 = 0;
  b5 = 0;
  b6 = 0;
  b7 = 0;
  b8 = 0;
  b9 = 0;
  b10 = 0;
  b11 = 0;
  b12 = 0;
  b13 = 0;
  b14 = 0;
  b15 = .4518777011767389121126592910724741440985372571238852320538195476794258345326944681173;
  b16 = .1571644835050410342525892218785914023945906781166129113400861908694541485546171465980;
  b17 = 1.587056574024945279877130621604901273153398820663935100541595105228850005414302839405;
  b18 = .1457164101207840798598697293357824068221719394674815441268910066025070033320249231167;
  b19 = .6756690992874993365704777906957166121534838869284524431811474400358351375791575269125e-1;
  b20 = .2263702829370636470261171189724674060331565741885612750461048803721449531302274282834;
  b21 = .9585657493693419066100150421626762898713313017234629340723230525017707321578125838875;
  b22 = -.5737510952253720251243392241397701824182886145229967660377157325379672132470673999182e-1;
  b23 = -.8207294363127299380342111455087248111504033000422844250308430626629630466499721876523;
  b24 = -.8199946795810496733757980690172928576129340372072025495094374198905045106663577622820;
  b25 = -.9218218898708361658735554044867437117607070020394289598912304554724677090414683888511;
  b26 = 0;
  d1 = .2611453567558047174327245053028336466626806615883961914681281336363189972915245544873e-1;
  d2 = 0;
  d3 = 0;
  d4 = 0;
  d5 = 0;
  d6 = 0;
  d7 = 0;
  d8 = 0;
  d9 = 0;
  d10 = 0;
  d11 = 0;
  d12 = 0;
  d13 = 0;
  d14 = 0;
  d15 = .6130789590044024420375347235457514503322168389154611016924109584567258667669428084328e-1;
  d16 = .2225176387207912085954871898906978473895949131807976650805573845853338932568562016822;
  d17 = -.6452541374509687528609607022885324791758461431623241957335732784511248267767614258399;
  d18 = .1428813432326775237316522259538641672308222297639395170211160113665720641033224975230;
  d19 = .1998503703511764116053495106126866164319741023221332167221899831502033833897226507607e-1;
  d20 = .2271215662186804051109071916317932366756181626009529050272447503184463361820614170681;
  d21 = .3555653412963184679461951194576569724124866534036012519458466746801575903667969215385;
  d22 = -.5084144204984584161908312330177564954877067154001079429559777704302946848644928736984e-1;
  d23 = .3451056668568212037936618506732414889276840710489545476476791628380007277211978725437;
  d24 = -.1182965488838884338743861432044520655991039617854064994056837668531638997325328536863;
  d25 = 0;
  d26 = .4137931034482758620689655172413793103448275862068965517241379310344827586206896551724;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+1]=a82;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+1]=a92;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+1]=a102;
  T.A[9*s+2]=a103;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+1]=a112;
  T.A[10*s+2]=a113;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+1]=a122;
  T.A[11*s+2]=a123;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+1]=a132;
  T.A[12*s+2]=a133;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  T.A[12*s+11]=a1312;
  T.A[13*s+0]=a141;
  T.A[13*s+1]=a142;
  T.A[13*s+2]=a143;
  T.A[13*s+3]=a144;
  T.A[13*s+4]=a145;
  T.A[13*s+5]=a146;
  T.A[13*s+6]=a147;
  T.A[13*s+7]=a148;
  T.A[13*s+8]=a149;
  T.A[13*s+9]=a1410;
  T.A[13*s+10]=a1411;
  T.A[13*s+11]=a1412;
  T.A[13*s+12]=a1413;
  T.A[14*s+0]=a151;
  T.A[14*s+1]=a152;
  T.A[14*s+2]=a153;
  T.A[14*s+3]=a154;
  T.A[14*s+4]=a155;
  T.A[14*s+5]=a156;
  T.A[14*s+6]=a157;
  T.A[14*s+7]=a158;
  T.A[14*s+8]=a159;
  T.A[14*s+9]=a1510;
  T.A[14*s+10]=a1511;
  T.A[14*s+11]=a1512;
  T.A[14*s+12]=a1513;
  T.A[14*s+13]=a1514;
  T.A[15*s+0]=a161;
  T.A[15*s+1]=a162;
  T.A[15*s+2]=a163;
  T.A[15*s+3]=a164;
  T.A[15*s+4]=a165;
  T.A[15*s+5]=a166;
  T.A[15*s+6]=a167;
  T.A[15*s+7]=a168;
  T.A[15*s+8]=a169;
  T.A[15*s+9]=a1610;
  T.A[15*s+10]=a1611;
  T.A[15*s+11]=a1612;
  T.A[15*s+12]=a1613;
  T.A[15*s+13]=a1614;
  T.A[15*s+14]=a1615;
  T.A[16*s+0]=a171;
  T.A[16*s+1]=a172;
  T.A[16*s+2]=a173;
  T.A[16*s+3]=a174;
  T.A[16*s+4]=a175;
  T.A[16*s+5]=a176;
  T.A[16*s+6]=a177;
  T.A[16*s+7]=a178;
  T.A[16*s+8]=a179;
  T.A[16*s+9]=a1710;
  T.A[16*s+10]=a1711;
  T.A[16*s+11]=a1712;
  T.A[16*s+12]=a1713;
  T.A[16*s+13]=a1714;
  T.A[16*s+14]=a1715;
  T.A[16*s+15]=a1716;
  T.A[17*s+0]=a181;
  T.A[17*s+1]=a182;
  T.A[17*s+2]=a183;
  T.A[17*s+3]=a184;
  T.A[17*s+4]=a185;
  T.A[17*s+5]=a186;
  T.A[17*s+6]=a187;
  T.A[17*s+7]=a188;
  T.A[17*s+8]=a189;
  T.A[17*s+9]=a1810;
  T.A[17*s+10]=a1811;
  T.A[17*s+11]=a1812;
  T.A[17*s+12]=a1813;
  T.A[17*s+13]=a1814;
  T.A[17*s+14]=a1815;
  T.A[17*s+15]=a1816;
  T.A[17*s+16]=a1817;
  T.A[18*s+0]=a191;
  T.A[18*s+1]=a192;
  T.A[18*s+2]=a193;
  T.A[18*s+3]=a194;
  T.A[18*s+4]=a195;
  T.A[18*s+5]=a196;
  T.A[18*s+6]=a197;
  T.A[18*s+7]=a198;
  T.A[18*s+8]=a199;
  T.A[18*s+9]=a1910;
  T.A[18*s+10]=a1911;
  T.A[18*s+11]=a1912;
  T.A[18*s+12]=a1913;
  T.A[18*s+13]=a1914;
  T.A[18*s+14]=a1915;
  T.A[18*s+15]=a1916;
  T.A[18*s+16]=a1917;
  T.A[18*s+17]=a1918;
  T.A[19*s+0]=a201;
  T.A[19*s+1]=a202;
  T.A[19*s+2]=a203;
  T.A[19*s+3]=a204;
  T.A[19*s+4]=a205;
  T.A[19*s+5]=a206;
  T.A[19*s+6]=a207;
  T.A[19*s+7]=a208;
  T.A[19*s+8]=a209;
  T.A[19*s+9]=a2010;
  T.A[19*s+10]=a2011;
  T.A[19*s+11]=a2012;
  T.A[19*s+12]=a2013;
  T.A[19*s+13]=a2014;
  T.A[19*s+14]=a2015;
  T.A[19*s+15]=a2016;
  T.A[19*s+16]=a2017;
  T.A[19*s+17]=a2018;
  T.A[19*s+18]=a2019;
  T.A[20*s+0]=a211;
  T.A[20*s+1]=a212;
  T.A[20*s+2]=a213;
  T.A[20*s+3]=a214;
  T.A[20*s+4]=a215;
  T.A[20*s+5]=a216;
  T.A[20*s+6]=a217;
  T.A[20*s+7]=a218;
  T.A[20*s+8]=a219;
  T.A[20*s+9]=a2110;
  T.A[20*s+10]=a2111;
  T.A[20*s+11]=a2112;
  T.A[20*s+12]=a2113;
  T.A[20*s+13]=a2114;
  T.A[20*s+14]=a2115;
  T.A[20*s+15]=a2116;
  T.A[20*s+16]=a2117;
  T.A[20*s+17]=a2118;
  T.A[20*s+18]=a2119;
  T.A[20*s+19]=a2120;
  T.A[21*s+0]=a221;
  T.A[21*s+1]=a222;
  T.A[21*s+2]=a223;
  T.A[21*s+3]=a224;
  T.A[21*s+4]=a225;
  T.A[21*s+5]=a226;
  T.A[21*s+6]=a227;
  T.A[21*s+7]=a228;
  T.A[21*s+8]=a229;
  T.A[21*s+9]=a2210;
  T.A[21*s+10]=a2211;
  T.A[21*s+11]=a2212;
  T.A[21*s+12]=a2213;
  T.A[21*s+13]=a2214;
  T.A[21*s+14]=a2215;
  T.A[21*s+15]=a2216;
  T.A[21*s+16]=a2217;
  T.A[21*s+17]=a2218;
  T.A[21*s+18]=a2219;
  T.A[21*s+19]=a2220;
  T.A[21*s+20]=a2221;
  T.A[22*s+0]=a231;
  T.A[22*s+1]=a232;
  T.A[22*s+2]=a233;
  T.A[22*s+3]=a234;
  T.A[22*s+4]=a235;
  T.A[22*s+5]=a236;
  T.A[22*s+6]=a237;
  T.A[22*s+7]=a238;
  T.A[22*s+8]=a239;
  T.A[22*s+9]=a2310;
  T.A[22*s+10]=a2311;
  T.A[22*s+11]=a2312;
  T.A[22*s+12]=a2313;
  T.A[22*s+13]=a2314;
  T.A[22*s+14]=a2315;
  T.A[22*s+15]=a2316;
  T.A[22*s+16]=a2317;
  T.A[22*s+17]=a2318;
  T.A[22*s+18]=a2319;
  T.A[22*s+19]=a2320;
  T.A[22*s+20]=a2321;
  T.A[22*s+21]=a2322;
  T.A[23*s+0]=a241;
  T.A[23*s+1]=a242;
  T.A[23*s+2]=a243;
  T.A[23*s+3]=a244;
  T.A[23*s+4]=a245;
  T.A[23*s+5]=a246;
  T.A[23*s+6]=a247;
  T.A[23*s+7]=a248;
  T.A[23*s+8]=a249;
  T.A[23*s+9]=a2410;
  T.A[23*s+10]=a2411;
  T.A[23*s+11]=a2412;
  T.A[23*s+12]=a2413;
  T.A[23*s+13]=a2414;
  T.A[23*s+14]=a2415;
  T.A[23*s+15]=a2416;
  T.A[23*s+16]=a2417;
  T.A[23*s+17]=a2418;
  T.A[23*s+18]=a2419;
  T.A[23*s+19]=a2420;
  T.A[23*s+20]=a2421;
  T.A[23*s+21]=a2422;
  T.A[23*s+22]=a2423;
  T.A[24*s+0]=a251;
  T.A[24*s+1]=a252;
  T.A[24*s+2]=a253;
  T.A[24*s+3]=a254;
  T.A[24*s+4]=a255;
  T.A[24*s+5]=a256;
  T.A[24*s+6]=a257;
  T.A[24*s+7]=a258;
  T.A[24*s+8]=a259;
  T.A[24*s+9]=a2510;
  T.A[24*s+10]=a2511;
  T.A[24*s+11]=a2512;
  T.A[24*s+12]=a2513;
  T.A[24*s+13]=a2514;
  T.A[24*s+14]=a2515;
  T.A[24*s+15]=a2516;
  T.A[24*s+16]=a2517;
  T.A[24*s+17]=a2518;
  T.A[24*s+18]=a2519;
  T.A[24*s+19]=a2520;
  T.A[24*s+20]=a2521;
  T.A[24*s+21]=a2522;
  T.A[24*s+22]=a2523;
  T.A[24*s+23]=a2524;
  T.A[25*s+0]=a261;
  T.A[25*s+1]=a262;
  T.A[25*s+2]=a263;
  T.A[25*s+3]=a264;
  T.A[25*s+4]=a265;
  T.A[25*s+5]=a266;
  T.A[25*s+6]=a267;
  T.A[25*s+7]=a268;
  T.A[25*s+8]=a269;
  T.A[25*s+9]=a2610;
  T.A[25*s+10]=a2611;
  T.A[25*s+11]=a2612;
  T.A[25*s+12]=a2613;
  T.A[25*s+13]=a2614;
  T.A[25*s+14]=a2615;
  T.A[25*s+15]=a2616;
  T.A[25*s+16]=a2617;
  T.A[25*s+17]=a2618;
  T.A[25*s+18]=a2619;
  T.A[25*s+19]=a2620;
  T.A[25*s+20]=a2621;
  T.A[25*s+21]=a2622;
  T.A[25*s+22]=a2623;
  T.A[25*s+23]=a2624;
  T.A[25*s+24]=a2625;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=b2;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.b[13]=b14;
  T.b[14]=b15;
  T.b[15]=b16;
  T.b[16]=b17;
  T.b[17]=b18;
  T.b[18]=b19;
  T.b[19]=b20;
  T.b[20]=b21;
  T.b[21]=b22;
  T.b[22]=b23;
  T.b[23]=b24;
  T.b[24]=b25;
  T.b[25]=b26;
  T.bhat[0]=d1;
  T.bhat[1]=d2;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
  T.bhat[13]=d14;
  T.bhat[14]=d15;
  T.bhat[15]=d16;
  T.bhat[16]=d17;
  T.bhat[17]=d18;
  T.bhat[18]=d19;
  T.bhat[19]=d20;
  T.bhat[20]=d21;
  T.bhat[21]=d22;
  T.bhat[22]=d23;
  T.bhat[23]=d24;
  T.bhat[24]=d25;
  T.bhat[25]=d26;
}

static void rksTableauO129(rksTableau &T) {
  const int s = 29; T.s = s; T.adaptive = 1; T.errOrder = 9;
  double c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c26, c27, c28, a21, a31, a32, a41, a42, a43, a51, a52, a53, a54, a61, a62, a63, a64, a65, a71, a72, a73, a74, a75, a76, a81, a82, a83, a84, a85, a86, a87, a91, a92, a93, a94, a95, a96, a97, a98, a101, a102, a103, a104, a105, a106, a107, a108, a109, a111, a112, a113, a114, a115, a116, a117, a118, a119, a1110, a121, a122, a123, a124, a125, a126, a127, a128, a129, a1210, a1211, a131, a132, a133, a134, a135, a136, a137, a138, a139, a1310, a1311, a1312, a141, a142, a143, a144, a145, a146, a147, a148, a149, a1410, a1411, a1412, a1413, a151, a152, a153, a154, a155, a156, a157, a158, a159, a1510, a1511, a1512, a1513, a1514, a161, a162, a163, a164, a165, a166, a167, a168, a169, a1610, a1611, a1612, a1613, a1614, a1615, a171, a172, a173, a174, a175, a176, a177, a178, a179, a1710, a1711, a1712, a1713, a1714, a1715, a1716, a181, a182, a183, a184, a185, a186, a187, a188, a189, a1810, a1811, a1812, a1813, a1814, a1815, a1816, a1817, a191, a192, a193, a194, a195, a196, a197, a198, a199, a1910, a1911, a1912, a1913, a1914, a1915, a1916, a1917, a1918, a201, a202, a203, a204, a205, a206, a207, a208, a209, a2010, a2011, a2012, a2013, a2014, a2015, a2016, a2017, a2018, a2019, a211, a212, a213, a214, a215, a216, a217, a218, a219, a2110, a2111, a2112, a2113, a2114, a2115, a2116, a2117, a2118, a2119, a2120, a221, a222, a223, a224, a225, a226, a227, a228, a229, a2210, a2211, a2212, a2213, a2214, a2215, a2216, a2217, a2218, a2219, a2220, a2221, a231, a232, a233, a234, a235, a236, a237, a238, a239, a2310, a2311, a2312, a2313, a2314, a2315, a2316, a2317, a2318, a2319, a2320, a2321, a2322, a241, a242, a243, a244, a245, a246, a247, a248, a249, a2410, a2411, a2412, a2413, a2414, a2415, a2416, a2417, a2418, a2419, a2420, a2421, a2422, a2423, a251, a252, a253, a254, a255, a256, a257, a258, a259, a2510, a2511, a2512, a2513, a2514, a2515, a2516, a2517, a2518, a2519, a2520, a2521, a2522, a2523, a2524, a261, a262, a263, a264, a265, a266, a267, a268, a269, a2610, a2611, a2612, a2613, a2614, a2615, a2616, a2617, a2618, a2619, a2620, a2621, a2622, a2623, a2624, a2625, a271, a272, a273, a274, a275, a276, a277, a278, a279, a2710, a2711, a2712, a2713, a2714, a2715, a2716, a2717, a2718, a2719, a2720, a2721, a2722, a2723, a2724, a2725, a2726, a281, a282, a283, a284, a285, a286, a287, a288, a289, a2810, a2811, a2812, a2813, a2814, a2815, a2816, a2817, a2818, a2819, a2820, a2821, a2822, a2823, a2824, a2825, a2826, a2827, a291, a292, a293, a294, a295, a296, a297, a298, a299, a2910, a2911, a2912, a2913, a2914, a2915, a2916, a2917, a2918, a2919, a2920, a2921, a2922, a2923, a2924, a2925, a2926, a2927, a2928, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22, b23, b24, b25, b26, b27, b28, b29, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29;
  c2 = .4351851851851851851851851851851851851851851851851851851851851851851851851851851851852;
  c3 = .4429824561403508771929824561403508771929824561403508771929824561403508771929824561404;
  c4 = .6644736842105263157894736842105263157894736842105263157894736842105263157894736842105;
  c5 = .1069403994175161223216143124609943831911795298522987310172664863740378614520490950697;
  c6 = .1644736842105263157894736842105263157894736842105263157894736842105263157894736842105;
  c7 = .5843251088534107402031930333817126269956458635703918722786647314949201741654571843251;
  c8 = .6382358235823582358235823582358235823582358235823582358235823582358235823582358235824e-1;
  c9 = .2;
  c10 = .3333333333333333333333333333333333333333333333333333333333333333333333333333333333333;
  c11 = .9446116054065563496847375720237340591364440847545279548637407620203329280139199114088;
  c12 = .5179584680428461035835615460185207449756996828425551866437683588600807071775316523299e-1;
  c13 = .8488805186071653506398389301626743020641481756400195420459339398355773991365476236893e-1;
  c14 = .2655756032646428930981140590456168352972012641640776214486652703185222349414361456016;
  c15 = .5;
  c16 = .7344243967353571069018859409543831647027987358359223785513347296814777650585638543984;
  c17 = .9151119481392834649360161069837325697935851824359980457954066060164422600863452376311;
  c18 = .9446116054065563496847375720237340591364440847545279548637407620203329280139199114088;
  c19 = .3333333333333333333333333333333333333333333333333333333333333333333333333333333333333;
  c20 = .2;
  c21 = .5843251088534107402031930333817126269956458635703918722786647314949201741654571843251;
  c22 = .1644736842105263157894736842105263157894736842105263157894736842105263157894736842105;
  c23 = .4429824561403508771929824561403508771929824561403508771929824561403508771929824561404;
  c24 = .4351851851851851851851851851851851851851851851851851851851851851851851851851851851852;
  c26 = .4970267001007476028032930885363848318550815967070143979104342007017543082576391973337;
  c27 = .8043478260869565217391304347826086956521739130434782608695652173913043478260869565217;
  c28 = .8717948717948717948717948717948717948717948717948717948717948717948717948717948717949;
  a21 = .4351851851851851851851851851851851851851851851851851851851851851851851851851851851852;
  a31 = .2175227402212137286104398734798923400326123258875071216675507357419304139407870179368;
  a32 = .2254597159191371485825425826604585371603701302528437555254317203984204632521954382036;
  a41 = .1661184210526315789473684210526315789473684210526315789473684210526315789473684210526;
  a42 = 0;
  a43 = .4983552631578947368421052631578947368421052631578947368421052631578947368421052631579;
  a51 = .8681163193918508865080629713483674348617570159685880673878476405659784672427434203840e-1;
  a52 = 0;
  a53 = .3456981948164897000739691452644149026866903085576649936607708344667115529230058847943e-1;
  a54 = -.1444105200331793633658889920028385056366520260032657508759536112923114056452583544813e-1;
  a61 = .3850951504524952575244726520324404860898823844950484490138343541601000605579674908780e-1;
  a62 = 0;
  a63 = 0;
  a64 = .9889604363651382462812798900870548282288802143553903392828423536490053424426345577709e-4;
  a65 = .1258652731216402762123982910182735616976625577395859318541619645591514091994326716669;
  a71 = .5247404461891304721708365639844356354386624470737997647397329116437616959059854701778;
  a72 = 0;
  a73 = 0;
  a74 = .7610651429965941990931946190296373660533922130208900643092560661920652146268815424885e-1;
  a75 = -2.135538596825204678401302463372339475781988628449683891486081018353295131152366999963;
  a76 = 2.119016745189825526524339470866652730733632823644186992594087231585247087949150559862;
  a81 = .3572122856624484555412656871410309454657254402954350529356994466234212462699695879099e-1;
  a82 = 0;
  a83 = 0;
  a84 = 0;
  a85 = .4596205641509305161863483311675360886293026632692961086374314175410528945129931429031e-1;
  a86 = -.1800016957713219812376781513215261066354472582005125759344515744644655947135707070493e-1;
  a87 = .1404669540301245333646491248782654898654978218139650184903068535815036288843799818693e-3;
  a91 = .1888176809184108413680419706237505704281864015973418413153299364663524364341955734913e-1;
  a92 = 0;
  a93 = 0;
  a94 = 0;
  a95 = 0;
  a96 = .8381198329740939383758467268684128106638290469173528975872944409244863365387981657651e-1;
  a97 = .9031585241436450659339626877786556727134244569192456020771509670971435444573825121328e-5;
  a98 = .9729721702550808557495179062390587533407132090396133365371679075124515126725605224924e-1;
  a101 = -.4080067703469846387001599007585094416645285179656696882006645696781274941696191338570e-1;
  a102 = 0;
  a103 = 0;
  a104 = 0;
  a105 = 0;
  a106 = -.6005539308646711394173617417282635784603029781694346062080009050806164919590318269156;
  a107 = .1585222658367901823092810289995660365558277114283363946284113044047141269675809741481e-2;
  a108 = .3026658851734428583799288358366834830670493200050903757742452562250879940584583879948;
  a109 = .6704368334008921764176894190107687125274815661799611686408713261126274393811928758984;
  a111 = 6.344326927733666873696263058980778771328558370914152084380122688263116113189631874466;
  a112 = 0;
  a113 = 0;
  a114 = 0;
  a115 = 0;
  a116 = 0;
  a117 = 1.975263319684766813850955500728188106557880392559127720135666889111189136253813975729;
  a118 = -13.82822337504897849061527131653164721900473695395858210726164571154383357718661916508;
  a119 = 14.82423926991066347292103944504014293055447712090877566105421769783705551827529470930;
  a1110 = -8.370994536873562320168249116193728530299734845668945403444620801647194262518201483006;
  a121 = -.9910783781470375735018424706990342484478259963427121190616343432259493286903501070750e-1;
  a122 = 0;
  a123 = 0;
  a124 = 0;
  a125 = 0;
  a126 = -1.046319958132641377892211373672215159710875883372049858922517534871447317958500644882;
  a127 = -.4662578801256029967826301701912208605727776822301916928051722653634090636623902615221e-3;
  a128 = .4243518408197806737520879584209906999975132950474577957967837636354467847566731274758;
  a129 = .8350767448298191909277817680995907515365477233544915021061336714983604939879298121048;
  a1210 = -.6204379562043795620437956204379562043795620437956204379562043795620437956204379562044e-1;
  a1211 = .3051106025934401220442410373760488176964149504195270785659801678108314263920671243326e-3;
  a131 = .1731635805893703684448829089734942070445562382116745420114094326722705203174775140332e-1;
  a132 = 0;
  a133 = 0;
  a134 = 0;
  a135 = 0;
  a136 = 0;
  a137 = 0;
  a138 = 0;
  a139 = .8690027159880925811795093256330430220184719404902985010762237927304116502630885425916e-3;
  a1310 = -.9198044746158460357290146202828770811642343120551843217420771155968903404182588708338e-4;
  a1311 = .1833594777349928706130754092439075395240645268074916590065144720493485758190652981058e-6;
  a1312 = .6679448817377525524901838117990401028051762116902291244289142812068791591710992924480e-1;
  a141 = .1497702285787817250603134142375551527881374815295532946701659971640303704469333256722e-1;
  a142 = 0;
  a143 = 0;
  a144 = 0;
  a145 = 0;
  a146 = 0;
  a147 = 0;
  a148 = 0;
  a149 = .1299053198125687130806056561360945267563487713700848629960379786258472215118648828622;
  a1410 = .4444252090193421415880477613428889941728587016143899156135432416265141679149665438354e-2;
  a1411 = -.2185380330434085597487818850115824768660155325792369439283645364044817815712239904765e-5;
  a1412 = .6235770138025566019351747531432668514647398310437277172705875380249580436468314885283e-1;
  a1413 = .5389349250407735998767659637686133399860483467584655047185578940287507515886082812093e-1;
  a151 = .1956886038861791180930007029621719896471785620210633245797118818081004119873004962774;
  a152 = 0;
  a153 = 0;
  a154 = 0;
  a155 = 0;
  a156 = 0;
  a157 = 0;
  a158 = 0;
  a159 = 1.132878041352190960246053936692850204846641933665191609652843227265803479941175715645;
  a1510 = .9754686357770750130309903534180324613492470735016258589778436907188769670738143543989;
  a1511 = .4607633555393391875625052489818876130584589587566695260680043303699764560744838206512e-3;
  a1512 = -.4833419929869440350491944569648386990066349467457051943375195864364996227787596872297;
  a1513 = .2837255261146423361717978265203569959020643630988946562818139270625464117810614651744;
  a1514 = -1.604879577498682731680210867877554840351555444499826924680761144749197624460666828087;
  a161 = -.6628581660952109109950216641584960475848453023977418371554567406302394052049642993363;
  a162 = 0;
  a163 = 0;
  a164 = 0;
  a165 = 0;
  a166 = 0;
  a167 = 0;
  a168 = 0;
  a169 = -5.301219753823165631138506283750981613687493380976143446419698251739201243786272433269;
  a1610 = -5.493744530005151771950832780438489668352801158207831962667055860430416893664761589330;
  a1611 = .6448107716343851659685115053410322161204197332247306929859781244843876115320017653542e-2;
  a1612 = 2.226911096857986220827171118161838780361655511952909244621862410327793805385281866476;
  a1613 = -.8260094546883369994121948528168152647220748853112540011094140493170779782006783468229;
  a1614 = 9.736973734199539440165209948098457305642565726332220268712929325968631361580234056276;
  a1615 = 1.047923362573352907746375340805459350884588027111516805638308114257144242834404582751;
  a171 = 9.451896878619703179615895147719645120861585484250220485534769741441232402797149004402;
  a172 = 0;
  a173 = 0;
  a174 = 0;
  a175 = 0;
  a176 = 0;
  a177 = 0;
  a178 = 0;
  a179 = 74.07837676951819392708315469048815926453257791083353986411194358002277837032529121349;
  a1710 = 80.08971633421252928663933464653279322909492698803384552922422585968931770071180510457;
  a1711 = -.1241702484260160323471778040612055855620194337087768444994146945534716737633295008233;
  a1712 = -32.04108125365225923900659816438329678215226279318337501510105142207810131441066638158;
  a1713 = 15.51919421000708125838831543558938623465692911684926717234376165413330242668122537342;
  a1714 = -136.4444237346563024309541869705527862321862115141467999073204884062451779712319267938;
  a1715 = -11.36109896858298168444633695622136642878717316645309025294277498926997344146696251530;
  a1716 = 1.746701961099335199963616081872403749335232589961167014444435282876535760443759733215;
  a181 = 1.059086740089530572084686602455620391446019980075022020682291957643879144548290466243;
  a182 = 0;
  a183 = 0;
  a184 = 0;
  a185 = 0;
  a186 = 0;
  a187 = 1.975263319684766813850955500728188106557880392559127720135666889111189136253813975729;
  a188 = -13.82822337504897849061527131653164721900473695395858210726164571154383357718661916508;
  a189 = -26.72676722061492102175215840759265382960786498230207636418025793583833107088763635551;
  a1810 = -53.49798986553787152824955918158085193982743760760492627590775850823540448031315562431;
  a1811 = .7179812487812968675250181644918743516316099586081748670146732996043783143974128607587e-1;
  a1812 = 18.05559723664965139501142471462605880255358278669785143935434349294122071206578011157;
  a1813 = -8.765163819232982113792324392383185125858432915348234208512916467037776994687248464210;
  a1814 = 76.87522358555575651988028075659565616302805855797883225625477685501643635288672453023;
  a1815 = 6.541007260506904941709851688460080123863085658071943624909334991972860537373864701434;
  a1816 = -.8461837296739539683436767973109554382501241857205608248520628152174372445350689400749;
  a1817 = .3096334815052354314802658810823658907325235844531318754050068324709258105543338930304e-1;
  a191 = -.9776673260040047976710026469624134014469714141641816253185153958371390334954366206102e-1;
  a192 = 0;
  a193 = 0;
  a194 = 0;
  a195 = 0;
  a196 = -.6005539308646711394173617417282635784603029781694346062080009050806164919590318269156;
  a197 = .1585222658367901823092810289995660365558277114283363946284113044047141269675809741481e-2;
  a198 = .3026658851734428583799288358366834830670493200050903757742452562250879940584583879948;
  a199 = .4285248952296136656501504776128834912424841890282487604085652387338305228046948965999;
  a1910 = -.2494936049956423479331713062821820760626388763661853215936426940474748269791918051828e-1;
  a1911 = .2011342149874705950638155217740310555133571768677965217932407885434178872356469888792e-1;
  a1912 = .1265809640150575117740069284741749647793461966567090378701137188708013785394651655531;
  a1913 = -.7625505511021937468044360160133448561674800889340550320120549846220783051800508863794e-2;
  a1914 = .2257683215130023640661938534278959810874704491725768321513002364066193853427895981087;
  a1915 = -.2745492489167708483879226183906024865124767290578864277433486220883760763766017543321e-1;
  a1916 = .7783761260627937338705544952370637303437265442198231246348540202794422441986844495010e-2;
  a1917 = .1680830476266175444138000910160343423421707378563384687234091879582812751321589095322e-4;
  a1918 = -.2135549195295375067495229039525877007339581782873806009604806379884885927885993014667e-1;
  a201 = .4918722777774214635454694806095471334564471116346374258920200467475606642975663633169e-1;
  a202 = 0;
  a203 = 0;
  a204 = 0;
  a205 = 0;
  a206 = .8381198329740939383758467268684128106638290469173528975872944409244863365387981657651e-1;
  a207 = .9031585241436450659339626877786556727134244569192456020771509670971435444573825121328e-5;
  a208 = .9729721702550808557495179062390587533407132090396133365371679075124515126725605224924e-1;
  a209 = -.7780700010332922911083090560520956762627768671838912755501179322945892692289272010345e-1;
  a2010 = .2322816011687352870985186646759446719321113455516507860765563208248738749278233611786;
  a2011 = -.1069713522971685698540328329852038445947544770807964964519088069861375575524890552230e-1;
  a2012 = -.1273357855008010164642613326804825893357151796749893640146110897003842493364412495521;
  a2013 = .1195596306045770335798408097988199315656849089154404333183111529360783290389646594900;
  a2014 = .1268882175226586102719033232628398791540785498489425981873111782477341389728096676737;
  a2015 = .2044989775051124744376278118609406952965235173824130879345603271983640081799591002045e-1;
  a2016 = -.3909856506984242164691157929655038995858933888334921275260056794557284704385445582377e-2;
  a2017 = -.9117410119332842109125953032188840693633119317454401715027062207348542639139803659651e-5;
  a2018 = .1126093151077456154585623418197514536662349793737574398626301058923382946852045056475e-1;
  a2019 = -.3209868434922071245903287586373535845929558438862699119277778588606558307508436673461;
  a211 = .5247404461891304721708365639844356354386624470737997647397329116437616959059854701778;
  a212 = 0;
  a213 = 0;
  a214 = .7610651429965941990931946190296373660533922130208900643092560661920652146268815424885e-1;
  a215 = -2.135538596825204678401302463372339475781988628449683891486081018353295131152366999963;
  a216 = 2.119016745189825526524339470866652730733632823644186992594087231585247087949150559862;
  a217 = 0;
  a218 = 0;
  a219 = -.6068669292751351984511141135476525247489731841233590933089036540462466843434512014723;
  a2110 = -.6975023816048118989159659127206041759560640169551283418432821453966450135301718130674;
  a2111 = -.2539552135383387231958801508125376392283214913791972115937570999107334296436460657368e-1;
  a2112 = 0;
  a2113 = 0;
  a2114 = 0;
  a2115 = 0;
  a2116 = 0;
  a2117 = 0;
  a2118 = .2539552135383387231958801508125376392283214913791972115937570999107334296436460657368e-1;
  a2119 = .6975023816048118989159659127206041759560640169551283418432821453966450135301718130674;
  a2120 = .6068669292751351984511141135476525247489731841233590933089036540462466843434512014723;
  a221 = .3850951504524952575244726520324404860898823844950484490138343541601000605579674908780e-1;
  a222 = 0;
  a223 = 0;
  a224 = .9889604363651382462812798900870548282288802143553903392828423536490053424426345577709e-4;
  a225 = .1258652731216402762123982910182735616976625577395859318541619645591514091994326716669;
  a226 = 0;
  a227 = .1148546657824708136802241649776446744025406100582655444423341878411637297467351936831;
  a228 = 0;
  a229 = -.1299246462925958527077352114079744607750386324042347411360003531859939834460301561195;
  a2210 = -.3664591598580916638621456622089859363144583004356560608229075984337239720284817960193;
  a2211 = 0;
  a2212 = 0;
  a2213 = 0;
  a2214 = 0;
  a2215 = 0;
  a2216 = 0;
  a2217 = 0;
  a2218 = 0;
  a2219 = .3664591598580916638621456622089859363144583004356560608229075984337239720284817960193;
  a2220 = .1299246462925958527077352114079744607750386324042347411360003531859939834460301561195;
  a2221 = -.1148546657824708136802241649776446744025406100582655444423341878411637297467351936831;
  a231 = .2175227402212137286104398734798923400326123258875071216675507357419304139407870179368;
  a232 = .2254597159191371485825425826604585371603701302528437555254317203984204632521954382036;
  a233 = 0;
  a234 = 0;
  a235 = 0;
  a236 = -.7003676470588235294117647058823529411764705882352941176470588235294117647058823529412;
  a237 = -.3841432262252079340469853296205751191707035527677198880717241749265840367609120672261;
  a238 = 0;
  a239 = 0;
  a2310 = 0;
  a2311 = 0;
  a2312 = 0;
  a2313 = 0;
  a2314 = 0;
  a2315 = 0;
  a2316 = 0;
  a2317 = 0;
  a2318 = 0;
  a2319 = 0;
  a2320 = 0;
  a2321 = .3841432262252079340469853296205751191707035527677198880717241749265840367609120672261;
  a2322 = .7003676470588235294117647058823529411764705882352941176470588235294117647058823529412;
  a241 = .4351851851851851851851851851851851851851851851851851851851851851851851851851851851852;
  a242 = 0;
  a243 = -.4244806610219170956648818689597945762515945523075081224120777274585003570971510665439;
  a244 = 0;
  a245 = 0;
  a246 = 0;
  a247 = 0;
  a248 = 0;
  a249 = 0;
  a2410 = 0;
  a2411 = 0;
  a2412 = 0;
  a2413 = 0;
  a2414 = 0;
  a2415 = 0;
  a2416 = 0;
  a2417 = 0;
  a2418 = 0;
  a2419 = 0;
  a2420 = 0;
  a2421 = 0;
  a2422 = 0;
  a2423 = .4244806610219170956648818689597945762515945523075081224120777274585003570971510665439;
  a251 = 14.54990971513478580914495526598245283940024810265087163382498896386800769812364397026;
  a252 = -2.609444444444444444444444444444444444444444444444444444444444444444444444444444444444;
  a253 = -2.016004609236637754870351028563643794559738431497207211298306162299623087053267335725;
  a254 = 0;
  a255 = 0;
  a256 = -1.666875;
  a257 = -1.840010137609049715480551028603992780751854184812582730319893278211480604766702319850;
  a258 = 0;
  a259 = 112.8850026879393650927243496060343039004107661597559344830054269802155904451112935844;
  a2510 = 123.3942086822776167534198661922997568027612066045886506351026838994566451157760161079;
  a2511 = -.7912126656078716671206667350992937018785263673658058027818457299775466324638082312137;
  a2512 = -50.05149873558555185701853277950418028083920384013973228797374663569746882026502756132;
  a2513 = 24.88778291494286023489265916235844707524170235927746532159915897878512972222657954044;
  a2514 = -212.1164197057320847511194729602871445996396409264694299135694995667783941778631290781;
  a2515 = -17.89082255740024165397920079197116480379826062999613139163493416384514698556677051724;
  a2516 = 2.509716434086569985363077956674238286724267256774032757480361264001771307725305753519;
  a2517 = .1162475886937088360535666476065014131326443901611275096836079807311057386735832665899;
  a2518 = .5840328281597421751021178306186155103094570763962374837158281176580253234011237044692;
  a2519 = 1.584417806712708397483834073813399023446091584920600008797782958694409008929749703075;
  a2520 = 1.338635006378392645053446531474068534729248229446179562750186952887872256191439757181;
  a2521 = 1.840010137609049715480551028603992780751854184812582730319893278211480604766702319850;
  a2522 = 1.666875;
  a2523 = 2.016004609236637754870351028563643794559738431497207211298306162299623087053267335725;
  a2524 = 2.609444444444444444444444444444444444444444444444444444444444444444444444444444444444;
  a261 = .4213659219087082450668941175898637044576077571412870755532166081975824639155816025423;
  a262 = 0;
  a263 = 0;
  a264 = 0;
  a265 = 0;
  a266 = 2.360375290413766425107807321597798068298832670876354152068674897991709951684675057597;
  a267 = .7887926811836902144270824477231365437442962856383491851349713361086542561144935964760e-1;
  a268 = -1.881850641776530466652474803895333308262409829541779704822719782535259533895036456964;
  a269 = -1.304700734906095391371228323883517431348033016096248869612640797091741378769699510853;
  a2610 = .1146971532060496506611311517441641422873299688900135018999537656144573134542692746612;
  a2611 = -.5223613182942077907170609676910338480915071906000186351215047373740452623507436746018e-2;
  a2612 = .7134840563194221964556259902880063405282394887795535106616674222878805188799073074497;
  a2613 = 0;
  a2614 = 0;
  a2615 = 0;
  a2616 = 0;
  a2617 = 0;
  a2618 = 0;
  a2619 = 0;
  a2620 = 0;
  a2621 = 0;
  a2622 = 0;
  a2623 = 0;
  a2624 = 0;
  a2625 = 0;
  a271 = -1.016867684065179179311540011641152067739527559831057381007428484343486238202737995538;
  a272 = 0;
  a273 = 0;
  a274 = 0;
  a275 = 0;
  a276 = -7.712044352285817603610736737545203003182107799250377304475316992902646516184627861475;
  a277 = -.4034008409374858753410643280039779311650023076266296210030337872976327092081717454860;
  a278 = 6.739165476490825275476530741799137001781411805688541615342712456896969009075069743117;
  a279 = 6.014994643407224294180918860565568523540180411761624603152605084567461778121724411633;
  a2710 = -1.138427387973993086846707441740657423236451007119331997503516596250482644801941463866;
  a2711 = .5009271973181599563449431362188397685579770251743883251930235916805543539186803311245e-1;
  a2712 = -3.113250932564715585587456369457050044282973515833171733791753110672694235302567710668;
  a2713 = 0;
  a2714 = 0;
  a2715 = 0;
  a2716 = 0;
  a2717 = 0;
  a2718 = 0;
  a2719 = 0;
  a2720 = 0;
  a2721 = 0;
  a2722 = 0;
  a2723 = 0;
  a2724 = 0;
  a2725 = 0;
  a2726 = 1.384086184284282287144691407184059663080846182736441247635994288225760468937471545692;
  a281 = 1.131093475949031458408970675798323789793651098141584053672723684523337489654810813567;
  a282 = 0;
  a283 = 0;
  a284 = 0;
  a285 = 0;
  a286 = -11.30475611955440577592346561419842170756276153921836803995513790246783417109406727002;
  a287 = .8673508908529372037894544277195364499375277491231106383198729858086287127092489733738e-1;
  a288 = 4.971317844154333915807514558966554931901059536397301577948574837963960265600948244094;
  a289 = 14.86493772010299652718002500847699984298963300991290479767277341466986550270499895786;
  a2810 = -5.526130551905351405702373768620234518347747212552226033118137855980983127699224086148;
  a2811 = .1017790491986200061558195486579246543940163889995857946318495764475820398967567810759;
  a2812 = -5.412708567655345677389304794550103449135846140313886894023804835964789906470796078495;
  a2813 = 0;
  a2814 = 0;
  a2815 = 0;
  a2816 = 0;
  a2817 = 0;
  a2818 = 0;
  a2819 = 0;
  a2820 = 0;
  a2821 = 0;
  a2822 = 0;
  a2823 = 0;
  a2824 = 0;
  a2825 = 0;
  a2826 = 2.119905903216124397337756706998226742489167533804374626293399155463227287948398852330;
  a2827 = -.1603789707964253713820928925063521366431305782887091520824325014403564569409562398070;
  a291 = 46.12864603958015905056850990838704062569763496465412763519385202873225169326091596022;
  a292 = 0;
  a293 = 0;
  a294 = 0;
  a295 = 0;
  a296 = 27.91300163119399908845158457840426795358131126287389180101909720743727165524743997096;
  a297 = 16.11362689862451240990975288339484000039234961533373225370151713993919933068137837655;
  a298 = -125.4696763444318726329250646477825268481685879278990572088898486327607587168793188547;
  a299 = 76.57182020120529497684089567511627659347626021577320126137924234653281598557298427556;
  a2910 = -48.97805558723490361747755876313897556229903489002597907918723603724236906416246304792;
  a2911 = -1.242830487244052672528847080627776989497284066852925470069357906835696725179561610962;
  a2912 = 18.85807213383620068645464308722546866214730809725606975205204214283143331463602851729;
  a2913 = 0;
  a2914 = 0;
  a2915 = 0;
  a2916 = 0;
  a2917 = 0;
  a2918 = 0;
  a2919 = 0;
  a2920 = 0;
  a2921 = 0;
  a2922 = 0;
  a2923 = 0;
  a2924 = 0;
  a2925 = 0;
  a2926 = -8.871982194511738170929283011936752038683182063824370821380217475185779921363890310462;
  a2927 = -2.069534982695615656321598541301254038890059866116256078922522295738656144906973495868;
  a2928 = 2.046912691678016537956965912259391642243284658827565955103431482290288593093460219351;
  b1 = .2380952380952380952380952380952380952380952380952380952380952380952380952380952380952e-1;
  b2 = -.11;
  b3 = -.17;
  b4 = 0;
  b5 = 0;
  b6 = -.19;
  b7 = -.21;
  b8 = 0;
  b9 = -.23;
  b10 = -.27;
  b11 = -.29;
  b12 = 0;
  b13 = .1384130236807829740053502031450331467488136400899412345912671194817223119377730668077;
  b14 = .2158726906049313117089355111406811389654720741957730511230185948039919737765126474781;
  b15 = .2438095238095238095238095238095238095238095238095238095238095238095238095238095238095;
  b16 = .2158726906049313117089355111406811389654720741957730511230185948039919737765126474781;
  b17 = .1384130236807829740053502031450331467488136400899412345912671194817223119377730668077;
  b18 = .29;
  b19 = .27;
  b20 = .23;
  b21 = .21;
  b22 = .19;
  b23 = .17;
  b24 = .11;
  b25 = .2380952380952380952380952380952380952380952380952380952380952380952380952380952380952e-1;
  b26 = 0;
  b27 = 0;
  b28 = 0;
  b29 = 0;
  d1 = .1357267366422036691624508570375039921213961405197383540990729271585608575744367221422e-1;
  d2 = 0;
  d3 = 0;
  d4 = 0;
  d5 = 0;
  d6 = 0;
  d7 = 0;
  d8 = 0;
  d9 = .1957242608025905233613155193355413222756712243739956539857744660017249157522413539338;
  d10 = .6188866347435608661616060238380345321740579185698262925072441030810461130118224626650e-1;
  d11 = .2356461254963383884566009189531765712282459575681722654363888116993755654359289982043;
  d12 = .9356981277656948171659125355126822887878370575133833270918675400840988929847712528189e-1;
  d13 = 0;
  d14 = 0;
  d15 = 0;
  d16 = 0;
  d17 = 0;
  d18 = 0;
  d19 = 0;
  d20 = 0;
  d21 = 0;
  d22 = 0;
  d23 = 0;
  d24 = 0;
  d25 = 0;
  d26 = .2788382624223597882496809755901865993371492355322230983832374605891024297949723722505;
  d27 = .4265887719284871852244002213010235951116369449606049493109912099005098392090245967637;
  d28 = -.2878025166474501962999477241233861487151201542084751225511851437798538088192303308430;
  d29 = -.1802605391747162424104685269536402054591231988681564193502526144322952773004003407203e-1;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+1]=a42;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+1]=a52;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+1]=a62;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+1]=a72;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+1]=a82;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+1]=a92;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+1]=a102;
  T.A[9*s+2]=a103;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  T.A[9*s+8]=a109;
  T.A[10*s+0]=a111;
  T.A[10*s+1]=a112;
  T.A[10*s+2]=a113;
  T.A[10*s+3]=a114;
  T.A[10*s+4]=a115;
  T.A[10*s+5]=a116;
  T.A[10*s+6]=a117;
  T.A[10*s+7]=a118;
  T.A[10*s+8]=a119;
  T.A[10*s+9]=a1110;
  T.A[11*s+0]=a121;
  T.A[11*s+1]=a122;
  T.A[11*s+2]=a123;
  T.A[11*s+3]=a124;
  T.A[11*s+4]=a125;
  T.A[11*s+5]=a126;
  T.A[11*s+6]=a127;
  T.A[11*s+7]=a128;
  T.A[11*s+8]=a129;
  T.A[11*s+9]=a1210;
  T.A[11*s+10]=a1211;
  T.A[12*s+0]=a131;
  T.A[12*s+1]=a132;
  T.A[12*s+2]=a133;
  T.A[12*s+3]=a134;
  T.A[12*s+4]=a135;
  T.A[12*s+5]=a136;
  T.A[12*s+6]=a137;
  T.A[12*s+7]=a138;
  T.A[12*s+8]=a139;
  T.A[12*s+9]=a1310;
  T.A[12*s+10]=a1311;
  T.A[12*s+11]=a1312;
  T.A[13*s+0]=a141;
  T.A[13*s+1]=a142;
  T.A[13*s+2]=a143;
  T.A[13*s+3]=a144;
  T.A[13*s+4]=a145;
  T.A[13*s+5]=a146;
  T.A[13*s+6]=a147;
  T.A[13*s+7]=a148;
  T.A[13*s+8]=a149;
  T.A[13*s+9]=a1410;
  T.A[13*s+10]=a1411;
  T.A[13*s+11]=a1412;
  T.A[13*s+12]=a1413;
  T.A[14*s+0]=a151;
  T.A[14*s+1]=a152;
  T.A[14*s+2]=a153;
  T.A[14*s+3]=a154;
  T.A[14*s+4]=a155;
  T.A[14*s+5]=a156;
  T.A[14*s+6]=a157;
  T.A[14*s+7]=a158;
  T.A[14*s+8]=a159;
  T.A[14*s+9]=a1510;
  T.A[14*s+10]=a1511;
  T.A[14*s+11]=a1512;
  T.A[14*s+12]=a1513;
  T.A[14*s+13]=a1514;
  T.A[15*s+0]=a161;
  T.A[15*s+1]=a162;
  T.A[15*s+2]=a163;
  T.A[15*s+3]=a164;
  T.A[15*s+4]=a165;
  T.A[15*s+5]=a166;
  T.A[15*s+6]=a167;
  T.A[15*s+7]=a168;
  T.A[15*s+8]=a169;
  T.A[15*s+9]=a1610;
  T.A[15*s+10]=a1611;
  T.A[15*s+11]=a1612;
  T.A[15*s+12]=a1613;
  T.A[15*s+13]=a1614;
  T.A[15*s+14]=a1615;
  T.A[16*s+0]=a171;
  T.A[16*s+1]=a172;
  T.A[16*s+2]=a173;
  T.A[16*s+3]=a174;
  T.A[16*s+4]=a175;
  T.A[16*s+5]=a176;
  T.A[16*s+6]=a177;
  T.A[16*s+7]=a178;
  T.A[16*s+8]=a179;
  T.A[16*s+9]=a1710;
  T.A[16*s+10]=a1711;
  T.A[16*s+11]=a1712;
  T.A[16*s+12]=a1713;
  T.A[16*s+13]=a1714;
  T.A[16*s+14]=a1715;
  T.A[16*s+15]=a1716;
  T.A[17*s+0]=a181;
  T.A[17*s+1]=a182;
  T.A[17*s+2]=a183;
  T.A[17*s+3]=a184;
  T.A[17*s+4]=a185;
  T.A[17*s+5]=a186;
  T.A[17*s+6]=a187;
  T.A[17*s+7]=a188;
  T.A[17*s+8]=a189;
  T.A[17*s+9]=a1810;
  T.A[17*s+10]=a1811;
  T.A[17*s+11]=a1812;
  T.A[17*s+12]=a1813;
  T.A[17*s+13]=a1814;
  T.A[17*s+14]=a1815;
  T.A[17*s+15]=a1816;
  T.A[17*s+16]=a1817;
  T.A[18*s+0]=a191;
  T.A[18*s+1]=a192;
  T.A[18*s+2]=a193;
  T.A[18*s+3]=a194;
  T.A[18*s+4]=a195;
  T.A[18*s+5]=a196;
  T.A[18*s+6]=a197;
  T.A[18*s+7]=a198;
  T.A[18*s+8]=a199;
  T.A[18*s+9]=a1910;
  T.A[18*s+10]=a1911;
  T.A[18*s+11]=a1912;
  T.A[18*s+12]=a1913;
  T.A[18*s+13]=a1914;
  T.A[18*s+14]=a1915;
  T.A[18*s+15]=a1916;
  T.A[18*s+16]=a1917;
  T.A[18*s+17]=a1918;
  T.A[19*s+0]=a201;
  T.A[19*s+1]=a202;
  T.A[19*s+2]=a203;
  T.A[19*s+3]=a204;
  T.A[19*s+4]=a205;
  T.A[19*s+5]=a206;
  T.A[19*s+6]=a207;
  T.A[19*s+7]=a208;
  T.A[19*s+8]=a209;
  T.A[19*s+9]=a2010;
  T.A[19*s+10]=a2011;
  T.A[19*s+11]=a2012;
  T.A[19*s+12]=a2013;
  T.A[19*s+13]=a2014;
  T.A[19*s+14]=a2015;
  T.A[19*s+15]=a2016;
  T.A[19*s+16]=a2017;
  T.A[19*s+17]=a2018;
  T.A[19*s+18]=a2019;
  T.A[20*s+0]=a211;
  T.A[20*s+1]=a212;
  T.A[20*s+2]=a213;
  T.A[20*s+3]=a214;
  T.A[20*s+4]=a215;
  T.A[20*s+5]=a216;
  T.A[20*s+6]=a217;
  T.A[20*s+7]=a218;
  T.A[20*s+8]=a219;
  T.A[20*s+9]=a2110;
  T.A[20*s+10]=a2111;
  T.A[20*s+11]=a2112;
  T.A[20*s+12]=a2113;
  T.A[20*s+13]=a2114;
  T.A[20*s+14]=a2115;
  T.A[20*s+15]=a2116;
  T.A[20*s+16]=a2117;
  T.A[20*s+17]=a2118;
  T.A[20*s+18]=a2119;
  T.A[20*s+19]=a2120;
  T.A[21*s+0]=a221;
  T.A[21*s+1]=a222;
  T.A[21*s+2]=a223;
  T.A[21*s+3]=a224;
  T.A[21*s+4]=a225;
  T.A[21*s+5]=a226;
  T.A[21*s+6]=a227;
  T.A[21*s+7]=a228;
  T.A[21*s+8]=a229;
  T.A[21*s+9]=a2210;
  T.A[21*s+10]=a2211;
  T.A[21*s+11]=a2212;
  T.A[21*s+12]=a2213;
  T.A[21*s+13]=a2214;
  T.A[21*s+14]=a2215;
  T.A[21*s+15]=a2216;
  T.A[21*s+16]=a2217;
  T.A[21*s+17]=a2218;
  T.A[21*s+18]=a2219;
  T.A[21*s+19]=a2220;
  T.A[21*s+20]=a2221;
  T.A[22*s+0]=a231;
  T.A[22*s+1]=a232;
  T.A[22*s+2]=a233;
  T.A[22*s+3]=a234;
  T.A[22*s+4]=a235;
  T.A[22*s+5]=a236;
  T.A[22*s+6]=a237;
  T.A[22*s+7]=a238;
  T.A[22*s+8]=a239;
  T.A[22*s+9]=a2310;
  T.A[22*s+10]=a2311;
  T.A[22*s+11]=a2312;
  T.A[22*s+12]=a2313;
  T.A[22*s+13]=a2314;
  T.A[22*s+14]=a2315;
  T.A[22*s+15]=a2316;
  T.A[22*s+16]=a2317;
  T.A[22*s+17]=a2318;
  T.A[22*s+18]=a2319;
  T.A[22*s+19]=a2320;
  T.A[22*s+20]=a2321;
  T.A[22*s+21]=a2322;
  T.A[23*s+0]=a241;
  T.A[23*s+1]=a242;
  T.A[23*s+2]=a243;
  T.A[23*s+3]=a244;
  T.A[23*s+4]=a245;
  T.A[23*s+5]=a246;
  T.A[23*s+6]=a247;
  T.A[23*s+7]=a248;
  T.A[23*s+8]=a249;
  T.A[23*s+9]=a2410;
  T.A[23*s+10]=a2411;
  T.A[23*s+11]=a2412;
  T.A[23*s+12]=a2413;
  T.A[23*s+13]=a2414;
  T.A[23*s+14]=a2415;
  T.A[23*s+15]=a2416;
  T.A[23*s+16]=a2417;
  T.A[23*s+17]=a2418;
  T.A[23*s+18]=a2419;
  T.A[23*s+19]=a2420;
  T.A[23*s+20]=a2421;
  T.A[23*s+21]=a2422;
  T.A[23*s+22]=a2423;
  T.A[24*s+0]=a251;
  T.A[24*s+1]=a252;
  T.A[24*s+2]=a253;
  T.A[24*s+3]=a254;
  T.A[24*s+4]=a255;
  T.A[24*s+5]=a256;
  T.A[24*s+6]=a257;
  T.A[24*s+7]=a258;
  T.A[24*s+8]=a259;
  T.A[24*s+9]=a2510;
  T.A[24*s+10]=a2511;
  T.A[24*s+11]=a2512;
  T.A[24*s+12]=a2513;
  T.A[24*s+13]=a2514;
  T.A[24*s+14]=a2515;
  T.A[24*s+15]=a2516;
  T.A[24*s+16]=a2517;
  T.A[24*s+17]=a2518;
  T.A[24*s+18]=a2519;
  T.A[24*s+19]=a2520;
  T.A[24*s+20]=a2521;
  T.A[24*s+21]=a2522;
  T.A[24*s+22]=a2523;
  T.A[24*s+23]=a2524;
  T.A[25*s+0]=a261;
  T.A[25*s+1]=a262;
  T.A[25*s+2]=a263;
  T.A[25*s+3]=a264;
  T.A[25*s+4]=a265;
  T.A[25*s+5]=a266;
  T.A[25*s+6]=a267;
  T.A[25*s+7]=a268;
  T.A[25*s+8]=a269;
  T.A[25*s+9]=a2610;
  T.A[25*s+10]=a2611;
  T.A[25*s+11]=a2612;
  T.A[25*s+12]=a2613;
  T.A[25*s+13]=a2614;
  T.A[25*s+14]=a2615;
  T.A[25*s+15]=a2616;
  T.A[25*s+16]=a2617;
  T.A[25*s+17]=a2618;
  T.A[25*s+18]=a2619;
  T.A[25*s+19]=a2620;
  T.A[25*s+20]=a2621;
  T.A[25*s+21]=a2622;
  T.A[25*s+22]=a2623;
  T.A[25*s+23]=a2624;
  T.A[25*s+24]=a2625;
  T.A[26*s+0]=a271;
  T.A[26*s+1]=a272;
  T.A[26*s+2]=a273;
  T.A[26*s+3]=a274;
  T.A[26*s+4]=a275;
  T.A[26*s+5]=a276;
  T.A[26*s+6]=a277;
  T.A[26*s+7]=a278;
  T.A[26*s+8]=a279;
  T.A[26*s+9]=a2710;
  T.A[26*s+10]=a2711;
  T.A[26*s+11]=a2712;
  T.A[26*s+12]=a2713;
  T.A[26*s+13]=a2714;
  T.A[26*s+14]=a2715;
  T.A[26*s+15]=a2716;
  T.A[26*s+16]=a2717;
  T.A[26*s+17]=a2718;
  T.A[26*s+18]=a2719;
  T.A[26*s+19]=a2720;
  T.A[26*s+20]=a2721;
  T.A[26*s+21]=a2722;
  T.A[26*s+22]=a2723;
  T.A[26*s+23]=a2724;
  T.A[26*s+24]=a2725;
  T.A[26*s+25]=a2726;
  T.A[27*s+0]=a281;
  T.A[27*s+1]=a282;
  T.A[27*s+2]=a283;
  T.A[27*s+3]=a284;
  T.A[27*s+4]=a285;
  T.A[27*s+5]=a286;
  T.A[27*s+6]=a287;
  T.A[27*s+7]=a288;
  T.A[27*s+8]=a289;
  T.A[27*s+9]=a2810;
  T.A[27*s+10]=a2811;
  T.A[27*s+11]=a2812;
  T.A[27*s+12]=a2813;
  T.A[27*s+13]=a2814;
  T.A[27*s+14]=a2815;
  T.A[27*s+15]=a2816;
  T.A[27*s+16]=a2817;
  T.A[27*s+17]=a2818;
  T.A[27*s+18]=a2819;
  T.A[27*s+19]=a2820;
  T.A[27*s+20]=a2821;
  T.A[27*s+21]=a2822;
  T.A[27*s+22]=a2823;
  T.A[27*s+23]=a2824;
  T.A[27*s+24]=a2825;
  T.A[27*s+25]=a2826;
  T.A[27*s+26]=a2827;
  T.A[28*s+0]=a291;
  T.A[28*s+1]=a292;
  T.A[28*s+2]=a293;
  T.A[28*s+3]=a294;
  T.A[28*s+4]=a295;
  T.A[28*s+5]=a296;
  T.A[28*s+6]=a297;
  T.A[28*s+7]=a298;
  T.A[28*s+8]=a299;
  T.A[28*s+9]=a2910;
  T.A[28*s+10]=a2911;
  T.A[28*s+11]=a2912;
  T.A[28*s+12]=a2913;
  T.A[28*s+13]=a2914;
  T.A[28*s+14]=a2915;
  T.A[28*s+15]=a2916;
  T.A[28*s+16]=a2917;
  T.A[28*s+17]=a2918;
  T.A[28*s+18]=a2919;
  T.A[28*s+19]=a2920;
  T.A[28*s+20]=a2921;
  T.A[28*s+21]=a2922;
  T.A[28*s+22]=a2923;
  T.A[28*s+23]=a2924;
  T.A[28*s+24]=a2925;
  T.A[28*s+25]=a2926;
  T.A[28*s+26]=a2927;
  T.A[28*s+27]=a2928;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=b2;
  T.b[2]=b3;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.b[10]=b11;
  T.b[11]=b12;
  T.b[12]=b13;
  T.b[13]=b14;
  T.b[14]=b15;
  T.b[15]=b16;
  T.b[16]=b17;
  T.b[17]=b18;
  T.b[18]=b19;
  T.b[19]=b20;
  T.b[20]=b21;
  T.b[21]=b22;
  T.b[22]=b23;
  T.b[23]=b24;
  T.b[24]=b25;
  T.b[25]=b26;
  T.b[26]=b27;
  T.b[27]=b28;
  T.b[28]=b29;
  T.bhat[0]=d1;
  T.bhat[1]=d2;
  T.bhat[2]=d3;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
  T.bhat[10]=d11;
  T.bhat[11]=d12;
  T.bhat[12]=d13;
  T.bhat[13]=d14;
  T.bhat[14]=d15;
  T.bhat[15]=d16;
  T.bhat[16]=d17;
  T.bhat[17]=d18;
  T.bhat[18]=d19;
  T.bhat[19]=d20;
  T.bhat[20]=d21;
  T.bhat[21]=d22;
  T.bhat[22]=d23;
  T.bhat[23]=d24;
  T.bhat[24]=d25;
  T.bhat[25]=d26;
  T.bhat[26]=d27;
  T.bhat[27]=d28;
  T.bhat[28]=d29;
}


static void rksTableauTmy7(rksTableau &T) {
  const int s = 10; T.s = s; T.adaptive = 1; T.errOrder = 6;
  double c2, c3, c4, c5, c6, c7, a21, a31, a32, a41, a43, a51, a53, a54, a61, a63, a64, a65, a71, a73, a74, a75, a76, a81, a83, a84, a85, a86, a87, a91, a93, a94, a95, a96, a97, a98, a101, a103, a104, a105, a106, a107, a108, b1, b4, b5, b6, b7, b8, b9, d1, d4, d5, d6, d7, d8, d10, b10, d9;
  c2 = 1.17249697651707701958958240365275541154938139862762978340867788103969008491621556948e-1;
  c3 = 1.75874546477561552938437360547913311732407209794144467511301682155953512737432335422e-1;
  c4 = 4.98740110191398804083870799279316917774238190981016789826879390030550695876961641904e-1;
  c5 = 7.72121690184483890484305252570415231459539559725240465368497233385136757456140428136e-1;
  c6 = 9.91185669604776798407733864088711970429343190218936593687802104065965311344896218368e-1;
  c7 = 9.99501958209766146741222013447128336314037987005636927534919520974350847802956557173e-1;
  a21 = 7.81664651011384679726388269101836941032920932418419855605785254026460056610810379655e-2;
  a31 = 2.93124244129269254897395600913188852887345349656907445852169470259922521229053892370e-2;
  a32 = 8.79372732387807764692186802739566558662036048970722337556508410779767563687161677111e-2;
  a41 = 4.39686366193903882346093401369783279331018024485361168778254205389883781843580838556e-2;
  a43 = 1.31905909858171164703828020410934983799305407345608350633476261616965134553074251567e-1;
  a51 = 7.36183483773838301137467440432293617773745769566559352064158169276511636772818047624e-1;
  a53 = -2.8337999624233305489833449285093928914333838008174765752291037107738552931533021862e0;
  a54 = 2.59635658884089105192974828735641619143387622223193401299182493152789435225744578050e0;
  a61 = -1.2062819391370865506697620184830239019171754039710073663311602518203645149254198062e1;
  a63 = 4.82083810017524271975659667922264091479441542231216831647946440093325968906031102739e1;
  a64 = -3.8058630464634340873444069083787729069064369254270820186967532356616235833794046365e1;
  a65 = 2.68519054443726307306002772896197417175150863058445115085298809887242084990127458112e0;
  a71 = 1.05219572763201983263505214763646609723666799568263028237820996736734142311884342584e2;
  a73 = -4.1792888626241254423220090632342880293119367792924845836746399438762371771451577296e2;
  a74 = 3.32315550449933288661672145724565517116512854686499482568452142501473088581714784505e2;
  a75 = -1.9827591183572936803332556016677309852484292769162933988083092149281072338147267880e1;
  a76 = 1.21253990245498590876383571598269791392765963386781814296174940276352447040880996631e0;
  a81 = 1.14677557186317412777743962690232735459737977190570736656211705995064581461317681933e2;
  a83 = -4.5556121698960972374080326255401158427067549223697915892004464775745251977067045545e2;
  a84 = 3.62240955539231418592330010344238763220304805879492791461256324311641976192069398751e2;
  a85 = -2.1671904421828087451986108533232639587308358187016768553336461965905412178073526990e1;
  a86 = 1.31891320071378062521147328839749773179746572184191322809881380227032622170542838742e0;
  a87 = -4.8025566150346557548532221776442175423603809038769446508148646446010785455700778379e-3;
  a91 = 1.15213348705537687993035352351751211261221390205532219553682499922866411003708635464e2;
  a93 = -4.5769356568613233007695763193867218928825178792989579576839091929408867970634224781e2;
  a94 = 3.63936882188627343156610348429345771832876086443132263964527166628432626799709477320e2;
  a95 = -2.1776682078900292925704872499507823559172972633117930893260468010116663302476870140e1;
  a96 = 1.32506708878784683255232611830210096225232882939350575184015801179589744903384223122e0;
  a97 = -4.5181909867689836302434492066200005706653127046344820697054796285731244656302707584e-3;
  a98 = -5.3202693348599590527901201245120835437960233962812632873177926101911916720679884551e-4;
  a101 = 1.15189282458001936117920705886017744028740693856879151180637835667882356288187386531e2;
  a103 = -4.5759802227164297152415973359923257831016657330505652256715384079434141772791111922e2;
  a104 = 3.63861025631214796441099277168858079581214448605485290879259097757252663203289829524e2;
  a105 = -2.1772127540275557896757277605910244539671287153847928139832256704010642220109132278e1;
  a106 = 1.32488046450743160713676531081866548206715403286785238191455539176685996800150702540e0;
  a107 = -4.5057252106918316437819055679743451410766257174024328014118092080515035470892820222e-3;
  a108 = -5.3301659494291359595525498369189704335941061044130202397950934176800791138229684609e-4;
  b1 = 5.12601424974468664207334896467928192605824917480148880826975913119881823951586777925e-2;
  b4 = 2.75216384562126285009146859937694605711798961533809877651595696709236573932734806466e-1;
  b5 = 3.36966503407105433969810392957648333413899278562014739968412391495966278358775981408e-1;
  b6 = 1.89860722449065759713312922673232536516903910783954769517234286153030491598156826344e-1;
  b7 = 8.46109941851440210639662306074261643772685843568635085555647158979307742068711096848e0;
  b8 = -1.3015941672640542180639580507865346065998831988530342657791656834495604969014688205e2;
  b9 = 1.21845013554975275354886178352695475927358276806989281447140156789492750743174944785e2;
  d1 = 5.10024175903771829479213461329816195478958717593517724635264723499155306525942551495e-2;
  d4 = 2.76139295046665466902620362521695093962636297177632180749087744362855793802931918925e-1;
  d5 = 3.33788602069686023554754031916460354879718501346567668959954157311633875576165846369e-1;
  d6 = 2.01965311390813930477691388976007742664325756050214204738992402177761682278944674291e-1;
  d7 = 5.75507545904181112023147975998801190281904137375217710125886160168224573401735815006e0;
  d8 = -8.5617971085139353724114466889535156713873617800085942928170422377884412616327994845e1;
  d10 = 8.00000000000000000000000000000000000000000000000000000000000000000000000000000000000e1;
  b10 = 0.0;
  d9 = 0.0;
  T.A[1*s+0]=a21;
  T.A[2*s+0]=a31;
  T.A[2*s+1]=a32;
  T.A[3*s+0]=a41;
  T.A[3*s+2]=a43;
  T.A[4*s+0]=a51;
  T.A[4*s+2]=a53;
  T.A[4*s+3]=a54;
  T.A[5*s+0]=a61;
  T.A[5*s+2]=a63;
  T.A[5*s+3]=a64;
  T.A[5*s+4]=a65;
  T.A[6*s+0]=a71;
  T.A[6*s+2]=a73;
  T.A[6*s+3]=a74;
  T.A[6*s+4]=a75;
  T.A[6*s+5]=a76;
  T.A[7*s+0]=a81;
  T.A[7*s+2]=a83;
  T.A[7*s+3]=a84;
  T.A[7*s+4]=a85;
  T.A[7*s+5]=a86;
  T.A[7*s+6]=a87;
  T.A[8*s+0]=a91;
  T.A[8*s+2]=a93;
  T.A[8*s+3]=a94;
  T.A[8*s+4]=a95;
  T.A[8*s+5]=a96;
  T.A[8*s+6]=a97;
  T.A[8*s+7]=a98;
  T.A[9*s+0]=a101;
  T.A[9*s+2]=a103;
  T.A[9*s+3]=a104;
  T.A[9*s+4]=a105;
  T.A[9*s+5]=a106;
  T.A[9*s+6]=a107;
  T.A[9*s+7]=a108;
  for (int i=0;i<s;++i){double cs=0;for(int j=0;j<i;++j)cs+=T.A[i*s+j];T.c[i]=cs;}
  T.b[0]=b1;
  T.b[1]=0.0;
  T.b[2]=0.0;
  T.b[3]=b4;
  T.b[4]=b5;
  T.b[5]=b6;
  T.b[6]=b7;
  T.b[7]=b8;
  T.b[8]=b9;
  T.b[9]=b10;
  T.bhat[0]=d1;
  T.bhat[1]=0.0;
  T.bhat[2]=0.0;
  T.bhat[3]=d4;
  T.bhat[4]=d5;
  T.bhat[5]=d6;
  T.bhat[6]=d7;
  T.bhat[7]=d8;
  T.bhat[8]=d9;
  T.bhat[9]=d10;
}


static rksTableau rksGetTableau(int method) {
  rksTableau T; std::memset(&T, 0, sizeof(T));
  switch (method) {
  case 282: rksTableauTmy7(T); break; // tmy7adj
  case 300: rksTableauC108(T); break; // c108s
  case 301: rksTableauB109(T); break; // b109s
  case 302: rksTableauS1110a(T); break; // s1110as
  case 304: rksTableauO129(T); break; // o129s
  case 267: rksTableauF45(T); break; // f45s
  case 268: rksTableauT54(T); break; // t54s
  case 270: rksTableauPp54(T); break; // pp54s
  case 271: rksTableauPp54b(T); break; // pp54bs
  case 272: rksTableauBs54(T); break; // bs54s
  case 273: rksTableauSs54(T); break; // ss54s
  case 274: rksTableauDp65(T); break; // dp65s
  case 275: rksTableauC65(T); break; // c65s
  case 276: rksTableauTp64(T); break; // tp64s
  case 277: rksTableauV65r(T); break; // v65rs
  case 279: rksTableauDverk65(T); break; // dverk65s
  case 280: rksTableauTf65(T); break; // tf65s
  case 281: rksTableauTp75(T); break; // tp75s
  case 283: rksTableauTmy7s(T); break; // tmy7ss
  case 284: rksTableauV76r(T); break; // v76rs
  case 285: rksTableauSs76(T); break; // ss76s
  case 286: rksTableauV78(T); break; // v78s
  case 287: rksTableauDverk78(T); break; // dverk78s
  case 288: rksTableauDp85(T); break; // dp85s
  case 289: rksTableauTp86(T); break; // tp86s
  case 290: rksTableauV87e(T); break; // v87es
  case 291: rksTableauV87r(T); break; // v87rs
  case 292: rksTableauEv87(T); break; // ev87s
  case 293: rksTableauK87(T); break; // k87s
  case 295: rksTableauV89(T); break; // v89s
  case 296: rksTableauT98a(T); break; // t98as
  case 297: rksTableauV98r(T); break; // v98rs
  case 298: rksTableauS98(T); break; // s98s
  case 226: rksTableauDop5(T); break;  // dop54s/dp54s -- DoPri 5(4), same tableau as dop5s
  case 230: rksTableauVern98(T); break;// vern98s -- adaptive Verner 9(8)
  case 206: rksTableauRk4(T); break;   // rk4s -- classical RK4
  case 213: rksTableauRos4(T); break;  // ros4s -- Shampine ROS4 (stiff)
  case 236: rksTableauRadau5(T); break;// radauiia5s -- Radau IIA 3-stage 5th (stiff)
  case 233: rksTableauBackwardEuler(T); break; // backwardEulers -- implicit Euler (stiff)
  case 234: rksTableauGauss6(T); break;// gauss6s -- Gauss-Legendre 3-stage 6th (stiff)
  case 238: rksTableauSdirk43(T); break;// sdirk43s -- SDIRK 5-stage order 3 (stiff)
  case 235: rksTableauLobattoIIIC6(T); break;// iiic6s -- Lobatto IIIC 4-stage 6th (stiff)
  case 231: rksTableauGrk4a(T); break;   // ros43s -- GRK4A Rosenbrock 4-stage 4th (stiff)
  case 232: rksTableauRos6(T); break;    // ros6s  -- ROW6A Rosenbrock 6-stage 6th (stiff)
  case 237: rksTableauGeng5(T); break;   // geng5s -- Geng5 fully-implicit 3-stage 5th (stiff)
  case 239:                            // eulers -- forward Euler
    T.s = 1; T.c[0] = 0; T.b[0] = 1.0; break;
  case 240:                            // midpoints -- explicit midpoint (RK2)
    T.s = 2; T.c[0] = 0; T.c[1] = 0.5; T.b[0] = 0; T.b[1] = 1.0; T.A[1*2+0] = 0.5; break;
  case 241:                            // heuns -- Heun / explicit trapezoid (RK2)
    T.s = 2; T.c[0] = 0; T.c[1] = 1.0; T.b[0] = 0.5; T.b[1] = 0.5; T.A[1*2+0] = 1.0; break;
  case 243:                            // rk3s -- Kutta's 3rd-order method (RK3)
    T.s = 3; T.c[0] = 0; T.c[1] = 0.5; T.c[2] = 1.0;
    T.b[0] = 1.0/6; T.b[1] = 2.0/3; T.b[2] = 1.0/6;
    T.A[1*3+0] = 0.5; T.A[2*3+0] = -1.0; T.A[2*3+1] = 2.0; break;
  case 210: rksTableauDop5(T); break;  // dop5s -- adaptive Dormand-Prince 5(4)
  case 225: rksTableauRk43(T); break;  // rk43s -- adaptive Runge-Kutta 4(3)
  case 200: rksTableauDop853(T); break;// dop853s -- adaptive Dormand-Prince 8(5,3)
  case 207: rksTableauCk54(T); break;  // ck54s -- adaptive Cash-Karp 5(4)
  case 265: rksTableauBs32(T); break;  // bs32s -- adaptive Bogacki-Shampine 3(2)
  case 227: rksTableauVern65(T); break;// vern65s -- adaptive Verner 6(5)
  case 228: rksTableauVern76(T); break;// vern76s -- adaptive Verner 7(6)
  case 229: rksTableauDop87(T); break; // dop87s -- adaptive Prince-Dormand 8(7)
  case 205: rksTableauF78(T); break;   // f78s -- adaptive Fehlberg 7(8)
  default:  rksTableauRk4(T); break;
  }
  return T;
}

struct rk4s_rec {
  int s = 0;                 // stages per step (homogeneous methods); composite uses per-step
  int nq = 0;                // base states recorded per stage
  std::vector<double> h;     // realized dt per step
  std::vector<double> t0;    // step start time per step
  std::vector<double> a;     // stage states, flattened; stage i of step n at
                             // &a[(n*s + i)*nq] (homogeneous) or &a[aOff[n] + i*nq] (composite)
  std::vector<double> J;     // Rosenbrock only: frozen df/dy per step (nq*nq)
  std::vector<double> k;     // Rosenbrock only: forward stage vectors k_i (s*nq)
  // -- composite (AutoSwitch) only: per-step method + offsets (variable s per step) --
  int composite = 0;
  std::vector<int> method;   // per-step method code (rksGetTableau input)
  std::vector<size_t> aOff, JOff, kOff;  // per-step offsets into a/J/k (SIZE_MAX if none)
  void clear() { h.clear(); t0.clear(); a.clear(); J.clear(); k.clear(); method.clear(); aOff.clear(); JOff.clear(); kOff.clear(); }
  size_t nStep() const { return h.size(); }
};

// DDE history: record one accepted step [t0, t0+dt] as a cubic-Hermite dense
// segment (from the step endpoints y0,y1 and their derivatives) so later delay()
// lookups can interpolate it.  Reuses the ros4 type-1 (4-sample cubic) reader in
// par_solve.cpp; the 2 extra dydt() evaluations are only paid for delay models.
static inline void rk4s_hist_push(rx_solving_options_ind *ind, rx_solving_options *op,
                                  t_dydt dydt, int *neq, int nAll, double t0, double dt,
                                  const double *y0, const double *y1) {
  if (!ind->delayHistOn) return;
  static thread_local std::vector<double> hbuf;
  if ((int)hbuf.size() < 6 * nAll) hbuf.resize(6 * nAll);
  double *f0 = hbuf.data(), *f1 = f0 + nAll;
  double *S[4] = { f1 + nAll, f1 + 2*nAll, f1 + 3*nAll, f1 + 4*nAll };
  dydt(neq, t0,      const_cast<double*>(y0), f0);
  dydt(neq, t0 + dt, const_cast<double*>(y1), f1);
  const double ss[4] = {0.0, 1.0/3.0, 2.0/3.0, 1.0};
  for (int q = 0; q < 4; ++q) {
    double s = ss[q], a2 = s*s, a3 = a2*s;
    double h00 = 2*a3 - 3*a2 + 1, h10 = a3 - 2*a2 + s, h01 = -2*a3 + 3*a2, h11 = a3 - a2;
    for (int m = 0; m < nAll; ++m)
      S[q][m] = h00*y0[m] + h10*dt*f0[m] + h01*y1[m] + h11*dt*f1[m];
  }
  rxDelayHistPushSamples(ind, op, t0, dt, S[0], S[1], S[2], S[3]);
}

// Cap |dt| so no step overshoots the smallest delay (keeps delay() lookups inside
// already-recorded history) or the user's HMAX.  No-op when neither applies.
static inline double rk4s_hist_cap(rx_solving_options_ind *ind, double dt, int sign) {
  if (ind->delayHistOn && R_FINITE(ind->delayMinT) && fabs(dt) > ind->delayMinT)
    dt = sign * ind->delayMinT;
  if (R_FINITE(ind->HMAX) && ind->HMAX > 0.0 && fabs(dt) > ind->HMAX)
    dt = sign * ind->HMAX;
  return dt;
}

// One explicit-RK step over [t, t+dt] for the tableau `T`.  dydt READS/WRITES
// the FULL state (all `nAll` = neqOde entries), so the stage/derivative buffers
// MUST be nAll wide; we only RECORD the first `nRec` (base) states of each stage.
// In adjoint mode the rx__sens_* compartments have d/dt=0, so advancing them is
// a no-op that keeps them at 0 -- exactly what the backward sweep expects.
// scratch layout: k[0..s-1] (each nAll) then atmp (nAll) -> (s+1)*nAll.
static inline void rk4s_step_record(t_dydt dydt, int *neq, const rksTableau &T,
                                    int nAll, int nRec, double t, double dt, double *y,
                                    double *scratch, rk4s_rec *rec) {
  int s = T.s;
  double *atmp = scratch + s * nAll;
  for (int i = 0; i < s; ++i) {
    // a_i = y + dt * sum_{j<i} A[i][j] * k_j
    for (int m = 0; m < nAll; ++m) atmp[m] = y[m];
    for (int j = 0; j < i; ++j) {
      double aij = T.A[i * s + j];
      if (aij != 0.0) { const double *kj = scratch + j * nAll; for (int m = 0; m < nAll; ++m) atmp[m] += dt * aij * kj[m]; }
    }
    if (rec) rec->a.insert(rec->a.end(), atmp, atmp + nRec);
    dydt(neq, t + T.c[i] * dt, atmp, scratch + i * nAll);
  }
  // y_next = y + dt * sum_i b_i k_i
  for (int i = 0; i < s; ++i) {
    double bi = T.b[i];
    if (bi != 0.0) { const double *ki = scratch + i * nAll; for (int m = 0; m < nAll; ++m) y[m] += dt * bi * ki[m]; }
  }
  if (rec) { rec->t0.push_back(t); rec->h.push_back(dt); }
}

// Stage-recording analogue of rk4_do_steps (src/rk4.cpp): same step-size logic;
// advances all `nAll` states, records the first `nRec` (base) states per stage.
static inline void rk4s_do_steps(rx_solving_options_ind *ind, rx_solving_options *op,
                                 t_dydt dydt, int *neq, const rksTableau &T,
                                 int nAll, int nRec, double *yp,
                                 double xp, double xout, rk4s_rec *rec,
                                 std::vector<double> &scratch) {
  double t = xp;
  double dt = op->HMIN > 0.0 ? op->HMIN : 0.01;
  if (dt <= 0.0) dt = 0.01;
  if (fabs(xout - xp) / dt >= op->mxstep) {
    dt = fabs(xout - xp) / (double)(op->mxstep - 10);
  }
  int sign = (xout > xp) ? 1 : -1;
  dt = sign * dt;
  dt = rk4s_hist_cap(ind, dt, sign);   // DDE: keep steps within delayMinT

  // dydt writes nAll derivatives into each k buffer -> (s k-buffers + atmp).
  if ((int)scratch.size() < (T.s + 1) * nAll) scratch.resize((T.s + 1) * nAll);
  std::vector<double> y0h; if (ind->delayHistOn) y0h.resize(nAll);

  error_checker check(ind, ind->rc, op->mxstep);
  zero_copy_state chk(yp, nAll);

  while ((sign > 0 && t < xout) || (sign < 0 && t > xout)) {
    double current_dt = dt;
    if ((sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout)) {
      current_dt = xout - t;
    }
    if (ind->delayHistOn) for (int m = 0; m < nAll; ++m) y0h[m] = yp[m];
    try {
      rk4s_step_record(dydt, neq, T, nAll, nRec, t, current_dt, yp, scratch.data(), rec);
    } catch (const std::exception &e) {
      if (ind->rc[0] == 0) ind->rc[0] = -2019;
      ind->err = 1;
      break;
    }
    rk4s_hist_push(ind, op, dydt, neq, nAll, t, current_dt, y0h.data(), yp);
    t += current_dt;
    check(chk, t);
    if (ind->err != 0) break;
  }
}

// One trial step of an adaptive embedded pair: compute all stages a_i (into
// `stages`) and k_i (into `kbuf`), the high-order (b) and low-order (bhat)
// solutions, and return the scaled error norm.  Does NOT advance y.
static inline double rks_try_step(t_dydt dydt, int *neq, const rksTableau &T, int nAll,
                                  double t, double dt, const double *y, double *stages,
                                  double *kbuf, double *yhigh, double *ylow,
                                  double atol, double rtol) {
  int s = T.s;
  for (int i = 0; i < s; ++i) {
    double *ai = stages + i * nAll;
    for (int m = 0; m < nAll; ++m) ai[m] = y[m];
    for (int j = 0; j < i; ++j) {
      double aij = T.A[i * s + j];
      if (aij != 0.0) { const double *kj = kbuf + j * nAll; for (int m = 0; m < nAll; ++m) ai[m] += dt * aij * kj[m]; }
    }
    dydt(neq, t + T.c[i] * dt, ai, kbuf + i * nAll);
  }
  for (int m = 0; m < nAll; ++m) { yhigh[m] = y[m]; ylow[m] = y[m]; }
  for (int i = 0; i < s; ++i) {
    double bi = T.b[i], bh = T.bhat[i]; const double *ki = kbuf + i * nAll;
    for (int m = 0; m < nAll; ++m) { yhigh[m] += dt * bi * ki[m]; ylow[m] += dt * bh * ki[m]; }
  }
  double err = 0.0;
  for (int m = 0; m < nAll; ++m) {
    double sc = atol + rtol * fmax(fabs(y[m]), fabs(yhigh[m]));
    double e = (yhigh[m] - ylow[m]) / sc; err += e * e;
  }
  return sqrt(err / (nAll > 0 ? nAll : 1));
}

// Adaptive embedded-RK driver: standard error-controlled step selection.  The
// realized (accepted) step sequence is RECORDED (h + stages) and FROZEN -- the
// discrete adjoint transposes that fixed sequence (step-size control is not
// differentiated), reusing the same table-driven backward transpose.
static inline void rks_do_steps_adaptive(rx_solving_options_ind *ind, rx_solving_options *op,
                                         t_dydt dydt, int *neq, const rksTableau &T,
                                         int nAll, int nRec, double *yp,
                                         double xp, double xout, rk4s_rec *rec,
                                         std::vector<double> &scratch) {
  int s = T.s;
  double atol = op->ATOL > 0 ? op->ATOL : 1e-8, rtol = op->RTOL > 0 ? op->RTOL : 1e-6;
  double t = xp; int sign = (xout > xp) ? 1 : -1;
  double span = fabs(xout - xp); if (span == 0.0) return;
  double dt = op->H0 > 0 ? op->H0 : span / 100.0; if (dt <= 0.0) dt = span / 100.0;
  dt = sign * dt;
  if ((int)scratch.size() < (2 * s + 2) * nAll) scratch.resize((2 * s + 2) * nAll);
  double *stages = scratch.data(), *kbuf = stages + s * nAll,
         *yhigh = kbuf + s * nAll, *ylow = yhigh + nAll;
  error_checker check(ind, ind->rc, op->mxstep);
  zero_copy_state chk(yp, nAll);
  const double SAFE = 0.9, FACMIN = 0.2, FACMAX = 5.0;
  int expo = T.errOrder + 1, nrej = 0;
  dt = rk4s_hist_cap(ind, dt, sign);   // DDE: keep steps within delayMinT
  while ((sign > 0 && t < xout) || (sign < 0 && t > xout)) {
    if ((sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout)) dt = xout - t;
    dt = rk4s_hist_cap(ind, dt, sign);
    double err;
    try { err = rks_try_step(dydt, neq, T, nAll, t, dt, yp, stages, kbuf, yhigh, ylow, atol, rtol); }
    catch (const std::exception &e) { if (ind->rc[0] == 0) ind->rc[0] = -2019; ind->err = 1; break; }
    double fac = (err > 0.0) ? SAFE * pow(err, -1.0 / expo) : FACMAX;
    if (fac < FACMIN) fac = FACMIN; if (fac > FACMAX) fac = FACMAX;
    if (err <= 1.0 || fabs(dt) <= 1e-13 * span) {
      for (int i = 0; i < s; ++i) rec->a.insert(rec->a.end(), stages + i * nAll, stages + i * nAll + nRec);
      rec->t0.push_back(t); rec->h.push_back(dt);
      rk4s_hist_push(ind, op, dydt, neq, nAll, t, dt, yp, yhigh);
      for (int m = 0; m < nAll; ++m) yp[m] = yhigh[m];
      t += dt; check(chk, t); if (ind->err != 0) break;
      dt *= fac; dt = rk4s_hist_cap(ind, dt, sign); nrej = 0;
    } else {
      dt *= fac;
      if (++nrej > 50) { ind->err = 1; if (ind->rc[0] == 0) ind->rc[0] = -2019; break; }
    }
  }
}

// Dispatch a solve interval to the fixed-step or adaptive driver by the tableau.
// One fixed-step Rosenbrock step over [t, t+dt].  J = df/dy at step start (from
// the rx__adjFX_* lhs via calc_lhs) is frozen; W = I/(dt*gamma) - J is factored
// once and reused for all stages.  Advances the nBase base states, records the
// stage states (nRec) + the frozen J for the backward transpose.
static inline void ros_step_record(t_dydt dydt, int *neq, const rksTableau &T,
                                   int cSub, int fxOff, int nAll, int nBase, int nRec,
                                   double t, double dt, double *y, rx_solving_options_ind *ind,
                                   double *scratch, int *piv, double *W, rk4s_rec *rec) {
  int s = T.s;
  double *kbuf = scratch, *u = kbuf + s * nBase, *fbuf = u + nAll, *rhs = fbuf + nAll;
  // J = F_X(y) via calc_lhs; record it; W = I/(dt*gamma) - J; factor.
  calc_lhs(cSub, t, y, ind->lhs);
  if (rec) rec->J.insert(rec->J.end(), &ind->lhs[fxOff], &ind->lhs[fxOff] + nBase*nBase);
  double invhg = 1.0 / (dt * T.gamma);
  for (int i = 0; i < nBase; ++i)
    for (int j = 0; j < nBase; ++j) W[i*nBase+j] = (i==j ? invhg : 0.0) - ind->lhs[fxOff + i*nBase + j];
  if (!luFactor(W, nBase, piv)) { ind->err = 1; if (ind->rc[0]==0) ind->rc[0] = -2019; return; }
  for (int i = 0; i < s; ++i) {
    for (int m = 0; m < nAll; ++m) u[m] = y[m];
    for (int j = 0; j < i; ++j) { double aij = T.A[i*s+j]; if (aij != 0.0) { const double *kj = kbuf + j*nBase; for (int m = 0; m < nBase; ++m) u[m] += aij * kj[m]; } }
    if (rec) rec->a.insert(rec->a.end(), u, u + nRec);
    dydt(neq, t, u, fbuf);
    for (int m = 0; m < nBase; ++m) rhs[m] = fbuf[m];
    for (int j = 0; j < i; ++j) { double g = T.gam[i*s+j]; if (g != 0.0) { const double *kj = kbuf + j*nBase; for (int m = 0; m < nBase; ++m) rhs[m] += (1.0/dt) * g * kj[m]; } }
    luSolve(W, nBase, piv, rhs);
    double *ki = kbuf + i*nBase; for (int m = 0; m < nBase; ++m) ki[m] = rhs[m];
    if (rec) rec->k.insert(rec->k.end(), ki, ki + nBase);
  }
  for (int i = 0; i < s; ++i) { double mi = T.b[i]; const double *ki = kbuf + i*nBase; for (int m = 0; m < nBase; ++m) y[m] += mi * ki[m]; }
  if (rec) { rec->t0.push_back(t); rec->h.push_back(dt); }
}

static inline void ros_do_steps(rx_solving_options_ind *ind, rx_solving_options *op,
                                t_dydt dydt, int *neq, const rksTableau &T,
                                int nAll, int nRec, double *yp, double xp, double xout,
                                rk4s_rec *rec, std::vector<double> &scratch) {
  int nBase = op->adjNbase, fxOff = op->adjFxOff, cSub = neq[1], s = T.s;
  double t = xp;
  double dt = op->HMIN > 0.0 ? op->HMIN : 0.01; if (dt <= 0.0) dt = 0.01;
  if (fabs(xout - xp) / dt >= op->mxstep) dt = fabs(xout - xp) / (double)(op->mxstep - 10);
  int sign = (xout > xp) ? 1 : -1; dt = sign * dt;
  dt = rk4s_hist_cap(ind, dt, sign);   // DDE: keep steps within delayMinT
  if ((int)scratch.size() < s*nBase + 2*nAll + nBase) scratch.resize(s*nBase + 2*nAll + nBase);
  std::vector<double> W(nBase*nBase); std::vector<int> piv(nBase);
  std::vector<double> y0h; if (ind->delayHistOn) y0h.resize(nAll);
  error_checker check(ind, ind->rc, op->mxstep); zero_copy_state chk(yp, nAll);
  while ((sign > 0 && t < xout) || (sign < 0 && t > xout)) {
    double cdt = dt; if ((sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout)) cdt = xout - t;
    if (ind->delayHistOn) for (int m = 0; m < nAll; ++m) y0h[m] = yp[m];
    ros_step_record(dydt, neq, T, cSub, fxOff, nAll, nBase, nRec, t, cdt, yp, ind, scratch.data(), piv.data(), W.data(), rec);
    if (ind->err != 0) break;
    rk4s_hist_push(ind, op, dydt, neq, nAll, t, cdt, y0h.data(), yp);
    t += cdt; check(chk, t); if (ind->err != 0) break;
  }
}

// Fully-implicit RK (Radau IIA) forward step.  Newton-solves the coupled stage
// system G_i = k_i - f(u_i) = 0, u_i = y + h sum_j A_ij k_j, with the exact
// Jacobian M_{ij} = delta_ij I - h A_ij J(u_i) (an (s*n)x(s*n) linear solve per
// Newton iteration).  Records the converged stage states u_i for the backward.
static inline void radau_step_record(t_dydt dydt, int *neq, const rksTableau &T,
                                     int cSub, int fxOff, int nAll, int nBase, int nRec,
                                     double t, double dt, double *y, rx_solving_options_ind *ind,
                                     rk4s_rec *rec) {
  int s = T.s, n = nBase, sn = s*n;
  std::vector<double> K(sn, 0.0), G(sn), u(nAll, 0.0), fbuf(nAll), M(sn*sn);
  std::vector<int> piv(sn);
  const int maxit = 50; const double tol = 1e-12;
  for (int it = 0; it < maxit; ++it) {
    for (int i = 0; i < s; ++i) {
      for (int m = 0; m < nAll; ++m) u[m] = y[m];
      for (int j = 0; j < s; ++j) { double aij = T.A[i*s+j]; if (aij != 0.0) for (int m = 0; m < n; ++m) u[m] += dt*aij*K[j*n+m]; }
      dydt(neq, t, u.data(), fbuf.data());
      calc_lhs(cSub, t, u.data(), ind->lhs);                // F_X(u_i) -> ind->lhs[fxOff..]
      for (int m = 0; m < n; ++m) G[i*n+m] = K[i*n+m] - fbuf[m];
      for (int j = 0; j < s; ++j) { double aij = T.A[i*s+j];
        for (int a = 0; a < n; ++a) for (int b = 0; b < n; ++b)
          M[(i*n+a)*sn + (j*n+b)] = ((i==j && a==b) ? 1.0 : 0.0) - dt*aij*ind->lhs[fxOff + a*n + b];
      }
    }
    if (!luFactor(M.data(), sn, piv.data())) { ind->err = 1; if (ind->rc[0]==0) ind->rc[0] = -2019; return; }
    for (int q = 0; q < sn; ++q) G[q] = -G[q];              // solve M dK = -G
    luSolve(M.data(), sn, piv.data(), G.data());            // G is now dK
    double nrm = 0; for (int q = 0; q < sn; ++q) { K[q] += G[q]; double a = fabs(G[q]); if (a > nrm) nrm = a; }
    if (nrm < tol) break;
  }
  // record converged stages u_i (recompute) + advance y_new = y + h sum b_i k_i.
  if (rec) {
    for (int i = 0; i < s; ++i) {
      for (int m = 0; m < nAll; ++m) u[m] = y[m];
      for (int j = 0; j < s; ++j) { double aij = T.A[i*s+j]; if (aij != 0.0) for (int m = 0; m < n; ++m) u[m] += dt*aij*K[j*n+m]; }
      rec->a.insert(rec->a.end(), u.begin(), u.begin() + nRec);
    }
    rec->t0.push_back(t); rec->h.push_back(dt);
  }
  for (int i = 0; i < s; ++i) { double bi = T.b[i]; for (int m = 0; m < n; ++m) y[m] += dt*bi*K[i*n+m]; }
}

static inline void radau_do_steps(rx_solving_options_ind *ind, rx_solving_options *op,
                                  t_dydt dydt, int *neq, const rksTableau &T,
                                  int nAll, int nRec, double *yp, double xp, double xout,
                                  rk4s_rec *rec, std::vector<double> &scratch) {
  int nBase = op->adjNbase, fxOff = op->adjFxOff, cSub = neq[1];
  double t = xp;
  double dt = op->HMIN > 0.0 ? op->HMIN : 0.01; if (dt <= 0.0) dt = 0.01;
  if (fabs(xout - xp) / dt >= op->mxstep) dt = fabs(xout - xp) / (double)(op->mxstep - 10);
  int sign = (xout > xp) ? 1 : -1; dt = sign * dt;
  dt = rk4s_hist_cap(ind, dt, sign);   // DDE: keep steps within delayMinT
  error_checker check(ind, ind->rc, op->mxstep); zero_copy_state chk(yp, nAll);
  std::vector<double> y0h; if (ind->delayHistOn) y0h.resize(nAll);
  (void)scratch;
  while ((sign > 0 && t < xout) || (sign < 0 && t > xout)) {
    double cdt = dt; if ((sign > 0 && t + dt > xout) || (sign < 0 && t + dt < xout)) cdt = xout - t;
    if (ind->delayHistOn) for (int m = 0; m < nAll; ++m) y0h[m] = yp[m];
    radau_step_record(dydt, neq, T, cSub, fxOff, nAll, nBase, nRec, t, cdt, yp, ind, rec);
    if (ind->err != 0) break;
    rk4s_hist_push(ind, op, dydt, neq, nAll, t, cdt, y0h.data(), yp);
    t += cdt; check(chk, t); if (ind->err != 0) break;
  }
}

static inline void rks_run_method(const rksTableau &T, rx_solving_options_ind *ind,
                                  rx_solving_options *op, t_dydt dydt, int *neq,
                                  int nAll, int nRec, double *yp, double xp, double xout,
                                  rk4s_rec *rec, std::vector<double> &scratch) {
  if (T.implicitRK)      radau_do_steps(ind, op, dydt, neq, T, nAll, nRec, yp, xp, xout, rec, scratch);
  else if (T.rosenbrock) ros_do_steps(ind, op, dydt, neq, T, nAll, nRec, yp, xp, xout, rec, scratch);
  else if (T.adaptive)   rks_do_steps_adaptive(ind, op, dydt, neq, T, nAll, nRec, yp, xp, xout, rec, scratch);
  else                   rk4s_do_steps(ind, op, dydt, neq, T, nAll, nRec, yp, xp, xout, rec, scratch);
}

// AutoSwitch stiffness heuristic for the interval [xp,xout]: Gershgorin spectral
// radius of J=F_X(y) times the interval length.  (For the adjoint the exact
// threshold only affects which method runs -- the transpose is exact for either.)
static inline bool compositeStiff(rx_solving_options_ind *ind, rx_solving_options *op,
                                  int cSub, double *y, double xp, double xout) {
  int n = op->adjNbase, fxOff = op->adjFxOff;
  calc_lhs(cSub, xp, y, ind->lhs);
  double specrad = 0;
  for (int i = 0; i < n; ++i) { double rs = 0; for (int j = 0; j < n; ++j) rs += fabs(ind->lhs[fxOff+i*n+j]); if (rs > specrad) specrad = rs; }
  return specrad * fabs(xout - xp) > 50.0;
}

static inline void rks_step_interval(rx_solving_options_ind *ind, rx_solving_options *op,
                                     t_dydt dydt, int *neq, const rksTableau &T,
                                     int nAll, int nRec, double *yp, double xp, double xout,
                                     rk4s_rec *rec, std::vector<double> &scratch) {
  if (!rec->composite) { rks_run_method(T, ind, op, dydt, neq, nAll, nRec, yp, xp, xout, rec, scratch); return; }
  // AutoSwitch composite: pick non-stiff (op->stiff) or stiff (op->stiff2) for
  // this interval, run it, and tag every step it produced with its method+offsets.
  int mcode = compositeStiff(ind, op, neq[1], yp, xp, xout) ? op->stiff2 : op->stiff;
  rksTableau Tm = rksGetTableau(mcode);
  size_t nStart = rec->nStep(), aS = rec->a.size(), JS = rec->J.size(), kS = rec->k.size();
  rks_run_method(Tm, ind, op, dydt, neq, nAll, nRec, yp, xp, xout, rec, scratch);
  int sm = Tm.s, nb = op->adjNbase;
  for (size_t nn = nStart; nn < rec->nStep(); ++nn) {
    rec->method.push_back(mcode);
    rec->aOff.push_back(aS + (nn - nStart) * (size_t)(sm * nRec));
    if (Tm.rosenbrock) { rec->JOff.push_back(JS + (nn - nStart) * (size_t)(nb*nb)); rec->kOff.push_back(kS + (nn - nStart) * (size_t)(sm*nb)); }
    else { rec->JOff.push_back((size_t)-1); rec->kOff.push_back((size_t)-1); }
  }
}

// Evaluate F_X (nBase x nBase) and F_p (nBase x np) at base-state vector `a` and
// time `t` via calc_lhs, into fx[nBase*nBase] (row-major i*nBase+j) and
// fp[nBase*np] (i*np+p).  Ascratch is a full-length (eff) state buffer.
static inline void rk4s_eval_jac(int cSub, double t, const double *a, int nBase, int np,
                                 int fxOff, int fpOff, double *Ascratch, int eff,
                                 rx_solving_options_ind *ind, double *fx, double *fp) {
  for (int i = 0; i < eff; ++i) Ascratch[i] = 0.0;
  for (int i = 0; i < nBase; ++i) Ascratch[i] = a[i];
  calc_lhs(cSub, t, Ascratch, ind->lhs);
  for (int i = 0; i < nBase * nBase; ++i) fx[i] = ind->lhs[fxOff + i];
  for (int i = 0; i < nBase * np; ++i)  fp[i] = ind->lhs[fpOff + i];
}

// The shared explicit-RK backward transpose (Phase 1 DRY seed): fills the
// rx__sens_* output slots for every observation.  Precomputes each step's 4
// stage Jacobians ONCE (calc_lhs), then runs an independent reset sweep per
// (observation, base-state) pair.
// Rosenbrock backward transpose (frozen-J, so EXACT for linear f; for nonlinear
// f it drops the d(J)/dy = f'' term).  Reverse of one Rosenbrock step under W
// (frozen) and the exact stage Jacobians J(u_i)=F_X(u_i)/F_p(u_i): rhsbar =
// W^{-T} kbar_i; ubar = J(u_i)^T rhsbar; mu += F_p(u_i)^T rhsbar; couple back
// through gam_ij/h (into rhs) and alpha_ij (into u).  Full-trajectory reset
// sweeps identical to the explicit case.
// DDE anticipating-term helper, shared by every backward fill.  It precomputes
// the delayed Jacobian F_Xd and delay durations tau at each step start, then per
// reset sweep tracks the costate lam at step boundaries and, after each step's
// standard transpose, injects the anticipating contribution
//   lam_j(t_n) += h * F_Xd_ij(t_n+tau) * lam_i(t_n+tau).
// The step cap (h <= delayMinT <= tau) guarantees t_n+tau lands on an
// already-swept later boundary, so lam there is available (binary search + linear
// interpolation).  Inactive (no-op) unless the model has delay() (adjFxdOff>=0).
struct rk4s_dde {
  bool active = false, hasDtau = false;
  int nBase = 0, nfx = 0, np = 0;
  const rk4s_rec *rec = nullptr;
  std::vector<double> FXd, TAU, DTAU, lamStore;   // DTAU: per step (i*nBase+j)*np+p
  double tObs = 0.0; size_t fromStep = 0;
  double btime(size_t m) const {
    size_t nStep = rec->nStep();
    return (m < nStep) ? rec->t0[m] : (rec->t0[nStep-1] + rec->h[nStep-1]);
  }
  // Interpolate lam_istate at time tq >= boundary (n+1) from the stored costate;
  // returns via *mOut the bracketing step (for reading step-local F_Xd/dtau).
  double interpLamI(double tq, int istate, size_t after, size_t *mOut) const {
    size_t lo = after, hi = fromStep, m = after;
    while (lo <= hi) { size_t mid = (lo+hi)/2; if (btime(mid) <= tq) { m = mid; lo = mid+1; } else { if (mid == 0) break; hi = mid-1; } }
    *mOut = (m < fromStep) ? m : (fromStep > 0 ? fromStep - 1 : 0);
    double bm = btime(m), bm1 = btime(m+1 <= fromStep ? m+1 : fromStep);
    if (m >= fromStep || bm1 <= bm) return lamStore[fromStep*nBase+istate];
    double w = (tq-bm)/(bm1-bm);
    return (1.0-w)*lamStore[m*nBase+istate] + w*lamStore[(m+1)*nBase+istate];
  }
  // stageStart(n) -> pointer to the nBase base states recorded at step n's start.
  template<class F>
  void init(rx_solving_options *op, rx_solving_options_ind *ind, int cSub,
            const rk4s_rec &r, int nb, int np_, int eff, std::vector<double> &Ascratch, F stageStart) {
    int fxdOff = op->adjFxdOff, tauOff = op->adjTauOff, dtauOff = op->adjDtauOff;
    active = (fxdOff >= 0 && tauOff >= 0);
    if (!active) return;
    rec = &r; nBase = nb; nfx = nb*nb; np = np_; hasDtau = (dtauOff >= 0);
    size_t nStep = r.nStep(); int ndt = nfx*np;
    FXd.resize(nStep*nfx); TAU.resize(nStep*nfx); if (hasDtau) DTAU.resize(nStep*ndt);
    for (size_t n = 0; n < nStep; ++n) {
      const double *a0 = stageStart(n);
      for (int q = 0; q < eff; ++q) Ascratch[q] = 0.0;
      for (int q = 0; q < nb; ++q) Ascratch[q] = a0[q];
      calc_lhs(cSub, r.t0[n], Ascratch.data(), ind->lhs);
      for (int q = 0; q < nfx; ++q) { FXd[n*nfx+q] = ind->lhs[fxdOff+q]; TAU[n*nfx+q] = ind->lhs[tauOff+q]; }
      if (hasDtau) for (int q = 0; q < ndt; ++q) DTAU[n*ndt+q] = ind->lhs[dtauOff+q];
    }
  }
  void beginSweep(size_t from, const std::vector<double> &lam) {
    if (!active) return;
    fromStep = from; tObs = (from > 0) ? btime(from) : 0.0;
    lamStore.assign((from+1)*nBase, 0.0);
    for (int j = 0; j < nBase; ++j) lamStore[from*nBase+j] = lam[j];
  }
  void applyStep(size_t n, std::vector<double> &lam) {
    if (!active) return;
    double h = rec->h[n], t0n = rec->t0[n];
    for (int istate = 0; istate < nBase; ++istate)
      for (int jstate = 0; jstate < nBase; ++jstate) {
        double tau = TAU[n*nfx + istate*nBase + jstate];
        if (tau <= 0.0) continue;
        double tq = t0n + tau;
        if (tq > tObs + 1e-12) continue;         // lam_i beyond the obs time is 0
        size_t m; double lami = interpLamI(tq, istate, n+1, &m);
        lam[jstate] += h * FXd[m*nfx + istate*nBase + jstate] * lami;
      }
    for (int j = 0; j < nBase; ++j) lamStore[n*nBase+j] = lam[j];
  }
  // Dose-induced breaking-point jump (call after the sweep, lamStore complete for
  // this obs,k).  A dose into state j jumps delay(y_j,tau) at t_dose+tau; the
  // 1st-order tau-sensitivity jump  [S_i] = -F_Xd_ij*[y_j]*dtau/dp  contributes
  // mu[p] += lam_i(t_dose+tau) * [S_i]  (the anticipating term already propagates
  // this to 2tau, 3tau, ...).
  void applyDoseJumps(std::vector<double> &mu, const std::vector<rk4s_dose> &doses) {
    if (!active || !hasDtau || doses.empty()) return;
    int ndt = nfx*np;
    for (size_t di = 0; di < doses.size(); ++di) {
      if (doses[di].type != 0) continue;   // DDE dose-jump is additive-bolus only
      int jstate = doses[di].cmt; double a = doses[di].amt;
      double tDose = btime(doses[di].step);
      size_t sd = (doses[di].step < rec->nStep()) ? doses[di].step : (rec->nStep()-1);
      for (int istate = 0; istate < nBase; ++istate) {
        double tau = TAU[sd*nfx + istate*nBase + jstate];
        if (tau <= 0.0) continue;                       // no delay(y_j) in f_istate
        double xi = tDose + tau;
        if (xi > tObs + 1e-12 || xi <= tDose) continue; // beyond obs, or nothing to propagate
        size_t m; double lami = interpLamI(xi, istate, doses[di].step + 1, &m);
        double fxd = FXd[m*nfx + istate*nBase + jstate];
        const double *dt = &DTAU[m*ndt + (istate*nBase+jstate)*np];
        for (int p = 0; p < np; ++p) mu[p] += -lami * fxd * a * dt[p];
      }
    }
  }
};

// Build the modeled-rate infusion dual maps (shared by all fills): the dR/dtheta
// lhs block (returned, or NULL) plus per-step forcingCmt[n] (>=0 inside an infusion
// window) and offCmt[n]/offFac[n] (the off-boundary step and its -amt/R factor).
static const double *rk4sBuildInfusMaps(rx_solving_options *op, rx_solving_options_ind *ind,
    int cSub, rk4s_rec &rec, const std::vector<rk4s_infus> &infus, int nBase, int np,
    size_t nStep, std::vector<double> &Ascratch, std::vector<double> &dRateV,
    std::vector<int> &forcingCmt, std::vector<double> &forcingMult,
    std::vector<int> &offCmt, std::vector<double> &offFac) {
  if (op->adjDrateOff < 0 || infus.empty()) return NULL;
  calc_lhs(cSub, rec.t0.empty() ? 0.0 : rec.t0[0], Ascratch.data(), ind->lhs);
  dRateV.resize(nBase * np);
  for (int i = 0; i < nBase * np; ++i) dRateV[i] = ind->lhs[op->adjDrateOff + i];
  forcingCmt.assign(nStep, -1); forcingMult.assign(nStep, 0.0);
  offCmt.assign(nStep, -1); offFac.assign(nStep, 0.0);
  // durMult folds in the rate/dur runtime factor (1 for rate, amt for dur), since
  // dR/dtheta = durMult * (the emitted rx__adjDrate block).
  for (size_t w = 0; w < infus.size(); ++w) {
    const rk4s_infus &F = infus[w];
    size_t off = (F.offStep == (size_t)-1) ? nStep : F.offStep;
    for (size_t _n = F.onStep; _n < off && _n < nStep; ++_n) { forcingCmt[_n] = F.cmt; forcingMult[_n] = F.durMult; }
    if (F.offStep != (size_t)-1 && F.offStep < nStep && F.R != 0.0) {
      offCmt[F.offStep] = F.cmt; offFac[F.offStep] = -(F.amt / F.R) * F.durMult;
    }
  }
  return dRateV.data();
}

static void ros_backward_fill(rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind,
                              int cSub, rk4s_rec &rec, const rksTableau &T, int nBase, int np,
                              int fxOff, int fpOff, int dfOff, int sensOff,
                              const std::vector<size_t> &boundary,
                              const std::vector<rk4s_dose> &doses,
                              const std::vector<rk4s_infus> &infus,
                              const std::vector<size_t> &boundaryDose) {
  size_t nStep = rec.nStep(); if (nStep == 0) return;
  int eff = rxEffNeq(ind, op); int s = T.s; int nfx = nBase*nBase, nfp = nBase*np;
  // precompute per step: factored W (+piv); per stage: F_X(u_i), F_p(u_i).
  std::vector<double> Wf(nStep*nfx); std::vector<int> pivf(nStep*nBase);
  std::vector<double> FX(nStep*s*nfx), FP(nStep*s*nfp);
  std::vector<double> Ascratch(eff, 0.0);
  // dJ/dtheta at each step's start (u_0 = y_start), for the W-depends-on-theta term.
  int jpOff = op->adjJpOff, njp = nfx*np; std::vector<double> Jp;
  if (jpOff >= 0) Jp.resize(nStep*njp);
  int jyOff = op->adjJyOff, njy = nfx*nBase; std::vector<double> Jy;   // dJ/dy = f'' (nonlinear)
  if (jyOff >= 0) Jy.resize(nStep*njy);
  for (size_t n = 0; n < nStep; ++n) {
    double h = rec.h[n], invhg = 1.0/(h*T.gamma); const double *Jn = &rec.J[n*nfx];
    double *Wn = &Wf[n*nfx]; int *pn = &pivf[n*nBase];
    for (int i = 0; i < nBase; ++i) for (int j = 0; j < nBase; ++j) Wn[i*nBase+j] = (i==j?invhg:0.0) - Jn[i*nBase+j];
    luFactor(Wn, nBase, pn);
    for (int i = 0; i < s; ++i) {
      rk4s_eval_jac(cSub, rec.t0[n], &rec.a[(n*s+i)*nBase], nBase, np, fxOff, fpOff,
                    Ascratch.data(), eff, ind, &FX[(n*s+i)*nfx], &FP[(n*s+i)*nfp]);
      if (i == 0 && jpOff >= 0) for (int q = 0; q < njp; ++q) Jp[n*njp+q] = ind->lhs[jpOff+q];
      if (i == 0 && jyOff >= 0) for (int q = 0; q < njy; ++q) Jy[n*njy+q] = ind->lhs[jyOff+q];
    }
  }
  std::vector<double> dFdp; bool haveDose = (dfOff >= 0) && !doses.empty();
  if (haveDose) { dFdp.resize(nBase*np); calc_lhs(cSub, rec.t0.empty()?0.0:rec.t0[0], Ascratch.data(), ind->lhs); for (int i = 0; i < nBase*np; ++i) dFdp[i] = ind->lhs[dfOff+i]; }
  std::vector<double> dlagV; const double *dlagP = NULL;   // modeled-alag transversality
  if (op->adjDlagOff >= 0 && !doses.empty()) { if (!haveDose) calc_lhs(cSub, rec.t0.empty()?0.0:rec.t0[0], Ascratch.data(), ind->lhs); dlagV.resize(nBase*np); for (int i = 0; i < nBase*np; ++i) dlagV[i] = ind->lhs[op->adjDlagOff+i]; dlagP = dlagV.data(); }

  std::vector<double> lam(nBase), ybar(nBase), ubar(nBase), rhsbar(nBase), kbar(s*nBase), mu(np);
  std::vector<double> dRateV, forcingMult; std::vector<int> forcingCmt, offCmt; std::vector<double> offFac;   // modeled-rate dual
  const double *dRatep = rk4sBuildInfusMaps(op, ind, cSub, rec, infus, nBase, np, rec.nStep(), Ascratch, dRateV, forcingCmt, forcingMult, offCmt, offFac);
  auto stepT = [&](size_t n, std::vector<double> &lamR, std::vector<double> &muR) {
    double h = rec.h[n]; const double *Wn = &Wf[n*nfx]; const int *pn = &pivf[n*nBase];
    for (int m = 0; m < nBase; ++m) ybar[m] = lamR[m];
    for (int i = 0; i < s; ++i) { double mi = T.b[i]; for (int m = 0; m < nBase; ++m) kbar[i*nBase+m] = mi * lamR[m]; }
    for (int i = s-1; i >= 0; --i) {
      for (int m = 0; m < nBase; ++m) rhsbar[m] = kbar[i*nBase+m];
      luSolveT(Wn, nBase, pn, rhsbar.data());
      const double *fx = &FX[(n*s+i)*nfx], *fp = &FP[(n*s+i)*nfp];
      for (int j = 0; j < nBase; ++j) { double v = 0; for (int ii = 0; ii < nBase; ++ii) v += fx[ii*nBase+j]*rhsbar[ii]; ubar[j] = v; }
      for (int p = 0; p < np; ++p) { double v = 0; for (int ii = 0; ii < nBase; ++ii) v += fp[ii*np+p]*rhsbar[ii]; muR[p] += v; }
      if (dRatep && forcingCmt[n] >= 0) { int rc = forcingCmt[n]; for (int p = 0; p < np; ++p) muR[p] += forcingMult[n]*dRatep[rc*np+p]*rhsbar[rc]; }  // infusion forcing
      // W depends on theta through J(y_start): mu += (dJ/dtheta k_i)^T rhsbar_i.
      if (jpOff >= 0) {
        const double *Jpn = &Jp[n*njp]; const double *ki = &rec.k[(n*s+i)*nBase];
        for (int p = 0; p < np; ++p) { double v = 0;
          for (int a = 0; a < nBase; ++a) { double sb = 0; for (int b = 0; b < nBase; ++b) sb += Jpn[(a*nBase+b)*np+p]*ki[b]; v += rhsbar[a]*sb; }
          muR[p] += v; }
      }
      // W depends on y_start through J: ybar += (dJ/dy k_i)^T rhsbar_i (nonlinear f).
      if (jyOff >= 0) {
        const double *Jyn = &Jy[n*njy]; const double *ki = &rec.k[(n*s+i)*nBase];
        for (int c = 0; c < nBase; ++c) { double v = 0;
          for (int a = 0; a < nBase; ++a) { double sb = 0; for (int b = 0; b < nBase; ++b) sb += Jyn[(a*nBase+b)*nBase+c]*ki[b]; v += rhsbar[a]*sb; }
          ybar[c] += v; }
      }
      for (int j = 0; j < i; ++j) { double g = T.gam[i*s+j]; if (g != 0.0) { double *kbj = &kbar[j*nBase]; for (int m = 0; m < nBase; ++m) kbj[m] += (g/h)*rhsbar[m]; } }
      for (int m = 0; m < nBase; ++m) ybar[m] += ubar[m];
      for (int j = 0; j < i; ++j) { double a = T.A[i*s+j]; if (a != 0.0) { double *kbj = &kbar[j*nBase]; for (int m = 0; m < nBase; ++m) kbj[m] += a*ubar[m]; } }
    }
    for (int m = 0; m < nBase; ++m) lamR[m] = ybar[m];
    if (dRatep && offCmt[n] >= 0) { int oc = offCmt[n]; for (int p = 0; p < np; ++p) muR[p] += offFac[n]*dRatep[oc*np+p]*lamR[oc]; }  // infusion off-boundary
    if (!doses.empty()) rk4sApplyEventJumps(n, lamR, muR, doses, dFdp, haveDose, nBase, np, &FX[n*s*nfx], dlagP);
  };
  rk4s_dde dde;   // DDE anticipating term (no-op unless the model has delay())
  dde.init(op, ind, cSub, rec, nBase, np, eff, Ascratch, [&](size_t n){ return &rec.a[(n*s)*nBase]; });
  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    size_t fromStep = boundary[i]; double *out = getSolve(i);
    for (int k = 0; k < nBase; ++k) {
      for (int j = 0; j < nBase; ++j) lam[j] = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) mu[p] = 0.0;
      // coincident state-jump at the obs's own step (see rk4s_backward_fill)
      if (!doses.empty()) rk4sApplyEventJumps(fromStep, lam, mu, doses, dFdp, haveDose, nBase, np,
                                              &FX[(fromStep < nStep ? fromStep : nStep - 1)*s*nfx], dlagP, boundaryDose[i]);
      dde.beginSweep(fromStep, lam);
      for (size_t nn = fromStep; nn >= 1; --nn) { stepT(nn - 1, lam, mu); dde.applyStep(nn - 1, lam); }
      dde.applyDoseJumps(mu, doses);
      for (int p = 0; p < np; ++p) out[sensOff + k*np + p] = mu[p];
    }
  }
}

// Fully-implicit RK (Radau IIA) backward transpose.  EXACT with first derivatives
// only (the implicit-function theorem gives the sensitivity of the converged
// stages).  Per step: rebuild M_{ij} = delta_ij I - h A_ij J(u_i), factor; the
// transpose solves M^T z = h b (x) lam (coupled (s*n)x(s*n)), then
// ybar = lam + sum_i J(u_i)^T z_i and mu += sum_i F_p(u_i)^T z_i.
static void radau_backward_fill(rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind,
                                int cSub, rk4s_rec &rec, const rksTableau &T, int nBase, int np,
                                int fxOff, int fpOff, int dfOff, int sensOff,
                                const std::vector<size_t> &boundary,
                                const std::vector<rk4s_dose> &doses,
                                const std::vector<rk4s_infus> &infus,
                                const std::vector<size_t> &boundaryDose) {
  size_t nStep = rec.nStep(); if (nStep == 0) return;
  int eff = rxEffNeq(ind, op); int s = T.s, n = nBase, sn = s*n; int nfx = n*n, nfp = n*np;
  std::vector<double> Mf(nStep*sn*sn); std::vector<int> pivf(nStep*sn);
  std::vector<double> FX(nStep*s*nfx), FP(nStep*s*nfp);
  std::vector<double> Ascratch(eff, 0.0);
  for (size_t nn = 0; nn < nStep; ++nn) {
    double h = rec.h[nn];
    for (int i = 0; i < s; ++i)
      rk4s_eval_jac(cSub, rec.t0[nn], &rec.a[(nn*s+i)*n], n, np, fxOff, fpOff, Ascratch.data(), eff, ind, &FX[(nn*s+i)*nfx], &FP[(nn*s+i)*nfp]);
    double *Mn = &Mf[nn*sn*sn];
    for (int i = 0; i < s; ++i) { const double *Ji = &FX[(nn*s+i)*nfx];
      for (int j = 0; j < s; ++j) { double aij = T.A[i*s+j];
        for (int a = 0; a < n; ++a) for (int b = 0; b < n; ++b)
          Mn[(i*n+a)*sn + (j*n+b)] = ((i==j && a==b) ? 1.0 : 0.0) - h*aij*Ji[a*n+b];
      } }
    luFactor(Mn, sn, &pivf[nn*sn]);
  }
  std::vector<double> dFdp; bool haveDose = (dfOff >= 0) && !doses.empty();
  if (haveDose) { dFdp.resize(n*np); calc_lhs(cSub, rec.t0.empty()?0.0:rec.t0[0], Ascratch.data(), ind->lhs); for (int i = 0; i < n*np; ++i) dFdp[i] = ind->lhs[dfOff+i]; }
  std::vector<double> dlagV; const double *dlagP = NULL;   // modeled-alag transversality
  if (op->adjDlagOff >= 0 && !doses.empty()) { if (!haveDose) calc_lhs(cSub, rec.t0.empty()?0.0:rec.t0[0], Ascratch.data(), ind->lhs); dlagV.resize(n*np); for (int i = 0; i < n*np; ++i) dlagV[i] = ind->lhs[op->adjDlagOff+i]; dlagP = dlagV.data(); }

  std::vector<double> lam(n), ybar(n), z(sn), mu(np);
  std::vector<double> dRateV, forcingMult; std::vector<int> forcingCmt, offCmt; std::vector<double> offFac;   // modeled-rate dual
  const double *dRatep = rk4sBuildInfusMaps(op, ind, cSub, rec, infus, n, np, rec.nStep(), Ascratch, dRateV, forcingCmt, forcingMult, offCmt, offFac);
  auto stepT = [&](size_t nn, std::vector<double> &lamR, std::vector<double> &muR) {
    double h = rec.h[nn]; const double *Mn = &Mf[nn*sn*sn]; const int *pn = &pivf[nn*sn];
    for (int i = 0; i < s; ++i) { double hb = h*T.b[i]; for (int m = 0; m < n; ++m) z[i*n+m] = hb*lamR[m]; }
    luSolveT(Mn, sn, pn, z.data());                    // M^T z = h b (x) lam
    for (int c = 0; c < n; ++c) ybar[c] = lamR[c];
    for (int i = 0; i < s; ++i) { const double *Ji = &FX[(nn*s+i)*nfx]; const double *zi = &z[i*n];
      for (int c = 0; c < n; ++c) { double v = 0; for (int a = 0; a < n; ++a) v += Ji[a*n+c]*zi[a]; ybar[c] += v; } }
    for (int i = 0; i < s; ++i) { const double *Fpi = &FP[(nn*s+i)*nfp]; const double *zi = &z[i*n];
      for (int p = 0; p < np; ++p) { double v = 0; for (int a = 0; a < n; ++a) v += Fpi[a*np+p]*zi[a]; muR[p] += v; }
      if (dRatep && forcingCmt[nn] >= 0) { int rc = forcingCmt[nn]; for (int p = 0; p < np; ++p) muR[p] += forcingMult[nn]*dRatep[rc*np+p]*zi[rc]; } }  // infusion forcing
    for (int c = 0; c < n; ++c) lamR[c] = ybar[c];
    if (dRatep && offCmt[nn] >= 0) { int oc = offCmt[nn]; for (int p = 0; p < np; ++p) muR[p] += offFac[nn]*dRatep[oc*np+p]*lamR[oc]; }  // infusion off-boundary
    if (!doses.empty()) rk4sApplyEventJumps(nn, lamR, muR, doses, dFdp, haveDose, n, np, &FX[nn*s*nfx], dlagP);
  };
  rk4s_dde dde;   // DDE anticipating term (no-op unless the model has delay())
  dde.init(op, ind, cSub, rec, n, np, eff, Ascratch, [&](size_t nn){ return &rec.a[(nn*s)*n]; });
  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    size_t fromStep = boundary[i]; double *out = getSolve(i);
    for (int k = 0; k < n; ++k) {
      for (int j = 0; j < n; ++j) lam[j] = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) mu[p] = 0.0;
      // coincident state-jump at the obs's own step (see rk4s_backward_fill)
      if (!doses.empty()) rk4sApplyEventJumps(fromStep, lam, mu, doses, dFdp, haveDose, n, np,
                                              &FX[(fromStep < nStep ? fromStep : nStep - 1)*s*nfx], dlagP, boundaryDose[i]);
      dde.beginSweep(fromStep, lam);
      for (size_t nn = fromStep; nn >= 1; --nn) { stepT(nn - 1, lam, mu); dde.applyStep(nn - 1, lam); }
      dde.applyDoseJumps(mu, doses);
      for (int p = 0; p < np; ++p) out[sensOff + k*np + p] = mu[p];
    }
  }
}

// Shared steady-state IC applicator for the composite (AutoSwitch) backward fill.
// The explicit rk4s_backward_fill carries its own inline version; this mirrors it
// so composite ss dosing gets the same monodromy / continuous / ss=2 / interior-
// ss1 IC terms.  All ss-period Jacobians are transposed with the tableau Tss used
// to RECORD the ss period (for a composite solve that is the primary explicit
// tableau, e.g. dop853 -- a stiff/implicit primary never reaches this fill).
struct rk4sSsIc {
  int nBase = 0, np = 0, sN = 0, nfx = 0, nfp = 0, maxGeo = 0;
  const rk4s_rec *ssRec = NULL, *ss2Rec = NULL; rksTableau Tss;
  size_t ssN = 0, ss2N = 0;
  std::vector<std::vector<double> > ssFXs, ssFPs, ss2FXs, ss2FPs;  // period Jacobians
  bool haveMono = false;
  std::vector<double> S2; std::vector<char> isSs2;                 // ss=2 superposition
  const std::vector<size_t> *ss2Steps = NULL, *boundarySs2 = NULL;
  std::vector<double> Sss1; std::vector<char> isSs1;               // interior ss=1 reset
  const std::vector<size_t> *ss1Steps = NULL, *boundarySs1 = NULL;
  bool haveCont = false; std::vector<double> FXc, contFP; std::vector<int> contPiv;  // continuous ss=1
  std::vector<double> Ybar, kbar, abar;                            // period-transpose scratch

  bool active() const { return haveMono || haveCont || !S2.empty() || !Sss1.empty(); }

  // transpose one recorded ss period (Jacobians FXs/FPs, N steps): muAcc += B^T nu, nu := M^T nu.
  void periodT(const rk4s_rec *R, std::vector<std::vector<double> > &FXs,
               std::vector<std::vector<double> > &FPs, size_t N,
               std::vector<double> &nu, std::vector<double> &muAcc) {
    for (size_t nn = N; nn >= 1; --nn) {
      size_t n = nn - 1; double h = R->h[n];
      for (int j = 0; j < nBase; ++j) Ybar[j] = nu[j];
      for (int i = 0; i < sN; ++i) for (int j = 0; j < nBase; ++j) kbar[i*nBase+j] = h*Tss.b[i]*nu[j];
      for (int i = sN-1; i >= 0; --i) {
        const double *fx = &FXs[i][n*nfx], *fp = &FPs[i][n*nfp]; const double *kb = &kbar[i*nBase];
        for (int j = 0; j < nBase; ++j) { double s=0; for (int q=0;q<nBase;++q) s+=fx[q*nBase+j]*kb[q]; abar[j]=s; }
        for (int p = 0; p < np; ++p) { double s=0; for (int q=0;q<nBase;++q) s+=fp[q*np+p]*kb[q]; muAcc[p]+=s; }
        for (int j = 0; j < nBase; ++j) Ybar[j]+=abar[j];
        for (int js = 0; js < i; ++js) { double aij=Tss.A[i*sN+js]; if (aij!=0.0){ double*kbj=&kbar[js*nBase]; for(int j=0;j<nBase;++j) kbj[j]+=h*aij*abar[j]; } }
      }
      for (int j = 0; j < nBase; ++j) nu[j] = Ybar[j];
    }
  }
  // materialize the fixed-point sensitivity (I-M)^{-1}B row-by-row (geometric series).
  void fillFixedPoint(const rk4s_rec *R, std::vector<std::vector<double> > &FXs,
                      std::vector<std::vector<double> > &FPs, size_t N, std::vector<double> &S) {
    S.assign((size_t)nBase*np, 0.0);
    for (int k = 0; k < nBase; ++k) {
      std::vector<double> nu(nBase, 0.0); nu[k] = 1.0; std::vector<double> muAdd(np, 0.0);
      for (int git = 0; git < maxGeo; ++git) { periodT(R, FXs, FPs, N, nu, muAdd);
        double n2=0; for (int j=0;j<nBase;++j) n2+=fabs(nu[j]); if (n2<1e-14) break; }
      for (int p = 0; p < np; ++p) S[(size_t)k*np+p] = muAdd[p];
    }
  }

  void build(int cSub, rx_solving_options_ind *ind, rx_solving_options *op, rk4s_rec &rec,
             int fxOff, int fpOff, int eff, std::vector<double> &Ascratch,
             const rk4s_rec *ssRec_, const double *ssContY, const rk4s_rec *ss2Rec_, const double *ss2ContY,
             const std::vector<size_t> *ss2Steps_, const std::vector<size_t> *boundarySs2_,
             const std::vector<size_t> *ss1Steps_, const std::vector<size_t> *boundarySs1_,
             const rksTableau &Tss_, int nBase_, int np_, size_t nStep) {
    nBase = nBase_; np = np_; Tss = Tss_; sN = Tss.s; nfx = nBase*nBase; nfp = nBase*np;
    maxGeo = op->maxSS + 5; haveMono = false; haveCont = false;
    ssRec = ssRec_; ss2Rec = ss2Rec_;
    ss2Steps = ss2Steps_; boundarySs2 = boundarySs2_; ss1Steps = ss1Steps_; boundarySs1 = boundarySs1_;
    Ybar.assign(nBase, 0.0); kbar.assign((size_t)sN*nBase, 0.0); abar.assign(nBase, 0.0);
    // ss=1 bolus / periodic-infusion monodromy period Jacobians
    ssN = (ssRec ? ssRec->nStep() : 0);
    if (ssN > 0) {
      ssFXs.resize(sN); ssFPs.resize(sN);
      for (int i = 0; i < sN; ++i) { ssFXs[i].resize(ssN*nfx); ssFPs[i].resize(ssN*nfp); }
      for (size_t n = 0; n < ssN; ++n) { double h = ssRec->h[n], t0s = ssRec->t0[n];
        for (int i = 0; i < sN; ++i)
          rk4s_eval_jac(cSub, t0s + Tss.c[i]*h, &ssRec->a[(n*sN+i)*nBase], nBase, np, fxOff, fpOff, Ascratch.data(), eff, ind, &ssFXs[i][n*nfx], &ssFPs[i][n*nfp]); }
      haveMono = true;
    }
    // continuous / full-interval ss=1: -J^{-1} df/dp at Y_ss
    if (ssContY) {
      FXc.assign(nBase*nBase, 0.0); contFP.assign(nBase*np, 0.0); contPiv.assign(nBase, 0);
      rk4s_eval_jac(cSub, rec.t0.empty()?0.0:rec.t0[0], ssContY, nBase, np, fxOff, fpOff, Ascratch.data(), eff, ind, FXc.data(), contFP.data());
      haveCont = luFactor(FXc.data(), nBase, contPiv.data());
    }
    // ss=2 superposition matrix S2
    if (ss2ContY && ss2Steps && !ss2Steps->empty()) {          // continuous ss=2: linear solve
      std::vector<double> J2(nBase*nBase), FP2(nBase*np); std::vector<int> piv2(nBase);
      rk4s_eval_jac(cSub, rec.t0.empty()?0.0:rec.t0[0], ss2ContY, nBase, np, fxOff, fpOff, Ascratch.data(), eff, ind, J2.data(), FP2.data());
      if (luFactor(J2.data(), nBase, piv2.data())) {
        S2.assign((size_t)nBase*np, 0.0);
        for (int k = 0; k < nBase; ++k) { std::vector<double> z(nBase,0.0); z[k]=1.0; luSolveT(J2.data(), nBase, piv2.data(), z.data());
          for (int p = 0; p < np; ++p) { double s=0; for (int j=0;j<nBase;++j) s+=z[j]*FP2[j*np+p]; S2[(size_t)k*np+p] = -s; } }
      }
    } else {
      ss2N = (ss2Rec ? ss2Rec->nStep() : 0);
      if (ss2N > 0 && ss2Steps && !ss2Steps->empty()) {         // bolus / finite-infusion ss=2: monodromy
        ss2FXs.resize(sN); ss2FPs.resize(sN);
        for (int i = 0; i < sN; ++i) { ss2FXs[i].resize(ss2N*nfx); ss2FPs[i].resize(ss2N*nfp); }
        for (size_t n = 0; n < ss2N; ++n) { double h = ss2Rec->h[n], t0s = ss2Rec->t0[n];
          for (int i = 0; i < sN; ++i)
            rk4s_eval_jac(cSub, t0s + Tss.c[i]*h, &ss2Rec->a[(n*sN+i)*nBase], nBase, np, fxOff, fpOff, Ascratch.data(), eff, ind, &ss2FXs[i][n*nfx], &ss2FPs[i][n*nfp]); }
        fillFixedPoint(ss2Rec, ss2FXs, ss2FPs, ss2N, S2);
      }
    }
    if (!S2.empty()) { isSs2.assign(nStep+1, 0); for (size_t s : *ss2Steps) if (s <= nStep) isSs2[s] = 1; }
    // interior ss=1 reset matrix Sss1 (only if there is an interior ss=1 step)
    if (ssN > 0 && ss1Steps) {
      bool anyInt = false; for (size_t s : *ss1Steps) if (s > 0 && s <= nStep) { anyInt = true; break; }
      if (anyInt) { fillFixedPoint(ssRec, ssFXs, ssFPs, ssN, Sss1);
        isSs1.assign(nStep+1, 0); for (size_t s : *ss1Steps) if (s > 0 && s <= nStep) isSs1[s] = 1; }
    }
  }

  // obs seed: coincident ss=2 / interior-ss=1 event at the observation's own step.
  void seedBoundary(size_t fromStep, int i, std::vector<double> &lam, std::vector<double> &mu) {
    if (!S2.empty() && boundarySs2)
      for (size_t z = 0; z < (*boundarySs2)[i] && z < ss2Steps->size(); ++z)
        if ((*ss2Steps)[z] == fromStep) { for (int p=0;p<np;++p){ double s=0; for(int q=0;q<nBase;++q) s+=lam[q]*S2[(size_t)q*np+p]; mu[p]+=s; } break; }
    if (!Sss1.empty() && boundarySs1)
      for (size_t z = 0; z < (*boundarySs1)[i] && z < ss1Steps->size(); ++z)
        if ((*ss1Steps)[z] == fromStep && fromStep > 0) { for (int p=0;p<np;++p){ double s=0; for(int q=0;q<nBase;++q) s+=lam[q]*Sss1[(size_t)q*np+p]; mu[p]+=s; } for(int j=0;j<nBase;++j) lam[j]=0.0; break; }
  }
  // interior sweep step: ss=2 pass-through add, interior ss=1 add-then-reset.
  void applyInterior(size_t step, std::vector<double> &lam, std::vector<double> &mu) {
    if (!S2.empty() && isSs2[step]) for (int p=0;p<np;++p){ double s=0; for(int q=0;q<nBase;++q) s+=lam[q]*S2[(size_t)q*np+p]; mu[p]+=s; }
    if (!Sss1.empty() && isSs1[step]) { for (int p=0;p<np;++p){ double s=0; for(int q=0;q<nBase;++q) s+=lam[q]*Sss1[(size_t)q*np+p]; mu[p]+=s; } for(int j=0;j<nBase;++j) lam[j]=0.0; }
  }
  // window-start IC: monodromy geometric series (bolus/periodic) OR continuous linear solve.
  void applyWindowIC(std::vector<double> &lam, std::vector<double> &mu) {
    if (haveMono) { std::vector<double> nu(lam.begin(), lam.begin()+nBase), muAdd(np, 0.0);
      for (int git = 0; git < maxGeo; ++git) { periodT(ssRec, ssFXs, ssFPs, ssN, nu, muAdd);
        double n2=0; for (int j=0;j<nBase;++j) n2+=fabs(nu[j]); if (n2<1e-14) break; }
      for (int p = 0; p < np; ++p) mu[p] += muAdd[p];
    } else if (haveCont) { std::vector<double> w(lam.begin(), lam.begin()+nBase);
      luSolveT(FXc.data(), nBase, contPiv.data(), w.data());
      for (int p = 0; p < np; ++p) { double s=0; for (int j=0;j<nBase;++j) s+=w[j]*contFP[j*np+p]; mu[p]-=s; } }
  }
};

// AutoSwitch composite backward: each step was tagged (rec.method) with the
// method that ran it; precompute each step's transpose data by its method and
// dispatch per step in the reset sweep (explicit table-driven OR Rosenbrock).
struct rksStepPre { int method, s, rosen; double h; size_t aOff;
                    std::vector<double> FX, FP, Wlu, Jp, Jy, kf; std::vector<int> piv; };
static void composite_backward_fill(rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind,
                                    int cSub, rk4s_rec &rec, int nBase, int np,
                                    int fxOff, int fpOff, int dfOff, int sensOff,
                                    const std::vector<size_t> &boundary,
                                    const std::vector<rk4s_dose> &doses,
                                    const std::vector<rk4s_infus> &infus,
                                    const std::vector<size_t> &boundaryDose,
                                    rk4sSsIc *ssic = NULL) {
  size_t nStep = rec.nStep(); if (nStep == 0) return;
  int eff = rxEffNeq(ind, op), n = nBase, nfx = n*n, nfp = n*np;
  int jpOff = op->adjJpOff, jyOff = op->adjJyOff, njp = nfx*np, njy = nfx*n;
  std::vector<double> Ascratch(eff, 0.0);
  std::vector<rksStepPre> pre(nStep);
  for (size_t nn = 0; nn < nStep; ++nn) {
    rksStepPre &P = pre[nn]; P.method = rec.method[nn]; rksTableau Tm = rksGetTableau(P.method);
    int s = Tm.s; P.s = s; P.rosen = Tm.rosenbrock; P.h = rec.h[nn]; P.aOff = rec.aOff[nn];
    P.FX.resize(s*nfx); P.FP.resize(s*nfp);
    for (int i = 0; i < s; ++i) {
      rk4s_eval_jac(cSub, rec.t0[nn], &rec.a[P.aOff + (size_t)i*n], n, np, fxOff, fpOff, Ascratch.data(), eff, ind, &P.FX[i*nfx], &P.FP[i*nfp]);
      if (i == 0 && Tm.rosenbrock && jpOff >= 0) { P.Jp.assign(&ind->lhs[jpOff], &ind->lhs[jpOff]+njp); if (jyOff>=0) P.Jy.assign(&ind->lhs[jyOff], &ind->lhs[jyOff]+njy); }
    }
    if (Tm.rosenbrock) {
      double invhg = 1.0/(P.h*Tm.gamma); const double *Jn = &rec.J[rec.JOff[nn]];
      P.Wlu.resize(nfx); P.piv.resize(n);
      for (int a = 0; a < n; ++a) for (int b = 0; b < n; ++b) P.Wlu[a*n+b] = (a==b?invhg:0.0) - Jn[a*n+b];
      luFactor(P.Wlu.data(), n, P.piv.data());
      P.kf.assign(&rec.k[rec.kOff[nn]], &rec.k[rec.kOff[nn]] + (size_t)s*n);
    }
  }
  std::vector<double> dFdp; bool haveDose = (dfOff >= 0) && !doses.empty();
  if (haveDose) { dFdp.resize(n*np); calc_lhs(cSub, rec.t0.empty()?0.0:rec.t0[0], Ascratch.data(), ind->lhs); for (int i = 0; i < n*np; ++i) dFdp[i] = ind->lhs[dfOff+i]; }
  std::vector<double> dlagV; const double *dlagP = NULL;   // modeled-alag transversality
  if (op->adjDlagOff >= 0 && !doses.empty()) { if (!haveDose) calc_lhs(cSub, rec.t0.empty()?0.0:rec.t0[0], Ascratch.data(), ind->lhs); dlagV.resize(n*np); for (int i = 0; i < n*np; ++i) dlagV[i] = ind->lhs[op->adjDlagOff+i]; dlagP = dlagV.data(); }

  std::vector<double> lam(n), ybar(n), ubar(n), rhsbar(n), kbar(16*n), mu(np);
  std::vector<double> dRateV, forcingMult; std::vector<int> forcingCmt, offCmt; std::vector<double> offFac;   // modeled-rate dual
  const double *dRatep = rk4sBuildInfusMaps(op, ind, cSub, rec, infus, n, np, rec.nStep(), Ascratch, dRateV, forcingCmt, forcingMult, offCmt, offFac);
  auto stepT = [&](size_t nn, std::vector<double> &lamR, std::vector<double> &muR) {
    rksStepPre &P = pre[nn]; rksTableau Tm = rksGetTableau(P.method); int s = P.s; double h = P.h;
    for (int c = 0; c < n; ++c) ybar[c] = lamR[c];
    // explicit: kbar_i = h*b_i*lam;  Rosenbrock: kbar_i = m_i*lam (h absorbed in k_i via W solve).
    for (int i = 0; i < s; ++i) { double bi = P.rosen ? Tm.b[i] : h*Tm.b[i]; for (int m = 0; m < n; ++m) kbar[i*n+m] = bi*lamR[m]; }
    for (int i = s-1; i >= 0; --i) {
      const double *fx = &P.FX[i*nfx], *fp = &P.FP[i*nfp];
      if (P.rosen) {                                  // Rosenbrock single-step transpose
        for (int m = 0; m < n; ++m) rhsbar[m] = kbar[i*n+m];
        luSolveT(P.Wlu.data(), n, P.piv.data(), rhsbar.data());
        for (int j = 0; j < n; ++j) { double v=0; for (int a=0;a<n;++a) v += fx[a*n+j]*rhsbar[a]; ubar[j]=v; }
        for (int p = 0; p < np; ++p) { double v=0; for (int a=0;a<n;++a) v += fp[a*np+p]*rhsbar[a]; muR[p]+=v; }
        if (jpOff >= 0) { const double *ki=&P.kf[i*n]; for (int p=0;p<np;++p){ double v=0; for (int a=0;a<n;++a){ double sb=0; for (int b=0;b<n;++b) sb+=P.Jp[(a*n+b)*np+p]*ki[b]; v+=rhsbar[a]*sb;} muR[p]+=v; } }
        if (jyOff >= 0) { const double *ki=&P.kf[i*n]; for (int c=0;c<n;++c){ double v=0; for (int a=0;a<n;++a){ double sb=0; for (int b=0;b<n;++b) sb+=P.Jy[(a*n+b)*n+c]*ki[b]; v+=rhsbar[a]*sb;} ybar[c]+=v; } }
        for (int j = 0; j < i; ++j) { double g=Tm.gam[i*s+j]; if (g!=0.0){ double *kbj=&kbar[j*n]; for (int m=0;m<n;++m) kbj[m]+=(g/h)*rhsbar[m]; } }
        for (int m = 0; m < n; ++m) ybar[m] += ubar[m];
        for (int j = 0; j < i; ++j) { double aa=Tm.A[i*s+j]; if (aa!=0.0){ double *kbj=&kbar[j*n]; for (int m=0;m<n;++m) kbj[m]+=aa*ubar[m]; } }
      } else {                                        // explicit table-driven transpose
        const double *kb = &kbar[i*n];
        for (int j = 0; j < n; ++j) { double v=0; for (int a=0;a<n;++a) v += fx[a*n+j]*kb[a]; ubar[j]=v; }
        for (int p = 0; p < np; ++p) { double v=0; for (int a=0;a<n;++a) v += fp[a*np+p]*kb[a]; muR[p]+=v; }
        if (dRatep && forcingCmt[nn] >= 0) { int rc = forcingCmt[nn]; for (int p = 0; p < np; ++p) muR[p] += forcingMult[nn]*dRatep[rc*np+p]*kb[rc]; }  // infusion forcing
        for (int m = 0; m < n; ++m) ybar[m] += ubar[m];
        for (int j = 0; j < i; ++j) { double aa=Tm.A[i*s+j]; if (aa!=0.0){ double *kbj=&kbar[j*n]; for (int m=0;m<n;++m) kbj[m]+=h*aa*ubar[m]; } }
      }
    }
    for (int c = 0; c < n; ++c) lamR[c] = ybar[c];
    if (dRatep && offCmt[nn] >= 0) { int oc = offCmt[nn]; for (int p = 0; p < np; ++p) muR[p] += offFac[nn]*dRatep[oc*np+p]*lamR[oc]; }  // infusion off-boundary
    if (!doses.empty()) rk4sApplyEventJumps(nn, lamR, muR, doses, dFdp, haveDose, n, np, P.FX.data(), dlagP);
  };
  rk4s_dde dde;   // DDE anticipating term (no-op unless the model has delay())
  dde.init(op, ind, cSub, rec, n, np, eff, Ascratch, [&](size_t nn){ return &rec.a[rec.aOff[nn]]; });
  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    size_t fromStep = boundary[i]; double *out = getSolve(i);
    for (int k = 0; k < n; ++k) {
      for (int j = 0; j < n; ++j) lam[j] = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) mu[p] = 0.0;
      // coincident state-jump at the obs's own step (see rk4s_backward_fill)
      if (!doses.empty()) rk4sApplyEventJumps(fromStep, lam, mu, doses, dFdp, haveDose, n, np,
                                              pre[fromStep < nStep ? fromStep : nStep - 1].FX.data(), dlagP, boundaryDose[i]);
      if (ssic) ssic->seedBoundary(fromStep, i, lam, mu);   // coincident ss=2 / interior-ss=1 IC
      dde.beginSweep(fromStep, lam);
      for (size_t nn = fromStep; nn >= 1; --nn) { stepT(nn - 1, lam, mu); dde.applyStep(nn - 1, lam);
        if (ssic) ssic->applyInterior(nn - 1, lam, mu); }   // interior ss=2 / ss=1 events
      dde.applyDoseJumps(mu, doses);
      if (ssic) ssic->applyWindowIC(lam, mu);               // window-start monodromy / continuous IC
      for (int p = 0; p < np; ++p) out[sensOff + k*np + p] = mu[p];
    }
  }
}

static void rk4s_backward_fill(rx_solve *rx, rx_solving_options *op, rx_solving_options_ind *ind,
                               int cSub, rk4s_rec &rec, const rksTableau &T, int nBase, int np,
                               int fxOff, int fpOff, int dfOff, int sensOff,
                               const std::vector<size_t> &boundary,
                               const std::vector<rk4s_dose> &doses,
                               const std::vector<rk4s_infus> &infus,
                               const std::vector<size_t> &boundaryDose,
                               const rk4s_rec *ssRec = NULL,
                               const double *ssContY = NULL,
                               const rk4s_rec *ss2Rec = NULL,
                               const std::vector<size_t> *ss2Steps = NULL,
                               const std::vector<size_t> *boundarySs2 = NULL,
                               const std::vector<size_t> *ss1Steps = NULL,
                               const std::vector<size_t> *boundarySs1 = NULL,
                               const double *ss2ContY = NULL,
                               int ssMdl = 0, double ssMdlAmt = 0.0, double ssMdlR = 0.0,
                               int ssMdlCmt = -1, size_t ssMdlOnSteps = 0) {
  size_t nStep = rec.nStep();
  if (nStep == 0) return;
  if (rec.composite) {
    // Build the ss IC (recorded with the composite's primary explicit tableau T,
    // e.g. dop853) and hand it to the composite fill so ss dosing gets the same
    // monodromy / continuous / ss=2 / interior-ss=1 IC terms as the explicit path.
    rk4sSsIc ssic; std::vector<double> Asc(rxEffNeq(ind, op), 0.0);
    if (ssRec || ssContY || ss2Rec || ss2ContY)
      ssic.build(cSub, ind, op, rec, fxOff, fpOff, rxEffNeq(ind, op), Asc,
                 ssRec, ssContY, ss2Rec, ss2ContY, ss2Steps, boundarySs2, ss1Steps, boundarySs1,
                 T, nBase, np, nStep);
    composite_backward_fill(rx, op, ind, cSub, rec, nBase, np, fxOff, fpOff, dfOff, sensOff, boundary, doses, infus, boundaryDose,
                            ssic.active() ? &ssic : NULL);
    return;
  }
  if (T.implicitRK) { radau_backward_fill(rx, op, ind, cSub, rec, T, nBase, np, fxOff, fpOff, dfOff, sensOff, boundary, doses, infus, boundaryDose); return; }
  if (T.rosenbrock) { ros_backward_fill(rx, op, ind, cSub, rec, T, nBase, np, fxOff, fpOff, dfOff, sensOff, boundary, doses, infus, boundaryDose); return; }
  int eff = rxEffNeq(ind, op);
  int nfx = nBase * nBase, nfp = nBase * np;
  int sN = T.s;

  // Precompute each step's per-stage Jacobians: FXs[i] holds stage i for all
  // steps (n*nfx + ...), FPs[i] likewise.  Stage i of step n is at
  // &rec.a[(n*sN + i)*nBase], time t0 + c_i*h.
  std::vector<std::vector<double> > FXs(sN), FPs(sN);
  for (int i = 0; i < sN; ++i) { FXs[i].resize(nStep * nfx); FPs[i].resize(nStep * nfp); }
  std::vector<double> Ascratch(eff, 0.0);
  for (size_t n = 0; n < nStep; ++n) {
    double h = rec.h[n], t0 = rec.t0[n];
    for (int i = 0; i < sN; ++i)
      rk4s_eval_jac(cSub, t0 + T.c[i] * h, &rec.a[(n * sN + i) * nBase], nBase, np,
                    fxOff, fpOff, Ascratch.data(), eff, ind,
                    &FXs[i][n * nfx], &FPs[i][n * nfp]);
  }

  // dF/dtheta block (param-only) read once; used for the additive-bolus
  // dose-parameter jump transpose.  dFdp[c*np + p] = dF_c/dtheta_p.
  std::vector<double> dFdp;
  bool haveDose = (dfOff >= 0) && !doses.empty();
  if (haveDose) {
    dFdp.resize(nBase * np);
    calc_lhs(cSub, rec.t0.empty() ? 0.0 : rec.t0[0], Ascratch.data(), ind->lhs);
    for (int i = 0; i < nBase * np; ++i) dFdp[i] = ind->lhs[dfOff + i];
  }
  // dlag/dtheta block (modeled-alag transversality dose-time jump).
  std::vector<double> dlagV; const double *dlagP = NULL;
  if (op->adjDlagOff >= 0 && !doses.empty()) {
    if (!haveDose) calc_lhs(cSub, rec.t0.empty() ? 0.0 : rec.t0[0], Ascratch.data(), ind->lhs);
    dlagV.resize(nBase * np);
    for (int i = 0; i < nBase * np; ++i) dlagV[i] = ind->lhs[op->adjDlagOff + i];
    dlagP = dlagV.data();
  }
  // Modeled-rate infusion dual: dR/dtheta block + per-step maps.  forcingCmt[n]>=0
  // marks steps inside an infusion window (in-window forcing quadrature); offCmt[n]
  // marks the off-boundary step (moving-boundary transversality, factor -amt/R).
  std::vector<double> dRateV, forcingMult; std::vector<int> forcingCmt, offCmt; std::vector<double> offFac;
  const double *dRatep = rk4sBuildInfusMaps(op, ind, cSub, rec, infus, nBase, np, nStep, Ascratch, dRateV, forcingCmt, forcingMult, offCmt, offFac);

  // DDE anticipating term (delayed Jacobian) -- shared helper, no-op unless the
  // model has delay().  Step start state for method T is stage 0 (c_0 = 0).
  rk4s_dde dde;
  dde.init(op, ind, cSub, rec, nBase, np, eff, Ascratch, [&](size_t n){ return &rec.a[(n * sN) * nBase]; });

  std::vector<double> lam(nBase), Ybar(nBase), abar(nBase), kbar(sN * nBase), mu(np);

  // The shared table-driven explicit-RK reverse-mode transpose (DRY): reverse of
  // one forward step under tableau T.  Given the incoming costate lamR
  // (= adjoint of y_{n+1}), produces the outgoing costate (= adjoint of y_n) and
  // accumulates the quadrature muR; plus the additive-bolus dose-parameter jump.
  // Init kbar_i = h b_i lam; then for i = s-1..0: abar = FX_i^T kbar_i,
  // muR += FP_i^T kbar_i, Ybar += abar, kbar_j += h A[i][j] abar (j<i).
  auto stepTranspose = [&](size_t n, std::vector<double> &lamR, std::vector<double> &muR) {
    double h = rec.h[n];
    for (int j = 0; j < nBase; ++j) Ybar[j] = lamR[j];
    for (int i = 0; i < sN; ++i)
      for (int j = 0; j < nBase; ++j) kbar[i * nBase + j] = h * T.b[i] * lamR[j];
    for (int i = sN - 1; i >= 0; --i) {
      const double *fx = &FXs[i][n * nfx], *fp = &FPs[i][n * nfp];
      const double *kb = &kbar[i * nBase];
      for (int j = 0; j < nBase; ++j) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fx[ii * nBase + j] * kb[ii]; abar[j] = s; }
      for (int p = 0; p < np; ++p) { double s = 0; for (int ii = 0; ii < nBase; ++ii) s += fp[ii * np + p] * kb[ii]; muR[p] += s; }
      // infusion forcing: in-window steps add F_p[ci] += dR/dtheta to the quadrature.
      if (dRatep && forcingCmt[n] >= 0) {
        int rc = forcingCmt[n]; for (int p = 0; p < np; ++p) muR[p] += forcingMult[n] * dRatep[rc * np + p] * kb[rc];
      }
      for (int j = 0; j < nBase; ++j) Ybar[j] += abar[j];
      for (int jstage = 0; jstage < i; ++jstage) {
        double aij = T.A[i * sN + jstage];
        if (aij != 0.0) { double *kbj = &kbar[jstage * nBase]; for (int j = 0; j < nBase; ++j) kbj[j] += h * aij * abar[j]; }
      }
    }
    for (int j = 0; j < nBase; ++j) lamR[j] = Ybar[j];   // additive bolus: dD/dX = I
    // infusion off-boundary transversality (lamR = lambda at the off-step here):
    if (dRatep && offCmt[n] >= 0) {
      int oc = offCmt[n]; for (int p = 0; p < np; ++p) muR[p] += offFac[n] * dRatep[oc * np + p] * lamR[oc];
    }
    if (!doses.empty()) rk4sApplyEventJumps(n, lamR, muR, doses, dFdp, haveDose, nBase, np,
                                            &FXs[0][n * nfx], dlagP);
  };

  // Steady-state IC term: the window starts from the converged trough yp*, whose
  // dY*/dp is the fixed point S_ss = (I-M)^{-1} B (M=dPhi/dY, B=dPhi/dp of one
  // steady-state period Phi = flow_tau . dose).  After the window sweep, lam is
  // the costate at yp*, and the IC contribution to mu is lam^T S_ss =
  // sum_{j>=0} lam^T M^j B.  Each term is one backward transpose of the recorded
  // ss period (ssRec, pure flow from the peak; bolus dose Jacobian is I -> no
  // dose jump): ssPeriodTranspose accumulates B^T nu into muAcc and replaces nu
  // with M^T nu, so iterating it from nu=lam sums the geometric series.
  std::vector<std::vector<double> > ssFXs, ssFPs;
  size_t ssN = (ssRec ? ssRec->nStep() : 0);
  if (ssN > 0) {
    ssFXs.resize(sN); ssFPs.resize(sN);
    for (int i = 0; i < sN; ++i) { ssFXs[i].resize(ssN * nfx); ssFPs[i].resize(ssN * nfp); }
    for (size_t n = 0; n < ssN; ++n) {
      double h = ssRec->h[n], t0s = ssRec->t0[n];
      for (int i = 0; i < sN; ++i)
        rk4s_eval_jac(cSub, t0s + T.c[i] * h, &ssRec->a[(n * sN + i) * nBase], nBase, np,
                      fxOff, fpOff, Ascratch.data(), eff, ind, &ssFXs[i][n * nfx], &ssFPs[i][n * nfp]);
    }
  }
  // Modeled rate()/dur() ss infusion: the ON rate R = amt/D(p) and the ON->OFF
  // boundary D(p) depend on the parameters, so the monodromy B gains a forcing
  // term durMult*dR/dp over the ON phase and a transversality term -(amt/R)*
  // durMult*dR/dp*lambda_c at the boundary (the exact ss analogue of the non-ss
  // rk4sBuildInfusMaps forcing/off-boundary).  dRate is read once at the trough
  // (param-only rate assumption, as in the non-ss path).
  std::vector<double> dRateSs; double ssDurMult = 0.0, ssOffFac = 0.0; int ssRc = -1;
  bool ssMdlOn = (ssMdl != 0 && ssN > 0 && op->adjDrateOff >= 0 && ssMdlCmt >= 0 &&
                  ssMdlCmt < nBase && ssMdlR != 0.0 && ssMdlOnSteps <= ssN);
  if (ssMdlOn) {
    dRateSs.assign((size_t)nBase * np, 0.0);
    for (int _i = 0; _i < eff; ++_i) Ascratch[_i] = 0.0;
    for (int _i = 0; _i < nBase; ++_i) Ascratch[_i] = ssRec->a[_i];   // trough
    calc_lhs(cSub, ssRec->t0[0], Ascratch.data(), ind->lhs);
    for (int _i = 0; _i < nBase * np; ++_i) dRateSs[_i] = ind->lhs[op->adjDrateOff + _i];
    ssDurMult = (ssMdl == 2) ? ssMdlAmt : 1.0;             // dur: amt, rate: 1
    ssOffFac = -(ssMdlAmt / ssMdlR) * ssDurMult;
    ssRc = ssMdlCmt;
  }
  auto ssPeriodTranspose = [&](std::vector<double> &nu, std::vector<double> &muAcc) {
    for (size_t nn = ssN; nn >= 1; --nn) {
      size_t n = nn - 1; double h = ssRec->h[n];
      for (int j = 0; j < nBase; ++j) Ybar[j] = nu[j];
      for (int i = 0; i < sN; ++i) for (int j = 0; j < nBase; ++j) kbar[i * nBase + j] = h * T.b[i] * nu[j];
      for (int i = sN - 1; i >= 0; --i) {
        const double *fx = &ssFXs[i][n * nfx], *fp = &ssFPs[i][n * nfp];
        const double *kb = &kbar[i * nBase];
        for (int j = 0; j < nBase; ++j) { double s = 0; for (int q = 0; q < nBase; ++q) s += fx[q * nBase + j] * kb[q]; abar[j] = s; }
        for (int p = 0; p < np; ++p) { double s = 0; for (int q = 0; q < nBase; ++q) s += fp[q * np + p] * kb[q]; muAcc[p] += s; }
        if (ssMdlOn && n < ssMdlOnSteps)                  // ON-phase dR/dp forcing
          for (int p = 0; p < np; ++p) muAcc[p] += ssDurMult * dRateSs[ssRc * np + p] * kb[ssRc];
        for (int j = 0; j < nBase; ++j) Ybar[j] += abar[j];
        for (int js = 0; js < i; ++js) { double aij = T.A[i * sN + js]; if (aij != 0.0) { double *kbj = &kbar[js * nBase]; for (int j = 0; j < nBase; ++j) kbj[j] += h * aij * abar[j]; } }
      }
      for (int j = 0; j < nBase; ++j) nu[j] = Ybar[j];
      if (ssMdlOn && n == ssMdlOnSteps)                   // ON->OFF moving-boundary transversality
        for (int p = 0; p < np; ++p) muAcc[p] += ssOffFac * dRateSs[ssRc * np + p] * nu[ssRc];
    }
  };

  // Continuous / full-interval infusion IC sensitivity.  At the constant steady
  // state Y_ss, f(Y_ss,p) + R.e = 0, so J.(dY_ss/dp) + df/dp = 0 (fixed rate,
  // dR/dp = 0) => S_ss = -J^{-1} df/dp.  We need lam^T S_ss = -(J^{-T} lam)^T
  // df/dp per observation, so LU-factor J once (FXc) and keep df/dp (ssFP).
  bool ssContOn = (ssContY != NULL);
  std::vector<double> FXc(ssContOn ? nBase * nBase : 0), ssFP(ssContOn ? nBase * np : 0);
  std::vector<int> ssPiv(ssContOn ? nBase : 0);
  if (ssContOn) {
    rk4s_eval_jac(cSub, rec.t0.empty() ? 0.0 : rec.t0[0], ssContY, nBase, np,
                  fxOff, fpOff, Ascratch.data(), eff, ind, FXc.data(), ssFP.data());
    if (!luFactor(FXc.data(), nBase, ssPiv.data())) ssContOn = false;  // singular J
  }

  // ss=2 superposition: at an interior ss2 event Y_after = Y_before + Y_ss_new(p)
  // (dPhi/dY = I), so the sweep passes the costate through unchanged and adds
  // lambda^T dY_ss_new/dp.  Precompute the new-regimen period Jacobians, form the
  // steady-state sensitivity matrix S2 = dY_ss_new/dp = (I-M)^{-1}B row-by-row
  // (row k = e_k^T (I-M)^{-1}B, the geometric series seeded with e_k), and mark
  // the event steps for the sweep below.
  size_t ss2N = (ss2Rec ? ss2Rec->nStep() : 0);
  std::vector<double> S2; std::vector<char> isSs2;
  if (ss2ContY && ss2Steps && !ss2Steps->empty()) {
    // CONTINUOUS ss=2 infusion: Y_ss_new is the constant steady state, so
    // S2 = dY_ss_new/dp = -J^{-1} df/dp at ss2ContY.  Materialize row-by-row:
    // row k = e_k^T (-J^{-1}) df/dp = -(J^{-T} e_k)^T df/dp, so LU-factor J once
    // and luSolveT(e_k) gives (J^{-1})_{k,:}.  Downstream S2 handling is identical.
    std::vector<double> J2(nBase * nBase), FP2(nBase * np); std::vector<int> piv2(nBase);
    rk4s_eval_jac(cSub, rec.t0.empty() ? 0.0 : rec.t0[0], ss2ContY, nBase, np,
                  fxOff, fpOff, Ascratch.data(), eff, ind, J2.data(), FP2.data());
    if (luFactor(J2.data(), nBase, piv2.data())) {
      S2.assign((size_t)nBase * np, 0.0);
      for (int k = 0; k < nBase; ++k) {
        std::vector<double> z(nBase, 0.0); z[k] = 1.0;
        luSolveT(J2.data(), nBase, piv2.data(), z.data());   // z = (J^{-1})_{k,:}
        for (int p = 0; p < np; ++p) { double s = 0; for (int j = 0; j < nBase; ++j) s += z[j] * FP2[j * np + p]; S2[(size_t)k * np + p] = -s; }
      }
      isSs2.assign(nStep + 1, 0);
      for (size_t s : *ss2Steps) if (s <= nStep) isSs2[s] = 1;
    }
  } else if (ss2N > 0 && ss2Steps && !ss2Steps->empty()) {
    std::vector<std::vector<double> > ss2FXs(sN), ss2FPs(sN);
    for (int i = 0; i < sN; ++i) { ss2FXs[i].resize(ss2N * nfx); ss2FPs[i].resize(ss2N * nfp); }
    for (size_t n = 0; n < ss2N; ++n) {
      double h = ss2Rec->h[n], t0s = ss2Rec->t0[n];
      for (int i = 0; i < sN; ++i)
        rk4s_eval_jac(cSub, t0s + T.c[i] * h, &ss2Rec->a[(n * sN + i) * nBase], nBase, np,
                      fxOff, fpOff, Ascratch.data(), eff, ind, &ss2FXs[i][n * nfx], &ss2FPs[i][n * nfp]);
    }
    auto ss2PeriodTranspose = [&](std::vector<double> &nu, std::vector<double> &muAcc) {
      for (size_t nn = ss2N; nn >= 1; --nn) {
        size_t n = nn - 1; double h = ss2Rec->h[n];
        for (int j = 0; j < nBase; ++j) Ybar[j] = nu[j];
        for (int i = 0; i < sN; ++i) for (int j = 0; j < nBase; ++j) kbar[i * nBase + j] = h * T.b[i] * nu[j];
        for (int i = sN - 1; i >= 0; --i) {
          const double *fx = &ss2FXs[i][n * nfx], *fp = &ss2FPs[i][n * nfp]; const double *kb = &kbar[i * nBase];
          for (int j = 0; j < nBase; ++j) { double s = 0; for (int q = 0; q < nBase; ++q) s += fx[q * nBase + j] * kb[q]; abar[j] = s; }
          for (int p = 0; p < np; ++p) { double s = 0; for (int q = 0; q < nBase; ++q) s += fp[q * np + p] * kb[q]; muAcc[p] += s; }
          for (int j = 0; j < nBase; ++j) Ybar[j] += abar[j];
          for (int js = 0; js < i; ++js) { double aij = T.A[i * sN + js]; if (aij != 0.0) { double *kbj = &kbar[js * nBase]; for (int j = 0; j < nBase; ++j) kbj[j] += h * aij * abar[j]; } }
        }
        for (int j = 0; j < nBase; ++j) nu[j] = Ybar[j];
      }
    };
    S2.assign((size_t)nBase * np, 0.0);
    for (int k = 0; k < nBase; ++k) {
      std::vector<double> nu(nBase, 0.0); nu[k] = 1.0; std::vector<double> muAdd(np, 0.0);
      for (int git = 0; git < op->maxSS + 5; ++git) {
        ss2PeriodTranspose(nu, muAdd);
        double nn2 = 0; for (int j = 0; j < nBase; ++j) nn2 += fabs(nu[j]);
        if (nn2 < 1e-14) break;
      }
      for (int p = 0; p < np; ++p) S2[(size_t)k * np + p] = muAdd[p];
    }
    isSs2.assign(nStep + 1, 0);
    for (size_t s : *ss2Steps) if (s <= nStep) isSs2[s] = 1;
  }

  // Interior ss=1 reset IC: S_ss1 = dY_ss1/dp = (I-M)^{-1}B of the window-start
  // ss=1 regimen (ssRec, same regimen), row-by-row via the geometric series.
  // Applied (then a reset lambda:=0) at each INTERIOR ss=1 step (step > 0; step 0
  // is the window start, handled by the geometric-series IC term below).
  std::vector<double> Sss1; std::vector<char> isSs1;
  if (ssN > 0 && ss1Steps) {
    bool anyInterior = false;
    for (size_t s : *ss1Steps) if (s > 0 && s <= nStep) { anyInterior = true; break; }
    if (anyInterior) {
      Sss1.assign((size_t)nBase * np, 0.0);
      for (int k = 0; k < nBase; ++k) {
        std::vector<double> nu(nBase, 0.0); nu[k] = 1.0; std::vector<double> muAdd(np, 0.0);
        for (int git = 0; git < op->maxSS + 5; ++git) {
          ssPeriodTranspose(nu, muAdd);
          double n2 = 0; for (int j = 0; j < nBase; ++j) n2 += fabs(nu[j]);
          if (n2 < 1e-14) break;
        }
        for (int p = 0; p < np; ++p) Sss1[(size_t)k * np + p] = muAdd[p];
      }
      isSs1.assign(nStep + 1, 0);
      for (size_t s : *ss1Steps) if (s > 0 && s <= nStep) isSs1[s] = 1;
    }
  }

  // Full-trajectory: for each observation and each base state k, an independent
  // reset sweep boundary[i]->0 with terminal covector e_k.
  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    size_t fromStep = boundary[i];
    double *out = getSolve(i);
    for (int k = 0; k < nBase; ++k) {
      for (int j = 0; j < nBase; ++j) lam[j] = (j == k) ? 1.0 : 0.0;
      for (int p = 0; p < np; ++p) mu[p] = 0.0;
      // An observation whose recorded step IS a state-jump step (reset / replace
      // / multiply, or a modeled-F/alag bolus at the obs time) reads the
      // POST-jump state, but the sweep below only applies jumps at steps
      // fromStep-1..0 -- so transpose that coincident jump into the seed here.
      // A reset adds no steps, so the pre- AND post-reset observations share the
      // same boundary step; boundaryDose[i] (the dose count when the obs was
      // stored) disambiguates -- only jumps recorded BEFORE this observation
      // apply to it.
      if (!doses.empty())
        rk4sApplyEventJumps(fromStep, lam, mu, doses, dFdp, haveDose, nBase, np,
                            &FXs[0][(fromStep < nStep ? fromStep : nStep - 1) * nfx], dlagP,
                            boundaryDose[i]);
      // An observation coincident with an ss=2 event reads the post-superposition
      // state, but its boundary step is not swept below -- add lambda^T S2 here,
      // gated on boundarySs2[i] (ss2 events recorded before this observation), so
      // a pre-ss2 observation at the same time is unaffected.
      if (!S2.empty() && boundarySs2)
        for (size_t z = 0; z < (*boundarySs2)[i] && z < ss2Steps->size(); ++z)
          if ((*ss2Steps)[z] == fromStep) {
            for (int p = 0; p < np; ++p) { double s = 0; for (int q = 0; q < nBase; ++q) s += lam[q] * S2[(size_t)q * np + p]; mu[p] += s; }
            break;
          }
      // An observation coincident with an interior ss=1 reset reads Y_ss1: add
      // lambda^T S_ss1 then reset lambda := 0 so the sweep below contributes
      // nothing from before the event (gated by boundarySs1[i]).
      if (!Sss1.empty() && boundarySs1)
        for (size_t z = 0; z < (*boundarySs1)[i] && z < ss1Steps->size(); ++z)
          if ((*ss1Steps)[z] == fromStep && fromStep > 0) {
            for (int p = 0; p < np; ++p) { double s = 0; for (int q = 0; q < nBase; ++q) s += lam[q] * Sss1[(size_t)q * np + p]; mu[p] += s; }
            for (int j = 0; j < nBase; ++j) lam[j] = 0.0;
            break;
          }
      dde.beginSweep(fromStep, lam);
      for (size_t nn = fromStep; nn >= 1; --nn) {
        stepTranspose(nn - 1, lam, mu); dde.applyStep(nn - 1, lam);
        // interior ss=2 superposition: costate at this event (post-ss2 side) adds
        // lambda^T S2; it then passes through the additive jump unchanged.
        if (!S2.empty() && isSs2[nn - 1])
          for (int p = 0; p < np; ++p) { double s = 0; for (int q = 0; q < nBase; ++q) s += lam[q] * S2[(size_t)q * np + p]; mu[p] += s; }
        // interior ss=1 reset: add lambda^T S_ss1, then reset lambda := 0 (the
        // reset re-establishes Y_ss1, wiping the pre-event sensitivity).
        if (!Sss1.empty() && isSs1[nn - 1]) {
          for (int p = 0; p < np; ++p) { double s = 0; for (int q = 0; q < nBase; ++q) s += lam[q] * Sss1[(size_t)q * np + p]; mu[p] += s; }
          for (int j = 0; j < nBase; ++j) lam[j] = 0.0;
        }
      }
      dde.applyDoseJumps(mu, doses);
      if (ssN > 0) {                       // monodromy IC term (bolus / periodic infusion)
        std::vector<double> nu = lam, muAdd(np, 0.0);
        for (int git = 0; git < op->maxSS + 5; ++git) {
          ssPeriodTranspose(nu, muAdd);
          double nn2 = 0; for (int j = 0; j < nBase; ++j) nn2 += fabs(nu[j]);
          if (nn2 < 1e-14) break;
        }
        for (int p = 0; p < np; ++p) mu[p] += muAdd[p];
      } else if (ssContOn) {               // continuous-infusion linear-solve IC term
        std::vector<double> w(lam.begin(), lam.begin() + nBase);   // w = J^{-T} lam
        luSolveT(FXc.data(), nBase, ssPiv.data(), w.data());
        for (int p = 0; p < np; ++p) { double s = 0; for (int j = 0; j < nBase; ++j) s += w[j] * ssFP[j * np + p]; mu[p] -= s; }
      }
      for (int p = 0; p < np; ++p) out[sensOff + k * np + p] = mu[p];
    }
  }
}

extern "C" void ind_rk4s_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neq,
                           t_dydt c_dydt, t_update_inis u_inis) {
  clock_t t0 = clock();
  int i;
  int istate = 1;
  void* ctx = NULL;

  neq[1] = rx->ordId[solveid]-1;
  rx_solving_options_ind *ind = &(rx->subjects[neq[1]]);
  int eff = rxEffNeq(ind, op);
  neq[0] = eff;

  double xout;
  int localBadSolve = 0;

  if (!iniSubject(neq[1], 0, ind, op, rx, u_inis)) return;

  double xp = getAllTimes(ind, 0);
  ind->solvedIdx = 0;

  int neqOde = op->neq - op->numLin - op->numLinSens;
  // DDE history: record dense steps for this subject when the model uses delay().
  // A single warm-up RHS evaluation lets _rxDelay learn the minimum delay so the
  // step size can be capped (keeping delay() lookups inside recorded history).
  ind->delayHistOn = op->hasDelay;
  ind->delayHistN  = 0;
  ind->delayT0     = xp;
  ind->delayMinT   = R_PosInf;
  ind->delayWarmed = 0;
  if (ind->delayHistOn) {
    std::vector<double> _wu(neqOde);
    c_dydt(neq, xp, getSolve(0), _wu.data());
    ind->delayWarmed = 1;
  }
  // dydt reads/writes the FULL neqOde state, so we step all of it (the adjoint
  // rx__sens_* compartments have d/dt=0 -> they stay 0); we RECORD only the
  // nBase base-state stages the backward sweep needs.
  int adj = op->adjoint;
  int nAll = neqOde;                          // full state advanced each step
  int nRec = adj ? op->adjNbase : neqOde;     // base states recorded per stage
  double *yp;

  rksTableau T = rksGetTableau(op->stiff);    // tableau for this method (primary if composite)
  rk4s_rec rec;
  rec.s = T.s;
  rec.nq = nRec;
  rec.composite = (op->stiff2 > 0) ? 1 : 0;   // AutoSwitch composite (dop853s+ros4s)
  std::vector<double> scratch;
  std::vector<size_t> boundary(ind->n_all_times, 0);  // cumulative steps at each stored time
  std::vector<size_t> boundaryDose(ind->n_all_times, 0); // # doses recorded when each time was stored
  std::vector<size_t> boundarySs2(ind->n_all_times, 0);  // # ss2 events recorded when each time was stored
  std::vector<size_t> boundarySs1(ind->n_all_times, 0);  // # ss1 events recorded when each time was stored
  std::vector<rk4s_dose> doseRec;                     // additive boluses for the F-jump transpose
  std::vector<rk4s_infus> infusRec;                   // modeled-rate infusion windows (rate/dur dual)
  int _infusOnCmt = -1;                               // set when the current event opens an infusion
  // Steady-state (ss=1 bolus) initial-condition sensitivity: after handleSS the
  // window starts from the converged TROUGH yp* = flow_tau(dose(yp*)).  Its
  // dY*/dp feeds the backward sweep's IC term via a one-period monodromy
  // (rk4s_ss_*).  ssRec records that one steady-state period's flow (from the
  // PEAK = dose(trough)); ssTrough is yp*, ssCmt/ssAmt/ssIi the bolus + period.
  rk4s_rec ssRec; bool ssActive = false; int ssCmt = -1; double ssAmt = 0.0, ssIi = 0.0;
  std::vector<double> ssTrough;
  int _ssPendCmt = -1; double _ssPendAmt = 0.0, _ssPendIi = 0.0;
  // steady-state fixed-rate infusion (dur<ii): period is ON for ssInfDur (rate
  // ssInfRate into ssInfCmt) then OFF for ssInfDur2; handed over from handleSS.
  bool ssInf = false; int ssInfCmt = -1; double ssInfDur = 0.0, ssInfDur2 = 0.0, ssInfRate = 0.0, ssInfRate2 = 0.0;
  // MODELED rate()/dur() ss infusion: moving boundary -> the monodromy B needs a
  // dRate forcing over the ON phase + a transversality term at the ON->OFF step.
  int ssInfModeled = 0; double ssInfAmt = 0.0; size_t ssOnSteps = 0;
  // continuous / full-interval infusion: constant steady state, dY_ss/dp via a
  // -J^{-1} df/dp linear solve (no monodromy).  ssContY holds Y_ss.
  bool ssCont = false; std::vector<double> ssContY;
  // ss==2 (superposition, bolus): Y_after = Y_before + Y_ss_new.  ss2Rec records
  // the new regimen's steady-state period (flow over ss2Ii from the peak Y_ss_new
  // captured by handleSS); ss2Steps marks each interior ss2 event step where the
  // backward sweep adds lambda^T dY_ss_new/dp (the superposition IC term).
  bool ss2Active = false; double ss2Ii = 0.0; int ss2Cmt = -1; double ss2Amt = 0.0;
  bool ss2Inf = false; int ss2InfCmt = -1;
  double ss2InfDur = 0.0, ss2InfDur2 = 0.0, ss2InfRate = 0.0, ss2InfRate2 = 0.0;
  // CONTINUOUS / full-interval ss==2 infusion: the added regimen reaches a
  // constant steady state Y_ss_new (ss2ContY, published by handleSS); its
  // superposition IC dY_ss_new/dp = -J^{-1} df/dp is a linear solve (kind 2).
  bool ss2Cont = false; std::vector<double> ss2ContY;
  std::vector<double> ss2Peak;
  std::vector<size_t> ss2Steps; rk4s_rec ss2Rec;
  int _ss2PendIi = 0; double _ss2Ii = 0.0; int _ss2Cmt = -1; double _ss2Amt = 0.0;
  // Interior ss=1 resets: an ss=1 event past the window start re-establishes the
  // regimen's steady state (Y_ss1, same monodromy as the window-start ss=1 in
  // ssRec).  At each such step the backward sweep adds lambda^T dY_ss1/dp then
  // RESETS lambda := 0 (the reset wipes the pre-event sensitivity).
  std::vector<size_t> ss1Steps;

  for(i = 0; i < ind->n_all_times; i++) {
    ind->idx=i;
    ind->linSS=0;
    if (ind->mainSorted == 0) {
      double *_rtime = ind->timeThread;
      for (int _j = i; _j < ind->n_all_times; _j++) {
        int _raw = ind->ix[_j];
        int _evid = getEvid(ind, _raw);
        if (_evid >= 10 && _evid <= 99) {
          _rtime[_raw] = ind->mtime[_evid - 10];
        } else if (!isObs(_evid)) {
          int _wh, _cmt, _wh100, _whI, _wh0;
          getWh(_evid, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
          if (_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) {
            _rtime[_raw] = getAllTimes(ind, _raw);
          }
        }
      }
      reSortMainTimeline(ind, i);
      ind->mainSorted = 1;
    }
    _growSolveIfNeeded(ind, op, i, 1);
    yp   = getSolve(i);
    xout = ind->timeThread[ind->ix[i]];

    if (getEvid(ind, ind->ix[i]) != 3 && !isSameTime(xout, xp)) {
      if (ind->err){
        ind->rc[0] = -1000;
        badSolveExit(i);
        localBadSolve = 1;
      } else {
        if (handleExtraDose(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                            xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx)) {
            if (!localBadSolve && !isSameTime(ind->extraDoseNewXout, xp)) {
              preSolve(op, ind, xp, ind->extraDoseNewXout, yp);
              if (nAll > 0) {
                  rks_step_interval(ind, op, c_dydt, neq, T, nAll, nRec, yp, xp, ind->extraDoseNewXout, &rec, scratch);
              }
              copyLinCmt(neq, ind, op, yp);
              const char* err_msg = "rk4s failed";
              postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 7, true, ind, op, rx);
              if (*(ind->rc) < 0) localBadSolve = 1;
              xp = ind->extraDoseNewXout;
            }
            if (!localBadSolve) {
              int idx = ind->idx;
              int ixds = ind->ixds;
              int trueIdx = ind->extraDoseTimeIdx[ind->idxExtra];
              ind->idx = -1-trueIdx;
              handle_evid(ind->extraDoseEvid[trueIdx], neq[0],
                          ind->BadDose, ind->InfusionRate, ind->dose, yp, xout, neq[1], ind);
              istate = 1;
              ind->ixds = ixds;
              ind->idx = idx;
              ind->idxExtra++;
              if (!isSameTime(xout, ind->extraDoseNewXout)) {
                preSolve(op, ind, ind->extraDoseNewXout, xout, yp);
                if (nAll > 0) {
                    rks_step_interval(ind, op, c_dydt, neq, T, nAll, nRec, yp, ind->extraDoseNewXout, xout, &rec, scratch);
                }
                copyLinCmt(neq, ind, op, yp);
                const char* err_msg = "rk4s failed";
                postSolve(neq, &istate, ind->rc, &idx, yp, &err_msg, 9, false, ind, op, rx);
                if (*(ind->rc) < 0) localBadSolve = 1;
                ind->extraDoseNewXout = xout;
              }
              xp = ind->extraDoseNewXout;
            }
        }
        if (!localBadSolve && !isSameTime(xout, xp)) {
          preSolve(op, ind, xp, xout, yp);
          if (nAll > 0) {
              rks_step_interval(ind, op, c_dydt, neq, T, nAll, nRec, yp, xp, xout, &rec, scratch);
          }
          copyLinCmt(neq, ind, op, yp);
          const char* err_msg = "rk4s failed";
          postSolve(neq, &istate, ind->rc, &i, yp, &err_msg, 7, true, ind, op, rx);
          if (*(ind->rc) < 0) localBadSolve = 1;
        }
        xp = xout;
      }
    }
    ind->_newind = 2;
    if (!localBadSolve){
      ind->idx = i;
      // Record forward state-jump events for the adjoint transpose jumps, tagged
      // with the cumulative step count (the jump precedes the next RK step, whose
      // 0-based index == rec.nStep() at this point).
      if (adj) {
        int _evCur = getEvid(ind, ind->ix[i]);
        if (_evCur == 3) {                                  // reset -> lambda := 0
          rk4s_dose _d; _d.step = rec.nStep(); _d.cmt = 0; _d.amt = 0.0; _d.type = 3;
          doseRec.push_back(_d);
        } else if (isDose(_evCur)) {
          int _wh, _cmtD, _wh100, _whI, _wh0;
          getWh(_evCur, &_wh, &_cmtD, &_wh100, &_whI, &_wh0);
          if (_cmtD >= 0 && _cmtD < op->adjNbase) {
            if (_whI == 0) {                                // additive bolus (F-jump)
              // A superposition (ss==2) dose is NOT applied to the window state
              // (its effect is the added Y_ss_new), so do not record it as a
              // window F-jump.
              if (op->adjDfOff >= 0 && _wh0 != EVID0_SS2 && _wh0 != EVID0_SS20) {
                rk4s_dose _d; _d.step = rec.nStep(); _d.cmt = _cmtD;
                _d.amt = getDose(ind, ind->ix[i]); _d.type = 0;
                doseRec.push_back(_d);
              }
              // ss=1 reset bolus: mark for one-period monodromy capture (the
              // trough yp* is read after handleSS below).  ii read now, before
              // handleEvid1 advances ind->ixds past this dose.
              if (_wh0 == EVID0_SS) {
                _ssPendCmt = _cmtD; _ssPendAmt = getDose(ind, ind->ix[i]);
                _ssPendIi = getIiNumber(ind, ind->ixds);
              } else if (_wh0 == EVID0_SS2 || _wh0 == EVID0_SS20) {
                // ss=2 superposition bolus: capture the new regimen's cmt/amt/ii;
                // Y_ss_new (the period-boundary trough) is read after handleSS.
                _ss2Ii = getIiNumber(ind, ind->ixds); _ss2PendIi = 1;
                _ss2Cmt = _cmtD; _ss2Amt = getDose(ind, ind->ix[i]);
              }
            } else if (_whI == EVIDF_REPLACE) {             // replace -> lambda[c] := 0
              rk4s_dose _d; _d.step = rec.nStep(); _d.cmt = _cmtD; _d.amt = 0.0; _d.type = 5;
              doseRec.push_back(_d);
            } else if (_whI == EVIDF_MULT) {                // multiply -> lambda[c] *= alpha
              rk4s_dose _d; _d.step = rec.nStep(); _d.cmt = _cmtD;
              _d.amt = getAmt(ind, ind->id, _cmtD, getDoseIndex(ind, ind->idx), xout, yp);
              _d.type = 6;
              doseRec.push_back(_d);
            } else if ((_whI == EVIDF_MODEL_RATE_ON || _whI == EVIDF_MODEL_DUR_ON) && op->adjDrateOff >= 0) {
              // modeled rate()/dur() infusion start: R read AFTER handleEvid1.  durMult
              // folds in the runtime factor (1 for rate, amt for dur).  The amount is
              // BIOAVAILABILITY-adjusted (getAmt = F*dose): the effective rate is
              // R = F*amt/dur and the moving-boundary tau2 = tau1 + F*amt/rate, so
              // durMult/offFac must use F*amt to stay consistent with the captured
              // R_eff (else F != 1 mis-scales the dR/dp forcing + off transversality).
              double _amt = getAmt(ind, ind->id, _cmtD, getDose(ind, ind->ix[i]), xout, yp);
              rk4s_infus _f; _f.onStep = rec.nStep(); _f.offStep = (size_t)-1;
              _f.cmt = _cmtD; _f.R = 0.0; _f.amt = _amt;
              _f.durMult = (_whI == EVIDF_MODEL_DUR_ON) ? _amt : 1.0;
              infusRec.push_back(_f);
              _infusOnCmt = _cmtD;
            } else if ((_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) && op->adjDrateOff >= 0) {
              for (size_t _w = infusRec.size(); _w-- > 0;)      // close the open window
                if (infusRec[_w].cmt == _cmtD && infusRec[_w].offStep == (size_t)-1) {
                  infusRec[_w].offStep = rec.nStep(); break;
                }
            }
          }
        } else if (!isObs(_evCur) && op->adjDrateOff >= 0) {
          // the modeled rate()/dur() OFF event is not isDose(): close the window.
          int _wh, _cmtD, _wh100, _whI, _wh0;
          getWh(_evCur, &_wh, &_cmtD, &_wh100, &_whI, &_wh0);
          if ((_whI == EVIDF_MODEL_RATE_OFF || _whI == EVIDF_MODEL_DUR_OFF) && _cmtD >= 0 && _cmtD < op->adjNbase) {
            for (size_t _w = infusRec.size(); _w-- > 0;)
              if (infusRec[_w].cmt == _cmtD && infusRec[_w].offStep == (size_t)-1) {
                infusRec[_w].offStep = rec.nStep(); break;
              }
          }
        }
      }
      if (getEvid(ind, ind->ix[i]) == 3) {
        handleEvid3(ind, op, rx, neq, &xp, &xout, yp, &(istate), u_inis);
      } else if (handleEvid1(&i, rx, neq, yp, &xout)){
        handleSS(neq, ind->BadDose, ind->InfusionRate, ind->dose, yp, xout,
                 xp, ind->id, &i, ind->n_all_times, &istate, op, ind, u_inis, ctx);
        if (ind->wh0 == EVID0_OFF){
          ind->solve[ind->cmt] = op->inits[ind->cmt];
        }
        if (rx->istateReset) istate = 1;
        xp = xout;
        // ss=1 bolus just converged: yp is the trough yp*.  Record it (one ss
        // regimen supported; a later ss dose overwrites -- the last-in-window
        // trough is the one whose IC the sweep reaches).
        // A MODELED rate/dur ss on a path that carries the moving boundary but is
        // not yet implemented for adjoint (large-duration / full-interval /
        // continuous, flagged _adjSSinfModeled < 0) -- error rather than return
        // silently-wrong sensitivities.  Only periodic modeled (dur<ii) is covered.
        if (_adjSSinfModeled < 0) {
          ind->err = 1; if (ind->rc[0] == 0) ind->rc[0] = -2020; localBadSolve = 1;
        }
        // ss=2 superposition (interior, bolus OR fixed-rate infusion).  Caught
        // FIRST: an ss2 infusion also fires the _adjSSinfKind handoff, but it is
        // an interior superposition, not a window-start infusion ss.  Y_ss_new
        // was published in _adjSS2peak; one ss2Rec/S2 serves all ss2 events.
        if (_adjSS2 && (_ss2PendIi || _adjSSinfKind == 1)) {
          if (!ss2Active) {
            ss2Active = true; ss2Peak.assign(_adjSS2peak.begin(), _adjSS2peak.end());
            if (_ss2PendIi) { ss2Ii = _ss2Ii; ss2Cmt = _ss2Cmt; ss2Amt = _ss2Amt; }
            else { ss2Inf = true; ss2InfCmt = _adjSSinfCmt; ss2InfDur = _adjSSinfDur;
                   ss2InfDur2 = _adjSSinfDur2; ss2InfRate = _adjSSinfRate; ss2InfRate2 = _adjSSinfRate2; }
          }
          ss2Steps.push_back(rec.nStep());
          _adjSS2 = 0; _ss2PendIi = 0; _adjSSinfKind = 0;
        } else if (_adjSS2 && _adjSSinfKind == 2) {
          // CONTINUOUS / full-interval ss=2 infusion: the added regimen reaches a
          // constant steady state Y_ss_new; its superposition IC dY_ss_new/dp =
          // -J^{-1} df/dp is a linear solve (materialized into S2 in backward_fill).
          if (!ss2Active) {
            ss2Active = true; ss2Cont = true;
            ss2ContY.assign(_adjSS2peak.begin(), _adjSS2peak.end());
          }
          ss2Steps.push_back(rec.nStep());
          _adjSS2 = 0; _adjSSinfKind = 0;
        } else if (_ssPendCmt >= 0) {
          ssActive = true; ssCmt = _ssPendCmt; ssAmt = _ssPendAmt; ssIi = _ssPendIi;
          ssTrough.assign(yp, yp + eff);
          ss1Steps.push_back(rec.nStep());   // every ss=1 event step (0 = window start)
          _ssPendCmt = -1;
        } else if (_adjSSinfKind == 1 && !ssActive) {
          // fixed-rate periodic infusion steady state: yp is the pre-infusion
          // trough (period boundary); record the ON/OFF period after the loop.
          ssActive = true; ssInf = true; ssInfCmt = _adjSSinfCmt;
          ssInfDur = _adjSSinfDur; ssInfDur2 = _adjSSinfDur2; ssInfRate = _adjSSinfRate; ssInfRate2 = _adjSSinfRate2;
          ssInfModeled = _adjSSinfModeled; ssInfAmt = _adjSSinfAmt;
          ssTrough.assign(yp, yp + eff);
          ss1Steps.push_back(rec.nStep());   // window-start ss=1 infusion (step 0)
          _adjSSinfKind = 0;
        } else if (_adjSSinfKind == 1 && ssActive) {
          // interior ss=1 infusion reset (same regimen as the window start).
          ss1Steps.push_back(rec.nStep());
          _adjSSinfKind = 0;
        } else if (_adjSSinfKind == 2 && !ssActive && !ssCont) {
          // continuous / full-interval infusion: yp is the constant steady state
          // Y_ss; the IC sensitivity is the linear solve done in backward_fill.
          ssCont = true; ssContY.assign(yp, yp + eff);
          _adjSSinfKind = 0;
        }
      }
      // Capture the modeled infusion rate R now that handleEvid1 has set it.
      if (_infusOnCmt >= 0 && !infusRec.empty()) {
        infusRec.back().R = ind->InfusionRate[_infusOnCmt];
        _infusOnCmt = -1;
      }
      int _mtime_requeued = 0;
      if (rx->nMtime > 0) {
        if (recomputeMtimeIfNeeded(rx, ind, yp, i, xout)) {
          ind->mainSorted = 0;
          _mtime_requeued = 1;
        }
      }
      if (rx->needSort & needSortAlag) {
        if (refreshLagTimesIfNeeded(rx, ind, yp, i + 1, xout)) {
          ind->mainSorted = 0;
        }
      }
      updateSolve(ind, op, neq, xout, i, ind->n_all_times);
      if (_mtime_requeued) i--;
    }
    if (adj && i >= 0 && i < ind->n_all_times) { boundary[i] = rec.nStep(); boundaryDose[i] = doseRec.size(); boundarySs2[i] = ss2Steps.size(); boundarySs1[i] = ss1Steps.size(); }
    ind->solvedIdx = i;
  }

  // Record one steady-state period's flow (from the PEAK = dose(trough)) for the
  // backward IC-term monodromy.  Done once here on a copy so the actual solve is
  // untouched; only for the table-driven explicit path (not composite/implicit/
  // Rosenbrock, which the ss IC term does not yet cover).
  if (adj && ssActive && !localBadSolve && ind->err == 0 &&
      !T.implicitRK && !T.rosenbrock) {
    std::vector<double> ytmp = ssTrough, ssScratch;
    if (ssInf) {
      // Two-phase steady-state infusion period from yp*: constant rate ssInfRate
      // over ssInfDur, then ssInfRate2 over ssInfDur2.  Fixed-rate periodic
      // (dur<ii): (R, dur) then (0, ii-dur).  Large-duration (dur>=ii,
      // overlapping infusions): ((numDoseInf+1)*R, offTime) then (numDoseInf*R,
      // addTime).  The rate is constant per phase (dR/dp == 0), so M=dPhi/dY and
      // B=dPhi/dp come entirely from the recorded flow (same monodromy as the
      // bolus; the infusion only shifts WHERE the model Jacobian is sampled).
      double _savedR = ind->InfusionRate[ssInfCmt], _t = 0.0;
      if (ssInfDur > 0.0) {
        ind->InfusionRate[ssInfCmt] = ssInfRate;
        rks_step_interval(ind, op, c_dydt, neq, T, nAll, op->adjNbase, ytmp.data(), _t, _t + ssInfDur, &ssRec, ssScratch);
        _t += ssInfDur;
      }
      ssOnSteps = ssRec.nStep();          // ON-phase step count (for the modeled-dR boundary)
      if (ssInfDur2 > 0.0) {
        ind->InfusionRate[ssInfCmt] = ssInfRate2;
        rks_step_interval(ind, op, c_dydt, neq, T, nAll, op->adjNbase, ytmp.data(), _t, _t + ssInfDur2, &ssRec, ssScratch);
      }
      ind->InfusionRate[ssInfCmt] = _savedR;
    } else if (ssIi > 0.0) {
      // bolus: yp after handleSS is the POST-dose peak yp* = dose(flow_tau(yp*));
      // for a bolus dose Jac is I / dose_dp is 0, so M=d(flow_tau)/dY,
      // B=d(flow_tau)/dp -- record just the flow over one period FROM the peak
      // (do NOT re-apply the dose; yp already contains it).
      rks_step_interval(ind, op, c_dydt, neq, T, nAll, op->adjNbase,
                        ytmp.data(), 0.0, ssIi, &ssRec, ssScratch);
    } else {
      ssActive = false;
    }
  } else {
    ssActive = false;
  }

  // Record the ss=2 regimen's steady-state period from Y_ss_new (bolus / finite
  // infusion).  A CONTINUOUS ss=2 infusion has no periodic monodromy -- its S2 is
  // a -J^{-1} df/dp linear solve at Y_ss_new (ss2ContY), built in backward_fill --
  // so it records nothing here but must stay ss2Active.
  if (adj && ss2Active && ss2Cont && !localBadSolve && ind->err == 0 &&
      !T.implicitRK && !T.rosenbrock) {
    // nothing to record; ss2ContY drives the linear-solve S2 below.
  } else if (adj && ss2Active && !localBadSolve && ind->err == 0 &&
      !T.implicitRK && !T.rosenbrock &&
      (int)ss2Peak.size() >= op->adjNbase && (ss2Inf || ss2Ii > 0.0)) {
    std::vector<double> ytmp2 = ss2Peak, ss2Scratch;
    if (ss2Inf) {
      // Fixed-rate infusion regimen: two-phase period from Y_ss_new (the period
      // boundary) -- ss2InfRate over ss2InfDur then ss2InfRate2 over ss2InfDur2
      // (same as the window-start infusion ss; dR/dp == 0).
      double _sr = ind->InfusionRate[ss2InfCmt], _t = 0.0;
      if (ss2InfDur > 0.0) { ind->InfusionRate[ss2InfCmt] = ss2InfRate;
        rks_step_interval(ind, op, c_dydt, neq, T, nAll, op->adjNbase, ytmp2.data(), _t, _t + ss2InfDur, &ss2Rec, ss2Scratch); _t += ss2InfDur; }
      if (ss2InfDur2 > 0.0) { ind->InfusionRate[ss2InfCmt] = ss2InfRate2;
        rks_step_interval(ind, op, c_dydt, neq, T, nAll, op->adjNbase, ytmp2.data(), _t, _t + ss2InfDur2, &ss2Rec, ss2Scratch); }
      ind->InfusionRate[ss2InfCmt] = _sr;
    } else {
      // Bolus: ss2Peak is the period-boundary TROUGH, so apply the dose to reach
      // the peak = dose(trough) and record the flow FROM there (the trajectory
      // the monodromy M=dPhi/dY, B=dPhi/dp are evaluated along).
      if (ss2Cmt >= 0 && ss2Cmt < eff) ytmp2[ss2Cmt] += ss2Amt;
      rks_step_interval(ind, op, c_dydt, neq, T, nAll, op->adjNbase, ytmp2.data(), 0.0, ss2Ii, &ss2Rec, ss2Scratch);
    }
  } else {
    ss2Active = false;
  }

  // ---- backward reverse-mode sweep -> fill rx__sens_* output slots ----
  if (adj && !localBadSolve && ind->err == 0) {
    rk4s_backward_fill(rx, op, ind, neq[1], rec, T, op->adjNbase, op->adjNp,
                       op->adjFxOff, op->adjFpOff, op->adjDfOff, op->adjSensOff,
                       boundary, doseRec, infusRec, boundaryDose, ssActive ? &ssRec : NULL,
                       ssCont ? ssContY.data() : NULL,
                       ss2Active ? &ss2Rec : NULL, &ss2Steps, &boundarySs2, &ss1Steps, &boundarySs1,
                       (ss2Active && ss2Cont) ? ss2ContY.data() : NULL,
                       ssInfModeled, ssInfAmt, ssInfRate, ssInfCmt, ssOnSteps);
  }

  ind->solveTime += ((double)(clock() - t0))/CLOCKS_PER_SEC;
}

extern "C" void ind_rk4s(rx_solve *rx, int solveid,
                         t_dydt c_dydt, t_update_inis u_inis){
  rx_solving_options *op = rx->op;
  int neq[2];
  neq[0] = op->neq;
  neq[1] = 0;
  ind_rk4s_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_rk4s(rx_solve *rx){
  rx_solving_options *op = rx->op;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  uint32_t nsub = rx->nsub, nsim = rx->nsim;
  int nsolve = (int)(nsim*nsub);

  uint32_t seed0 = getRxSeed1(cores);
  int abort = 0;

#ifdef _OPENMP
#pragma omp parallel for num_threads(cores)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++){
    int neq[2];
    neq[0] = op->neq;
    neq[1] = 0;
    int localAbort;
#ifdef _OPENMP
#pragma omp atomic read
#endif
    localAbort = abort;
    if (localAbort == 0){
      setSeedEng1(seed0 + rx->ordId[solveid] - 1);
      ind_rk4s_0(rx, op, solveid, neq, dydt, update_inis);

      if (op->badSolve) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        abort = 1;
      }
    }
  }
}

#endif // IN_PAR_SOLVE
