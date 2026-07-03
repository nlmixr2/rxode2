// Exact discrete-adjoint driver for Adams-Bashforth (method="abs", code 208 =
// base ab 8 + 200).  Fills the same rx__sens_<state>_BY_<param>__ columns as the
// other adjoint methods, as the exact reverse-mode transpose of ab.cpp's classical
// fixed-order fixed-step AB step map (NOT liblsoda's Nordsieck form).
//
// #included into par_solve.cpp AFTER ab.cpp (same TU), so it sees ab.cpp's recording
// buffers (abAdjSteps/abAdjCalls) and ab_coef, plus calc_lhs / dydt / getSolve.
//
// ab_do_steps re-initialises (RK4 startup + fresh f-history) every call, so each call
// is a self-contained integration and calls chain only through y.  Per call, order k,
// step dt (last step clipped to the output time), y-points p_0..p_M:
//   startup step m (m<k-1): p_{m+1} = RK4(p_m)   [also supplies f(p_m) to the history]
//   AB step m (m>=k-1):     p_{m+1} = p_m + dt*sum_{i=0}^{k-1} c_i f(p_{m-i})
// The reverse mode keeps a costate per y-point (lam[0..M]); processing steps in
// reverse, an AB step distributes lam[j+1] to lam[j..j-k+1] via  dt*c_i*J(p_{j-i})^T
// and accumulates mu += dt*c_i*F_p(p_{j-i})^T lam[j+1] (the i=0 term also carries the
// identity from the +p_j).  A startup step uses the classical RK4 transpose (stages
// recomputed from the recorded y).  lam[0] (costate of the call start) chains to the
// previous call's end.  Interior dose jumps are the additive-bolus identity here
// (multi-dose works); reset/replace/multiply/infusion duals are a follow-up.
//
// Observations land exactly on call ends (the last step clips to the output time), so
// no interpolation is needed.  The initial y0 is p-independent (dy0/dp = 0), so unlike
// liblsoda's Nordsieck init there is no t0 quadrature term.
#ifdef IN_PAR_SOLVE

extern "C" void ind_ab_adj_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neqUnused,
                             t_dydt c_dydt, t_update_inis u_inis) {
  (void) neqUnused;
  int cSub = rx->ordId[solveid] - 1;
  rx_solving_options_ind *ind = &(rx->subjects[cSub]);
  int nBase = op->adjNbase, np = op->adjNp, eff = rxEffNeq(ind, op);
  int fxOff = op->adjFxOff, fpOff = op->adjFpOff, sensOff = op->adjSensOff;

  // ---- forward: run the ordinary AB solve, recording each call's steps -----------
  abAdjSteps.clear(); abAdjCalls.clear(); abAdjNbase = nBase;
  abAdjActive = true;
  int neqF[2]; neqF[0] = op->neq; neqF[1] = 0;
  ind_ab_0(rx, op, solveid, neqF, c_dydt, u_inis);
  abAdjActive = false;
  if (op->badSolve || ind->err != 0 || abAdjCalls.empty()) return;

  double *lhs = ind->lhs;
  std::vector<double> A(eff, 0.0), Aw(eff, 0.0);
  std::vector<double> mu(np, 0.0);
  std::vector<double> k1(eff), k2(eff), k3(eff), k4(eff);
  int neqA[2]; neqA[0] = eff; neqA[1] = cSub;

  // fx = df_i/dy_j (row-major i*nBase+j), fp = df_i/dp (i*np+p), at (t, base state yb).
  auto abEval = [&](double t, const double *yb, const double **fx, const double **fp) {
    for (int i = 0; i < eff; ++i) A[i] = 0.0;
    for (int i = 0; i < nBase; ++i) A[i] = yb[i];
    calc_lhs(cSub, t, A.data(), lhs); *fx = &lhs[fxOff]; *fp = &lhs[fpOff];
  };
  auto fEval = [&](double t, const double *yb, std::vector<double> &out) {
    for (int i = 0; i < eff; ++i) Aw[i] = 0.0;
    for (int i = 0; i < nBase; ++i) Aw[i] = yb[i];
    dydt(neqA, t, Aw.data(), out.data());
  };

  // Classical RK4 reverse mode over one startup step (stages recomputed from yb).
  // yout = yb + dt/6 (k1+2k2+2k3+k4); k1=f(yb), k2=f(yb+.5dt k1), k3=f(yb+.5dt k2),
  // k4=f(yb+dt k3).  Adds to lamIn (costate of yb) and mu.
  std::vector<double> s2(nBase), s3(nBase), s4(nBase), tmp(nBase);
  std::vector<double> k1b(nBase), k2b(nBase), k3b(nBase), k4b(nBase), sb(nBase);
  auto rk4T = [&](const double *yb, double t, double dt, const double *lamOut, double *lamIn) {
    fEval(t, yb, k1);
    for (int b = 0; b < nBase; ++b) s2[b] = yb[b] + 0.5 * dt * k1[b];  fEval(t + 0.5*dt, s2.data(), k2);
    for (int b = 0; b < nBase; ++b) s3[b] = yb[b] + 0.5 * dt * k2[b];  fEval(t + 0.5*dt, s3.data(), k3);
    for (int b = 0; b < nBase; ++b) s4[b] = yb[b] + dt * k3[b];        fEval(t + dt, s4.data(), k4);
    const double *fx1, *fp1, *fx2, *fp2, *fx3, *fp3, *fx4, *fp4;
    // (each abEval overwrites lhs, so pull the needed blocks by copying out.)
    std::vector<double> J1(nBase*nBase), P1(nBase*np), J2(nBase*nBase), P2(nBase*np),
                        J3(nBase*nBase), P3(nBase*np), J4(nBase*nBase), P4(nBase*np);
    abEval(t, yb, &fx1, &fp1);           for (int q=0;q<nBase*nBase;++q) J1[q]=fx1[q]; for(int q=0;q<nBase*np;++q) P1[q]=fp1[q];
    abEval(t+0.5*dt, s2.data(), &fx2,&fp2); for (int q=0;q<nBase*nBase;++q) J2[q]=fx2[q]; for(int q=0;q<nBase*np;++q) P2[q]=fp2[q];
    abEval(t+0.5*dt, s3.data(), &fx3,&fp3); for (int q=0;q<nBase*nBase;++q) J3[q]=fx3[q]; for(int q=0;q<nBase*np;++q) P3[q]=fp3[q];
    abEval(t+dt, s4.data(), &fx4,&fp4);     for (int q=0;q<nBase*nBase;++q) J4[q]=fx4[q]; for(int q=0;q<nBase*np;++q) P4[q]=fp4[q];
    auto Jt = [&](const std::vector<double> &J, const double *v, std::vector<double> &r){ for(int b=0;b<nBase;++b){double s=0;for(int a=0;a<nBase;++a)s+=J[a*nBase+b]*v[a];r[b]=s;} };
    auto Pt = [&](const std::vector<double> &P, const double *v){ for(int p=0;p<np;++p){double s=0;for(int a=0;a<nBase;++a)s+=P[a*np+p]*v[a];mu[p]+=s;} };
    // reverse (topological): yout -> k4,s4 -> k3,s3 -> k2,s2 -> k1
    for (int b=0;b<nBase;++b){ double a=lamOut[b]; sb[b]=a; k1b[b]=(dt/6)*a; k2b[b]=(2*dt/6)*a; k3b[b]=(2*dt/6)*a; k4b[b]=(dt/6)*a; }
    Jt(J4, k4b.data(), tmp); Pt(P4, k4b.data());                 // k4=f(s4)
    for (int b=0;b<nBase;++b){ sb[b]+=tmp[b]; k3b[b]+=dt*tmp[b]; } // s4=yb+dt k3
    Jt(J3, k3b.data(), tmp); Pt(P3, k3b.data());                 // k3=f(s3)
    for (int b=0;b<nBase;++b){ sb[b]+=tmp[b]; k2b[b]+=0.5*dt*tmp[b]; } // s3=yb+.5dt k2
    Jt(J2, k2b.data(), tmp); Pt(P2, k2b.data());                 // k2=f(s2)
    for (int b=0;b<nBase;++b){ sb[b]+=tmp[b]; k1b[b]+=0.5*dt*tmp[b]; } // s2=yb+.5dt k1
    Jt(J1, k1b.data(), tmp); Pt(P1, k1b.data());                 // k1=f(yb)
    for (int b=0;b<nBase;++b) lamIn[b] += sb[b] + tmp[b];
  };

  std::vector<double> endCostate(nBase), lam;
  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    double ti = ind->timeThread[ind->ix[i]];
    int cObs = -1;
    for (int c = (int)abAdjCalls.size() - 1; c >= 0; --c) { if (fabs(abAdjCalls[c].t1 - ti) < 1e-9) { cObs = c; break; } }
    double *out = getSolve(i);
    for (int k = 0; k < nBase; ++k) {
      std::fill(mu.begin(), mu.end(), 0.0);
      if (cObs >= 0) {
        std::fill(endCostate.begin(), endCostate.end(), 0.0); endCostate[k] = 1.0;
        for (int c = cObs; c >= 0; --c) {
          const abRecCall &call = abAdjCalls[c];
          int M = (int)(call.stepEnd - call.stepBegin), ord = call.order;
          const double *cc = ab_coef[ord - 1];
          lam.assign((size_t)(M + 1) * nBase, 0.0);
          for (int b = 0; b < nBase; ++b) lam[(size_t)M * nBase + b] = endCostate[b];
          for (int j = M - 1; j >= 0; --j) {
            size_t g = call.stepBegin + j;
            const abRecStep &st = abAdjSteps[g];
            const double *lamOut = &lam[(size_t)(j + 1) * nBase];
            double *lamJ = &lam[(size_t)j * nBase];
            if (st.isRK4) {
              rk4T(st.y.data(), st.t, st.dt, lamOut, lamJ);
            } else {
              for (int ii = 0; ii < ord; ++ii) {
                const abRecStep &sh = abAdjSteps[g - ii];      // history point p_{j-ii}
                const double *fx, *fp; abEval(sh.t, sh.y.data(), &fx, &fp);
                double *lamTgt = &lam[(size_t)(j - ii) * nBase];
                for (int b = 0; b < nBase; ++b) { double s = 0; for (int a = 0; a < nBase; ++a) s += fx[a * nBase + b] * lamOut[a]; lamTgt[b] += st.dt * cc[ii] * s; }
                if (ii == 0) for (int b = 0; b < nBase; ++b) lamTgt[b] += lamOut[b];  // + identity from p_j
                for (int p = 0; p < np; ++p) { double s = 0; for (int a = 0; a < nBase; ++a) s += fp[a * np + p] * lamOut[a]; mu[p] += st.dt * cc[ii] * s; }
              }
            }
          }
          for (int b = 0; b < nBase; ++b) endCostate[b] = lam[b];  // -> previous call (bolus identity)
        }
      }
      for (int p = 0; p < np; ++p) out[sensOff + k * np + p] = mu[p];
    }
  }
}

extern "C" void ind_ab_adj(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op;
  int neq[2]; neq[0] = op->neq; neq[1] = 0;
  ind_ab_adj_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_ab_adj(rx_solve *rx) {
  // serial (thread_local recording buffers); parallel is a follow-up.
  rx_solving_options *op = rx->op;
  int nsolve = (int)(rx->nsim * rx->nsub);
  for (int solveid = 0; solveid < nsolve; ++solveid) {
    int neq[2]; neq[0] = op->neq; neq[1] = 0;
    ind_ab_adj_0(rx, op, solveid, neq, dydt, update_inis);
    if (op->badSolve) break;
  }
}

#endif // IN_PAR_SOLVE
