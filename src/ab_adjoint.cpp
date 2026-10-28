// Exact discrete-adjoint driver for Adams-Bashforth (method="abs", code 208).
// Fills the same rx__sens_<state>_BY_<param>__ columns as the other adjoint
// methods, as the exact reverse-mode transpose of ab.cpp's fixed-order
// fixed-step AB step map.
//
// #included into par_solve.cpp after ab.cpp (same TU), so it sees ab.cpp's
// recording buffers (abAdjSteps/abAdjCalls) and ab_coef, plus calc_lhs/dydt.
//
// Per call (order k, step dt, y-points p_0..p_M): a startup step is RK4, an AB
// step is p_{m+1} = p_m + dt*sum c_i f(p_{m-i}).  The reverse mode keeps a costate
// per y-point; an AB step distributes lam[j+1] via dt*c_i*J(p_{j-i})^T and
// accumulates mu += dt*c_i*F_p(p_{j-i})^T lam[j+1], a startup step uses the RK4
// transpose.  The events at a call start (a bolus with its modeled f() and alag()
// duals, reset, replace, multiply) transpose last first; infusion duals are not
// carried, and rxSolve() refuses them for abs.
//
// Observations land on call ends (no interpolation).  y0 is p-independent, so
// there is no t0 quadrature term.
#ifdef IN_PAR_SOLVE

extern "C" void ind_ab_adj_0(rx_solve *rx, rx_solving_options *op, int solveid, int *neqUnused,
                             t_dydt c_dydt, t_update_inis u_inis) {
  (void) neqUnused;
  int cSub = solveid; // subject id, not an rx->ordId position
  rx_solving_options_ind *ind = &(rx->subjects[cSub]);
  int nBase = op->adjNbase, np = op->adjNp, eff = rxEffNeq(ind, op);
  int fxOff = op->adjFxOff, fpOff = op->adjFpOff, sensOff = op->adjSensOff;

  // ---- forward: run the ordinary AB solve, recording each call's steps -----------
  abAdjSteps.clear(); abAdjCalls.clear(); abAdjNbase = nBase; _adjJumpCap.clear();
  abAdjActive = true;
  int neqF[2]; neqF[0] = op->neq; neqF[1] = 0;
  ind_ab_0(rx, op, solveid, neqF, c_dydt, u_inis);
  abAdjActive = false;
  // this subject's failure, not op->badSolve, which any subject's sets
  if (ind->rc[0] < 0 || ind->err != 0) return;
  if (abAdjCalls.empty()) {             // no step taken: still at the initial conditions
    rk4sIniOnlyFill(op, ind, cSub, nBase, np, sensOff);
    return;
  }

  // ---- the events at a time from the event table, in the order the forward
  // solve applied them: 0 bolus (dPhi/dy=I; its modeled F and alag duals, raw amt
  // in factor and F*amt in amtF) | 3 reset (->0) | 5 replace[c] (->0) | 6
  // multiply[c] (*=factor).
  struct abJump { int type, cmt; double factor, amtF = 0.0; int rec = -1; };   // rec: a replace/multiply's record index
  // the start state of call c
  auto abCallY0 = [&](size_t c) {
    const abRecCall &C = abAdjCalls[c];
    std::vector<double> y(nBase, 0.0);
    if (C.stepEnd > C.stepBegin) y = abAdjSteps[C.stepBegin].y;
    return y;
  };
  // the events at time t among the first `upTo` records (a reset only when it
  // follows a solved interval, `reset`); y is the state at t, for a bolus's F*amt
  auto abEventsAt = [&](double t, int upTo, bool reset, const double *y) {
    std::vector<abJump> ev;
    std::vector<double> _y(eff, 0.0);
    for (int b = 0; b < nBase; ++b) _y[b] = y[b];
    for (int i = 0; i < upTo; ++i) {
      int e = getEvid(ind, ind->ix[i]);
      if (isObs(e)) continue;
      if (!rxAdjSameTime(ind->timeThread[ind->ix[i]], t)) continue;
      if (e == 3) { if (reset) ev.push_back({3, -1, 1.0}); continue; }
      if (!isDose(e)) continue;
      int _wh, _cmt, _wh100, _whI, _wh0; getWh(e, &_wh, &_cmt, &_wh100, &_whI, &_wh0);
      if (_cmt < 0 || _cmt >= nBase) continue;
      if (_whI == EVIDF_REPLACE) { abJump _j = {5, _cmt, 0.0}; _j.rec = i; ev.push_back(_j); }
      else if (_whI == EVIDF_MULT) { abJump _j = {6, _cmt, getDose(ind, ind->ix[i])}; _j.rec = i; ev.push_back(_j); }
      else if (_whI == 0 && _wh0 == EVID0_REGULAR && (op->adjDfOff >= 0 || op->adjDlagOff >= 0)) {
        double _amt = getDose(ind, ind->ix[i]);
        abJump _j = {0, _cmt, _amt};
        _j.amtF = getAmt(ind, ind->id, _cmt, _amt, t, _y.data());
        ev.push_back(_j);
      }
    }
    return ev;
  };
  size_t nCalls = abAdjCalls.size();
  std::vector<std::vector<abJump> > callEv(nCalls);
  for (size_t c = 0; c < nCalls; ++c)
    callEv[c] = abEventsAt(abAdjCalls[c].t0, ind->n_all_times, c >= 1, abCallY0(c).data());

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

  std::vector<double> dx0V;                         // parameter-dependent initial conditions
  rk4sIniDx0(op, ind, cSub, abAdjCalls[0].t0, eff, nBase, np, dx0V);
  const double *dx0P = dx0V.empty() ? NULL : dx0V.data();
  // the events of call c's start, last first: a bolus adds its modeled-F dual
  // mu += amt*dF/dp*lam[c] and alag transversality mu += -F*amt*dlag/dp*(lam^T
  // F_X[:,c]) with F_X at the call's start state; reset adds the initial-condition
  // term, then 0; replace[c] -> 0; multiply[c] *= factor, each after its alag()
  // dual (rxAdjJumpTerm)
  auto abJumpT = [&](const std::vector<abJump> &ev, double t, const double *y, double *cost, double *muP) {
    for (size_t e = ev.size(); e-- > 0;) {
      const abJump &E = ev[e];
      if (E.type == 0) {
        const double *fx, *fp; abEval(t, y, &fx, &fp);
        if (op->adjDfOff >= 0) {
          const double *dF = &lhs[op->adjDfOff];
          for (int p = 0; p < np; ++p) muP[p] += E.factor * dF[E.cmt * np + p] * cost[E.cmt];
        }
        if (op->adjDlagOff >= 0) {
          const double *dlag = &lhs[op->adjDlagOff];
          double sc = 0.0;
          for (int b = 0; b < nBase; ++b) sc += cost[b] * fx[b * nBase + E.cmt];
          for (int p = 0; p < np; ++p) muP[p] += -E.amtF * dlag[E.cmt * np + p] * sc;
        }
      }
      else if (E.type == 3) {
        rk4sAddIni(dx0P, cost, muP, nBase, np);
        for (int b = 0; b < nBase; ++b) cost[b] = 0.0;
      }
      else if (E.type == 5 || E.type == 6) {
        rxAdjJumpTerm(E.rec, E.cmt, cost, muP, nBase, np, op->adjDlagOff >= 0 ? &lhs[op->adjDlagOff] : NULL);
        if (E.type == 5) cost[E.cmt] = 0.0; else cost[E.cmt] *= E.factor;
      }
    }
  };
  std::vector<double> endCostate(nBase), lam;
  for (int i = 0; i < ind->n_all_times; ++i) {
    if (!isObs(getEvid(ind, ind->ix[i]))) continue;
    double ti = ind->timeThread[ind->ix[i]];
    int cObs = -1;
    for (int c = (int)abAdjCalls.size() - 1; c >= 0; --c) { if (rxAdjSameTime(abAdjCalls[c].t1, ti)) { cObs = c; break; } }
    double *out = getSolve(i);
    for (int k = 0; k < nBase; ++k) {
      std::fill(mu.begin(), mu.end(), 0.0);
      if (cObs >= 0) {
        std::fill(endCostate.begin(), endCostate.end(), 0.0); endCostate[k] = 1.0;
        // an observation at an event time reads the state after the jumps recorded
        // before it: those that open the next call, or, after the last call, the
        // ones at the last record time
        if (cObs + 1 < (int)nCalls && rxAdjSameTime(abAdjCalls[cObs + 1].t0, ti))
          abJumpT(callEv[cObs + 1], abAdjCalls[cObs + 1].t0, abCallY0(cObs + 1).data(), endCostate.data(), mu.data());
        else
          abJumpT(abEventsAt(ti, i, true, out), ti, out, endCostate.data(), mu.data());
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
          for (int b = 0; b < nBase; ++b) endCostate[b] = lam[b];  // costate of call c's start
          // the jumps at t0_c -> costate of the pre-event state (call c-1's end, or
          // the initial conditions for call 0)
          abJumpT(callEv[c], abAdjCalls[c].t0, abCallY0(c).data(), endCostate.data(), mu.data());
        }
        rk4sAddIni(dx0P, endCostate.data(), mu.data(), nBase, np);   // window start t0
      } else {
        // no call ends at ti: the observation is at t0, or at a reset (abs does not
        // integrate up to a reset).  It reads the initial conditions after the
        // jumps of the call that starts there.
        std::fill(endCostate.begin(), endCostate.end(), 0.0); endCostate[k] = 1.0;
        bool opens = false;
        for (size_t c = 0; c < nCalls && !opens; ++c)
          if (rxAdjSameTime(abAdjCalls[c].t0, ti)) {
            abJumpT(callEv[c], abAdjCalls[c].t0, abCallY0(c).data(), endCostate.data(), mu.data());
            opens = true;
          }
        // a reset at the last record time opens no call
        if (!opens) abJumpT(abEventsAt(ti, i, true, out), ti, out, endCostate.data(), mu.data());
        rk4sAddIni(dx0P, endCostate.data(), mu.data(), nBase, np);
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
  // Parallel across subjects: the recording buffers (abAdjSteps/abAdjCalls) are
  // thread_local and each subject uses its own ind->lhs / getSolve storage, so
  // distinct subjects never alias (bit-identical to serial).
  rx_solving_options *op = &op_global;
#ifdef _OPENMP
  int cores = op->cores;
#else
  int cores = 1;
#endif
  int nsolve = (int)(rx->nsim * rx->nsub);
  int displayProgress = (op->nDisplayProgress <= nsolve);
  clock_t t0 = clock();
  int curTick = 0, cur = 0, abort = 0;
  uint32_t seed0 = getRxSeed1(cores);
#ifdef _OPENMP
#pragma omp parallel for num_threads(op->cores) schedule(dynamic,1)
#endif
  for (int solveid = 0; solveid < nsolve; solveid++) {
    int localAbort;
#pragma omp atomic read
    localAbort = abort;
    if (localAbort == 0) {
      // rx->ordId walks positions (sortIds); the drivers take subject ids.
      int _id = rx->ordId[solveid] - 1;
      setSeedEng1(seed0 + _id);
      int neq[2]; neq[0] = op->neq; neq[1] = 0;
      ind_ab_adj_0(rx, op, _id, neq, dydt, update_inis);
      if (displayProgress) {
#pragma omp critical
        cur++;
#ifdef _OPENMP
        if (omp_get_thread_num() == 0)
#endif
        {
          curTick = par_progress(cur, nsolve, curTick, cores, t0, 0);
          int la2;
#pragma omp atomic read
          la2 = abort;
          if (la2 == 0 && checkInterrupt()) { int nA = 1;
#pragma omp atomic write
            abort = nA; }
        }
      }
    }
  }
  setRxSeedFinal(seed0 + (uint32_t)nsolve);
  if (abort == 1) { op->abort = 1; par_progress(cur, nsolve, curTick, cores, t0, 1); }
  else if (displayProgress && curTick < 50) par_progress(nsolve, nsolve, curTick, cores, t0, 0);
}

#endif // IN_PAR_SOLVE
