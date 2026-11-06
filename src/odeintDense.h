#ifndef ODEINTDENSE_H
#define ODEINTDENSE_H

// One dense segment [xp, xout] with an odeint dense-output stepper: fills the
// observation slots obs_first..obs_last by dense interpolation and records each
// accepted step into the delay history: NS = 4 samples give an exact cubic
// record (ros4), NS = 8 a degree-7 record (dop5, bs).  The caller configures
// `sys` (xout_/sign_) and the stepper (tolerances, step cap).
// Leaves yp = state at xout and returns 1 on success, -2 past mxstep (> 0)
// steps between outputs (as dop853's "larger nmax is needed"), -4 on failure.
template <class DenseStepper, class System, class State, int NS = 4>
static int rxOdeintDenseSegment(DenseStepper &dense_stepper, System &sys,
                                rx_solving_options *op, rx_solving_options_ind *ind,
                                int neqOde, double xp, double xout, double dt,
                                double *yp, int obs_first, int obs_last, int solveid,
                                int mxstep) {
  State state(neqOde);
  std::vector<State> q(NS, State(neqOde));
  const double *qp[NS];
  bool fwd = (xout >= xp);
  if (!fwd) dt = -dt;
  int obs = obs_first;
  int nstep = 0;
  try {
    std::copy(yp, yp + neqOde, state.begin());
    dense_stepper.initialize(state, xp, dt);
    while ((fwd && dense_stepper.current_time() < xout) ||
           (!fwd && dense_stepper.current_time() > xout)) {
      if (mxstep > 0 && ++nstep > mxstep) return -2;
      dense_stepper.do_step(sys);
      if (ind->err) { if (ind->rc[0] == 0) ind->rc[0] = -2019; return -4; }
      double t_old = dense_stepper.previous_time();
      double t_cur = dense_stepper.current_time();
      double hh = t_cur - t_old;
      if (ind->delayHistOn) {
        for (int k = 0; k < NS; k++) {
          dense_stepper.calc_state(k == NS - 1 ? t_cur : t_old + hh * k / (NS - 1.0), q[k]);
          qp[k] = &q[k][0];
        }
        if (NS == 4) rxDelayHistPushSamples(ind, op, t_old, hh, qp[0], qp[1], qp[2], qp[3]);
        else rxDelayHistPushNodes(ind, op, t_old, hh, qp);
      }
      while (obs <= obs_last) {
        int raw = ind->ix[obs];
        double t_obs = ind->timeThread[raw];
        if ((fwd && t_obs > t_cur + 1e-8) || (!fwd && t_obs < t_cur - 1e-8)) break;
        if (!isObs(getEvid(ind, raw))) { obs++; continue; }
        _growSolveIfNeeded(ind, op, obs, (obs + 1 != ind->n_all_times));
        double *slot = getSolve(obs);
        dense_stepper.calc_state(t_obs, state);
        for (int j = 0; j < neqOde; j++) slot[j] = state[j];
        calc_lhs(solveid, t_obs, slot, ind->lhs);
        obs++;
        nstep = 0;
      }
    }
    dense_stepper.calc_state(xout, state);
    std::copy(state.begin(), state.end(), yp);
  } catch (const std::exception &e) {
    if (ind->rc[0] == 0) ind->rc[0] = -2019;
    return -4;
  }
  return 1;
}

#endif // ODEINTDENSE_H
