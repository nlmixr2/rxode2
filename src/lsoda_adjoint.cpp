// Exact discrete-adjoint driver for liblsoda (method="liblsodaadj", code 202 =
// base liblsoda 2 + 200).  Fills the same rx__sens_<state>_BY_<param>__ columns
// as rk4s / dop853s / cvodesadj, but as the EXACT reverse-mode transpose of
// liblsoda's OWN variable-order / variable-step Nordsieck multistep step map
// (Adams / BDF via stoda) -- not a continuous-adjoint approximation.
//
// #included into par_solve.cpp (guarded by IN_PAR_SOLVE, like rk4s.cpp /
// cvodes_adjoint.cpp) so it sees calc_lhs / iniSubject / getSolve / handle_evid
// and the dydt globals.  Compiles to an empty .o when built standalone.
//
// Math (see ~/src/rxode2-liblsoda-adjoint-plan.md for the full derivation).
// Per ACCEPTED liblsoda step (order nq, step h, coeffs el[], Nordsieck yh):
//   PREDICT  yh_pred = P(h) yh          (Pascal triangle, linear)
//   CORRECT  acor = h*f(y) - yh_pred[2],  y = yh_pred[1] + el[1]*acor
//            solved via  Pmat = I - h*el[1]*J,  solsy = Pmat^{-1}
//   UPDATE   yh_new[j] = yh_pred[j] + el[j]*acor            (linear)
// Reverse-mode transpose (costate Lambda is yh-shaped):
//   UPDATE^T  adj_yh_pred[j] += Lambda_new[j];  adj_acor += sum_j el[j]*Lambda_new[j]
//   CORRECT^T w = Pmat^{-T} adj_acor;  adj_yh_pred[1] += h*J^T w;
//             adj_yh_pred[2] += -w;    mu += h*(dF/dp)^T w   (quadrature; F_p=rx__adjFP)
//   PREDICT^T reverse Pascal loop
// with scaleh^T (diagonal rh^(j-1)), order-change^T (Nordsieck row add/drop) and
// method-switch^T (el change) applied from a recorded accepted-step schedule.
//
// STATUS: P0 -- method registration + dispatch wiring only.  The recording fork
// of stoda and the transpose replay are not implemented yet; this skeleton fills
// the output with NA and flags a bad solve so the wiring stays green and an
// explicit method="liblsodaadj" fails loudly rather than silently returning zeros.
#ifdef IN_PAR_SOLVE

extern "C" void ind_liblsodaadj_0(rx_solve *rx, rx_solving_options *op, int solveid,
                                  int *neq, t_dydt c_dydt, t_update_inis u_inis) {
  (void) c_dydt; (void) u_inis; (void) neq; (void) solveid;
  rx_solving_options_ind *ind = &(rx->subjects[solveid]);
  // Not yet implemented: fill NA and flag a bad solve (mirrors badSolveExit).
  for (int j = rxEffNeq(ind, op) * (ind->n_all_times); j--;) {
    ind->solve[j] = NA_REAL;
  }
  ind->rc[0] = -2020; // liblsodaadj not yet implemented
  op->badSolve = 1;
}

extern "C" void ind_liblsodaadj(rx_solve *rx, int solveid, t_dydt c_dydt, t_update_inis u_inis) {
  rx_solving_options *op = rx->op;
  int neq[2]; neq[0] = op->neq; neq[1] = 0;
  ind_liblsodaadj_0(rx, op, solveid, neq, c_dydt, u_inis);
}

extern "C" void par_liblsodaadj(rx_solve *rx) {
  rx_solving_options *op = rx->op;
  int nsolve = (int)(rx->nsim * rx->nsub);
  for (int solveid = 0; solveid < nsolve; ++solveid) {
    int neq[2]; neq[0] = op->neq; neq[1] = 0;
    ind_liblsodaadj_0(rx, op, solveid, neq, dydt, update_inis);
    if (op->badSolve) break;
  }
}

#endif // IN_PAR_SOLVE
