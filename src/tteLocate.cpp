#define R_NO_REMAP
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <vector>
#include <boost/math/tools/roots.hpp>
#include "../inst/include/rxode2.h"
#include "tte.h"
#include "tteState.h"

// Open each endpoint's window at its start record (no interval straddles it)
static void rxTteOpenWindows(int subj, double xout, const double *yout) {
  for (int k = 1; k <= rxTteN(); k++) {
    rxTteState *st = rxTteGet(subj, k);
    if (st == NULL || st->started || ISNA(st->start) || xout < st->start) continue;
    st->started = 1;
    st->hStart = st->h0 = yout[rxTteEp(k).hIdx];
  }
}

// Can endpoint k have an event in (xs, xout]?
static bool rxTteCanFire(const rxTteState *st, int k, double xs, double xout,
                         const double *yout) {
  if (st == NULL || st->done || !st->started || xs < st->start) return false;
  if (!ISNA(st->end) && xout > st->end) return false;
  return yout[rxTteEp(k).hIdx] >= st->h0 + st->e;
}

// Time in (xs, xout] where endpoint k's cumulative hazard reaches its
// threshold: toms748 on H(t) - H0 - E, H(t) re-integrated from ys
static double rxTteRoot(const rxTteState *st, int k, int neq, double xs,
                        const double *ys, double xout, const double *yout,
                        const rxTteInteg &integ) {
  int h = rxTteEp(k).hIdx;
  double target = st->h0 + st->e;
  double flo = ys[h] - target, fhi = yout[h] - target;
  if (flo >= 0.0) return xs;
  if (fhi == 0.0) return xout;
  std::vector<double> tmp(neq);
  auto g = [&](double t) {
    std::copy(ys, ys + neq, tmp.begin());
    integ(tmp.data(), xs, t);
    return tmp[h] - target;
  };
  boost::uintmax_t maxIter = 100;
  boost::math::tools::eps_tolerance<double> tol(40);
  return boost::math::tools::toms748_solve(g, xs, xout, flo, fhi, tol, maxIter).second;
}

// Earliest tte event in (xs, xout]; yStar gets the state at that time
int rxTteLocate(int subj, int neq, double xs, const double *ys, double xout,
                const double *yout, const rxTteInteg &integ, double *tStar,
                int *kOut, double *yStar) {
  if (rxTteN() == 0) return 0;
  int found = 0;
  for (int k = 1; xout > xs && k <= rxTteN(); k++) {
    rxTteState *st = rxTteGet(subj, k);
    if (!rxTteCanFire(st, k, xs, xout, yout)) continue;
    double t = rxTteRoot(st, k, neq, xs, ys, xout, yout, integ);
    if (found && t >= *tStar) continue;
    found = 1;
    *tStar = t;
    *kOut = k;
  }
  if (found) {
    std::copy(ys, ys + neq, yStar);
    integ(yStar, xs, *tStar);
  }
  // windows open on the trajectory that is kept: up to the event, if any
  rxTteOpenWindows(subj, found ? *tStar : xout, found ? yStar : yout);
  return found;
}
