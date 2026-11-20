// Anderson acceleration of the steady-state one-cycle map u -> P(u)
// (rxControl(ssMethod = "anderson"), nlmixr2/rxode2#1762).
//
// The map acts on the whole state vector, forward-sensitivity states
// included, so the joint fixed point also gives the sensitivities of the
// steady state.  States whose ssAtol or ssRtol is not finite (tte()
// cumulative hazards, output sinks) take no part: they keep P(u).
#ifndef __RXODE2_SS_ANDERSON_H__
#define __RXODE2_SS_ANDERSON_H__

#include <vector>
#include <cmath>
#include <algorithm>

// history depth of the accelerator
#define RXSS_ANDERSON_DEPTH 5
// AA steps without cutting the smallest residual by RXSS_ANDERSON_PROGRESS
// before falling back to plain iteration (no periodic steady state, or one
// AA cannot reach)
#define RXSS_ANDERSON_STALL 20
#define RXSS_ANDERSON_PROGRESS 0.9
// a residual this many times the smallest one restarts the history
#define RXSS_ANDERSON_RESTART 10.0
// restarts before falling back to plain iteration
#define RXSS_ANDERSON_MAXRESTART 3
// divergence ceiling: an AA step longer than this many plain steps
// (a carryover ratio above 1 - 1/RXSS_ANDERSON_MAXRATIO, or none below 1 as
// with dosing above the elimination capacity) is rejected as a restart, and
// AA falls back once its iterate moves farther than this many first
// residuals.  It is lowered to 0.1/ssRtol: a state that only grows passes
// the relative test once it has moved 1/ssRtol of its (constant) residual.
#define RXSS_ANDERSON_MAXRATIO 1e4

class rxSsAnderson {
public:
  bool on = false;
  // the last cycle's input came from an AA step (not plain iteration)
  bool mixed = false;
  // AA gave up: the caller restarts plain iteration from start()
  bool fellBack = false;

  void init(int neq, const double *ssAtol, const double *ssRtol) {
    on = false;
    mixed = false;
    fellBack = false;
    idx.clear();
    for (int k = 0; k < neq; k++) {
      if (std::isfinite(ssAtol[k]) && std::isfinite(ssRtol[k])) idx.push_back(k);
    }
    na = (int)idx.size();
    if (na == 0) return;
    maxRatio = RXSS_ANDERSON_MAXRATIO;
    for (int j = 0; j < na; j++) {
      if (ssRtol[idx[j]] > 0.0 && 0.1/ssRtol[idx[j]] < maxRatio) {
        maxRatio = 0.1/ssRtol[idx[j]];
      }
    }
    m = RXSS_ANDERSON_DEPTH;
    dF.assign((size_t)m*na, 0.0);
    dG.assign((size_t)m*na, 0.0);
    fPrev.assign(na, 0.0);
    gPrev.assign(na, 0.0);
    gLast.assign(neq, 0.0);
    xPrev.assign(neq, 0.0);
    x0.assign(neq, 0.0);
    w.assign(na, 0.0);
    nHist = 0; head = 0; havePrev = false; haveX = false;
    best = INFINITY; stall = 0; restarts = 0;
    nAll = neq;
    on = true;
  }

  // the input x of this cycle is settled: it moved by less than the
  // steady-state tolerance from the previous cycle's input.  Before the first
  // accelerated step the inputs are plain iterates, which the residual test
  // alone judges, as with ssMethod = "iterate"
  bool settled(const double *x, const double *ssAtol, const double *ssRtol) {
    if (!haveX) return true;
    for (int k = nAll; k--;) {
      if (ssRtol[k]*fabs(x[k]) + ssAtol[k] <= fabs(x[k] - xPrev[k])) return false;
    }
    return true;
  }

  // the last true cycle output P(x)
  const double *last() const { return gLast.data(); }

  // the input of the first accelerated cycle, where plain iteration
  // restarts after a fall back
  const double *start() const { return x0.data(); }

  // give up (eg. the dose budget is spent without converging); returns true
  // when AA was running, so the caller restarts plain iteration at start()
  bool giveUp() {
    if (!on || !haveX) return false;
    fallBack();
    return true;
  }

  // x is this cycle's input and g = P(x) its output (in yp); overwrite yp
  // with the next input.  Returns false once AA has fallen back to plain
  // iteration (fellBack; yp is then left as g).
  bool step(double *yp, const double *x,
            const double *ssAtol, const double *ssRtol) {
    if (!on) return false;
    if (!haveX) std::copy(x, x + nAll, x0.begin());
    std::copy(x, x + nAll, xPrev.begin());
    haveX = true;
    std::copy(yp, yp + nAll, gLast.begin());
    mixed = false;
    std::vector<double> f(na), g(na);
    double r = 0.0;
    for (int j = 0; j < na; j++) {
      int k = idx[j];
      g[j] = yp[k];
      f[j] = yp[k] - x[k];
      if (!std::isfinite(g[j]) || !std::isfinite(f[j])) return fallBack();
      // weights are fixed at the first step, so a state that only grows
      // (no steady state) cannot shrink its own residual
      if (!havePrev) w[j] = 1.0/(ssRtol[k]*fabs(g[j]) + ssAtol[k]);
      r += f[j]*w[j]*f[j]*w[j];
    }
    r = sqrt(r);
    if (!havePrev) {
      r0 = r;
    } else {
      // total travel ceiling
      double t = 0.0;
      for (int j = 0; j < na; j++) {
        double d = (x[idx[j]] - x0[idx[j]])*w[j];
        t += d*d;
      }
      if (!(sqrt(t) <= maxRatio*r0)) return fallBack();
    }
    if (havePrev) {
      double *df = &dF[(size_t)head*na];
      double *dg = &dG[(size_t)head*na];
      for (int j = 0; j < na; j++) {
        df[j] = f[j] - fPrev[j];
        dg[j] = g[j] - gPrev[j];
      }
      head = (head + 1) % m;
      if (nHist < m) nHist++;
    }
    std::copy(f.begin(), f.end(), fPrev.begin());
    std::copy(g.begin(), g.end(), gPrev.begin());
    havePrev = true;
    if (r < RXSS_ANDERSON_PROGRESS*best) {
      best = r;
      stall = 0;
    } else if (++stall > RXSS_ANDERSON_STALL) {
      return fallBack();
    }
    if (r > RXSS_ANDERSON_RESTART*best) {
      // diverging: drop the history and take a plain step
      if (++restarts > RXSS_ANDERSON_MAXRESTART) return fallBack();
      nHist = 0; head = 0;
      return true;
    }
    if (nHist == 0) return true;
    // weighted least squares min ||W(f - dF gamma)|| by modified Gram-Schmidt;
    // a column that is (nearly) dependent on the earlier ones is dropped
    std::vector<double> Q((size_t)nHist*na), R((size_t)nHist*nHist, 0.0);
    std::vector<int> col;
    // newest column first, so a dependent column dropped is an older one
    for (int t = 0; t < nHist; t++) {
      int c = (head - 1 - t + 2*m) % m;
      const double *df = &dF[(size_t)c*na];
      double *q = &Q[(size_t)col.size()*na];
      double n0 = 0.0;
      for (int j = 0; j < na; j++) {
        q[j] = df[j]*w[j];
        n0 += q[j]*q[j];
      }
      n0 = sqrt(n0);
      int nc = (int)col.size();
      for (int p = 0; p < nc; p++) {
        const double *qp = &Q[(size_t)p*na];
        double d = 0.0;
        for (int j = 0; j < na; j++) d += qp[j]*q[j];
        R[(size_t)p*nHist + nc] = d;
        for (int j = 0; j < na; j++) q[j] -= d*qp[j];
      }
      double nn = 0.0;
      for (int j = 0; j < na; j++) nn += q[j]*q[j];
      nn = sqrt(nn);
      if (!(nn > 1e-10*n0) || !(n0 > 0.0)) continue;
      for (int j = 0; j < na; j++) q[j] /= nn;
      R[(size_t)nc*nHist + nc] = nn;
      col.push_back(c);
    }
    int nc = (int)col.size();
    if (nc == 0) return true;
    std::vector<double> gam(nc, 0.0);
    for (int p = 0; p < nc; p++) {
      const double *qp = &Q[(size_t)p*na];
      double d = 0.0;
      for (int j = 0; j < na; j++) d += qp[j]*f[j]*w[j];
      gam[p] = d;
    }
    for (int p = nc; p--;) {
      for (int s = p + 1; s < nc; s++) gam[p] -= R[(size_t)p*nHist + s]*gam[s];
      gam[p] /= R[(size_t)p*nHist + p];
    }
    std::vector<double> xn(g);
    for (int p = 0; p < nc; p++) {
      const double *dg = &dG[(size_t)col[p]*na];
      for (int j = 0; j < na; j++) xn[j] -= gam[p]*dg[j];
    }
    double sx = 0.0;
    for (int j = 0; j < na; j++) {
      double d = (xn[j] - g[j])*w[j];
      sx += d*d;
    }
    if (!(sqrt(sx) <= maxRatio*r)) {
      // past the divergence ceiling: take the plain step instead
      if (++restarts > RXSS_ANDERSON_MAXRESTART) return fallBack();
      nHist = 0; head = 0;
      return true;
    }
    // keep a non-negative state non-negative by shortening the step
    double theta = 1.0;
    for (int j = 0; j < na; j++) {
      if (g[j] >= 0.0 && xn[j] < 0.0) {
        double t = 0.9*g[j]/(g[j] - xn[j]);
        if (t < theta) theta = t;
      }
    }
    for (int j = 0; j < na; j++) {
      yp[idx[j]] = g[j] + theta*(xn[j] - g[j]);
    }
    mixed = true;
    return true;
  }

private:
  bool fallBack() {
    on = false;
    mixed = false;
    fellBack = true;
    return false;
  }
  std::vector<int> idx;
  int na = 0, nAll = 0, m = RXSS_ANDERSON_DEPTH;
  // ring buffers of the residual and output differences (column-major)
  std::vector<double> dF, dG;
  std::vector<double> fPrev, gPrev, gLast, xPrev, x0, w;
  int nHist = 0, head = 0;
  bool havePrev = false, haveX = false;
  double best = INFINITY, r0 = 0.0, maxRatio = RXSS_ANDERSON_MAXRATIO;
  int stall = 0, restarts = 0;
};

#endif // __RXODE2_SS_ANDERSON_H__
