#ifndef RXODE2LINCMT_LINK_H
#define RXODE2LINCMT_LINK_H
/*
 * rxode2's side of the rxode2lincmt tables (src/rxode2lincmtLink.c).  The
 * _p_* pointers start at harmless stubs and are re-bound by `.linkAll()`;
 * call the linCmt kernels only through them.
 */
#include <rxode2lincmtPtrs.h>

#if defined(__cplusplus)
extern "C" {
#endif

double rxode2LinCmtAFwd(rx_solve *rx, int id, double _t, int linCmt, int ncmt,
                        int oral0, int which, int trans, double p1, double v1,
                        double p2, double p3, double p4, double p5, double ka);
double rxode2LinCmtBFwd(rx_solve *rx, int id, double _t, int linCmt, int ncmt,
                        int oral0, int which1, int which2, int trans, double p1,
                        double v1, double p2, double p3, double p4, double p5,
                        double ka);
int rxode2LinCmtThread(int mx);

#if defined(__cplusplus)
}
#endif

#endif
